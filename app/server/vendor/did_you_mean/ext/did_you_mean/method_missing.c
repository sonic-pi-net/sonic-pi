#include <ruby.h>
#include "internal.h"
#include "vm_method.c"

#ifndef UNREACHABLE
# define UNREACHABLE /* unreachable */
#endif

static inline const rb_data_type_t *
threadptr_data_type(void)
{
  static const rb_data_type_t *thread_data_type;
  if (!thread_data_type) {
    VALUE current_thread = rb_thread_current();
    thread_data_type = RTYPEDDATA_TYPE(current_thread);
  }
  return thread_data_type;
}

#define ruby_thread_data_type *threadptr_data_type()
#define ruby_threadptr_data_type *threadptr_data_type()

#define ruby_current_thread ((rb_thread_t *)RTYPEDDATA_DATA(rb_thread_current()))

NORETURN(static void raise_method_missing(rb_thread_t *th, int argc, const VALUE *argv,
                                          VALUE obj, int call_status));

static VALUE
rb_method_missing(int argc, const VALUE *argv, VALUE obj)
{
  rb_thread_t *th;
  GetThreadPtr(rb_thread_current(), th);
  raise_method_missing(th, argc, argv, obj, th->method_missing_reason);
  UNREACHABLE;
}

#define NOEX_MISSING   0x80

static VALUE
make_no_method_exception(VALUE exc, const char *format, VALUE obj, int argc, const VALUE *argv)
{
  int n = 0;
  VALUE mesg;
  VALUE args[3];

  if (!format) {
    format = "undefined method `%s' for %s";
  }
  mesg = rb_const_get(exc, rb_intern("message"));
  // if (rb_method_basic_definition_p(CLASS_OF(mesg), '!')) {
  //   args[n++] = rb_name_err_mesg_new(mesg, rb_str_new2(format), obj, argv[0]);
  // }
  // else {
       args[n++] = rb_funcall(mesg, '!', 3, rb_str_new2(format), obj, argv[0]);
  // }

  args[n++] = argv[0];
  if (exc == rb_eNoMethodError) {
    args[n++] = rb_ary_new4(argc - 1, argv + 1);
    rb_ary_store(args[n - 1], 0, obj);
  }
  return rb_class_new_instance(n, args, exc);
}

static void
raise_method_missing(rb_thread_t *th, int argc, const VALUE *argv, VALUE obj,
                     int last_call_status)
{
  VALUE exc = rb_eNoMethodError;
  const char *format = 0;

  if (argc == 0 || !SYMBOL_P(argv[0])) {
    rb_raise(rb_eArgError, "no id given");
  }

  ruby_stack_check();

  if (last_call_status & NOEX_PRIVATE) {
    format = "private method `%s' called for %s";
  }
  else if (last_call_status & NOEX_PROTECTED) {
    format = "protected method `%s' called for %s";
  }
  else if (last_call_status & NOEX_VCALL) {
    format = "undefined local variable or method `%s' for %s";
    exc = rb_eNameError;
  }
  else if (last_call_status & NOEX_SUPER) {
    format = "super: no superclass method `%s' for %s";
  }

  {
    exc = make_no_method_exception(exc, format, obj, argc, argv);
    if (!(last_call_status & NOEX_MISSING)) {
      th->cfp = RUBY_VM_PREVIOUS_CONTROL_FRAME(th->cfp);
    }
    rb_exc_raise(exc);
  }
}

void
Init_method_missing()
{
  VALUE defined_class;
  rb_method_entry_t *me;

  me = search_method(rb_cBasicObject, rb_intern("method_missing"), &defined_class);
  me->def->body.cfunc.func = rb_method_missing;
}
