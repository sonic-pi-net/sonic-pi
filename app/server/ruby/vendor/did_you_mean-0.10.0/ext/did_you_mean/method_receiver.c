#include <ruby.h>

static const rb_data_type_t *type;

static VALUE
name_err_receiver(VALUE self)
{
  VALUE *ptr, mesg = rb_attr_get(self, rb_intern("mesg"));
  TypedData_Get_Struct(mesg, VALUE, type, ptr);
  return ptr[1];
}

void
Init_method_receiver()
{
  VALUE err_mesg = rb_funcall(rb_cNameErrorMesg, '!', 3, Qnil, Qnil, Qnil);
  type = RTYPEDDATA(err_mesg)->type;

  rb_define_method(rb_eNameError, "receiver", name_err_receiver, 0);
}
