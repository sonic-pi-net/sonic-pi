#include <ruby.h>

#include "atomic_boolean.h"
#include "atomic_reference.h"

void atomic_boolean_mark(void *value) {
  rb_gc_mark_maybe((VALUE) value);
}

VALUE atomic_boolean_allocate(VALUE klass) {
  return rb_data_object_wrap(klass, (void *) Qfalse, atomic_boolean_mark, NULL);
}

VALUE method_atomic_boolean_initialize(int argc, VALUE* argv, VALUE self) {
  VALUE value = Qfalse;
  rb_check_arity(argc, 0, 1);
  if (argc == 1) value = TRUTHY(argv[0]);
  DATA_PTR(self) = (void *) value;
  return(self);
}

VALUE method_atomic_boolean_value(VALUE self) {
  return(ir_get(self));
}

VALUE method_atomic_boolean_value_set(VALUE self, VALUE value) {
  VALUE new_value = TRUTHY(value);
  return(ir_set(self, new_value));
}

VALUE method_atomic_boolean_true_question(VALUE self) {
  return(method_atomic_boolean_value(self));
}

VALUE method_atomic_boolean_false_question(VALUE self) {
  VALUE current = method_atomic_boolean_value(self);
  return(current == Qfalse ? Qtrue : Qfalse);
}

VALUE method_atomic_boolean_make_true(VALUE self) {
  return(ir_compare_and_set(self, Qfalse, Qtrue));
}

VALUE method_atomic_boolean_make_false(VALUE self) {
  return(ir_compare_and_set(self, Qtrue, Qfalse));
}
