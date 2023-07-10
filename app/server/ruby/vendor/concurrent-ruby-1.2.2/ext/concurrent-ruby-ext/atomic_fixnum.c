#include <ruby.h>

#include "atomic_fixnum.h"
#include "atomic_reference.h"

void atomic_fixnum_mark(void *value) {
  rb_gc_mark_maybe((VALUE) value);
}

VALUE atomic_fixnum_allocate(VALUE klass) {
  return rb_data_object_wrap(klass, (void *) Qnil, atomic_fixnum_mark, NULL);
}

VALUE method_atomic_fixnum_initialize(int argc, VALUE* argv, VALUE self) {
  VALUE value = LL2NUM(0);
  rb_check_arity(argc, 0, 1);
  if (argc == 1) {
    Check_Type(argv[0], T_FIXNUM);
    value = argv[0];
  }
  DATA_PTR(self) = (void *) value;
  return(self);
}

VALUE method_atomic_fixnum_value(VALUE self) {
  return (VALUE) DATA_PTR(self);
}

VALUE method_atomic_fixnum_value_set(VALUE self, VALUE value) {
  Check_Type(value, T_FIXNUM);
  DATA_PTR(self) = (void *) value;
  return(value);
}

VALUE method_atomic_fixnum_increment(int argc, VALUE* argv, VALUE self) {
  long long value = NUM2LL((VALUE) DATA_PTR(self));
  long long delta = 1;
  rb_check_arity(argc, 0, 1);
  if (argc == 1) {
    Check_Type(argv[0], T_FIXNUM);
    delta = NUM2LL(argv[0]);
  }
  return method_atomic_fixnum_value_set(self, LL2NUM(value + delta));
}

VALUE method_atomic_fixnum_decrement(int argc, VALUE* argv, VALUE self) {
  long long value = NUM2LL((VALUE) DATA_PTR(self));
  long long delta = 1;
  rb_check_arity(argc, 0, 1);
  if (argc == 1) {
    Check_Type(argv[0], T_FIXNUM);
    delta = NUM2LL(argv[0]);
  }
  return method_atomic_fixnum_value_set(self, LL2NUM(value - delta));
}

VALUE method_atomic_fixnum_compare_and_set(VALUE self, VALUE rb_expect, VALUE rb_update) {
  Check_Type(rb_expect, T_FIXNUM);
  Check_Type(rb_update, T_FIXNUM);
  return ir_compare_and_set(self, rb_expect, rb_update);
}

VALUE method_atomic_fixnum_update(VALUE self) {
  VALUE old_value, new_value;
  for (;;) {
    old_value = method_atomic_fixnum_value(self);
    new_value = rb_yield(old_value);
    if (ir_compare_and_set(self, old_value, new_value) == Qtrue) {
      return new_value;
    }
  }
}
