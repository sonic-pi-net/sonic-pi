#include <ruby.h>

#include "atomic_fixnum.h"
#include "atomic_reference.h"
#include "ruby_193_compatible.h"

void atomic_fixnum_mark(void *value) {
  rb_gc_mark_maybe((VALUE) value);
}

VALUE atomic_fixnum_allocate(VALUE klass) {
  return rb_data_object_alloc(klass, (void *) Qnil, atomic_fixnum_mark, NULL);
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

VALUE method_atomic_fixnum_increment(VALUE self) {
  long long value = NUM2LL((VALUE) DATA_PTR(self));
  return method_atomic_fixnum_value_set(self, LL2NUM(value + 1));
}

VALUE method_atomic_fixnum_decrement(VALUE self) {
  long long value = NUM2LL((VALUE) DATA_PTR(self));
  return method_atomic_fixnum_value_set(self, LL2NUM(value - 1));
}

VALUE method_atomic_fixnum_compare_and_set(VALUE self, VALUE rb_expect, VALUE rb_update) {
  Check_Type(rb_expect, T_FIXNUM);
  Check_Type(rb_update, T_FIXNUM);
  return ir_compare_and_set(self, rb_expect, rb_update);
}
