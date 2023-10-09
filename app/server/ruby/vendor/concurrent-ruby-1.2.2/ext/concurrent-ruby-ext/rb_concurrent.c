#include <ruby.h>

#include "atomic_reference.h"
#include "atomic_boolean.h"
#include "atomic_fixnum.h"

// module and class definitions

static VALUE rb_mConcurrent;
static VALUE rb_cAtomicReference;
static VALUE rb_cAtomicBoolean;
static VALUE rb_cAtomicFixnum;

// Init_extension

void Init_concurrent_ruby_ext(void) {

  // define modules and classes
  rb_mConcurrent = rb_define_module("Concurrent");
  rb_cAtomicReference = rb_define_class_under(rb_mConcurrent, "CAtomicReference", rb_cObject);
  rb_cAtomicBoolean = rb_define_class_under(rb_mConcurrent, "CAtomicBoolean", rb_cObject);
  rb_cAtomicFixnum = rb_define_class_under(rb_mConcurrent, "CAtomicFixnum", rb_cObject);

  // CAtomicReference
  rb_define_alloc_func(rb_cAtomicReference, ir_alloc);
  rb_define_method(rb_cAtomicReference, "initialize", ir_initialize, -1);
  rb_define_method(rb_cAtomicReference, "get", ir_get, 0);
  rb_define_method(rb_cAtomicReference, "set", ir_set, 1);
  rb_define_method(rb_cAtomicReference, "get_and_set", ir_get_and_set, 1);
  rb_define_method(rb_cAtomicReference, "_compare_and_set", ir_compare_and_set, 2);
  rb_define_alias(rb_cAtomicReference, "value", "get");
  rb_define_alias(rb_cAtomicReference, "value=", "set");
  rb_define_alias(rb_cAtomicReference, "swap", "get_and_set");

  // CAtomicBoolean
  rb_define_alloc_func(rb_cAtomicBoolean, atomic_boolean_allocate);
  rb_define_method(rb_cAtomicBoolean, "initialize", method_atomic_boolean_initialize, -1);
  rb_define_method(rb_cAtomicBoolean, "value", method_atomic_boolean_value, 0);
  rb_define_method(rb_cAtomicBoolean, "value=", method_atomic_boolean_value_set, 1);
  rb_define_method(rb_cAtomicBoolean, "true?", method_atomic_boolean_true_question, 0);
  rb_define_method(rb_cAtomicBoolean, "false?", method_atomic_boolean_false_question, 0);
  rb_define_method(rb_cAtomicBoolean, "make_true", method_atomic_boolean_make_true, 0);
  rb_define_method(rb_cAtomicBoolean, "make_false", method_atomic_boolean_make_false, 0);

  // CAtomicFixnum
  rb_define_const(rb_cAtomicFixnum, "MIN_VALUE", LL2NUM(LLONG_MIN));
  rb_define_const(rb_cAtomicFixnum, "MAX_VALUE", LL2NUM(LLONG_MAX));
  rb_define_alloc_func(rb_cAtomicFixnum, atomic_fixnum_allocate);
  rb_define_method(rb_cAtomicFixnum, "initialize", method_atomic_fixnum_initialize, -1);
  rb_define_method(rb_cAtomicFixnum, "value", method_atomic_fixnum_value, 0);
  rb_define_method(rb_cAtomicFixnum, "value=", method_atomic_fixnum_value_set, 1);
  rb_define_method(rb_cAtomicFixnum, "increment", method_atomic_fixnum_increment, -1);
  rb_define_method(rb_cAtomicFixnum, "decrement", method_atomic_fixnum_decrement, -1);
  rb_define_method(rb_cAtomicFixnum, "compare_and_set", method_atomic_fixnum_compare_and_set, 2);
  rb_define_method(rb_cAtomicFixnum, "update", method_atomic_fixnum_update, 0);
  rb_define_alias(rb_cAtomicFixnum, "up", "increment");
  rb_define_alias(rb_cAtomicFixnum, "down", "decrement");
}
