#ifndef __ATOMIC_BOOLEAN_H__
#define __ATOMIC_BOOLEAN_H__

#define TRUTHY(value)(value == Qfalse || value == Qnil ? Qfalse : Qtrue)

void atomic_boolean_mark(void*);
VALUE atomic_boolean_allocate(VALUE);
VALUE method_atomic_boolean_initialize(int, VALUE*, VALUE);
VALUE method_atomic_boolean_value(VALUE);
VALUE method_atomic_boolean_value_set(VALUE, VALUE);
VALUE method_atomic_boolean_true_question(VALUE);
VALUE method_atomic_boolean_false_question(VALUE);
VALUE method_atomic_boolean_make_true(VALUE);
VALUE method_atomic_boolean_make_false(VALUE);

#endif
