#ifndef __ATOMIC_FIXNUM_H__
#define __ATOMIC_FIXNUM_H__

void atomic_fixnum_mark(void*);
VALUE atomic_fixnum_allocate(VALUE);
VALUE method_atomic_fixnum_initialize(int, VALUE*, VALUE);
VALUE method_atomic_fixnum_value(VALUE);
VALUE method_atomic_fixnum_value_set(VALUE, VALUE);
VALUE method_atomic_fixnum_increment(int, VALUE*, VALUE);
VALUE method_atomic_fixnum_decrement(int, VALUE*, VALUE);
VALUE method_atomic_fixnum_compare_and_set(VALUE, VALUE, VALUE);
VALUE method_atomic_fixnum_update(VALUE);

#endif
