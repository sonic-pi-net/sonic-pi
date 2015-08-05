#include <ruby.h>
/*#if defined(__sun)*/
/*#include <atomic.h>*/
/*#endif*/

/*#ifdef HAVE_LIBKERN_OSATOMIC_H*/
/*#include <libkern/OSAtomic.h>*/
/*#endif*/

#include "atomic_reference.h"

void ir_mark(void *value) {
  rb_gc_mark_maybe((VALUE) value);
}

VALUE ir_alloc(VALUE klass) {
  return rb_data_object_alloc(klass, (void *) Qnil, ir_mark, NULL);
}

VALUE ir_initialize(int argc, VALUE* argv, VALUE self) {
  VALUE value = Qnil;
  if (rb_scan_args(argc, argv, "01", &value) == 1) {
    value = argv[0];
  }
  DATA_PTR(self) = (void *) value;
  return Qnil;
}

VALUE ir_get(VALUE self) {
#if HAVE_GCC_SYNC
  __sync_synchronize();
#elif defined _MSC_VER
  MemoryBarrier();
#elif __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1050
  OSMemoryBarrier();
#endif
  return (VALUE) DATA_PTR(self);
}

VALUE ir_set(VALUE self, VALUE new_value) {
  DATA_PTR(self) = (void *) new_value;
#if HAVE_GCC_SYNC
  __sync_synchronize();
#elif defined _MSC_VER
  MemoryBarrier();
#elif __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1050
  OSMemoryBarrier();
#endif
  return new_value;
}

VALUE ir_get_and_set(VALUE self, VALUE new_value) {
  VALUE old_value;
  for (;;) {
    old_value = ir_get(self);
    if (ir_compare_and_set(self, old_value, new_value) == Qtrue) {
      return old_value;
    }
  }
}

VALUE ir_compare_and_set(volatile VALUE self, VALUE expect_value, VALUE new_value) {
#if __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1050
  if (OSAtomicCompareAndSwap64(expect_value, new_value, &DATA_PTR(self))) {
    return Qtrue;
  }
#elif defined(__sun)
  /*  Assuming VALUE is uintptr_t */
  /*  Based on the definition of uintptr_t from /usr/include/sys/int_types.h */
#if defined(_LP64) || defined(_I32LPx)
  /*  64-bit: uintptr_t === unsigned long */
  if (atomic_cas_ulong((uintptr_t *) &DATA_PTR(self), expect_value, new_value)) {
    return Qtrue;
  }
#else
  /*  32-bit: uintptr_t === unsigned int */
  if (atomic_cas_uint((uintptr_t *) &DATA_PTR(self), expect_value, new_value)) {
    return Qtrue;
  }
#endif
#elif defined _MSC_VER && defined _M_AMD64
  if (InterlockedCompareExchange64((LONGLONG*)&DATA_PTR(self), new_value, expect_value)) {
    return Qtrue;
  }
#elif defined _MSC_VER && defined _M_IX86
  if (InterlockedCompareExchange((LONG*)&DATA_PTR(self), new_value, expect_value)) {
    return Qtrue;
  }
#else
  if (__sync_bool_compare_and_swap(&DATA_PTR(self), expect_value, new_value)) {
    return Qtrue;
  }
#endif
  return Qfalse;
}
