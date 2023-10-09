#include <ruby.h>
/*#if defined(__sun)*/
/*#include <atomic.h>*/
/*#endif*/

/*#ifdef HAVE_LIBKERN_OSATOMIC_H*/
/*#include <libkern/OSAtomic.h>*/
/*#endif*/

/*
Following the wisdom of postgres, we opt to use platform specific memory barriers when available.
These are generally more performant. In this PR, we add specific cases for i386, x86_64.

In the future, we could look at using pg's atomics library directly:
https://github.com/postgres/postgres/tree/9d4649ca49416111aee2c84b7e4441a0b7aa2fac/src/include/port/atomics

Point of contact @ianks
*/

#include "atomic_reference.h"

#if (defined(__i386__) || defined(__i386)) && (defined(__GNUC__) || defined(__INTEL_COMPILER))
#define memory_barrier() \
  __asm__ __volatile__ ("lock; addl $0,0(%%esp)" : : : "memory", "cc")
#elif defined(__x86_64__) && (defined(__GNUC__) || defined(__INTEL_COMPILER))
#define memory_barrier() \
  __asm__ __volatile__ ("lock; addl $0,0(%%rsp)" : : : "memory", "cc")
#elif defined(HAVE_GCC__ATOMIC_INT32_CAS)
#define memory_barrier() \
  __atomic_thread_fence(__ATOMIC_SEQ_CST)
#elif (__GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 1))
#define memory_barrier() \
  __sync_synchronize();
#elif defined _MSC_VER
#define memory_barrier() \
  MemoryBarrier();
#elif __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1050
#define memory_barrier() \
  OSMemoryBarrier();
#endif

void ir_mark(void *value) {
  rb_gc_mark_maybe((VALUE) value);
}

VALUE ir_alloc(VALUE klass) {
  return rb_data_object_wrap(klass, (void *) Qnil, ir_mark, NULL);
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
  memory_barrier();
  return (VALUE) DATA_PTR(self);
}

VALUE ir_set(VALUE self, VALUE new_value) {
  DATA_PTR(self) = (void *) new_value;
  memory_barrier();
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
#if defined(__ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__) && __ENVIRONMENT_MAC_OS_X_VERSION_MIN_REQUIRED__ >= 1050
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
