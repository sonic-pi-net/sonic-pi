#ifndef __ATOMIC_REFERENCE_H__
#define __ATOMIC_REFERENCE_H__

#if defined(__sun)
#include <atomic.h>
#endif

#ifdef HAVE_LIBKERN_OSATOMIC_H
#include <libkern/OSAtomic.h>
#endif

void ir_mark(void*);
VALUE ir_alloc(VALUE);
VALUE ir_initialize(int, VALUE*, VALUE);
VALUE ir_get(VALUE);
VALUE ir_set(VALUE, VALUE);
VALUE ir_get_and_set(VALUE, VALUE);
VALUE ir_compare_and_set(volatile VALUE, VALUE, VALUE);

#endif
