/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

/* :nodoc: */

#include "ruby_prof.h"

static VALUE cMeasureMemory;


#if defined(HAVE_RB_GC_ALLOCATED_SIZE)
  VALUE rb_gc_allocated_size();
#endif

#if defined(HAVE_RB_GC_MALLOC_ALLOCATED_SIZE)
  size_t rb_gc_malloc_allocated_size();
#endif

#if defined(HAVE_RB_HEAP_TOTAL_MEM)
  //FIXME: did not find the patch to check prototype, assuming it to return size_t
  size_t rb_heap_total_mem();
#endif

static double
measure_memory()
{
#if defined(HAVE_RB_GC_ALLOCATED_SIZE)
#define MEASURE_MEMORY_ENABLED Qtrue
#if defined(HAVE_LONG_LONG)
    return NUM2LL(rb_gc_allocated_size()) / 1024.0;
#else
    return NUM2ULONG(rb_gc_allocated_size()) / 1024.0;
#endif

#elif defined(HAVE_RB_GC_MALLOC_ALLOCATED_SIZE)
#define MEASURE_MEMORY_ENABLED Qtrue
    return rb_gc_malloc_allocated_size() / 1024.0;

#elif defined(HAVE_RB_GC_TOTAL_MALLOCED_BYTES)
#define MEASURE_MEMORY_ENABLED Qtrue
    return rb_gc_total_malloced_bytes() / 1024.0;

#elif defined(HAVE_RB_HEAP_TOTAL_MEM)
#define MEASURE_MEMORY_ENABLED Qtrue
    return rb_heap_total_mem() / 1024.0;

#else
#define MEASURE_MEMORY_ENABLED Qfalse
    return 0;
#endif
}

prof_measurer_t* prof_measurer_memory()
{
  prof_measurer_t* measure = ALLOC(prof_measurer_t);
  measure->measure = measure_memory;
  return measure;
}

/* call-seq:
   measure_process_time -> float

Returns the process time.*/
static VALUE
prof_measure_memory(VALUE self)
{
    return rb_float_new(measure_memory());
}

void rp_init_measure_memory()
{
    rb_define_const(mProf, "MEMORY", INT2NUM(MEASURE_MEMORY));
    rb_define_const(mProf, "MEMORY_ENABLED", MEASURE_MEMORY_ENABLED);

    cMeasureMemory = rb_define_class_under(mMeasure, "Memory", rb_cObject);
    rb_define_singleton_method(cMeasureMemory, "measure", prof_measure_memory, 0);
}
