/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

/* :nodoc: */

#include "ruby_prof.h"

static VALUE cMeasureAllocations;

#if defined(HAVE_RB_OS_ALLOCATED_OBJECTS)
  unsigned LONG_LONG rb_os_allocated_objects();
#endif

#if defined(HAVE_RB_GC_STAT)
size_t rb_gc_stat(VALUE key);
#endif

static double
measure_allocations()
{
#if defined(HAVE_RB_OS_ALLOCATED_OBJECTS)
#define MEASURE_ALLOCATIONS_ENABLED Qtrue
    return rb_os_allocated_objects();

#elif defined(HAVE_RB_GC_STAT) && RUBY_VERSION == 210
#define MEASURE_ALLOCATIONS_ENABLED Qtrue
    static VALUE total_alloc_symbol = 0;
    if (!total_alloc_symbol) {
      total_alloc_symbol = ID2SYM(rb_intern_const("total_allocated_object"));
    }
    return rb_gc_stat(total_alloc_symbol);

#else
#define MEASURE_ALLOCATIONS_ENABLED Qfalse
    return 0;
#endif
}


prof_measurer_t* prof_measurer_allocations()
{
  prof_measurer_t* measure = ALLOC(prof_measurer_t);
  measure->measure = measure_allocations;
  return measure;
}

/* call-seq:
     measure -> int

Returns the number of Ruby object allocations. */

static VALUE
prof_measure_allocations(VALUE self)
{
#if defined(HAVE_LONG_LONG)
    return ULL2NUM(measure_allocations());
#else
    return ULONG2NUM(measure_allocations());
#endif
}

void rp_init_measure_allocations()
{
    rb_define_const(mProf, "ALLOCATIONS", INT2NUM(MEASURE_ALLOCATIONS));
    rb_define_const(mProf, "ALLOCATIONS_ENABLED", MEASURE_ALLOCATIONS_ENABLED);

    cMeasureAllocations = rb_define_class_under(mMeasure, "Allocations", rb_cObject);
    rb_define_singleton_method(cMeasureAllocations, "measure", prof_measure_allocations, 0);
}
