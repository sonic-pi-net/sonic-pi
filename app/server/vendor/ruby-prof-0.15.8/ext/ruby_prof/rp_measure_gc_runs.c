/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

/* :nodoc: */

#include "ruby_prof.h"

static VALUE cMeasureGcRuns;

#if defined(HAVE_RB_GC_COLLECTIONS)
  VALUE rb_gc_collections(void);
#endif

#if defined(HAVE_RB_GC_COUNT)
  size_t rb_gc_count(void);
#endif

#if defined(HAVE_RB_GC_HEAP_INFO)
  VALUE rb_gc_heap_info(void);
#endif


static double
measure_gc_runs()
{
#if defined(HAVE_RB_GC_COLLECTIONS)
#define MEASURE_GC_RUNS_ENABLED Qtrue
  return NUM2INT(rb_gc_collections());

#elif defined(HAVE_RB_GC_COUNT)
#define MEASURE_GC_RUNS_ENABLED Qtrue
  return rb_gc_count();

#elif defined(HAVE_RB_GC_HEAP_INFO)
#define MEASURE_GC_RUNS_ENABLED Qtrue
  VALUE h = rb_gc_heap_info();
  return NUM2UINT(rb_hash_aref(h, rb_str_new2("num_gc_passes")));

#else
#define MEASURE_GC_RUNS_ENABLED Qfalse
  return 0;
#endif
}

prof_measurer_t* prof_measurer_gc_runs()
{
  prof_measurer_t* measure = ALLOC(prof_measurer_t);
  measure->measure = measure_gc_runs;
  return measure;
}

/* call-seq:
   measure -> int

Returns the number of GC runs.*/
static VALUE
prof_measure_gc_runs(VALUE self)
{
#if defined(HAVE_LONG_LONG)
    return ULL2NUM(measure_gc_runs());
#else
    return ULONG2NUM(measure_gc_runs());
#endif
}

void rp_init_measure_gc_runs()
{
    rb_define_const(mProf, "GC_RUNS", INT2NUM(MEASURE_GC_RUNS));
    rb_define_const(mProf, "GC_RUNS_ENABLED", MEASURE_GC_RUNS_ENABLED);
    
    cMeasureGcRuns = rb_define_class_under(mMeasure, "GcRuns", rb_cObject);
    rb_define_singleton_method(cMeasureGcRuns, "measure", prof_measure_gc_runs, 0);
}
