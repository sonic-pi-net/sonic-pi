/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

/* :nodoc: */

#include "ruby_prof.h"

static VALUE cMeasureGcTimes;

#if defined(HAVE_RB_GC_TIME)
  VALUE rb_gc_time();
#endif

static double
measure_gc_time()
{
#if defined(HAVE_RB_GC_TOTAL_TIME)
#define MEASURE_GC_TIME_ENABLED Qtrue
    return rb_gc_total_time();

#elif defined(HAVE_RB_GC_TIME)
#define MEASURE_GC_TIME_ENABLED Qtrue
    const double conversion = 1000000.0;
#if HAVE_LONG_LONG
    return NUM2LL(rb_gc_time()) / conversion;
#else
    return NUM2LONG(rb_gc_time()) / conversion;
#endif

#else
#define MEASURE_GC_TIME_ENABLED Qfalse
    return 0.0;
#endif
}

prof_measurer_t* prof_measurer_gc_time()
{
  prof_measurer_t* measure = ALLOC(prof_measurer_t);
  measure->measure = measure_gc_time;
  return measure;
}

/* call-seq:
   measure -> float

Returns the time spent performing GC.*/
static VALUE
prof_measure_gc_time(VALUE self)
{
    return rb_float_new(measure_gc_time());
}

void rp_init_measure_gc_time()
{
    rb_define_const(mProf, "GC_TIME", INT2NUM(MEASURE_GC_TIME));
    rb_define_const(mProf, "GC_TIME_ENABLED", MEASURE_GC_TIME_ENABLED);

    cMeasureGcTimes = rb_define_class_under(mMeasure, "GcTime", rb_cObject);
    rb_define_singleton_method(cMeasureGcTimes, "measure", prof_measure_gc_time, 0);
}
