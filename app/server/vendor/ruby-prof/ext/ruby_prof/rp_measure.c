/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

#include "ruby_prof.h"

VALUE mMeasure;

prof_measurer_t* prof_get_measurer(prof_measure_mode_t measure)
{
    switch (measure)
    {
    case MEASURE_ALLOCATIONS:
      return prof_measurer_allocations();
      break;
    case MEASURE_CPU_TIME:
      return prof_measurer_cpu_time();
      break;
    case MEASURE_GC_RUNS:
      return prof_measurer_gc_runs();
      break;
    case MEASURE_GC_TIME:
      return prof_measurer_gc_time();
      break;
    case MEASURE_MEMORY:
      return prof_measurer_memory();
      break;
    case MEASURE_PROCESS_TIME:
      return prof_measurer_process_time();
      break;
    case MEASURE_WALL_TIME:
      return prof_measurer_wall_time();
      break;
	default:
	  rb_raise(rb_eArgError, "Unknown measure mode: %d", measure);
    }
};

void rp_init_measure()
{
    mMeasure = rb_define_module_under(mProf, "Measure");
    rp_init_measure_allocations();
    rp_init_measure_cpu_time();
    rp_init_measure_gc_runs();
    rp_init_measure_gc_time();
    rp_init_measure_memory();
    rp_init_measure_process_time();
    rp_init_measure_wall_time();
}
