/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

#include "ruby_prof.h"
#include <time.h>

static VALUE cMeasureProcessTime;

static double
measure_process_time()
{
#if defined(__linux__)
    struct timespec clock;
    clock_gettime(CLOCK_PROCESS_CPUTIME_ID , &clock);
    return clock.tv_sec + (clock.tv_nsec/1000000000.0);
#elif defined(_win32)
	FILETIME createTime;
	FILETIME exitTime;
	FILETIME sysTime;
	FILETIME cpuTime;

	ULARGE_INTEGER sysTimeInt;
	ULARGE_INTEGER cpuTimeInt;
	ULONGLONG totalTime;

	GetProcessTimes(GetCurrentProcess(), &createTime, &exitTime, &sysTime, &cpuTime); 

	/* Doing this based on MSFT's recommendation in the FILETIME structure documentation at
	  http://msdn.microsoft.com/en-us/library/ms724284%28VS.85%29.aspx*/

	sysTimeInt.LowPart = sysTime.dwLowDateTime;
	sysTimeInt.HighPart = sysTime.dwHighDateTime;
	cpuTimeInt.LowPart = cpuTime.dwLowDateTime;
	cpuTimeInt.HighPart = cpuTime.dwHighDateTime;

	totalTime = sysTimeInt.QuadPart + cpuTimeInt.QuadPart;

	// Times are in 100-nanosecond time units.  So instead of 10-9 use 10-7
	return totalTime / 10000000.0;
#else
    return ((double)clock()) / CLOCKS_PER_SEC;
#endif
}

/* call-seq:
   measure_process_time -> float

Returns the process time.*/
static VALUE
prof_measure_process_time(VALUE self)
{
    return rb_float_new(measure_process_time());
}

prof_measurer_t* prof_measurer_process_time()
{
  prof_measurer_t* measure = ALLOC(prof_measurer_t);
  measure->measure = measure_process_time;
  return measure;
}


void rp_init_measure_process_time()
{
    rb_define_const(mProf, "CLOCKS_PER_SEC", INT2NUM(CLOCKS_PER_SEC));
    rb_define_const(mProf, "PROCESS_TIME", INT2NUM(MEASURE_PROCESS_TIME));
	rb_define_const(mProf, "PROCESS_TIME_ENABLED", Qtrue);

    cMeasureProcessTime = rb_define_class_under(mMeasure, "ProcessTime", rb_cObject);
    rb_define_singleton_method(cMeasureProcessTime, "measure", prof_measure_process_time, 0);
}
