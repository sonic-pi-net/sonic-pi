/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

#include "ruby_prof.h"

static VALUE cMeasureCpuTime;

/* The _WIN32 check is needed for msys (and maybe cygwin?) */
#if defined(__GNUC__) && !defined(_WIN32)

#include <sys/resource.h>
#include <stdint.h>
#include <time.h>

static unsigned long long get_cpu_time()
{
#if defined(__i386__) || defined(__x86_64__)
    uint32_t a, d;
    __asm__ volatile("rdtsc" : "=a" (a), "=d" (d));
    return ((uint64_t)d << 32) + a;
#elif defined(__powerpc__) || defined(__ppc__)
    unsigned long long x, y;

    __asm__ __volatile__ ("\n\
    1:  mftbu   %1\n\
        mftb    %L0\n\
        mftbu   %0\n\
        cmpw    %0,%1\n\
        bne-    1b"
        : "=r" (x), "=r" (y));

    return x;
#endif
}

static unsigned long long get_cpu_frequency()
{
    static unsigned long long cpu_frequency;

    if(!cpu_frequency) {
        unsigned long long x, y;

        struct timespec ts;
        ts.tv_sec = 0;
        ts.tv_nsec = 500000000;
        x = get_cpu_time();
        nanosleep(&ts, NULL);
        y = get_cpu_time();
        cpu_frequency = (y - x) * 2;
    }

    return cpu_frequency;
}

static double
measure_cpu_time()
{
    struct rusage rusage;
    getrusage(RUSAGE_SELF, &rusage);

    double seconds = 0;

    seconds += rusage.ru_utime.tv_sec;
    seconds += rusage.ru_stime.tv_sec;

    seconds += rusage.ru_utime.tv_usec / 1000000.0;
    seconds += rusage.ru_stime.tv_usec / 1000000.0;

    return seconds;
}

#elif defined(_WIN32)

static unsigned long long get_cpu_time()
{
    LARGE_INTEGER time;
    QueryPerformanceCounter(&time);
    return time.QuadPart;
}

static unsigned long long get_cpu_frequency()
{
    static unsigned long long cpu_frequency;

    if(!cpu_frequency) {
        LARGE_INTEGER cpu_frequency_struct;
        QueryPerformanceFrequency(&cpu_frequency_struct);
        cpu_frequency = cpu_frequency_struct.QuadPart;
    }

    return cpu_frequency;
}

static double
measure_cpu_time()
{
    return ((double)get_cpu_time()) / get_cpu_frequency();
}
#endif


prof_measurer_t* prof_measurer_cpu_time()
{
    prof_measurer_t* measure = ALLOC(prof_measurer_t);
    measure->measure = measure_cpu_time;
    return measure;
}

/* call-seq:
   measure -> float

Returns the cpu time.*/
static VALUE
prof_measure_cpu_time(VALUE self)
{
    return rb_float_new(measure_cpu_time());
}

/* call-seq:
   cpu_frequency -> int

Returns the cpu's frequency.  This value is needed when
RubyProf::measure_mode is set to CPU_TIME. */
static VALUE
prof_get_cpu_frequency(VALUE self)
{
    return ULL2NUM(get_cpu_frequency());
}

void rp_init_measure_cpu_time()
{
    rb_define_const(mProf, "CPU_TIME", INT2NUM(MEASURE_CPU_TIME));
    rb_define_const(mProf, "CPU_TIME_ENABLED", Qtrue);

    cMeasureCpuTime = rb_define_class_under(mMeasure, "CpuTime", rb_cObject);
    rb_define_singleton_method(cMeasureCpuTime, "measure", prof_measure_cpu_time, 0);
    rb_define_singleton_method(cMeasureCpuTime, "frequency", prof_get_cpu_frequency, 0);
}
