/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

#ifndef __RP_MEASUREMENT_H__
#define __RP_MEASUREMENT_H__

extern VALUE mMeasure;

typedef double (*get_measurement)();

typedef struct
{
    get_measurement measure;
} prof_measurer_t;

typedef enum
{
    MEASURE_WALL_TIME,
    MEASURE_PROCESS_TIME,
    MEASURE_CPU_TIME,
    MEASURE_ALLOCATIONS,
    MEASURE_MEMORY,
    MEASURE_GC_TIME,
    MEASURE_GC_RUNS,
} prof_measure_mode_t;

prof_measurer_t* prof_get_measurer(prof_measure_mode_t measure);
prof_measurer_t* prof_measurer_allocations();
prof_measurer_t* prof_measurer_cpu_time();
prof_measurer_t* prof_measurer_gc_runs();
prof_measurer_t* prof_measurer_gc_time();
prof_measurer_t* prof_measurer_memory();
prof_measurer_t* prof_measurer_process_time();
prof_measurer_t* prof_measurer_wall_time();

void rp_init_measure();
void rp_init_measure_allocations();
void rp_init_measure_cpu_time();
void rp_init_measure_gc_runs();
void rp_init_measure_gc_time();
void rp_init_measure_memory();
void rp_init_measure_process_time();
void rp_init_measure_wall_time();

#endif //__RP_MEASUREMENT_H__
