/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

#ifndef __RUBY_PROF_H__
#define __RUBY_PROF_H__

#include <ruby.h>
#include <stdio.h>

#if RUBY_VERSION == 186
# error 1.8.6 is not supported. Please upgrade to 1.9.3 or higher.
#endif

#if RUBY_VERSION == 187
# error 1.8.7 is not supported. Please upgrade to 1.9.3 or higher.
#endif

#if RUBY_VERSION == 190
# error 1.9.0 is not supported. Please upgrade to 1.9.3 or higher.
#endif

#if RUBY_VERSION == 191
# error 1.9.1 is not supported. Please upgrade to 1.9.3 or higher.
#endif

#if RUBY_VERSION == 192
# error 1.9.2 is not supported. Please upgrade to 1.9.3 or higher.
#endif

#include "rp_measure.h"
#include "rp_method.h"
#include "rp_call_info.h"
#include "rp_stack.h"
#include "rp_thread.h"

extern VALUE mProf;
extern VALUE cProfile;

void method_key(prof_method_key_t* key, VALUE klass, ID mid);

typedef struct
{
    VALUE running;
    VALUE paused;
    prof_measurer_t* measurer;
    VALUE threads;
    st_table* threads_tbl;
    st_table* exclude_threads_tbl;
    thread_data_t* last_thread_data;
    double measurement_at_pause_resume;
} prof_profile_t;


#endif //__RUBY_PROF_H__
