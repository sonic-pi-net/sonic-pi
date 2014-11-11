/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

/* ruby-prof tracks the time spent executing every method in ruby programming.
   The main players are:

     profile_t         - This represents 1 profile.
     thread_data_t     - Stores data about a single thread.
     prof_stack_t      - The method call stack in a particular thread
     prof_method_t     - Profiling information about each method
     prof_call_info_t  - Keeps track a method's callers and callees.

  The final result is an instance of a profile object which has a hash table of
  thread_data_t, keyed on the thread id.  Each thread in turn has a hash table
  of prof_method_t, keyed on the method id.  A hash table is used for quick 
  look up when doing a profile.  However, it is exposed to Ruby as an array.

  Each prof_method_t has two hash tables, parent and children, of prof_call_info_t.
  These objects keep track of a method's callers (who called the method) and its
  callees (who the method called).  These are keyed the method id, but once again,
  are exposed to Ruby as arrays.  Each prof_call_into_t maintains a pointer to the
  caller or callee method, thereby making it easy to navigate through the call
  hierarchy in ruby - which is very helpful for creating call graphs.
*/

#include "ruby_prof.h"
#include <assert.h>

VALUE mProf;
VALUE cProfile;

static prof_profile_t*
prof_get_profile(VALUE self)
{
    /* Can't use Data_Get_Struct because that triggers the event hook
       ending up in endless recursion. */
    return (prof_profile_t*)RDATA(self)->data;
}

/* support tracing ruby events from ruby-prof. useful for getting at
   what actually happens inside the ruby interpreter (and ruby-prof).
   set environment variable RUBY_PROF_TRACE to filename you want to
   find the trace in.
 */
static FILE* trace_file = NULL;

/* Copied from thread.c (1.9.3) */
static const char *
get_event_name(rb_event_flag_t event)
{
  switch (event) {
  case RUBY_EVENT_LINE:
    return "line";
  case RUBY_EVENT_CLASS:
    return "class";
  case RUBY_EVENT_END:
    return "end";
  case RUBY_EVENT_CALL:
    return "call";
  case RUBY_EVENT_RETURN:
    return "return";
  case RUBY_EVENT_C_CALL:
    return "c-call";
  case RUBY_EVENT_C_RETURN:
    return "c-return";
  case RUBY_EVENT_RAISE:
    return "raise";
  default:
    return "unknown";
  }
}

static prof_method_t*
create_method(rb_event_flag_t event, VALUE klass, ID mid, const char* source_file, int line)
{
    /* Line numbers are not accurate for c method calls */
    if (event == RUBY_EVENT_C_CALL)
    {
		line = 0;
		source_file = NULL;
    }

    return prof_method_create(klass, mid, source_file, line);
}


static prof_method_t*
get_method(rb_event_flag_t event, VALUE klass, ID mid, thread_data_t* thread_data)
{
    prof_method_key_t key;
    prof_method_t *method = NULL;

    method_key(&key, klass, mid);
    method = method_table_lookup(thread_data->method_table, &key);

    if (!method)
    {
 	  const char* source_file = rb_sourcefile();
      int line = rb_sourceline();

	  method = create_method(event, klass, mid, source_file, line);
      method_table_insert(thread_data->method_table, method->key, method);
    }
    return method;
}

static int
pop_frames(st_data_t key, st_data_t value, st_data_t data)
{
    VALUE fiber_id = (VALUE)key;
    thread_data_t* thread_data = (thread_data_t *) value;
    prof_profile_t* profile = (prof_profile_t*) data;
	double measurement = profile->measurer->measure();

    if (!profile->last_thread_data || profile->last_thread_data->fiber_id != fiber_id)
      thread_data = switch_thread(profile, Qnil, fiber_id);
    else
      thread_data = profile->last_thread_data;

    while (prof_stack_pop(thread_data->stack, measurement))
    {
    }

    return ST_CONTINUE;
}

static void
prof_pop_threads(prof_profile_t* profile)
{
    st_foreach(profile->threads_tbl, pop_frames, (st_data_t) profile);
}

/* ===========  Profiling ================= */
static void
prof_trace(prof_profile_t* profile, rb_event_flag_t event, ID mid, VALUE klass, double measurement)
{
    static VALUE last_fiber_id = Qnil;

    VALUE thread = rb_thread_current();
    VALUE thread_id = rb_obj_id(thread);
    VALUE fiber = rb_fiber_current();
    VALUE fiber_id = rb_obj_id(fiber);
    const char* class_name = NULL;
    const char* method_name = rb_id2name(mid);
    const char* source_file = rb_sourcefile();
    unsigned int source_line = rb_sourceline();

    const char* event_name = get_event_name(event);

    if (klass != 0)
        klass = (BUILTIN_TYPE(klass) == T_ICLASS ? RBASIC(klass)->klass : klass);

    class_name = rb_class2name(klass);

    if (last_fiber_id != fiber_id)
    {
        fprintf(trace_file, "\n");
    }

    fprintf(trace_file, "%2lu:%2lu:%2ums %-8s %s:%2d  %s#%s\n",
            (unsigned long) thread_id, (unsigned long) fiber_id, (unsigned int) measurement*1000,
            event_name, source_file, source_line, class_name, method_name);
    fflush(trace_file);
    last_fiber_id = fiber_id;
}

static void
prof_event_hook(rb_event_flag_t event, VALUE data, VALUE self, ID mid, VALUE klass)
{
    prof_profile_t* profile = prof_get_profile(data);
    VALUE thread = Qnil;
    VALUE thread_id = Qnil;
    VALUE fiber = Qnil;
    VALUE fiber_id = Qnil;
    thread_data_t* thread_data = NULL;
    prof_frame_t *frame = NULL;
    double measurement;

    if (event != RUBY_EVENT_C_CALL && event != RUBY_EVENT_C_RETURN) {
        // guess these are already set for C calls in 1.9, then?
        rb_frame_method_id_and_class(&mid, &klass);
    }

    /* Get current measurement */
    measurement = profile->measurer->measure();

    /* Special case - skip any methods from the mProf
       module or cProfile class since they clutter
       the results but aren't important to them results. */
    if (self == mProf || klass == cProfile)
		return;

    if (trace_file != NULL)
    {
        prof_trace(profile, event, mid, klass, measurement);
    }

    /* Get the current thread and fiber information. */
    thread = rb_thread_current();
    thread_id = rb_obj_id(thread);
    fiber = rb_fiber_current();
    fiber_id = rb_obj_id(fiber);

    if (st_lookup(profile->exclude_threads_tbl, (st_data_t) thread_id, 0))
    {
      return;
    }

    /* Was there a context switch? */
    if (!profile->last_thread_data || profile->last_thread_data->fiber_id != fiber_id)
      thread_data = switch_thread(profile, thread_id, fiber_id);
    else
      thread_data = profile->last_thread_data;

    /* Get the current frame for the current thread. */
    frame = prof_stack_peek(thread_data->stack);

    switch (event) {
    case RUBY_EVENT_LINE:
    {
      /* Keep track of the current line number in this method.  When
         a new method is called, we know what line number it was
         called from. */

      if (frame)
      {
        frame->line = rb_sourceline();
        break;
      }

      /* If we get here there was no frame, which means this is
         the first method seen for this thread, so fall through
         to below to create it. */
    }
    case RUBY_EVENT_CALL:
    case RUBY_EVENT_C_CALL:
    {
        prof_call_info_t *call_info = NULL;
        prof_method_t *method = NULL;

        method = get_method(event, klass, mid, thread_data);

        if (!frame)
        {
          call_info = prof_call_info_create(method, NULL);
          prof_add_call_info(method->call_infos, call_info);
        }
        else
        {
          call_info = call_info_table_lookup(frame->call_info->call_infos, method->key);

          if (!call_info)
          {
            /* This call info does not yet exist.  So create it, then add
               it to previous callinfo's children and to the current method .*/
            call_info = prof_call_info_create(method, frame->call_info);
            call_info_table_insert(frame->call_info->call_infos, method->key, call_info);
            prof_add_call_info(method->call_infos, call_info);
          }

          // Unpause the parent frame. If currently paused then:
          // 1) The child frame will begin paused.
          // 2) The parent will inherit the child's dead time.
          prof_frame_unpause(frame, measurement);
        }

        /* Push a new frame onto the stack for a new c-call or ruby call (into a method) */
        frame = prof_stack_push(thread_data->stack, measurement);
        frame->call_info = call_info;
		frame->call_info->depth = frame->depth;
        frame->pause_time = profile->paused == Qtrue ? measurement : -1;
        frame->line = rb_sourceline();
        break;
    }
    case RUBY_EVENT_RETURN:
    case RUBY_EVENT_C_RETURN:
    {
	  prof_stack_pop(thread_data->stack, measurement);
      break;
    }
  }
}

void
prof_install_hook(VALUE self)
{
    rb_add_event_hook(prof_event_hook,
          RUBY_EVENT_CALL | RUBY_EVENT_RETURN |
          RUBY_EVENT_C_CALL | RUBY_EVENT_C_RETURN |
          RUBY_EVENT_LINE, self);
}

void
prof_remove_hook()
{
    rb_remove_event_hook(prof_event_hook);
}

static int
collect_threads(st_data_t key, st_data_t value, st_data_t result)
{
    thread_data_t* thread_data = (thread_data_t*) value;
    VALUE threads_array = (VALUE) result;
	rb_ary_push(threads_array, prof_thread_wrap(thread_data));
    return ST_CONTINUE;
}

/* ========  Profile Class ====== */
static int
mark_threads(st_data_t key, st_data_t value, st_data_t result)
{
    thread_data_t *thread = (thread_data_t *) value;
    prof_thread_mark(thread);
    return ST_CONTINUE;
}

static void
prof_mark(prof_profile_t *profile)
{
	st_foreach(profile->threads_tbl, mark_threads, 0);
}

/* Freeing the profile creates a cascade of freeing.
   It fress the thread table, which frees its methods,
   which frees its call infos. */
static void
prof_free(prof_profile_t *profile)
{
	profile->last_thread_data = NULL;

	threads_table_free(profile->threads_tbl);
    profile->threads_tbl = NULL;

    st_free_table(profile->exclude_threads_tbl);
    profile->exclude_threads_tbl = NULL;

	xfree(profile->measurer);
	profile->measurer = NULL;

    xfree(profile);
}

static VALUE
prof_allocate(VALUE klass)
{
    VALUE result;
    prof_profile_t* profile;
    result = Data_Make_Struct(klass, prof_profile_t, prof_mark, prof_free, profile);
    profile->threads_tbl = threads_table_create();
	profile->exclude_threads_tbl = threads_table_create();
    profile->running = Qfalse;
    return result;
}

/* call-seq:
   RubyProf::Profile.new(mode, exclude_threads) -> instance

   Returns a new profiler.
   
   == Parameters
   mode::  Measure mode (optional). Specifies the profile measure mode.  If not specified, defaults
           to RubyProf::WALL_TIME.
   exclude_threads:: Threads to exclude from the profiling results (optional). */
static VALUE
prof_initialize(int argc,  VALUE *argv, VALUE self)
{
    prof_profile_t* profile = prof_get_profile(self);
    VALUE mode;
    prof_measure_mode_t measurer = MEASURE_WALL_TIME;
    VALUE exclude_threads;
    int i;
    
    switch (rb_scan_args(argc, argv, "02", &mode, &exclude_threads))
    {
      case 0:
      {
        measurer = MEASURE_WALL_TIME;
        exclude_threads = rb_ary_new();
        break;
      }
      case 1:
      {
        measurer = (prof_measure_mode_t)NUM2INT(mode);
        exclude_threads = rb_ary_new();
        break;
      }
      case 2:
      {
        Check_Type(exclude_threads, T_ARRAY);
        measurer = (prof_measure_mode_t)NUM2INT(mode);
        break;
      }
    }

    profile->measurer = prof_get_measurer(measurer);

    for (i = 0; i < RARRAY_LEN(exclude_threads); i++)
    {
        VALUE thread = rb_ary_entry(exclude_threads, i);
        VALUE thread_id = rb_obj_id(thread);
        st_insert(profile->exclude_threads_tbl, thread_id, Qtrue);
    }

    return self;
}

/* call-seq:
   paused? -> boolean

   Returns whether a profile is currently paused.*/
static VALUE
prof_paused(VALUE self)
{
    prof_profile_t* profile = prof_get_profile(self);
    return profile->paused;
}

/* call-seq:
   running? -> boolean

   Returns whether a profile is currently running.*/
static VALUE
prof_running(VALUE self)
{
    prof_profile_t* profile = prof_get_profile(self);
    return profile->running;
}

/* call-seq:
   start -> RubyProf

   Starts recording profile data.*/
static VALUE
prof_start(VALUE self)
{
    char* trace_file_name;

    prof_profile_t* profile = prof_get_profile(self);

    if (profile->running == Qtrue)
    {
        rb_raise(rb_eRuntimeError, "RubyProf.start was already called");
    }

    profile->running = Qtrue;
    profile->paused = Qfalse;
    profile->last_thread_data = NULL;


    /* open trace file if environment wants it */
    trace_file_name = getenv("RUBY_PROF_TRACE");
    if (trace_file_name != NULL) 
    {
      if (strcmp(trace_file_name, "stdout") == 0) 
      {
        trace_file = stdout;
      } 
      else if (strcmp(trace_file_name, "stderr") == 0)
      {
        trace_file = stderr;
      }
      else 
      {
        trace_file = fopen(trace_file_name, "w");
      }
    }

    prof_install_hook(self);
    return self;
}

/* call-seq:
   pause -> RubyProf

   Pauses collecting profile data. */
static VALUE
prof_pause(VALUE self)
{
    prof_profile_t* profile = prof_get_profile(self);
    if (profile->running == Qfalse)
    {
        rb_raise(rb_eRuntimeError, "RubyProf is not running.");
    }

    if (profile->paused == Qfalse)
    {
        profile->paused = Qtrue;
        profile->measurement_at_pause_resume = profile->measurer->measure();
        st_foreach(profile->threads_tbl, pause_thread, (st_data_t) profile);
    }

    return self;
}

/* call-seq:
   resume {block} -> RubyProf

   Resumes recording profile data.*/
static VALUE
prof_resume(VALUE self)
{
    prof_profile_t* profile = prof_get_profile(self);
    if (profile->running == Qfalse)
    {
        rb_raise(rb_eRuntimeError, "RubyProf is not running.");
    }

    if (profile->paused == Qtrue)
    {
        profile->paused = Qfalse;
        profile->measurement_at_pause_resume = profile->measurer->measure();
        st_foreach(profile->threads_tbl, unpause_thread, (st_data_t) profile);
    }

    return rb_block_given_p() ? rb_ensure(rb_yield, self, prof_pause, self) : self;
}

/* call-seq:
   stop -> self

   Stops collecting profile data.*/
static VALUE
prof_stop(VALUE self)
{
    prof_profile_t* profile = prof_get_profile(self);

    if (profile->running == Qfalse)
    {
        rb_raise(rb_eRuntimeError, "RubyProf.start was not yet called");
    }
  
    prof_remove_hook();

    /* close trace file if open */
    if (trace_file != NULL) 
    {
      if (trace_file !=stderr && trace_file != stdout)
      {
#ifdef _MSC_VER
          _fcloseall();
#else
        fclose(trace_file);
#endif
      }
      trace_file = NULL;
    }
    
    prof_pop_threads(profile);

    /* Unset the last_thread_data (very important!)
       and the threads table */
    profile->running = profile->paused = Qfalse;
    profile->last_thread_data = NULL;

    /* Post process result */
    rb_funcall(self, rb_intern("post_process") , 0);

    return self;
}

/* call-seq:
   profile {block} -> RubyProf::Result

Profiles the specified block and returns a RubyProf::Result object. */
static VALUE
prof_profile(int argc,  VALUE *argv, VALUE klass)
{
    int result;
    VALUE profile = rb_class_new_instance(argc, argv, cProfile);

    if (!rb_block_given_p())
    {
        rb_raise(rb_eArgError, "A block must be provided to the profile method.");
    }

    prof_start(profile);
    rb_protect(rb_yield, profile, &result);
    return prof_stop(profile);
}

/* call-seq:
   threads -> Array of RubyProf::Thread

Returns an array of RubyProf::Thread instances that were executed
while the the program was being run. */
static VALUE
prof_threads(VALUE self)
{
	VALUE result = rb_ary_new();
    prof_profile_t* profile = prof_get_profile(self);
    st_foreach(profile->threads_tbl, collect_threads, result);
    return result;
}

void Init_ruby_prof()
{
    mProf = rb_define_module("RubyProf");

    rp_init_measure();
    rp_init_method_info();
    rp_init_call_info();
    rp_init_thread();

    cProfile = rb_define_class_under(mProf, "Profile", rb_cObject);
    rb_define_singleton_method(cProfile, "profile", prof_profile, -1);
    rb_define_alloc_func (cProfile, prof_allocate);
    rb_define_method(cProfile, "initialize", prof_initialize, -1);
    rb_define_method(cProfile, "start", prof_start, 0);
    rb_define_method(cProfile, "stop", prof_stop, 0);
    rb_define_method(cProfile, "resume", prof_resume, 0);
    rb_define_method(cProfile, "pause", prof_pause, 0);
    rb_define_method(cProfile, "running?", prof_running, 0);
    rb_define_method(cProfile, "paused?", prof_paused, 0);
    rb_define_method(cProfile, "threads", prof_threads, 0);
}
