/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

#include "ruby_prof.h"

VALUE cRpThread;

/* ======   thread_data_t  ====== */
thread_data_t*
thread_data_create()
{
    thread_data_t* result = ALLOC(thread_data_t);
    result->stack = prof_stack_create();
    result->method_table = method_table_create();
	result->object = Qnil;
	result->methods = Qnil;
    return result;
}

/* The underlying c structures are freed when the parent profile is freed.  
   However, on shutdown the Ruby GC frees objects in any will-nilly order.
   That means the ruby thread object wrapping the c thread struct may
   be freed before the parent profile.  Thus we add in a free function
   for the garbage collector so that if it does get called will nil 
   out our Ruby object reference.*/
static void
thread_data_ruby_gc_free(thread_data_t* thread_data)
{
	/* Has this thread object been accessed by Ruby?  If
	  yes clean it up so to avoid a segmentation fault. */
	if (thread_data->object != Qnil)
	{
		RDATA(thread_data->object)->data = NULL;
		RDATA(thread_data->object)->dfree = NULL;
		RDATA(thread_data->object)->dmark = NULL;
    }
	thread_data->object = Qnil;
}

static void
thread_data_free(thread_data_t* thread_data)
{
	thread_data_ruby_gc_free(thread_data);
    method_table_free(thread_data->method_table);
    prof_stack_free(thread_data->stack);

    thread_data->thread_id = Qnil;

	xfree(thread_data);
}

static int
mark_methods(st_data_t key, st_data_t value, st_data_t result)
{
    prof_method_t *method = (prof_method_t *) value;
    prof_method_mark(method);
    return ST_CONTINUE;
}

void
prof_thread_mark(thread_data_t *thread)
{
	if (thread->object != Qnil)
		rb_gc_mark(thread->object);
	
	if (thread->methods != Qnil)
		rb_gc_mark(thread->methods);

	if (thread->thread_id != Qnil)
		rb_gc_mark(thread->thread_id);

	if (thread->fiber_id != Qnil)
		rb_gc_mark(thread->fiber_id);

	st_foreach(thread->method_table, mark_methods, 0);
}

VALUE
prof_thread_wrap(thread_data_t *thread)
{
  if (thread->object == Qnil)
  {
    thread->object = Data_Wrap_Struct(cRpThread, prof_thread_mark, thread_data_ruby_gc_free, thread);
  }
  return thread->object;
}

static thread_data_t*
prof_get_thread(VALUE self)
{
    /* Can't use Data_Get_Struct because that triggers the event hook
       ending up in endless recursion. */
	thread_data_t* result = DATA_PTR(self);
	if (!result)
	    rb_raise(rb_eRuntimeError, "This RubyProf::Thread instance has already been freed, likely because its profile has been freed.");

   return result;
}

/* ======   Thread Table  ====== */
/* The thread table is hash keyed on ruby thread_id that stores instances
   of thread_data_t. */

st_table *
threads_table_create()
{
    return st_init_numtable();
}

static int
thread_table_free_iterator(st_data_t key, st_data_t value, st_data_t dummy)
{
    thread_data_free((thread_data_t*)value);
    return ST_CONTINUE;
}

void
threads_table_free(st_table *table)
{
    st_foreach(table, thread_table_free_iterator, 0);
    st_free_table(table);
}

size_t
threads_table_insert(prof_profile_t* profile, VALUE fiber, thread_data_t *thread_data)
{
    /* Its too slow to key on the real thread id so just typecast thread instead. */
    return st_insert(profile->threads_tbl, (st_data_t) fiber, (st_data_t) thread_data);
}

thread_data_t *
threads_table_lookup(prof_profile_t* profile, VALUE thread_id, VALUE fiber_id)
{
    thread_data_t* result;
    st_data_t val;

    /* Its too slow to key on the real thread id so just typecast thread instead. */
    if (st_lookup(profile->threads_tbl, (st_data_t) fiber_id, &val))
    {
      result = (thread_data_t *) val;
    }
    else
    {
        result = thread_data_create();
        result->thread_id = thread_id;
        result->fiber_id = fiber_id;

        /* Insert the table */
        threads_table_insert(profile, fiber_id, result);
    }
    return result;
}

thread_data_t *
switch_thread(void* prof, VALUE thread_id, VALUE fiber_id)
{
    prof_profile_t* profile = (prof_profile_t*)prof;
    double measurement = profile->measurer->measure();

    /* Get new thread information. */
    thread_data_t *thread_data = threads_table_lookup(profile, thread_id, fiber_id);

    /* Get current frame for this thread */
    prof_frame_t *frame = prof_stack_peek(thread_data->stack);

    /* Update the time this thread waited for another thread */
    if (frame)
    {
        frame->wait_time += measurement - frame->switch_time;
        frame->switch_time = measurement;
    }

    /* Save on the last thread the time of the context switch
       and reset this thread's last context switch to 0.*/
    if (profile->last_thread_data)
    {
       prof_frame_t *last_frame = prof_stack_peek(profile->last_thread_data->stack);
       if (last_frame)
         last_frame->switch_time = measurement;
    }

    profile->last_thread_data = thread_data;
    return thread_data;
}

int pause_thread(st_data_t key, st_data_t value, st_data_t data) 
{
    thread_data_t* thread_data = (thread_data_t *) value;
	prof_profile_t* profile = (prof_profile_t*)data;

    prof_frame_t* frame = prof_stack_peek(thread_data->stack);
	prof_frame_pause(frame, profile->measurement_at_pause_resume);

    return ST_CONTINUE;
}

int unpause_thread(st_data_t key, st_data_t value, st_data_t data) 
{
    thread_data_t* thread_data = (thread_data_t *) value;
	prof_profile_t* profile = (prof_profile_t*)data;

    prof_frame_t* frame = prof_stack_peek(thread_data->stack);
	prof_frame_unpause(frame, profile->measurement_at_pause_resume);

    return ST_CONTINUE;
}

static int
collect_methods(st_data_t key, st_data_t value, st_data_t result)
{
    /* Called for each method stored in a thread's method table.
       We want to store the method info information into an array.*/
    VALUE methods = (VALUE) result;
    prof_method_t *method = (prof_method_t *) value;
    rb_ary_push(methods, prof_method_wrap(method));

    return ST_CONTINUE;
}


/* call-seq:
   id -> number

Returns the id of this thread. */
static VALUE
prof_thread_id(VALUE self)
{
    thread_data_t* thread = prof_get_thread(self);
	return thread->thread_id;
}

/* call-seq:
   fiber_id -> number

Returns the fiber id of this thread. */
static VALUE
prof_fiber_id(VALUE self)
{
    thread_data_t* thread = prof_get_thread(self);
	return thread->fiber_id;
}

/* call-seq:
   methods -> Array of MethodInfo

Returns an array of methods that were called from this
thread during program execution. */
static VALUE
prof_thread_methods(VALUE self)
{
    thread_data_t* thread = prof_get_thread(self);
	if (thread->methods == Qnil)
	{
		thread->methods = rb_ary_new();
	    st_foreach(thread->method_table, collect_methods, thread->methods);
	}
	return thread->methods;
}

void rp_init_thread()
{
    cRpThread = rb_define_class_under(mProf, "Thread", rb_cObject);
    rb_undef_method(CLASS_OF(cRpThread), "new");

    rb_define_method(cRpThread, "id", prof_thread_id, 0);
    rb_define_method(cRpThread, "fiber_id", prof_fiber_id, 0);
    rb_define_method(cRpThread, "methods", prof_thread_methods, 0);
}
