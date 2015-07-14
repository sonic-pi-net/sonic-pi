/* Copyright (C) 2005-2013 Shugo Maeda <shugo@ruby-lang.org> and Charlie Savage <cfis@savagexi.com>
   Please see the LICENSE file for copyright and distribution information */

#include "ruby_prof.h"

#define INITIAL_CALL_INFOS_SIZE 2

VALUE cCallInfo;


// Forward declarations
st_table * call_info_table_create();


/* =======  prof_call_info_t   ========*/
prof_call_info_t *
prof_call_info_create(prof_method_t* method, prof_call_info_t* parent)
{
    prof_call_info_t *result = ALLOC(prof_call_info_t);
    result->object = Qnil;
    result->target = method;
    result->parent = parent;
    result->call_infos = call_info_table_create();
    result->children = Qnil;

    result->called = 0;
    result->total_time = 0;
    result->self_time = 0;
    result->wait_time = 0;
    result->line = 0;
    return result;
}
static void
prof_call_info_ruby_gc_free(prof_call_info_t *call_info)
{
	/* Has this thread object been accessed by Ruby?  If
	   yes clean it up so to avoid a segmentation fault. */
	if (call_info->object != Qnil)
	{
		RDATA(call_info->object)->data = NULL;
		RDATA(call_info->object)->dfree = NULL;
		RDATA(call_info->object)->dmark = NULL;
    }
	call_info->object = Qnil;
}

static void
prof_call_info_free(prof_call_info_t *call_info)
{
	prof_call_info_ruby_gc_free(call_info);
	st_free_table(call_info->call_infos);
	xfree(call_info);
}

static void
prof_call_info_mark(prof_call_info_t *call_info)
{
	if (call_info->object)
		rb_gc_mark(call_info->object);

	if (call_info->children)
		rb_gc_mark(call_info->children);

	/* We don't mark the call info child table since that will be done
	   via the appropriate method */
}

VALUE
prof_call_info_wrap(prof_call_info_t *call_info)
{
  if (call_info->object == Qnil)
  {
    call_info->object = Data_Wrap_Struct(cCallInfo, prof_call_info_mark, prof_call_info_ruby_gc_free, call_info);
  }
  return call_info->object;
}

static prof_call_info_t *
prof_get_call_info(VALUE self)
{
    /* Can't use Data_Get_Struct because that triggers the event hook
       ending up in endless recursion. */
	prof_call_info_t* result = DATA_PTR(self);

	if (!result)
	    rb_raise(rb_eRuntimeError, "This RubyProf::CallInfo instance has already been freed, likely because its profile has been freed.");

   return result;
}

/* =======  Call Info Table   ========*/
st_table *
call_info_table_create()
{
  return st_init_table(&type_method_hash);
}

size_t
call_info_table_insert(st_table *table, const prof_method_key_t *key, prof_call_info_t *val)
{
  return st_insert(table, (st_data_t) key, (st_data_t) val);
}

prof_call_info_t *
call_info_table_lookup(st_table *table, const prof_method_key_t *key)
{
    st_data_t val;
    if (st_lookup(table, (st_data_t) key, &val))
    {
      return (prof_call_info_t *) val;
    }
    else
    {
      return NULL;
    }
}


/* =======  RubyProf::CallInfo   ========*/

/* Document-class: RubyProf::CallInfo
RubyProf::CallInfo is a helper class used by RubyProf::MethodInfo
to keep track of which child methods were called and how long
they took to execute. */


/* call-seq:
   called -> MethodInfo

Returns the target method. */
static VALUE
prof_call_info_target(VALUE self)
{
    /* Target is a pointer to a method_info - so we have to be careful
       about the GC.  We will wrap the method_info but provide no
       free method so the underlying object is not freed twice! */

    prof_call_info_t *result = prof_get_call_info(self);
    return prof_method_wrap(result->target);
}

/* call-seq:
   called -> int

Returns the total amount of times this method was called. */
static VALUE
prof_call_info_called(VALUE self)
{
    prof_call_info_t *result = prof_get_call_info(self);
    return INT2NUM(result->called);
}

/* call-seq:
   called=n -> n

Sets the call count to n. */
static VALUE
prof_call_info_set_called(VALUE self, VALUE called)
{
    prof_call_info_t *result = prof_get_call_info(self);
    result->called = NUM2INT(called);
    return called;
}

/* call-seq:
   depth -> int

   returns the depth of this call info in the call graph */
static VALUE
prof_call_info_depth(VALUE self)
{
  prof_call_info_t *result = prof_get_call_info(self);
  return rb_int_new(result->depth);
}

/* call-seq:
   line_no -> int

   returns the line number of the method */
static VALUE
prof_call_info_line(VALUE self)
{
  prof_call_info_t *result = prof_get_call_info(self);
  return rb_int_new(result->line);
}

/* call-seq:
   total_time -> float

Returns the total amount of time spent in this method and its children. */
static VALUE
prof_call_info_total_time(VALUE self)
{
    prof_call_info_t *result = prof_get_call_info(self);
    return rb_float_new(result->total_time);
}

/* call-seq:
   add_total_time(call_info) -> nil

adds total time time from call_info to self. */
static VALUE
prof_call_info_add_total_time(VALUE self, VALUE other)
{
    prof_call_info_t *result = prof_get_call_info(self);
    prof_call_info_t *other_info = prof_get_call_info(other);

    result->total_time += other_info->total_time;
    return Qnil;
}

/* call-seq:
   self_time -> float

Returns the total amount of time spent in this method. */
static VALUE
prof_call_info_self_time(VALUE self)
{
    prof_call_info_t *result = prof_get_call_info(self);

    return rb_float_new(result->self_time);
}

/* call-seq:
   add_self_time(call_info) -> nil

adds self time from call_info to self. */
static VALUE
prof_call_info_add_self_time(VALUE self, VALUE other)
{
    prof_call_info_t *result = prof_get_call_info(self);
    prof_call_info_t *other_info = prof_get_call_info(other);

    result->self_time += other_info->self_time;
    return Qnil;
}

/* call-seq:
   wait_time -> float

Returns the total amount of time this method waited for other threads. */
static VALUE
prof_call_info_wait_time(VALUE self)
{
    prof_call_info_t *result = prof_get_call_info(self);

    return rb_float_new(result->wait_time);
}

/* call-seq:
   add_wait_time(call_info) -> nil

adds wait time from call_info to self. */

static VALUE
prof_call_info_add_wait_time(VALUE self, VALUE other)
{
    prof_call_info_t *result = prof_get_call_info(self);
    prof_call_info_t *other_info = prof_get_call_info(other);

    result->wait_time += other_info->wait_time;
    return Qnil;
}

/* call-seq:
   parent -> call_info

Returns the call_infos parent call_info object (the method that called this method).*/
static VALUE
prof_call_info_parent(VALUE self)
{
    prof_call_info_t *result = prof_get_call_info(self);
    if (result->parent)
      return prof_call_info_wrap(result->parent);
    else
      return Qnil;
}

/* call-seq:
   parent=new_parent -> new_parent

Changes the parent of self to new_parent and returns it.*/
static VALUE
prof_call_info_set_parent(VALUE self, VALUE new_parent)
{
    prof_call_info_t *result = prof_get_call_info(self);
    if (new_parent == Qnil)
      result->parent = NULL;
    else
      result->parent = prof_get_call_info(new_parent);
    return prof_call_info_parent(self);
}

static int
prof_call_info_collect_children(st_data_t key, st_data_t value, st_data_t result)
{
    prof_call_info_t *call_info = (prof_call_info_t *) value;
    VALUE arr = (VALUE) result;
    rb_ary_push(arr, prof_call_info_wrap(call_info));
    return ST_CONTINUE;
}

/* call-seq:
   children -> hash

Returns an array of call info objects of methods that this method
called (ie, children).*/
static VALUE
prof_call_info_children(VALUE self)
{
    prof_call_info_t *call_info = prof_get_call_info(self);
    if (call_info->children == Qnil)
    {
      call_info->children = rb_ary_new();
      st_foreach(call_info->call_infos, prof_call_info_collect_children, call_info->children);
    }
    return call_info->children;
}

/* =======  Call Infos   ========*/
prof_call_infos_t*
prof_call_infos_create()
{
   prof_call_infos_t *result = ALLOC(prof_call_infos_t);
   result->start = ALLOC_N(prof_call_info_t*, INITIAL_CALL_INFOS_SIZE);
   result->end = result->start + INITIAL_CALL_INFOS_SIZE;
   result->ptr = result->start;
   result->object = Qnil;
   return result;
}

void
prof_call_infos_mark(prof_call_infos_t *call_infos)
{
    prof_call_info_t **call_info;

	if (call_infos->object)
		rb_gc_mark(call_infos->object);

    for(call_info=call_infos->start; call_info<call_infos->ptr; call_info++)
    {
		prof_call_info_mark(*call_info);
    }
}

void
prof_call_infos_free(prof_call_infos_t *call_infos)
{
    prof_call_info_t **call_info;

    for(call_info=call_infos->start; call_info<call_infos->ptr; call_info++)
    {
		prof_call_info_free(*call_info);
    }
}

void
prof_add_call_info(prof_call_infos_t *call_infos, prof_call_info_t *call_info)
{
  if (call_infos->ptr == call_infos->end)
  {
    size_t len = call_infos->ptr - call_infos->start;
    size_t new_capacity = (call_infos->end - call_infos->start) * 2;
    REALLOC_N(call_infos->start, prof_call_info_t*, new_capacity);
    call_infos->ptr = call_infos->start + len;
    call_infos->end = call_infos->start + new_capacity;
  }
  *call_infos->ptr = call_info;
  call_infos->ptr++;
}

VALUE
prof_call_infos_wrap(prof_call_infos_t *call_infos)
{
  if (call_infos->object == Qnil)
  {
    prof_call_info_t **i;
    call_infos->object = rb_ary_new();
    for(i=call_infos->start; i<call_infos->ptr; i++)
    {
      VALUE call_info = prof_call_info_wrap(*i);
      rb_ary_push(call_infos->object, call_info);
    }
  }
  return call_infos->object;
}

void rp_init_call_info()
{
    /* CallInfo */
    cCallInfo = rb_define_class_under(mProf, "CallInfo", rb_cObject);
    rb_undef_method(CLASS_OF(cCallInfo), "new");
    rb_define_method(cCallInfo, "parent", prof_call_info_parent, 0);
    rb_define_method(cCallInfo, "parent=", prof_call_info_set_parent, 1);
    rb_define_method(cCallInfo, "children", prof_call_info_children, 0);
    rb_define_method(cCallInfo, "target", prof_call_info_target, 0);
    rb_define_method(cCallInfo, "called", prof_call_info_called, 0);
    rb_define_method(cCallInfo, "called=", prof_call_info_set_called, 1);
    rb_define_method(cCallInfo, "total_time", prof_call_info_total_time, 0);
    rb_define_method(cCallInfo, "add_total_time", prof_call_info_add_total_time, 1);
    rb_define_method(cCallInfo, "self_time", prof_call_info_self_time, 0);
    rb_define_method(cCallInfo, "add_self_time", prof_call_info_add_self_time, 1);
    rb_define_method(cCallInfo, "wait_time", prof_call_info_wait_time, 0);
    rb_define_method(cCallInfo, "add_wait_time", prof_call_info_add_wait_time, 1);
    rb_define_method(cCallInfo, "depth", prof_call_info_depth, 0);
    rb_define_method(cCallInfo, "line", prof_call_info_line, 0);
}
