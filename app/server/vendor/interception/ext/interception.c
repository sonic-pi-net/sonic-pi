#include "ruby.h"

static VALUE rb_mInterception;

struct FRAME {
    VALUE self;
    int argc;
    ID last_func;
    ID orig_func;
    VALUE last_class;
    struct FRAME *prev;
    struct FRAME *tmp;
    struct RNode *node;
    int iter;
    int flags;
    unsigned long uniq;
} *ruby_frame;

#ifdef RUBY_18

#include "node.h"

void
interception_hook(rb_event_t event, NODE *node, VALUE self, ID mid, VALUE klass)
{
    VALUE bself = ruby_frame->prev->self;
    if (node == ruby_frame->node) {
        bself = ruby_frame->prev->prev->self;
    }

    VALUE binding = rb_funcall(bself, rb_intern("binding"), 0, NULL);
    rb_funcall(rb_mInterception, rb_intern("rescue"), 2, ruby_errinfo, binding);
}

VALUE
interception_start(VALUE self)
{
    rb_add_event_hook(interception_hook, RUBY_EVENT_RAISE);
    return Qnil;
}

VALUE
interception_stop(VALUE self)
{
    rb_remove_event_hook(interception_hook);
    return Qnil;
}

#elif RUBY_19

void
interception_hook(rb_event_flag_t evflag, VALUE data, VALUE self, ID mid, VALUE klass)
{
    VALUE binding = rb_funcall(rb_mKernel, rb_intern("binding"), 0, NULL);
    rb_funcall(rb_mInterception, rb_intern("rescue"), 2, rb_errinfo(), binding);
}

VALUE
interception_start(VALUE self)
{
    rb_add_event_hook(interception_hook, RUBY_EVENT_RAISE, rb_mInterception);
    return Qnil;
}

VALUE
interception_stop(VALUE self)
{
    rb_remove_event_hook(interception_hook);
    return Qnil;
}
#endif

void
Init_interception()
{
    rb_mInterception = rb_define_module("Interception");

    rb_define_singleton_method(rb_mInterception, "start", interception_start, 0);
    rb_define_singleton_method(rb_mInterception, "stop", interception_stop, 0);
}
