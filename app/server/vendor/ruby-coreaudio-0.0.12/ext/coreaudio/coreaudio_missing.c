
#include "ruby.h"
#include "coreaudio.h"

#ifndef HAVE_RB_ALLOC_TMP_BUFFER
void *
rb_alloc_tmp_buffer(volatile VALUE *store, long len)
{
	VALUE s = rb_str_tmp_new(len);
	*store = s;
	return RSTRING_PTR(s);
}
#endif

#ifndef HAVE_RB_FREE_TMP_BUFFER
void
rb_free_tmp_buffer(volatile VALUE *store)
{
    VALUE s = *store;
    *store = 0;
        if (s) rb_str_clear(s);
}
#endif

