#include "clar_libgit2.h"
#include "posix.h"
#include "filter.h"
#include "buf_text.h"
#include "git2/sys/filter.h"

#define VERY_SECURE_ENCRYPTION(b) ((b) ^ 0xff)

int bitflip_filter_apply(
	git_filter     *self,
	void          **payload,
	git_buf        *to,
	const git_buf  *from,
	const git_filter_source *source)
{
	const unsigned char *src = (const unsigned char *)from->ptr;
	unsigned char *dst;
	size_t i;

	GIT_UNUSED(self); GIT_UNUSED(payload);

	/* verify that attribute path match worked as expected */
	cl_assert_equal_i(
		0, git__strncmp("hero", git_filter_source_path(source), 4));

	if (!from->size)
		return 0;

	cl_git_pass(git_buf_grow(to, from->size));

	dst = (unsigned char *)to->ptr;

	for (i = 0; i < from->size; i++)
		dst[i] = VERY_SECURE_ENCRYPTION(src[i]);

	to->size = from->size;

	return 0;
}

static void bitflip_filter_free(git_filter *f)
{
	git__free(f);
}

git_filter *create_bitflip_filter(void)
{
	git_filter *filter = git__calloc(1, sizeof(git_filter));
	cl_assert(filter);

	filter->version = GIT_FILTER_VERSION;
	filter->attributes = "+bitflip";
	filter->shutdown = bitflip_filter_free;
	filter->apply = bitflip_filter_apply;

	return filter;
}


int reverse_filter_apply(
	git_filter     *self,
	void          **payload,
	git_buf        *to,
	const git_buf  *from,
	const git_filter_source *source)
{
	const unsigned char *src = (const unsigned char *)from->ptr;
	const unsigned char *end = src + from->size;
	unsigned char *dst;

	GIT_UNUSED(self); GIT_UNUSED(payload); GIT_UNUSED(source);

	/* verify that attribute path match worked as expected */
	cl_assert_equal_i(
		0, git__strncmp("hero", git_filter_source_path(source), 4));

	if (!from->size)
		return 0;

	cl_git_pass(git_buf_grow(to, from->size));

	dst = (unsigned char *)to->ptr + from->size - 1;

	while (src < end)
		*dst-- = *src++;

	to->size = from->size;

	return 0;
}

static void reverse_filter_free(git_filter *f)
{
	git__free(f);
}

git_filter *create_reverse_filter(const char *attrs)
{
	git_filter *filter = git__calloc(1, sizeof(git_filter));
	cl_assert(filter);

	filter->version = GIT_FILTER_VERSION;
	filter->attributes = attrs;
	filter->shutdown = reverse_filter_free;
	filter->apply = reverse_filter_apply;

	return filter;
}

int erroneous_filter_stream(
	git_writestream **out,
	git_filter *self,
	void **payload,
	const git_filter_source *src,
	git_writestream *next)
{
	GIT_UNUSED(out);
	GIT_UNUSED(self);
	GIT_UNUSED(payload);
	GIT_UNUSED(src);
	GIT_UNUSED(next);
	return -1;
}

static void erroneous_filter_free(git_filter *f)
{
	git__free(f);
}

git_filter *create_erroneous_filter(const char *attrs)
{
	git_filter *filter = git__calloc(1, sizeof(git_filter));
	cl_assert(filter);

	filter->version = GIT_FILTER_VERSION;
	filter->attributes = attrs;
	filter->stream = erroneous_filter_stream;
	filter->shutdown = erroneous_filter_free;

	return filter;
}
