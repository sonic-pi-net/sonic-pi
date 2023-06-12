#include "clar_libgit2.h"
#include "posix.h"
#include "blob.h"
#include "filter.h"
#include "git2/sys/filter.h"
#include "git2/sys/repository.h"
#include "custom_helpers.h"

static git_repository *g_repo = NULL;

static git_filter *create_wildcard_filter(void);

#define DATA_LEN 32

static unsigned char input[] = {
	0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
	0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
	0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
	0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
};

static unsigned char reversed[] = {
	0x1f, 0x1e, 0x1d, 0x1c, 0x1b, 0x1a, 0x19, 0x18,
	0x17, 0x16, 0x15, 0x14, 0x13, 0x12, 0x11, 0x10,
	0x0f, 0x0e, 0x0d, 0x0c, 0x0b, 0x0a, 0x09, 0x08,
	0x07, 0x06, 0x05, 0x04, 0x03, 0x02, 0x01, 0x00,
};

static unsigned char flipped[] = {
	0xff, 0xfe, 0xfd, 0xfc, 0xfb, 0xfa, 0xf9, 0xf8,
	0xf7, 0xf6, 0xf5, 0xf4, 0xf3, 0xf2, 0xf1, 0xf0,
	0xef, 0xee, 0xed, 0xec, 0xeb, 0xea, 0xe9, 0xe8,
	0xe7, 0xe6, 0xe5, 0xe4, 0xe3, 0xe2, 0xe1, 0xe0,
};

void test_filter_wildcard__initialize(void)
{
	cl_git_pass(git_filter_register(
		"wildcard", create_wildcard_filter(), GIT_FILTER_DRIVER_PRIORITY));

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_rewritefile(
		"empty_standard_repo/.gitattributes",
		"* binary\n"
		"hero-flip-* filter=wcflip\n"
		"hero-reverse-* filter=wcreverse\n"
		"none-* filter=unregistered\n");
}

void test_filter_wildcard__cleanup(void)
{
	cl_git_pass(git_filter_unregister("wildcard"));

	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

static int wildcard_filter_check(
	git_filter  *self,
	void **payload,
	const git_filter_source *src,
	const char **attr_values)
{
	GIT_UNUSED(self);
	GIT_UNUSED(src);

	if (strcmp(attr_values[0], "wcflip") == 0 ||
		strcmp(attr_values[0], "wcreverse") == 0) {
		*payload = git__strdup(attr_values[0]);
		GIT_ERROR_CHECK_ALLOC(*payload);
		return 0;
	}

	return GIT_PASSTHROUGH;
}

static int wildcard_filter_apply(
	git_filter     *self,
	void          **payload,
	git_str        *to,
	const git_str  *from,
	const git_filter_source *source)
{
	const char *filtername = *payload;

	if (filtername && strcmp(filtername, "wcflip") == 0)
		return bitflip_filter_apply(self, payload, to, from, source);
	else if (filtername && strcmp(filtername, "wcreverse") == 0)
		return reverse_filter_apply(self, payload, to, from, source);

	cl_fail("Unexpected attribute");
	return GIT_PASSTHROUGH;
}

static int wildcard_filter_stream(
	git_writestream **out,
	git_filter *self,
	void **payload,
	const git_filter_source *src,
	git_writestream *next)
{
	return git_filter_buffered_stream_new(out,
		self, wildcard_filter_apply, NULL, payload, src, next);
}

static void wildcard_filter_cleanup(git_filter *self, void *payload)
{
	GIT_UNUSED(self);
	git__free(payload);
}

static void wildcard_filter_free(git_filter *f)
{
	git__free(f);
}

static git_filter *create_wildcard_filter(void)
{
	git_filter *filter = git__calloc(1, sizeof(git_filter));
	cl_assert(filter);

	filter->version = GIT_FILTER_VERSION;
	filter->attributes = "filter=*";
	filter->check = wildcard_filter_check;
	filter->stream = wildcard_filter_stream;
	filter->cleanup = wildcard_filter_cleanup;
	filter->shutdown = wildcard_filter_free;

	return filter;
}

void test_filter_wildcard__reverse(void)
{
	git_filter_list *fl;
	git_buf out = GIT_BUF_INIT;

	cl_git_pass(git_filter_list_load(
		&fl, g_repo, NULL, "hero-reverse-foo", GIT_FILTER_TO_ODB, 0));

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, (char *)input, DATA_LEN));

	cl_assert_equal_i(DATA_LEN, out.size);

	cl_assert_equal_i(
		0, memcmp(reversed, out.ptr, out.size));

	git_filter_list_free(fl);
	git_buf_dispose(&out);
}

void test_filter_wildcard__flip(void)
{
	git_filter_list *fl;
	git_buf out = GIT_BUF_INIT;

	cl_git_pass(git_filter_list_load(
		&fl, g_repo, NULL, "hero-flip-foo", GIT_FILTER_TO_ODB, 0));

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, (char *)input, DATA_LEN));

	cl_assert_equal_i(DATA_LEN, out.size);

	cl_assert_equal_i(
		0, memcmp(flipped, out.ptr, out.size));

	git_filter_list_free(fl);
	git_buf_dispose(&out);
}

void test_filter_wildcard__none(void)
{
	git_filter_list *fl;
	git_buf out = GIT_BUF_INIT;

	cl_git_pass(git_filter_list_load(
		&fl, g_repo, NULL, "none-foo", GIT_FILTER_TO_ODB, 0));

	cl_git_pass(git_filter_list_apply_to_buffer(&out, fl, (char *)input, DATA_LEN));

	cl_assert_equal_i(DATA_LEN, out.size);

	cl_assert_equal_i(
		0, memcmp(input, out.ptr, out.size));

	git_filter_list_free(fl);
	git_buf_dispose(&out);
}
