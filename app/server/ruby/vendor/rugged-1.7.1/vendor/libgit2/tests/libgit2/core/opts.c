#include "clar_libgit2.h"
#include "cache.h"

void test_core_opts__cleanup(void)
{
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_EXTENSIONS, NULL, 0));
}

void test_core_opts__readwrite(void)
{
	size_t old_val = 0;
	size_t new_val = 0;

	git_libgit2_opts(GIT_OPT_GET_MWINDOW_SIZE, &old_val);
	git_libgit2_opts(GIT_OPT_SET_MWINDOW_SIZE, (size_t)1234);
	git_libgit2_opts(GIT_OPT_GET_MWINDOW_SIZE, &new_val);

	cl_assert(new_val == 1234);

	git_libgit2_opts(GIT_OPT_SET_MWINDOW_SIZE, old_val);
	git_libgit2_opts(GIT_OPT_GET_MWINDOW_SIZE, &new_val);

	cl_assert(new_val == old_val);
}

void test_core_opts__invalid_option(void)
{
	cl_git_fail(git_libgit2_opts(-1, "foobar"));
}

void test_core_opts__extensions_query(void)
{
	git_strarray out = { 0 };

	cl_git_pass(git_libgit2_opts(GIT_OPT_GET_EXTENSIONS, &out));

	cl_assert_equal_sz(out.count, 2);
	cl_assert_equal_s("noop", out.strings[0]);
	cl_assert_equal_s("objectformat", out.strings[1]);

	git_strarray_dispose(&out);
}

void test_core_opts__extensions_add(void)
{
	const char *in[] = { "foo" };
	git_strarray out = { 0 };

	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_EXTENSIONS, in, ARRAY_SIZE(in)));
	cl_git_pass(git_libgit2_opts(GIT_OPT_GET_EXTENSIONS, &out));

	cl_assert_equal_sz(out.count, 3);
	cl_assert_equal_s("foo", out.strings[0]);
	cl_assert_equal_s("noop", out.strings[1]);
	cl_assert_equal_s("objectformat", out.strings[2]);

	git_strarray_dispose(&out);
}

void test_core_opts__extensions_remove(void)
{
	const char *in[] = { "bar", "!negate", "!noop", "baz" };
	git_strarray out = { 0 };

	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_EXTENSIONS, in, ARRAY_SIZE(in)));
	cl_git_pass(git_libgit2_opts(GIT_OPT_GET_EXTENSIONS, &out));

	cl_assert_equal_sz(out.count, 3);
	cl_assert_equal_s("bar", out.strings[0]);
	cl_assert_equal_s("baz", out.strings[1]);
	cl_assert_equal_s("objectformat", out.strings[2]);

	git_strarray_dispose(&out);
}

void test_core_opts__extensions_uniq(void)
{
	const char *in[] = { "foo", "noop", "bar", "bar", "foo", "objectformat" };
	git_strarray out = { 0 };

	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_EXTENSIONS, in, ARRAY_SIZE(in)));
	cl_git_pass(git_libgit2_opts(GIT_OPT_GET_EXTENSIONS, &out));

	cl_assert_equal_sz(out.count, 4);
	cl_assert_equal_s("bar", out.strings[0]);
	cl_assert_equal_s("foo", out.strings[1]);
	cl_assert_equal_s("noop", out.strings[2]);
	cl_assert_equal_s("objectformat", out.strings[3]);

	git_strarray_dispose(&out);
}
