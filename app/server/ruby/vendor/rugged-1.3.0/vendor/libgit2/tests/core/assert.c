#ifdef GIT_ASSERT_HARD
# undef GIT_ASSERT_HARD
#endif

#define GIT_ASSERT_HARD 0

#include "clar_libgit2.h"

static const char *hello_world = "hello, world";
static const char *fail = "FAIL";

static int dummy_fn(const char *myarg)
{
	GIT_ASSERT_ARG(myarg);
	GIT_ASSERT_ARG(myarg != hello_world);
	return 0;
}

static const char *fn_returns_string(const char *myarg)
{
	GIT_ASSERT_ARG_WITH_RETVAL(myarg, fail);
	GIT_ASSERT_ARG_WITH_RETVAL(myarg != hello_world, fail);

	return myarg;
}

static int bad_math(void)
{
	GIT_ASSERT(1 + 1 == 3);
	return 42;
}

static const char *bad_returns_string(void)
{
	GIT_ASSERT_WITH_RETVAL(1 + 1 == 3, NULL);
	return hello_world;
}

void test_core_assert__argument(void)
{
	cl_git_fail(dummy_fn(NULL));
	cl_assert(git_error_last());
	cl_assert_equal_i(GIT_ERROR_INVALID, git_error_last()->klass);
	cl_assert_equal_s("invalid argument: 'myarg'", git_error_last()->message);

	cl_git_fail(dummy_fn(hello_world));
	cl_assert(git_error_last());
	cl_assert_equal_i(GIT_ERROR_INVALID, git_error_last()->klass);
	cl_assert_equal_s("invalid argument: 'myarg != hello_world'", git_error_last()->message);

	cl_git_pass(dummy_fn("foo"));
}

void test_core_assert__argument_with_non_int_return_type(void)
{
	const char *foo = "foo";

	cl_assert_equal_p(fail, fn_returns_string(NULL));
	cl_assert_equal_i(GIT_ERROR_INVALID, git_error_last()->klass);
	cl_assert_equal_s("invalid argument: 'myarg'", git_error_last()->message);

	cl_assert_equal_p(fail, fn_returns_string(hello_world));
	cl_assert_equal_i(GIT_ERROR_INVALID, git_error_last()->klass);
	cl_assert_equal_s("invalid argument: 'myarg != hello_world'", git_error_last()->message);

	cl_assert_equal_p(foo, fn_returns_string(foo));
}

void test_core_assert__argument_with_void_return_type(void)
{
	const char *foo = "foo";

	git_error_clear();
	fn_returns_string(hello_world);
	cl_assert_equal_i(GIT_ERROR_INVALID, git_error_last()->klass);
	cl_assert_equal_s("invalid argument: 'myarg != hello_world'", git_error_last()->message);

	git_error_clear();
	cl_assert_equal_p(foo, fn_returns_string(foo));
	cl_assert_equal_p(NULL, git_error_last());
}

void test_core_assert__internal(void)
{
	cl_git_fail(bad_math());
	cl_assert(git_error_last());
	cl_assert_equal_i(GIT_ERROR_INTERNAL, git_error_last()->klass);
	cl_assert_equal_s("unrecoverable internal error: '1 + 1 == 3'", git_error_last()->message);

	cl_assert_equal_p(NULL, bad_returns_string());
	cl_assert(git_error_last());
	cl_assert_equal_i(GIT_ERROR_INTERNAL, git_error_last()->klass);
	cl_assert_equal_s("unrecoverable internal error: '1 + 1 == 3'", git_error_last()->message);
}
