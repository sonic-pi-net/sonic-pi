#include "clar_libgit2.h"

void test_core_errors__public_api(void)
{
	char *str_in_error;

	giterr_clear();
	cl_assert(giterr_last() == NULL);

	giterr_set_oom();

	cl_assert(giterr_last() != NULL);
	cl_assert(giterr_last()->klass == GITERR_NOMEMORY);
	str_in_error = strstr(giterr_last()->message, "memory");
	cl_assert(str_in_error != NULL);

	giterr_clear();

	giterr_set_str(GITERR_REPOSITORY, "This is a test");

	cl_assert(giterr_last() != NULL);
	str_in_error = strstr(giterr_last()->message, "This is a test");
	cl_assert(str_in_error != NULL);

	giterr_clear();
	cl_assert(giterr_last() == NULL);
}

#include "common.h"
#include "util.h"
#include "posix.h"

void test_core_errors__new_school(void)
{
	char *str_in_error;

	giterr_clear();
	cl_assert(giterr_last() == NULL);

	giterr_set_oom(); /* internal fn */

	cl_assert(giterr_last() != NULL);
	cl_assert(giterr_last()->klass == GITERR_NOMEMORY);
	str_in_error = strstr(giterr_last()->message, "memory");
	cl_assert(str_in_error != NULL);

	giterr_clear();

	giterr_set(GITERR_REPOSITORY, "This is a test"); /* internal fn */

	cl_assert(giterr_last() != NULL);
	str_in_error = strstr(giterr_last()->message, "This is a test");
	cl_assert(str_in_error != NULL);

	giterr_clear();
	cl_assert(giterr_last() == NULL);

	do {
		struct stat st;
		memset(&st, 0, sizeof(st));
		cl_assert(p_lstat("this_file_does_not_exist", &st) < 0);
		GIT_UNUSED(st);
	} while (false);
	giterr_set(GITERR_OS, "stat failed"); /* internal fn */

	cl_assert(giterr_last() != NULL);
	str_in_error = strstr(giterr_last()->message, "stat failed");
	cl_assert(str_in_error != NULL);
	cl_assert(git__prefixcmp(str_in_error, "stat failed: ") == 0);
	cl_assert(strlen(str_in_error) > strlen("stat failed: "));

#ifdef GIT_WIN32
	giterr_clear();

	/* The MSDN docs use this to generate a sample error */
	cl_assert(GetProcessId(NULL) == 0);
	giterr_set(GITERR_OS, "GetProcessId failed"); /* internal fn */

	cl_assert(giterr_last() != NULL);
	str_in_error = strstr(giterr_last()->message, "GetProcessId failed");
	cl_assert(str_in_error != NULL);
	cl_assert(git__prefixcmp(str_in_error, "GetProcessId failed: ") == 0);
	cl_assert(strlen(str_in_error) > strlen("GetProcessId failed: "));
#endif

	giterr_clear();
}

void test_core_errors__restore(void)
{
	git_error_state err_state = {0};

	giterr_clear();
	cl_assert(giterr_last() == NULL);

	cl_assert_equal_i(0, giterr_state_capture(&err_state, 0));

	memset(&err_state, 0x0, sizeof(git_error_state));

	giterr_set(42, "Foo: %s", "bar");
	cl_assert_equal_i(-1, giterr_state_capture(&err_state, -1));

	cl_assert(giterr_last() == NULL);

	giterr_set(99, "Bar: %s", "foo");

	giterr_state_restore(&err_state);

	cl_assert_equal_i(42, giterr_last()->klass);
	cl_assert_equal_s("Foo: bar", giterr_last()->message);
}

void test_core_errors__free_state(void)
{
	git_error_state err_state = {0};

	giterr_clear();

	giterr_set(42, "Foo: %s", "bar");
	cl_assert_equal_i(-1, giterr_state_capture(&err_state, -1));

	giterr_set(99, "Bar: %s", "foo");

	giterr_state_free(&err_state);

	cl_assert_equal_i(99, giterr_last()->klass);
	cl_assert_equal_s("Bar: foo", giterr_last()->message);

	giterr_state_restore(&err_state);

	cl_assert(giterr_last() == NULL);
}

void test_core_errors__restore_oom(void)
{
	git_error_state err_state = {0};
	const git_error *oom_error = NULL;

	giterr_clear();

	giterr_set_oom(); /* internal fn */
	oom_error = giterr_last();
	cl_assert(oom_error);

	cl_assert_equal_i(-1, giterr_state_capture(&err_state, -1));

	cl_assert(giterr_last() == NULL);
	cl_assert_equal_i(GITERR_NOMEMORY, err_state.error_msg.klass);
	cl_assert_equal_s("Out of memory", err_state.error_msg.message);

	giterr_state_restore(&err_state);

	cl_assert(giterr_last()->klass == GITERR_NOMEMORY);
	cl_assert_(giterr_last() == oom_error, "static oom error not restored");

	giterr_clear();
}

static int test_arraysize_multiply(size_t nelem, size_t size)
{
	size_t out;
	GITERR_CHECK_ALLOC_MULTIPLY(&out, nelem, size);
	return 0;
}

void test_core_errors__integer_overflow_alloc_multiply(void)
{
	cl_git_pass(test_arraysize_multiply(10, 10));
	cl_git_pass(test_arraysize_multiply(1000, 1000));
	cl_git_pass(test_arraysize_multiply(SIZE_MAX/sizeof(void *), sizeof(void *)));
	cl_git_pass(test_arraysize_multiply(0, 10));
	cl_git_pass(test_arraysize_multiply(10, 0));

	cl_git_fail(test_arraysize_multiply(SIZE_MAX-1, sizeof(void *)));
	cl_git_fail(test_arraysize_multiply((SIZE_MAX/sizeof(void *))+1, sizeof(void *)));

	cl_assert_equal_i(GITERR_NOMEMORY, giterr_last()->klass);
	cl_assert_equal_s("Out of memory", giterr_last()->message);
}

static int test_arraysize_add(size_t one, size_t two)
{
	size_t out;
	GITERR_CHECK_ALLOC_ADD(&out, one, two);
	return 0;
}

void test_core_errors__integer_overflow_alloc_add(void)
{
	cl_git_pass(test_arraysize_add(10, 10));
	cl_git_pass(test_arraysize_add(1000, 1000));
	cl_git_pass(test_arraysize_add(SIZE_MAX-10, 10));

	cl_git_fail(test_arraysize_multiply(SIZE_MAX-1, 2));
	cl_git_fail(test_arraysize_multiply(SIZE_MAX, SIZE_MAX));

	cl_assert_equal_i(GITERR_NOMEMORY, giterr_last()->klass);
	cl_assert_equal_s("Out of memory", giterr_last()->message);
}

void test_core_errors__integer_overflow_sets_oom(void)
{
	size_t out;

	giterr_clear();
	cl_assert(!GIT_ADD_SIZET_OVERFLOW(&out, SIZE_MAX-1, 1));
	cl_assert_equal_p(NULL, giterr_last());

	giterr_clear();
	cl_assert(!GIT_ADD_SIZET_OVERFLOW(&out, 42, 69));
	cl_assert_equal_p(NULL, giterr_last());

	giterr_clear();
	cl_assert(GIT_ADD_SIZET_OVERFLOW(&out, SIZE_MAX, SIZE_MAX));
	cl_assert_equal_i(GITERR_NOMEMORY, giterr_last()->klass);
	cl_assert_equal_s("Out of memory", giterr_last()->message);

	giterr_clear();
	cl_assert(GIT_ADD_SIZET_OVERFLOW(&out, SIZE_MAX, SIZE_MAX));
	cl_assert_equal_i(GITERR_NOMEMORY, giterr_last()->klass);
	cl_assert_equal_s("Out of memory", giterr_last()->message);
}
