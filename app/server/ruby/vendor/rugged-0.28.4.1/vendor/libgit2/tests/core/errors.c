#include "clar_libgit2.h"

void test_core_errors__public_api(void)
{
	char *str_in_error;

	git_error_clear();
	cl_assert(git_error_last() == NULL);

	git_error_set_oom();

	cl_assert(git_error_last() != NULL);
	cl_assert(git_error_last()->klass == GIT_ERROR_NOMEMORY);
	str_in_error = strstr(git_error_last()->message, "memory");
	cl_assert(str_in_error != NULL);

	git_error_clear();

	git_error_set_str(GIT_ERROR_REPOSITORY, "This is a test");

	cl_assert(git_error_last() != NULL);
	str_in_error = strstr(git_error_last()->message, "This is a test");
	cl_assert(str_in_error != NULL);

	git_error_clear();
	cl_assert(git_error_last() == NULL);
}

#include "common.h"
#include "util.h"
#include "posix.h"

void test_core_errors__new_school(void)
{
	char *str_in_error;

	git_error_clear();
	cl_assert(git_error_last() == NULL);

	git_error_set_oom(); /* internal fn */

	cl_assert(git_error_last() != NULL);
	cl_assert(git_error_last()->klass == GIT_ERROR_NOMEMORY);
	str_in_error = strstr(git_error_last()->message, "memory");
	cl_assert(str_in_error != NULL);

	git_error_clear();

	git_error_set(GIT_ERROR_REPOSITORY, "This is a test"); /* internal fn */

	cl_assert(git_error_last() != NULL);
	str_in_error = strstr(git_error_last()->message, "This is a test");
	cl_assert(str_in_error != NULL);

	git_error_clear();
	cl_assert(git_error_last() == NULL);

	do {
		struct stat st;
		memset(&st, 0, sizeof(st));
		cl_assert(p_lstat("this_file_does_not_exist", &st) < 0);
		GIT_UNUSED(st);
	} while (false);
	git_error_set(GIT_ERROR_OS, "stat failed"); /* internal fn */

	cl_assert(git_error_last() != NULL);
	str_in_error = strstr(git_error_last()->message, "stat failed");
	cl_assert(str_in_error != NULL);
	cl_assert(git__prefixcmp(str_in_error, "stat failed: ") == 0);
	cl_assert(strlen(str_in_error) > strlen("stat failed: "));

#ifdef GIT_WIN32
	git_error_clear();

	/* The MSDN docs use this to generate a sample error */
	cl_assert(GetProcessId(NULL) == 0);
	git_error_set(GIT_ERROR_OS, "GetProcessId failed"); /* internal fn */

	cl_assert(git_error_last() != NULL);
	str_in_error = strstr(git_error_last()->message, "GetProcessId failed");
	cl_assert(str_in_error != NULL);
	cl_assert(git__prefixcmp(str_in_error, "GetProcessId failed: ") == 0);
	cl_assert(strlen(str_in_error) > strlen("GetProcessId failed: "));
#endif

	git_error_clear();
}

void test_core_errors__restore(void)
{
	git_error_state err_state = {0};

	git_error_clear();
	cl_assert(git_error_last() == NULL);

	cl_assert_equal_i(0, git_error_state_capture(&err_state, 0));

	memset(&err_state, 0x0, sizeof(git_error_state));

	git_error_set(42, "Foo: %s", "bar");
	cl_assert_equal_i(-1, git_error_state_capture(&err_state, -1));

	cl_assert(git_error_last() == NULL);

	git_error_set(99, "Bar: %s", "foo");

	git_error_state_restore(&err_state);

	cl_assert_equal_i(42, git_error_last()->klass);
	cl_assert_equal_s("Foo: bar", git_error_last()->message);
}

void test_core_errors__free_state(void)
{
	git_error_state err_state = {0};

	git_error_clear();

	git_error_set(42, "Foo: %s", "bar");
	cl_assert_equal_i(-1, git_error_state_capture(&err_state, -1));

	git_error_set(99, "Bar: %s", "foo");

	git_error_state_free(&err_state);

	cl_assert_equal_i(99, git_error_last()->klass);
	cl_assert_equal_s("Bar: foo", git_error_last()->message);

	git_error_state_restore(&err_state);

	cl_assert(git_error_last() == NULL);
}

void test_core_errors__restore_oom(void)
{
	git_error_state err_state = {0};
	const git_error *oom_error = NULL;

	git_error_clear();

	git_error_set_oom(); /* internal fn */
	oom_error = git_error_last();
	cl_assert(oom_error);

	cl_assert_equal_i(-1, git_error_state_capture(&err_state, -1));

	cl_assert(git_error_last() == NULL);
	cl_assert_equal_i(GIT_ERROR_NOMEMORY, err_state.error_msg.klass);
	cl_assert_equal_s("Out of memory", err_state.error_msg.message);

	git_error_state_restore(&err_state);

	cl_assert(git_error_last()->klass == GIT_ERROR_NOMEMORY);
	cl_assert_(git_error_last() == oom_error, "static oom error not restored");

	git_error_clear();
}

static int test_arraysize_multiply(size_t nelem, size_t size)
{
	size_t out;
	GIT_ERROR_CHECK_ALLOC_MULTIPLY(&out, nelem, size);
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

	cl_assert_equal_i(GIT_ERROR_NOMEMORY, git_error_last()->klass);
	cl_assert_equal_s("Out of memory", git_error_last()->message);
}

static int test_arraysize_add(size_t one, size_t two)
{
	size_t out;
	GIT_ERROR_CHECK_ALLOC_ADD(&out, one, two);
	return 0;
}

void test_core_errors__integer_overflow_alloc_add(void)
{
	cl_git_pass(test_arraysize_add(10, 10));
	cl_git_pass(test_arraysize_add(1000, 1000));
	cl_git_pass(test_arraysize_add(SIZE_MAX-10, 10));

	cl_git_fail(test_arraysize_multiply(SIZE_MAX-1, 2));
	cl_git_fail(test_arraysize_multiply(SIZE_MAX, SIZE_MAX));

	cl_assert_equal_i(GIT_ERROR_NOMEMORY, git_error_last()->klass);
	cl_assert_equal_s("Out of memory", git_error_last()->message);
}

void test_core_errors__integer_overflow_sets_oom(void)
{
	size_t out;

	git_error_clear();
	cl_assert(!GIT_ADD_SIZET_OVERFLOW(&out, SIZE_MAX-1, 1));
	cl_assert_equal_p(NULL, git_error_last());

	git_error_clear();
	cl_assert(!GIT_ADD_SIZET_OVERFLOW(&out, 42, 69));
	cl_assert_equal_p(NULL, git_error_last());

	git_error_clear();
	cl_assert(GIT_ADD_SIZET_OVERFLOW(&out, SIZE_MAX, SIZE_MAX));
	cl_assert_equal_i(GIT_ERROR_NOMEMORY, git_error_last()->klass);
	cl_assert_equal_s("Out of memory", git_error_last()->message);

	git_error_clear();
	cl_assert(GIT_ADD_SIZET_OVERFLOW(&out, SIZE_MAX, SIZE_MAX));
	cl_assert_equal_i(GIT_ERROR_NOMEMORY, git_error_last()->klass);
	cl_assert_equal_s("Out of memory", git_error_last()->message);
}
