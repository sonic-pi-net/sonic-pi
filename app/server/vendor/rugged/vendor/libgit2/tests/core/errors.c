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

	cl_assert_equal_i(0, giterr_capture(&err_state, 0));

	memset(&err_state, 0x0, sizeof(git_error_state));

	giterr_set(42, "Foo: %s", "bar");
	cl_assert_equal_i(-1, giterr_capture(&err_state, -1));

	cl_assert(giterr_last() == NULL);

	giterr_set(99, "Bar: %s", "foo");

	giterr_restore(&err_state);

	cl_assert_equal_i(42, giterr_last()->klass);
	cl_assert_equal_s("Foo: bar", giterr_last()->message);
}
