#include "clar_libgit2.h"

#include "git2/clone.h"
#include "clone.h"
#include "buffer.h"
#include "futils.h"

static git_buf path = GIT_BUF_INIT;

void test_win32_longpath__initialize(void)
{
#ifdef GIT_WIN32
	const char *base = clar_sandbox_path();
	size_t base_len = strlen(base);
	size_t remain = MAX_PATH - base_len;
	size_t i;

	git_buf_clear(&path);
	git_buf_puts(&path, base);
	git_buf_putc(&path, '/');

	cl_assert(remain < (MAX_PATH - 5));

	for (i = 0; i < (remain - 5); i++)
		git_buf_putc(&path, 'a');
#endif
}

void test_win32_longpath__cleanup(void)
{
	git_buf_dispose(&path);
}

#ifdef GIT_WIN32
void assert_name_too_long(void)
{
	const git_error *err;
	size_t expected_len, actual_len;
	char *expected_msg;

	err = git_error_last();
	actual_len = strlen(err->message);

	expected_msg = git_win32_get_error_message(ERROR_FILENAME_EXCED_RANGE);
	expected_len = strlen(expected_msg);

	/* check the suffix */
	cl_assert_equal_s(expected_msg, err->message + (actual_len - expected_len));

	git__free(expected_msg);
}
#endif

void test_win32_longpath__errmsg_on_checkout(void)
{
#ifdef GIT_WIN32
	git_repository *repo;

	cl_git_fail(git_clone(&repo, cl_fixture("testrepo.git"), path.ptr, NULL));
	assert_name_too_long();
#endif
}
