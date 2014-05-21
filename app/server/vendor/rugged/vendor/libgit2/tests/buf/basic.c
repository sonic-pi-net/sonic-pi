#include "clar_libgit2.h"
#include "buffer.h"

static const char *test_string = "Have you seen that? Have you seeeen that??";

void test_buf_basic__resize(void)
{
	git_buf buf1 = GIT_BUF_INIT;
	git_buf_puts(&buf1, test_string);
	cl_assert(git_buf_oom(&buf1) == 0);
	cl_assert_equal_s(git_buf_cstr(&buf1), test_string);

	git_buf_puts(&buf1, test_string);
	cl_assert(strlen(git_buf_cstr(&buf1)) == strlen(test_string) * 2);
	git_buf_free(&buf1);
}

void test_buf_basic__printf(void)
{
	git_buf buf2 = GIT_BUF_INIT;
	git_buf_printf(&buf2, "%s %s %d ", "shoop", "da", 23);
	cl_assert(git_buf_oom(&buf2) == 0);
	cl_assert_equal_s(git_buf_cstr(&buf2), "shoop da 23 ");

	git_buf_printf(&buf2, "%s %d", "woop", 42);
	cl_assert(git_buf_oom(&buf2) == 0);
	cl_assert_equal_s(git_buf_cstr(&buf2), "shoop da 23 woop 42");
	git_buf_free(&buf2);
}
