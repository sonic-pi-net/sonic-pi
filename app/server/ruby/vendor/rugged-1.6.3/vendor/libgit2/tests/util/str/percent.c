#include "clar_libgit2.h"

static void expect_decode_pass(const char *expected, const char *encoded)
{
	git_str in = GIT_STR_INIT, out = GIT_STR_INIT;

	/*
	 * ensure that we only read the given length of the input buffer
	 * by putting garbage at the end.  this will ensure that we do
	 * not, eg, rely on nul-termination or walk off the end of the buf.
	 */
	cl_git_pass(git_str_puts(&in, encoded));
	cl_git_pass(git_str_PUTS(&in, "TRAILER"));

	cl_git_pass(git_str_decode_percent(&out, in.ptr, strlen(encoded)));

	cl_assert_equal_s(expected, git_str_cstr(&out));
	cl_assert_equal_i(strlen(expected), git_str_len(&out));

	git_str_dispose(&in);
	git_str_dispose(&out);
}

void test_str_percent__decode_succeeds(void)
{
	expect_decode_pass("", "");
	expect_decode_pass(" ", "%20");
	expect_decode_pass("a", "a");
	expect_decode_pass(" a", "%20a");
	expect_decode_pass("a ", "a%20");
	expect_decode_pass("github.com", "github.com");
	expect_decode_pass("github.com", "githu%62.com");
	expect_decode_pass("github.com", "github%2ecom");
	expect_decode_pass("foo bar baz", "foo%20bar%20baz");
	expect_decode_pass("foo bar baz", "foo%20bar%20baz");
	expect_decode_pass("foo bar ", "foo%20bar%20");
}

void test_str_percent__ignores_invalid(void)
{
	expect_decode_pass("githu%%.com", "githu%%.com");
	expect_decode_pass("github.co%2", "github.co%2");
	expect_decode_pass("github%2.com", "github%2.com");
	expect_decode_pass("githu%2z.com", "githu%2z.com");
	expect_decode_pass("github.co%9z", "github.co%9z");
	expect_decode_pass("github.co%2", "github.co%2");
	expect_decode_pass("github.co%", "github.co%");
}
