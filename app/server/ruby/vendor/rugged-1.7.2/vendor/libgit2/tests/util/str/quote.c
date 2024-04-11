#include "clar_libgit2.h"

static void expect_quote_pass(const char *expected, const char *str)
{
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_str_puts(&buf, str));
	cl_git_pass(git_str_quote(&buf));

	cl_assert_equal_s(expected, git_str_cstr(&buf));
	cl_assert_equal_i(strlen(expected), git_str_len(&buf));

	git_str_dispose(&buf);
}

void test_str_quote__quote_succeeds(void)
{
	expect_quote_pass("", "");
	expect_quote_pass("foo", "foo");
	expect_quote_pass("foo/bar/baz.c", "foo/bar/baz.c");
	expect_quote_pass("foo bar", "foo bar");
	expect_quote_pass("\"\\\"leading quote\"", "\"leading quote");
	expect_quote_pass("\"slash\\\\y\"", "slash\\y");
	expect_quote_pass("\"foo\\r\\nbar\"", "foo\r\nbar");
	expect_quote_pass("\"foo\\177bar\"", "foo\177bar");
	expect_quote_pass("\"foo\\001bar\"", "foo\001bar");
	expect_quote_pass("\"foo\\377bar\"", "foo\377bar");
}

static void expect_unquote_pass(const char *expected, const char *quoted)
{
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_str_puts(&buf, quoted));
	cl_git_pass(git_str_unquote(&buf));

	cl_assert_equal_s(expected, git_str_cstr(&buf));
	cl_assert_equal_i(strlen(expected), git_str_len(&buf));

	git_str_dispose(&buf);
}

static void expect_unquote_fail(const char *quoted)
{
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_str_puts(&buf, quoted));
	cl_git_fail(git_str_unquote(&buf));

	git_str_dispose(&buf);
}

void test_str_quote__unquote_succeeds(void)
{
	expect_unquote_pass("", "\"\"");
	expect_unquote_pass(" ", "\" \"");
	expect_unquote_pass("foo", "\"foo\"");
	expect_unquote_pass("foo bar", "\"foo bar\"");
	expect_unquote_pass("foo\"bar", "\"foo\\\"bar\"");
	expect_unquote_pass("foo\\bar", "\"foo\\\\bar\"");
	expect_unquote_pass("foo\tbar", "\"foo\\tbar\"");
	expect_unquote_pass("\vfoo\tbar\n", "\"\\vfoo\\tbar\\n\"");
	expect_unquote_pass("foo\nbar", "\"foo\\012bar\"");
	expect_unquote_pass("foo\r\nbar", "\"foo\\015\\012bar\"");
	expect_unquote_pass("foo\r\nbar", "\"\\146\\157\\157\\015\\012\\142\\141\\162\"");
	expect_unquote_pass("newline: \n", "\"newline: \\012\"");
	expect_unquote_pass("0xff: \377", "\"0xff: \\377\"");
}

void test_str_quote__unquote_fails(void)
{
	expect_unquote_fail("no quotes at all");
	expect_unquote_fail("\"no trailing quote");
	expect_unquote_fail("no leading quote\"");
	expect_unquote_fail("\"invalid \\z escape char\"");
	expect_unquote_fail("\"\\q invalid escape char\"");
	expect_unquote_fail("\"invalid escape char \\p\"");
	expect_unquote_fail("\"invalid \\1 escape char \"");
	expect_unquote_fail("\"invalid \\14 escape char \"");
	expect_unquote_fail("\"invalid \\280 escape char\"");
	expect_unquote_fail("\"invalid \\378 escape char\"");
	expect_unquote_fail("\"invalid \\380 escape char\"");
	expect_unquote_fail("\"invalid \\411 escape char\"");
	expect_unquote_fail("\"truncated escape char \\\"");
	expect_unquote_fail("\"truncated escape char \\0\"");
	expect_unquote_fail("\"truncated escape char \\01\"");
}
