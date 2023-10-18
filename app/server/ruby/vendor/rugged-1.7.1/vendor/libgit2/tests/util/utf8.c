#include "clar_libgit2.h"
#include "utf8.h"

void test_utf8__char_length(void)
{
	cl_assert_equal_i(0, git_utf8_char_length("", 0));
	cl_assert_equal_i(1, git_utf8_char_length("$", 1));
	cl_assert_equal_i(5, git_utf8_char_length("abcde", 5));
	cl_assert_equal_i(1, git_utf8_char_length("\xc2\xa2", 2));
	cl_assert_equal_i(2, git_utf8_char_length("\x24\xc2\xa2", 3));
	cl_assert_equal_i(1, git_utf8_char_length("\xf0\x90\x8d\x88", 4));

	/* uncontinued character counted as single characters */
	cl_assert_equal_i(2, git_utf8_char_length("\x24\xc2", 2));
	cl_assert_equal_i(3, git_utf8_char_length("\x24\xc2\xc2\xa2", 4));

	/* invalid characters are counted as single characters */
	cl_assert_equal_i(4, git_utf8_char_length("\x24\xc0\xc0\x34", 4));
	cl_assert_equal_i(4, git_utf8_char_length("\x24\xf5\xfd\xc2", 4));
}
