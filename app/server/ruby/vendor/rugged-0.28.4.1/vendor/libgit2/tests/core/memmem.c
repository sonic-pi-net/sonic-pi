#include "clar_libgit2.h"

static void assert_found(const char *haystack, const char *needle, size_t expected_pos)
{
	cl_assert_equal_p(git__memmem(haystack, haystack ? strlen(haystack) : 0,
				      needle, needle ? strlen(needle) : 0),
			  haystack + expected_pos);
}

static void assert_absent(const char *haystack, const char *needle)
{
	cl_assert_equal_p(git__memmem(haystack, haystack ? strlen(haystack) : 0,
				      needle, needle ? strlen(needle) : 0),
			  NULL);
}

void test_core_memmem__found(void)
{
	assert_found("a", "a", 0);
	assert_found("ab", "a", 0);
	assert_found("ba", "a", 1);
	assert_found("aa", "a", 0);
	assert_found("aab", "aa", 0);
	assert_found("baa", "aa", 1);
	assert_found("dabc", "abc", 1);
	assert_found("abababc", "abc", 4);
}

void test_core_memmem__absent(void)
{
	assert_absent("a", "b");
	assert_absent("a", "aa");
	assert_absent("ba", "ab");
	assert_absent("ba", "ab");
	assert_absent("abc", "abcd");
	assert_absent("abcabcabc", "bcac");
}

void test_core_memmem__edgecases(void)
{
	assert_absent(NULL, NULL);
	assert_absent("a", NULL);
	assert_absent(NULL, "a");
	assert_absent("", "a");
	assert_absent("a", "");
}
