#include "clar_libgit2.h"

/* compare prefixes */
void test_core_string__0(void)
{
	cl_assert(git__prefixcmp("", "") == 0);
	cl_assert(git__prefixcmp("a", "") == 0);
	cl_assert(git__prefixcmp("", "a") < 0);
	cl_assert(git__prefixcmp("a", "b") < 0);
	cl_assert(git__prefixcmp("b", "a") > 0);
	cl_assert(git__prefixcmp("ab", "a") == 0);
	cl_assert(git__prefixcmp("ab", "ac") < 0);
	cl_assert(git__prefixcmp("ab", "aa") > 0);
}

/* compare suffixes */
void test_core_string__1(void)
{
	cl_assert(git__suffixcmp("", "") == 0);
	cl_assert(git__suffixcmp("a", "") == 0);
	cl_assert(git__suffixcmp("", "a") < 0);
	cl_assert(git__suffixcmp("a", "b") < 0);
	cl_assert(git__suffixcmp("b", "a") > 0);
	cl_assert(git__suffixcmp("ba", "a") == 0);
	cl_assert(git__suffixcmp("zaa", "ac") < 0);
	cl_assert(git__suffixcmp("zaz", "ac") > 0);
}

/* compare icase sorting with case equality */
void test_core_string__2(void)
{
	cl_assert(git__strcasesort_cmp("", "") == 0);
	cl_assert(git__strcasesort_cmp("foo", "foo") == 0);
	cl_assert(git__strcasesort_cmp("foo", "bar") > 0);
	cl_assert(git__strcasesort_cmp("bar", "foo") < 0);
	cl_assert(git__strcasesort_cmp("foo", "FOO") > 0);
	cl_assert(git__strcasesort_cmp("FOO", "foo") < 0);
	cl_assert(git__strcasesort_cmp("foo", "BAR") > 0);
	cl_assert(git__strcasesort_cmp("BAR", "foo") < 0);
	cl_assert(git__strcasesort_cmp("fooBar", "foobar") < 0);
}
