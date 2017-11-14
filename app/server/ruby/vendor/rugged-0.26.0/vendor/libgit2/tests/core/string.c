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

void test_core_string__strcmp(void)
{
	cl_assert(git__strcmp("", "") == 0);
	cl_assert(git__strcmp("foo", "foo") == 0);
	cl_assert(git__strcmp("Foo", "foo") < 0);
	cl_assert(git__strcmp("foo", "FOO") > 0);
	cl_assert(git__strcmp("foo", "fOO") > 0);

	cl_assert(strcmp("rt\303\202of", "rt dev\302\266h") > 0);
	cl_assert(strcmp("e\342\202\254ghi=", "et") > 0);
	cl_assert(strcmp("rt dev\302\266h", "rt\303\202of") < 0);
	cl_assert(strcmp("et", "e\342\202\254ghi=") < 0);
	cl_assert(strcmp("\303\215", "\303\255") < 0);

	cl_assert(git__strcmp("rt\303\202of", "rt dev\302\266h") > 0);
	cl_assert(git__strcmp("e\342\202\254ghi=", "et") > 0);
	cl_assert(git__strcmp("rt dev\302\266h", "rt\303\202of") < 0);
	cl_assert(git__strcmp("et", "e\342\202\254ghi=") < 0);
	cl_assert(git__strcmp("\303\215", "\303\255") < 0);
}

void test_core_string__strcasecmp(void)
{
	cl_assert(git__strcasecmp("", "") == 0);
	cl_assert(git__strcasecmp("foo", "foo") == 0);
	cl_assert(git__strcasecmp("foo", "Foo") == 0);
	cl_assert(git__strcasecmp("foo", "FOO") == 0);
	cl_assert(git__strcasecmp("foo", "fOO") == 0);

	cl_assert(strcasecmp("rt\303\202of", "rt dev\302\266h") > 0);
	cl_assert(strcasecmp("e\342\202\254ghi=", "et") > 0);
	cl_assert(strcasecmp("rt dev\302\266h", "rt\303\202of") < 0);
	cl_assert(strcasecmp("et", "e\342\202\254ghi=") < 0);
	cl_assert(strcasecmp("\303\215", "\303\255") < 0);

	cl_assert(git__strcasecmp("rt\303\202of", "rt dev\302\266h") > 0);
	cl_assert(git__strcasecmp("e\342\202\254ghi=", "et") > 0);
	cl_assert(git__strcasecmp("rt dev\302\266h", "rt\303\202of") < 0);
	cl_assert(git__strcasecmp("et", "e\342\202\254ghi=") < 0);
	cl_assert(git__strcasecmp("\303\215", "\303\255") < 0);
}
