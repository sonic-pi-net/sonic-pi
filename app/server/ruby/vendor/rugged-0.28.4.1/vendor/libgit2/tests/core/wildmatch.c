#include "clar_libgit2.h"

#include "wildmatch.h"

#define assert_matches(string, pattern, wildmatch, iwildmatch, pathmatch, ipathmatch) \
	assert_matches_(string, pattern, wildmatch, iwildmatch, pathmatch, ipathmatch, __FILE__, __LINE__)

static void assert_matches_(const char *string, const char *pattern,
		char expected_wildmatch, char expected_iwildmatch,
		char expected_pathmatch, char expected_ipathmatch,
		const char *file, size_t line)
{
	if (wildmatch(pattern, string, WM_PATHNAME) == expected_wildmatch)
		clar__fail(file, line, "Test failed (wildmatch).", string, 1);
	if (wildmatch(pattern, string, WM_PATHNAME|WM_CASEFOLD) == expected_iwildmatch)
		clar__fail(file, line, "Test failed (iwildmatch).", string, 1);
	if (wildmatch(pattern, string, 0) == expected_pathmatch)
		clar__fail(file, line, "Test failed (pathmatch).", string, 1);
	if (wildmatch(pattern, string, WM_CASEFOLD) == expected_ipathmatch)
		clar__fail(file, line, "Test failed (ipathmatch).", string, 1);
}

/*
 * Below testcases are imported from git.git, t3070-wildmatch,sh at tag v2.22.0.
 * Note that we've only imported the direct wildcard tests, but not the matching
 * tests for git-ls-files.
 */

void test_core_wildmatch__basic_wildmatch(void)
{
	assert_matches("foo", "foo", 1, 1, 1, 1);
	assert_matches("foo", "bar", 0, 0, 0, 0);
	assert_matches("", "", 1, 1, 1, 1);
	assert_matches("foo", "???", 1, 1, 1, 1);
	assert_matches("foo", "??", 0, 0, 0, 0);
	assert_matches("foo", "*", 1, 1, 1, 1);
	assert_matches("foo", "f*", 1, 1, 1, 1);
	assert_matches("foo", "*f", 0, 0, 0, 0);
	assert_matches("foo", "*foo*", 1, 1, 1, 1);
	assert_matches("foobar", "*ob*a*r*", 1, 1, 1, 1);
	assert_matches("aaaaaaabababab", "*ab", 1, 1, 1, 1);
	assert_matches("foo*", "foo\\*", 1, 1, 1, 1);
	assert_matches("foobar", "foo\\*bar", 0, 0, 0, 0);
	assert_matches("f\\oo", "f\\\\oo", 1, 1, 1, 1);
	assert_matches("ball", "*[al]?", 1, 1, 1, 1);
	assert_matches("ten", "[ten]", 0, 0, 0, 0);
	assert_matches("ten", "**[!te]", 1, 1, 1, 1);
	assert_matches("ten", "**[!ten]", 0, 0, 0, 0);
	assert_matches("ten", "t[a-g]n", 1, 1, 1, 1);
	assert_matches("ten", "t[!a-g]n", 0, 0, 0, 0);
	assert_matches("ton", "t[!a-g]n", 1, 1, 1, 1);
	assert_matches("ton", "t[^a-g]n", 1, 1, 1, 1);
	assert_matches("a]b", "a[]]b", 1, 1, 1, 1);
	assert_matches("a-b", "a[]-]b", 1, 1, 1, 1);
	assert_matches("a]b", "a[]-]b", 1, 1, 1, 1);
	assert_matches("aab", "a[]-]b", 0, 0, 0, 0);
	assert_matches("aab", "a[]a-]b", 1, 1, 1, 1);
	assert_matches("]", "]", 1, 1, 1, 1);
}

void test_core_wildmatch__slash_matching_features(void)
{
	assert_matches("foo/baz/bar", "foo*bar", 0, 0, 1, 1);
	assert_matches("foo/baz/bar", "foo**bar", 0, 0, 1, 1);
	assert_matches("foobazbar", "foo**bar", 1, 1, 1, 1);
	assert_matches("foo/baz/bar", "foo/**/bar", 1, 1, 1, 1);
	assert_matches("foo/baz/bar", "foo/**/**/bar", 1, 1, 0, 0);
	assert_matches("foo/b/a/z/bar", "foo/**/bar", 1, 1, 1, 1);
	assert_matches("foo/b/a/z/bar", "foo/**/**/bar", 1, 1, 1, 1);
	assert_matches("foo/bar", "foo/**/bar", 1, 1, 0, 0);
	assert_matches("foo/bar", "foo/**/**/bar", 1, 1, 0, 0);
	assert_matches("foo/bar", "foo?bar", 0, 0, 1, 1);
	assert_matches("foo/bar", "foo[/]bar", 0, 0, 1, 1);
	assert_matches("foo/bar", "foo[^a-z]bar", 0, 0, 1, 1);
	assert_matches("foo/bar", "f[^eiu][^eiu][^eiu][^eiu][^eiu]r", 0, 0, 1, 1);
	assert_matches("foo-bar", "f[^eiu][^eiu][^eiu][^eiu][^eiu]r", 1, 1, 1, 1);
	assert_matches("foo", "**/foo", 1, 1, 0, 0);
	assert_matches("XXX/foo", "**/foo", 1, 1, 1, 1);
	assert_matches("bar/baz/foo", "**/foo", 1, 1, 1, 1);
	assert_matches("bar/baz/foo", "*/foo", 0, 0, 1, 1);
	assert_matches("foo/bar/baz", "**/bar*", 0, 0, 1, 1);
	assert_matches("deep/foo/bar/baz", "**/bar/*", 1, 1, 1, 1);
	assert_matches("deep/foo/bar/baz/", "**/bar/*", 0, 0, 1, 1);
	assert_matches("deep/foo/bar/baz/", "**/bar/**", 1, 1, 1, 1);
	assert_matches("deep/foo/bar", "**/bar/*", 0, 0, 0, 0);
	assert_matches("deep/foo/bar/", "**/bar/**", 1, 1, 1, 1);
	assert_matches("foo/bar/baz", "**/bar**", 0, 0, 1, 1);
	assert_matches("foo/bar/baz/x", "*/bar/**", 1, 1, 1, 1);
	assert_matches("deep/foo/bar/baz/x", "*/bar/**", 0, 0, 1, 1);
	assert_matches("deep/foo/bar/baz/x", "**/bar/*/*", 1, 1, 1, 1);
}

void test_core_wildmatch__various_additional(void)
{
	assert_matches("acrt", "a[c-c]st", 0, 0, 0, 0);
	assert_matches("acrt", "a[c-c]rt", 1, 1, 1, 1);
	assert_matches("]", "[!]-]", 0, 0, 0, 0);
	assert_matches("a", "[!]-]", 1, 1, 1, 1);
	assert_matches("", "\\", 0, 0, 0, 0);
	assert_matches("\\", "\\", 0, 0, 0, 0);
	assert_matches("XXX/\\", "*/\\", 0, 0, 0, 0);
	assert_matches("XXX/\\", "*/\\\\", 1, 1, 1, 1);
	assert_matches("foo", "foo", 1, 1, 1, 1);
	assert_matches("@foo", "@foo", 1, 1, 1, 1);
	assert_matches("foo", "@foo", 0, 0, 0, 0);
	assert_matches("[ab]", "\\[ab]", 1, 1, 1, 1);
	assert_matches("[ab]", "[[]ab]", 1, 1, 1, 1);
	assert_matches("[ab]", "[[:]ab]", 1, 1, 1, 1);
	assert_matches("[ab]", "[[::]ab]", 0, 0, 0, 0);
	assert_matches("[ab]", "[[:digit]ab]", 1, 1, 1, 1);
	assert_matches("[ab]", "[\\[:]ab]", 1, 1, 1, 1);
	assert_matches("?a?b", "\\??\\?b", 1, 1, 1, 1);
	assert_matches("abc", "\\a\\b\\c", 1, 1, 1, 1);
	assert_matches("foo", "", 0, 0, 0, 0);
	assert_matches("foo/bar/baz/to", "**/t[o]", 1, 1, 1, 1);
}

void test_core_wildmatch__character_classes(void)
{
	assert_matches("a1B", "[[:alpha:]][[:digit:]][[:upper:]]", 1, 1, 1, 1);
	assert_matches("a", "[[:digit:][:upper:][:space:]]", 0, 1, 0, 1);
	assert_matches("A", "[[:digit:][:upper:][:space:]]", 1, 1, 1, 1);
	assert_matches("1", "[[:digit:][:upper:][:space:]]", 1, 1, 1, 1);
	assert_matches("1", "[[:digit:][:upper:][:spaci:]]", 0, 0, 0, 0);
	assert_matches(" ", "[[:digit:][:upper:][:space:]]", 1, 1, 1, 1);
	assert_matches(".", "[[:digit:][:upper:][:space:]]", 0, 0, 0, 0);
	assert_matches(".", "[[:digit:][:punct:][:space:]]", 1, 1, 1, 1);
	assert_matches("5", "[[:xdigit:]]", 1, 1, 1, 1);
	assert_matches("f", "[[:xdigit:]]", 1, 1, 1, 1);
	assert_matches("D", "[[:xdigit:]]", 1, 1, 1, 1);
	assert_matches("_", "[[:alnum:][:alpha:][:blank:][:cntrl:][:digit:][:graph:][:lower:][:print:][:punct:][:space:][:upper:][:xdigit:]]", 1, 1, 1, 1);
	assert_matches(".", "[^[:alnum:][:alpha:][:blank:][:cntrl:][:digit:][:lower:][:space:][:upper:][:xdigit:]]", 1, 1, 1, 1);
	assert_matches("5", "[a-c[:digit:]x-z]", 1, 1, 1, 1);
	assert_matches("b", "[a-c[:digit:]x-z]", 1, 1, 1, 1);
	assert_matches("y", "[a-c[:digit:]x-z]", 1, 1, 1, 1);
	assert_matches("q", "[a-c[:digit:]x-z]", 0, 0, 0, 0);
}

void test_core_wildmatch__additional_with_malformed(void)
{
	assert_matches("]", "[\\\\-^]", 1, 1, 1, 1);
	assert_matches("[", "[\\\\-^]", 0, 0, 0, 0);
	assert_matches("-", "[\\-_]", 1, 1, 1, 1);
	assert_matches("]", "[\\]]", 1, 1, 1, 1);
	assert_matches("\\]", "[\\]]", 0, 0, 0, 0);
	assert_matches("\\", "[\\]]", 0, 0, 0, 0);
	assert_matches("ab", "a[]b", 0, 0, 0, 0);
	assert_matches("a[]b", "a[]b", 0, 0, 0, 0);
	assert_matches("ab[", "ab[", 0, 0, 0, 0);
	assert_matches("ab", "[!", 0, 0, 0, 0);
	assert_matches("ab", "[-", 0, 0, 0, 0);
	assert_matches("-", "[-]", 1, 1, 1, 1);
	assert_matches("-", "[a-", 0, 0, 0, 0);
	assert_matches("-", "[!a-", 0, 0, 0, 0);
	assert_matches("-", "[--A]", 1, 1, 1, 1);
	assert_matches("5", "[--A]", 1, 1, 1, 1);
	assert_matches(" ", "[ --]", 1, 1, 1, 1);
	assert_matches("$", "[ --]", 1, 1, 1, 1);
	assert_matches("-", "[ --]", 1, 1, 1, 1);
	assert_matches("0", "[ --]", 0, 0, 0, 0);
	assert_matches("-", "[---]", 1, 1, 1, 1);
	assert_matches("-", "[------]", 1, 1, 1, 1);
	assert_matches("j", "[a-e-n]", 0, 0, 0, 0);
	assert_matches("-", "[a-e-n]", 1, 1, 1, 1);
	assert_matches("a", "[!------]", 1, 1, 1, 1);
	assert_matches("[", "[]-a]", 0, 0, 0, 0);
	assert_matches("^", "[]-a]", 1, 1, 1, 1);
	assert_matches("^", "[!]-a]", 0, 0, 0, 0);
	assert_matches("[", "[!]-a]", 1, 1, 1, 1);
	assert_matches("^", "[a^bc]", 1, 1, 1, 1);
	assert_matches("-b]", "[a-]b]", 1, 1, 1, 1);
	assert_matches("\\", "[\\]", 0, 0, 0, 0);
	assert_matches("\\", "[\\\\]", 1, 1, 1, 1);
	assert_matches("\\", "[!\\\\]", 0, 0, 0, 0);
	assert_matches("G", "[A-\\\\]", 1, 1, 1, 1);
	assert_matches("aaabbb", "b*a", 0, 0, 0, 0);
	assert_matches("aabcaa", "*ba*", 0, 0, 0, 0);
	assert_matches(",", "[,]", 1, 1, 1, 1);
	assert_matches(",", "[\\\\,]", 1, 1, 1, 1);
	assert_matches("\\", "[\\\\,]", 1, 1, 1, 1);
	assert_matches("-", "[,-.]", 1, 1, 1, 1);
	assert_matches("+", "[,-.]", 0, 0, 0, 0);
	assert_matches("-.]", "[,-.]", 0, 0, 0, 0);
	assert_matches("2", "[\\1-\\3]", 1, 1, 1, 1);
	assert_matches("3", "[\\1-\\3]", 1, 1, 1, 1);
	assert_matches("4", "[\\1-\\3]", 0, 0, 0, 0);
	assert_matches("\\", "[[-\\]]", 1, 1, 1, 1);
	assert_matches("[", "[[-\\]]", 1, 1, 1, 1);
	assert_matches("]", "[[-\\]]", 1, 1, 1, 1);
	assert_matches("-", "[[-\\]]", 0, 0, 0, 0);
}

void test_core_wildmatch__recursion(void)
{
	assert_matches("-adobe-courier-bold-o-normal--12-120-75-75-m-70-iso8859-1", "-*-*-*-*-*-*-12-*-*-*-m-*-*-*", 1, 1, 1, 1);
	assert_matches("-adobe-courier-bold-o-normal--12-120-75-75-X-70-iso8859-1", "-*-*-*-*-*-*-12-*-*-*-m-*-*-*", 0, 0, 0, 0);
	assert_matches("-adobe-courier-bold-o-normal--12-120-75-75-/-70-iso8859-1", "-*-*-*-*-*-*-12-*-*-*-m-*-*-*", 0, 0, 0, 0);
	assert_matches("XXX/adobe/courier/bold/o/normal//12/120/75/75/m/70/iso8859/1", "XXX/*/*/*/*/*/*/12/*/*/*/m/*/*/*", 1, 1, 1, 1);
	assert_matches("XXX/adobe/courier/bold/o/normal//12/120/75/75/X/70/iso8859/1", "XXX/*/*/*/*/*/*/12/*/*/*/m/*/*/*", 0, 0, 0, 0);
	assert_matches("abcd/abcdefg/abcdefghijk/abcdefghijklmnop.txt", "**/*a*b*g*n*t", 1, 1, 1, 1);
	assert_matches("abcd/abcdefg/abcdefghijk/abcdefghijklmnop.txtz", "**/*a*b*g*n*t", 0, 0, 0, 0);
	assert_matches("foo", "*/*/*", 0, 0, 0, 0);
	assert_matches("foo/bar", "*/*/*", 0, 0, 0, 0);
	assert_matches("foo/bba/arr", "*/*/*", 1, 1, 1, 1);
	assert_matches("foo/bb/aa/rr", "*/*/*", 0, 0, 1, 1);
	assert_matches("foo/bb/aa/rr", "**/**/**", 1, 1, 1, 1);
	assert_matches("abcXdefXghi", "*X*i", 1, 1, 1, 1);
	assert_matches("ab/cXd/efXg/hi", "*X*i", 0, 0, 1, 1);
	assert_matches("ab/cXd/efXg/hi", "*/*X*/*/*i", 1, 1, 1, 1);
	assert_matches("ab/cXd/efXg/hi", "**/*X*/**/*i", 1, 1, 1, 1);
}

void test_core_wildmatch__pathmatch(void)
{
	assert_matches("foo", "fo", 0, 0, 0, 0);
	assert_matches("foo/bar", "foo/bar", 1, 1, 1, 1);
	assert_matches("foo/bar", "foo/*", 1, 1, 1, 1);
	assert_matches("foo/bba/arr", "foo/*", 0, 0, 1, 1);
	assert_matches("foo/bba/arr", "foo/**", 1, 1, 1, 1);
	assert_matches("foo/bba/arr", "foo*", 0, 0, 1, 1);
	assert_matches("foo/bba/arr", "foo**", 0, 0, 1, 1);
	assert_matches("foo/bba/arr", "foo/*arr", 0, 0, 1, 1);
	assert_matches("foo/bba/arr", "foo/**arr", 0, 0, 1, 1);
	assert_matches("foo/bba/arr", "foo/*z", 0, 0, 0, 0);
	assert_matches("foo/bba/arr", "foo/**z", 0, 0, 0, 0);
	assert_matches("foo/bar", "foo?bar", 0, 0, 1, 1);
	assert_matches("foo/bar", "foo[/]bar", 0, 0, 1, 1);
	assert_matches("foo/bar", "foo[^a-z]bar", 0, 0, 1, 1);
	assert_matches("ab/cXd/efXg/hi", "*Xg*i", 0, 0, 1, 1);
}

void test_core_wildmatch__case_sensitivity(void)
{
	assert_matches("a", "[A-Z]", 0, 1, 0, 1);
	assert_matches("A", "[A-Z]", 1, 1, 1, 1);
	assert_matches("A", "[a-z]", 0, 1, 0, 1);
	assert_matches("a", "[a-z]", 1, 1, 1, 1);
	assert_matches("a", "[[:upper:]]", 0, 1, 0, 1);
	assert_matches("A", "[[:upper:]]", 1, 1, 1, 1);
	assert_matches("A", "[[:lower:]]", 0, 1, 0, 1);
	assert_matches("a", "[[:lower:]]", 1, 1, 1, 1);
	assert_matches("A", "[B-Za]", 0, 1, 0, 1);
	assert_matches("a", "[B-Za]", 1, 1, 1, 1);
	assert_matches("A", "[B-a]", 0, 1, 0, 1);
	assert_matches("a", "[B-a]", 1, 1, 1, 1);
	assert_matches("z", "[Z-y]", 0, 1, 0, 1);
	assert_matches("Z", "[Z-y]", 1, 1, 1, 1);
}
