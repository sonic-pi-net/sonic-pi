#include "clar_libgit2.h"

#include <locale.h>

#include "regexp.h"
#include "userdiff.h"

#if LC_ALL > 0
static const char *old_locales[LC_ALL];
#endif

static git_regexp regex;

void test_core_regexp__initialize(void)
{
#if LC_ALL > 0
	memset(&old_locales, 0, sizeof(old_locales));
#endif
}

void test_core_regexp__cleanup(void)
{
	git_regexp_dispose(&regex);
}

static void try_set_locale(int category)
{
#if LC_ALL > 0
	old_locales[category] = setlocale(category, NULL);
#endif

	if (!setlocale(category, "UTF-8") &&
	    !setlocale(category, "c.utf8") &&
	    !setlocale(category, "en_US.UTF-8"))
		cl_skip();

	if (MB_CUR_MAX == 1)
		cl_fail("Expected locale to be switched to multibyte");
}


void test_core_regexp__compile_ignores_global_locale_ctype(void)
{
	try_set_locale(LC_CTYPE);
	cl_git_pass(git_regexp_compile(&regex, "[\xc0-\xff][\x80-\xbf]", 0));
}

void test_core_regexp__compile_ignores_global_locale_collate(void)
{
#ifdef GIT_WIN32
	cl_skip();
#endif

	try_set_locale(LC_COLLATE);
	cl_git_pass(git_regexp_compile(&regex, "[\xc0-\xff][\x80-\xbf]", 0));
}

void test_core_regexp__regex_matches_digits_with_locale(void)
{
	char c, str[2];

#ifdef GIT_WIN32
	cl_skip();
#endif

	try_set_locale(LC_COLLATE);
	try_set_locale(LC_CTYPE);

	cl_git_pass(git_regexp_compile(&regex, "[[:digit:]]", 0));

	str[1] = '\0';
	for (c = '0'; c <= '9'; c++) {
	    str[0] = c;
	    cl_git_pass(git_regexp_match(&regex, str));
	}
}

void test_core_regexp__regex_matches_alphabet_with_locale(void)
{
	char c, str[2];

#ifdef GIT_WIN32
	cl_skip();
#endif

	try_set_locale(LC_COLLATE);
	try_set_locale(LC_CTYPE);

	cl_git_pass(git_regexp_compile(&regex, "[[:alpha:]]", 0));

	str[1] = '\0';
	for (c = 'a'; c <= 'z'; c++) {
	    str[0] = c;
	    cl_git_pass(git_regexp_match(&regex, str));
	}
	for (c = 'A'; c <= 'Z'; c++) {
	    str[0] = c;
	    cl_git_pass(git_regexp_match(&regex, str));
	}
}

void test_core_regexp__compile_userdiff_regexps(void)
{
	size_t idx;

	for (idx = 0; idx < ARRAY_SIZE(builtin_defs); ++idx) {
		git_diff_driver_definition ddef = builtin_defs[idx];

		cl_git_pass(git_regexp_compile(&regex, ddef.fns, ddef.flags));
		git_regexp_dispose(&regex);

		cl_git_pass(git_regexp_compile(&regex, ddef.words, 0));
		git_regexp_dispose(&regex);
	}
}

void test_core_regexp__simple_search_matches(void)
{
	cl_git_pass(git_regexp_compile(&regex, "a", 0));
	cl_git_pass(git_regexp_search(&regex, "a", 0, NULL));
}

void test_core_regexp__case_insensitive_search_matches(void)
{
	cl_git_pass(git_regexp_compile(&regex, "a", GIT_REGEXP_ICASE));
	cl_git_pass(git_regexp_search(&regex, "A", 0, NULL));
}

void test_core_regexp__nonmatching_search_returns_error(void)
{
	cl_git_pass(git_regexp_compile(&regex, "a", 0));
	cl_git_fail(git_regexp_search(&regex, "b", 0, NULL));
}

void test_core_regexp__search_finds_complete_match(void)
{
	git_regmatch matches[1];

	cl_git_pass(git_regexp_compile(&regex, "abc", 0));
	cl_git_pass(git_regexp_search(&regex, "abc", 1, matches));
	cl_assert_equal_i(matches[0].start, 0);
	cl_assert_equal_i(matches[0].end, 3);
}

void test_core_regexp__search_finds_correct_offsets(void)
{
	git_regmatch matches[3];

	cl_git_pass(git_regexp_compile(&regex, "(a*)(b*)", 0));
	cl_git_pass(git_regexp_search(&regex, "ab", 3, matches));
	cl_assert_equal_i(matches[0].start, 0);
	cl_assert_equal_i(matches[0].end, 2);
	cl_assert_equal_i(matches[1].start, 0);
	cl_assert_equal_i(matches[1].end, 1);
	cl_assert_equal_i(matches[2].start, 1);
	cl_assert_equal_i(matches[2].end, 2);
}

void test_core_regexp__search_finds_empty_group(void)
{
	git_regmatch matches[3];

	cl_git_pass(git_regexp_compile(&regex, "(a*)(b*)c", 0));
	cl_git_pass(git_regexp_search(&regex, "ac", 3, matches));
	cl_assert_equal_i(matches[0].start, 0);
	cl_assert_equal_i(matches[0].end, 2);
	cl_assert_equal_i(matches[1].start, 0);
	cl_assert_equal_i(matches[1].end, 1);
	cl_assert_equal_i(matches[2].start, 1);
	cl_assert_equal_i(matches[2].end, 1);
}

void test_core_regexp__search_fills_matches_with_first_matching_groups(void)
{
	git_regmatch matches[2];

	cl_git_pass(git_regexp_compile(&regex, "(a)(b)(c)", 0));
	cl_git_pass(git_regexp_search(&regex, "abc", 2, matches));
	cl_assert_equal_i(matches[0].start, 0);
	cl_assert_equal_i(matches[0].end, 3);
	cl_assert_equal_i(matches[1].start, 0);
	cl_assert_equal_i(matches[1].end, 1);
}

void test_core_regexp__search_skips_nonmatching_group(void)
{
	git_regmatch matches[4];

	cl_git_pass(git_regexp_compile(&regex, "(a)(b)?(c)", 0));
	cl_git_pass(git_regexp_search(&regex, "ac", 4, matches));
	cl_assert_equal_i(matches[0].start, 0);
	cl_assert_equal_i(matches[0].end, 2);
	cl_assert_equal_i(matches[1].start, 0);
	cl_assert_equal_i(matches[1].end, 1);
	cl_assert_equal_i(matches[2].start, -1);
	cl_assert_equal_i(matches[2].end, -1);
	cl_assert_equal_i(matches[3].start, 1);
	cl_assert_equal_i(matches[3].end, 2);
}

void test_core_regexp__search_initializes_trailing_nonmatching_groups(void)
{
	git_regmatch matches[3];

	cl_git_pass(git_regexp_compile(&regex, "(a)bc", 0));
	cl_git_pass(git_regexp_search(&regex, "abc", 3, matches));
	cl_assert_equal_i(matches[0].start, 0);
	cl_assert_equal_i(matches[0].end, 3);
	cl_assert_equal_i(matches[1].start, 0);
	cl_assert_equal_i(matches[1].end, 1);
	cl_assert_equal_i(matches[2].start, -1);
	cl_assert_equal_i(matches[2].end, -1);
}
