#include "clar_libgit2.h"

#include "userdiff.h"

static git_regexp regex;

void test_diff_userdiff__cleanup(void)
{
	git_regexp_dispose(&regex);
}

void test_diff_userdiff__compile_userdiff_regexps(void)
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
