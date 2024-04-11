/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "clar_libgit2.h"

#include "git2/sys/repository.h"
#include "attr.h"

static git_repository *g_repo = NULL;

void test_attr_macro__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

void test_attr_macro__macros(void)
{
	const char *names[7] = { "rootattr", "binary", "diff", "crlf", "merge", "text", "frotz" };
	const char *names2[5] = { "mymacro", "positive", "negative", "rootattr", "another" };
	const char *names3[3] = { "macro2", "multi2", "multi3" };
	const char *values[7];

	g_repo = cl_git_sandbox_init("attr");

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "binfile", 7, names));

	cl_assert(GIT_ATTR_IS_TRUE(values[0]));
	cl_assert(GIT_ATTR_IS_TRUE(values[1]));
	cl_assert(GIT_ATTR_IS_FALSE(values[2]));
	cl_assert(GIT_ATTR_IS_FALSE(values[3]));
	cl_assert(GIT_ATTR_IS_FALSE(values[4]));
	cl_assert(GIT_ATTR_IS_FALSE(values[5]));
	cl_assert(GIT_ATTR_IS_UNSPECIFIED(values[6]));

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "macro_test", 5, names2));

	cl_assert(GIT_ATTR_IS_TRUE(values[0]));
	cl_assert(GIT_ATTR_IS_TRUE(values[1]));
	cl_assert(GIT_ATTR_IS_FALSE(values[2]));
	cl_assert(GIT_ATTR_IS_UNSPECIFIED(values[3]));
	cl_assert_equal_s("77", values[4]);

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "macro_test", 3, names3));

	cl_assert(GIT_ATTR_IS_TRUE(values[0]));
	cl_assert(GIT_ATTR_IS_FALSE(values[1]));
	cl_assert_equal_s("answer", values[2]);
}

void test_attr_macro__bad_macros(void)
{
	const char *names[6] = { "rootattr", "positive", "negative",
		"firstmacro", "secondmacro", "thirdmacro" };
	const char *values[6];

	g_repo = cl_git_sandbox_init("attr");

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "macro_bad", 6, names));

	/* these three just confirm that the "mymacro" rule ran */
	cl_assert(GIT_ATTR_IS_UNSPECIFIED(values[0]));
	cl_assert(GIT_ATTR_IS_TRUE(values[1]));
	cl_assert(GIT_ATTR_IS_FALSE(values[2]));

	/* file contains:
	 *     # let's try some malicious macro defs
	 *     [attr]firstmacro -thirdmacro -secondmacro
	 *     [attr]secondmacro firstmacro -firstmacro
	 *     [attr]thirdmacro secondmacro=hahaha -firstmacro
	 *     macro_bad firstmacro secondmacro thirdmacro
	 *
	 * firstmacro assignment list ends up with:
	 *     -thirdmacro -secondmacro
	 * secondmacro assignment list expands "firstmacro" and ends up with:
	 *     -thirdmacro -secondmacro -firstmacro
	 * thirdmacro assignment don't expand so list ends up with:
	 *     secondmacro="hahaha"
	 *
	 * macro_bad assignment list ends up with:
	 *     -thirdmacro -secondmacro firstmacro &&
	 *     -thirdmacro -secondmacro -firstmacro secondmacro &&
	 *     secondmacro="hahaha" thirdmacro
	 *
	 * so summary results should be:
	 *     -firstmacro secondmacro="hahaha" thirdmacro
	 */
	cl_assert(GIT_ATTR_IS_FALSE(values[3]));
	cl_assert_equal_s("hahaha", values[4]);
	cl_assert(GIT_ATTR_IS_TRUE(values[5]));
}

void test_attr_macro__macros_in_root_wd_apply(void)
{
	const char *value;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_pass(p_mkdir("empty_standard_repo/dir", 0777));
	cl_git_rewritefile("empty_standard_repo/.gitattributes", "[attr]customattr key=value\n");
	cl_git_rewritefile("empty_standard_repo/dir/.gitattributes", "file customattr\n");

	cl_git_pass(git_attr_get(&value, g_repo, 0, "dir/file", "key"));
	cl_assert_equal_s(value, "value");
}

void test_attr_macro__changing_macro_in_root_wd_updates_attributes(void)
{
	const char *value;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_rewritefile("empty_standard_repo/.gitattributes",
			   "[attr]customattr key=first\n"
			   "file customattr\n");
	cl_git_pass(git_attr_get(&value, g_repo, 0, "file", "key"));
	cl_assert_equal_s(value, "first");

	cl_git_rewritefile("empty_standard_repo/.gitattributes",
			   "[attr]customattr key=second\n"
			   "file customattr\n");
	cl_git_pass(git_attr_get(&value, g_repo, 0, "file", "key"));
	cl_assert_equal_s(value, "second");
}

void test_attr_macro__macros_in_subdir_do_not_apply(void)
{
	const char *value;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_pass(p_mkdir("empty_standard_repo/dir", 0777));
	cl_git_rewritefile("empty_standard_repo/dir/.gitattributes",
			   "[attr]customattr key=value\n"
			   "file customattr\n");

	/* This should _not_ pass, as macros in subdirectories shall be ignored */
	cl_git_pass(git_attr_get(&value, g_repo, 0, "dir/file", "key"));
	cl_assert_equal_p(value, NULL);
}

void test_attr_macro__adding_macro_succeeds(void)
{
	const char *value;

	g_repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_pass(git_attr_add_macro(g_repo, "macro", "key=value"));
	cl_git_rewritefile("empty_standard_repo/.gitattributes", "file.txt macro\n");

	cl_git_pass(git_attr_get(&value, g_repo, 0, "file.txt", "key"));
	cl_assert_equal_s(value, "value");
}

void test_attr_macro__adding_boolean_macros_succeeds(void)
{
	const char *value;

	g_repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_pass(git_attr_add_macro(g_repo, "macro-pos", "positive"));
	cl_git_pass(git_attr_add_macro(g_repo, "macro-neg", "-negative"));
	cl_git_rewritefile("empty_standard_repo/.gitattributes", "file.txt macro-pos macro-neg\n");

	cl_git_pass(git_attr_get(&value, g_repo, 0, "file.txt", "positive"));
	cl_assert(GIT_ATTR_IS_TRUE(value));
	cl_git_pass(git_attr_get(&value, g_repo, 0, "file.txt", "negative"));
	cl_assert(GIT_ATTR_IS_FALSE(value));
}

void test_attr_macro__redefining_macro_succeeds(void)
{
	const char *value;

	g_repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_pass(git_attr_add_macro(g_repo, "macro", "key=value1"));
	cl_git_pass(git_attr_add_macro(g_repo, "macro", "key=value2"));
	cl_git_rewritefile("empty_standard_repo/.gitattributes", "file.txt macro");

	cl_git_pass(git_attr_get(&value, g_repo, 0, "file.txt", "key"));
	cl_assert_equal_s(value, "value2");
}

void test_attr_macro__recursive_macro_resolves(void)
{
	const char *value;

	g_repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_pass(git_attr_add_macro(g_repo, "expandme", "key=value"));
	cl_git_pass(git_attr_add_macro(g_repo, "macro", "expandme"));
	cl_git_rewritefile("empty_standard_repo/.gitattributes", "file.txt macro");

	cl_git_pass(git_attr_get(&value, g_repo, 0, "file.txt", "key"));
	cl_assert_equal_s(value, "value");
}
