#include "clar_libgit2.h"
#include "git2/sys/filter.h"
#include "crlf.h"

static git_repository *g_repo = NULL;

void test_filter_query__initialize(void)
{
	g_repo = cl_git_sandbox_init("crlf");

	cl_git_mkfile("crlf/.gitattributes",
		"*.txt text\n"
		"*.bin binary\n"
		"*.crlf text eol=crlf\n"
		"*.lf text eol=lf\n"
		"*.binident binary ident\n"
		"*.ident text ident\n"
		"*.identcrlf ident text eol=crlf\n"
		"*.identlf ident text eol=lf\n"
		"*.custom custom ident text\n");
}

void test_filter_query__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static int filter_for(const char *filename, const char *filter)
{
	git_filter_list *fl;
	int filtered;

	cl_git_pass(git_filter_list_load(
		&fl, g_repo, NULL, filename, GIT_FILTER_TO_WORKTREE, 0));
	filtered = git_filter_list_contains(fl, filter);
	git_filter_list_free(fl);

	return filtered;
}

void test_filter_query__filters(void)
{
	cl_assert_equal_i(1, filter_for("text.txt", "crlf"));
	cl_assert_equal_i(0, filter_for("binary.bin", "crlf"));

	cl_assert_equal_i(1, filter_for("foo.lf", "crlf"));
	cl_assert_equal_i(0, filter_for("foo.lf", "ident"));

	cl_assert_equal_i(1, filter_for("id.ident", "crlf"));
	cl_assert_equal_i(1, filter_for("id.ident", "ident"));

	cl_assert_equal_i(0, filter_for("id.binident", "crlf"));
	cl_assert_equal_i(1, filter_for("id.binident", "ident"));
}

void test_filter_query__autocrlf_true_implies_crlf(void)
{
	cl_repo_set_bool(g_repo, "core.autocrlf", true);
	cl_assert_equal_i(1, filter_for("not_in_gitattributes", "crlf"));
	cl_assert_equal_i(1, filter_for("foo.txt", "crlf"));
	cl_assert_equal_i(0, filter_for("foo.bin", "crlf"));
	cl_assert_equal_i(1, filter_for("foo.lf", "crlf"));

	cl_repo_set_bool(g_repo, "core.autocrlf", false);
	cl_assert_equal_i(0, filter_for("not_in_gitattributes", "crlf"));
	cl_assert_equal_i(1, filter_for("foo.txt", "crlf"));
	cl_assert_equal_i(0, filter_for("foo.bin", "crlf"));
	cl_assert_equal_i(1, filter_for("foo.lf", "crlf"));
}

void test_filter_query__unknown(void)
{
	cl_assert_equal_i(1, filter_for("foo.custom", "crlf"));
	cl_assert_equal_i(1, filter_for("foo.custom", "ident"));
	cl_assert_equal_i(0, filter_for("foo.custom", "custom"));
}

void test_filter_query__custom(void)
{
	git_filter custom = { GIT_FILTER_VERSION };

	cl_git_pass(git_filter_register(
		"custom", &custom, 42));

	cl_assert_equal_i(1, filter_for("foo.custom", "crlf"));
	cl_assert_equal_i(1, filter_for("foo.custom", "ident"));
	cl_assert_equal_i(1, filter_for("foo.custom", "custom"));

	git_filter_unregister("custom");
}
