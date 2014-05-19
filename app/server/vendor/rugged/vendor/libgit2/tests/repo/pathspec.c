#include "clar_libgit2.h"
#include "git2/pathspec.h"

static git_repository *g_repo;

void test_repo_pathspec__initialize(void)
{
	g_repo = cl_git_sandbox_init("status");
}

void test_repo_pathspec__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

static char *str0[] = { "*_file", "new_file", "garbage" };
static char *str1[] = { "*_FILE", "NEW_FILE", "GARBAGE" };
static char *str2[] = { "staged_*" };
static char *str3[] = { "!subdir", "*_file", "new_file" };
static char *str4[] = { "*" };
static char *str5[] = { "S*" };

void test_repo_pathspec__workdir0(void)
{
	git_strarray s;
	git_pathspec *ps;
	git_pathspec_match_list *m;

	/* { "*_file", "new_file", "garbage" } */
	s.strings = str0; s.count = ARRAY_SIZE(str0);
	cl_git_pass(git_pathspec_new(&ps, &s));

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo, 0, ps));
	cl_assert_equal_sz(10, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(0, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo,
		GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(10, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(1, git_pathspec_match_list_failed_entrycount(m));
	cl_assert_equal_s("garbage", git_pathspec_match_list_failed_entry(m, 0));
	git_pathspec_match_list_free(m);

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo,
		GIT_PATHSPEC_FIND_FAILURES | GIT_PATHSPEC_FAILURES_ONLY, ps));
	cl_assert_equal_sz(0, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(1, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	git_pathspec_free(ps);
}

void test_repo_pathspec__workdir1(void)
{
	git_strarray s;
	git_pathspec *ps;
	git_pathspec_match_list *m;

	/* { "*_FILE", "NEW_FILE", "GARBAGE" } */
	s.strings = str1; s.count = ARRAY_SIZE(str1);
	cl_git_pass(git_pathspec_new(&ps, &s));

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo,
		GIT_PATHSPEC_IGNORE_CASE, ps));
	cl_assert_equal_sz(10, git_pathspec_match_list_entrycount(m));
	git_pathspec_match_list_free(m);

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo,
		GIT_PATHSPEC_USE_CASE, ps));
	cl_assert_equal_sz(0, git_pathspec_match_list_entrycount(m));
	git_pathspec_match_list_free(m);

	cl_git_fail(git_pathspec_match_workdir(&m, g_repo,
		GIT_PATHSPEC_USE_CASE | GIT_PATHSPEC_NO_MATCH_ERROR, ps));

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo,
		GIT_PATHSPEC_IGNORE_CASE | GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(10, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(1, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo,
		GIT_PATHSPEC_USE_CASE | GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(0, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(3, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	git_pathspec_free(ps);
}

void test_repo_pathspec__workdir2(void)
{
	git_strarray s;
	git_pathspec *ps;
	git_pathspec_match_list *m;

	/* { "staged_*" } */
	s.strings = str2; s.count = ARRAY_SIZE(str2);
	cl_git_pass(git_pathspec_new(&ps, &s));

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo, 0, ps));
	cl_assert_equal_sz(5, git_pathspec_match_list_entrycount(m));
	git_pathspec_match_list_free(m);

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo,
		GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(5, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(0, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	cl_git_fail(git_pathspec_match_workdir(&m, g_repo,
		GIT_PATHSPEC_NO_GLOB | GIT_PATHSPEC_NO_MATCH_ERROR, ps));

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo,
		GIT_PATHSPEC_NO_GLOB | GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(0, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(1, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	git_pathspec_free(ps);
}

void test_repo_pathspec__workdir3(void)
{
	git_strarray s;
	git_pathspec *ps;
	git_pathspec_match_list *m;

	/* { "!subdir", "*_file", "new_file" } */
	s.strings = str3; s.count = ARRAY_SIZE(str3);
	cl_git_pass(git_pathspec_new(&ps, &s));

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo, 0, ps));
	cl_assert_equal_sz(7, git_pathspec_match_list_entrycount(m));
	git_pathspec_match_list_free(m);

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo,
		GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(7, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(0, git_pathspec_match_list_failed_entrycount(m));

	cl_assert_equal_s("current_file", git_pathspec_match_list_entry(m, 0));
	cl_assert_equal_s("modified_file", git_pathspec_match_list_entry(m, 1));
	cl_assert_equal_s("new_file", git_pathspec_match_list_entry(m, 2));
	cl_assert_equal_s("staged_changes_modified_file", git_pathspec_match_list_entry(m, 3));
	cl_assert_equal_s("staged_delete_modified_file", git_pathspec_match_list_entry(m, 4));
	cl_assert_equal_s("staged_new_file", git_pathspec_match_list_entry(m, 5));
	cl_assert_equal_s("staged_new_file_modified_file", git_pathspec_match_list_entry(m, 6));
	cl_assert_equal_s(NULL, git_pathspec_match_list_entry(m, 7));

	git_pathspec_match_list_free(m);

	git_pathspec_free(ps);
}

void test_repo_pathspec__workdir4(void)
{
	git_strarray s;
	git_pathspec *ps;
	git_pathspec_match_list *m;

	/* { "*" } */
	s.strings = str4; s.count = ARRAY_SIZE(str4);
	cl_git_pass(git_pathspec_new(&ps, &s));

	cl_git_pass(git_pathspec_match_workdir(&m, g_repo, 0, ps));
	cl_assert_equal_sz(13, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_s("这", git_pathspec_match_list_entry(m, 12));
	git_pathspec_match_list_free(m);

	git_pathspec_free(ps);
}


void test_repo_pathspec__index0(void)
{
	git_index *idx;
	git_strarray s;
	git_pathspec *ps;
	git_pathspec_match_list *m;

	cl_git_pass(git_repository_index(&idx, g_repo));

	/* { "*_file", "new_file", "garbage" } */
	s.strings = str0; s.count = ARRAY_SIZE(str0);
	cl_git_pass(git_pathspec_new(&ps, &s));

	cl_git_pass(git_pathspec_match_index(&m, idx, 0, ps));
	cl_assert_equal_sz(9, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(0, git_pathspec_match_list_failed_entrycount(m));
	cl_assert_equal_s("current_file", git_pathspec_match_list_entry(m, 0));
	cl_assert_equal_s("modified_file", git_pathspec_match_list_entry(m, 1));
	cl_assert_equal_s("staged_changes_modified_file", git_pathspec_match_list_entry(m, 2));
	cl_assert_equal_s("staged_new_file", git_pathspec_match_list_entry(m, 3));
	cl_assert_equal_s("staged_new_file_deleted_file", git_pathspec_match_list_entry(m, 4));
	cl_assert_equal_s("staged_new_file_modified_file", git_pathspec_match_list_entry(m, 5));
	cl_assert_equal_s("subdir/current_file", git_pathspec_match_list_entry(m, 6));
	cl_assert_equal_s("subdir/deleted_file", git_pathspec_match_list_entry(m, 7));
	cl_assert_equal_s("subdir/modified_file", git_pathspec_match_list_entry(m, 8));
	cl_assert_equal_s(NULL, git_pathspec_match_list_entry(m, 9));
	git_pathspec_match_list_free(m);

	cl_git_pass(git_pathspec_match_index(&m, idx,
		GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(9, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(2, git_pathspec_match_list_failed_entrycount(m));
	cl_assert_equal_s("new_file", git_pathspec_match_list_failed_entry(m, 0));
	cl_assert_equal_s("garbage", git_pathspec_match_list_failed_entry(m, 1));
	cl_assert_equal_s(NULL, git_pathspec_match_list_failed_entry(m, 2));
	git_pathspec_match_list_free(m);

	git_pathspec_free(ps);
	git_index_free(idx);
}

void test_repo_pathspec__index1(void)
{
	/* Currently the USE_CASE and IGNORE_CASE flags don't work on the
	 * index because the index sort order for the index iterator is
	 * set by the index itself.  I think the correct fix is for the
	 * index not to embed a global sort order but to support traversal
	 * in either case sensitive or insensitive order in a stateless
	 * manner.
	 *
	 * Anyhow, as it is, there is no point in doing this test.
	 */
#if 0
	git_index *idx;
	git_strarray s;
	git_pathspec *ps;
	git_pathspec_match_list *m;

	cl_git_pass(git_repository_index(&idx, g_repo));

	/* { "*_FILE", "NEW_FILE", "GARBAGE" } */
	s.strings = str1; s.count = ARRAY_SIZE(str1);
	cl_git_pass(git_pathspec_new(&ps, &s));

	cl_git_pass(git_pathspec_match_index(&m, idx,
		GIT_PATHSPEC_USE_CASE, ps));
	cl_assert_equal_sz(0, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(0, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	cl_git_pass(git_pathspec_match_index(&m, idx,
		GIT_PATHSPEC_USE_CASE | GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(0, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(3, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	cl_git_pass(git_pathspec_match_index(&m, idx,
		GIT_PATHSPEC_IGNORE_CASE | GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(10, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(2, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	git_pathspec_free(ps);
	git_index_free(idx);
#endif
}

void test_repo_pathspec__tree0(void)
{
	git_object *tree;
	git_strarray s;
	git_pathspec *ps;
	git_pathspec_match_list *m;

	/* { "*_file", "new_file", "garbage" } */
	s.strings = str0; s.count = ARRAY_SIZE(str0);
	cl_git_pass(git_pathspec_new(&ps, &s));

	cl_git_pass(git_revparse_single(&tree, g_repo, "HEAD~2^{tree}"));

	cl_git_pass(git_pathspec_match_tree(&m, (git_tree *)tree,
		GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(4, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_s("current_file", git_pathspec_match_list_entry(m, 0));
	cl_assert_equal_s("modified_file", git_pathspec_match_list_entry(m, 1));
	cl_assert_equal_s("staged_changes_modified_file", git_pathspec_match_list_entry(m, 2));
	cl_assert_equal_s("staged_delete_modified_file", git_pathspec_match_list_entry(m, 3));
	cl_assert_equal_s(NULL, git_pathspec_match_list_entry(m, 4));
	cl_assert_equal_sz(2, git_pathspec_match_list_failed_entrycount(m));
	cl_assert_equal_s("new_file", git_pathspec_match_list_failed_entry(m, 0));
	cl_assert_equal_s("garbage", git_pathspec_match_list_failed_entry(m, 1));
	cl_assert_equal_s(NULL, git_pathspec_match_list_failed_entry(m, 2));
	git_pathspec_match_list_free(m);

	git_object_free(tree);

	cl_git_pass(git_revparse_single(&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(git_pathspec_match_tree(&m, (git_tree *)tree,
		GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(7, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_s("current_file", git_pathspec_match_list_entry(m, 0));
	cl_assert_equal_s("modified_file", git_pathspec_match_list_entry(m, 1));
	cl_assert_equal_s("staged_changes_modified_file", git_pathspec_match_list_entry(m, 2));
	cl_assert_equal_s("staged_delete_modified_file", git_pathspec_match_list_entry(m, 3));
	cl_assert_equal_s("subdir/current_file", git_pathspec_match_list_entry(m, 4));
	cl_assert_equal_s("subdir/deleted_file", git_pathspec_match_list_entry(m, 5));
	cl_assert_equal_s("subdir/modified_file", git_pathspec_match_list_entry(m, 6));
	cl_assert_equal_s(NULL, git_pathspec_match_list_entry(m, 7));
	cl_assert_equal_sz(2, git_pathspec_match_list_failed_entrycount(m));
	cl_assert_equal_s("new_file", git_pathspec_match_list_failed_entry(m, 0));
	cl_assert_equal_s("garbage", git_pathspec_match_list_failed_entry(m, 1));
	cl_assert_equal_s(NULL, git_pathspec_match_list_failed_entry(m, 2));
	git_pathspec_match_list_free(m);

	git_object_free(tree);

	git_pathspec_free(ps);
}

void test_repo_pathspec__tree5(void)
{
	git_object *tree;
	git_strarray s;
	git_pathspec *ps;
	git_pathspec_match_list *m;

	/* { "S*" } */
	s.strings = str5; s.count = ARRAY_SIZE(str5);
	cl_git_pass(git_pathspec_new(&ps, &s));

	cl_git_pass(git_revparse_single(&tree, g_repo, "HEAD~2^{tree}"));

	cl_git_pass(git_pathspec_match_tree(&m, (git_tree *)tree,
		GIT_PATHSPEC_USE_CASE | GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(0, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_sz(1, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	cl_git_pass(git_pathspec_match_tree(&m, (git_tree *)tree,
		GIT_PATHSPEC_IGNORE_CASE | GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(5, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_s("staged_changes", git_pathspec_match_list_entry(m, 0));
	cl_assert_equal_s("staged_delete_modified_file", git_pathspec_match_list_entry(m, 4));
	cl_assert_equal_sz(0, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	git_object_free(tree);

	cl_git_pass(git_revparse_single(&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(git_pathspec_match_tree(&m, (git_tree *)tree,
		GIT_PATHSPEC_IGNORE_CASE | GIT_PATHSPEC_FIND_FAILURES, ps));
	cl_assert_equal_sz(9, git_pathspec_match_list_entrycount(m));
	cl_assert_equal_s("staged_changes", git_pathspec_match_list_entry(m, 0));
	cl_assert_equal_s("subdir.txt", git_pathspec_match_list_entry(m, 5));
	cl_assert_equal_s("subdir/current_file", git_pathspec_match_list_entry(m, 6));
	cl_assert_equal_sz(0, git_pathspec_match_list_failed_entrycount(m));
	git_pathspec_match_list_free(m);

	git_object_free(tree);

	git_pathspec_free(ps);
}

void test_repo_pathspec__in_memory(void)
{
	static char *strings[] = { "one", "two*", "!three*", "*four" };
	git_strarray s = { strings, ARRAY_SIZE(strings) };
	git_pathspec *ps;

	cl_git_pass(git_pathspec_new(&ps, &s));

	cl_assert(git_pathspec_matches_path(ps, 0, "one"));
	cl_assert(!git_pathspec_matches_path(ps, 0, "ONE"));
	cl_assert(git_pathspec_matches_path(ps, GIT_PATHSPEC_IGNORE_CASE, "ONE"));
	cl_assert(git_pathspec_matches_path(ps, 0, "two"));
	cl_assert(git_pathspec_matches_path(ps, 0, "two.txt"));
	cl_assert(!git_pathspec_matches_path(ps, 0, "three.txt"));
	cl_assert(git_pathspec_matches_path(ps, 0, "anything.four"));
	cl_assert(!git_pathspec_matches_path(ps, 0, "three.four"));
	cl_assert(!git_pathspec_matches_path(ps, 0, "nomatch"));
	cl_assert(!git_pathspec_matches_path(ps, GIT_PATHSPEC_NO_GLOB, "two"));
	cl_assert(git_pathspec_matches_path(ps, GIT_PATHSPEC_NO_GLOB, "two*"));
	cl_assert(!git_pathspec_matches_path(ps, GIT_PATHSPEC_NO_GLOB, "anyfour"));
	cl_assert(git_pathspec_matches_path(ps, GIT_PATHSPEC_NO_GLOB, "*four"));

	git_pathspec_free(ps);
}
