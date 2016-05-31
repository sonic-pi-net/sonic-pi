#include "clar_libgit2.h"
#include "diff_helpers.h"

static git_repository *g_repo = NULL;

void test_diff_index__initialize(void)
{
	g_repo = cl_git_sandbox_init("status");
}

void test_diff_index__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_diff_index__0(void)
{
	/* grabbed a couple of commit oids from the history of the attr repo */
	const char *a_commit = "26a125ee1bf"; /* the current HEAD */
	const char *b_commit = "0017bd4ab1ec3"; /* the start */
	git_tree *a = resolve_commit_oid_to_tree(g_repo, a_commit);
	git_tree *b = resolve_commit_oid_to_tree(g_repo, b_commit);
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	diff_expects exp;

	cl_assert(a);
	cl_assert(b);

	opts.context_lines = 1;
	opts.interhunk_lines = 1;

	memset(&exp, 0, sizeof(exp));

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, a, NULL, &opts));

	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	/* to generate these values:
	 * - cd to tests/resources/status,
	 * - mv .gitted .git
	 * - git diff --name-status --cached 26a125ee1bf
	 * - git diff -U1 --cached 26a125ee1bf
	 * - mv .git .gitted
	 */
	cl_assert_equal_i(8, exp.files);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_MODIFIED]);

	cl_assert_equal_i(8, exp.hunks);

	cl_assert_equal_i(11, exp.lines);
	cl_assert_equal_i(3, exp.line_ctxt);
	cl_assert_equal_i(6, exp.line_adds);
	cl_assert_equal_i(2, exp.line_dels);

	git_diff_free(diff);
	diff = NULL;
	memset(&exp, 0, sizeof(exp));

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, b, NULL, &opts));

	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	/* to generate these values:
	 * - cd to tests/resources/status,
	 * - mv .gitted .git
	 * - git diff --name-status --cached 0017bd4ab1ec3
	 * - git diff -U1 --cached 0017bd4ab1ec3
	 * - mv .git .gitted
	 */
	cl_assert_equal_i(12, exp.files);
	cl_assert_equal_i(7, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_MODIFIED]);

	cl_assert_equal_i(12, exp.hunks);

	cl_assert_equal_i(16, exp.lines);
	cl_assert_equal_i(3, exp.line_ctxt);
	cl_assert_equal_i(11, exp.line_adds);
	cl_assert_equal_i(2, exp.line_dels);

	git_diff_free(diff);
	diff = NULL;

	git_tree_free(a);
	git_tree_free(b);
}

static int diff_stop_after_2_files(
	const git_diff_delta *delta,
	float progress,
	void *payload)
{
	diff_expects *e = payload;

	GIT_UNUSED(progress);
	GIT_UNUSED(delta);

	e->files++;

	return (e->files == 2);
}

void test_diff_index__1(void)
{
	/* grabbed a couple of commit oids from the history of the attr repo */
	const char *a_commit = "26a125ee1bf"; /* the current HEAD */
	const char *b_commit = "0017bd4ab1ec3"; /* the start */
	git_tree *a = resolve_commit_oid_to_tree(g_repo, a_commit);
	git_tree *b = resolve_commit_oid_to_tree(g_repo, b_commit);
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	diff_expects exp;

	cl_assert(a);
	cl_assert(b);

	opts.context_lines = 1;
	opts.interhunk_lines = 1;

	memset(&exp, 0, sizeof(exp));

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, a, NULL, &opts));

	cl_assert_equal_i(1, git_diff_foreach(
		diff, diff_stop_after_2_files, NULL, NULL, NULL, &exp) );

	cl_assert_equal_i(2, exp.files);

	git_diff_free(diff);
	diff = NULL;

	git_tree_free(a);
	git_tree_free(b);
}

void test_diff_index__checks_options_version(void)
{
	const char *a_commit = "26a125ee1bf";
	git_tree *a = resolve_commit_oid_to_tree(g_repo, a_commit);
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	const git_error *err;

	opts.version = 0;
	cl_git_fail(git_diff_tree_to_index(&diff, g_repo, a, NULL, &opts));
	err = giterr_last();
	cl_assert_equal_i(GITERR_INVALID, err->klass);
	cl_assert_equal_p(diff, NULL);

	giterr_clear();
	opts.version = 1024;
	cl_git_fail(git_diff_tree_to_index(&diff, g_repo, a, NULL, &opts));
	err = giterr_last();
	cl_assert_equal_i(GITERR_INVALID, err->klass);
	cl_assert_equal_p(diff, NULL);

	git_tree_free(a);
}

static void do_conflicted_diff(diff_expects *exp, unsigned long flags)
{
	const char *a_commit = "26a125ee1bf"; /* the current HEAD */
	git_tree *a = resolve_commit_oid_to_tree(g_repo, a_commit);
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_index_entry ancestor = {{0}}, ours = {{0}}, theirs = {{0}};
	git_diff *diff = NULL;
	git_index *index;

	cl_assert(a);

	opts.context_lines = 1;
	opts.interhunk_lines = 1;
	opts.flags |= flags;

	memset(exp, 0, sizeof(diff_expects));

	cl_git_pass(git_repository_index(&index, g_repo));

	ancestor.path = ours.path = theirs.path = "staged_changes";
	ancestor.mode = ours.mode = theirs.mode = GIT_FILEMODE_BLOB;

	git_oid_fromstr(&ancestor.id, "d427e0b2e138501a3d15cc376077a3631e15bd46");
	git_oid_fromstr(&ours.id, "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf");
	git_oid_fromstr(&theirs.id, "2bd0a343aeef7a2cf0d158478966a6e587ff3863");

	cl_git_pass(git_index_conflict_add(index, &ancestor, &ours, &theirs));
	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, a, index, &opts));

	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, exp));

	git_diff_free(diff);
	git_tree_free(a);
	git_index_free(index);
}

void test_diff_index__reports_conflicts(void)
{
	diff_expects exp;

	do_conflicted_diff(&exp, 0);

	cl_assert_equal_i(8, exp.files);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_CONFLICTED]);

	cl_assert_equal_i(7, exp.hunks);

	cl_assert_equal_i(9, exp.lines);
	cl_assert_equal_i(2, exp.line_ctxt);
	cl_assert_equal_i(5, exp.line_adds);
	cl_assert_equal_i(2, exp.line_dels);
}

void test_diff_index__reports_conflicts_when_reversed(void)
{
	diff_expects exp;

	do_conflicted_diff(&exp, GIT_DIFF_REVERSE);

	cl_assert_equal_i(8, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_CONFLICTED]);

	cl_assert_equal_i(7, exp.hunks);

	cl_assert_equal_i(9, exp.lines);
	cl_assert_equal_i(2, exp.line_ctxt);
	cl_assert_equal_i(2, exp.line_adds);
	cl_assert_equal_i(5, exp.line_dels);
}

void test_diff_index__not_in_head_conflicted(void)
{
	const char *a_commit = "26a125ee1bf"; /* the current HEAD */
	git_index_entry theirs = {{0}};
	git_index *index;
	git_diff *diff;
	const git_diff_delta *delta;

	git_tree *a = resolve_commit_oid_to_tree(g_repo, a_commit);

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_read_tree(index, a));

	theirs.path = "file_not_in_head";
	theirs.mode = GIT_FILEMODE_BLOB;
	git_oid_fromstr(&theirs.id, "2bd0a343aeef7a2cf0d158478966a6e587ff3863");
	cl_git_pass(git_index_conflict_add(index, NULL, NULL, &theirs));

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, a, index, NULL));

	cl_assert_equal_i(git_diff_num_deltas(diff), 1);
	delta = git_diff_get_delta(diff, 0);
	cl_assert_equal_i(delta->status, GIT_DELTA_CONFLICTED);

	git_diff_free(diff);
	git_index_free(index);
	git_tree_free(a);
}

void test_diff_index__to_index(void)
{
	const char *a_commit = "26a125ee1bf"; /* the current HEAD */
	git_tree *old_tree;
	git_index *old_index;
	git_index *new_index;
	git_diff *diff;
	diff_expects exp;

	cl_git_pass(git_index_new(&old_index));
	old_tree = resolve_commit_oid_to_tree(g_repo, a_commit);
	cl_git_pass(git_index_read_tree(old_index, old_tree));

	cl_git_pass(git_repository_index(&new_index, g_repo));

	cl_git_pass(git_diff_index_to_index(&diff, g_repo, old_index, new_index, NULL));

	memset(&exp, 0, sizeof(diff_expects));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(8, exp.files);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(0, exp.file_status[GIT_DELTA_CONFLICTED]);

	git_diff_free(diff);
	git_index_free(new_index);
	git_index_free(old_index);
	git_tree_free(old_tree);
}
