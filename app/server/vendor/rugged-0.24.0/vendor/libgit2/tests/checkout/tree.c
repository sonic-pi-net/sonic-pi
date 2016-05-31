#include "clar_libgit2.h"
#include "checkout_helpers.h"

#include "git2/checkout.h"
#include "repository.h"
#include "buffer.h"
#include "fileops.h"

static git_repository *g_repo;
static git_checkout_options g_opts;
static git_object *g_object;

void test_checkout_tree__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");

	GIT_INIT_STRUCTURE(&g_opts, GIT_CHECKOUT_OPTIONS_VERSION);
	g_opts.checkout_strategy = GIT_CHECKOUT_FORCE;
}

void test_checkout_tree__cleanup(void)
{
	git_object_free(g_object);
	g_object = NULL;

	cl_git_sandbox_cleanup();

	if (git_path_isdir("alternative"))
		git_futils_rmdir_r("alternative", NULL, GIT_RMDIR_REMOVE_FILES);
}

void test_checkout_tree__cannot_checkout_a_non_treeish(void)
{
	/* blob */
	cl_git_pass(git_revparse_single(&g_object, g_repo, "a71586c1dfe8a71c6cbf6c129f404c5642ff31bd"));
	cl_git_fail(git_checkout_tree(g_repo, g_object, NULL));
}

void test_checkout_tree__can_checkout_a_subdirectory_from_a_commit(void)
{
	char *entries[] = { "ab/de/" };

	g_opts.paths.strings = entries;
	g_opts.paths.count = 1;

	cl_git_pass(git_revparse_single(&g_object, g_repo, "subtrees"));

	cl_assert_equal_i(false, git_path_isdir("./testrepo/ab/"));

	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));

	cl_assert_equal_i(true, git_path_isfile("./testrepo/ab/de/2.txt"));
	cl_assert_equal_i(true, git_path_isfile("./testrepo/ab/de/fgh/1.txt"));
}

void test_checkout_tree__can_checkout_and_remove_directory(void)
{
	cl_assert_equal_i(false, git_path_isdir("./testrepo/ab/"));

	/* Checkout brach "subtrees" and update HEAD, so that HEAD matches the
	 * current working tree
	 */
	cl_git_pass(git_revparse_single(&g_object, g_repo, "subtrees"));
	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));

	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/subtrees"));

	cl_assert_equal_i(true, git_path_isdir("./testrepo/ab/"));
	cl_assert_equal_i(true, git_path_isfile("./testrepo/ab/de/2.txt"));
	cl_assert_equal_i(true, git_path_isfile("./testrepo/ab/de/fgh/1.txt"));

	git_object_free(g_object);
	g_object = NULL;

	/* Checkout brach "master" and update HEAD, so that HEAD matches the
	 * current working tree
	 */
	cl_git_pass(git_revparse_single(&g_object, g_repo, "master"));
	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));

	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/master"));

	/* This directory should no longer exist */
	cl_assert_equal_i(false, git_path_isdir("./testrepo/ab/"));
}

void test_checkout_tree__can_checkout_a_subdirectory_from_a_subtree(void)
{
	char *entries[] = { "de/" };

	g_opts.paths.strings = entries;
	g_opts.paths.count = 1;

	cl_git_pass(git_revparse_single(&g_object, g_repo, "subtrees:ab"));

	cl_assert_equal_i(false, git_path_isdir("./testrepo/de/"));

	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));

	cl_assert_equal_i(true, git_path_isfile("./testrepo/de/2.txt"));
	cl_assert_equal_i(true, git_path_isfile("./testrepo/de/fgh/1.txt"));
}

static void progress(const char *path, size_t cur, size_t tot, void *payload)
{
	bool *was_called = (bool*)payload;
	GIT_UNUSED(path); GIT_UNUSED(cur); GIT_UNUSED(tot);
	*was_called = true;
}

void test_checkout_tree__calls_progress_callback(void)
{
	bool was_called = 0;

	g_opts.progress_cb = progress;
	g_opts.progress_payload = &was_called;

	cl_git_pass(git_revparse_single(&g_object, g_repo, "master"));

	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));

	cl_assert_equal_i(was_called, true);
}

void test_checkout_tree__doesnt_write_unrequested_files_to_worktree(void)
{
	git_oid master_oid;
	git_oid chomped_oid;
	git_commit* p_master_commit;
	git_commit* p_chomped_commit;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	git_oid_fromstr(&master_oid, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	git_oid_fromstr(&chomped_oid, "e90810b8df3e80c413d903f631643c716887138d");
	cl_git_pass(git_commit_lookup(&p_master_commit, g_repo, &master_oid));
	cl_git_pass(git_commit_lookup(&p_chomped_commit, g_repo, &chomped_oid));

	/* GIT_CHECKOUT_NONE should not add any file to the working tree from the
	 * index as it is supposed to be a dry run.
	 */
	opts.checkout_strategy = GIT_CHECKOUT_NONE;
	git_checkout_tree(g_repo, (git_object*)p_chomped_commit, &opts);
	cl_assert_equal_i(false, git_path_isfile("testrepo/readme.txt"));

	git_commit_free(p_master_commit);
	git_commit_free(p_chomped_commit);
}

void test_checkout_tree__can_switch_branches(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid oid;
	git_object *obj = NULL;

	assert_on_branch(g_repo, "master");

	/* do first checkout with FORCE because we don't know if testrepo
	 * base data is clean for a checkout or not
	 */
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/dir"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/dir"));

	cl_assert(git_path_isfile("testrepo/README"));
	cl_assert(git_path_isfile("testrepo/branch_file.txt"));
	cl_assert(git_path_isfile("testrepo/new.txt"));
	cl_assert(git_path_isfile("testrepo/a/b.txt"));

	cl_assert(!git_path_isdir("testrepo/ab"));

	assert_on_branch(g_repo, "dir");

	git_object_free(obj);

	/* do second checkout safe because we should be clean after first */
	opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/subtrees"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/subtrees"));

	cl_assert(git_path_isfile("testrepo/README"));
	cl_assert(git_path_isfile("testrepo/branch_file.txt"));
	cl_assert(git_path_isfile("testrepo/new.txt"));
	cl_assert(git_path_isfile("testrepo/ab/4.txt"));
	cl_assert(git_path_isfile("testrepo/ab/c/3.txt"));
	cl_assert(git_path_isfile("testrepo/ab/de/2.txt"));
	cl_assert(git_path_isfile("testrepo/ab/de/fgh/1.txt"));

	cl_assert(!git_path_isdir("testrepo/a"));

	assert_on_branch(g_repo, "subtrees");

	git_object_free(obj);
}

void test_checkout_tree__can_remove_untracked(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_REMOVE_UNTRACKED;

	cl_git_mkfile("testrepo/untracked_file", "as you wish");
	cl_assert(git_path_isfile("testrepo/untracked_file"));

	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_assert(!git_path_isfile("testrepo/untracked_file"));
}

void test_checkout_tree__can_remove_ignored(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	int ignored = 0;

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_REMOVE_IGNORED;

	cl_git_mkfile("testrepo/ignored_file", "as you wish");

	cl_git_pass(git_ignore_add_rule(g_repo, "ignored_file\n"));

	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "ignored_file"));
	cl_assert_equal_i(1, ignored);

	cl_assert(git_path_isfile("testrepo/ignored_file"));

	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_assert(!git_path_isfile("testrepo/ignored_file"));
}

static int checkout_tree_with_blob_ignored_in_workdir(int strategy, bool isdir)
{
	git_oid oid;
	git_object *obj = NULL;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	int ignored = 0, error;

	assert_on_branch(g_repo, "master");

	/* do first checkout with FORCE because we don't know if testrepo
	 * base data is clean for a checkout or not
	 */
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/dir"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/dir"));

	cl_assert(git_path_isfile("testrepo/README"));
	cl_assert(git_path_isfile("testrepo/branch_file.txt"));
	cl_assert(git_path_isfile("testrepo/new.txt"));
	cl_assert(git_path_isfile("testrepo/a/b.txt"));

	cl_assert(!git_path_isdir("testrepo/ab"));

	assert_on_branch(g_repo, "dir");

	git_object_free(obj);

	opts.checkout_strategy = strategy;

	if (isdir) {
		cl_must_pass(p_mkdir("testrepo/ab", 0777));
		cl_must_pass(p_mkdir("testrepo/ab/4.txt", 0777));

		cl_git_mkfile("testrepo/ab/4.txt/file1.txt", "as you wish");
		cl_git_mkfile("testrepo/ab/4.txt/file2.txt", "foo bar foo");
		cl_git_mkfile("testrepo/ab/4.txt/file3.txt", "inky blinky pinky clyde");

		cl_assert(git_path_isdir("testrepo/ab/4.txt"));
	} else {
		cl_must_pass(p_mkdir("testrepo/ab", 0777));
		cl_git_mkfile("testrepo/ab/4.txt", "as you wish");

		cl_assert(git_path_isfile("testrepo/ab/4.txt"));
	}

	cl_git_pass(git_ignore_add_rule(g_repo, "ab/4.txt\n"));

	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "ab/4.txt"));
	cl_assert_equal_i(1, ignored);

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/subtrees"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	error = git_checkout_tree(g_repo, obj, &opts);

	git_object_free(obj);

	return error;
}

void test_checkout_tree__conflict_on_ignored_when_not_overwriting(void)
{
	int error;

	cl_git_fail(error = checkout_tree_with_blob_ignored_in_workdir(
		GIT_CHECKOUT_SAFE | GIT_CHECKOUT_DONT_OVERWRITE_IGNORED, false));

	cl_assert_equal_i(GIT_ECONFLICT, error);
}

void test_checkout_tree__can_overwrite_ignored_by_default(void)
{
	cl_git_pass(checkout_tree_with_blob_ignored_in_workdir(GIT_CHECKOUT_SAFE, false));

	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/subtrees"));

	cl_assert(git_path_isfile("testrepo/ab/4.txt"));

	assert_on_branch(g_repo, "subtrees");
}

void test_checkout_tree__conflict_on_ignored_folder_when_not_overwriting(void)
{
	int error;

	cl_git_fail(error = checkout_tree_with_blob_ignored_in_workdir(
		GIT_CHECKOUT_SAFE | GIT_CHECKOUT_DONT_OVERWRITE_IGNORED, true));

	cl_assert_equal_i(GIT_ECONFLICT, error);
}

void test_checkout_tree__can_overwrite_ignored_folder_by_default(void)
{
	cl_git_pass(checkout_tree_with_blob_ignored_in_workdir(GIT_CHECKOUT_SAFE, true));

	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/subtrees"));

	cl_assert(git_path_isfile("testrepo/ab/4.txt"));

	assert_on_branch(g_repo, "subtrees");

}

void test_checkout_tree__can_update_only(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid oid;
	git_object *obj = NULL;

	/* first let's get things into a known state - by checkout out the HEAD */

	assert_on_branch(g_repo, "master");

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_assert(!git_path_isdir("testrepo/a"));

	check_file_contents_nocr("testrepo/branch_file.txt", "hi\nbye!\n");

	/* now checkout branch but with update only */

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_UPDATE_ONLY;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/dir"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/dir"));

	assert_on_branch(g_repo, "dir");

	/* this normally would have been created (which was tested separately in
	 * the test_checkout_tree__can_switch_branches test), but with
	 * UPDATE_ONLY it will not have been created.
	 */
	cl_assert(!git_path_isdir("testrepo/a"));

	/* but this file still should have been updated */
	check_file_contents_nocr("testrepo/branch_file.txt", "hi\n");

	git_object_free(obj);
}

void test_checkout_tree__can_checkout_with_pattern(void)
{
	char *entries[] = { "[l-z]*.txt" };

	/* reset to beginning of history (i.e. just a README file) */

	g_opts.checkout_strategy =
		GIT_CHECKOUT_FORCE | GIT_CHECKOUT_REMOVE_UNTRACKED;

	cl_git_pass(git_revparse_single(&g_object, g_repo, "8496071c1b46c854b31185ea97743be6a8774479"));

	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));
	cl_git_pass(
		git_repository_set_head_detached(g_repo, git_object_id(g_object)));

	git_object_free(g_object);
	g_object = NULL;

	cl_assert(git_path_exists("testrepo/README"));
	cl_assert(!git_path_exists("testrepo/branch_file.txt"));
	cl_assert(!git_path_exists("testrepo/link_to_new.txt"));
	cl_assert(!git_path_exists("testrepo/new.txt"));

	/* now to a narrow patterned checkout */

	g_opts.checkout_strategy = GIT_CHECKOUT_SAFE;
	g_opts.paths.strings = entries;
	g_opts.paths.count = 1;

	cl_git_pass(git_revparse_single(&g_object, g_repo, "refs/heads/master"));

	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));

	cl_assert(git_path_exists("testrepo/README"));
	cl_assert(!git_path_exists("testrepo/branch_file.txt"));
	cl_assert(git_path_exists("testrepo/link_to_new.txt"));
	cl_assert(git_path_exists("testrepo/new.txt"));
}

void test_checkout_tree__can_disable_pattern_match(void)
{
	char *entries[] = { "b*.txt" };

	/* reset to beginning of history (i.e. just a README file) */

	g_opts.checkout_strategy =
		GIT_CHECKOUT_FORCE | GIT_CHECKOUT_REMOVE_UNTRACKED;

	cl_git_pass(git_revparse_single(&g_object, g_repo, "8496071c1b46c854b31185ea97743be6a8774479"));

	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));
	cl_git_pass(
		git_repository_set_head_detached(g_repo, git_object_id(g_object)));

	git_object_free(g_object);
	g_object = NULL;

	cl_assert(!git_path_isfile("testrepo/branch_file.txt"));

	/* now to a narrow patterned checkout, but disable pattern */

	g_opts.checkout_strategy =
		GIT_CHECKOUT_SAFE |
		GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH;
	g_opts.paths.strings = entries;
	g_opts.paths.count = 1;

	cl_git_pass(git_revparse_single(&g_object, g_repo, "refs/heads/master"));

	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));

	cl_assert(!git_path_isfile("testrepo/branch_file.txt"));

	/* let's try that again, but allow the pattern match */

	g_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));

	cl_assert(git_path_isfile("testrepo/branch_file.txt"));
}

void assert_conflict(
	const char *entry_path,
	const char *new_content,
	const char *parent_sha,
	const char *commit_sha)
{
	git_index *index;
	git_object *hack_tree;
	git_reference *branch, *head;
	git_buf file_path = GIT_BUF_INIT;

	cl_git_pass(git_repository_index(&index, g_repo));

	/* Create a branch pointing at the parent */
	cl_git_pass(git_revparse_single(&g_object, g_repo, parent_sha));
	cl_git_pass(git_branch_create(&branch, g_repo,
		"potential_conflict", (git_commit *)g_object, 0));

	/* Make HEAD point to this branch */
	cl_git_pass(git_reference_symbolic_create(
		&head, g_repo, "HEAD", git_reference_name(branch), 1, NULL));
	git_reference_free(head);
	git_reference_free(branch);

	/* Checkout the parent */
	g_opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));

	/* Hack-ishy workaound to ensure *all* the index entries
	 * match the content of the tree
	 */
	cl_git_pass(git_object_peel(&hack_tree, g_object, GIT_OBJ_TREE));
	cl_git_pass(git_index_read_tree(index, (git_tree *)hack_tree));
	git_object_free(hack_tree);
	git_object_free(g_object);
	g_object = NULL;

	/* Create a conflicting file */
	cl_git_pass(git_buf_joinpath(&file_path, "./testrepo", entry_path));
	cl_git_mkfile(git_buf_cstr(&file_path), new_content);
	git_buf_free(&file_path);

	/* Trying to checkout the original commit */
	cl_git_pass(git_revparse_single(&g_object, g_repo, commit_sha));

	g_opts.checkout_strategy = GIT_CHECKOUT_SAFE;
	cl_assert_equal_i(
		GIT_ECONFLICT, git_checkout_tree(g_repo, g_object, &g_opts));

	/* Stage the conflicting change */
	cl_git_pass(git_index_add_bypath(index, entry_path));
	cl_git_pass(git_index_write(index));
	git_index_free(index);

	cl_assert_equal_i(
		GIT_ECONFLICT, git_checkout_tree(g_repo, g_object, &g_opts));
}

void test_checkout_tree__checking_out_a_conflicting_type_change_returns_ECONFLICT(void)
{
	/*
	 * 099faba adds a symlink named 'link_to_new.txt'
	 * a65fedf is the parent of 099faba
	 */

	assert_conflict("link_to_new.txt", "old.txt", "a65fedf", "099faba");
}

void test_checkout_tree__checking_out_a_conflicting_type_change_returns_ECONFLICT_2(void)
{
	/*
	 * cf80f8d adds a directory named 'a/'
	 * a4a7dce is the parent of cf80f8d
	 */

	assert_conflict("a", "hello\n", "a4a7dce", "cf80f8d");
}

void test_checkout_tree__checking_out_a_conflicting_content_change_returns_ECONFLICT(void)
{
	/*
	 * c47800c adds a symlink named 'branch_file.txt'
	 * 5b5b025 is the parent of 763d71a
	 */

	assert_conflict("branch_file.txt", "hello\n", "5b5b025", "c47800c");
}

void test_checkout_tree__donot_update_deleted_file_by_default(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid old_id, new_id;
	git_commit *old_commit = NULL, *new_commit = NULL;
	git_index *index = NULL;
	checkout_counts ct;

	opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	memset(&ct, 0, sizeof(ct));
	opts.notify_flags = GIT_CHECKOUT_NOTIFY_ALL;
	opts.notify_cb = checkout_count_callback;
	opts.notify_payload = &ct;

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(git_oid_fromstr(&old_id, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644"));
	cl_git_pass(git_commit_lookup(&old_commit, g_repo, &old_id));
	cl_git_pass(git_reset(g_repo, (git_object *)old_commit, GIT_RESET_HARD, NULL));

	cl_git_pass(p_unlink("testrepo/branch_file.txt"));
	cl_git_pass(git_index_remove_bypath(index ,"branch_file.txt"));
	cl_git_pass(git_index_write(index));

	cl_assert(!git_path_exists("testrepo/branch_file.txt"));

	cl_git_pass(git_oid_fromstr(&new_id, "099fabac3a9ea935598528c27f866e34089c2eff"));
	cl_git_pass(git_commit_lookup(&new_commit, g_repo, &new_id));


	cl_git_fail(git_checkout_tree(g_repo, (git_object *)new_commit, &opts));

	cl_assert_equal_i(1, ct.n_conflicts);
	cl_assert_equal_i(1, ct.n_updates);

	git_commit_free(old_commit);
	git_commit_free(new_commit);
	git_index_free(index);
}

struct checkout_cancel_at {
	const char *filename;
	int error;
	int count;
};

static int checkout_cancel_cb(
	git_checkout_notify_t why,
	const char *path,
	const git_diff_file *b,
	const git_diff_file *t,
	const git_diff_file *w,
	void *payload)
{
	struct checkout_cancel_at *ca = payload;

	GIT_UNUSED(why); GIT_UNUSED(b); GIT_UNUSED(t); GIT_UNUSED(w);

	ca->count++;

	if (!strcmp(path, ca->filename))
		return ca->error;

	return 0;
}

void test_checkout_tree__can_cancel_checkout_from_notify(void)
{
	struct checkout_cancel_at ca;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid oid;
	git_object *obj = NULL;

	assert_on_branch(g_repo, "master");

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/dir"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	ca.filename = "new.txt";
	ca.error = -5555;
	ca.count = 0;

	opts.notify_flags = GIT_CHECKOUT_NOTIFY_UPDATED;
	opts.notify_cb = checkout_cancel_cb;
	opts.notify_payload = &ca;
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_assert(!git_path_exists("testrepo/new.txt"));

	cl_git_fail_with(git_checkout_tree(g_repo, obj, &opts), -5555);

	cl_assert(!git_path_exists("testrepo/new.txt"));

	/* on case-insensitive FS = a/b.txt, branch_file.txt, new.txt */
	/* on case-sensitive FS   = README, then above */

	if (git_path_exists("testrepo/.git/CoNfIg")) /* case insensitive */
		cl_assert_equal_i(3, ca.count);
	else
		cl_assert_equal_i(4, ca.count);

	/* and again with a different stopping point and return code */
	ca.filename = "README";
	ca.error = 123;
	ca.count = 0;

	cl_git_fail_with(git_checkout_tree(g_repo, obj, &opts), 123);

	cl_assert(!git_path_exists("testrepo/new.txt"));

	if (git_path_exists("testrepo/.git/CoNfIg")) /* case insensitive */
		cl_assert_equal_i(4, ca.count);
	else
		cl_assert_equal_i(1, ca.count);

	git_object_free(obj);
}

void test_checkout_tree__can_checkout_with_last_workdir_item_missing(void)
{
	git_index *index = NULL;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid tree_id, commit_id;
	git_tree *tree = NULL;
	git_commit *commit = NULL;

	git_repository_index(&index, g_repo);

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_reference_name_to_id(&commit_id, g_repo, "refs/heads/master"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &commit_id));

	cl_git_pass(git_checkout_tree(g_repo, (git_object *)commit, &opts));
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/master"));

	cl_git_pass(p_mkdir("./testrepo/this-is-dir", 0777));
	cl_git_mkfile("./testrepo/this-is-dir/contained_file", "content\n");

	cl_git_pass(git_index_add_bypath(index, "this-is-dir/contained_file"));
	git_index_write_tree(&tree_id, index);
	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

	cl_git_pass(p_unlink("./testrepo/this-is-dir/contained_file"));

	opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	opts.checkout_strategy = 1;
	git_checkout_tree(g_repo, (git_object *)tree, &opts);

	git_tree_free(tree);
	git_commit_free(commit);
	git_index_free(index);
}

void test_checkout_tree__issue_1397(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	const char *partial_oid = "8a7ef04";
	git_object *tree = NULL;

	test_checkout_tree__cleanup(); /* cleanup default checkout */

	g_repo = cl_git_sandbox_init("issue_1397");

	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	cl_git_pass(git_revparse_single(&tree, g_repo, partial_oid));

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_checkout_tree(g_repo, tree, &opts));

	check_file_contents("./issue_1397/crlf_file.txt", "first line\r\nsecond line\r\nboth with crlf");

	git_object_free(tree);
}

void test_checkout_tree__can_write_to_empty_dirs(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid oid;
	git_object *obj = NULL;

	assert_on_branch(g_repo, "master");

	cl_git_pass(p_mkdir("testrepo/a", 0777));

	/* do first checkout with FORCE because we don't know if testrepo
	 * base data is clean for a checkout or not
	 */
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/dir"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));

	cl_assert(git_path_isfile("testrepo/a/b.txt"));

	git_object_free(obj);
}

void test_checkout_tree__fails_when_dir_in_use(void)
{
#ifdef GIT_WIN32
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid oid;
	git_object *obj = NULL;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/dir"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));

	cl_assert(git_path_isfile("testrepo/a/b.txt"));

	git_object_free(obj);

	cl_git_pass(p_chdir("testrepo/a"));

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/master"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_fail(git_checkout_tree(g_repo, obj, &opts));

	cl_git_pass(p_chdir("../.."));

	cl_assert(git_path_is_empty_dir("testrepo/a"));

	git_object_free(obj);
#endif
}

void test_checkout_tree__can_continue_when_dir_in_use(void)
{
#ifdef GIT_WIN32
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid oid;
	git_object *obj = NULL;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE |
		GIT_CHECKOUT_SKIP_LOCKED_DIRECTORIES;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/dir"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));

	cl_assert(git_path_isfile("testrepo/a/b.txt"));

	git_object_free(obj);

	cl_git_pass(p_chdir("testrepo/a"));

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/master"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));

	cl_git_pass(p_chdir("../.."));

	cl_assert(git_path_is_empty_dir("testrepo/a"));

	git_object_free(obj);
#endif
}

void test_checkout_tree__target_directory_from_bare(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid oid;
	checkout_counts cts;
	memset(&cts, 0, sizeof(cts));

	test_checkout_tree__cleanup(); /* cleanup default checkout */

	g_repo = cl_git_sandbox_init("testrepo.git");
	cl_assert(git_repository_is_bare(g_repo));

	opts.checkout_strategy = GIT_CHECKOUT_SAFE |
		GIT_CHECKOUT_RECREATE_MISSING;

	opts.notify_flags = GIT_CHECKOUT_NOTIFY_ALL;
	opts.notify_cb = checkout_count_callback;
	opts.notify_payload = &cts;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "HEAD"));
	cl_git_pass(git_object_lookup(&g_object, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_fail(git_checkout_tree(g_repo, g_object, &opts));

	opts.target_directory = "alternative";
	cl_assert(!git_path_isdir("alternative"));

	cl_git_pass(git_checkout_tree(g_repo, g_object, &opts));

	cl_assert_equal_i(0, cts.n_untracked);
	cl_assert_equal_i(0, cts.n_ignored);
	cl_assert_equal_i(3, cts.n_updates);

	check_file_contents_nocr("./alternative/README", "hey there\n");
	check_file_contents_nocr("./alternative/branch_file.txt", "hi\nbye!\n");
	check_file_contents_nocr("./alternative/new.txt", "my new file\n");

	cl_git_pass(git_futils_rmdir_r(
		"alternative", NULL, GIT_RMDIR_REMOVE_FILES));
}

void test_checkout_tree__extremely_long_file_name(void)
{
	// A utf-8 string with 83 characters, but 249 bytes.
	const char *longname = "\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97\xe5\x8f\x97";
	char path[1024];

	g_opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	cl_git_pass(git_revparse_single(&g_object, g_repo, "long-file-name"));
	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));

	sprintf(path, "testrepo/%s.txt", longname);
	cl_assert(git_path_exists(path));

	git_object_free(g_object);
	cl_git_pass(git_revparse_single(&g_object, g_repo, "master"));
	cl_git_pass(git_checkout_tree(g_repo, g_object, &g_opts));
	cl_assert(!git_path_exists(path));
}

static void create_conflict(const char *path)
{
	git_index *index;
	git_index_entry entry;

	cl_git_pass(git_repository_index(&index, g_repo));

	memset(&entry, 0x0, sizeof(git_index_entry));
	entry.mode = 0100644;
	GIT_IDXENTRY_STAGE_SET(&entry, 1);
	git_oid_fromstr(&entry.id, "d427e0b2e138501a3d15cc376077a3631e15bd46");
	entry.path = path;
	cl_git_pass(git_index_add(index, &entry));

	GIT_IDXENTRY_STAGE_SET(&entry, 2);
	git_oid_fromstr(&entry.id, "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf");
	cl_git_pass(git_index_add(index, &entry));

	GIT_IDXENTRY_STAGE_SET(&entry, 3);
	git_oid_fromstr(&entry.id, "2bd0a343aeef7a2cf0d158478966a6e587ff3863");
	cl_git_pass(git_index_add(index, &entry));

	git_index_write(index);
	git_index_free(index);
}

void test_checkout_tree__fails_when_conflicts_exist_in_index(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid oid;
	git_object *obj = NULL;

	opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "HEAD"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	create_conflict("conflicts.txt");

	cl_git_fail(git_checkout_tree(g_repo, obj, &opts));

	git_object_free(obj);
}

void test_checkout_tree__filemode_preserved_in_index(void)
{
	git_oid executable_oid;
	git_commit *commit;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;
	const git_index_entry *entry;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_repository_index(&index, g_repo));

	/* test a freshly added executable */
	cl_git_pass(git_oid_fromstr(&executable_oid, "afe4393b2b2a965f06acf2ca9658eaa01e0cd6b6"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &executable_oid));

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));
	cl_assert(entry = git_index_get_bypath(index, "executable.txt", 0));
	cl_assert(GIT_PERMS_IS_EXEC(entry->mode));

	git_commit_free(commit);


	/* Now start with a commit which has a text file */
	cl_git_pass(git_oid_fromstr(&executable_oid, "cf80f8de9f1185bf3a05f993f6121880dd0cfbc9"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &executable_oid));

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));
	cl_assert(entry = git_index_get_bypath(index, "a/b.txt", 0));
	cl_assert(!GIT_PERMS_IS_EXEC(entry->mode));

	git_commit_free(commit);


	/* And then check out to a commit which converts the text file to an executable */
	cl_git_pass(git_oid_fromstr(&executable_oid, "144344043ba4d4a405da03de3844aa829ae8be0e"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &executable_oid));

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));
	cl_assert(entry = git_index_get_bypath(index, "a/b.txt", 0));
	cl_assert(GIT_PERMS_IS_EXEC(entry->mode));

	git_commit_free(commit);


	/* Finally, check out the text file again and check that the exec bit is cleared */
	cl_git_pass(git_oid_fromstr(&executable_oid, "cf80f8de9f1185bf3a05f993f6121880dd0cfbc9"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &executable_oid));

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));
	cl_assert(entry = git_index_get_bypath(index, "a/b.txt", 0));
	cl_assert(!GIT_PERMS_IS_EXEC(entry->mode));

	git_commit_free(commit);


	git_index_free(index);
}

mode_t read_filemode(const char *path)
{
	git_buf fullpath = GIT_BUF_INIT;
	struct stat st;
	mode_t result;

	git_buf_joinpath(&fullpath, "testrepo", path);
	cl_must_pass(p_stat(fullpath.ptr, &st));

	result = GIT_PERMS_IS_EXEC(st.st_mode) ?
		GIT_FILEMODE_BLOB_EXECUTABLE : GIT_FILEMODE_BLOB;

	git_buf_free(&fullpath);

	return result;
}

void test_checkout_tree__filemode_preserved_in_workdir(void)
{
#ifndef GIT_WIN32
	git_oid executable_oid;
	git_commit *commit;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	/* test a freshly added executable */
	cl_git_pass(git_oid_fromstr(&executable_oid, "afe4393b2b2a965f06acf2ca9658eaa01e0cd6b6"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &executable_oid));

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));
	cl_assert(GIT_PERMS_IS_EXEC(read_filemode("executable.txt")));

	git_commit_free(commit);


	/* Now start with a commit which has a text file */
	cl_git_pass(git_oid_fromstr(&executable_oid, "cf80f8de9f1185bf3a05f993f6121880dd0cfbc9"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &executable_oid));

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));
	cl_assert(!GIT_PERMS_IS_EXEC(read_filemode("a/b.txt")));

	git_commit_free(commit);


	/* And then check out to a commit which converts the text file to an executable */
	cl_git_pass(git_oid_fromstr(&executable_oid, "144344043ba4d4a405da03de3844aa829ae8be0e"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &executable_oid));

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));
	cl_assert(GIT_PERMS_IS_EXEC(read_filemode("a/b.txt")));

	git_commit_free(commit);


	/* Finally, check out the text file again and check that the exec bit is cleared */
	cl_git_pass(git_oid_fromstr(&executable_oid, "cf80f8de9f1185bf3a05f993f6121880dd0cfbc9"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &executable_oid));

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));
	cl_assert(!GIT_PERMS_IS_EXEC(read_filemode("a/b.txt")));

	git_commit_free(commit);
#endif
}

void test_checkout_tree__removes_conflicts(void)
{
	git_oid commit_id;
	git_commit *commit;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;
	
	cl_git_pass(git_oid_fromstr(&commit_id, "afe4393b2b2a965f06acf2ca9658eaa01e0cd6b6"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &commit_id));

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_remove(index, "executable.txt", 0));

	create_conflict("executable.txt");
	cl_git_mkfile("testrepo/executable.txt", "This is the conflict file.\n");

	create_conflict("other.txt");
	cl_git_mkfile("testrepo/other.txt", "This is another conflict file.\n");

	git_index_write(index);

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));

	cl_assert_equal_p(NULL, git_index_get_bypath(index, "executable.txt", 1));
	cl_assert_equal_p(NULL, git_index_get_bypath(index, "executable.txt", 2));
	cl_assert_equal_p(NULL, git_index_get_bypath(index, "executable.txt", 3));

	cl_assert_equal_p(NULL, git_index_get_bypath(index, "other.txt", 1));
	cl_assert_equal_p(NULL, git_index_get_bypath(index, "other.txt", 2));
	cl_assert_equal_p(NULL, git_index_get_bypath(index, "other.txt", 3));

	cl_assert(!git_path_exists("testrepo/other.txt"));

	git_commit_free(commit);
	git_index_free(index);
}


void test_checkout_tree__removes_conflicts_only_by_pathscope(void)
{
	git_oid commit_id;
	git_commit *commit;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;
	const char *path = "executable.txt";
	
	cl_git_pass(git_oid_fromstr(&commit_id, "afe4393b2b2a965f06acf2ca9658eaa01e0cd6b6"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &commit_id));

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	opts.paths.count = 1;
	opts.paths.strings = (char **)&path;

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_remove(index, "executable.txt", 0));

	create_conflict("executable.txt");
	cl_git_mkfile("testrepo/executable.txt", "This is the conflict file.\n");

	create_conflict("other.txt");
	cl_git_mkfile("testrepo/other.txt", "This is another conflict file.\n");

	git_index_write(index);

	cl_git_pass(git_checkout_tree(g_repo, (const git_object *)commit, &opts));

	cl_assert_equal_p(NULL, git_index_get_bypath(index, "executable.txt", 1));
	cl_assert_equal_p(NULL, git_index_get_bypath(index, "executable.txt", 2));
	cl_assert_equal_p(NULL, git_index_get_bypath(index, "executable.txt", 3));

	cl_assert(git_index_get_bypath(index, "other.txt", 1) != NULL);
	cl_assert(git_index_get_bypath(index, "other.txt", 2) != NULL);
	cl_assert(git_index_get_bypath(index, "other.txt", 3) != NULL);

	cl_assert(git_path_exists("testrepo/other.txt"));

	git_commit_free(commit);
	git_index_free(index);
}

void test_checkout_tree__case_changing_rename(void)
{
	git_index *index;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid master_id, dir_commit_id, tree_id, commit_id;
	git_commit *master_commit, *dir_commit;
	git_tree *tree;
	git_signature *signature;
	const git_index_entry *index_entry;
	bool case_sensitive;

	assert_on_branch(g_repo, "master");

	cl_git_pass(git_repository_index(&index, g_repo));

	/* Switch branches and perform a case-changing rename */

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_reference_name_to_id(&dir_commit_id, g_repo, "refs/heads/dir"));
	cl_git_pass(git_commit_lookup(&dir_commit, g_repo, &dir_commit_id));

	cl_git_pass(git_checkout_tree(g_repo, (git_object *)dir_commit, &opts));
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/dir"));

	cl_assert(git_path_isfile("testrepo/README"));
	case_sensitive = !git_path_isfile("testrepo/readme");

	cl_assert(index_entry = git_index_get_bypath(index, "README", 0));
	cl_assert_equal_s("README", index_entry->path);

	cl_git_pass(git_index_remove_bypath(index, "README"));
	cl_git_pass(p_rename("testrepo/README", "testrepo/__readme__"));
	cl_git_pass(p_rename("testrepo/__readme__", "testrepo/readme"));
	cl_git_append2file("testrepo/readme", "An addendum...");
	cl_git_pass(git_index_add_bypath(index, "readme"));

	cl_git_pass(git_index_write(index));

	cl_git_pass(git_index_write_tree(&tree_id, index));
	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

	cl_git_pass(git_signature_new(&signature, "Renamer", "rename@contoso.com", time(NULL), 0));

	cl_git_pass(git_commit_create(&commit_id, g_repo, "refs/heads/dir", signature, signature, NULL, "case-changing rename", tree, 1, (const git_commit **)&dir_commit));

	cl_assert(git_path_isfile("testrepo/readme"));
	if (case_sensitive)
		cl_assert(!git_path_isfile("testrepo/README"));

	cl_assert(index_entry = git_index_get_bypath(index, "readme", 0));
	cl_assert_equal_s("readme", index_entry->path);

	/* Switching back to master should rename readme -> README */
	opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_name_to_id(&master_id, g_repo, "refs/heads/master"));
	cl_git_pass(git_commit_lookup(&master_commit, g_repo, &master_id));

	cl_git_pass(git_checkout_tree(g_repo, (git_object *)master_commit, &opts));
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/master"));
	
	assert_on_branch(g_repo, "master");

	cl_assert(git_path_isfile("testrepo/README"));
	if (case_sensitive)
		cl_assert(!git_path_isfile("testrepo/readme"));

	cl_assert(index_entry = git_index_get_bypath(index, "README", 0));
	cl_assert_equal_s("README", index_entry->path);

	git_index_free(index);
	git_signature_free(signature);
	git_tree_free(tree);
	git_commit_free(dir_commit);
	git_commit_free(master_commit);
}

void perfdata_cb(const git_checkout_perfdata *in, void *payload)
{
	memcpy(payload, in, sizeof(git_checkout_perfdata));
}

void test_checkout_tree__can_collect_perfdata(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid oid;
	git_object *obj = NULL;
	git_checkout_perfdata perfdata = {0};

	opts.perfdata_cb = perfdata_cb;
	opts.perfdata_payload = &perfdata;

	assert_on_branch(g_repo, "master");
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/dir"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));

	cl_assert(perfdata.mkdir_calls > 0);
	cl_assert(perfdata.stat_calls > 0);

	git_object_free(obj);
}

void update_attr_callback(
	const char *path,
	size_t completed_steps,
	size_t total_steps,
	void *payload)
{
	GIT_UNUSED(completed_steps);
	GIT_UNUSED(total_steps);
	GIT_UNUSED(payload);

	if (path && strcmp(path, "ident1.txt") == 0)
		cl_git_write2file("testrepo/.gitattributes",
			"*.txt ident\n", 12, O_RDWR|O_CREAT, 0666);
}

void test_checkout_tree__caches_attributes_during_checkout(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid oid;
	git_object *obj = NULL;
	git_buf ident1 = GIT_BUF_INIT, ident2 = GIT_BUF_INIT;
	char *ident_paths[] = { "ident1.txt", "ident2.txt" };

	opts.progress_cb = update_attr_callback;

	assert_on_branch(g_repo, "master");
	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	opts.paths.strings = ident_paths;
	opts.paths.count = 2;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/ident"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));

	cl_git_pass(git_futils_readbuffer(&ident1, "testrepo/ident1.txt"));
	cl_git_pass(git_futils_readbuffer(&ident2, "testrepo/ident2.txt"));

	cl_assert_equal_strn(ident1.ptr, "# $Id$", 6);
	cl_assert_equal_strn(ident2.ptr, "# $Id$", 6);

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));

	cl_git_pass(git_futils_readbuffer(&ident1, "testrepo/ident1.txt"));
	cl_git_pass(git_futils_readbuffer(&ident2, "testrepo/ident2.txt"));

	cl_assert_equal_strn(ident1.ptr, "# $Id: ", 7);
	cl_assert_equal_strn(ident2.ptr, "# $Id: ", 7);

	git_buf_free(&ident1);
	git_buf_free(&ident2);
	git_object_free(obj);
}

void test_checkout_tree__can_not_update_index(void)
{
	git_oid oid;
	git_object *head;
	unsigned int status;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;

	opts.checkout_strategy |=
		GIT_CHECKOUT_FORCE | GIT_CHECKOUT_DONT_UPDATE_INDEX;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "HEAD"));
	cl_git_pass(git_object_lookup(&head, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_reset(g_repo, head, GIT_RESET_HARD, &g_opts));

	cl_assert_equal_i(false, git_path_isdir("./testrepo/ab/"));

	cl_git_pass(git_revparse_single(&g_object, g_repo, "subtrees"));

	cl_git_pass(git_checkout_tree(g_repo, g_object, &opts));

	cl_assert_equal_i(true, git_path_isfile("./testrepo/ab/de/2.txt"));
	cl_git_pass(git_status_file(&status, g_repo, "ab/de/2.txt"));
	cl_assert_equal_i(GIT_STATUS_WT_NEW, status);

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_status_file(&status, g_repo, "ab/de/2.txt"));
	cl_assert_equal_i(GIT_STATUS_WT_NEW, status);

	git_object_free(head);
	git_index_free(index);
}

void test_checkout_tree__can_update_but_not_write_index(void)
{
	git_oid oid;
	git_object *head;
	unsigned int status;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;
	git_repository *other;

	opts.checkout_strategy |=
		GIT_CHECKOUT_FORCE | GIT_CHECKOUT_DONT_WRITE_INDEX;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "HEAD"));
	cl_git_pass(git_object_lookup(&head, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_reset(g_repo, head, GIT_RESET_HARD, &g_opts));

	cl_assert_equal_i(false, git_path_isdir("./testrepo/ab/"));

	cl_git_pass(git_revparse_single(&g_object, g_repo, "subtrees"));

	cl_git_pass(git_checkout_tree(g_repo, g_object, &opts));

	cl_assert_equal_i(true, git_path_isfile("./testrepo/ab/de/2.txt"));
	cl_git_pass(git_status_file(&status, g_repo, "ab/de/2.txt"));
	cl_assert_equal_i(GIT_STATUS_INDEX_NEW, status);

	cl_git_pass(git_repository_open(&other, "testrepo"));
	cl_git_pass(git_status_file(&status, other, "ab/de/2.txt"));
	cl_assert_equal_i(GIT_STATUS_WT_NEW, status);
	git_repository_free(other);

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_repository_open(&other, "testrepo"));
	cl_git_pass(git_status_file(&status, other, "ab/de/2.txt"));
	cl_assert_equal_i(GIT_STATUS_INDEX_NEW, status);
	git_repository_free(other);

	git_object_free(head);
	git_index_free(index);
}

/* Emulate checking out in a repo created by clone --no-checkout,
 * which would not have written an index. */
void test_checkout_tree__safe_proceeds_if_no_index(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_oid oid;
	git_object *obj = NULL;

	assert_on_branch(g_repo, "master");
	cl_must_pass(p_unlink("testrepo/.git/index"));

	/* do second checkout safe because we should be clean after first */
	opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/subtrees"));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));

	cl_git_pass(git_checkout_tree(g_repo, obj, &opts));
	cl_git_pass(git_repository_set_head(g_repo, "refs/heads/subtrees"));

	cl_assert(git_path_isfile("testrepo/README"));
	cl_assert(git_path_isfile("testrepo/branch_file.txt"));
	cl_assert(git_path_isfile("testrepo/new.txt"));
	cl_assert(git_path_isfile("testrepo/ab/4.txt"));
	cl_assert(git_path_isfile("testrepo/ab/c/3.txt"));
	cl_assert(git_path_isfile("testrepo/ab/de/2.txt"));
	cl_assert(git_path_isfile("testrepo/ab/de/fgh/1.txt"));

	cl_assert(!git_path_isdir("testrepo/a"));

	assert_on_branch(g_repo, "subtrees");

	git_object_free(obj);
}

