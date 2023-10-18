#include "clar_libgit2.h"
#include "diff_helpers.h"

static git_repository *g_repo = NULL;

void test_diff_rename__initialize(void)
{
	g_repo = cl_git_sandbox_init("renames");

	cl_repo_set_bool(g_repo, "core.autocrlf", false);
}

void test_diff_rename__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

#define INITIAL_COMMIT "31e47d8c1fa36d7f8d537b96158e3f024de0a9f2"
#define COPY_RENAME_COMMIT "2bc7f351d20b53f1c72c16c4b036e491c478c49a"
#define REWRITE_COPY_COMMIT "1c068dee5790ef1580cfc4cd670915b48d790084"
#define RENAME_MODIFICATION_COMMIT "19dd32dfb1520a64e5bbaae8dce6ef423dfa2f13"
#define REWRITE_DELETE_COMMIT "84d8efa38af7ace2b302de0adbda16b1f1cd2e1b"
#define DELETE_RENAME_COMMIT "be053a189b0bbde545e0a3f59ce00b46ad29ce0d"
#define BREAK_REWRITE_BASE_COMMIT "db98035f715427eef1f5e17f03e1801c05301e9e"
#define BREAK_REWRITE_COMMIT "7e7bfb88ba9bc65fd700fee1819cf1c317aafa56"

/*
 * Renames repo has:
 *
 * commit 31e47d8c1fa36d7f8d537b96158e3f024de0a9f2 -
 *   serving.txt     (25 lines)
 *   sevencities.txt (50 lines)
 * commit 2bc7f351d20b53f1c72c16c4b036e491c478c49a -
 *   serving.txt     -> sixserving.txt  (rename, no change, 100% match)
 *   sevencities.txt -> sevencities.txt (no change)
 *   sevencities.txt -> songofseven.txt (copy, no change, 100% match)
 * commit 1c068dee5790ef1580cfc4cd670915b48d790084
 *   songofseven.txt -> songofseven.txt (major rewrite, <20% match - split)
 *   sixserving.txt  -> sixserving.txt  (indentation change)
 *   sixserving.txt  -> ikeepsix.txt    (copy, add title, >80% match)
 *   sevencities.txt                    (no change)
 * commit 19dd32dfb1520a64e5bbaae8dce6ef423dfa2f13
 *   songofseven.txt -> untimely.txt    (rename, convert to crlf)
 *   ikeepsix.txt    -> ikeepsix.txt    (reorder sections in file)
 *   sixserving.txt  -> sixserving.txt  (whitespace change - not just indent)
 *   sevencities.txt -> songof7cities.txt (rename, small text changes)
 * commit 84d8efa38af7ace2b302de0adbda16b1f1cd2e1b
 *   songof7cities.txt -> songof7citie.txt (major rewrite, <20% match)
 *   ikeepsix.txt      ->                  (deleted)
 *   untimely.txt                          (no change)
 *   sixserving.txt                        (no change)
 * commit be053a189b0bbde545e0a3f59ce00b46ad29ce0d
 *   ikeepsix.txt      ->              (deleted)
 *   songof7cities.txt -> ikeepsix.txt (rename, 100% match)
 *   untimely.txt                      (no change)
 *   sixserving.txt                    (no change)
 */

void test_diff_rename__match_oid(void)
{
	const char *old_sha = INITIAL_COMMIT;
	const char *new_sha = COPY_RENAME_COMMIT;
	git_tree *old_tree, *new_tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;

	old_tree = resolve_commit_oid_to_tree(g_repo, old_sha);
	new_tree = resolve_commit_oid_to_tree(g_repo, new_sha);

	/* Must pass GIT_DIFF_INCLUDE_UNMODIFIED if you expect to emulate
	 * --find-copies-harder during rename transformion...
	 */
	diffopts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;

	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	/* git diff --no-renames \
	 *          31e47d8c1fa36d7f8d537b96158e3f024de0a9f2 \
	 *          2bc7f351d20b53f1c72c16c4b036e491c478c49a
	 */
	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);

	/* git diff 31e47d8c1fa36d7f8d537b96158e3f024de0a9f2 \
	 *          2bc7f351d20b53f1c72c16c4b036e491c478c49a
	 * don't use NULL opts to avoid config `diff.renames` contamination
	 */
	opts.flags = GIT_DIFF_FIND_RENAMES;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(3, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);

	git_diff_free(diff);

	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	/* git diff --find-copies-harder \
	 *          31e47d8c1fa36d7f8d537b96158e3f024de0a9f2 \
	 *          2bc7f351d20b53f1c72c16c4b036e491c478c49a
	 */
	opts.flags = GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(3, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_COPIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);

	git_diff_free(diff);

	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	/* git diff --find-copies-harder -M100 -B100 \
	 *          31e47d8c1fa36d7f8d537b96158e3f024de0a9f2 \
	 *          2bc7f351d20b53f1c72c16c4b036e491c478c49a
	 */
	opts.flags = GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED |
		GIT_DIFF_FIND_EXACT_MATCH_ONLY;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(3, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_COPIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);

	git_diff_free(diff);

	git_tree_free(old_tree);
	git_tree_free(new_tree);
}

void test_diff_rename__checks_options_version(void)
{
	const char *old_sha = INITIAL_COMMIT;
	const char *new_sha = COPY_RENAME_COMMIT;
	git_tree *old_tree, *new_tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	const git_error *err;

	old_tree = resolve_commit_oid_to_tree(g_repo, old_sha);
	new_tree = resolve_commit_oid_to_tree(g_repo, new_sha);
	diffopts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;
	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	opts.version = 0;
	cl_git_fail(git_diff_find_similar(diff, &opts));
	err = git_error_last();
	cl_assert_equal_i(GIT_ERROR_INVALID, err->klass);

	git_error_clear();
	opts.version = 1024;
	cl_git_fail(git_diff_find_similar(diff, &opts));
	err = git_error_last();
	cl_assert_equal_i(GIT_ERROR_INVALID, err->klass);

	git_diff_free(diff);
	git_tree_free(old_tree);
	git_tree_free(new_tree);
}

void test_diff_rename__not_exact_match(void)
{
	const char *sha0 = COPY_RENAME_COMMIT;
	const char *sha1 = REWRITE_COPY_COMMIT;
	const char *sha2 = RENAME_MODIFICATION_COMMIT;
	git_tree *old_tree, *new_tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;

	/* == Changes =====================================================
	 * songofseven.txt -> songofseven.txt (major rewrite, <20% match - split)
	 * sixserving.txt  -> sixserving.txt  (indentation change)
	 * sixserving.txt  -> ikeepsix.txt    (copy, add title, >80% match)
	 * sevencities.txt                    (no change)
	 */

	old_tree = resolve_commit_oid_to_tree(g_repo, sha0);
	new_tree = resolve_commit_oid_to_tree(g_repo, sha1);

	/* Must pass GIT_DIFF_INCLUDE_UNMODIFIED if you expect to emulate
	 * --find-copies-harder during rename transformion...
	 */
	diffopts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;

	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	/* git diff --no-renames \
	 *          2bc7f351d20b53f1c72c16c4b036e491c478c49a \
	 *          1c068dee5790ef1580cfc4cd670915b48d790084
	 */
	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_ADDED]);

	/* git diff -M 2bc7f351d20b53f1c72c16c4b036e491c478c49a \
	 *          1c068dee5790ef1580cfc4cd670915b48d790084
	 *
	 * must not pass NULL for opts because it will pick up environment
	 * values for "diff.renames" and test won't be consistent.
	 */
	opts.flags = GIT_DIFF_FIND_RENAMES;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_ADDED]);

	git_diff_free(diff);

	/* git diff -M -C \
	 *          2bc7f351d20b53f1c72c16c4b036e491c478c49a \
	 *          1c068dee5790ef1580cfc4cd670915b48d790084
	 */
	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	opts.flags = GIT_DIFF_FIND_RENAMES | GIT_DIFF_FIND_COPIES;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_COPIED]);

	git_diff_free(diff);

	/* git diff -M -C --find-copies-harder --break-rewrites \
	 *          2bc7f351d20b53f1c72c16c4b036e491c478c49a \
	 *          1c068dee5790ef1580cfc4cd670915b48d790084
	 */
	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	opts.flags = GIT_DIFF_FIND_ALL;
	opts.break_rewrite_threshold = 70;

	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(5, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_COPIED]);

	git_diff_free(diff);

	/* == Changes =====================================================
	 * songofseven.txt -> untimely.txt    (rename, convert to crlf)
	 * ikeepsix.txt    -> ikeepsix.txt    (reorder sections in file)
	 * sixserving.txt  -> sixserving.txt  (whitespace - not just indent)
	 * sevencities.txt -> songof7cities.txt (rename, small text changes)
	 */

	git_tree_free(old_tree);
	old_tree = new_tree;
	new_tree = resolve_commit_oid_to_tree(g_repo, sha2);

	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	/* git diff --no-renames \
	 *          1c068dee5790ef1580cfc4cd670915b48d790084 \
	 *          19dd32dfb1520a64e5bbaae8dce6ef423dfa2f13
	 */
	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(6, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_DELETED]);
	git_diff_free(diff);

	/* git diff -M -C \
	 *          1c068dee5790ef1580cfc4cd670915b48d790084 \
	 *          19dd32dfb1520a64e5bbaae8dce6ef423dfa2f13
	 */
	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	opts.flags = GIT_DIFF_FIND_RENAMES | GIT_DIFF_FIND_COPIES;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_RENAMED]);

	git_diff_free(diff);

	/* git diff -M -C --find-copies-harder --break-rewrites \
	 *          1c068dee5790ef1580cfc4cd670915b48d790084 \
	 *          19dd32dfb1520a64e5bbaae8dce6ef423dfa2f13
	 * with libgit2 default similarity comparison...
	 */
	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	opts.flags = GIT_DIFF_FIND_ALL;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	/* the default match algorithm is going to find the internal
	 * whitespace differences in the lines of sixserving.txt to be
	 * significant enough that this will decide to split it into an ADD
	 * and a DELETE
	 */

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(5, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_RENAMED]);

	git_diff_free(diff);

	/* git diff -M -C --find-copies-harder --break-rewrites \
	 *          1c068dee5790ef1580cfc4cd670915b48d790084 \
	 *          19dd32dfb1520a64e5bbaae8dce6ef423dfa2f13
	 * with ignore_space whitespace comparison
	 */
	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	opts.flags = GIT_DIFF_FIND_ALL | GIT_DIFF_FIND_IGNORE_WHITESPACE;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	/* Ignoring whitespace, this should no longer split sixserver.txt */

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_RENAMED]);

	git_diff_free(diff);

	git_tree_free(old_tree);
	git_tree_free(new_tree);
}

void test_diff_rename__test_small_files(void)
{
	git_index *index;
	git_reference *head_reference;
	git_commit *head_commit;
	git_tree *head_tree;
	git_tree *commit_tree;
	git_signature *signature;
	git_diff *diff;
	git_oid oid;
	const git_diff_delta *delta;
	git_diff_options diff_options = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options find_options = GIT_DIFF_FIND_OPTIONS_INIT;

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_mkfile("renames/small.txt", "Hello World!\n");
	cl_git_pass(git_index_add_bypath(index, "small.txt"));

	cl_git_pass(git_repository_head(&head_reference, g_repo));
	cl_git_pass(git_reference_peel((git_object**)&head_commit, head_reference, GIT_OBJECT_COMMIT));
	cl_git_pass(git_commit_tree(&head_tree, head_commit));
	cl_git_pass(git_index_write_tree(&oid, index));
	cl_git_pass(git_tree_lookup(&commit_tree, g_repo, &oid));
	cl_git_pass(git_signature_new(&signature, "Rename", "rename@example.com", 1404157834, 0));
	cl_git_pass(git_commit_create(&oid, g_repo, "HEAD", signature, signature, NULL, "Test commit", commit_tree, 1, (const git_commit**)&head_commit));

	cl_git_mkfile("renames/copy.txt", "Hello World!\n");
	cl_git_rmfile("renames/small.txt");

	diff_options.flags = GIT_DIFF_INCLUDE_UNTRACKED;
	cl_git_pass(git_diff_tree_to_workdir(&diff, g_repo, commit_tree, &diff_options));
	find_options.flags = GIT_DIFF_FIND_RENAMES | GIT_DIFF_FIND_FOR_UNTRACKED;
	cl_git_pass(git_diff_find_similar(diff, &find_options));

	cl_assert_equal_i(git_diff_num_deltas(diff), 1);
	delta = git_diff_get_delta(diff, 0);
	cl_assert_equal_i(delta->status, GIT_DELTA_RENAMED);
	cl_assert_equal_s(delta->old_file.path, "small.txt");
	cl_assert_equal_s(delta->new_file.path, "copy.txt");

	git_diff_free(diff);
	git_signature_free(signature);
	git_tree_free(commit_tree);
	git_tree_free(head_tree);
	git_commit_free(head_commit);
	git_reference_free(head_reference);
	git_index_free(index);
}

void test_diff_rename__working_directory_changes(void)
{
	const char *sha0 = COPY_RENAME_COMMIT;
	const char *blobsha = "66311f5cfbe7836c27510a3ba2f43e282e2c8bba";
	git_oid id;
	git_tree *tree;
	git_blob *blob;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;
	git_str old_content = GIT_STR_INIT, content = GIT_STR_INIT;;

	tree = resolve_commit_oid_to_tree(g_repo, sha0);
	diffopts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED | GIT_DIFF_INCLUDE_UNTRACKED;

	/*
	$ git cat-file -p 2bc7f351d20b53f1c72c16c4b036e491c478c49a^{tree}

	100644 blob 66311f5cfbe7836c27510a3ba2f43e282e2c8bba	sevencities.txt
	100644 blob ad0a8e55a104ac54a8a29ed4b84b49e76837a113	sixserving.txt
	100644 blob 66311f5cfbe7836c27510a3ba2f43e282e2c8bba	songofseven.txt

	$ for f in *.txt; do
		echo `git hash-object -t blob $f` $f
	done

	eaf4a3e3bfe68585e90cada20736ace491cd100b ikeepsix.txt
	f90d4fc20ecddf21eebe6a37e9225d244339d2b5 sixserving.txt
	4210ffd5c390b21dd5483375e75288dea9ede512 songof7cities.txt
	9a69d960ae94b060f56c2a8702545e2bb1abb935 untimely.txt
	*/

	cl_git_pass(git_diff_tree_to_workdir(&diff, g_repo, tree, &diffopts));

	/* git diff --no-renames 2bc7f351d20b53f1c72c16c4b036e491c478c49a */
	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(6, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_UNTRACKED]);

	/* git diff -M 2bc7f351d20b53f1c72c16c4b036e491c478c49a */
	opts.flags = GIT_DIFF_FIND_ALL;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(5, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_RENAMED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_UNTRACKED]);

	git_diff_free(diff);

	/* rewrite files in the working directory with / without CRLF changes */

	cl_git_pass(
		git_futils_readbuffer(&old_content, "renames/songof7cities.txt"));
	cl_git_pass(
		git_str_lf_to_crlf(&content, &old_content));
	cl_git_pass(
		git_futils_writebuffer(&content, "renames/songof7cities.txt", 0, 0));

	cl_git_pass(git_diff_tree_to_workdir(&diff, g_repo, tree, &diffopts));

	/* git diff -M 2bc7f351d20b53f1c72c16c4b036e491c478c49a */
	opts.flags = GIT_DIFF_FIND_ALL;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(5, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_RENAMED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_UNTRACKED]);

	git_diff_free(diff);

	/* try a different whitespace option */

	cl_git_pass(git_diff_tree_to_workdir(&diff, g_repo, tree, &diffopts));

	opts.flags = GIT_DIFF_FIND_ALL | GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE;
	opts.rename_threshold = 70;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(6, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_UNTRACKED]);

	git_diff_free(diff);

	/* try a different matching option */

	cl_git_pass(git_diff_tree_to_workdir(&diff, g_repo, tree, &diffopts));

	opts.flags = GIT_DIFF_FIND_ALL | GIT_DIFF_FIND_EXACT_MATCH_ONLY;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(6, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_UNTRACKED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_DELETED]);

	git_diff_free(diff);

	/* again with exact match blob */

	cl_git_pass(git_oid__fromstr(&id, blobsha, GIT_OID_SHA1));
	cl_git_pass(git_blob_lookup(&blob, g_repo, &id));
	cl_git_pass(git_str_set(
		&content, git_blob_rawcontent(blob), (size_t)git_blob_rawsize(blob)));
	cl_git_rewritefile("renames/songof7cities.txt", content.ptr);
	git_blob_free(blob);

	cl_git_pass(git_diff_tree_to_workdir(&diff, g_repo, tree, &diffopts));

	opts.flags = GIT_DIFF_FIND_ALL | GIT_DIFF_FIND_EXACT_MATCH_ONLY;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	/*
	fprintf(stderr, "\n\n");
    diff_print_raw(stderr, diff);
	*/

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));

	cl_assert_equal_i(5, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_UNTRACKED]);

	git_diff_free(diff);

	git_tree_free(tree);
	git_str_dispose(&content);
	git_str_dispose(&old_content);
}

void test_diff_rename__patch(void)
{
	const char *sha0 = COPY_RENAME_COMMIT;
	const char *sha1 = REWRITE_COPY_COMMIT;
	git_tree *old_tree, *new_tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	git_patch *patch;
	const git_diff_delta *delta;
	git_buf buf = GIT_BUF_INIT;
	const char *expected = "diff --git a/sixserving.txt b/ikeepsix.txt\nindex ad0a8e5..36020db 100644\n--- a/sixserving.txt\n+++ b/ikeepsix.txt\n@@ -1,3 +1,6 @@\n+I Keep Six Honest Serving-Men\n+=============================\n+\n I KEEP six honest serving-men\n  (They taught me all I knew);\n Their names are What and Why and When\n@@ -21,4 +24,4 @@ She sends'em abroad on her own affairs,\n One million Hows, two million Wheres,\n And seven million Whys!\n \n-                -- Rudyard Kipling\n+  -- Rudyard Kipling\n";

	old_tree = resolve_commit_oid_to_tree(g_repo, sha0);
	new_tree = resolve_commit_oid_to_tree(g_repo, sha1);

	diffopts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;
	cl_git_pass(git_diff_tree_to_tree(
		&diff, g_repo, old_tree, new_tree, &diffopts));

	opts.flags = GIT_DIFF_FIND_RENAMES | GIT_DIFF_FIND_COPIES;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	/* == Changes =====================================================
	 * sixserving.txt  -> ikeepsix.txt    (copy, add title, >80% match)
	 * sevencities.txt                    (no change)
	 * sixserving.txt  -> sixserving.txt  (indentation change)
	 * songofseven.txt -> songofseven.txt (major rewrite, <20% match - split)
	 */

	cl_assert_equal_i(4, (int)git_diff_num_deltas(diff));

	cl_git_pass(git_patch_from_diff(&patch, diff, 0));
	cl_assert((delta = git_patch_get_delta(patch)) != NULL);
	cl_assert_equal_i(GIT_DELTA_COPIED, (int)delta->status);

	cl_git_pass(git_patch_to_buf(&buf, patch));
	cl_assert_equal_s(expected, buf.ptr);
	git_buf_dispose(&buf);

	git_patch_free(patch);

	cl_assert((delta = git_diff_get_delta(diff, 1)) != NULL);
	cl_assert_equal_i(GIT_DELTA_UNMODIFIED, (int)delta->status);

	cl_assert((delta = git_diff_get_delta(diff, 2)) != NULL);
	cl_assert_equal_i(GIT_DELTA_MODIFIED, (int)delta->status);

	cl_assert((delta = git_diff_get_delta(diff, 3)) != NULL);
	cl_assert_equal_i(GIT_DELTA_MODIFIED, (int)delta->status);

	git_diff_free(diff);
	git_tree_free(old_tree);
	git_tree_free(new_tree);
}

void test_diff_rename__file_exchange(void)
{
	git_str c1 = GIT_STR_INIT, c2 = GIT_STR_INIT;
	git_index *index;
	git_tree *tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;

	cl_git_pass(git_futils_readbuffer(&c1, "renames/untimely.txt"));
	cl_git_pass(git_futils_readbuffer(&c2, "renames/songof7cities.txt"));
	cl_git_pass(git_futils_writebuffer(&c1, "renames/songof7cities.txt", 0, 0));
	cl_git_pass(git_futils_writebuffer(&c2, "renames/untimely.txt", 0, 0));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_read_tree(index, tree));
	cl_git_pass(git_index_add_bypath(index, "songof7cities.txt"));
	cl_git_pass(git_index_add_bypath(index, "untimely.txt"));

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(2, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);

	opts.flags = GIT_DIFF_FIND_ALL;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(2, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_RENAMED]);

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);

	git_str_dispose(&c1);
	git_str_dispose(&c2);
}

void test_diff_rename__file_exchange_three(void)
{
	git_str c1 = GIT_STR_INIT, c2 = GIT_STR_INIT, c3 = GIT_STR_INIT;
	git_index *index;
	git_tree *tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;

	cl_git_pass(git_futils_readbuffer(&c1, "renames/untimely.txt"));
	cl_git_pass(git_futils_readbuffer(&c2, "renames/songof7cities.txt"));
	cl_git_pass(git_futils_readbuffer(&c3, "renames/ikeepsix.txt"));

	cl_git_pass(git_futils_writebuffer(&c1, "renames/ikeepsix.txt", 0, 0));
	cl_git_pass(git_futils_writebuffer(&c2, "renames/untimely.txt", 0, 0));
	cl_git_pass(git_futils_writebuffer(&c3, "renames/songof7cities.txt", 0, 0));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_read_tree(index, tree));
	cl_git_pass(git_index_add_bypath(index, "songof7cities.txt"));
	cl_git_pass(git_index_add_bypath(index, "untimely.txt"));
	cl_git_pass(git_index_add_bypath(index, "ikeepsix.txt"));

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(3, exp.files);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_MODIFIED]);

	opts.flags = GIT_DIFF_FIND_ALL;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(3, exp.files);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_RENAMED]);

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);

	git_str_dispose(&c1);
	git_str_dispose(&c2);
	git_str_dispose(&c3);
}

void test_diff_rename__file_partial_exchange(void)
{
	git_str c1 = GIT_STR_INIT, c2 = GIT_STR_INIT;
	git_index *index;
	git_tree *tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;
	int i;

	cl_git_pass(git_futils_readbuffer(&c1, "renames/untimely.txt"));
	cl_git_pass(git_futils_writebuffer(&c1, "renames/songof7cities.txt", 0, 0));
	for (i = 0; i < 100; ++i)
		cl_git_pass(git_str_puts(&c2, "this is not the content you are looking for\n"));
	cl_git_pass(git_futils_writebuffer(&c2, "renames/untimely.txt", 0, 0));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_read_tree(index, tree));
	cl_git_pass(git_index_add_bypath(index, "songof7cities.txt"));
	cl_git_pass(git_index_add_bypath(index, "untimely.txt"));

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(2, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);

	opts.flags = GIT_DIFF_FIND_ALL;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(3, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);

	git_str_dispose(&c1);
	git_str_dispose(&c2);
}

void test_diff_rename__rename_and_copy_from_same_source(void)
{
	git_str c1 = GIT_STR_INIT, c2 = GIT_STR_INIT;
	git_index *index;
	git_tree *tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;

	/* put the first 2/3 of file into one new place
	 * and the second 2/3 of file into another new place
	 */
	cl_git_pass(git_futils_readbuffer(&c1, "renames/songof7cities.txt"));
	cl_git_pass(git_str_set(&c2, c1.ptr, c1.size));
	git_str_truncate(&c1, c1.size * 2 / 3);
	git_str_consume(&c2, ((char *)c2.ptr) + (c2.size / 3));
	cl_git_pass(git_futils_writebuffer(&c1, "renames/song_a.txt", 0, 0));
	cl_git_pass(git_futils_writebuffer(&c2, "renames/song_b.txt", 0, 0));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_read_tree(index, tree));
	cl_git_pass(git_index_add_bypath(index, "song_a.txt"));
	cl_git_pass(git_index_add_bypath(index, "song_b.txt"));

	diffopts.flags = GIT_DIFF_INCLUDE_UNMODIFIED;

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(6, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(4, exp.file_status[GIT_DELTA_UNMODIFIED]);

	opts.flags = GIT_DIFF_FIND_ALL;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(6, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_COPIED]);
	cl_assert_equal_i(4, exp.file_status[GIT_DELTA_UNMODIFIED]);

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);

	git_str_dispose(&c1);
	git_str_dispose(&c2);
}

void test_diff_rename__from_deleted_to_split(void)
{
	git_str c1 = GIT_STR_INIT;
	git_index *index;
	git_tree *tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;

	/* old file is missing, new file is actually old file renamed */

	cl_git_pass(git_futils_readbuffer(&c1, "renames/songof7cities.txt"));
	cl_git_pass(git_futils_writebuffer(&c1, "renames/untimely.txt", 0, 0));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_read_tree(index, tree));
	cl_git_pass(git_index_remove_bypath(index, "songof7cities.txt"));
	cl_git_pass(git_index_add_bypath(index, "untimely.txt"));

	diffopts.flags = GIT_DIFF_INCLUDE_UNMODIFIED;

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_UNMODIFIED]);

	opts.flags = GIT_DIFF_FIND_ALL;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_UNMODIFIED]);

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);

	git_str_dispose(&c1);
}

struct rename_expected
{
	size_t len;

	unsigned int *status;
	const char **sources;
	const char **targets;

	size_t idx;
};

static int test_names_expected(const git_diff_delta *delta, float progress, void *p)
{
	struct rename_expected *expected = p;

	GIT_UNUSED(progress);

	cl_assert(expected->idx < expected->len);

	cl_assert_equal_i(delta->status, expected->status[expected->idx]);

	cl_assert(git__strcmp(expected->sources[expected->idx],
		delta->old_file.path) == 0);
	cl_assert(git__strcmp(expected->targets[expected->idx],
		delta->new_file.path) == 0);

	expected->idx++;

	return 0;
}

void test_diff_rename__rejected_match_can_match_others(void)
{
	git_reference *head, *selfsimilar;
	git_index *index;
	git_tree *tree;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options findopts = GIT_DIFF_FIND_OPTIONS_INIT;
	git_str one = GIT_STR_INIT, two = GIT_STR_INIT;
	unsigned int status[] = { GIT_DELTA_RENAMED, GIT_DELTA_RENAMED };
	const char *sources[] = { "Class1.cs", "Class2.cs" };
	const char *targets[] = { "ClassA.cs", "ClassB.cs" };
	struct rename_expected expect = { 2, status, sources, targets };
	char *ptr;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	findopts.flags = GIT_DIFF_FIND_RENAMES;

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_git_pass(git_reference_symbolic_set_target(
		&selfsimilar, head, "refs/heads/renames_similar", NULL));
	cl_git_pass(git_checkout_head(g_repo, &opts));
	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(git_futils_readbuffer(&one, "renames/Class1.cs"));
	cl_git_pass(git_futils_readbuffer(&two, "renames/Class2.cs"));

	cl_git_pass(p_unlink("renames/Class1.cs"));
	cl_git_pass(p_unlink("renames/Class2.cs"));

	cl_git_pass(git_index_remove_bypath(index, "Class1.cs"));
	cl_git_pass(git_index_remove_bypath(index, "Class2.cs"));

	cl_assert(ptr = strstr(one.ptr, "Class1"));
	ptr[5] = 'A';

	cl_assert(ptr = strstr(two.ptr, "Class2"));
	ptr[5] = 'B';

	cl_git_pass(
		git_futils_writebuffer(&one, "renames/ClassA.cs", O_RDWR|O_CREAT, 0777));
	cl_git_pass(
		git_futils_writebuffer(&two, "renames/ClassB.cs", O_RDWR|O_CREAT, 0777));

	cl_git_pass(git_index_add_bypath(index, "ClassA.cs"));
	cl_git_pass(git_index_add_bypath(index, "ClassB.cs"));

	cl_git_pass(git_index_write(index));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(
		git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	cl_git_pass(git_diff_find_similar(diff, &findopts));

	cl_git_pass(git_diff_foreach(
		diff, test_names_expected, NULL, NULL, NULL, &expect));

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);
	git_reference_free(head);
	git_reference_free(selfsimilar);
	git_str_dispose(&one);
	git_str_dispose(&two);
}

static void write_similarity_file_two(const char *filename, size_t b_lines)
{
	git_str contents = GIT_STR_INIT;
	size_t i;

	for (i = 0; i < b_lines; i++)
		git_str_printf(&contents, "%02d - bbbbb\r\n", (int)(i+1));

	for (i = b_lines; i < 50; i++)
		git_str_printf(&contents, "%02d - aaaaa%s", (int)(i+1), (i == 49 ? "" : "\r\n"));

	cl_git_pass(
		git_futils_writebuffer(&contents, filename, O_RDWR|O_CREAT, 0777));

	git_str_dispose(&contents);
}

void test_diff_rename__rejected_match_can_match_others_two(void)
{
	git_reference *head, *selfsimilar;
	git_index *index;
	git_tree *tree;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options findopts = GIT_DIFF_FIND_OPTIONS_INIT;
	unsigned int status[] = { GIT_DELTA_RENAMED, GIT_DELTA_RENAMED };
	const char *sources[] = { "a.txt", "b.txt" };
	const char *targets[] = { "c.txt", "d.txt" };
	struct rename_expected expect = { 2, status, sources, targets };

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	findopts.flags = GIT_DIFF_FIND_RENAMES;

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_git_pass(git_reference_symbolic_set_target(
		&selfsimilar, head, "refs/heads/renames_similar_two", NULL));
	cl_git_pass(git_checkout_head(g_repo, &opts));
	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(p_unlink("renames/a.txt"));
	cl_git_pass(p_unlink("renames/b.txt"));

	cl_git_pass(git_index_remove_bypath(index, "a.txt"));
	cl_git_pass(git_index_remove_bypath(index, "b.txt"));

	write_similarity_file_two("renames/c.txt", 7);
	write_similarity_file_two("renames/d.txt", 8);

	cl_git_pass(git_index_add_bypath(index, "c.txt"));
	cl_git_pass(git_index_add_bypath(index, "d.txt"));

	cl_git_pass(git_index_write(index));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(
		git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	cl_git_pass(git_diff_find_similar(diff, &findopts));

	cl_git_pass(git_diff_foreach(
		diff, test_names_expected, NULL, NULL, NULL, &expect));
	cl_assert(expect.idx > 0);

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);
	git_reference_free(head);
	git_reference_free(selfsimilar);
}

void test_diff_rename__rejected_match_can_match_others_three(void)
{
	git_reference *head, *selfsimilar;
	git_index *index;
	git_tree *tree;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options findopts = GIT_DIFF_FIND_OPTIONS_INIT;

	/* Both cannot be renames from a.txt */
	unsigned int status[] = { GIT_DELTA_ADDED, GIT_DELTA_RENAMED };
	const char *sources[] = { "0001.txt", "a.txt" };
	const char *targets[] = { "0001.txt", "0002.txt" };
	struct rename_expected expect = { 2, status, sources, targets };

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	findopts.flags = GIT_DIFF_FIND_RENAMES;

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_git_pass(git_reference_symbolic_set_target(
		&selfsimilar, head, "refs/heads/renames_similar_two", NULL));
	cl_git_pass(git_checkout_head(g_repo, &opts));
	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(p_unlink("renames/a.txt"));

	cl_git_pass(git_index_remove_bypath(index, "a.txt"));

	write_similarity_file_two("renames/0001.txt", 7);
	write_similarity_file_two("renames/0002.txt", 0);

	cl_git_pass(git_index_add_bypath(index, "0001.txt"));
	cl_git_pass(git_index_add_bypath(index, "0002.txt"));

	cl_git_pass(git_index_write(index));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(
		git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	cl_git_pass(git_diff_find_similar(diff, &findopts));

	cl_git_pass(git_diff_foreach(
		diff, test_names_expected, NULL, NULL, NULL, &expect));

	cl_assert(expect.idx == expect.len);

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);
	git_reference_free(head);
	git_reference_free(selfsimilar);
}

void test_diff_rename__can_rename_from_rewrite(void)
{
	git_index *index;
	git_tree *tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options findopts = GIT_DIFF_FIND_OPTIONS_INIT;

	unsigned int status[] = { GIT_DELTA_RENAMED, GIT_DELTA_RENAMED };
	const char *sources[] = { "ikeepsix.txt", "songof7cities.txt" };
	const char *targets[] = { "songof7cities.txt", "this-is-a-rename.txt" };
	struct rename_expected expect = { 2, status, sources, targets };

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(p_rename("renames/songof7cities.txt", "renames/this-is-a-rename.txt"));
	cl_git_pass(p_rename("renames/ikeepsix.txt", "renames/songof7cities.txt"));

	cl_git_pass(git_index_remove_bypath(index, "ikeepsix.txt"));

	cl_git_pass(git_index_add_bypath(index, "songof7cities.txt"));
	cl_git_pass(git_index_add_bypath(index, "this-is-a-rename.txt"));

	cl_git_pass(git_index_write(index));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(
		git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	findopts.flags |= GIT_DIFF_FIND_AND_BREAK_REWRITES |
		GIT_DIFF_FIND_REWRITES |
		GIT_DIFF_FIND_RENAMES_FROM_REWRITES;

	cl_git_pass(git_diff_find_similar(diff, &findopts));

	cl_git_pass(git_diff_foreach(
		diff, test_names_expected, NULL, NULL, NULL, &expect));

	cl_assert(expect.idx == expect.len);

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);
}

void test_diff_rename__case_changes_are_split(void)
{
	git_index *index;
	git_tree *tree;
	git_diff *diff = NULL;
	diff_expects exp;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(p_rename("renames/ikeepsix.txt", "renames/IKEEPSIX.txt"));

	cl_git_pass(git_index_remove_bypath(index, "ikeepsix.txt"));
	cl_git_pass(git_index_add_bypath(index, "IKEEPSIX.txt"));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, tree, index, NULL));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(2, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_ADDED]);

	opts.flags = GIT_DIFF_FIND_ALL;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(1, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);

	git_diff_free(diff);
	git_index_free(index);
	git_tree_free(tree);
}

void test_diff_rename__unmodified_can_be_renamed(void)
{
	git_index *index;
	git_tree *tree;
	git_diff *diff = NULL;
	diff_expects exp;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(p_rename("renames/ikeepsix.txt", "renames/ikeepsix2.txt"));

	cl_git_pass(git_index_remove_bypath(index, "ikeepsix.txt"));
	cl_git_pass(git_index_add_bypath(index, "ikeepsix2.txt"));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(2, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_ADDED]);

	opts.flags = GIT_DIFF_FIND_ALL;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(1, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(1, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);

	git_diff_free(diff);
	git_index_free(index);
	git_tree_free(tree);
}

void test_diff_rename__rewrite_on_single_file(void)
{
	git_index *index;
	git_diff *diff = NULL;
	diff_expects exp;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options findopts = GIT_DIFF_FIND_OPTIONS_INIT;

	diffopts.flags = GIT_DIFF_INCLUDE_UNTRACKED;

	findopts.flags = GIT_DIFF_FIND_FOR_UNTRACKED |
		GIT_DIFF_FIND_AND_BREAK_REWRITES |
		GIT_DIFF_FIND_RENAMES_FROM_REWRITES;

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_rewritefile("renames/ikeepsix.txt",
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n" \
		"This is enough content for the file to be rewritten.\n");

	cl_git_pass(git_diff_index_to_workdir(&diff, g_repo, index, &diffopts));
	cl_git_pass(git_diff_find_similar(diff, &findopts));

	memset(&exp, 0, sizeof(exp));

	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(2, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNTRACKED]);

	git_diff_free(diff);
	git_index_free(index);
}

void test_diff_rename__can_find_copy_to_split(void)
{
	git_str c1 = GIT_STR_INIT;
	git_index *index;
	git_tree *tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;

	cl_git_pass(git_futils_readbuffer(&c1, "renames/songof7cities.txt"));
	cl_git_pass(git_futils_writebuffer(&c1, "renames/untimely.txt", 0, 0));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_read_tree(index, tree));
	cl_git_pass(git_index_add_bypath(index, "untimely.txt"));

	diffopts.flags = GIT_DIFF_INCLUDE_UNMODIFIED;

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_UNMODIFIED]);

	opts.flags = GIT_DIFF_FIND_ALL;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(5, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_COPIED]);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_UNMODIFIED]);

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);

	git_str_dispose(&c1);
}

void test_diff_rename__can_delete_unmodified_deltas(void)
{
	git_str c1 = GIT_STR_INIT;
	git_index *index;
	git_tree *tree;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;

	cl_git_pass(git_futils_readbuffer(&c1, "renames/songof7cities.txt"));
	cl_git_pass(git_futils_writebuffer(&c1, "renames/untimely.txt", 0, 0));

	cl_git_pass(
		git_revparse_single((git_object **)&tree, g_repo, "HEAD^{tree}"));

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_read_tree(index, tree));
	cl_git_pass(git_index_add_bypath(index, "untimely.txt"));

	diffopts.flags = GIT_DIFF_INCLUDE_UNMODIFIED;

	cl_git_pass(git_diff_tree_to_index(&diff, g_repo, tree, index, &diffopts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(3, exp.file_status[GIT_DELTA_UNMODIFIED]);

	opts.flags = GIT_DIFF_FIND_ALL | GIT_DIFF_FIND_REMOVE_UNMODIFIED;
	cl_git_pass(git_diff_find_similar(diff, &opts));

	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(2, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_COPIED]);

	git_diff_free(diff);
	git_tree_free(tree);
	git_index_free(index);

	git_str_dispose(&c1);
}

void test_diff_rename__matches_config_behavior(void)
{
	const char *sha0 = INITIAL_COMMIT;
	const char *sha1 = COPY_RENAME_COMMIT;
	const char *sha2 = REWRITE_COPY_COMMIT;

	git_tree *tree0, *tree1, *tree2;
	git_config *cfg;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;

	opts.flags = GIT_DIFF_FIND_BY_CONFIG;
	tree0 = resolve_commit_oid_to_tree(g_repo, sha0);
	tree1 = resolve_commit_oid_to_tree(g_repo, sha1);
	tree2 = resolve_commit_oid_to_tree(g_repo, sha2);

	diffopts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;
	cl_git_pass(git_repository_config(&cfg, g_repo));

	/* diff.renames = false; no rename detection should happen */
	cl_git_pass(git_config_set_bool(cfg, "diff.renames", false));
	cl_git_pass(git_diff_tree_to_tree(
				&diff, g_repo, tree0, tree1, &diffopts));
	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_find_similar(diff, &opts));
	cl_git_pass(git_diff_foreach(diff,
				diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	git_diff_free(diff);

	/* diff.renames = true; should act like -M */
	cl_git_pass(git_config_set_bool(cfg, "diff.renames", true));
	cl_git_pass(git_diff_tree_to_tree(
				&diff, g_repo, tree0, tree1, &diffopts));
	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_find_similar(diff, &opts));
	cl_git_pass(git_diff_foreach(diff,
				diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(3, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);
	git_diff_free(diff);

	/* diff.renames = copies; should act like -M -C */
	cl_git_pass(git_config_set_string(cfg, "diff.renames", "copies"));
	cl_git_pass(git_diff_tree_to_tree(
				&diff, g_repo, tree1, tree2, &diffopts));
	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_find_similar(diff, &opts));
	cl_git_pass(git_diff_foreach(diff,
				diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_COPIED]);
	git_diff_free(diff);

	/* NULL find options is the same as GIT_DIFF_FIND_BY_CONFIG */
	cl_git_pass(git_diff_tree_to_tree(
				&diff, g_repo, tree1, tree2, &diffopts));
	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_find_similar(diff, NULL));
	cl_git_pass(git_diff_foreach(diff,
				diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_COPIED]);
	git_diff_free(diff);

	/* Cleanup */
	git_tree_free(tree0);
	git_tree_free(tree1);
	git_tree_free(tree2);
	git_config_free(cfg);
}

void test_diff_rename__can_override_thresholds_when_obeying_config(void)
{
	const char *sha1 = COPY_RENAME_COMMIT;
	const char *sha2 = REWRITE_COPY_COMMIT;

	git_tree *tree1, *tree2;
	git_config *cfg;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;

	tree1 = resolve_commit_oid_to_tree(g_repo, sha1);
	tree2 = resolve_commit_oid_to_tree(g_repo, sha2);

	diffopts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;
	opts.flags = GIT_DIFF_FIND_BY_CONFIG;

	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_pass(git_config_set_string(cfg, "diff.renames", "copies"));
	git_config_free(cfg);

	/* copy threshold = 96%, should see creation of ikeepsix.txt */
	opts.copy_threshold = 96;
	cl_git_pass(git_diff_tree_to_tree(
				&diff, g_repo, tree1, tree2, &diffopts));
	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_find_similar(diff, &opts));
	cl_git_pass(git_diff_foreach(diff,
				diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_ADDED]);
	git_diff_free(diff);

	/* copy threshold = 20%, should see sixserving.txt => ikeepsix.txt */
	opts.copy_threshold = 20;
	cl_git_pass(git_diff_tree_to_tree(
				&diff, g_repo, tree1, tree2, &diffopts));
	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_find_similar(diff, &opts));
	cl_git_pass(git_diff_foreach(diff,
				diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(4, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_COPIED]);
	git_diff_free(diff);

	/* Cleanup */
	git_tree_free(tree1);
	git_tree_free(tree2);
}

void test_diff_rename__by_config_doesnt_mess_with_whitespace_settings(void)
{
	const char *sha1 = REWRITE_COPY_COMMIT;
	const char *sha2 = RENAME_MODIFICATION_COMMIT;

	git_tree *tree1, *tree2;
	git_config *cfg;
	git_diff *diff;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options opts = GIT_DIFF_FIND_OPTIONS_INIT;
	diff_expects exp;

	tree1 = resolve_commit_oid_to_tree(g_repo, sha1);
	tree2 = resolve_commit_oid_to_tree(g_repo, sha2);

	diffopts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;
	opts.flags = GIT_DIFF_FIND_BY_CONFIG;

	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_pass(git_config_set_string(cfg, "diff.renames", "copies"));
	git_config_free(cfg);

	/* Don't ignore whitespace; this should find a change in sixserving.txt */
	opts.flags |= 0 | GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE;
	cl_git_pass(git_diff_tree_to_tree(
				&diff, g_repo, tree1, tree2, &diffopts));
	memset(&exp, 0, sizeof(exp));
	cl_git_pass(git_diff_find_similar(diff, &opts));
	cl_git_pass(git_diff_foreach(diff,
				diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(5, exp.files);
	cl_assert_equal_i(2, exp.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_ADDED]);
	git_diff_free(diff);

	/* Cleanup */
	git_tree_free(tree1);
	git_tree_free(tree2);
}

static void expect_files_renamed(const char *one, const char *two, uint32_t whitespace_flags)
{
	git_index *index;
	git_diff *diff = NULL;
	diff_expects exp;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options findopts = GIT_DIFF_FIND_OPTIONS_INIT;

	diffopts.flags = GIT_DIFF_INCLUDE_UNTRACKED;
	findopts.flags = GIT_DIFF_FIND_FOR_UNTRACKED |
		GIT_DIFF_FIND_AND_BREAK_REWRITES |
		GIT_DIFF_FIND_RENAMES_FROM_REWRITES |
		whitespace_flags;

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_rewritefile("renames/ikeepsix.txt", one);
	cl_git_pass(git_index_add_bypath(index, "ikeepsix.txt"));

	cl_git_rmfile("renames/ikeepsix.txt");
	cl_git_rewritefile("renames/ikeepsix2.txt", two);

	cl_git_pass(git_diff_index_to_workdir(&diff, g_repo, index, &diffopts));
	cl_git_pass(git_diff_find_similar(diff, &findopts));

	memset(&exp, 0, sizeof(exp));

	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(1, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_RENAMED]);

	git_diff_free(diff);
	git_index_free(index);
}

/* test some variations on empty and blank files */
void test_diff_rename__empty_files_renamed(void)
{
	/* empty files are identical when ignoring whitespace or not */
	expect_files_renamed("", "", GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE);
	expect_files_renamed("", "",  GIT_DIFF_FIND_IGNORE_WHITESPACE);
}

/* test that blank files are similar when ignoring whitespace */
void test_diff_rename__blank_files_renamed_when_ignoring_whitespace(void)
{
	expect_files_renamed("", "\n\n",  GIT_DIFF_FIND_IGNORE_WHITESPACE);
	expect_files_renamed("", "\r\n\r\n",  GIT_DIFF_FIND_IGNORE_WHITESPACE);
	expect_files_renamed("\r\n\r\n", "\n\n\n",  GIT_DIFF_FIND_IGNORE_WHITESPACE);

	expect_files_renamed("    ", "\n\n",  GIT_DIFF_FIND_IGNORE_WHITESPACE);
	expect_files_renamed("   \n   \n", "\n\n",  GIT_DIFF_FIND_IGNORE_WHITESPACE);
}

/* blank files are not similar when whitespace is not ignored */
static void expect_files_not_renamed(const char *one, const char *two, uint32_t whitespace_flags)
{
	git_index *index;
	git_diff *diff = NULL;
	diff_expects exp;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options findopts = GIT_DIFF_FIND_OPTIONS_INIT;

	diffopts.flags = GIT_DIFF_INCLUDE_UNTRACKED;

	findopts.flags = GIT_DIFF_FIND_FOR_UNTRACKED |
		whitespace_flags;

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_rewritefile("renames/ikeepsix.txt", one);
	cl_git_pass(git_index_add_bypath(index, "ikeepsix.txt"));

	cl_git_rmfile("renames/ikeepsix.txt");
	cl_git_rewritefile("renames/ikeepsix2.txt", two);

	cl_git_pass(git_diff_index_to_workdir(&diff, g_repo, index, &diffopts));
	cl_git_pass(git_diff_find_similar(diff, &findopts));

	memset(&exp, 0, sizeof(exp));

	cl_git_pass(git_diff_foreach(
		diff, diff_file_cb, diff_binary_cb, diff_hunk_cb, diff_line_cb, &exp));
	cl_assert_equal_i(2, exp.files);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(1, exp.file_status[GIT_DELTA_UNTRACKED]);

	git_diff_free(diff);
	git_index_free(index);
}

/* test that blank files are similar when ignoring renames */
void test_diff_rename__blank_files_not_renamed_when_not_ignoring_whitespace(void)
{
	expect_files_not_renamed("", "\r\n\r\n\r\n",  GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE);
	expect_files_not_renamed("", "\n\n\n\n",  GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE);
	expect_files_not_renamed("\n\n\n\n", "\r\n\r\n\r\n",  GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE);
}

/* test that 100% renames and copies emit the correct patch file
 * git diff --find-copies-harder -M100 -B100 \
 *          31e47d8c1fa36d7f8d537b96158e3f024de0a9f2 \
 *          2bc7f351d20b53f1c72c16c4b036e491c478c49a
 */
void test_diff_rename__identical(void)
{
	const char *old_sha = INITIAL_COMMIT;
	const char *new_sha = COPY_RENAME_COMMIT;
	git_tree *old_tree, *new_tree;
    git_diff *diff;
	git_diff_options diff_opts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options find_opts = GIT_DIFF_FIND_OPTIONS_INIT;
	git_buf diff_buf = GIT_BUF_INIT;
	const char *expected =
		"diff --git a/serving.txt b/sixserving.txt\n"
		"similarity index 100%\n"
		"rename from serving.txt\n"
		"rename to sixserving.txt\n"
		"diff --git a/sevencities.txt b/songofseven.txt\n"
		"similarity index 100%\n"
		"copy from sevencities.txt\n"
		"copy to songofseven.txt\n";

	old_tree = resolve_commit_oid_to_tree(g_repo, old_sha);
	new_tree = resolve_commit_oid_to_tree(g_repo, new_sha);

	diff_opts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;
	find_opts.flags = GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED |
		GIT_DIFF_FIND_EXACT_MATCH_ONLY;

	cl_git_pass(git_diff_tree_to_tree(&diff,
		g_repo, old_tree, new_tree, &diff_opts));
	cl_git_pass(git_diff_find_similar(diff, &find_opts));

	cl_git_pass(git_diff_to_buf(&diff_buf, diff, GIT_DIFF_FORMAT_PATCH));

	cl_assert_equal_s(expected, diff_buf.ptr);

	git_buf_dispose(&diff_buf);
	git_diff_free(diff);
	git_tree_free(old_tree);
	git_tree_free(new_tree);
}

void test_diff_rename__rewrite_and_delete(void)
{
	const char *old_sha = RENAME_MODIFICATION_COMMIT;
	const char *new_sha = REWRITE_DELETE_COMMIT;
	git_tree *old_tree, *new_tree;
	git_diff *diff;
	git_diff_find_options find_opts = GIT_DIFF_FIND_OPTIONS_INIT;
	git_buf diff_buf = GIT_BUF_INIT;
	const char *expected =
		"diff --git a/ikeepsix.txt b/ikeepsix.txt\n"
		"deleted file mode 100644\n"
		"index eaf4a3e..0000000\n"
		"--- a/ikeepsix.txt\n"
		"+++ /dev/null\n"
		"@@ -1,27 +0,0 @@\n"
		"-I Keep Six Honest Serving-Men\n"
		"-=============================\n"
		"-\n"
		"-She sends'em abroad on her own affairs,\n"
		"- From the second she opens her eyes—\n"
		"-One million Hows, two million Wheres,\n"
		"-And seven million Whys!\n"
		"-\n"
		"-I let them rest from nine till five,\n"
		"- For I am busy then,\n"
		"-As well as breakfast, lunch, and tea,\n"
		"- For they are hungry men.\n"
		"-But different folk have different views;\n"
		"-I know a person small—\n"
		"-She keeps ten million serving-men,\n"
		"-Who get no rest at all!\n"
		"-\n"
		"-  -- Rudyard Kipling\n"
		"-\n"
		"-I KEEP six honest serving-men\n"
		"- (They taught me all I knew);\n"
		"-Their names are What and Why and When\n"
		"- And How and Where and Who.\n"
		"-I send them over land and sea,\n"
		"- I send them east and west;\n"
		"-But after they have worked for me,\n"
		"- I give them all a rest.\n"
		"diff --git a/songof7cities.txt b/songof7cities.txt\n"
		"index 4210ffd..95ceb12 100644\n"
		"--- a/songof7cities.txt\n"
		"+++ b/songof7cities.txt\n"
		"@@ -1,45 +1,45 @@\n"
		"-The Song of Seven Cities\n"
		"+THE SONG OF SEVEN CITIES\n"
		" ------------------------\n"
		" \n"
		"-I WAS Lord of Cities very sumptuously builded.\n"
		"-Seven roaring Cities paid me tribute from afar.\n"
		"-Ivory their outposts were--the guardrooms of them gilded,\n"
		"-And garrisoned with Amazons invincible in war.\n"
		"-\n"
		"-All the world went softly when it walked before my Cities--\n"
		"-Neither King nor Army vexed my peoples at their toil,\n"
		"-Never horse nor chariot irked or overbore my Cities,\n"
		"-Never Mob nor Ruler questioned whence they drew their spoil.\n"
		"-\n"
		"-Banded, mailed and arrogant from sunrise unto sunset;\n"
		"-Singing while they sacked it, they possessed the land at large.\n"
		"-Yet when men would rob them, they resisted, they made onset\n"
		"-And pierced the smoke of battle with a thousand-sabred charge.\n"
		"-\n"
		"-So they warred and trafficked only yesterday, my Cities.\n"
		"-To-day there is no mark or mound of where my Cities stood.\n"
		"-For the River rose at midnight and it washed away my Cities.\n"
		"-They are evened with Atlantis and the towns before the Flood.\n"
		"-\n"
		"-Rain on rain-gorged channels raised the water-levels round them,\n"
		"-Freshet backed on freshet swelled and swept their world from sight,\n"
		"-Till the emboldened floods linked arms and, flashing forward, drowned them--\n"
		"-Drowned my Seven Cities and their peoples in one night!\n"
		"-\n"
		"-Low among the alders lie their derelict foundations,\n"
		"-The beams wherein they trusted and the plinths whereon they built--\n"
		"-My rulers and their treasure and their unborn populations,\n"
		"-Dead, destroyed, aborted, and defiled with mud and silt!\n"
		"-\n"
		"-The Daughters of the Palace whom they cherished in my Cities,\n"
		"-My silver-tongued Princesses, and the promise of their May--\n"
		"-Their bridegrooms of the June-tide--all have perished in my Cities,\n"
		"-With the harsh envenomed virgins that can neither love nor play.\n"
		"-\n"
		"-I was Lord of Cities--I will build anew my Cities,\n"
		"-Seven, set on rocks, above the wrath of any flood.\n"
		"-Nor will I rest from search till I have filled anew my Cities\n"
		"-With peoples undefeated of the dark, enduring blood.\n"
		"+I WAS LORD OF CITIES VERY SUMPTUOUSLY BUILDED.\n"
		"+SEVEN ROARING CITIES PAID ME TRIBUTE FROM AFAR.\n"
		"+IVORY THEIR OUTPOSTS WERE--THE GUARDROOMS OF THEM GILDED,\n"
		"+AND GARRISONED WITH AMAZONS INVINCIBLE IN WAR.\n"
		"+\n"
		"+ALL THE WORLD WENT SOFTLY WHEN IT WALKED BEFORE MY CITIES--\n"
		"+NEITHER KING NOR ARMY VEXED MY PEOPLES AT THEIR TOIL,\n"
		"+NEVER HORSE NOR CHARIOT IRKED OR OVERBORE MY CITIES,\n"
		"+NEVER MOB NOR RULER QUESTIONED WHENCE THEY DREW THEIR SPOIL.\n"
		"+\n"
		"+BANDED, MAILED AND ARROGANT FROM SUNRISE UNTO SUNSET;\n"
		"+SINGING WHILE THEY SACKED IT, THEY POSSESSED THE LAND AT LARGE.\n"
		"+YET WHEN MEN WOULD ROB THEM, THEY RESISTED, THEY MADE ONSET\n"
		"+AND PIERCED THE SMOKE OF BATTLE WITH A THOUSAND-SABRED CHARGE.\n"
		"+\n"
		"+SO THEY WARRED AND TRAFFICKED ONLY YESTERDAY, MY CITIES.\n"
		"+TO-DAY THERE IS NO MARK OR MOUND OF WHERE MY CITIES STOOD.\n"
		"+FOR THE RIVER ROSE AT MIDNIGHT AND IT WASHED AWAY MY CITIES.\n"
		"+THEY ARE EVENED WITH ATLANTIS AND THE TOWNS BEFORE THE FLOOD.\n"
		"+\n"
		"+RAIN ON RAIN-GORGED CHANNELS RAISED THE WATER-LEVELS ROUND THEM,\n"
		"+FRESHET BACKED ON FRESHET SWELLED AND SWEPT THEIR WORLD FROM SIGHT,\n"
		"+TILL THE EMBOLDENED FLOODS LINKED ARMS AND, FLASHING FORWARD, DROWNED THEM--\n"
		"+DROWNED MY SEVEN CITIES AND THEIR PEOPLES IN ONE NIGHT!\n"
		"+\n"
		"+LOW AMONG THE ALDERS LIE THEIR DERELICT FOUNDATIONS,\n"
		"+THE BEAMS WHEREIN THEY TRUSTED AND THE PLINTHS WHEREON THEY BUILT--\n"
		"+MY RULERS AND THEIR TREASURE AND THEIR UNBORN POPULATIONS,\n"
		"+DEAD, DESTROYED, ABORTED, AND DEFILED WITH MUD AND SILT!\n"
		"+\n"
		"+THE DAUGHTERS OF THE PALACE WHOM THEY CHERISHED IN MY CITIES,\n"
		"+MY SILVER-TONGUED PRINCESSES, AND THE PROMISE OF THEIR MAY--\n"
		"+THEIR BRIDEGROOMS OF THE JUNE-TIDE--ALL HAVE PERISHED IN MY CITIES,\n"
		"+WITH THE HARSH ENVENOMED VIRGINS THAT CAN NEITHER LOVE NOR PLAY.\n"
		"+\n"
		"+I WAS LORD OF CITIES--I WILL BUILD ANEW MY CITIES,\n"
		"+SEVEN, SET ON ROCKS, ABOVE THE WRATH OF ANY FLOOD.\n"
		"+NOR WILL I REST FROM SEARCH TILL I HAVE FILLED ANEW MY CITIES\n"
		"+WITH PEOPLES UNDEFEATED OF THE DARK, ENDURING BLOOD.\n"
		" \n"
		" To the sound of trumpets shall their seed restore my Cities\n"
		" Wealthy and well-weaponed, that once more may I behold\n";

	old_tree = resolve_commit_oid_to_tree(g_repo, old_sha);
	new_tree = resolve_commit_oid_to_tree(g_repo, new_sha);

	find_opts.flags = GIT_DIFF_FIND_RENAMES_FROM_REWRITES;

	cl_git_pass(git_diff_tree_to_tree(&diff, g_repo, old_tree, new_tree, NULL));
	cl_git_pass(git_diff_find_similar(diff, &find_opts));

	cl_git_pass(git_diff_to_buf(&diff_buf, diff, GIT_DIFF_FORMAT_PATCH));

	cl_assert_equal_s(expected, diff_buf.ptr);

	git_buf_dispose(&diff_buf);
	git_diff_free(diff);
	git_tree_free(old_tree);
	git_tree_free(new_tree);
}

void test_diff_rename__delete_and_rename(void)
{
	const char *old_sha = RENAME_MODIFICATION_COMMIT;
	const char *new_sha = DELETE_RENAME_COMMIT;
	git_tree *old_tree, *new_tree;
	git_diff *diff;
	git_diff_find_options find_opts = GIT_DIFF_FIND_OPTIONS_INIT;
	git_buf diff_buf = GIT_BUF_INIT;
	const char *expected =
		"diff --git a/sixserving.txt b/sixserving.txt\n"
		"deleted file mode 100644\n"
		"index f90d4fc..0000000\n"
		"--- a/sixserving.txt\n"
		"+++ /dev/null\n"
		"@@ -1,25 +0,0 @@\n"
		"-I  KEEP  six  honest  serving-men\n"
		"-  (They  taught  me  all  I  knew);\n"
		"-Their  names  are  What  and  Why  and  When\n"
		"-  And  How  and  Where  and  Who.\n"
		"-I  send  them  over  land  and  sea,\n"
		"-  I  send  them  east  and  west;\n"
		"-But  after  they  have  worked  for  me,\n"
		"-  I  give  them  all  a  rest.\n"
		"-\n"
		"-I  let  them  rest  from  nine  till  five,\n"
		"-  For  I  am  busy  then,\n"
		"-As  well  as  breakfast,  lunch,  and  tea,\n"
		"-  For  they  are  hungry  men.\n"
		"-But  different  folk  have  different  views;\n"
		"-I  know  a  person  small—\n"
		"-She  keeps  ten  million  serving-men,\n"
		"-Who  get  no  rest  at  all!\n"
		"-\n"
		"-She  sends'em  abroad  on  her  own  affairs,\n"
		"-  From  the  second  she  opens  her  eyes—\n"
		"-One  million  Hows,  two  million  Wheres,\n"
		"-And  seven  million  Whys!\n"
		"-\n"
		"-    --  Rudyard  Kipling\n"
		"-\n"
		"diff --git a/songof7cities.txt b/sixserving.txt\n"
		"similarity index 100%\n"
		"rename from songof7cities.txt\n"
		"rename to sixserving.txt\n";

	old_tree = resolve_commit_oid_to_tree(g_repo, old_sha);
	new_tree = resolve_commit_oid_to_tree(g_repo, new_sha);

	find_opts.flags = GIT_DIFF_FIND_RENAMES_FROM_REWRITES;

	cl_git_pass(git_diff_tree_to_tree(&diff, g_repo, old_tree, new_tree, NULL));
	cl_git_pass(git_diff_find_similar(diff, &find_opts));

	cl_git_pass(git_diff_to_buf(&diff_buf, diff, GIT_DIFF_FORMAT_PATCH));

	cl_assert_equal_s(expected, diff_buf.ptr);

	git_buf_dispose(&diff_buf);
	git_diff_free(diff);
	git_tree_free(old_tree);
	git_tree_free(new_tree);
}

/*
 * The break_rewrite branch contains a testcase reduced from
 * a real-world scenario, rather than being "constructed" like
 * the above tests seem to be. There are two commits layered
 * on top of the repo's initial commit; the base commit which
 * clears out the files from the initial commit and installs
 * four files. And then there's the modification commit which
 * mutates the files in such a way as to trigger the bug in
 * libgit2.
 * commit db98035f715427eef1f5e17f03e1801c05301e9e
 *   serving.txt     (deleted)
 *   sevencities.txt (deleted)
 *   AAA             (313 lines)
 *   BBB             (314 lines)
 *   CCC             (704 lines)
 *   DDD             (314 lines, identical to BBB)
 * commit 7e7bfb88ba9bc65fd700fee1819cf1c317aafa56
 *   This deletes CCC and makes slight modifications
 *   to AAA, BBB, and DDD. The find_best_matches loop
 *   for git_diff_find_similar computes the following:
 *   CCC    moved to    AAA     (similarity 91)
 *   CCC    copied to   AAA     (similarity 91)
 *   DDD    moved to    BBB     (similarity 52)
 *   CCC    copied to   BBB     (similarity 90)
 *   BBB    moved to    DDD     (similarity 52)
 *   CCC    copied to   DDD     (similarity 90)
 * The code to rewrite the diffs by resolving these
 * copies/renames would resolve the BBB <-> DDD moves
 * but then still leave BBB as a rename target for
 * the deleted file CCC. Since the split flag on BBB
 * was cleared, this would trigger an error.
 */
void test_diff_rename__break_rewrite(void)
{
	const char *old_sha = BREAK_REWRITE_BASE_COMMIT;
	const char *new_sha = BREAK_REWRITE_COMMIT;
	git_tree *old_tree, *new_tree;
	git_diff *diff;
	git_diff_find_options find_opts = GIT_DIFF_FIND_OPTIONS_INIT;

	old_tree = resolve_commit_oid_to_tree(g_repo, old_sha);
	new_tree = resolve_commit_oid_to_tree(g_repo, new_sha);

	find_opts.flags = GIT_DIFF_FIND_RENAMES | GIT_DIFF_FIND_COPIES | GIT_DIFF_BREAK_REWRITES | GIT_DIFF_BREAK_REWRITES_FOR_RENAMES_ONLY;

	cl_git_pass(git_diff_tree_to_tree(&diff, g_repo, old_tree, new_tree, NULL));
	cl_git_pass(git_diff_find_similar(diff, &find_opts));

	git_diff_free(diff);
	git_tree_free(old_tree);
	git_tree_free(new_tree);
}
