#include "clar.h"
#include "clar_libgit2.h"

#include "buffer.h"
#include "futils.h"
#include "git2/revert.h"

#include "../merge/merge_helpers.h"

#define TEST_REPO_PATH "revert"

static git_repository *repo;
static git_index *repo_index;

/* Fixture setup and teardown */
void test_revert_workdir__initialize(void)
{
	git_config *cfg;

	repo = cl_git_sandbox_init(TEST_REPO_PATH);
	git_repository_index(&repo_index, repo);

	/* Ensure that the user's merge.conflictstyle doesn't interfere */
	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_set_string(cfg, "merge.conflictstyle", "merge"));
	git_config_free(cfg);
}

void test_revert_workdir__cleanup(void)
{
	git_index_free(repo_index);
	cl_git_sandbox_cleanup();
}

/* git reset --hard 72333f47d4e83616630ff3b0ffe4c0faebcc3c45
 * git revert --no-commit d1d403d22cbe24592d725f442835cf46fe60c8ac */
void test_revert_workdir__automerge(void)
{
	git_commit *head, *commit;
	git_oid head_oid, revert_oid;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "caf99de3a49827117bb66721010eac461b06a80c", 0, "file1.txt" },
		{ 0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt" },
		{ 0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt" },
		{ 0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 0, "file6.txt" },
	};

	git_oid_fromstr(&head_oid, "72333f47d4e83616630ff3b0ffe4c0faebcc3c45");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&revert_oid, "d1d403d22cbe24592d725f442835cf46fe60c8ac");
	cl_git_pass(git_commit_lookup(&commit, repo, &revert_oid));
	cl_git_pass(git_revert(repo, commit, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 4));

	git_commit_free(commit);
	git_commit_free(head);
}

/* git revert --no-commit 72333f47d4e83616630ff3b0ffe4c0faebcc3c45 */
void test_revert_workdir__conflicts(void)
{
	git_reference *head_ref;
	git_commit *head, *commit;
	git_oid revert_oid;
	git_buf conflicting_buf = GIT_BUF_INIT, mergemsg_buf = GIT_BUF_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "7731926a337c4eaba1e2187d90ebfa0a93659382", 1, "file1.txt" },
		{ 0100644, "4b8fcff56437e60f58e9a6bc630dd242ebf6ea2c", 2, "file1.txt" },
		{ 0100644, "3a3ef367eaf3fe79effbfb0a56b269c04c2b59fe", 3, "file1.txt" },
		{ 0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt" },
		{ 0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt" },
		{ 0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 0, "file6.txt" },
	};

	git_oid_fromstr(&revert_oid, "72333f47d4e83616630ff3b0ffe4c0faebcc3c45");

	cl_git_pass(git_repository_head(&head_ref, repo));
	cl_git_pass(git_reference_peel((git_object **)&head, head_ref, GIT_OBJECT_COMMIT));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	cl_git_pass(git_commit_lookup(&commit, repo, &revert_oid));
	cl_git_pass(git_revert(repo, commit, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 6));

	cl_git_pass(git_futils_readbuffer(&conflicting_buf,
		TEST_REPO_PATH "/file1.txt"));
	cl_assert(strcmp(conflicting_buf.ptr, "!File one!\n" \
		"!File one!\n" \
		"File one!\n" \
		"File one\n" \
		"File one\n" \
		"File one\n" \
		"File one\n" \
		"File one\n" \
		"File one\n" \
		"File one\n" \
		"<<<<<<< HEAD\n" \
		"File one!\n" \
		"!File one!\n" \
		"!File one!\n" \
		"!File one!\n" \
		"=======\n" \
		"File one\n" \
		"File one\n" \
		"File one\n" \
		"File one\n" \
		">>>>>>> parent of 72333f4... automergeable changes\n") == 0);

	cl_assert(git_path_exists(TEST_REPO_PATH "/.git/MERGE_MSG"));
	cl_git_pass(git_futils_readbuffer(&mergemsg_buf,
		TEST_REPO_PATH "/.git/MERGE_MSG"));
	cl_assert(strcmp(mergemsg_buf.ptr,
		"Revert \"automergeable changes\"\n" \
		"\n" \
		"This reverts commit 72333f47d4e83616630ff3b0ffe4c0faebcc3c45.\n"
		"\n" \
		"Conflicts:\n" \
		"\tfile1.txt\n") == 0);

	git_commit_free(commit);
	git_commit_free(head);
	git_reference_free(head_ref);
	git_buf_dispose(&mergemsg_buf);
	git_buf_dispose(&conflicting_buf);
}

/* git reset --hard 39467716290f6df775a91cdb9a4eb39295018145
 * git revert --no-commit ebb03002cee5d66c7732dd06241119fe72ab96a5
*/
void test_revert_workdir__orphan(void)
{
	git_commit *head, *commit;
	git_oid head_oid, revert_oid;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "296a6d3be1dff05c5d1f631d2459389fa7b619eb", 0, "file-mainline.txt" },
	};

	git_oid_fromstr(&head_oid, "39467716290f6df775a91cdb9a4eb39295018145");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&revert_oid, "ebb03002cee5d66c7732dd06241119fe72ab96a5");
	cl_git_pass(git_commit_lookup(&commit, repo, &revert_oid));
	cl_git_pass(git_revert(repo, commit, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 1));

	git_commit_free(commit);
	git_commit_free(head);
}

/*
 * revert the same commit twice (when the first reverts cleanly):
 *
 * git revert 2d440f2
 * git revert 2d440f2
 */
void test_revert_workdir__again(void)
{
	git_reference *head_ref;
	git_commit *orig_head;
	git_tree *reverted_tree;
	git_oid reverted_tree_oid, reverted_commit_oid;
	git_signature *signature;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "7731926a337c4eaba1e2187d90ebfa0a93659382", 0, "file1.txt" },
		{ 0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt" },
		{ 0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt" },
		{ 0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 0, "file6.txt" },
	};

	cl_git_pass(git_repository_head(&head_ref, repo));
	cl_git_pass(git_reference_peel((git_object **)&orig_head, head_ref, GIT_OBJECT_COMMIT));
	cl_git_pass(git_reset(repo, (git_object *)orig_head, GIT_RESET_HARD, NULL));

	cl_git_pass(git_revert(repo, orig_head, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 4));

	cl_git_pass(git_index_write_tree(&reverted_tree_oid, repo_index));
	cl_git_pass(git_tree_lookup(&reverted_tree, repo, &reverted_tree_oid));

	cl_git_pass(git_signature_new(&signature, "Reverter", "reverter@example.org", time(NULL), 0));
	cl_git_pass(git_commit_create(&reverted_commit_oid, repo, "HEAD", signature, signature, NULL, "Reverted!", reverted_tree, 1, (const git_commit **)&orig_head));

	cl_git_pass(git_revert(repo, orig_head, NULL));
	cl_assert(merge_test_index(repo_index, merge_index_entries, 4));

	git_signature_free(signature);
	git_tree_free(reverted_tree);
	git_commit_free(orig_head);
	git_reference_free(head_ref);
}

/* git reset --hard 72333f47d4e83616630ff3b0ffe4c0faebcc3c45
 * git revert --no-commit d1d403d22cbe24592d725f442835cf46fe60c8ac */
void test_revert_workdir__again_after_automerge(void)
{
	git_commit *head, *commit;
	git_tree *reverted_tree;
	git_oid head_oid, revert_oid, reverted_tree_oid, reverted_commit_oid;
	git_signature *signature;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "caf99de3a49827117bb66721010eac461b06a80c", 0, "file1.txt" },
		{ 0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt" },
		{ 0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt" },
		{ 0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 0, "file6.txt" },
	};

	struct merge_index_entry second_revert_entries[] = {
		{ 0100644, "3a3ef367eaf3fe79effbfb0a56b269c04c2b59fe", 1, "file1.txt" },
		{ 0100644, "caf99de3a49827117bb66721010eac461b06a80c", 2, "file1.txt" },
		{ 0100644, "747726e021bc5f44b86de60e3032fd6f9f1b8383", 3, "file1.txt" },
		{ 0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt" },
		{ 0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt" },
		{ 0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 0, "file6.txt" },
	};

	git_oid_fromstr(&head_oid, "72333f47d4e83616630ff3b0ffe4c0faebcc3c45");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&revert_oid, "d1d403d22cbe24592d725f442835cf46fe60c8ac");
	cl_git_pass(git_commit_lookup(&commit, repo, &revert_oid));
	cl_git_pass(git_revert(repo, commit, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 4));

	cl_git_pass(git_index_write_tree(&reverted_tree_oid, repo_index));
	cl_git_pass(git_tree_lookup(&reverted_tree, repo, &reverted_tree_oid));

	cl_git_pass(git_signature_new(&signature, "Reverter", "reverter@example.org", time(NULL), 0));
	cl_git_pass(git_commit_create(&reverted_commit_oid, repo, "HEAD", signature, signature, NULL, "Reverted!", reverted_tree, 1, (const git_commit **)&head));

	cl_git_pass(git_revert(repo, commit, NULL));
	cl_assert(merge_test_index(repo_index, second_revert_entries, 6));

	git_signature_free(signature);
	git_tree_free(reverted_tree);
	git_commit_free(commit);
	git_commit_free(head);
}

/*
 * revert the same commit twice (when the first reverts cleanly):
 *
 * git revert 2d440f2
 * git revert 2d440f2
 */
void test_revert_workdir__again_after_edit(void)
{
	git_reference *head_ref;
	git_commit *orig_head, *commit;
	git_tree *reverted_tree;
	git_oid orig_head_oid, revert_oid, reverted_tree_oid, reverted_commit_oid;
	git_signature *signature;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "3721552e06c4bdc7d478e0674e6304888545d5fd", 0, "file1.txt" },
		{ 0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt" },
		{ 0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt" },
		{ 0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 0, "file6.txt" },
	};

	cl_git_pass(git_repository_head(&head_ref, repo));

	cl_git_pass(git_oid_fromstr(&orig_head_oid, "399fb3aba3d9d13f7d40a9254ce4402067ef3149"));
	cl_git_pass(git_commit_lookup(&orig_head, repo, &orig_head_oid));
	cl_git_pass(git_reset(repo, (git_object *)orig_head, GIT_RESET_HARD, NULL));

	cl_git_pass(git_oid_fromstr(&revert_oid, "2d440f2b3147d3dc7ad1085813478d6d869d5a4d"));
	cl_git_pass(git_commit_lookup(&commit, repo, &revert_oid));

	cl_git_pass(git_revert(repo, commit, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 4));

	cl_git_pass(git_index_write_tree(&reverted_tree_oid, repo_index));
	cl_git_pass(git_tree_lookup(&reverted_tree, repo, &reverted_tree_oid));

	cl_git_pass(git_signature_new(&signature, "Reverter", "reverter@example.org", time(NULL), 0));
	cl_git_pass(git_commit_create(&reverted_commit_oid, repo, "HEAD", signature, signature, NULL, "Reverted!", reverted_tree, 1, (const git_commit **)&orig_head));

	cl_git_pass(git_revert(repo, commit, NULL));
	cl_assert(merge_test_index(repo_index, merge_index_entries, 4));

	git_signature_free(signature);
	git_tree_free(reverted_tree);
	git_commit_free(commit);
	git_commit_free(orig_head);
	git_reference_free(head_ref);
}

/*
 * revert the same commit twice (when the first reverts cleanly):
 *
 * git reset --hard 75ec9929465623f17ff3ad68c0438ea56faba815
 * git revert 97e52d5e81f541080cd6b92829fb85bc4d81d90b
 */
void test_revert_workdir__again_after_edit_two(void)
{
	git_buf diff_buf = GIT_BUF_INIT;
	git_config *config;
	git_oid head_commit_oid, revert_commit_oid;
	git_commit *head_commit, *revert_commit;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "a8c86221b400b836010567cc3593db6e96c1a83a", 1, "file.txt" },
		{ 0100644, "46ff0854663aeb2182b9838c8da68e33ac23bc1e", 2, "file.txt" },
		{ 0100644, "21a96a98ed84d45866e1de6e266fd3a61a4ae9dc", 3, "file.txt" },
	};

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_bool(config, "core.autocrlf", 0));

	cl_git_pass(git_oid_fromstr(&head_commit_oid, "75ec9929465623f17ff3ad68c0438ea56faba815"));
	cl_git_pass(git_commit_lookup(&head_commit, repo, &head_commit_oid));
	cl_git_pass(git_reset(repo, (git_object *)head_commit, GIT_RESET_HARD, NULL));

	cl_git_pass(git_oid_fromstr(&revert_commit_oid, "97e52d5e81f541080cd6b92829fb85bc4d81d90b"));
	cl_git_pass(git_commit_lookup(&revert_commit, repo, &revert_commit_oid));

	cl_git_pass(git_revert(repo, revert_commit, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 3));

	cl_git_pass(git_futils_readbuffer(&diff_buf, "revert/file.txt"));
	cl_assert_equal_s(
			"a\n" \
			"<<<<<<< HEAD\n" \
			"=======\n" \
			"a\n" \
			">>>>>>> parent of 97e52d5... Revert me\n" \
			"a\n" \
			"a\n" \
			"a\n" \
			"a\n" \
			"ab",
		diff_buf.ptr);

	git_commit_free(revert_commit);
	git_commit_free(head_commit);
	git_config_free(config);
	git_buf_dispose(&diff_buf);
}

/* git reset --hard 72333f47d4e83616630ff3b0ffe4c0faebcc3c45
 * git revert --no-commit d1d403d22cbe24592d725f442835cf46fe60c8ac */
void test_revert_workdir__conflict_use_ours(void)
{
	git_commit *head, *commit;
	git_oid head_oid, revert_oid;
	git_revert_options opts = GIT_REVERT_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "caf99de3a49827117bb66721010eac461b06a80c", 0, "file1.txt" },
		{ 0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt" },
		{ 0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt" },
		{ 0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 0, "file6.txt" },
	};

	struct merge_index_entry merge_filesystem_entries[] = {
		{ 0100644, "caf99de3a49827117bb66721010eac461b06a80c", 0, "file1.txt" },
		{ 0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt" },
		{ 0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt" },
		{ 0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 0, "file6.txt" },
	};

	opts.checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_USE_OURS;

	git_oid_fromstr(&head_oid, "72333f47d4e83616630ff3b0ffe4c0faebcc3c45");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&revert_oid, "d1d403d22cbe24592d725f442835cf46fe60c8ac");
	cl_git_pass(git_commit_lookup(&commit, repo, &revert_oid));
	cl_git_pass(git_revert(repo, commit, &opts));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 4));
	cl_assert(merge_test_workdir(repo, merge_filesystem_entries, 4));

	git_commit_free(commit);
	git_commit_free(head);
}

/* git reset --hard cef56612d71a6af8d8015691e4865f7fece905b5
 * git revert --no-commit 55568c8de5322ff9a95d72747a239cdb64a19965
 */
void test_revert_workdir__rename_1_of_2(void)
{
	git_commit *head, *commit;
	git_oid head_oid, revert_oid;
	git_revert_options opts = GIT_REVERT_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "747726e021bc5f44b86de60e3032fd6f9f1b8383", 0, "file1.txt" },
		{ 0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt" },
		{ 0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt" },
		{ 0100644, "55acf326a69f0aab7a974ec53ffa55a50bcac14e", 3, "file4.txt" },
		{ 0100644, "55acf326a69f0aab7a974ec53ffa55a50bcac14e", 1, "file5.txt" },
		{ 0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 2, "file6.txt" },
	};

	opts.merge_opts.flags |= GIT_MERGE_FIND_RENAMES;
	opts.merge_opts.rename_threshold = 50;

	git_oid_fromstr(&head_oid, "cef56612d71a6af8d8015691e4865f7fece905b5");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&revert_oid, "55568c8de5322ff9a95d72747a239cdb64a19965");
	cl_git_pass(git_commit_lookup(&commit, repo, &revert_oid));
	cl_git_pass(git_revert(repo, commit, &opts));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 6));

	git_commit_free(commit);
	git_commit_free(head);
}

/* git reset --hard 55568c8de5322ff9a95d72747a239cdb64a19965
 * git revert --no-commit HEAD~1 */
void test_revert_workdir__rename(void)
{
	git_commit *head, *commit;
	git_oid head_oid, revert_oid;
	git_revert_options opts = GIT_REVERT_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "55acf326a69f0aab7a974ec53ffa55a50bcac14e", 1, "file4.txt" },
		{ 0100644, "55acf326a69f0aab7a974ec53ffa55a50bcac14e", 2, "file5.txt" },
	};

	struct merge_name_entry merge_name_entries[] = {
		{ "file4.txt", "file5.txt", "" },
	};

	opts.merge_opts.flags |= GIT_MERGE_FIND_RENAMES;
	opts.merge_opts.rename_threshold = 50;

	git_oid_fromstr(&head_oid, "55568c8de5322ff9a95d72747a239cdb64a19965");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&revert_oid, "0aa8c7e40d342fff78d60b29a4ba8e993ed79c51");
	cl_git_pass(git_commit_lookup(&commit, repo, &revert_oid));
	cl_git_pass(git_revert(repo, commit, &opts));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 2));
	cl_assert(merge_test_names(repo_index, merge_name_entries, 1));

	git_commit_free(commit);
	git_commit_free(head);
}

/* git revert --no-commit HEAD */
void test_revert_workdir__head(void)
{
	git_reference *head;
	git_commit *commit;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "7731926a337c4eaba1e2187d90ebfa0a93659382", 0, "file1.txt" },
		{ 0100644, "0ab09ea6d4c3634bdf6c221626d8b6f7dd890767", 0, "file2.txt" },
		{ 0100644, "f4e107c230d08a60fb419d19869f1f282b272d9c", 0, "file3.txt" },
		{ 0100644, "0f5bfcf58c558d865da6be0281d7795993646cee", 0, "file6.txt" },
	};

	/* HEAD is 2d440f2b3147d3dc7ad1085813478d6d869d5a4d */
	cl_git_pass(git_repository_head(&head, repo));
	cl_git_pass(git_reference_peel((git_object **)&commit, head, GIT_OBJECT_COMMIT));
	cl_git_pass(git_reset(repo, (git_object *)commit, GIT_RESET_HARD, NULL));
	cl_git_pass(git_revert(repo, commit, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 4));
	cl_assert(merge_test_workdir(repo, merge_index_entries, 4));

	git_reference_free(head);
	git_commit_free(commit);
}

void test_revert_workdir__nonmerge_fails_mainline_specified(void)
{
	git_reference *head;
	git_commit *commit;
	git_revert_options opts = GIT_REVERT_OPTIONS_INIT;

	cl_git_pass(git_repository_head(&head, repo));
	cl_git_pass(git_reference_peel((git_object **)&commit, head, GIT_OBJECT_COMMIT));

	opts.mainline = 1;
	cl_must_fail(git_revert(repo, commit, &opts));
	cl_assert(!git_path_exists(TEST_REPO_PATH "/.git/MERGE_MSG"));
	cl_assert(!git_path_exists(TEST_REPO_PATH "/.git/REVERT_HEAD"));

	git_reference_free(head);
	git_commit_free(commit);
}

/* git reset --hard 5acdc74af27172ec491d213ee36cea7eb9ef2579
 * git revert HEAD */
void test_revert_workdir__merge_fails_without_mainline_specified(void)
{
	git_commit *head;
	git_oid head_oid;

	git_oid_fromstr(&head_oid, "5acdc74af27172ec491d213ee36cea7eb9ef2579");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	cl_must_fail(git_revert(repo, head, NULL));
	cl_assert(!git_path_exists(TEST_REPO_PATH "/.git/MERGE_MSG"));
	cl_assert(!git_path_exists(TEST_REPO_PATH "/.git/REVERT_HEAD"));

	git_commit_free(head);
}

/* git reset --hard 5acdc74af27172ec491d213ee36cea7eb9ef2579
 * git revert HEAD -m1 --no-commit */
void test_revert_workdir__merge_first_parent(void)
{
	git_commit *head;
	git_oid head_oid;
	git_revert_options opts = GIT_REVERT_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "296a6d3be1dff05c5d1f631d2459389fa7b619eb", 0, "file-mainline.txt" },
		{ 0100644, "0cdb66192ee192f70f891f05a47636057420e871", 0, "file1.txt" },
		{ 0100644, "73ec36fa120f8066963a0bc9105bb273dbd903d7", 0, "file2.txt" },
	};

	opts.mainline = 1;

	git_oid_fromstr(&head_oid, "5acdc74af27172ec491d213ee36cea7eb9ef2579");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	cl_git_pass(git_revert(repo, head, &opts));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 3));

	git_commit_free(head);
}

void test_revert_workdir__merge_second_parent(void)
{
	git_commit *head;
	git_oid head_oid;
	git_revert_options opts = GIT_REVERT_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "33c6fd981c49a2abf2971482089350bfc5cda8ea", 0, "file-branch.txt" },
		{ 0100644, "0cdb66192ee192f70f891f05a47636057420e871", 0, "file1.txt" },
		{ 0100644, "73ec36fa120f8066963a0bc9105bb273dbd903d7", 0, "file2.txt" },
	};

	opts.mainline = 2;

	git_oid_fromstr(&head_oid, "5acdc74af27172ec491d213ee36cea7eb9ef2579");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	cl_git_pass(git_revert(repo, head, &opts));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 3));

	git_commit_free(head);
}
