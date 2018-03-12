#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "buffer.h"
#include "merge.h"
#include "../merge_helpers.h"
#include "fileops.h"
#include "refs.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-resolve"

#define BRANCH_RENAME_OURS					"rename_conflict_ours"
#define BRANCH_RENAME_THEIRS				"rename_conflict_theirs"

// Fixture setup and teardown
void test_merge_workdir_renames__initialize(void)
{
	git_config *cfg;

	repo = cl_git_sandbox_init(TEST_REPO_PATH);

	/* Ensure that the user's merge.conflictstyle doesn't interfere */
	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_set_string(cfg, "merge.conflictstyle", "merge"));
	git_config_free(cfg);
}

void test_merge_workdir_renames__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_merge_workdir_renames__renames(void)
{
	git_merge_options merge_opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "68c6c84b091926c7d90aa6a79b2bc3bb6adccd8e", 0, "0a-no-change.txt" },
		{ 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 0, "0b-duplicated-in-ours.txt" },
		{ 0100644, "8aac75de2a34b4d340bf62a6e58197269cb55797", 0, "0b-rewritten-in-ours.txt" },
		{ 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 0, "0c-duplicated-in-theirs.txt" },
		{ 0100644, "7edc726325da726751a4195e434e4377b0f67f9a", 0, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "0d872f8e871a30208305978ecbf9e66d864f1638", 0, "1a-newname-in-ours-edited-in-theirs.txt" },
		{ 0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb", 0, "1a-newname-in-ours.txt" },
		{ 0100644, "ed9523e62e453e50dd9be1606af19399b96e397a", 0, "1b-newname-in-theirs-edited-in-ours.txt" },
		{ 0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136", 0, "1b-newname-in-theirs.txt" },
		{ 0100644, "178940b450f238a56c0d75b7955cb57b38191982", 0, "2-newname-in-both.txt" },
		{ 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 0, "3a-newname-in-ours-deleted-in-theirs.txt" },
		{ 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 0, "3b-newname-in-theirs-deleted-in-ours.txt" },
		{ 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 0, "4a-newname-in-ours-added-in-theirs.txt~HEAD" },
		{ 0100644, "8b5b53cb2aa9ceb1139f5312fcfa3cc3c5a47c9a", 0, "4a-newname-in-ours-added-in-theirs.txt~rename_conflict_theirs" },
		{ 0100644, "de872ee3618b894992e9d1e18ba2ebe256a112f9", 0, "4b-newname-in-theirs-added-in-ours.txt~HEAD" },
		{ 0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db", 0, "4b-newname-in-theirs-added-in-ours.txt~rename_conflict_theirs" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 0, "5a-newname-in-ours-added-in-theirs.txt~HEAD" },
		{ 0100644, "98ba4205fcf31f5dd93c916d35fe3f3b3d0e6714", 0, "5a-newname-in-ours-added-in-theirs.txt~rename_conflict_theirs" },
		{ 0100644, "385c8a0f26ddf79e9041e15e17dc352ed2c4cced", 0, "5b-newname-in-theirs-added-in-ours.txt~HEAD" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 0, "5b-newname-in-theirs-added-in-ours.txt~rename_conflict_theirs" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 0, "6-both-renamed-1-to-2-ours.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 0, "6-both-renamed-1-to-2-theirs.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 0, "7-both-renamed.txt~HEAD" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 0, "7-both-renamed.txt~rename_conflict_theirs" },
	};

	merge_opts.flags |= GIT_MERGE_FIND_RENAMES;
	merge_opts.rename_threshold = 50;

	cl_git_pass(merge_branches(repo, GIT_REFS_HEADS_DIR BRANCH_RENAME_OURS, GIT_REFS_HEADS_DIR BRANCH_RENAME_THEIRS, &merge_opts, NULL));
	cl_assert(merge_test_workdir(repo, merge_index_entries, 24));
}

void test_merge_workdir_renames__ours(void)
{
	git_index *index;
	git_merge_options merge_opts = GIT_MERGE_OPTIONS_INIT;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "68c6c84b091926c7d90aa6a79b2bc3bb6adccd8e", 0, "0a-no-change.txt" },
		{ 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 0, "0b-duplicated-in-ours.txt" },
		{ 0100644, "e376fbdd06ebf021c92724da9f26f44212734e3e", 0, "0b-rewritten-in-ours.txt" },
		{ 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 0, "0c-duplicated-in-theirs.txt" },
		{ 0100644, "efc9121fdedaf08ba180b53ebfbcf71bd488ed09", 0, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "0d872f8e871a30208305978ecbf9e66d864f1638", 0, "1a-newname-in-ours-edited-in-theirs.txt" },
		{ 0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb", 0, "1a-newname-in-ours.txt" },
		{ 0100644, "ed9523e62e453e50dd9be1606af19399b96e397a", 0, "1b-newname-in-theirs-edited-in-ours.txt" },
		{ 0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136", 0, "1b-newname-in-theirs.txt" },
		{ 0100644, "178940b450f238a56c0d75b7955cb57b38191982", 0, "2-newname-in-both.txt" },
		{ 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 0, "3a-newname-in-ours-deleted-in-theirs.txt" },
		{ 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 0, "3b-newname-in-theirs-deleted-in-ours.txt" },
		{ 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 0, "4a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "de872ee3618b894992e9d1e18ba2ebe256a112f9", 0, "4b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 0, "5a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "385c8a0f26ddf79e9041e15e17dc352ed2c4cced", 0, "5b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 0, "5b-renamed-in-theirs-added-in-ours.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 0, "6-both-renamed-1-to-2-ours.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 0, "7-both-renamed-side-2.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 0, "7-both-renamed.txt" },
	};

	merge_opts.flags |= GIT_MERGE_FIND_RENAMES;
	merge_opts.rename_threshold = 50;
	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_USE_OURS;

	cl_git_pass(merge_branches(repo, GIT_REFS_HEADS_DIR BRANCH_RENAME_OURS, GIT_REFS_HEADS_DIR BRANCH_RENAME_THEIRS, &merge_opts, &checkout_opts));
	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_write(index));
	cl_assert(merge_test_workdir(repo, merge_index_entries, 20));

	git_index_free(index);
}

void test_merge_workdir_renames__similar(void)
{
	git_merge_options merge_opts = GIT_MERGE_OPTIONS_INIT;

	/*
	 * Note: this differs slightly from the core git merge result - there, 4a is
	 * tracked as a rename/delete instead of a rename/add and the theirs side
	 * is not placed in workdir in any form.
	 */
	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "68c6c84b091926c7d90aa6a79b2bc3bb6adccd8e", 0, "0a-no-change.txt" },
		{ 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 0, "0b-duplicated-in-ours.txt" },
		{ 0100644, "8aac75de2a34b4d340bf62a6e58197269cb55797", 0, "0b-rewritten-in-ours.txt" },
		{ 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 0, "0c-duplicated-in-theirs.txt" },
		{ 0100644, "7edc726325da726751a4195e434e4377b0f67f9a", 0, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "0d872f8e871a30208305978ecbf9e66d864f1638", 0, "1a-newname-in-ours-edited-in-theirs.txt" },
		{ 0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb", 0, "1a-newname-in-ours.txt" },
		{ 0100644, "ed9523e62e453e50dd9be1606af19399b96e397a", 0, "1b-newname-in-theirs-edited-in-ours.txt" },
		{ 0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136", 0, "1b-newname-in-theirs.txt" },
		{ 0100644, "178940b450f238a56c0d75b7955cb57b38191982", 0, "2-newname-in-both.txt" },
		{ 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 0, "3a-newname-in-ours-deleted-in-theirs.txt" },
		{ 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 0, "3b-newname-in-theirs-deleted-in-ours.txt" },
		{ 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 0, "4a-newname-in-ours-added-in-theirs.txt~HEAD" },
		{ 0100644, "8b5b53cb2aa9ceb1139f5312fcfa3cc3c5a47c9a", 0, "4a-newname-in-ours-added-in-theirs.txt~rename_conflict_theirs" },
		{ 0100644, "de872ee3618b894992e9d1e18ba2ebe256a112f9", 0, "4b-newname-in-theirs-added-in-ours.txt~HEAD" },
		{ 0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db", 0, "4b-newname-in-theirs-added-in-ours.txt~rename_conflict_theirs" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 0, "5a-newname-in-ours-added-in-theirs.txt~HEAD" },
		{ 0100644, "98ba4205fcf31f5dd93c916d35fe3f3b3d0e6714", 0, "5a-newname-in-ours-added-in-theirs.txt~rename_conflict_theirs" },
		{ 0100644, "385c8a0f26ddf79e9041e15e17dc352ed2c4cced", 0, "5b-newname-in-theirs-added-in-ours.txt~HEAD" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 0, "5b-newname-in-theirs-added-in-ours.txt~rename_conflict_theirs" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 0, "6-both-renamed-1-to-2-ours.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 0, "6-both-renamed-1-to-2-theirs.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 0, "7-both-renamed.txt~HEAD" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 0, "7-both-renamed.txt~rename_conflict_theirs" },
	};

	merge_opts.flags |= GIT_MERGE_FIND_RENAMES;
	merge_opts.rename_threshold = 50;

	cl_git_pass(merge_branches(repo, GIT_REFS_HEADS_DIR BRANCH_RENAME_OURS, GIT_REFS_HEADS_DIR BRANCH_RENAME_THEIRS, &merge_opts, NULL));
	cl_assert(merge_test_workdir(repo, merge_index_entries, 24));
}

