#include "clar_libgit2.h"
#include "apply_helpers.h"
#include "../merge/merge_helpers.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-recursive"


void test_apply_tree__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
}

void test_apply_tree__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_apply_tree__one(void)
{
	git_oid a_oid, b_oid;
	git_commit *a_commit, *b_commit;
	git_tree *a_tree, *b_tree;
	git_diff *diff;
	git_index *index = NULL;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;

	struct merge_index_entry expected[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};

	git_oid_fromstr(&a_oid, "539bd011c4822c560c1d17cab095006b7a10f707");
	git_oid_fromstr(&b_oid, "7c7bf85e978f1d18c0566f702d2cb7766b9c8d4f");

	cl_git_pass(git_commit_lookup(&a_commit, repo, &a_oid));
	cl_git_pass(git_commit_lookup(&b_commit, repo, &b_oid));

	cl_git_pass(git_commit_tree(&a_tree, a_commit));
	cl_git_pass(git_commit_tree(&b_tree, b_commit));

	cl_git_pass(git_diff_tree_to_tree(&diff, repo, a_tree, b_tree, &opts));

	cl_git_pass(git_apply_to_tree(&index, repo, a_tree, diff, NULL));
	merge_test_index(index, expected, 6);

	git_index_free(index);
	git_diff_free(diff);
	git_tree_free(a_tree);
	git_tree_free(b_tree);
	git_commit_free(a_commit);
	git_commit_free(b_commit);
}

void test_apply_tree__adds_file(void)
{
	git_oid a_oid;
	git_commit *a_commit;
	git_tree *a_tree;
	git_diff *diff;
	git_index *index = NULL;

	struct merge_index_entry expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "6370543fcfedb3e6516ec53b06158f3687dc1447", 0, "newfile.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" },
	};

	git_oid_fromstr(&a_oid, "539bd011c4822c560c1d17cab095006b7a10f707");

	cl_git_pass(git_commit_lookup(&a_commit, repo, &a_oid));

	cl_git_pass(git_commit_tree(&a_tree, a_commit));

	cl_git_pass(git_diff_from_buffer(&diff,
		DIFF_ADD_FILE, strlen(DIFF_ADD_FILE)));

	cl_git_pass(git_apply_to_tree(&index, repo, a_tree, diff, NULL));
	merge_test_index(index, expected, 7);

	git_index_free(index);
	git_diff_free(diff);
	git_tree_free(a_tree);
	git_commit_free(a_commit);
}
