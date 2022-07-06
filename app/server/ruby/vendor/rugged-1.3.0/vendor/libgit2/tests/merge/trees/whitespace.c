#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "buffer.h"
#include "merge.h"
#include "../merge_helpers.h"
#include "futils.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-whitespace"

#define BRANCH_A_EOL  "branch_a_eol"
#define BRANCH_B_EOL  "branch_b_eol"

#define BRANCH_A_CHANGE  "branch_a_change"
#define BRANCH_B_CHANGE  "branch_b_change"

/* Fixture setup and teardown */
void test_merge_trees_whitespace__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
}

void test_merge_trees_whitespace__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_merge_trees_whitespace__conflict(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "4026a6c83f39c56881c9ac62e7582db9e3d33a4f", 1, "test.txt" },
		{ 0100644, "c3b1fb31424c98072542cc8e42b48c92e52f494a", 2, "test.txt" },
		{ 0100644, "262f67de0de2e535a59ae1bc3c739601e98c354d", 3, "test.txt" },
	};

	cl_git_pass(merge_trees_from_branches(&index, repo, BRANCH_A_EOL, BRANCH_B_EOL, &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 3));

	git_index_free(index);
}

void test_merge_trees_whitespace__eol(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ee3c2aac8e03224c323b58ecb1f9eef616745467", 0, "test.txt" },
	};

	opts.file_flags |= GIT_MERGE_FILE_IGNORE_WHITESPACE_EOL;

	cl_git_pass(merge_trees_from_branches(&index, repo, BRANCH_A_EOL, BRANCH_B_EOL, &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 1));

	git_index_free(index);
}

void test_merge_trees_whitespace__change(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "a827eab4fd66ab37a6ebcfaa7b7e341abfd55947", 0, "test.txt" },
	};

	opts.file_flags |= GIT_MERGE_FILE_IGNORE_WHITESPACE_CHANGE;

	cl_git_pass(merge_trees_from_branches(&index, repo, BRANCH_A_CHANGE, BRANCH_B_CHANGE, &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 1));

	git_index_free(index);
}
