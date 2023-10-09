#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "merge.h"
#include "../merge_helpers.h"
#include "../conflict_data.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-recursive"

void test_merge_workdir_recursive__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
}

void test_merge_workdir_recursive__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_merge_workdir_recursive__writes_conflict_with_virtual_base(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;
	git_str conflicting_buf = GIT_STR_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "fa567f568ed72157c0c617438d077695b99d9aac", 1, "veal.txt" },
		{ 0100644, "21950d5e4e4d1a871b4dfcf72ecb6b9c162c434e", 2, "veal.txt" },
		{ 0100644, "3855170cef875708da06ab9ad7fc6a73b531cda1", 3, "veal.txt" },
	};

	cl_git_pass(merge_branches(repo, GIT_REFS_HEADS_DIR "branchF-1", GIT_REFS_HEADS_DIR "branchF-2", &opts, NULL));

	cl_git_pass(git_repository_index(&index, repo));
	cl_assert(merge_test_index(index, merge_index_entries, 8));

	cl_git_pass(git_futils_readbuffer(&conflicting_buf, "merge-recursive/veal.txt"));

	cl_assert_equal_s(CONFLICTING_RECURSIVE_F1_TO_F2, conflicting_buf.ptr);

	git_index_free(index);
	git_str_dispose(&conflicting_buf);
}

void test_merge_workdir_recursive__conflicting_merge_base_with_diff3(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_str conflicting_buf = GIT_STR_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "0b01d2f70a1c6b9ab60c382f3f9cdc8173da6736", 1, "veal.txt" },
		{ 0100644, "37a5054a9f9b4628e3924c5cb8f2147c6e2a3efc", 2, "veal.txt" },
		{ 0100644, "d604c75019c282144bdbbf3fd3462ba74b240efc", 3, "veal.txt" },
	};

	opts.file_flags |= GIT_MERGE_FILE_STYLE_DIFF3;
	checkout_opts.checkout_strategy |= GIT_CHECKOUT_CONFLICT_STYLE_DIFF3;

	cl_git_pass(merge_branches(repo, GIT_REFS_HEADS_DIR "branchH-2", GIT_REFS_HEADS_DIR "branchH-1", &opts, &checkout_opts));

	cl_git_pass(git_repository_index(&index, repo));
	cl_assert(merge_test_index(index, merge_index_entries, 8));

	cl_git_pass(git_futils_readbuffer(&conflicting_buf, "merge-recursive/veal.txt"));

	cl_assert_equal_s(CONFLICTING_RECURSIVE_H2_TO_H1_WITH_DIFF3, conflicting_buf.ptr);

	git_index_free(index);
	git_str_dispose(&conflicting_buf);
}
