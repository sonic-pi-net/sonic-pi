#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "merge.h"
#include "../merge_helpers.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-recursive"

void test_merge_trees_recursive__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
}

void test_merge_trees_recursive__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_merge_trees_recursive__one_base_commit(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "dea7215f259b2cced87d1bda6c72f8b4ce37a2ff", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" },
	};

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchA-1", "branchA-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 6));

	git_index_free(index);
}

void test_merge_trees_recursive__one_base_commit_norecursive(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "dea7215f259b2cced87d1bda6c72f8b4ce37a2ff", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" },
	};

	opts.flags |= GIT_MERGE_NO_RECURSIVE;

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchA-1", "branchA-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 6));

	git_index_free(index);
}

void test_merge_trees_recursive__two_base_commits(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "666ffdfcf1eaa5641fa31064bf2607327e843c09", 0, "veal.txt" },
	};

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchB-1", "branchB-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 6));

	git_index_free(index);
}

void test_merge_trees_recursive__two_base_commits_norecursive(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "cb49ad76147f5f9439cbd6133708b76142660660", 1, "veal.txt" },
		{ 0100644, "b2a81ead9e722af0099fccfb478cea88eea749a2", 2, "veal.txt" },
		{ 0100644, "4e21d2d63357bde5027d1625f5ec6b430cdeb143", 3, "veal.txt" },
	};

	opts.flags |= GIT_MERGE_NO_RECURSIVE;

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchB-1", "branchB-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 8));

	git_index_free(index);
}

void test_merge_trees_recursive__two_levels_of_multiple_bases(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "15faa0c9991f2d65686e844651faa2ff9827887b", 0, "veal.txt" },
	};

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchC-1", "branchC-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 6));

	git_index_free(index);
}

void test_merge_trees_recursive__two_levels_of_multiple_bases_norecursive(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "b2a81ead9e722af0099fccfb478cea88eea749a2", 1, "veal.txt" },
		{ 0100644, "898d12687fb35be271c27c795a6b32c8b51da79e", 2, "veal.txt" },
		{ 0100644, "68a2e1ee61a23a4728fe6b35580fbbbf729df370", 3, "veal.txt" },
	};

	opts.flags |= GIT_MERGE_NO_RECURSIVE;

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchC-1", "branchC-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 8));

	git_index_free(index);
}

void test_merge_trees_recursive__three_levels_of_multiple_bases(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "d55e5dc038c52f1a36548625bcb666cbc06db9e6", 0, "veal.txt" },
	};

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchD-2", "branchD-1", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 6));

	git_index_free(index);
}

void test_merge_trees_recursive__three_levels_of_multiple_bases_norecursive(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "898d12687fb35be271c27c795a6b32c8b51da79e", 1, "veal.txt" },
		{ 0100644, "f1b44c04989a3a1c14b036cfadfa328d53a7bc5e", 2, "veal.txt" },
		{ 0100644, "5e8747f5200fac0f945a07daf6163ca9cb1a8da9", 3, "veal.txt" },
	};

	opts.flags |= GIT_MERGE_NO_RECURSIVE;

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchD-2", "branchD-1", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 8));

	git_index_free(index);
}

void test_merge_trees_recursive__three_base_commits(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4f7269b07c76d02755d75ccaf05c0b4c36cdc6c", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchE-1", "branchE-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 6));

	git_index_free(index);
}

void test_merge_trees_recursive__three_base_commits_norecursive(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "9e12bce04446d097ae1782967a5888c2e2a0d35b", 1, "gravy.txt" },
		{ 0100644, "d8dd349b78f19a4ebe3357bacb8138f00bf5ed41", 2, "gravy.txt" },
		{ 0100644, "e50fbbd701458757bdfe9815f58ed717c588d1b5", 3, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};

	opts.flags |= GIT_MERGE_NO_RECURSIVE;

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchE-1", "branchE-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 8));

	git_index_free(index);
}

void test_merge_trees_recursive__conflict(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

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

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchF-1", "branchF-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 8));

	git_index_free(index);
}

/* 
 * Branch G-1 and G-2 have three common ancestors (815b5a1, ad2ace9, 483065d).
 * The merge-base of the first two has two common ancestors (723181f, a34e5a1)
 * which themselves have two common ancestors (8f35f30, 3a3f5a6), which
 * finally has a common ancestor of 7c7bf85.  This virtual merge base will
 * be computed and merged with 483065d which also has a common ancestor of
 * 7c7bf85.
 */
void test_merge_trees_recursive__oh_so_many_levels_of_recursion(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "7c7e08f9559d9e1551b91e1cf68f1d0066109add", 0, "oyster.txt" },
		{ 0100644, "898d12687fb35be271c27c795a6b32c8b51da79e", 0, "veal.txt" },
	};

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchG-1", "branchG-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 6));

	git_index_free(index);
}

/* Branch H-1 and H-2 have two common ancestors (aa9e263, 6ef31d3).  The two
 * ancestors themselves conflict.
 */
void test_merge_trees_recursive__conflicting_merge_base(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "3a66812fed1e03ea4a6a7ee28d8a57aec1ca6537", 1, "veal.txt" },
		{ 0100644, "d604c75019c282144bdbbf3fd3462ba74b240efc", 2, "veal.txt" },
		{ 0100644, "37a5054a9f9b4628e3924c5cb8f2147c6e2a3efc", 3, "veal.txt" },
	};

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchH-1", "branchH-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 8));

	git_index_free(index);
}

/* Branch H-1 and H-2 have two common ancestors (aa9e263, 6ef31d3).  The two
 * ancestors themselves conflict.  The generated common ancestor file will
 * have diff3 style conflicts inside it.
 */
void test_merge_trees_recursive__conflicting_merge_base_with_diff3(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "cd17a91513f3aee9e44114d1ede67932dd41d2fc", 1, "veal.txt" },
		{ 0100644, "d604c75019c282144bdbbf3fd3462ba74b240efc", 2, "veal.txt" },
		{ 0100644, "37a5054a9f9b4628e3924c5cb8f2147c6e2a3efc", 3, "veal.txt" },
	};

	opts.file_flags |= GIT_MERGE_FILE_STYLE_DIFF3;

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchH-1", "branchH-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 8));

	git_index_free(index);
}

/* Branch I-1 and I-2 have two common ancestors (aa9e263, 6ef31d3).  The two
 * ancestors themselves conflict, but when each was merged, the conflicts were
 * resolved identically, thus merging I-1 into I-2 does not conflict.
 */
void test_merge_trees_recursive__conflicting_merge_base_since_resolved(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a02d4fd126e0cc8fb46ee48cf38bad36d44f2dbc", 0, "veal.txt" },
	};

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchI-1", "branchI-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 6));

	git_index_free(index);
}

/* There are multiple levels of criss-cross merges, and multiple recursive
 * merges would create a common ancestor that allows the merge to complete
 * successfully.  Test that we can build a single virtual base, then stop,
 * which will produce a conflicting merge.
 */
void test_merge_trees_recursive__recursionlimit(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "ce7e553c6feb6e5f3bd67e3c3be04182fe3094b4", 1, "gravy.txt" },
		{ 0100644, "d8dd349b78f19a4ebe3357bacb8138f00bf5ed41", 2, "gravy.txt" },
		{ 0100644, "e50fbbd701458757bdfe9815f58ed717c588d1b5", 3, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};

	opts.recursion_limit = 1;

	cl_git_pass(merge_commits_from_branches(&index, repo, "branchE-1", "branchE-2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 8));

	git_index_free(index);
}

