#include "clar_libgit2.h"

#include "blame.h"

git_blame *g_blame;

void test_blame_getters__initialize(void)
{
	size_t i;
	git_blame_options opts = GIT_BLAME_OPTIONS_INIT;

	git_blame_hunk hunks[] = {
		{ 3, {{0}},  1, NULL, {{0}}, "a", 0},
		{ 3, {{0}},  4, NULL, {{0}}, "b", 0},
		{ 3, {{0}},  7, NULL, {{0}}, "c", 0},
		{ 3, {{0}}, 10, NULL, {{0}}, "d", 0},
		{ 3, {{0}}, 13, NULL, {{0}}, "e", 0},
	};

	g_blame = git_blame__alloc(NULL, opts, "");

	for (i=0; i<5; i++) {
		git_blame_hunk *h = git__calloc(1, sizeof(git_blame_hunk));
		h->final_start_line_number = hunks[i].final_start_line_number;
		h->orig_path = git__strdup(hunks[i].orig_path);
		h->lines_in_hunk = hunks[i].lines_in_hunk;

		git_vector_insert(&g_blame->hunks, h);
	}
}

void test_blame_getters__cleanup(void)
{
	git_blame_free(g_blame);
}


void test_blame_getters__byindex(void)
{
	const git_blame_hunk *h = git_blame_get_hunk_byindex(g_blame, 2);
	cl_assert(h);
	cl_assert_equal_s(h->orig_path, "c");

	h = git_blame_get_hunk_byindex(g_blame, 95);
	cl_assert_equal_p(h, NULL);
}

void test_blame_getters__byline(void)
{
	const git_blame_hunk *h = git_blame_get_hunk_byline(g_blame, 5);
	cl_assert(h);
	cl_assert_equal_s(h->orig_path, "b");

	h = git_blame_get_hunk_byline(g_blame, 95);
	cl_assert_equal_p(h, NULL);
}
