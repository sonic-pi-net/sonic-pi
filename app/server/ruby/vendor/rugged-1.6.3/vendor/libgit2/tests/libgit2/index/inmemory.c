#include "clar_libgit2.h"

void test_index_inmemory__can_create_an_inmemory_index(void)
{
	git_index *index;

	cl_git_pass(git_index_new(&index));
	cl_assert_equal_i(0, (int)git_index_entrycount(index));

	git_index_free(index);
}

void test_index_inmemory__cannot_add_bypath_to_an_inmemory_index(void)
{
	git_index *index;

	cl_git_pass(git_index_new(&index));

	cl_assert_equal_i(GIT_ERROR, git_index_add_bypath(index, "test.txt"));

	git_index_free(index);
}
