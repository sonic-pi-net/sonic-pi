#include "clar_libgit2.h"

#include "refs.h"

static git_repository *g_repo;

void test_refs_update__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo.git");
}

void test_refs_update__cleanup(void)
{
   cl_git_sandbox_cleanup();
}

void test_refs_update__updating_the_target_of_a_symref_with_an_invalid_name_returns_EINVALIDSPEC(void)
{
	git_reference *head;

	cl_git_pass(git_reference_lookup(&head, g_repo, GIT_HEAD_FILE));
	cl_assert_equal_i(GIT_REFERENCE_SYMBOLIC, git_reference_type(head));
	git_reference_free(head);

	cl_assert_equal_i(GIT_EINVALIDSPEC, git_reference_symbolic_create(&head, g_repo, GIT_HEAD_FILE, "refs/heads/inv@{id", 1, NULL));
}
