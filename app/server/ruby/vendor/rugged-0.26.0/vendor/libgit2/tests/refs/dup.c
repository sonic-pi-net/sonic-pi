#include "clar_libgit2.h"
#include "refs.h"

static git_repository *g_repo;

void test_refs_dup__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo.git");
}

void test_refs_dup__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_refs_dup__direct(void)
{
	git_reference *a, *b;

	cl_git_pass(git_reference_lookup(&a, g_repo, "refs/heads/master"));
	cl_git_pass(git_reference_dup(&b, a));

	cl_assert(git_reference_cmp(a, b) == 0);

	git_reference_free(b);
	git_reference_free(a);
}

void test_refs_dup__symbolic(void)
{
	git_reference *a, *b;

	cl_git_pass(git_reference_lookup(&a, g_repo, "HEAD"));
	cl_git_pass(git_reference_dup(&b, a));

	cl_assert(git_reference_cmp(a, b) == 0);

	git_reference_free(b);
	git_reference_free(a);
}
