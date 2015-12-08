#include "clar_libgit2.h"

void test_refs_crashes__double_free(void)
{
	git_repository *repo;
	git_reference *ref, *ref2;
	const char *REFNAME = "refs/heads/xxx";

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_reference_symbolic_create(&ref, repo, REFNAME, "refs/heads/master", 0, NULL));
	cl_git_pass(git_reference_lookup(&ref2, repo, REFNAME));
	cl_git_pass(git_reference_delete(ref));
	git_reference_free(ref);
	git_reference_free(ref2);

	/* reference is gone from disk, so reloading it will fail */
	cl_git_fail(git_reference_lookup(&ref2, repo, REFNAME));

	git_repository_free(repo);
}
