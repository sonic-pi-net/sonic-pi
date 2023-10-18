#include "clar_libgit2.h"
#include "refs.h"

void test_refs_crashes__double_free(void)
{
	git_repository *repo;
	git_reference *ref, *ref2;
	const char *REFNAME = "refs/heads/xxx";

	repo = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_reference_symbolic_create(&ref, repo, REFNAME, "refs/heads/master", 0, NULL));
	cl_git_pass(git_reference_lookup(&ref2, repo, REFNAME));
	cl_git_pass(git_reference_delete(ref));
	git_reference_free(ref);
	git_reference_free(ref2);

	/* reference is gone from disk, so reloading it will fail */
	cl_git_fail(git_reference_lookup(&ref2, repo, REFNAME));

	cl_git_sandbox_cleanup();
}

void test_refs_crashes__empty_packedrefs(void)
{
	git_repository *repo;
	git_reference *ref;
	const char *REFNAME = "refs/heads/xxx";
	git_str temp_path = GIT_STR_INIT;
	int fd = 0;

	repo = cl_git_sandbox_init("empty_bare.git");

	/* create zero-length packed-refs file */
	cl_git_pass(git_str_joinpath(&temp_path, git_repository_path(repo), GIT_PACKEDREFS_FILE));
	cl_git_pass(((fd = p_creat(temp_path.ptr, 0644)) < 0));
	cl_git_pass(p_close(fd));

	/* should fail gracefully */
	cl_git_fail_with(
	        GIT_ENOTFOUND, git_reference_lookup(&ref, repo, REFNAME));

	cl_git_sandbox_cleanup();
	git_str_dispose(&temp_path);
}
