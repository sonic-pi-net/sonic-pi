#include "clar_libgit2.h"
#include "refs.h"
#include "worktree/worktree_helpers.h"

static git_repository *repo;

static void assert_checked_out(git_repository *repo, const char *branch, int checked_out)
{
	git_reference *ref;

	cl_git_pass(git_reference_lookup(&ref, repo, branch));
	cl_assert(git_branch_is_checked_out(ref) == checked_out);

	git_reference_free(ref);
}

void test_refs_branches_checkedout__simple_repo(void)
{
	repo = cl_git_sandbox_init("testrepo");
	assert_checked_out(repo, "refs/heads/master", 1);
	assert_checked_out(repo, "refs/heads/executable", 0);
	cl_git_sandbox_cleanup();
}

void test_refs_branches_checkedout__worktree(void)
{
	static worktree_fixture fixture =
	    WORKTREE_FIXTURE_INIT("testrepo", "testrepo-worktree");

	setup_fixture_worktree(&fixture);

	assert_checked_out(fixture.repo, "refs/heads/master", 1);
	assert_checked_out(fixture.repo, "refs/heads/testrepo-worktree", 1);

	assert_checked_out(fixture.worktree, "refs/heads/master", 1);
	assert_checked_out(fixture.worktree, "refs/heads/testrepo-worktree", 1);

	cleanup_fixture_worktree(&fixture);
}

void test_refs_branches_checkedout__head_is_not_checked_out(void)
{
	repo = cl_git_sandbox_init("testrepo");
	assert_checked_out(repo, "HEAD", 0);
	cl_git_sandbox_cleanup();
}

void test_refs_branches_checkedout__master_in_bare_repo_is_not_checked_out(void)
{
	repo = cl_git_sandbox_init("testrepo.git");
	assert_checked_out(repo, "refs/heads/master", 0);
	cl_git_sandbox_cleanup();
}
