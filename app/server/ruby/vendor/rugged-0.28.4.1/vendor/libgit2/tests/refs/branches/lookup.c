#include "clar_libgit2.h"
#include "refs.h"

static git_repository *repo;
static git_reference *branch;

void test_refs_branches_lookup__initialize(void)
{
	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));

	branch = NULL;
}

void test_refs_branches_lookup__cleanup(void)
{
	git_reference_free(branch);
	branch = NULL;

	git_repository_free(repo);
	repo = NULL;
}

void test_refs_branches_lookup__can_retrieve_a_local_branch_local(void)
{
	cl_git_pass(git_branch_lookup(&branch, repo, "br2", GIT_BRANCH_LOCAL));
}

void test_refs_branches_lookup__can_retrieve_a_local_branch_all(void)
{
	cl_git_pass(git_branch_lookup(&branch, repo, "br2", GIT_BRANCH_ALL));
}

void test_refs_branches_lookup__trying_to_retrieve_a_local_branch_remote(void)
{
	cl_git_fail(git_branch_lookup(&branch, repo, "br2", GIT_BRANCH_REMOTE));
}

void test_refs_branches_lookup__can_retrieve_a_remote_tracking_branch_remote(void)
{
	cl_git_pass(git_branch_lookup(&branch, repo, "test/master", GIT_BRANCH_REMOTE));
}

void test_refs_branches_lookup__can_retrieve_a_remote_tracking_branch_all(void)
{
	cl_git_pass(git_branch_lookup(&branch, repo, "test/master", GIT_BRANCH_ALL));
}

void test_refs_branches_lookup__trying_to_retrieve_a_remote_tracking_branch_local(void)
{
	cl_git_fail(git_branch_lookup(&branch, repo, "test/master", GIT_BRANCH_LOCAL));
}

void test_refs_branches_lookup__trying_to_retrieve_an_unknown_branch_returns_ENOTFOUND(void)
{
	cl_assert_equal_i(GIT_ENOTFOUND, git_branch_lookup(&branch, repo, "where/are/you", GIT_BRANCH_LOCAL));
	cl_assert_equal_i(GIT_ENOTFOUND, git_branch_lookup(&branch, repo, "over/here", GIT_BRANCH_REMOTE));
	cl_assert_equal_i(GIT_ENOTFOUND, git_branch_lookup(&branch, repo, "maybe/here", GIT_BRANCH_ALL));
}

void test_refs_branches_lookup__trying_to_retrieve_a_branch_with_an_invalid_name_returns_EINVALIDSPEC(void)
{
	cl_assert_equal_i(GIT_EINVALIDSPEC,
		git_branch_lookup(&branch, repo, "are/you/inv@{id", GIT_BRANCH_LOCAL));
	cl_assert_equal_i(GIT_EINVALIDSPEC,
		git_branch_lookup(&branch, repo, "yes/i am", GIT_BRANCH_REMOTE));
	cl_assert_equal_i(GIT_EINVALIDSPEC,
		git_branch_lookup(&branch, repo, "inv al/id", GIT_BRANCH_ALL));
}
