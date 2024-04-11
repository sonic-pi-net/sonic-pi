#include "clar_libgit2.h"
#include "branch.h"
#include "remote.h"

static git_repository *g_repo;
static const char *remote_tracking_branch_name = "refs/remotes/test/master";
static const char *expected_remote_name = "test";
static int expected_remote_name_length;

void test_refs_branches_remote__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");

	expected_remote_name_length = (int)strlen(expected_remote_name) + 1;
}

void test_refs_branches_remote__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_refs_branches_remote__can_get_remote_for_branch(void)
{
	git_buf remotename = {0};

	cl_git_pass(git_branch_remote_name(&remotename, g_repo, remote_tracking_branch_name));

	cl_assert_equal_s("test", remotename.ptr);
	git_buf_dispose(&remotename);
}

void test_refs_branches_remote__no_matching_remote_returns_error(void)
{
	const char *unknown = "refs/remotes/nonexistent/master";
	git_buf buf = GIT_BUF_INIT;

	git_error_clear();
	cl_git_fail_with(git_branch_remote_name(&buf, g_repo, unknown), GIT_ENOTFOUND);
	cl_assert(git_error_last() != NULL);
}

void test_refs_branches_remote__local_remote_returns_error(void)
{
	const char *local = "refs/heads/master";
	git_buf buf = GIT_BUF_INIT;

	git_error_clear();
	cl_git_fail_with(git_branch_remote_name(&buf, g_repo, local), GIT_ERROR);
	cl_assert(git_error_last() != NULL);
}

void test_refs_branches_remote__ambiguous_remote_returns_error(void)
{
	git_remote *remote;
	git_buf buf = GIT_BUF_INIT;

	/* Create the remote */
	cl_git_pass(git_remote_create_with_fetchspec(&remote, g_repo, "addtest", "http://github.com/libgit2/libgit2", "refs/heads/*:refs/remotes/test/*"));

	git_remote_free(remote);

	git_error_clear();
	cl_git_fail_with(git_branch_remote_name(&buf, g_repo, remote_tracking_branch_name), GIT_EAMBIGUOUS);
	cl_assert(git_error_last() != NULL);
}
