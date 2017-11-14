#include "clar_libgit2.h"
#include "remote.h"
#include "repository.h"

#define REPO_PATH "testrepo2/.gitted"
#define REMOTE_ORIGIN "origin"
#define REMOTE_INSTEADOF "insteadof-test"

static git_repository *g_repo;
static git_remote *g_remote;

void test_remote_insteadof__initialize(void)
{
	g_repo = NULL;
	g_remote = NULL;
}

void test_remote_insteadof__cleanup(void)
{
	git_repository_free(g_repo);
	git_remote_free(g_remote);
}

void test_remote_insteadof__url_insteadof_not_applicable(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture(REPO_PATH)));
	cl_git_pass(git_remote_lookup(&g_remote, g_repo, REMOTE_ORIGIN));

	cl_assert_equal_s(
		git_remote_url(g_remote),
		"https://github.com/libgit2/false.git");
}

void test_remote_insteadof__url_insteadof_applicable(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture(REPO_PATH)));
	cl_git_pass(git_remote_lookup(&g_remote, g_repo, REMOTE_INSTEADOF));

	cl_assert_equal_s(
	    git_remote_url(g_remote),
	    "http://github.com/libgit2/libgit2");
}

void test_remote_insteadof__pushurl_insteadof_not_applicable(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture(REPO_PATH)));
	cl_git_pass(git_remote_lookup(&g_remote, g_repo, REMOTE_ORIGIN));

	cl_assert_equal_p(git_remote_pushurl(g_remote), NULL);
}

void test_remote_insteadof__pushurl_insteadof_applicable(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture(REPO_PATH)));
	cl_git_pass(git_remote_lookup(&g_remote, g_repo, REMOTE_INSTEADOF));

	cl_assert_equal_s(
	    git_remote_pushurl(g_remote),
	    "git@github.com:libgit2/libgit2");
}

void test_remote_insteadof__anonymous_remote(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture(REPO_PATH)));
	cl_git_pass(git_remote_create_anonymous(&g_remote, g_repo,
	    "http://example.com/libgit2/libgit2"));

	cl_assert_equal_s(
	    git_remote_url(g_remote),
	    "http://github.com/libgit2/libgit2");
	cl_assert_equal_p(git_remote_pushurl(g_remote), NULL);
}
