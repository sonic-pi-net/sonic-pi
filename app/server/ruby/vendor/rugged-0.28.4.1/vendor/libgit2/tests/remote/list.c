#include "clar_libgit2.h"
#include "config/config_helpers.h"

static git_repository *_repo;

#define TEST_URL "http://github.com/libgit2/libgit2.git"

void test_remote_list__initialize(void)
{
	_repo = cl_git_sandbox_init("testrepo");
}

void test_remote_list__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_remote_list__always_checks_disk_config(void)
{
	git_repository *repo;
	git_strarray remotes;
	git_remote *remote;

	cl_git_pass(git_repository_open(&repo, git_repository_path(_repo)));

	cl_git_pass(git_remote_list(&remotes, _repo));
	cl_assert_equal_sz(remotes.count, 1);
	git_strarray_free(&remotes);

	cl_git_pass(git_remote_create(&remote, _repo, "valid-name", TEST_URL));

	cl_git_pass(git_remote_list(&remotes, _repo));
	cl_assert_equal_sz(remotes.count, 2);
	git_strarray_free(&remotes);

	cl_git_pass(git_remote_list(&remotes, repo));
	cl_assert_equal_sz(remotes.count, 2);
	git_strarray_free(&remotes);

	git_repository_free(repo);
	git_remote_free(remote);
}

