#include "clar_libgit2.h"

static git_remote *_remote;
static git_repository *_repo;
static git_config *_config;
static char url[] = "http://github.com/libgit2/libgit2.git";

void test_network_remote_createthenload__initialize(void)
{
	cl_fixture_sandbox("testrepo.git");

	cl_git_pass(git_repository_open(&_repo, "testrepo.git"));

	cl_git_pass(git_repository_config(&_config, _repo));
	cl_git_pass(git_config_set_string(_config, "remote.origin.fetch", "+refs/heads/*:refs/remotes/origin/*"));
	cl_git_pass(git_config_set_string(_config, "remote.origin.url", url));
	git_config_free(_config);

	cl_git_pass(git_remote_load(&_remote, _repo, "origin"));
}

void test_network_remote_createthenload__cleanup(void)
{
	git_remote_free(_remote);
	_remote = NULL;

	git_repository_free(_repo);
	_repo = NULL;

	cl_fixture_cleanup("testrepo.git");
}

void test_network_remote_createthenload__parsing(void)
{
	cl_assert_equal_s(git_remote_name(_remote), "origin");
	cl_assert_equal_s(git_remote_url(_remote), url);
}
