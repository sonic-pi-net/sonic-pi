#include "clar_libgit2.h"
#include "config/config_helpers.h"

#include "repository.h"

static git_repository *_repo;

void test_network_remote_delete__initialize(void)
{
	_repo = cl_git_sandbox_init("testrepo.git");
}

void test_network_remote_delete__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_network_remote_delete__remove_remote_tracking_branches(void)
{
	git_reference *ref;

	cl_git_pass(git_remote_delete(_repo, "test"));
	cl_git_fail_with(GIT_ENOTFOUND, git_reference_lookup(&ref, _repo, "refs/remotes/test/master"));
}

void test_network_remote_delete__remove_remote_configuration_settings(void)
{
	cl_assert(count_config_entries_match(_repo, "remote\\.test\\.+") > 0);

	cl_git_pass(git_remote_delete(_repo, "test"));

	cl_assert_equal_i(0, count_config_entries_match(_repo, "remote\\.test\\.+"));
}

void test_network_remote_delete__remove_branch_upstream_configuration_settings(void)
{
	assert_config_entry_existence(_repo, "branch.mergeless.remote", true);
	assert_config_entry_existence(_repo, "branch.master.remote", true);

	cl_git_pass(git_remote_delete(_repo, "test"));

	assert_config_entry_existence(_repo, "branch.mergeless.remote", false);
	assert_config_entry_existence(_repo, "branch.mergeless.merge", false);
	assert_config_entry_existence(_repo, "branch.master.remote", false);
	assert_config_entry_existence(_repo, "branch.master.merge", false);
}
