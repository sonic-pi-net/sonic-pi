#include "clar_libgit2.h"
#include "config/config_helpers.h"
#include "refs.h"

static git_repository *repo;
static git_reference *branch, *upstream;

void test_refs_branches_upstream__initialize(void)
{
	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));

	branch = NULL;
	upstream = NULL;
}

void test_refs_branches_upstream__cleanup(void)
{
	git_reference_free(upstream);
	git_reference_free(branch);
	branch = NULL;

	git_repository_free(repo);
	repo = NULL;
}

void test_refs_branches_upstream__can_retrieve_the_remote_tracking_reference_of_a_local_branch(void)
{
	cl_git_pass(git_reference_lookup(&branch, repo, "refs/heads/master"));

	cl_git_pass(git_branch_upstream(&upstream, branch));

	cl_assert_equal_s("refs/remotes/test/master", git_reference_name(upstream));
}

void test_refs_branches_upstream__can_retrieve_the_local_upstream_reference_of_a_local_branch(void)
{
	cl_git_pass(git_reference_lookup(&branch, repo, "refs/heads/track-local"));

	cl_git_pass(git_branch_upstream(&upstream, branch));

	cl_assert_equal_s("refs/heads/master", git_reference_name(upstream));
}

void test_refs_branches_upstream__cannot_retrieve_a_remote_upstream_reference_from_a_non_branch(void)
{
	cl_git_pass(git_reference_lookup(&branch, repo, "refs/tags/e90810b"));

	cl_git_fail(git_branch_upstream(&upstream, branch));
}

void test_refs_branches_upstream__trying_to_retrieve_a_remote_tracking_reference_from_a_plain_local_branch_returns_GIT_ENOTFOUND(void)
{
	cl_git_pass(git_reference_lookup(&branch, repo, "refs/heads/subtrees"));

	cl_assert_equal_i(GIT_ENOTFOUND, git_branch_upstream(&upstream, branch));
}

void test_refs_branches_upstream__trying_to_retrieve_a_remote_tracking_reference_from_a_branch_with_no_fetchspec_returns_GIT_ENOTFOUND(void)
{
	cl_git_pass(git_reference_lookup(&branch, repo, "refs/heads/cannot-fetch"));

	cl_assert_equal_i(GIT_ENOTFOUND, git_branch_upstream(&upstream, branch));
}

void test_refs_branches_upstream__upstream_remote(void)
{
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_branch_upstream_remote(&buf, repo, "refs/heads/master"));
	cl_assert_equal_s("test", buf.ptr);
	git_buf_free(&buf);
}

void test_refs_branches_upstream__upstream_remote_empty_value(void)
{
	git_repository *repository;
	git_config *cfg;
	git_buf buf = GIT_BUF_INIT;

	repository = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_repository_config(&cfg, repository));
	cl_git_pass(git_config_set_string(cfg, "branch.master.remote", ""));
	cl_git_fail_with(GIT_ENOTFOUND, git_branch_upstream_remote(&buf, repository, "refs/heads/master"));

	cl_git_pass(git_config_delete_entry(cfg, "branch.master.remote"));
	cl_git_fail_with(GIT_ENOTFOUND, git_branch_upstream_remote(&buf, repository, "refs/heads/master"));
	cl_git_sandbox_cleanup();
}

static void assert_merge_and_or_remote_key_missing(git_repository *repository, const git_commit *target, const char *entry_name)
{
	git_reference *branch;

	cl_assert_equal_i(GIT_OBJ_COMMIT, git_object_type((git_object*)target));
	cl_git_pass(git_branch_create(&branch, repository, entry_name, (git_commit*)target, 0));

	cl_assert_equal_i(GIT_ENOTFOUND, git_branch_upstream(&upstream, branch));

	git_reference_free(branch);
}

void test_refs_branches_upstream__retrieve_a_remote_tracking_reference_from_a_branch_with_no_remote_returns_GIT_ENOTFOUND(void)
{
	git_reference *head;
	git_repository *repository;
	git_commit *target;

	repository = cl_git_sandbox_init("testrepo.git");

	cl_git_pass(git_repository_head(&head, repository));
	cl_git_pass(git_reference_peel(((git_object **)&target), head, GIT_OBJ_COMMIT));
	git_reference_free(head);

	assert_merge_and_or_remote_key_missing(repository, target, "remoteless");
	assert_merge_and_or_remote_key_missing(repository, target, "mergeless");
	assert_merge_and_or_remote_key_missing(repository, target, "mergeandremoteless");

	git_commit_free(target);

	cl_git_sandbox_cleanup();
}

void test_refs_branches_upstream__set_unset_upstream(void)
{
	git_reference *branch;
	git_repository *repository;

	repository = cl_git_sandbox_init("testrepo.git");

	/* remote */
	cl_git_pass(git_reference_lookup(&branch, repository, "refs/heads/test"));
	cl_git_pass(git_branch_set_upstream(branch, "test/master"));

	assert_config_entry_value(repository, "branch.test.remote", "test");
	assert_config_entry_value(repository, "branch.test.merge", "refs/heads/master");

	git_reference_free(branch);

	/* local */
	cl_git_pass(git_reference_lookup(&branch, repository, "refs/heads/test"));
	cl_git_pass(git_branch_set_upstream(branch, "master"));

	assert_config_entry_value(repository, "branch.test.remote", ".");
	assert_config_entry_value(repository, "branch.test.merge", "refs/heads/master");

	/* unset */
	cl_git_pass(git_branch_set_upstream(branch, NULL));
	assert_config_entry_existence(repository, "branch.test.remote", false);
	assert_config_entry_existence(repository, "branch.test.merge", false);

	git_reference_free(branch);

	cl_git_pass(git_reference_lookup(&branch, repository, "refs/heads/master"));
	cl_git_pass(git_branch_set_upstream(branch, NULL));
	assert_config_entry_existence(repository, "branch.test.remote", false);
	assert_config_entry_existence(repository, "branch.test.merge", false);

	git_reference_free(branch);

	cl_git_sandbox_cleanup();
}

void test_refs_branches_upstream__no_fetch_refspec(void)
{
	git_reference *ref, *branch;
	git_repository *repo;
	git_remote *remote;
	git_config *cfg;

	repo = cl_git_sandbox_init("testrepo.git");

	cl_git_pass(git_remote_create_with_fetchspec(&remote, repo, "matching", ".", NULL));
	cl_git_pass(git_remote_add_push(repo, "matching", ":"));

	cl_git_pass(git_reference_lookup(&branch, repo, "refs/heads/test"));
	cl_git_pass(git_reference_create(&ref, repo, "refs/remotes/matching/master", git_reference_target(branch), 1, "fetch"));
	cl_git_fail(git_branch_set_upstream(branch, "matching/master"));
	cl_assert_equal_s("Could not determine remote for 'refs/remotes/matching/master'",
			  giterr_last()->message);

	/* we can't set it automatically, so let's test the user setting it by hand */
	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_set_string(cfg, "branch.test.remote", "matching"));
	cl_git_pass(git_config_set_string(cfg, "branch.test.merge", "refs/heads/master"));
	/* we still can't find it because there is no rule for that reference */
	cl_git_fail_with(GIT_ENOTFOUND, git_branch_upstream(&ref, branch));

	git_reference_free(ref);
	git_reference_free(branch);
	git_remote_free(remote);

	cl_git_sandbox_cleanup();
}
