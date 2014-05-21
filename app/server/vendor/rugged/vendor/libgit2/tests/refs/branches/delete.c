#include "clar_libgit2.h"
#include "refs.h"
#include "repo/repo_helpers.h"
#include "config/config_helpers.h"

static git_repository *repo;
static git_reference *fake_remote;

void test_refs_branches_delete__initialize(void)
{
	git_oid id;

	repo = cl_git_sandbox_init("testrepo.git");

	cl_git_pass(git_oid_fromstr(&id, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644"));
	cl_git_pass(git_reference_create(&fake_remote, repo, "refs/remotes/nulltoken/master", &id, 0, NULL, NULL));
}

void test_refs_branches_delete__cleanup(void)
{
	git_reference_free(fake_remote);
	fake_remote = NULL;

	cl_git_sandbox_cleanup();
	repo = NULL;
}

void test_refs_branches_delete__can_not_delete_a_branch_pointed_at_by_HEAD(void)
{
	git_reference *head;
	git_reference *branch;

	/* Ensure HEAD targets the local master branch */
	cl_git_pass(git_reference_lookup(&head, repo, GIT_HEAD_FILE));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));
	git_reference_free(head);

	cl_git_pass(git_branch_lookup(&branch, repo, "master", GIT_BRANCH_LOCAL));
	cl_git_fail(git_branch_delete(branch));
	git_reference_free(branch);
}

void test_refs_branches_delete__can_delete_a_branch_even_if_HEAD_is_missing(void)
{
	git_reference *head;
	git_reference *branch;

	cl_git_pass(git_reference_lookup(&head, repo, GIT_HEAD_FILE));
	git_reference_delete(head);
	git_reference_free(head);

	cl_git_pass(git_branch_lookup(&branch, repo, "br2", GIT_BRANCH_LOCAL));
	cl_git_pass(git_branch_delete(branch));
	git_reference_free(branch);
}

void test_refs_branches_delete__can_delete_a_branch_when_HEAD_is_unborn(void)
{
	git_reference *branch;

	make_head_unborn(repo, NON_EXISTING_HEAD);

	cl_git_pass(git_branch_lookup(&branch, repo, "br2", GIT_BRANCH_LOCAL));
	cl_git_pass(git_branch_delete(branch));
	git_reference_free(branch);
}

void test_refs_branches_delete__can_delete_a_branch_pointed_at_by_detached_HEAD(void)
{
	git_reference *head, *branch;

	cl_git_pass(git_reference_lookup(&head, repo, GIT_HEAD_FILE));
	cl_assert_equal_i(GIT_REF_SYMBOLIC, git_reference_type(head));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));
	git_reference_free(head);

	/* Detach HEAD and make it target the commit that "master" points to */
	git_repository_detach_head(repo, NULL, NULL);

	cl_git_pass(git_branch_lookup(&branch, repo, "master", GIT_BRANCH_LOCAL));
	cl_git_pass(git_branch_delete(branch));
	git_reference_free(branch);
}

void test_refs_branches_delete__can_delete_a_local_branch(void)
{
	git_reference *branch;
	cl_git_pass(git_branch_lookup(&branch, repo, "br2", GIT_BRANCH_LOCAL));
	cl_git_pass(git_branch_delete(branch));
	git_reference_free(branch);
}

void test_refs_branches_delete__can_delete_a_remote_branch(void)
{
	git_reference *branch;
	cl_git_pass(git_branch_lookup(&branch, repo, "nulltoken/master", GIT_BRANCH_REMOTE));
	cl_git_pass(git_branch_delete(branch));
	git_reference_free(branch);
}

void test_refs_branches_delete__deleting_a_branch_removes_related_configuration_data(void)
{
	git_reference *branch;

	assert_config_entry_existence(repo, "branch.track-local.remote", true);
	assert_config_entry_existence(repo, "branch.track-local.merge", true);

	cl_git_pass(git_branch_lookup(&branch, repo, "track-local", GIT_BRANCH_LOCAL));
	cl_git_pass(git_branch_delete(branch));
	git_reference_free(branch);

	assert_config_entry_existence(repo, "branch.track-local.remote", false);
	assert_config_entry_existence(repo, "branch.track-local.merge", false);
}

void test_refs_branches_delete__removes_reflog(void)
{
	git_reference *branch;
	git_reflog *log;
	git_oid oidzero = {{0}};
	git_signature *sig;

	/* Ensure the reflog has at least one entry */
	cl_git_pass(git_signature_now(&sig, "Me", "user@example.com"));
	cl_git_pass(git_reflog_read(&log, repo, "refs/heads/track-local"));
	cl_git_pass(git_reflog_append(log, &oidzero, sig, "message"));
	cl_assert(git_reflog_entrycount(log) > 0);
	git_signature_free(sig);
	git_reflog_free(log);

	cl_git_pass(git_branch_lookup(&branch, repo, "track-local", GIT_BRANCH_LOCAL));
	cl_git_pass(git_branch_delete(branch));
	git_reference_free(branch);

	/* Reading a nonexistant reflog creates it, but it should be empty */
	cl_git_pass(git_reflog_read(&log, repo, "refs/heads/track-local"));
	cl_assert_equal_i(0, git_reflog_entrycount(log));
	git_reflog_free(log);
}

