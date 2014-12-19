#include "clar_libgit2.h"
#include "posix.h"
#include "reset_helpers.h"
#include "path.h"

static git_repository *repo;
static git_object *target;

void test_reset_mixed__initialize(void)
{
	repo = cl_git_sandbox_init("attr");
	target = NULL;
}

void test_reset_mixed__cleanup(void)
{
	git_object_free(target);
	target = NULL;

	cl_git_sandbox_cleanup();
}

void test_reset_mixed__cannot_reset_in_a_bare_repository(void)
{
	git_repository *bare;

	cl_git_pass(git_repository_open(&bare, cl_fixture("testrepo.git")));
	cl_assert(git_repository_is_bare(bare) == true);

	cl_git_pass(git_revparse_single(&target, bare, KNOWN_COMMIT_IN_BARE_REPO));

	cl_assert_equal_i(GIT_EBAREREPO, git_reset(bare, target, GIT_RESET_MIXED, NULL, NULL, NULL));

	git_repository_free(bare);
}

void test_reset_mixed__resetting_refreshes_the_index_to_the_commit_tree(void)
{
	unsigned int status;

	cl_git_pass(git_status_file(&status, repo, "macro_bad"));
	cl_assert(status == GIT_STATUS_CURRENT);
	cl_git_pass(git_revparse_single(&target, repo, "605812a"));

	cl_git_pass(git_reset(repo, target, GIT_RESET_MIXED, NULL, NULL, NULL));

	cl_git_pass(git_status_file(&status, repo, "macro_bad"));
	cl_assert(status == GIT_STATUS_WT_NEW);
}

void test_reset_mixed__reflog_is_correct(void)
{
	const char *exp_msg = "commit: Updating test data so we can test inter-hunk-context";

	reflog_check(repo, "HEAD", 9, "yoram.harmelin@gmail.com", exp_msg);
	reflog_check(repo, "refs/heads/master", 9, "yoram.harmelin@gmail.com", exp_msg);

	/* Branch not moving, no reflog entry */
	cl_git_pass(git_revparse_single(&target, repo, "HEAD^{commit}"));
	cl_git_pass(git_reset(repo, target, GIT_RESET_MIXED, NULL, NULL, NULL));
	reflog_check(repo, "HEAD", 9, "yoram.harmelin@gmail.com", exp_msg);
	reflog_check(repo, "refs/heads/master", 9, "yoram.harmelin@gmail.com", exp_msg);

	git_object_free(target);
	target = NULL;

	/* Moved branch, expect default message */
	exp_msg = "reset: moving";
	cl_git_pass(git_revparse_single(&target, repo, "HEAD~^{commit}"));
	cl_git_pass(git_reset(repo, target, GIT_RESET_MIXED, NULL, NULL, NULL));
	reflog_check(repo, "HEAD", 10, NULL, exp_msg);
	reflog_check(repo, "refs/heads/master", 10, NULL, exp_msg);

	git_object_free(target);
	target = NULL;

	/* Moved branch, expect custom message */
	exp_msg = "message1";
	cl_git_pass(git_revparse_single(&target, repo, "HEAD~^{commit}"));
	cl_git_pass(git_reset(repo, target, GIT_RESET_MIXED, NULL, NULL, "message1"));
	reflog_check(repo, "HEAD", 11, NULL, exp_msg);
	reflog_check(repo, "refs/heads/master", 11, NULL, exp_msg);
}
