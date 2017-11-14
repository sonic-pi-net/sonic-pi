#include "clar_libgit2.h"
#include "stash_helpers.h"
#include "../submodule/submodule_helpers.h"

static git_repository *repo;
static git_signature *signature;
static git_oid stash_tip_oid;

static git_submodule *sm;

void test_stash_submodules__initialize(void)
{
	cl_git_pass(git_signature_new(&signature, "nulltoken", "emeric.fermas@gmail.com", 1323847743, 60)); /* Wed Dec 14 08:29:03 2011 +0100 */

	repo = setup_fixture_submodules();

	cl_git_pass(git_submodule_lookup(&sm, repo, "testrepo"));
}

void test_stash_submodules__cleanup(void)
{
	git_submodule_free(sm);
	sm = NULL;

	git_signature_free(signature);
	signature = NULL;
}

void test_stash_submodules__does_not_stash_modified_submodules(void)
{
	static git_index *smindex;
	static git_repository *smrepo;

	assert_status(repo, "modified", GIT_STATUS_WT_MODIFIED);

	/* modify file in submodule */
	cl_git_rewritefile("submodules/testrepo/README", "heyheyhey");
	assert_status(repo, "testrepo", GIT_STATUS_WT_MODIFIED);

	/* add file to index in submodule */
	cl_git_pass(git_submodule_open(&smrepo, sm));
	cl_git_pass(git_repository_index(&smindex, smrepo));
	cl_git_pass(git_index_add_bypath(smindex, "README"));

	/* commit changed index of submodule */
	cl_repo_commit_from_index(NULL, smrepo, NULL, 1372350000, "Modify it");
	assert_status(repo, "testrepo", GIT_STATUS_WT_MODIFIED);

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));

	assert_status(repo, "testrepo", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "modified", GIT_STATUS_CURRENT);

	git_index_free(smindex);
	git_repository_free(smrepo);
}

void test_stash_submodules__stash_is_empty_with_modified_submodules(void)
{
	static git_index *smindex;
	static git_repository *smrepo;

	cl_git_pass(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT));
	assert_status(repo, "modified", GIT_STATUS_CURRENT);

	/* modify file in submodule */
	cl_git_rewritefile("submodules/testrepo/README", "heyheyhey");
	assert_status(repo, "testrepo", GIT_STATUS_WT_MODIFIED);

	/* add file to index in submodule */
	cl_git_pass(git_submodule_open(&smrepo, sm));
	cl_git_pass(git_repository_index(&smindex, smrepo));
	cl_git_pass(git_index_add_bypath(smindex, "README"));

	/* commit changed index of submodule */
	cl_repo_commit_from_index(NULL, smrepo, NULL, 1372350000, "Modify it");
	assert_status(repo, "testrepo", GIT_STATUS_WT_MODIFIED);

	cl_git_fail_with(git_stash_save(&stash_tip_oid, repo, signature, NULL, GIT_STASH_DEFAULT), GIT_ENOTFOUND);

	git_index_free(smindex);
	git_repository_free(smrepo);
}
