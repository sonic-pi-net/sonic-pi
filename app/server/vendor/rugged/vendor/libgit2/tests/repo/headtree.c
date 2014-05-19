#include "clar_libgit2.h"
#include "repository.h"
#include "repo_helpers.h"
#include "posix.h"

static git_repository *repo;
static git_tree *tree;

void test_repo_headtree__initialize(void)
{
	repo = cl_git_sandbox_init("testrepo.git");
	tree = NULL;
}

void test_repo_headtree__cleanup(void)
{
	git_tree_free(tree);
	cl_git_sandbox_cleanup();
}

void test_repo_headtree__can_retrieve_the_root_tree_from_a_detached_head(void)
{
	cl_git_pass(git_repository_detach_head(repo, NULL, NULL));

	cl_git_pass(git_repository_head_tree(&tree, repo));

	cl_assert(git_oid_streq(git_tree_id(tree), "az"));
}

void test_repo_headtree__can_retrieve_the_root_tree_from_a_non_detached_head(void)
{
	cl_assert_equal_i(false, git_repository_head_detached(repo));

	cl_git_pass(git_repository_head_tree(&tree, repo));

	cl_assert(git_oid_streq(git_tree_id(tree), "az"));
}

void test_repo_headtree__when_head_is_unborn_returns_EUNBORNBRANCH(void)
{
	make_head_unborn(repo, NON_EXISTING_HEAD);

	cl_assert_equal_i(true, git_repository_head_unborn(repo));

	cl_assert_equal_i(GIT_EUNBORNBRANCH, git_repository_head_tree(&tree, repo));
}

void test_repo_headtree__when_head_is_missing_returns_ENOTFOUND(void)
{
	delete_head(repo);

	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_head_tree(&tree, repo));
}
