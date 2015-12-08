#include "clar_libgit2.h"

void test_repo_getters__is_empty_correctly_deals_with_pristine_looking_repos(void)
{
	git_repository *repo;

	repo = cl_git_sandbox_init("empty_bare.git");
	cl_git_remove_placeholders(git_repository_path(repo), "dummy-marker.txt");

	cl_assert_equal_i(true, git_repository_is_empty(repo));

	cl_git_sandbox_cleanup();
}

void test_repo_getters__is_empty_can_detect_used_repositories(void)
{
	git_repository *repo;

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));

	cl_assert_equal_i(false, git_repository_is_empty(repo));

	git_repository_free(repo);
}

void test_repo_getters__retrieving_the_odb_honors_the_refcount(void)
{
	git_odb *odb;
	git_repository *repo;

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));

	cl_git_pass(git_repository_odb(&odb, repo));
	cl_assert(((git_refcount *)odb)->refcount.val == 2);

	git_repository_free(repo);
	cl_assert(((git_refcount *)odb)->refcount.val == 1);

	git_odb_free(odb);
}
