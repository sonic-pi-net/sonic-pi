#include "clar_libgit2.h"
#include "git2/sys/repository.h"

void test_repo_new__has_nothing(void)
{
	git_repository *repo;

	cl_git_pass(git_repository_new(&repo));
	cl_assert_equal_b(true, git_repository_is_bare(repo));
	cl_assert_equal_p(NULL, git_repository_path(repo));
	cl_assert_equal_p(NULL, git_repository_workdir(repo));
	git_repository_free(repo);
}

void test_repo_new__is_bare_until_workdir_set(void)
{
	git_repository *repo;

	cl_git_pass(git_repository_new(&repo));
	cl_assert_equal_b(true, git_repository_is_bare(repo));

	cl_git_pass(git_repository_set_workdir(repo, clar_sandbox_path(), 0));
	cl_assert_equal_b(false, git_repository_is_bare(repo));

	git_repository_free(repo);
}

