#include "clar_libgit2.h"
#include "repository.h"
#include "worktree.h"
#include "worktree_helpers.h"

#define COMMON_REPO "testrepo"
#define WORKTREE_REPO "testrepo-worktree"

static worktree_fixture fixture =
	WORKTREE_FIXTURE_INIT(COMMON_REPO, WORKTREE_REPO);

static void assert_worktree_valid(git_repository *wt, const char *parentdir, const char *wtdir)
{
	cl_assert(wt->is_worktree);

	cl_assert_equal_s(wt->workdir, cl_git_sandbox_path(1, wtdir, NULL));
	cl_assert_equal_s(wt->gitlink, cl_git_sandbox_path(0, wtdir, ".git", NULL));
	cl_assert_equal_s(wt->gitdir, cl_git_sandbox_path(1, parentdir, ".git", "worktrees", wtdir, NULL));
}

void test_worktree_open__initialize(void)
{
	setup_fixture_worktree(&fixture);
}

void test_worktree_open__cleanup(void)
{
	cleanup_fixture_worktree(&fixture);
}

void test_worktree_open__repository(void)
{
	assert_worktree_valid(fixture.worktree, COMMON_REPO, WORKTREE_REPO);
}

void test_worktree_open__repository_through_workdir(void)
{
	git_repository *wt;

	cl_git_pass(git_repository_open(&wt, WORKTREE_REPO));
	assert_worktree_valid(wt, COMMON_REPO, WORKTREE_REPO);

	git_repository_free(wt);
}

void test_worktree_open__repository_through_gitlink(void)
{
	git_repository *wt;

	cl_git_pass(git_repository_open(&wt, WORKTREE_REPO "/.git"));
	assert_worktree_valid(wt, COMMON_REPO, WORKTREE_REPO);

	git_repository_free(wt);
}

void test_worktree_open__repository_through_gitdir(void)
{
	git_str gitdir_path = GIT_STR_INIT;
	git_repository *wt;

	cl_git_pass(git_str_joinpath(&gitdir_path, COMMON_REPO, ".git"));
	cl_git_pass(git_str_joinpath(&gitdir_path, gitdir_path.ptr, "worktrees"));
	cl_git_pass(git_str_joinpath(&gitdir_path, gitdir_path.ptr, "testrepo-worktree"));

	cl_git_pass(git_repository_open(&wt, gitdir_path.ptr));
	assert_worktree_valid(wt, COMMON_REPO, WORKTREE_REPO);

	git_str_dispose(&gitdir_path);
	git_repository_free(wt);
}

void test_worktree_open__open_discovered_worktree(void)
{
	git_buf path = GIT_BUF_INIT;
	git_repository *repo;

	cl_git_pass(git_repository_discover(&path,
		git_repository_workdir(fixture.worktree), false, NULL));
	cl_git_pass(git_repository_open(&repo, path.ptr));
	cl_assert_equal_s(git_repository_workdir(fixture.worktree),
		git_repository_workdir(repo));

	git_buf_dispose(&path);
	git_repository_free(repo);
}

void test_worktree_open__repository_with_nonexistent_parent(void)
{
	git_repository *repo;

	cleanup_fixture_worktree(&fixture);

	cl_fixture_sandbox(WORKTREE_REPO);
	cl_git_pass(p_chdir(WORKTREE_REPO));
	cl_git_pass(cl_rename(".gitted", ".git"));
	cl_git_pass(p_chdir(".."));

	cl_git_fail(git_repository_open(&repo, WORKTREE_REPO));

	cl_fixture_cleanup(WORKTREE_REPO);
}

void test_worktree_open__open_from_repository(void)
{
	git_worktree *opened, *lookedup;

	cl_git_pass(git_worktree_open_from_repository(&opened, fixture.worktree));
	cl_git_pass(git_worktree_lookup(&lookedup, fixture.repo, WORKTREE_REPO));

	cl_assert_equal_s(opened->name, lookedup->name);
	cl_assert_equal_s(opened->gitdir_path, lookedup->gitdir_path);
	cl_assert_equal_s(opened->gitlink_path, lookedup->gitlink_path);
	cl_assert_equal_s(opened->parent_path, lookedup->parent_path);
	cl_assert_equal_s(opened->commondir_path, lookedup->commondir_path);
	cl_assert_equal_i(opened->locked, lookedup->locked);

	git_worktree_free(opened);
	git_worktree_free(lookedup);
}

void test_worktree_open__open_from_nonworktree_fails(void)
{
	git_worktree *wt;

	cl_git_fail(git_worktree_open_from_repository(&wt, fixture.repo));
}
