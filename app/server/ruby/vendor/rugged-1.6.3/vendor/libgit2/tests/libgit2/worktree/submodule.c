#include "clar_libgit2.h"
#include "repository.h"
#include "worktree.h"
#include "worktree_helpers.h"

#define WORKTREE_PARENT "submodules-worktree-parent"
#define WORKTREE_CHILD "submodules-worktree-child"

static worktree_fixture parent
    = WORKTREE_FIXTURE_INIT("submodules", WORKTREE_PARENT);
static worktree_fixture child
    = WORKTREE_FIXTURE_INIT(NULL, WORKTREE_CHILD);

void test_worktree_submodule__initialize(void)
{
	setup_fixture_worktree(&parent);

	cl_git_pass(p_rename(
		"submodules/testrepo/.gitted",
		"submodules/testrepo/.git"));

	setup_fixture_worktree(&child);
}

void test_worktree_submodule__cleanup(void)
{
	cleanup_fixture_worktree(&child);
	cleanup_fixture_worktree(&parent);
}

void test_worktree_submodule__submodule_worktree_parent(void)
{
	cl_assert(git_repository_path(parent.worktree) != NULL);
	cl_assert(git_repository_workdir(parent.worktree) != NULL);

	cl_assert(!parent.repo->is_worktree);
	cl_assert(parent.worktree->is_worktree);
}

void test_worktree_submodule__submodule_worktree_child(void)
{
	cl_assert(!parent.repo->is_worktree);
	cl_assert(parent.worktree->is_worktree);
	cl_assert(child.worktree->is_worktree);
}

void test_worktree_submodule__open_discovered_submodule_worktree(void)
{
	git_buf path = GIT_BUF_INIT;
	git_repository *repo;

	cl_git_pass(git_repository_discover(&path,
		git_repository_workdir(child.worktree), false, NULL));
	cl_git_pass(git_repository_open(&repo, path.ptr));
	cl_assert_equal_s(git_repository_workdir(child.worktree),
		git_repository_workdir(repo));

	git_buf_dispose(&path);
	git_repository_free(repo);
}

void test_worktree_submodule__resolve_relative_url(void)
{
	git_str wt_path = GIT_STR_INIT;
	git_buf sm_relative_path = GIT_BUF_INIT, wt_relative_path = GIT_BUF_INIT;
	git_repository *repo;
	git_worktree *wt;

	cl_git_pass(git_futils_mkdir("subdir", 0755, GIT_MKDIR_PATH));
	cl_git_pass(git_fs_path_prettify_dir(&wt_path, "subdir", NULL));
	cl_git_pass(git_str_joinpath(&wt_path, wt_path.ptr, "wt"));

	/* Open child repository, which is a submodule */
	cl_git_pass(git_repository_open(&child.repo, WORKTREE_CHILD));

	/* Create worktree of submodule repository */
	cl_git_pass(git_worktree_add(&wt, child.repo, "subdir", wt_path.ptr, NULL));
	cl_git_pass(git_repository_open_from_worktree(&repo, wt));

	cl_git_pass(git_submodule_resolve_url(&sm_relative_path, repo,
		    "../" WORKTREE_CHILD));
	cl_git_pass(git_submodule_resolve_url(&wt_relative_path, child.repo,
		    "../" WORKTREE_CHILD));

	cl_assert_equal_s(sm_relative_path.ptr, wt_relative_path.ptr);

	git_worktree_free(wt);
	git_repository_free(repo);
	git_str_dispose(&wt_path);
	git_buf_dispose(&sm_relative_path);
	git_buf_dispose(&wt_relative_path);
}
