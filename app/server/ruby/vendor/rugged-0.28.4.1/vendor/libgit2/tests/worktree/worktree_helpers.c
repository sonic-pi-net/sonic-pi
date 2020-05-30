#include "clar_libgit2.h"
#include "worktree_helpers.h"

void cleanup_fixture_worktree(worktree_fixture *fixture)
{
	if (!fixture)
		return;

	if (fixture->repo) {
		git_repository_free(fixture->repo);
		fixture->repo = NULL;
	}
	if (fixture->worktree) {
		git_repository_free(fixture->worktree);
		fixture->worktree = NULL;
	}

	if (fixture->reponame)
		cl_fixture_cleanup(fixture->reponame);
	if (fixture->worktreename)
		cl_fixture_cleanup(fixture->worktreename);
}

void setup_fixture_worktree(worktree_fixture *fixture)
{
	if (fixture->reponame)
		fixture->repo = cl_git_sandbox_init(fixture->reponame);
	if (fixture->worktreename)
		fixture->worktree = cl_git_sandbox_init(fixture->worktreename);
}
