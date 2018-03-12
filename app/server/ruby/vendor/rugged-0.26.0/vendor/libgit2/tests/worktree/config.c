#include "clar_libgit2.h"
#include "worktree_helpers.h"

#define COMMON_REPO "testrepo"
#define WORKTREE_REPO "testrepo-worktree"

static worktree_fixture fixture =
	WORKTREE_FIXTURE_INIT(COMMON_REPO, WORKTREE_REPO);

void test_worktree_config__initialize(void)
{
	setup_fixture_worktree(&fixture);
}

void test_worktree_config__cleanup(void)
{
	cleanup_fixture_worktree(&fixture);
}

void test_worktree_config__open(void)
{
	git_config *cfg;

	cl_git_pass(git_repository_config(&cfg, fixture.worktree));
	cl_assert(cfg != NULL);

	git_config_free(cfg);
}

void test_worktree_config__set(void)
{
	git_config *cfg;
	int32_t val;

	cl_git_pass(git_repository_config(&cfg, fixture.worktree));
	cl_git_pass(git_config_set_int32(cfg, "core.dummy", 5));
	git_config_free(cfg);

	// reopen to verify configuration has been set in the
	// common dir
	cl_git_pass(git_repository_config(&cfg, fixture.repo));
	cl_git_pass(git_config_get_int32(&val, cfg, "core.dummy"));
	cl_assert_equal_i(val, 5);
	git_config_free(cfg);
}
