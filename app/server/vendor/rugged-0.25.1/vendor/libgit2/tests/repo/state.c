#include "clar_libgit2.h"
#include "buffer.h"
#include "refs.h"
#include "posix.h"
#include "fileops.h"

static git_repository *_repo;
static git_buf _path;

void test_repo_state__initialize(void)
{
	_repo = cl_git_sandbox_init("testrepo.git");
}

void test_repo_state__cleanup(void)
{
	cl_git_sandbox_cleanup();
	git_buf_free(&_path);
}

static void setup_simple_state(const char *filename)
{
	cl_git_pass(git_buf_joinpath(&_path, git_repository_path(_repo), filename));
	git_futils_mkpath2file(git_buf_cstr(&_path), 0777);
	cl_git_mkfile(git_buf_cstr(&_path), "dummy");
}

static void assert_repo_state(git_repository_state_t state)
{
	cl_assert_equal_i(state, git_repository_state(_repo));
}

void test_repo_state__none_with_HEAD_attached(void)
{
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__none_with_HEAD_detached(void)
{
	cl_git_pass(git_repository_detach_head(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__merge(void)
{
	setup_simple_state(GIT_MERGE_HEAD_FILE);
	assert_repo_state(GIT_REPOSITORY_STATE_MERGE);
	cl_git_pass(git_repository_state_cleanup(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__revert(void)
{
	setup_simple_state(GIT_REVERT_HEAD_FILE);
	assert_repo_state(GIT_REPOSITORY_STATE_REVERT);
	cl_git_pass(git_repository_state_cleanup(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__revert_sequence(void)
{
	setup_simple_state(GIT_REVERT_HEAD_FILE);
	setup_simple_state(GIT_SEQUENCER_TODO_FILE);
	assert_repo_state(GIT_REPOSITORY_STATE_REVERT_SEQUENCE);
	cl_git_pass(git_repository_state_cleanup(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__cherry_pick(void)
{
	setup_simple_state(GIT_CHERRYPICK_HEAD_FILE);
	assert_repo_state(GIT_REPOSITORY_STATE_CHERRYPICK);
	cl_git_pass(git_repository_state_cleanup(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__cherrypick_sequence(void)
{
	setup_simple_state(GIT_CHERRYPICK_HEAD_FILE);
	setup_simple_state(GIT_SEQUENCER_TODO_FILE);
	assert_repo_state(GIT_REPOSITORY_STATE_CHERRYPICK_SEQUENCE);
	cl_git_pass(git_repository_state_cleanup(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__bisect(void)
{
	setup_simple_state(GIT_BISECT_LOG_FILE);
	assert_repo_state(GIT_REPOSITORY_STATE_BISECT);
	cl_git_pass(git_repository_state_cleanup(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__rebase_interactive(void)
{
	setup_simple_state(GIT_REBASE_MERGE_INTERACTIVE_FILE);
	assert_repo_state(GIT_REPOSITORY_STATE_REBASE_INTERACTIVE);
	cl_git_pass(git_repository_state_cleanup(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__rebase_merge(void)
{
	setup_simple_state(GIT_REBASE_MERGE_DIR "whatever");
	assert_repo_state(GIT_REPOSITORY_STATE_REBASE_MERGE);
	cl_git_pass(git_repository_state_cleanup(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__rebase(void)
{
	setup_simple_state(GIT_REBASE_APPLY_REBASING_FILE);
	assert_repo_state(GIT_REPOSITORY_STATE_REBASE);
	cl_git_pass(git_repository_state_cleanup(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__apply_mailbox(void)
{
	setup_simple_state(GIT_REBASE_APPLY_APPLYING_FILE);
	assert_repo_state(GIT_REPOSITORY_STATE_APPLY_MAILBOX);
	cl_git_pass(git_repository_state_cleanup(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}

void test_repo_state__apply_mailbox_or_rebase(void)
{
	setup_simple_state(GIT_REBASE_APPLY_DIR "whatever");
	assert_repo_state(GIT_REPOSITORY_STATE_APPLY_MAILBOX_OR_REBASE);
	cl_git_pass(git_repository_state_cleanup(_repo));
	assert_repo_state(GIT_REPOSITORY_STATE_NONE);
}
