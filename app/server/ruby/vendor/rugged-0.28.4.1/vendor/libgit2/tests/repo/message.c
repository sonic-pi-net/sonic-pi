#include "clar_libgit2.h"
#include "buffer.h"
#include "refs.h"
#include "posix.h"

static git_repository *_repo;

void test_repo_message__initialize(void)
{
	_repo = cl_git_sandbox_init("testrepo.git");
}

void test_repo_message__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_repo_message__none(void)
{
	git_buf actual = GIT_BUF_INIT;
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_message(&actual, _repo));
}

void test_repo_message__message(void)
{
	git_buf path = GIT_BUF_INIT, actual = GIT_BUF_INIT;
	const char expected[] = "Test\n\nThis is a test of the emergency broadcast system\n";

	cl_git_pass(git_buf_joinpath(&path, git_repository_path(_repo), "MERGE_MSG"));
	cl_git_mkfile(git_buf_cstr(&path), expected);

	cl_git_pass(git_repository_message(&actual, _repo));
	cl_assert_equal_s(expected, git_buf_cstr(&actual));
	git_buf_dispose(&actual);

	cl_git_pass(p_unlink(git_buf_cstr(&path)));
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_message(&actual, _repo));
	git_buf_dispose(&path);
}
