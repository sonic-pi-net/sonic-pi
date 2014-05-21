#include "clar_libgit2.h"
#include "refs.h"
#include "repo_helpers.h"
#include "posix.h"

void make_head_unborn(git_repository* repo, const char *target)
{
	git_reference *head;

	cl_git_pass(git_reference_symbolic_create(&head, repo, GIT_HEAD_FILE, target, 1, NULL, NULL));
	git_reference_free(head);
}

void delete_head(git_repository* repo)
{
	git_buf head_path = GIT_BUF_INIT;

	cl_git_pass(git_buf_joinpath(&head_path, git_repository_path(repo), GIT_HEAD_FILE));
	cl_git_pass(p_unlink(git_buf_cstr(&head_path)));

	git_buf_free(&head_path);
}
