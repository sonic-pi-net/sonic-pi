#include "clar_libgit2.h"

void test_config_find__one(void)
{
	git_buf buf = GIT_BUF_INIT;

	cl_git_fail_with(GIT_ENOTFOUND, git_config_find_global(&buf));
	cl_git_fail_with(GIT_ENOTFOUND, git_config_find_xdg(&buf));
	cl_git_fail_with(GIT_ENOTFOUND, git_config_find_system(&buf));
	cl_git_fail_with(GIT_ENOTFOUND, git_config_find_programdata(&buf));
}
