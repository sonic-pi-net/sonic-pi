#include "clar_libgit2.h"
#include "global.h"

void test_core_useragent__get(void)
{
	const char *custom_name = "super duper git";
	git_buf buf = GIT_BUF_INIT;

	cl_assert_equal_p(NULL, git_libgit2__user_agent());
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_USER_AGENT, custom_name));
	cl_assert_equal_s(custom_name, git_libgit2__user_agent());

	cl_git_pass(git_libgit2_opts(GIT_OPT_GET_USER_AGENT, &buf));
	cl_assert_equal_s(custom_name, buf.ptr);

	git_buf_free(&buf);
}
