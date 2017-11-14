#include "clar_libgit2.h"
#include "netops.h"

void test_network_matchhost__match(void)
{
	cl_git_pass(gitno__match_host("*.example.org", "www.example.org"));
	cl_git_pass(gitno__match_host("*.foo.example.org", "www.foo.example.org"));
	cl_git_fail(gitno__match_host("*.foo.example.org", "foo.example.org"));
	cl_git_fail(gitno__match_host("*.foo.example.org", "www.example.org"));
	cl_git_fail(gitno__match_host("*.example.org", "example.org"));
	cl_git_fail(gitno__match_host("*.example.org", "www.foo.example.org"));
	cl_git_fail(gitno__match_host("*.example.org", "blah.www.www.example.org"));
}
