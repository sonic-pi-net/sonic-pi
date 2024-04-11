#include "clar_libgit2.h"
#include "net.h"

void test_hostname__matches_cert(void)
{
	cl_assert_equal_b(true, git_net_hostname_matches_cert("www.example.org", "*.example.org"));
	cl_assert_equal_b(true, git_net_hostname_matches_cert("www.foo.example.org", "*.foo.example.org"));
	cl_assert_equal_b(false, git_net_hostname_matches_cert("foo.example.org", "*.foo.example.org"));
	cl_assert_equal_b(false, git_net_hostname_matches_cert("www.example.org", "*.foo.example.org"));
	cl_assert_equal_b(false, git_net_hostname_matches_cert("example.org", "*.example.org"));
	cl_assert_equal_b(false, git_net_hostname_matches_cert("www.foo.example.org", "*.example.org"));
	cl_assert_equal_b(false, git_net_hostname_matches_cert("blah.www.www.example.org", "*.example.org"));
}
