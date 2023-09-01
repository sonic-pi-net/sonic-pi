#include "clar_libgit2.h"
#include "net.h"

void test_url_valid__test(void)
{
	cl_assert(git_net_str_is_url("http://example.com/"));
	cl_assert(git_net_str_is_url("file://localhost/tmp/foo/"));
	cl_assert(git_net_str_is_url("ssh://user@host:42/tmp"));
	cl_assert(git_net_str_is_url("git+ssh://user@host:42/tmp"));
	cl_assert(git_net_str_is_url("ssh+git://user@host:42/tmp"));
	cl_assert(git_net_str_is_url("https://user:pass@example.com/foo/bar"));

	cl_assert(!git_net_str_is_url("host:foo.git"));
	cl_assert(!git_net_str_is_url("host:/foo.git"));
	cl_assert(!git_net_str_is_url("[host:42]:/foo.git"));
	cl_assert(!git_net_str_is_url("[user@host:42]:/foo.git"));
}
