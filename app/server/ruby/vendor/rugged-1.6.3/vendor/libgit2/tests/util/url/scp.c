#include "clar_libgit2.h"
#include "net.h"

static git_net_url conndata;

void test_url_scp__initialize(void)
{
	memset(&conndata, 0, sizeof(conndata));
}

void test_url_scp__cleanup(void)
{
	git_net_url_dispose(&conndata);
}

/* Hostname */

void test_url_scp__hostname_trivial(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "example.com:/resource"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__hostname_bracketed(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[example.com]:/resource"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__hostname_root(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "example.com:/"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__hostname_user(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "git@example.com:/resource"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "git");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__hostname_user_bracketed(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[git@example.com]:/resource"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "git");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__hostname_port(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[example.com:42]:/resource"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "42");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_scp__hostname_user_port(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[git@example.com:42]:/resource"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "42");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "git");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_scp__ipv4_trivial(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "192.168.99.88:/resource/a/b/c"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "192.168.99.88");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "/resource/a/b/c");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__ipv4_bracketed(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[192.168.99.88]:/resource/a/b/c"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "192.168.99.88");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "/resource/a/b/c");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__ipv4_user(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "git@192.168.99.88:/resource/a/b/c"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "192.168.99.88");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "/resource/a/b/c");
	cl_assert_equal_s(conndata.username, "git");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__ipv4_port(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[192.168.99.88:1111]:/resource/a/b/c"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "192.168.99.88");
	cl_assert_equal_s(conndata.port, "1111");
	cl_assert_equal_s(conndata.path, "/resource/a/b/c");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_scp__ipv4_user_port(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[git@192.168.99.88:1111]:/resource/a/b/c"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "192.168.99.88");
	cl_assert_equal_s(conndata.port, "1111");
	cl_assert_equal_s(conndata.path, "/resource/a/b/c");
	cl_assert_equal_s(conndata.username, "git");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_scp__ipv6_trivial(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[fe80::dcad:beff:fe00:0001]:/resource/foo"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "[fe80::dcad:beff:fe00:0001]");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "/resource/foo");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__ipv6_user(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "git@[fe80::dcad:beff:fe00:0001]:/resource/foo"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "[fe80::dcad:beff:fe00:0001]");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "/resource/foo");
	cl_assert_equal_s(conndata.username, "git");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__ipv6_port(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[[fe80::dcad:beff:fe00:0001]:99]:/resource/foo"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "[fe80::dcad:beff:fe00:0001]");
	cl_assert_equal_s(conndata.port, "99");
	cl_assert_equal_s(conndata.path, "/resource/foo");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_scp__ipv6_user_port(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[git@[fe80::dcad:beff:fe00:0001]:99]:/resource/foo"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "[fe80::dcad:beff:fe00:0001]");
	cl_assert_equal_s(conndata.port, "99");
	cl_assert_equal_s(conndata.path, "/resource/foo");
	cl_assert_equal_s(conndata.username, "git");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_scp__hexhost_and_port(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[fe:22]:/resource/foo"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "fe");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "/resource/foo");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__malformed_ipv6_one(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "fe80::dcad:beff:fe00:0001]:/resource"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "fe80");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, ":dcad:beff:fe00:0001]:/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__malformed_ipv6_two(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "[fe80::dcad:beff:fe00:0001]:42]:/resource"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "[fe80::dcad:beff:fe00:0001]");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "42]:/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__malformed_ipv6_with_user(void)
{
	cl_git_pass(git_net_url_parse_scp(&conndata, "git@[fe80::dcad:beff:fe00:0001]:42]:/resource"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "[fe80::dcad:beff:fe00:0001]");
	cl_assert_equal_s(conndata.port, "22");
	cl_assert_equal_s(conndata.path, "42]:/resource");
	cl_assert_equal_s(conndata.username, "git");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_scp__invalid_addresses(void)
{
	/* Path is required */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"example.com"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"example.com:"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[example.com:42]:"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[git@example.com:42]:"));

	/* Host is required */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		":"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		":foo"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"git@:foo"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[]:"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"git@[]:"));

	/* User is required if specified */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"@example.com:foo"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"@:foo"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[@localhost:22]:foo"));

	/* Port is required in brackets */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[example.com:]:foo"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[git@example.com:]:foo"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[fe:]:foo"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[@localhost]:foo"));

	/* Extra brackets are disallowed */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[git@[[fe80::dcad:beff:fe00:0001]]:42]:foo"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[[git@[fe80::dcad:beff:fe00:0001]]:42]:foo"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[[git@[fe80::dcad:beff:fe00:0001]:42]]:foo"));

	/* Closing bracket missing */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[fe80::dcad:beff:fe00:0001:/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[[fe80::dcad:beff:fe00:0001]:42:/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[git@[fe80::dcad:beff:fe00:0001]:42:/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_scp(&conndata,
		"[git@[fe80::dcad:beff:fe00:0001:42]:/resource"));
}
