#include "clar_libgit2.h"
#include "net.h"

static git_net_url conndata;

void test_network_url_parse__initialize(void)
{
	memset(&conndata, 0, sizeof(conndata));
}

void test_network_url_parse__cleanup(void)
{
	git_net_url_dispose(&conndata);
}

/* Hostname */

void test_network_url_parse__hostname_trivial(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__hostname_root(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com/"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__hostname_implied_root(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__hostname_implied_root_custom_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com:42"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "42");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__hostname_implied_root_empty_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com:"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__hostname_encoded_password(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
				"https://user:pass%2fis%40bad@hostname.com:1234/"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "hostname.com");
	cl_assert_equal_s(conndata.port, "1234");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass/is@bad");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__hostname_user(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
				"https://user@example.com/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__hostname_user_pass(void)
{
	/* user:pass@hostname.tld/resource */
	cl_git_pass(git_net_url_parse(&conndata,
				"https://user:pass@example.com/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__hostname_port(void)
{
	/* hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse(&conndata,
				"https://example.com:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__hostname_empty_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com:/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__hostname_user_port(void)
{
	/* user@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse(&conndata,
				"https://user@example.com:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__hostname_user_pass_port(void)
{
	/* user:pass@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse(&conndata,
				"https://user:pass@example.com:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

/* IPv4 addresses */

void test_network_url_parse__ipv4_trivial(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://192.168.1.1/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv4_root(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://192.168.1.1/"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv4_implied_root(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://192.168.1.1"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv4_implied_root_custom_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://192.168.1.1:42"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "42");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__ipv4_implied_root_empty_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://192.168.1.1:"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv4_encoded_password(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://user:pass%2fis%40bad@192.168.1.1:1234/"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "1234");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass/is@bad");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__ipv4_user(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://user@192.168.1.1/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv4_user_pass(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://user:pass@192.168.1.1/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv4_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://192.168.1.1:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__ipv4_empty_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://192.168.1.1:/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv4_user_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://user@192.168.1.1:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__ipv4_user_pass_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://user:pass@192.168.1.1:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

/* IPv6 addresses */

void test_network_url_parse__ipv6_trivial(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://[fe80::dcad:beff:fe00:0001]/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv6_root(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://[fe80::dcad:beff:fe00:0001]/"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv6_implied_root(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://[fe80::dcad:beff:fe00:0001]"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv6_implied_root_custom_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://[fe80::dcad:beff:fe00:0001]:42"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "42");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__ipv6_implied_root_empty_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://[fe80::dcad:beff:fe00:0001]:"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv6_encoded_password(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://user:pass%2fis%40bad@[fe80::dcad:beff:fe00:0001]:1234/"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "1234");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass/is@bad");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__ipv6_user(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://user@[fe80::dcad:beff:fe00:0001]/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv6_user_pass(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://user:pass@[fe80::dcad:beff:fe00:0001]/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv6_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://[fe80::dcad:beff:fe00:0001]:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__ipv6_empty_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://[fe80::dcad:beff:fe00:0001]:/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_network_url_parse__ipv6_user_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://user@[fe80::dcad:beff:fe00:0001]:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__ipv6_user_pass_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://user:pass@[fe80::dcad:beff:fe00:0001]:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_network_url_parse__ipv6_invalid_addresses(void)
{
	/* Opening bracket missing */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001]/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001]/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001]"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001]:42"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001]:"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user:pass%2fis%40bad@fe80::dcad:beff:fe00:0001]:1234/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user@fe80::dcad:beff:fe00:0001]/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user:pass@fe80::dcad:beff:fe00:0001]/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://fe80::dcad:beff:fe00:0001]:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001]:/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user@fe80::dcad:beff:fe00:0001]:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user:pass@fe80::dcad:beff:fe00:0001]:9191/resource"));

	/* Closing bracket missing */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://[fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://[fe80::dcad:beff:fe00:0001/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://[fe80::dcad:beff:fe00:0001"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://[fe80::dcad:beff:fe00:0001:42"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://[fe80::dcad:beff:fe00:0001:"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user:pass%2fis%40bad@[fe80::dcad:beff:fe00:0001:1234/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user@[fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user:pass@[fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://[fe80::dcad:beff:fe00:0001:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://[fe80::dcad:beff:fe00:0001:/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user@[fe80::dcad:beff:fe00:0001:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user:pass@[fe80::dcad:beff:fe00:0001:9191/resource"));
	/* Both brackets missing */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001:42"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001:"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user:pass%2fis%40bad@fe80::dcad:beff:fe00:0001:1234/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user@fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user:pass@fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://fe80::dcad:beff:fe00:0001:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::dcad:beff:fe00:0001:/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user@fe80::dcad:beff:fe00:0001:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"https://user:pass@fe80::dcad:beff:fe00:0001:9191/resource"));

	/* Invalid chracter inside address */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata, "http://[fe8o::dcad:beff:fe00:0001]/resource"));
}
