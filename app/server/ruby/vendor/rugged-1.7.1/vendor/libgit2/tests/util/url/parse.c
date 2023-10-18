#include "clar_libgit2.h"
#include "net.h"

static git_net_url conndata;

void test_url_parse__initialize(void)
{
	memset(&conndata, 0, sizeof(conndata));
}

void test_url_parse__cleanup(void)
{
	git_net_url_dispose(&conndata);
}

/* Hostname */

void test_url_parse__hostname_trivial(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_parse__hostname_root(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com/"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_parse__hostname_implied_root(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_parse__hostname_numeric(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://8888888/"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "8888888");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_parse__hostname_implied_root_custom_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com:42"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "42");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__hostname_implied_root_empty_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com:"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_parse__hostname_encoded_password(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
				"https://user:pass%2fis%40bad@hostname.com:1234/"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "hostname.com");
	cl_assert_equal_s(conndata.port, "1234");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass/is@bad");
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__hostname_user(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
				"https://user@example.com/resource"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_parse__hostname_user_pass(void)
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
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_parse__hostname_port(void)
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
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__hostname_empty_port(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://example.com:/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_parse__hostname_user_port(void)
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
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__hostname_user_pass_port(void)
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
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__hostname_user_pass_port_query(void)
{
	/* user:pass@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse(&conndata,
				"https://user:pass@example.com:9191/resource?query=q&foo=bar&z=asdf"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_s(conndata.query, "query=q&foo=bar&z=asdf");
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__hostname_user_pass_port_fragment(void)
{
	/* user:pass@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse(&conndata,
				"https://user:pass@example.com:9191/resource#fragment"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_s(conndata.fragment, "fragment");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__hostname_user_pass_port_query_fragment(void)
{
	/* user:pass@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse(&conndata,
				"https://user:pass@example.com:9191/resource?query=q&foo=bar&z=asdf#fragment"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_s(conndata.query, "query=q&foo=bar&z=asdf");
	cl_assert_equal_s(conndata.fragment, "fragment");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__fragment_with_question_mark(void)
{
	/* user:pass@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse(&conndata,
				"https://user:pass@example.com:9191/resource#fragment_with?question_mark"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_s(conndata.fragment, "fragment_with?question_mark");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

/* IPv4 addresses */

void test_url_parse__ipv4_trivial(void)
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

void test_url_parse__ipv4_root(void)
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

void test_url_parse__ipv4_implied_root(void)
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

void test_url_parse__ipv4_implied_root_custom_port(void)
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

void test_url_parse__ipv4_implied_root_empty_port(void)
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

void test_url_parse__ipv4_encoded_password(void)
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

void test_url_parse__ipv4_user(void)
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

void test_url_parse__ipv4_user_pass(void)
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

void test_url_parse__ipv4_port(void)
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

void test_url_parse__ipv4_empty_port(void)
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

void test_url_parse__ipv4_user_port(void)
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

void test_url_parse__ipv4_user_pass_port(void)
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

void test_url_parse__ipv6_trivial(void)
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

void test_url_parse__ipv6_root(void)
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

void test_url_parse__ipv6_implied_root(void)
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

void test_url_parse__ipv6_implied_root_custom_port(void)
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

void test_url_parse__ipv6_implied_root_empty_port(void)
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

void test_url_parse__ipv6_encoded_password(void)
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

void test_url_parse__ipv6_user(void)
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

void test_url_parse__ipv6_user_pass(void)
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

void test_url_parse__ipv6_port(void)
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

void test_url_parse__ipv6_empty_port(void)
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

void test_url_parse__ipv6_user_port(void)
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

void test_url_parse__ipv6_user_pass_port(void)
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

void test_url_parse__ipv6_invalid_addresses(void)
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

	/* Invalid character inside address */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata, "http://[fe8o::dcad:beff:fe00:0001]/resource"));

	/* Characters before/after braces */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://fe80::[dcad:beff:fe00:0001]/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://cafe[fe80::dcad:beff:fe00:0001]/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse(&conndata,
		"http://[fe80::dcad:beff:fe00:0001]cafe/resource"));
}

/* Oddities */

void test_url_parse__empty_scheme(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "://example.com/resource"));
	cl_assert_equal_s(conndata.scheme, NULL);
	cl_assert_equal_s(conndata.host, NULL);
	cl_assert_equal_s(conndata.port, NULL);
	cl_assert_equal_s(conndata.path, "//example.com/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__invalid_scheme_is_relative(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "foo!bar://host:42/path/to/project?query_string=yes"));
	cl_assert_equal_p(conndata.scheme, NULL);
	cl_assert_equal_p(conndata.host, NULL);
	cl_assert_equal_p(conndata.port, NULL);
	cl_assert_equal_s(conndata.path, "foo!bar://host:42/path/to/project");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_s(conndata.query, "query_string=yes");
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__scheme_case_is_normalized(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "GIT+SSH://host:42/path/to/project"));
	cl_assert_equal_s(conndata.scheme, "git+ssh");
}

void test_url_parse__nonhierarchical_scheme(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "mailto:foobar@example.com"));
	cl_assert_equal_s(conndata.scheme, "mailto");
	cl_assert_equal_p(conndata.host, NULL);
	cl_assert_equal_p(conndata.port, NULL);
	cl_assert_equal_s(conndata.path, "foobar@example.com");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__no_scheme_relative_path(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "path"));
	cl_assert_equal_p(conndata.scheme, NULL);
	cl_assert_equal_p(conndata.host, NULL);
	cl_assert_equal_p(conndata.port, NULL);
	cl_assert_equal_s(conndata.path, "path");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__no_scheme_absolute_path(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "/path"));
	cl_assert_equal_p(conndata.scheme, NULL);
	cl_assert_equal_p(conndata.host, NULL);
	cl_assert_equal_p(conndata.port, NULL);
	cl_assert_equal_s(conndata.path, "/path");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__empty_path(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "mailto:"));
	cl_assert_equal_s(conndata.scheme, "mailto");
	cl_assert_equal_p(conndata.host, NULL);
	cl_assert_equal_p(conndata.port, NULL);
	cl_assert_equal_s(conndata.path, NULL);
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__empty_path_with_empty_authority(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "http://"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_p(conndata.host, NULL);
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_parse__http_follows_the_rfc(void)
{
	cl_git_fail(git_net_url_parse(&conndata, "https://my.email.address@gmail.com@source.developers.google.com:4433/p/my-project/r/my-repository"));
}

void test_url_parse__ssh_from_terrible_google_rfc_violating_products(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "ssh://my.email.address@gmail.com@source.developers.google.com:2022/p/my-project/r/my-repository"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "source.developers.google.com");
	cl_assert_equal_s(conndata.port, "2022");
	cl_assert_equal_s(conndata.path, "/p/my-project/r/my-repository");
	cl_assert_equal_s(conndata.username, "my.email.address@gmail.com");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__ssh_with_password_from_terrible_google_rfc_violating_products(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "ssh://my.email.address@gmail.com:seekret@source.developers.google.com:2022/p/my-project/r/my-repository"));
	cl_assert_equal_s(conndata.scheme, "ssh");
	cl_assert_equal_s(conndata.host, "source.developers.google.com");
	cl_assert_equal_s(conndata.port, "2022");
	cl_assert_equal_s(conndata.path, "/p/my-project/r/my-repository");
	cl_assert_equal_s(conndata.username, "my.email.address@gmail.com");
	cl_assert_equal_s(conndata.password, "seekret");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_parse__spaces_in_the_name(void)
{
	cl_git_pass(git_net_url_parse(&conndata, "https://libgit2@dev.azure.com/libgit2/test/_git/spaces%20in%20the%20name"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "dev.azure.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/libgit2/test/_git/spaces%20in%20the%20name");
	cl_assert_equal_s(conndata.username, "libgit2");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}
