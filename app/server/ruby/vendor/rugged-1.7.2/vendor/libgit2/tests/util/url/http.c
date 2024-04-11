#include "clar_libgit2.h"
#include "net.h"

static git_net_url conndata;

void test_url_http__initialize(void)
{
	memset(&conndata, 0, sizeof(conndata));
}

void test_url_http__cleanup(void)
{
	git_net_url_dispose(&conndata);
}

/* Hostname */

void test_url_http__has_scheme(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "http://example.com/resource"));
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

void test_url_http__no_scheme(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "example.com/resource"));
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

void test_url_http__hostname_root(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "example.com/"));
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

void test_url_http__hostname_implied_root(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "example.com"));
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

void test_url_http__hostname_numeric(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "8888888/"));
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

void test_url_http__hostname_implied_root_custom_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "example.com:42"));
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

void test_url_http__hostname_implied_root_empty_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "example.com:"));
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

void test_url_http__hostname_encoded_password(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
				"user:pass%2fis%40bad@hostname.com:1234/"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "hostname.com");
	cl_assert_equal_s(conndata.port, "1234");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass/is@bad");
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__hostname_user(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
				"user@example.com/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__hostname_user_pass(void)
{
	/* user:pass@hostname.tld/resource */
	cl_git_pass(git_net_url_parse_http(&conndata,
				"user:pass@example.com/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__hostname_port(void)
{
	/* hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse_http(&conndata,
				"example.com:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__hostname_empty_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "example.com:/resource"));
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

void test_url_http__hostname_user_port(void)
{
	/* user@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse_http(&conndata,
				"user@example.com:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__hostname_user_pass_port(void)
{
	/* user:pass@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse_http(&conndata,
				"user:pass@example.com:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__hostname_user_pass_port_query(void)
{
	/* user:pass@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse_http(&conndata,
				"user:pass@example.com:9191/resource?query=q&foo=bar&z=asdf"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_s(conndata.query, "query=q&foo=bar&z=asdf");
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__hostname_user_pass_port_fragment(void)
{
	/* user:pass@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse_http(&conndata,
				"user:pass@example.com:9191/resource#fragment"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_p(conndata.query, NULL);
	cl_assert_equal_s(conndata.fragment, "fragment");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__hostname_user_pass_port_query_fragment(void)
{
	/* user:pass@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse_http(&conndata,
				"user:pass@example.com:9191/resource?query=q&foo=bar&z=asdf#fragment"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_s(conndata.query, "query=q&foo=bar&z=asdf");
	cl_assert_equal_s(conndata.fragment, "fragment");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__fragment_with_question_mark(void)
{
	/* user:pass@hostname.tld:port/resource */
	cl_git_pass(git_net_url_parse_http(&conndata,
				"user:pass@example.com:9191/resource#fragment_with?question_mark"));
	cl_assert_equal_s(conndata.scheme, "http");
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

void test_url_http__ipv4_trivial(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "192.168.1.1/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv4_root(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "192.168.1.1/"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv4_implied_root(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "192.168.1.1"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv4_implied_root_custom_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "192.168.1.1:42"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "42");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__ipv4_implied_root_empty_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "192.168.1.1:"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv4_encoded_password(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"user:pass%2fis%40bad@192.168.1.1:1234/"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "1234");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass/is@bad");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__ipv4_user(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"user@192.168.1.1/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv4_user_pass(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"user:pass@192.168.1.1/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv4_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"192.168.1.1:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__ipv4_empty_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "192.168.1.1:/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv4_user_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"user@192.168.1.1:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__ipv4_user_pass_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"user:pass@192.168.1.1:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "192.168.1.1");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

/* IPv6 addresses */

void test_url_http__ipv6_trivial(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "[fe80::dcad:beff:fe00:0001]/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv6_root(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "[fe80::dcad:beff:fe00:0001]/"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv6_implied_root(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "[fe80::dcad:beff:fe00:0001]"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv6_implied_root_custom_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "[fe80::dcad:beff:fe00:0001]:42"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "42");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__ipv6_implied_root_empty_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "[fe80::dcad:beff:fe00:0001]:"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv6_encoded_password(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"user:pass%2fis%40bad@[fe80::dcad:beff:fe00:0001]:1234/"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "1234");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass/is@bad");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__ipv6_user(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"user@[fe80::dcad:beff:fe00:0001]/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv6_user_pass(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"user:pass@[fe80::dcad:beff:fe00:0001]/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv6_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"[fe80::dcad:beff:fe00:0001]:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__ipv6_empty_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "[fe80::dcad:beff:fe00:0001]:/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__ipv6_user_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"user@[fe80::dcad:beff:fe00:0001]:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__ipv6_user_pass_port(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata,
		"user:pass@[fe80::dcad:beff:fe00:0001]:9191/resource"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "fe80::dcad:beff:fe00:0001");
	cl_assert_equal_s(conndata.port, "9191");
	cl_assert_equal_s(conndata.path, "/resource");
	cl_assert_equal_s(conndata.username, "user");
	cl_assert_equal_s(conndata.password, "pass");
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 0);
}

void test_url_http__ipv6_invalid_addresses(void)
{
	/* Opening bracket missing */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001]/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001]/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001]"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001]:42"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001]:"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user:pass%2fis%40bad@fe80::dcad:beff:fe00:0001]:1234/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user@fe80::dcad:beff:fe00:0001]/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user:pass@fe80::dcad:beff:fe00:0001]/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001]:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001]:/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user@fe80::dcad:beff:fe00:0001]:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user:pass@fe80::dcad:beff:fe00:0001]:9191/resource"));

	/* Closing bracket missing */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"[fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"[fe80::dcad:beff:fe00:0001/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"[fe80::dcad:beff:fe00:0001"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"[fe80::dcad:beff:fe00:0001:42"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"[fe80::dcad:beff:fe00:0001:"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user:pass%2fis%40bad@[fe80::dcad:beff:fe00:0001:1234/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user@[fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user:pass@[fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"[fe80::dcad:beff:fe00:0001:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"[fe80::dcad:beff:fe00:0001:/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user@[fe80::dcad:beff:fe00:0001:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user:pass@[fe80::dcad:beff:fe00:0001:9191/resource"));

	/* Both brackets missing */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001:42"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001:"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user:pass%2fis%40bad@fe80::dcad:beff:fe00:0001:1234/"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user@fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user:pass@fe80::dcad:beff:fe00:0001/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::dcad:beff:fe00:0001:/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user@fe80::dcad:beff:fe00:0001:9191/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"user:pass@fe80::dcad:beff:fe00:0001:9191/resource"));

	/* Invalid character inside address */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata, "[fe8o::dcad:beff:fe00:0001]/resource"));

	/* Characters before/after braces */
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"fe80::[dcad:beff:fe00:0001]/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"cafe[fe80::dcad:beff:fe00:0001]/resource"));
	cl_git_fail_with(GIT_EINVALIDSPEC, git_net_url_parse_http(&conndata,
		"[fe80::dcad:beff:fe00:0001]cafe/resource"));
}

/* Oddities */

void test_url_http__invalid_scheme_is_relative(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "foo!bar://host:42/path/to/project?query_string=yes"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "foo!bar");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "//host:42/path/to/project");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_s(conndata.query, "query_string=yes");
	cl_assert_equal_p(conndata.fragment, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__scheme_case_is_normalized(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "GIT+SSH://host:42/path/to/project"));
	cl_assert_equal_s(conndata.scheme, "git+ssh");
}

void test_url_http__no_scheme_relative_path(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "path"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "path");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__no_scheme_absolute_path(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "/path"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_p(conndata.host, NULL);
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/path");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__empty_path_with_empty_authority(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, ""));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_p(conndata.host, NULL);
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}

void test_url_http__spaces_in_the_name(void)
{
	cl_git_pass(git_net_url_parse_http(&conndata, "libgit2@dev.azure.com/libgit2/test/_git/spaces%20in%20the%20name"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "dev.azure.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/libgit2/test/_git/spaces%20in%20the%20name");
	cl_assert_equal_s(conndata.username, "libgit2");
	cl_assert_equal_p(conndata.password, NULL);
	cl_assert_equal_i(git_net_url_is_default_port(&conndata), 1);
}
