#include "clar_libgit2.h"
#include "net.h"

static git_net_url conndata;

void test_network_urlparse__initialize(void)
{
	memset(&conndata, 0, sizeof(conndata));
}

void test_network_urlparse__cleanup(void)
{
	git_net_url_dispose(&conndata);
}

void test_network_urlparse__trivial(void)
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

void test_network_urlparse__root(void)
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

void test_network_urlparse__implied_root(void)
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

void test_network_urlparse__implied_root_custom_port(void)
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

void test_network_urlparse__implied_root_empty_port(void)
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

void test_network_urlparse__encoded_password(void)
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

void test_network_urlparse__user(void)
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

void test_network_urlparse__user_pass(void)
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

void test_network_urlparse__port(void)
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

void test_network_urlparse__empty_port(void)
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

void test_network_urlparse__user_port(void)
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

void test_network_urlparse__user_pass_port(void)
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
