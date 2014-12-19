#include "clar_libgit2.h"
#include "netops.h"

static char *host, *port, *path, *user, *pass;
static gitno_connection_data conndata;

void test_network_urlparse__initialize(void)
{
	host = port = path = user = pass = NULL;
	memset(&conndata, 0, sizeof(conndata));
}

void test_network_urlparse__cleanup(void)
{
#define FREE_AND_NULL(x) if (x) { git__free(x); x = NULL; }
	FREE_AND_NULL(host);
	FREE_AND_NULL(port);
	FREE_AND_NULL(path);
	FREE_AND_NULL(user);
	FREE_AND_NULL(pass);

	gitno_connection_data_free_ptrs(&conndata);
}

void test_network_urlparse__trivial(void)
{
	cl_git_pass(gitno_extract_url_parts(&host, &port, &path, &user, &pass,
				"http://example.com/resource", "8080"));
	cl_assert_equal_s(host, "example.com");
	cl_assert_equal_s(port, "8080");
	cl_assert_equal_s(path, "/resource");
	cl_assert_equal_p(user, NULL);
	cl_assert_equal_p(pass, NULL);
}

void test_network_urlparse__root(void)
{
	cl_git_pass(gitno_extract_url_parts(&host, &port, &path, &user, &pass,
				"http://example.com/", "8080"));
	cl_assert_equal_s(host, "example.com");
	cl_assert_equal_s(port, "8080");
	cl_assert_equal_s(path, "/");
	cl_assert_equal_p(user, NULL);
	cl_assert_equal_p(pass, NULL);
}

void test_network_urlparse__just_hostname(void)
{
	cl_git_fail_with(GIT_EINVALIDSPEC,
			 gitno_extract_url_parts(&host, &port, &path, &user, &pass,
						 "http://example.com", "8080"));
}

void test_network_urlparse__encoded_password(void)
{
	cl_git_pass(gitno_extract_url_parts(&host, &port, &path, &user, &pass,
				"https://user:pass%2fis%40bad@hostname.com:1234/", "1"));
	cl_assert_equal_s(host, "hostname.com");
	cl_assert_equal_s(port, "1234");
	cl_assert_equal_s(path, "/");
	cl_assert_equal_s(user, "user");
	cl_assert_equal_s(pass, "pass/is@bad");
}

void test_network_urlparse__user(void)
{
	cl_git_pass(gitno_extract_url_parts(&host, &port, &path, &user, &pass,
				"https://user@example.com/resource", "8080"));
	cl_assert_equal_s(host, "example.com");
	cl_assert_equal_s(port, "8080");
	cl_assert_equal_s(path, "/resource");
	cl_assert_equal_s(user, "user");
	cl_assert_equal_p(pass, NULL);
}

void test_network_urlparse__user_pass(void)
{
	/* user:pass@hostname.tld/resource */
	cl_git_pass(gitno_extract_url_parts(&host, &port, &path, &user, &pass,
				"https://user:pass@example.com/resource", "8080"));
	cl_assert_equal_s(host, "example.com");
	cl_assert_equal_s(port, "8080");
	cl_assert_equal_s(path, "/resource");
	cl_assert_equal_s(user, "user");
	cl_assert_equal_s(pass, "pass");
}

void test_network_urlparse__port(void)
{
	/* hostname.tld:port/resource */
	cl_git_pass(gitno_extract_url_parts(&host, &port, &path, &user, &pass,
				"https://example.com:9191/resource", "8080"));
	cl_assert_equal_s(host, "example.com");
	cl_assert_equal_s(port, "9191");
	cl_assert_equal_s(path, "/resource");
	cl_assert_equal_p(user, NULL);
	cl_assert_equal_p(pass, NULL);
}

void test_network_urlparse__user_port(void)
{
	/* user@hostname.tld:port/resource */
	cl_git_pass(gitno_extract_url_parts(&host, &port, &path, &user, &pass,
				"https://user@example.com:9191/resource", "8080"));
	cl_assert_equal_s(host, "example.com");
	cl_assert_equal_s(port, "9191");
	cl_assert_equal_s(path, "/resource");
	cl_assert_equal_s(user, "user");
	cl_assert_equal_p(pass, NULL);
}

void test_network_urlparse__user_pass_port(void)
{
	/* user:pass@hostname.tld:port/resource */
	cl_git_pass(gitno_extract_url_parts(&host, &port, &path, &user, &pass,
				"https://user:pass@example.com:9191/resource", "8080"));
	cl_assert_equal_s(host, "example.com");
	cl_assert_equal_s(port, "9191");
	cl_assert_equal_s(path, "/resource");
	cl_assert_equal_s(user, "user");
	cl_assert_equal_s(pass, "pass");
}

void test_network_urlparse__connection_data_http(void)
{
	cl_git_pass(gitno_connection_data_from_url(&conndata,
				"http://example.com/foo/bar/baz", "bar/baz"));
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/foo/");
	cl_assert_equal_p(conndata.user, NULL);
	cl_assert_equal_p(conndata.pass, NULL);
	cl_assert_equal_i(conndata.use_ssl, false);
}

void test_network_urlparse__connection_data_ssl(void)
{
	cl_git_pass(gitno_connection_data_from_url(&conndata,
				"https://example.com/foo/bar/baz", "bar/baz"));
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/foo/");
	cl_assert_equal_p(conndata.user, NULL);
	cl_assert_equal_p(conndata.pass, NULL);
	cl_assert_equal_i(conndata.use_ssl, true);
}

void test_network_urlparse__encoded_username_password(void)
{
	cl_git_pass(gitno_connection_data_from_url(&conndata,
				"https://user%2fname:pass%40word%zyx%v@example.com/foo/bar/baz", "bar/baz"));
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/foo/");
	cl_assert_equal_s(conndata.user, "user/name");
	cl_assert_equal_s(conndata.pass, "pass@word%zyx%v");
	cl_assert_equal_i(conndata.use_ssl, true);
}

void test_network_urlparse__connection_data_cross_host_redirect(void)
{
	conndata.host = git__strdup("bar.com");
	cl_git_fail_with(gitno_connection_data_from_url(&conndata,
				"https://foo.com/bar/baz", NULL),
			-1);
}

void test_network_urlparse__connection_data_http_downgrade(void)
{
	conndata.use_ssl = true;
	cl_git_fail_with(gitno_connection_data_from_url(&conndata,
				"http://foo.com/bar/baz", NULL),
			-1);
}

void test_network_urlparse__connection_data_relative_redirect(void)
{
	cl_git_pass(gitno_connection_data_from_url(&conndata,
				"http://foo.com/bar/baz/biff", NULL));
	cl_git_pass(gitno_connection_data_from_url(&conndata,
				"/zap/baz/biff?bam", NULL));
	cl_assert_equal_s(conndata.host, "foo.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/zap/baz/biff?bam");
	cl_assert_equal_p(conndata.user, NULL);
	cl_assert_equal_p(conndata.pass, NULL);
	cl_assert_equal_i(conndata.use_ssl, false);
}

void test_network_urlparse__connection_data_relative_redirect_ssl(void)
{
	cl_git_pass(gitno_connection_data_from_url(&conndata,
				"https://foo.com/bar/baz/biff", NULL));
	cl_git_pass(gitno_connection_data_from_url(&conndata,
				"/zap/baz/biff?bam", NULL));
	cl_assert_equal_s(conndata.host, "foo.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/zap/baz/biff?bam");
	cl_assert_equal_p(conndata.user, NULL);
	cl_assert_equal_p(conndata.pass, NULL);
	cl_assert_equal_i(conndata.use_ssl, true);
}

/* Run this under valgrind */
void test_network_urlparse__connection_data_cleanup(void)
{
	cl_git_pass(gitno_connection_data_from_url(&conndata,
				"http://foo.com/bar/baz/biff", "baz/biff"));
	cl_git_pass(gitno_connection_data_from_url(&conndata,
				"https://foo.com/bar/baz/biff", "baz/biff"));
}
