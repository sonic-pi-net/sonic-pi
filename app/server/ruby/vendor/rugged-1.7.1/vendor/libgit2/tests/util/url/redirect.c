#include "clar_libgit2.h"
#include "net.h"

static git_net_url conndata;

void test_url_redirect__initialize(void)
{
	memset(&conndata, 0, sizeof(conndata));
}

void test_url_redirect__cleanup(void)
{
	git_net_url_dispose(&conndata);
}

void test_url_redirect__redirect_http(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"http://example.com/foo/bar/baz"));
	cl_git_pass(git_net_url_apply_redirect(&conndata,
		"http://example.com/foo/bar/baz", false, "bar/baz"));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/foo/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
}

void test_url_redirect__redirect_ssl(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://example.com/foo/bar/baz"));
	cl_git_pass(git_net_url_apply_redirect(&conndata,
		"https://example.com/foo/bar/baz", false, "bar/baz"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/foo/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
}

void test_url_redirect__redirect_leaves_root_path(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://example.com/foo/bar/baz"));
	cl_git_pass(git_net_url_apply_redirect(&conndata,
		"https://example.com/foo/bar/baz", false, "/foo/bar/baz"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
}

void test_url_redirect__redirect_encoded_username_password(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://user%2fname:pass%40word%zyx%v@example.com/foo/bar/baz"));
	cl_git_pass(git_net_url_apply_redirect(&conndata,
		"https://user%2fname:pass%40word%zyx%v@example.com/foo/bar/baz", false, "bar/baz"));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "example.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/foo/");
	cl_assert_equal_s(conndata.username, "user/name");
	cl_assert_equal_s(conndata.password, "pass@word%zyx%v");
}

void test_url_redirect__redirect_cross_host_allowed(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://bar.com/bar/baz"));
	cl_git_pass(git_net_url_apply_redirect(&conndata,
		"https://foo.com/bar/baz", true, NULL));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "foo.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/bar/baz");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
}

void test_url_redirect__redirect_cross_host_denied(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://bar.com/bar/baz"));
	cl_git_fail_with(git_net_url_apply_redirect(&conndata,
		"https://foo.com/bar/baz", false, NULL), -1);
}

void test_url_redirect__redirect_http_downgrade_denied(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://foo.com/bar/baz"));
	cl_git_fail_with(git_net_url_apply_redirect(&conndata,
		"http://foo.com/bar/baz", true, NULL), -1);
}

void test_url_redirect__redirect_relative(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"http://foo.com/bar/baz/biff"));
	cl_git_pass(git_net_url_apply_redirect(&conndata,
		"/zap/baz/biff?bam", true, NULL));
	cl_assert_equal_s(conndata.scheme, "http");
	cl_assert_equal_s(conndata.host, "foo.com");
	cl_assert_equal_s(conndata.port, "80");
	cl_assert_equal_s(conndata.path, "/zap/baz/biff?bam");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
}

void test_url_redirect__redirect_relative_ssl(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://foo.com/bar/baz/biff"));
	cl_git_pass(git_net_url_apply_redirect(&conndata,
		"/zap/baz/biff?bam", true, NULL));
	cl_assert_equal_s(conndata.scheme, "https");
	cl_assert_equal_s(conndata.host, "foo.com");
	cl_assert_equal_s(conndata.port, "443");
	cl_assert_equal_s(conndata.path, "/zap/baz/biff?bam");
	cl_assert_equal_p(conndata.username, NULL);
	cl_assert_equal_p(conndata.password, NULL);
}

void test_url_redirect__service_query_no_query_params_in_location(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://foo.com/bar/info/refs?service=git-upload-pack"));
	cl_git_pass(git_net_url_apply_redirect(&conndata,
		"/baz/info/refs", true, "/info/refs?service=git-upload-pack"));
	cl_assert_equal_s(conndata.path, "/baz");
}

void test_url_redirect__service_query_with_query_params_in_location(void)
{
	cl_git_pass(git_net_url_parse(&conndata,
		"https://foo.com/bar/info/refs?service=git-upload-pack"));
	cl_git_pass(git_net_url_apply_redirect(&conndata,
		"/baz/info/refs?service=git-upload-pack", true, "/info/refs?service=git-upload-pack"));
	cl_assert_equal_s(conndata.path, "/baz");
}
