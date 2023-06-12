#include "clar_libgit2.h"
#include "futils.h"
#include "net.h"
#include "remote.h"

static git_repository *repo;
static git_net_url url = GIT_NET_URL_INIT;

static char *orig_http_proxy = NULL;
static char *orig_https_proxy = NULL;
static char *orig_no_proxy = NULL;

void test_remote_httpproxy__initialize(void)
{
	git_remote *remote;

	repo = cl_git_sandbox_init("testrepo");
	cl_git_pass(git_remote_create(&remote, repo, "lg2", "https://github.com/libgit2/libgit2"));
	cl_git_pass(git_net_url_parse(&url, "https://github.com/libgit2/libgit2"));

	git_remote_free(remote);

	/* Clear everything for a fresh start */
	orig_http_proxy = cl_getenv("HTTP_PROXY");
	orig_https_proxy = cl_getenv("HTTPS_PROXY");
	orig_no_proxy = cl_getenv("NO_PROXY");

	cl_setenv("HTTP_PROXY", NULL);
	cl_setenv("HTTPS_PROXY", NULL);
	cl_setenv("NO_PROXY", NULL);
}

void test_remote_httpproxy__cleanup(void)
{
	cl_setenv("HTTP_PROXY", orig_http_proxy);
	cl_setenv("HTTPS_PROXY", orig_https_proxy);
	cl_setenv("NO_PROXY", orig_no_proxy);

	git__free(orig_http_proxy);
	git__free(orig_https_proxy);
	git__free(orig_no_proxy);

	git_net_url_dispose(&url);
	cl_git_sandbox_cleanup();
}

static void assert_proxy_is(const char *expected)
{
	git_remote *remote;
	char *proxy;

	cl_git_pass(git_remote_lookup(&remote, repo, "lg2"));
	cl_git_pass(git_remote__http_proxy(&proxy, remote, &url));

	if (expected)
		cl_assert_equal_s(proxy, expected);
	else
		cl_assert_equal_p(proxy, expected);

	git_remote_free(remote);
	git__free(proxy);
}

static void assert_config_match(const char *config, const char *expected)
{
	git_remote *remote;
	char *proxy;

	if (config)
		cl_repo_set_string(repo, config, expected);

	cl_git_pass(git_remote_lookup(&remote, repo, "lg2"));
	cl_git_pass(git_remote__http_proxy(&proxy, remote, &url));

	if (expected)
		cl_assert_equal_s(proxy, expected);
	else
		cl_assert_equal_p(proxy, expected);

	git_remote_free(remote);
	git__free(proxy);
}

void test_remote_httpproxy__config_overrides(void)
{
	/*
	 * http.proxy should be honored, then http.<url>.proxy should
	 * be honored in increasing specificity of the url.  finally,
	 * remote.<name>.proxy is the most specific.
	 */
	assert_config_match(NULL, NULL);
	assert_config_match("http.proxy", "http://localhost:1/");
	assert_config_match("http.https://github.com.proxy", "http://localhost:2/");
	assert_config_match("http.https://github.com/.proxy", "http://localhost:3/");
	assert_config_match("http.https://github.com/libgit2.proxy", "http://localhost:4/");
	assert_config_match("http.https://github.com/libgit2/.proxy", "http://localhost:5/");
	assert_config_match("http.https://github.com/libgit2/libgit2.proxy", "http://localhost:6/");
	assert_config_match("remote.lg2.proxy", "http://localhost:7/");
}

void test_remote_httpproxy__config_empty_overrides(void)
{
	/*
	 * with greater specificity, an empty config entry overrides
	 * a set one
	 */
	assert_config_match("http.proxy", "http://localhost:1/");
	assert_config_match("http.https://github.com.proxy", "");
	assert_config_match("http.https://github.com/libgit2/libgit2.proxy", "http://localhost:2/");
	assert_config_match("remote.lg2.proxy", "");
}

static void assert_global_config_match(const char *config, const char *expected)
{
	git_remote *remote;
	char *proxy;
	git_config* cfg;

	if (config) {
		cl_git_pass(git_config_open_default(&cfg));
		git_config_set_string(cfg, config, expected);
		git_config_free(cfg);
	}

	cl_git_pass(git_remote_create_detached(&remote, "https://github.com/libgit2/libgit2"));
	cl_git_pass(git_remote__http_proxy(&proxy, remote, &url));

	if (expected)
		cl_assert_equal_s(proxy, expected);
	else
		cl_assert_equal_p(proxy, expected);

	git_remote_free(remote);
	git__free(proxy);
}

void test_remote_httpproxy__config_overrides_detached_remote(void)
{
	cl_fake_globalconfig(NULL);

	assert_global_config_match(NULL, NULL);
	assert_global_config_match("http.proxy", "http://localhost:1/");
	assert_global_config_match("http.https://github.com.proxy", "http://localhost:2/");
	assert_global_config_match("http.https://github.com/.proxy", "http://localhost:3/");
	assert_global_config_match("http.https://github.com/libgit2.proxy", "http://localhost:4/");
	assert_global_config_match("http.https://github.com/libgit2/.proxy", "http://localhost:5/");
	assert_global_config_match("http.https://github.com/libgit2/libgit2.proxy", "http://localhost:6/");
}

void test_remote_httpproxy__env(void)
{
	/* HTTP proxy is ignored for HTTPS */
	cl_setenv("HTTP_PROXY", "http://localhost:9/");
	assert_proxy_is(NULL);

	/* HTTPS proxy is honored for HTTPS */
	cl_setenv("HTTPS_PROXY", "http://localhost:10/");
	assert_proxy_is("http://localhost:10/");

	/* NO_PROXY is honored */
	cl_setenv("NO_PROXY", "github.com:443");
	assert_proxy_is(NULL);

	cl_setenv("NO_PROXY", "github.com:80");
	assert_proxy_is("http://localhost:10/");

	cl_setenv("NO_PROXY", "github.com");
	assert_proxy_is(NULL);

	cl_setenv("NO_PROXY", "github.dev,github.com,github.foo");
	assert_proxy_is(NULL);

	cl_setenv("HTTPS_PROXY", "");
	assert_proxy_is(NULL);

	/* configuration overrides environment variables */
	cl_setenv("HTTPS_PROXY", "http://localhost:10/");
	cl_setenv("NO_PROXY", "github.none");
	assert_config_match("http.https://github.com.proxy", "http://localhost:11/");
}
