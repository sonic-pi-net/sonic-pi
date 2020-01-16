#include "clar_libgit2.h"

#include "git2/clone.h"
#include "git2/cred_helpers.h"
#include "remote.h"
#include "futils.h"
#include "refs.h"

#define LIVE_REPO_URL "http://github.com/libgit2/TestGitRepository"
#define LIVE_EMPTYREPO_URL "http://github.com/libgit2/TestEmptyRepository"
#define BB_REPO_URL "https://libgit3@bitbucket.org/libgit2/testgitrepository.git"
#define BB_REPO_URL_WITH_PASS "https://libgit3:libgit3@bitbucket.org/libgit2/testgitrepository.git"
#define BB_REPO_URL_WITH_WRONG_PASS "https://libgit3:wrong@bitbucket.org/libgit2/testgitrepository.git"

#define SSH_REPO_URL "ssh://github.com/libgit2/TestGitRepository"

static git_repository *g_repo;
static git_clone_options g_options;

static char *_remote_url = NULL;
static char *_remote_user = NULL;
static char *_remote_pass = NULL;
static char *_remote_sslnoverify = NULL;
static char *_remote_ssh_pubkey = NULL;
static char *_remote_ssh_privkey = NULL;
static char *_remote_ssh_passphrase = NULL;
static char *_remote_ssh_fingerprint = NULL;
static char *_remote_proxy_scheme = NULL;
static char *_remote_proxy_host = NULL;
static char *_remote_proxy_user = NULL;
static char *_remote_proxy_pass = NULL;
static char *_remote_proxy_selfsigned = NULL;

static int _orig_proxies_need_reset = 0;
static char *_orig_http_proxy = NULL;
static char *_orig_https_proxy = NULL;

static int ssl_cert(git_cert *cert, int valid, const char *host, void *payload)
{
	GIT_UNUSED(cert);
	GIT_UNUSED(host);
	GIT_UNUSED(payload);

	if (_remote_sslnoverify != NULL)
		valid = 1;

	return valid ? 0 : GIT_ECERTIFICATE;
}

void test_online_clone__initialize(void)
{
	git_checkout_options dummy_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_fetch_options dummy_fetch = GIT_FETCH_OPTIONS_INIT;

	g_repo = NULL;

	memset(&g_options, 0, sizeof(git_clone_options));
	g_options.version = GIT_CLONE_OPTIONS_VERSION;
	g_options.checkout_opts = dummy_opts;
	g_options.checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;
	g_options.fetch_opts = dummy_fetch;
	g_options.fetch_opts.callbacks.certificate_check = ssl_cert;

	_remote_url = cl_getenv("GITTEST_REMOTE_URL");
	_remote_user = cl_getenv("GITTEST_REMOTE_USER");
	_remote_pass = cl_getenv("GITTEST_REMOTE_PASS");
	_remote_sslnoverify = cl_getenv("GITTEST_REMOTE_SSL_NOVERIFY");
	_remote_ssh_pubkey = cl_getenv("GITTEST_REMOTE_SSH_PUBKEY");
	_remote_ssh_privkey = cl_getenv("GITTEST_REMOTE_SSH_KEY");
	_remote_ssh_passphrase = cl_getenv("GITTEST_REMOTE_SSH_PASSPHRASE");
	_remote_ssh_fingerprint = cl_getenv("GITTEST_REMOTE_SSH_FINGERPRINT");
	_remote_proxy_scheme = cl_getenv("GITTEST_REMOTE_PROXY_SCHEME");
	_remote_proxy_host = cl_getenv("GITTEST_REMOTE_PROXY_HOST");
	_remote_proxy_user = cl_getenv("GITTEST_REMOTE_PROXY_USER");
	_remote_proxy_pass = cl_getenv("GITTEST_REMOTE_PROXY_PASS");
	_remote_proxy_selfsigned = cl_getenv("GITTEST_REMOTE_PROXY_SELFSIGNED");

	_orig_proxies_need_reset = 0;
}

void test_online_clone__cleanup(void)
{
	if (g_repo) {
		git_repository_free(g_repo);
		g_repo = NULL;
	}
	cl_fixture_cleanup("./foo");

	git__free(_remote_url);
	git__free(_remote_user);
	git__free(_remote_pass);
	git__free(_remote_sslnoverify);
	git__free(_remote_ssh_pubkey);
	git__free(_remote_ssh_privkey);
	git__free(_remote_ssh_passphrase);
	git__free(_remote_ssh_fingerprint);
	git__free(_remote_proxy_scheme);
	git__free(_remote_proxy_host);
	git__free(_remote_proxy_user);
	git__free(_remote_proxy_pass);
	git__free(_remote_proxy_selfsigned);

	if (_orig_proxies_need_reset) {
		cl_setenv("HTTP_PROXY", _orig_http_proxy);
		cl_setenv("HTTPS_PROXY", _orig_https_proxy);

		git__free(_orig_http_proxy);
		git__free(_orig_https_proxy);
	}
}

void test_online_clone__network_full(void)
{
	git_remote *origin;

	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));
	cl_assert(!git_repository_is_bare(g_repo));
	cl_git_pass(git_remote_lookup(&origin, g_repo, "origin"));

	cl_assert_equal_i(GIT_REMOTE_DOWNLOAD_TAGS_AUTO, origin->download_tags);

	git_remote_free(origin);
}

void test_online_clone__network_bare(void)
{
	git_remote *origin;

	g_options.bare = true;

	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));
	cl_assert(git_repository_is_bare(g_repo));
	cl_git_pass(git_remote_lookup(&origin, g_repo, "origin"));

	git_remote_free(origin);
}

void test_online_clone__empty_repository(void)
{
	git_reference *head;

	cl_git_pass(git_clone(&g_repo, LIVE_EMPTYREPO_URL, "./foo", &g_options));

	cl_assert_equal_i(true, git_repository_is_empty(g_repo));
	cl_assert_equal_i(true, git_repository_head_unborn(g_repo));

	cl_git_pass(git_reference_lookup(&head, g_repo, GIT_HEAD_FILE));
	cl_assert_equal_i(GIT_REFERENCE_SYMBOLIC, git_reference_type(head));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));

	git_reference_free(head);
}

static void checkout_progress(const char *path, size_t cur, size_t tot, void *payload)
{
	bool *was_called = (bool*)payload;
	GIT_UNUSED(path); GIT_UNUSED(cur); GIT_UNUSED(tot);
	(*was_called) = true;
}

static int fetch_progress(const git_indexer_progress *stats, void *payload)
{
	bool *was_called = (bool*)payload;
	GIT_UNUSED(stats);
	(*was_called) = true;
	return 0;
}

void test_online_clone__can_checkout_a_cloned_repo(void)
{
	git_buf path = GIT_BUF_INIT;
	git_reference *head;
	bool checkout_progress_cb_was_called = false,
		  fetch_progress_cb_was_called = false;

	g_options.checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;
	g_options.checkout_opts.progress_cb = &checkout_progress;
	g_options.checkout_opts.progress_payload = &checkout_progress_cb_was_called;
	g_options.fetch_opts.callbacks.transfer_progress = &fetch_progress;
	g_options.fetch_opts.callbacks.payload = &fetch_progress_cb_was_called;

	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(g_repo), "master.txt"));
	cl_assert_equal_i(true, git_path_isfile(git_buf_cstr(&path)));

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_assert_equal_i(GIT_REFERENCE_SYMBOLIC, git_reference_type(head));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));

	cl_assert_equal_i(true, checkout_progress_cb_was_called);
	cl_assert_equal_i(true, fetch_progress_cb_was_called);

	git_reference_free(head);
	git_buf_dispose(&path);
}

static int remote_mirror_cb(git_remote **out, git_repository *repo,
			    const char *name, const char *url, void *payload)
{
	int error;
	git_remote *remote;

	GIT_UNUSED(payload);

	if ((error = git_remote_create_with_fetchspec(&remote, repo, name, url, "+refs/*:refs/*")) < 0)
		return error;

	*out = remote;
	return 0;
}

void test_online_clone__clone_mirror(void)
{
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	git_reference *head;

	bool fetch_progress_cb_was_called = false;

	opts.fetch_opts.callbacks.transfer_progress = &fetch_progress;
	opts.fetch_opts.callbacks.payload = &fetch_progress_cb_was_called;

	opts.bare = true;
	opts.remote_cb = remote_mirror_cb;

	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo.git", &opts));

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_assert_equal_i(GIT_REFERENCE_SYMBOLIC, git_reference_type(head));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));

	cl_assert_equal_i(true, fetch_progress_cb_was_called);

	git_reference_free(head);
	git_repository_free(g_repo);
	g_repo = NULL;

	cl_fixture_cleanup("./foo.git");
}

static int update_tips(const char *refname, const git_oid *a, const git_oid *b, void *payload)
{
	int *callcount = (int*)payload;
	GIT_UNUSED(refname); GIT_UNUSED(a); GIT_UNUSED(b);
	*callcount = *callcount + 1;
	return 0;
}

void test_online_clone__custom_remote_callbacks(void)
{
	int callcount = 0;

	g_options.fetch_opts.callbacks.update_tips = update_tips;
	g_options.fetch_opts.callbacks.payload = &callcount;

	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));
	cl_assert(callcount > 0);
}

void test_online_clone__custom_headers(void)
{
	char *empty_header = "";
	char *unnamed_header = "this is a header about nothing";
	char *newlines = "X-Custom: almost OK\n";
	char *conflict = "Accept: defined-by-git";
	char *ok = "X-Custom: this should be ok";

	g_options.fetch_opts.custom_headers.count = 1;

	g_options.fetch_opts.custom_headers.strings = &empty_header;
	cl_git_fail(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));

	g_options.fetch_opts.custom_headers.strings = &unnamed_header;
	cl_git_fail(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));

	g_options.fetch_opts.custom_headers.strings = &newlines;
	cl_git_fail(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));

	g_options.fetch_opts.custom_headers.strings = &conflict;
	cl_git_fail(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));

	/* Finally, we got it right! */
	g_options.fetch_opts.custom_headers.strings = &ok;
	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));
}

static int cred_failure_cb(
	git_cred **cred,
	const char *url,
	const char *username_from_url,
	unsigned int allowed_types,
	void *data)
{
	GIT_UNUSED(cred); GIT_UNUSED(url); GIT_UNUSED(username_from_url);
	GIT_UNUSED(allowed_types); GIT_UNUSED(data);
	return -172;
}

void test_online_clone__cred_callback_failure_return_code_is_tunnelled(void)
{
	git__free(_remote_url);
	git__free(_remote_user);

	_remote_url = git__strdup("https://github.com/libgit2/non-existent");
	_remote_user = git__strdup("libgit2test");

	g_options.fetch_opts.callbacks.credentials = cred_failure_cb;

	cl_git_fail_with(-172, git_clone(&g_repo, _remote_url, "./foo", &g_options));
}

static int cred_count_calls_cb(git_cred **cred, const char *url, const char *user,
			       unsigned int allowed_types, void *data)
{
	size_t *counter = (size_t *) data;

	GIT_UNUSED(url); GIT_UNUSED(user); GIT_UNUSED(allowed_types);

	if (allowed_types == GIT_CREDTYPE_USERNAME)
		return git_cred_username_new(cred, "foo");

	(*counter)++;

	if (*counter == 3)
		return GIT_EUSER;

	return git_cred_userpass_plaintext_new(cred, "foo", "bar");
}

void test_online_clone__cred_callback_called_again_on_auth_failure(void)
{
	size_t counter = 0;

	git__free(_remote_url);
	git__free(_remote_user);

	_remote_url = git__strdup("https://gitlab.com/libgit2/non-existent");
	_remote_user = git__strdup("libgit2test");

	g_options.fetch_opts.callbacks.credentials = cred_count_calls_cb;
	g_options.fetch_opts.callbacks.payload = &counter;

	cl_git_fail_with(GIT_EUSER, git_clone(&g_repo, _remote_url, "./foo", &g_options));
	cl_assert_equal_i(3, counter);
}

int cred_default(
	git_cred **cred,
	const char *url,
	const char *user_from_url,
	unsigned int allowed_types,
	void *payload)
{
	GIT_UNUSED(url);
	GIT_UNUSED(user_from_url);
	GIT_UNUSED(payload);

	if (!(allowed_types & GIT_CREDTYPE_DEFAULT))
		return 0;

	return git_cred_default_new(cred);
}

void test_online_clone__credentials(void)
{
	/* Remote URL environment variable must be set.
	 * User and password are optional.
	 */
	git_cred_userpass_payload user_pass = {
		_remote_user,
		_remote_pass
	};

	if (!_remote_url)
		clar__skip();

	if (cl_is_env_set("GITTEST_REMOTE_DEFAULT")) {
		g_options.fetch_opts.callbacks.credentials = cred_default;
	} else {
		g_options.fetch_opts.callbacks.credentials = git_cred_userpass;
		g_options.fetch_opts.callbacks.payload = &user_pass;
	}

	cl_git_pass(git_clone(&g_repo, _remote_url, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");
}

void test_online_clone__bitbucket_style(void)
{
	git_cred_userpass_payload user_pass = {
		"libgit3", "libgit3"
	};

	g_options.fetch_opts.callbacks.credentials = git_cred_userpass;
	g_options.fetch_opts.callbacks.payload = &user_pass;

	cl_git_pass(git_clone(&g_repo, BB_REPO_URL, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");
}

void test_online_clone__bitbucket_uses_creds_in_url(void)
{
	git_cred_userpass_payload user_pass = {
		"libgit2", "wrong"
	};

	g_options.fetch_opts.callbacks.credentials = git_cred_userpass;
	g_options.fetch_opts.callbacks.payload = &user_pass;

	/*
	 * Correct user and pass are in the URL; the (incorrect) creds in
	 * the `git_cred_userpass_payload` should be ignored.
	 */
	cl_git_pass(git_clone(&g_repo, BB_REPO_URL_WITH_PASS, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");
}

void test_online_clone__bitbucket_falls_back_to_specified_creds(void)
{
	git_cred_userpass_payload user_pass = {
		"libgit2", "libgit2"
	};

	g_options.fetch_opts.callbacks.credentials = git_cred_userpass;
	g_options.fetch_opts.callbacks.payload = &user_pass;

	/*
	 * TODO: as of March 2018, bitbucket sporadically fails with
	 * 403s instead of replying with a 401 - but only sometimes.
	 */
	cl_skip();

	/*
	 * Incorrect user and pass are in the URL; the (correct) creds in
	 * the `git_cred_userpass_payload` should be used as a fallback.
	 */
	cl_git_pass(git_clone(&g_repo, BB_REPO_URL_WITH_WRONG_PASS, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");
}

static int cancel_at_half(const git_indexer_progress *stats, void *payload)
{
	GIT_UNUSED(payload);

	if (stats->received_objects > (stats->total_objects/2))
		return 4321;
	return 0;
}

void test_online_clone__can_cancel(void)
{
	g_options.fetch_opts.callbacks.transfer_progress = cancel_at_half;

	cl_git_fail_with(
		git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options), 4321);
}

static int cred_cb(git_cred **cred, const char *url, const char *user_from_url,
		   unsigned int allowed_types, void *payload)
{
	GIT_UNUSED(url); GIT_UNUSED(user_from_url); GIT_UNUSED(payload);

	if (allowed_types & GIT_CREDTYPE_USERNAME)
		return git_cred_username_new(cred, _remote_user);

	if (allowed_types & GIT_CREDTYPE_SSH_KEY)
		return git_cred_ssh_key_new(cred,
			_remote_user, _remote_ssh_pubkey,
			_remote_ssh_privkey, _remote_ssh_passphrase);

	git_error_set(GIT_ERROR_NET, "unexpected cred type");
	return -1;
}

static int check_ssh_auth_methods(git_cred **cred, const char *url, const char *username_from_url,
				  unsigned int allowed_types, void *data)
{
	int *with_user = (int *) data;
	GIT_UNUSED(cred); GIT_UNUSED(url); GIT_UNUSED(username_from_url); GIT_UNUSED(data);

	if (!*with_user)
		cl_assert_equal_i(GIT_CREDTYPE_USERNAME, allowed_types);
	else
		cl_assert(!(allowed_types & GIT_CREDTYPE_USERNAME));

	return GIT_EUSER;
}

void test_online_clone__ssh_auth_methods(void)
{
	int with_user;

#ifndef GIT_SSH
	clar__skip();
#endif
	g_options.fetch_opts.callbacks.credentials = check_ssh_auth_methods;
	g_options.fetch_opts.callbacks.payload = &with_user;
	g_options.fetch_opts.callbacks.certificate_check = NULL;

	with_user = 0;
	cl_git_fail_with(GIT_EUSER,
		git_clone(&g_repo, SSH_REPO_URL, "./foo", &g_options));

	with_user = 1;
	cl_git_fail_with(GIT_EUSER,
		git_clone(&g_repo, "ssh://git@github.com/libgit2/TestGitRepository", "./foo", &g_options));
}

static int custom_remote_ssh_with_paths(
	git_remote **out,
	git_repository *repo,
	const char *name,
	const char *url,
	void *payload)
{
	int error;

	GIT_UNUSED(payload);

	if ((error = git_remote_create(out, repo, name, url)) < 0)
		return error;

	return 0;
}

void test_online_clone__ssh_with_paths(void)
{
	char *bad_paths[] = {
		"/bin/yes",
		"/bin/false",
	};
	char *good_paths[] = {
		"/usr/bin/git-upload-pack",
		"/usr/bin/git-receive-pack",
	};
	git_strarray arr = {
		bad_paths,
		2,
	};

#ifndef GIT_SSH
	clar__skip();
#endif
	if (!_remote_url || !_remote_user || strncmp(_remote_url, "ssh://", 5) != 0)
		clar__skip();

	g_options.remote_cb = custom_remote_ssh_with_paths;
	g_options.fetch_opts.callbacks.transport = git_transport_ssh_with_paths;
	g_options.fetch_opts.callbacks.credentials = cred_cb;
	g_options.fetch_opts.callbacks.payload = &arr;
	g_options.fetch_opts.callbacks.certificate_check = NULL;

	cl_git_fail(git_clone(&g_repo, _remote_url, "./foo", &g_options));

	arr.strings = good_paths;
	cl_git_pass(git_clone(&g_repo, _remote_url, "./foo", &g_options));
}

static int cred_foo_bar(git_cred **cred, const char *url, const char *username_from_url,
				  unsigned int allowed_types, void *data)

{
	GIT_UNUSED(url); GIT_UNUSED(username_from_url); GIT_UNUSED(allowed_types); GIT_UNUSED(data);

	return git_cred_userpass_plaintext_new(cred, "foo", "bar");
}

void test_online_clone__ssh_cannot_change_username(void)
{
#ifndef GIT_SSH
	clar__skip();
#endif
	g_options.fetch_opts.callbacks.credentials = cred_foo_bar;

	cl_git_fail(git_clone(&g_repo, "ssh://git@github.com/libgit2/TestGitRepository", "./foo", &g_options));
}

int ssh_certificate_check(git_cert *cert, int valid, const char *host, void *payload)
{
	git_cert_hostkey *key;
	git_oid expected = {{0}}, actual = {{0}};

	GIT_UNUSED(valid);
	GIT_UNUSED(payload);

	cl_assert(_remote_ssh_fingerprint);

	cl_git_pass(git_oid_fromstrp(&expected, _remote_ssh_fingerprint));
	cl_assert_equal_i(GIT_CERT_HOSTKEY_LIBSSH2, cert->cert_type);
	key = (git_cert_hostkey *) cert;

	/*
	 * We need to figure out how long our input was to check for
	 * the type. Here we abuse the fact that both hashes fit into
	 * our git_oid type.
	 */
	if (strlen(_remote_ssh_fingerprint) == 32 && key->type & GIT_CERT_SSH_MD5) {
		memcpy(&actual.id, key->hash_md5, 16);
	} else 	if (strlen(_remote_ssh_fingerprint) == 40 && key->type & GIT_CERT_SSH_SHA1) {
		memcpy(&actual, key->hash_sha1, 20);
	} else {
		cl_fail("Cannot find a usable SSH hash");
	}

	cl_assert(!memcmp(&expected, &actual, 20));

	cl_assert_equal_s("localhost", host);

	return GIT_EUSER;
}

void test_online_clone__ssh_cert(void)
{
	g_options.fetch_opts.callbacks.certificate_check = ssh_certificate_check;

	if (!_remote_ssh_fingerprint)
		cl_skip();

	cl_git_fail_with(GIT_EUSER, git_clone(&g_repo, _remote_url, "./foo", &g_options));
}

static char *read_key_file(const char *path)
{
	FILE *f;
	char *buf;
	long key_length;

	if (!path || !*path)
		return NULL;

	cl_assert((f = fopen(path, "r")) != NULL);
	cl_assert(fseek(f, 0, SEEK_END) != -1);
	cl_assert((key_length = ftell(f)) != -1);
	cl_assert(fseek(f, 0, SEEK_SET) != -1);
	cl_assert((buf = malloc(key_length)) != NULL);
	cl_assert(fread(buf, key_length, 1, f) == 1);
	fclose(f);

	return buf;
}

static int ssh_memory_cred_cb(git_cred **cred, const char *url, const char *user_from_url,
		   unsigned int allowed_types, void *payload)
{
	GIT_UNUSED(url); GIT_UNUSED(user_from_url); GIT_UNUSED(payload);

	if (allowed_types & GIT_CREDTYPE_USERNAME)
		return git_cred_username_new(cred, _remote_user);

	if (allowed_types & GIT_CREDTYPE_SSH_KEY)
	{
		char *pubkey = read_key_file(_remote_ssh_pubkey);
		char *privkey = read_key_file(_remote_ssh_privkey);

		int ret = git_cred_ssh_key_memory_new(cred, _remote_user, pubkey, privkey, _remote_ssh_passphrase);

		if (privkey)
			free(privkey);
		if (pubkey)
			free(pubkey);
		return ret;
	}

	git_error_set(GIT_ERROR_NET, "unexpected cred type");
	return -1;
}

void test_online_clone__ssh_memory_auth(void)
{
#ifndef GIT_SSH_MEMORY_CREDENTIALS
	clar__skip();
#endif
	if (!_remote_url || !_remote_user || !_remote_ssh_privkey || strncmp(_remote_url, "ssh://", 5) != 0)
		clar__skip();

	g_options.fetch_opts.callbacks.credentials = ssh_memory_cred_cb;

	cl_git_pass(git_clone(&g_repo, _remote_url, "./foo", &g_options));
}

static int fail_certificate_check(git_cert *cert, int valid, const char *host, void *payload)
{
	GIT_UNUSED(cert);
	GIT_UNUSED(valid);
	GIT_UNUSED(host);
	GIT_UNUSED(payload);

	return GIT_ECERTIFICATE;
}

void test_online_clone__certificate_invalid(void)
{
	g_options.fetch_opts.callbacks.certificate_check = fail_certificate_check;

	cl_git_fail_with(git_clone(&g_repo, "https://github.com/libgit2/TestGitRepository", "./foo", &g_options),
		GIT_ECERTIFICATE);

#ifdef GIT_SSH
	cl_git_fail_with(git_clone(&g_repo, "ssh://github.com/libgit2/TestGitRepository", "./foo", &g_options),
		GIT_ECERTIFICATE);
#endif
}

static int succeed_certificate_check(git_cert *cert, int valid, const char *host, void *payload)
{
	GIT_UNUSED(cert);
	GIT_UNUSED(valid);
	GIT_UNUSED(payload);

	cl_assert_equal_s("github.com", host);

	return 0;
}

void test_online_clone__certificate_valid(void)
{
	g_options.fetch_opts.callbacks.certificate_check = succeed_certificate_check;

	cl_git_pass(git_clone(&g_repo, "https://github.com/libgit2/TestGitRepository", "./foo", &g_options));
}

void test_online_clone__start_with_http(void)
{
	g_options.fetch_opts.callbacks.certificate_check = succeed_certificate_check;

	cl_git_pass(git_clone(&g_repo, "http://github.com/libgit2/TestGitRepository", "./foo", &g_options));
}

static int called_proxy_creds;
static int proxy_cred_cb(git_cred **out, const char *url, const char *username, unsigned int allowed, void *payload)
{
	GIT_UNUSED(url);
	GIT_UNUSED(username);
	GIT_UNUSED(allowed);
	GIT_UNUSED(payload);

	called_proxy_creds = 1;
	return git_cred_userpass_plaintext_new(out, _remote_proxy_user, _remote_proxy_pass);
}

static int proxy_cert_cb(git_cert *cert, int valid, const char *host, void *payload)
{
	char *colon;
	size_t host_len;

	GIT_UNUSED(cert);
	GIT_UNUSED(valid);
	GIT_UNUSED(payload);

	cl_assert(_remote_proxy_host);

	if ((colon = strchr(_remote_proxy_host, ':')) != NULL)
		host_len = (colon - _remote_proxy_host);
	else
		host_len = strlen(_remote_proxy_host);

	if (_remote_proxy_selfsigned != NULL &&
	    strlen(host) == host_len &&
	    strncmp(_remote_proxy_host, host, host_len) == 0)
		valid = 1;

	return valid ? 0 : GIT_ECERTIFICATE;
}

void test_online_clone__proxy_credentials_request(void)
{
	git_buf url = GIT_BUF_INIT;

	if (!_remote_proxy_host || !_remote_proxy_user || !_remote_proxy_pass)
		cl_skip();

	cl_git_pass(git_buf_printf(&url, "%s://%s/",
		_remote_proxy_scheme ? _remote_proxy_scheme : "http",
		_remote_proxy_host));

	g_options.fetch_opts.proxy_opts.type = GIT_PROXY_SPECIFIED;
	g_options.fetch_opts.proxy_opts.url = url.ptr;
	g_options.fetch_opts.proxy_opts.credentials = proxy_cred_cb;
	g_options.fetch_opts.proxy_opts.certificate_check = proxy_cert_cb;
	called_proxy_creds = 0;
	cl_git_pass(git_clone(&g_repo, "http://github.com/libgit2/TestGitRepository", "./foo", &g_options));
	cl_assert(called_proxy_creds);

	git_buf_dispose(&url);
}

void test_online_clone__proxy_credentials_in_url(void)
{
	git_buf url = GIT_BUF_INIT;

	if (!_remote_proxy_host || !_remote_proxy_user || !_remote_proxy_pass)
		cl_skip();

	cl_git_pass(git_buf_printf(&url, "%s://%s:%s@%s/",
		_remote_proxy_scheme ? _remote_proxy_scheme : "http",
		_remote_proxy_user, _remote_proxy_pass, _remote_proxy_host));

	g_options.fetch_opts.proxy_opts.type = GIT_PROXY_SPECIFIED;
	g_options.fetch_opts.proxy_opts.url = url.ptr;
	g_options.fetch_opts.proxy_opts.certificate_check = proxy_cert_cb;
	called_proxy_creds = 0;
	cl_git_pass(git_clone(&g_repo, "http://github.com/libgit2/TestGitRepository", "./foo", &g_options));
	cl_assert(called_proxy_creds == 0);

	git_buf_dispose(&url);
}

void test_online_clone__proxy_credentials_in_environment(void)
{
	git_buf url = GIT_BUF_INIT;

	if (!_remote_proxy_host || !_remote_proxy_user || !_remote_proxy_pass)
		cl_skip();

	_orig_http_proxy = cl_getenv("HTTP_PROXY");
	_orig_https_proxy = cl_getenv("HTTPS_PROXY");
	_orig_proxies_need_reset = 1;

	g_options.fetch_opts.proxy_opts.type = GIT_PROXY_AUTO;
	g_options.fetch_opts.proxy_opts.certificate_check = proxy_cert_cb;

	cl_git_pass(git_buf_printf(&url, "%s://%s:%s@%s/",
		_remote_proxy_scheme ? _remote_proxy_scheme : "http",
		_remote_proxy_user, _remote_proxy_pass, _remote_proxy_host));

	cl_setenv("HTTP_PROXY", url.ptr);
	cl_setenv("HTTPS_PROXY", url.ptr);

	cl_git_pass(git_clone(&g_repo, "http://github.com/libgit2/TestGitRepository", "./foo", &g_options));

	git_buf_dispose(&url);
}

void test_online_clone__proxy_auto_not_detected(void)
{
	g_options.fetch_opts.proxy_opts.type = GIT_PROXY_AUTO;

	cl_git_pass(git_clone(&g_repo, "http://github.com/libgit2/TestGitRepository", "./foo", &g_options));
}

void test_online_clone__proxy_cred_callback_after_failed_url_creds(void)
{
	git_buf url = GIT_BUF_INIT;

	if (!_remote_proxy_host || !_remote_proxy_user || !_remote_proxy_pass)
		cl_skip();

	cl_git_pass(git_buf_printf(&url, "%s://invalid_user_name:INVALID_pass_WORD@%s/",
		_remote_proxy_scheme ? _remote_proxy_scheme : "http",
		_remote_proxy_host));

	g_options.fetch_opts.proxy_opts.type = GIT_PROXY_SPECIFIED;
	g_options.fetch_opts.proxy_opts.url = url.ptr;
	g_options.fetch_opts.proxy_opts.credentials = proxy_cred_cb;
	g_options.fetch_opts.proxy_opts.certificate_check = proxy_cert_cb;
	called_proxy_creds = 0;
	cl_git_pass(git_clone(&g_repo, "http://github.com/libgit2/TestGitRepository", "./foo", &g_options));
	cl_assert(called_proxy_creds);

	git_buf_dispose(&url);
}

void test_online_clone__azurerepos(void)
{
	cl_git_pass(git_clone(&g_repo, "https://libgit2@dev.azure.com/libgit2/test/_git/test", "./foo", &g_options));
	cl_assert(git_path_exists("./foo/master.txt"));
}

void test_online_clone__path_whitespace(void)
{
	cl_git_pass(git_clone(&g_repo, "https://libgit2@dev.azure.com/libgit2/test/_git/spaces%20in%20the%20name", "./foo", &g_options));
	cl_assert(git_path_exists("./foo/master.txt"));
}
