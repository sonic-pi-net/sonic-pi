#include "clar_libgit2.h"

#include "git2/clone.h"
#include "git2/cred_helpers.h"
#include "remote.h"
#include "futils.h"
#include "refs.h"

#define LIVE_REPO_URL "http://github.com/libgit2/TestGitRepository"
#define LIVE_EMPTYREPO_URL "http://github.com/libgit2/TestEmptyRepository"
#define BB_REPO_URL "https://libgit2-test@bitbucket.org/libgit2-test/testgitrepository.git"
#define BB_REPO_URL_WITH_PASS "https://libgit2-test:YT77Ppm2nq8w4TYjGS8U@bitbucket.org/libgit2-test/testgitrepository.git"
#define BB_REPO_URL_WITH_WRONG_PASS "https://libgit2-test:wrong@bitbucket.org/libgit2-test/testgitrepository.git"
#define GOOGLESOURCE_REPO_URL "https://chromium.googlesource.com/external/github.com/sergi/go-diff"

#define SSH_REPO_URL "ssh://github.com/libgit2/TestGitRepository"

static git_repository *g_repo;
static git_clone_options g_options;

static char *_remote_url = NULL;
static char *_remote_user = NULL;
static char *_remote_pass = NULL;
static char *_remote_branch = NULL;
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
static char *_remote_expectcontinue = NULL;
static char *_remote_redirect_initial = NULL;
static char *_remote_redirect_subsequent = NULL;

static char *_github_ssh_pubkey = NULL;
static char *_github_ssh_privkey = NULL;
static char *_github_ssh_passphrase = NULL;
static char *_github_ssh_remotehostkey = NULL;

static int _orig_proxies_need_reset = 0;
static char *_orig_http_proxy = NULL;
static char *_orig_https_proxy = NULL;
static char *_orig_no_proxy = NULL;

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
	_remote_branch = cl_getenv("GITTEST_REMOTE_BRANCH");
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
	_remote_expectcontinue = cl_getenv("GITTEST_REMOTE_EXPECTCONTINUE");
	_remote_redirect_initial = cl_getenv("GITTEST_REMOTE_REDIRECT_INITIAL");
	_remote_redirect_subsequent = cl_getenv("GITTEST_REMOTE_REDIRECT_SUBSEQUENT");

	_github_ssh_pubkey = cl_getenv("GITTEST_GITHUB_SSH_PUBKEY");
	_github_ssh_privkey = cl_getenv("GITTEST_GITHUB_SSH_KEY");
	_github_ssh_passphrase = cl_getenv("GITTEST_GITHUB_SSH_PASSPHRASE");
	_github_ssh_remotehostkey = cl_getenv("GITTEST_GITHUB_SSH_REMOTE_HOSTKEY");

	if (_remote_expectcontinue)
		git_libgit2_opts(GIT_OPT_ENABLE_HTTP_EXPECT_CONTINUE, 1);

	_orig_proxies_need_reset = 0;
}

void test_online_clone__cleanup(void)
{
	if (g_repo) {
		git_repository_free(g_repo);
		g_repo = NULL;
	}
	cl_fixture_cleanup("./foo");
	cl_fixture_cleanup("./initial");
	cl_fixture_cleanup("./subsequent");

	git__free(_remote_url);
	git__free(_remote_user);
	git__free(_remote_pass);
	git__free(_remote_branch);
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
	git__free(_remote_expectcontinue);
	git__free(_remote_redirect_initial);
	git__free(_remote_redirect_subsequent);

	git__free(_github_ssh_pubkey);
	git__free(_github_ssh_privkey);
	git__free(_github_ssh_passphrase);
	git__free(_github_ssh_remotehostkey);

	if (_orig_proxies_need_reset) {
		cl_setenv("HTTP_PROXY", _orig_http_proxy);
		cl_setenv("HTTPS_PROXY", _orig_https_proxy);
		cl_setenv("NO_PROXY", _orig_no_proxy);

		git__free(_orig_http_proxy);
		git__free(_orig_https_proxy);
		git__free(_orig_no_proxy);
	}

	git_libgit2_opts(GIT_OPT_SET_SSL_CERT_LOCATIONS, NULL, NULL);
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
	git_str path = GIT_STR_INIT;
	git_reference *head, *remote_head;
	bool checkout_progress_cb_was_called = false,
		  fetch_progress_cb_was_called = false;

	g_options.checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;
	g_options.checkout_opts.progress_cb = &checkout_progress;
	g_options.checkout_opts.progress_payload = &checkout_progress_cb_was_called;
	g_options.fetch_opts.callbacks.transfer_progress = &fetch_progress;
	g_options.fetch_opts.callbacks.payload = &fetch_progress_cb_was_called;

	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));

	cl_git_pass(git_str_joinpath(&path, git_repository_workdir(g_repo), "master.txt"));
	cl_assert_equal_i(true, git_fs_path_isfile(git_str_cstr(&path)));

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_assert_equal_i(GIT_REFERENCE_SYMBOLIC, git_reference_type(head));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));

	cl_git_pass(git_reference_lookup(&remote_head, g_repo, "refs/remotes/origin/HEAD"));
	cl_assert_equal_i(GIT_REFERENCE_SYMBOLIC, git_reference_type(remote_head));
	cl_assert_equal_s("refs/remotes/origin/master", git_reference_symbolic_target(remote_head));

	cl_assert_equal_i(true, checkout_progress_cb_was_called);
	cl_assert_equal_i(true, fetch_progress_cb_was_called);

	git_reference_free(remote_head);
	git_reference_free(head);
	git_str_dispose(&path);
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

void test_online_clone__long_custom_header(void)
{
	/* Long custom header with 1500 characters */
	char *ok = "X-Custom: a0MsqH2bXV9lILn7zkAHqKpGrOVvkik7SfoyqfXbFTxccsymN5SG9hEB0RLD9koTXKWtaI1vI9jHf5ViwLHq6xvkveFX9GiqaIhe3TRu5KDZrOBgeufdBYsTTONALPlpni9XVq71bR6x3AlVEqHdXi9qiq0TRuNiujMy0ZKs8LQkQVSE8kxWZXqLsO2IJtAPw5aqsUEenK5ec12GOeOTOYlSChGllzvl2Ow4SKlVg3t8NHVWvc8HyPGmBQ79l3qUMU30P0hnUXaIrhIzGgleYWnwhGFLpryxsQfCdwkdBMuvtLH0DnkhLoAkCmnCZItEExtHBOCirEzztoFMX3lH4lM4wMqePCU8II0qloNvzPgt6cBThQJP66FYUDSCwsSb63bcTWdVx7TCa6mAplkP49PKi5pFSvFKKbs5se5MPcBVG03GiatKszIQkii0vp6OV5b54Aym4N8hQJHFMhIChKiQM91tB7PQu9vPJE6h2bzAnQsn34bBPFZHT7pBplqkASiHDjw69YV6k3M8ffTOTr2ibQnTKxh1NH3ZRx6u0KxRty9i4YLMniZUZAfFgqbSW2xXk49e8J9VNFm7j2bgHp3t813wUzqnQL4NEc0CQlF0e6pId5ADXikoH6S7aMfuYUYi1Kn1i9m7UGtaB0U7dVC65uH9vIWKnyAcmBt0mN1aikRnjz7oBKjD65SRZrKWXeCDJkpgWlXnD5JjekDCyB9m3yGkaxy1FflI1kaa4kcVbPRfs6XebHRDl9golPBUyazRG1V1iOi1mKki9ClUNO8wviNfKm5eMbWW6hU8wMXh388EotRA73TUdL4JIfNpkC4XBFLNFbFtltzO34kxXBKvhj8t0XVZOp4AWpHEL3pUtuyKhNWaWlDF6ZhjCeO8vT1akKoYaA7t6nFyqawq5nPoB0iXEHQ7YugfYfgjzpNGLgvPJ6aLg9YIKZBqfi7J9xWb356IJvTQFswi7qm6Mu7IVXarS9m84b5IfT6UCVq84u4VcdBlDswNPTw6SbBtzg9vrLLs3MoTCzJY6fHPqnKt6YthgQwOOB1ig7GTSDiX3W3SMeaz5jTASociHrUS3HrwVSgjrODnF86962cv4s3DGYjiX2cIuNfq9mZVJlNsylZjFYFV9LzOjNLlSHZVJrrGQJLjmyOCwOMkG9u2xKdSvfjxTJzqhjhTvQSQZWhKt44hA9EidUqPqjc3MhfnZ6aeAIP232gtRHoRc7FdjRSan4Q3PWy02YiRodvKAafonwCOtMcm4MASrXBiBE1tibHLSTtK4UrodFNVhymtBCRnJdVRSgrCQcr2B5Jzs4Iv6uJlJqwwyuq6In54zcmecgJZezta84B3eFoSGJhCbI6Zza0khulccglCcppciWDStAHFhncePsCQL4tup0Z8fS01RksRQ7X1xgskVvQAKELThDqbJB4FJZwrwPXOpCweAoSONntp7Ly0lAUabw75gK5sR387IxNVdISmfP";

	g_options.fetch_opts.custom_headers.count = 1;
	g_options.fetch_opts.custom_headers.strings = &ok;
	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));
}

static int cred_failure_cb(
	git_credential **cred,
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

static int cred_count_calls_cb(git_credential **cred, const char *url, const char *user,
			       unsigned int allowed_types, void *data)
{
	size_t *counter = (size_t *) data;

	GIT_UNUSED(url); GIT_UNUSED(user); GIT_UNUSED(allowed_types);

	if (allowed_types == GIT_CREDENTIAL_USERNAME)
		return git_credential_username_new(cred, "foo");

	(*counter)++;

	if (*counter == 3)
		return GIT_EUSER;

	return git_credential_userpass_plaintext_new(cred, "foo", "bar");
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

static int cred_default(
	git_credential **cred,
	const char *url,
	const char *user_from_url,
	unsigned int allowed_types,
	void *payload)
{
	GIT_UNUSED(url);
	GIT_UNUSED(user_from_url);
	GIT_UNUSED(payload);

	if (!(allowed_types & GIT_CREDENTIAL_DEFAULT))
		return 0;

	return git_credential_default_new(cred);
}

void test_online_clone__credentials(void)
{
	/* Remote URL environment variable must be set.
	 * User and password are optional.
	 */
	git_credential_userpass_payload user_pass = {
		_remote_user,
		_remote_pass
	};

	if (!_remote_url)
		clar__skip();

	if (cl_is_env_set("GITTEST_REMOTE_DEFAULT")) {
		g_options.fetch_opts.callbacks.credentials = cred_default;
	} else {
		g_options.fetch_opts.callbacks.credentials = git_credential_userpass;
		g_options.fetch_opts.callbacks.payload = &user_pass;
	}

	cl_git_pass(git_clone(&g_repo, _remote_url, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");
}

void test_online_clone__credentials_via_custom_headers(void)
{
	const char *creds = "libgit2-test:YT77Ppm2nq8w4TYjGS8U";
	git_str auth = GIT_STR_INIT;

	cl_git_pass(git_str_puts(&auth, "Authorization: Basic "));
	cl_git_pass(git_str_encode_base64(&auth, creds, strlen(creds)));
	g_options.fetch_opts.custom_headers.count = 1;
	g_options.fetch_opts.custom_headers.strings = &auth.ptr;

	cl_git_pass(git_clone(&g_repo, "https://bitbucket.org/libgit2-test/testgitrepository.git", "./foo", &g_options));

	git_str_dispose(&auth);
}

void test_online_clone__bitbucket_style(void)
{
	git_credential_userpass_payload user_pass = {
		"libgit2-test", "YT77Ppm2nq8w4TYjGS8U"
	};

	g_options.fetch_opts.callbacks.credentials = git_credential_userpass;
	g_options.fetch_opts.callbacks.payload = &user_pass;

	cl_git_pass(git_clone(&g_repo, BB_REPO_URL, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");
}

void test_online_clone__bitbucket_uses_creds_in_url(void)
{
	git_credential_userpass_payload user_pass = {
		"libgit2-test", "wrong"
	};

	g_options.fetch_opts.callbacks.credentials = git_credential_userpass;
	g_options.fetch_opts.callbacks.payload = &user_pass;

	/*
	 * Correct user and pass are in the URL; the (incorrect) creds in
	 * the `git_credential_userpass_payload` should be ignored.
	 */
	cl_git_pass(git_clone(&g_repo, BB_REPO_URL_WITH_PASS, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");
}

void test_online_clone__bitbucket_falls_back_to_specified_creds(void)
{
	git_credential_userpass_payload user_pass = {
		"libgit2-test", "libgit2"
	};

	g_options.fetch_opts.callbacks.credentials = git_credential_userpass;
	g_options.fetch_opts.callbacks.payload = &user_pass;

	/*
	 * TODO: as of March 2018, bitbucket sporadically fails with
	 * 403s instead of replying with a 401 - but only sometimes.
	 */
	cl_skip();

	/*
	 * Incorrect user and pass are in the URL; the (correct) creds in
	 * the `git_credential_userpass_payload` should be used as a fallback.
	 */
	cl_git_pass(git_clone(&g_repo, BB_REPO_URL_WITH_WRONG_PASS, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");
}

void test_online_clone__googlesource(void)
{
#ifdef __APPLE__
	cl_skip();
#else
	cl_git_pass(git_clone(&g_repo, GOOGLESOURCE_REPO_URL, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");
#endif
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

	cl_git_fail_with(4321,
		git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));
}

static int cred_cb(git_credential **cred, const char *url, const char *user_from_url,
		   unsigned int allowed_types, void *payload)
{
	GIT_UNUSED(url); GIT_UNUSED(user_from_url); GIT_UNUSED(payload);

	if (allowed_types & GIT_CREDENTIAL_USERNAME)
		return git_credential_username_new(cred, _remote_user);

	if (allowed_types & GIT_CREDENTIAL_SSH_KEY)
		return git_credential_ssh_key_new(cred,
			_remote_user, _remote_ssh_pubkey,
			_remote_ssh_privkey, _remote_ssh_passphrase);

	git_error_set(GIT_ERROR_NET, "unexpected cred type");
	return -1;
}

static int check_ssh_auth_methods(git_credential **cred, const char *url, const char *username_from_url,
				  unsigned int allowed_types, void *data)
{
	int *with_user = (int *) data;
	GIT_UNUSED(cred); GIT_UNUSED(url); GIT_UNUSED(username_from_url); GIT_UNUSED(data);

	if (!*with_user)
		cl_assert_equal_i(GIT_CREDENTIAL_USERNAME, allowed_types);
	else
		cl_assert(!(allowed_types & GIT_CREDENTIAL_USERNAME));

	return GIT_EUSER;
}

static int succeed_certificate_check(git_cert *cert, int valid, const char *host, void *payload)
{
	GIT_UNUSED(cert);
	GIT_UNUSED(valid);
	GIT_UNUSED(payload);

	cl_assert_equal_s("github.com", host);

	return 0;
}

static int fail_certificate_check(git_cert *cert, int valid, const char *host, void *payload)
{
	GIT_UNUSED(cert);
	GIT_UNUSED(valid);
	GIT_UNUSED(host);
	GIT_UNUSED(payload);

	return GIT_ECERTIFICATE;
}

static int github_credentials(
	git_credential **cred,
	const char *url,
	const char *username_from_url,
	unsigned int allowed_types,
	void *data)
{
	GIT_UNUSED(url);
	GIT_UNUSED(username_from_url);
	GIT_UNUSED(data);

	if ((allowed_types & GIT_CREDENTIAL_USERNAME) != 0) {
		return git_credential_username_new(cred, "git");
	}

	cl_assert((allowed_types & GIT_CREDENTIAL_SSH_KEY) != 0);

	return git_credential_ssh_key_memory_new(cred,
		"git",
		_github_ssh_pubkey,
		_github_ssh_privkey,
		_github_ssh_passphrase);
}

void test_online_clone__ssh_github(void)
{
#if !defined(GIT_SSH) || !defined(GIT_SSH_MEMORY_CREDENTIALS)
	clar__skip();
#endif

	if (!_github_ssh_pubkey || !_github_ssh_privkey)
		clar__skip();

	cl_fake_homedir(NULL);

	g_options.fetch_opts.callbacks.credentials = github_credentials;
	g_options.fetch_opts.callbacks.certificate_check = succeed_certificate_check;

	cl_git_pass(git_clone(&g_repo, SSH_REPO_URL, "./foo", &g_options));
}

void test_online_clone__ssh_auth_methods(void)
{
	int with_user;

#ifndef GIT_SSH
	clar__skip();
#endif
	g_options.fetch_opts.callbacks.credentials = check_ssh_auth_methods;
	g_options.fetch_opts.callbacks.payload = &with_user;
	g_options.fetch_opts.callbacks.certificate_check = succeed_certificate_check;

	with_user = 0;
	cl_git_fail_with(GIT_EUSER,
		git_clone(&g_repo, SSH_REPO_URL, "./foo", &g_options));

	with_user = 1;
	cl_git_fail_with(GIT_EUSER,
		git_clone(&g_repo, "ssh://git@github.com/libgit2/TestGitRepository", "./foo", &g_options));
}

/*
 * Ensure that the certificate check callback is still called, and
 * can accept a host key that is not in the known hosts file.
 */
void test_online_clone__ssh_certcheck_accepts_unknown(void)
{
#if !defined(GIT_SSH) || !defined(GIT_SSH_MEMORY_CREDENTIALS)
	clar__skip();
#endif

	if (!_github_ssh_pubkey || !_github_ssh_privkey)
		clar__skip();

	cl_fake_homedir(NULL);

	g_options.fetch_opts.callbacks.credentials = github_credentials;

	/* Ensure we fail without the certificate check */
	cl_git_fail_with(GIT_ECERTIFICATE,
		git_clone(&g_repo, SSH_REPO_URL, "./foo", NULL));

	/* Set the callback to accept the certificate */
	g_options.fetch_opts.callbacks.certificate_check = succeed_certificate_check;

	cl_git_pass(git_clone(&g_repo, SSH_REPO_URL, "./foo", &g_options));
}

/*
 * Ensure that the known hosts file is read and the certificate check
 * callback is still called after that.
 */
void test_online_clone__ssh_certcheck_override_knownhosts(void)
{
	git_str knownhostsfile = GIT_STR_INIT;

#if !defined(GIT_SSH) || !defined(GIT_SSH_MEMORY_CREDENTIALS)
	clar__skip();
#endif

	if (!_github_ssh_pubkey || !_github_ssh_privkey || !_github_ssh_remotehostkey)
		clar__skip();

	g_options.fetch_opts.callbacks.credentials = github_credentials;

	cl_fake_homedir(&knownhostsfile);
	cl_git_pass(git_str_joinpath(&knownhostsfile, knownhostsfile.ptr, ".ssh"));
	cl_git_pass(p_mkdir(knownhostsfile.ptr, 0777));

	cl_git_pass(git_str_joinpath(&knownhostsfile, knownhostsfile.ptr, "known_hosts"));
	cl_git_rewritefile(knownhostsfile.ptr, _github_ssh_remotehostkey);

	/* Ensure we succeed without the certificate check */
	cl_git_pass(git_clone(&g_repo, SSH_REPO_URL, "./foo", &g_options));
	git_repository_free(g_repo);
	g_repo = NULL;

	/* Set the callback to reject the certificate */
	g_options.fetch_opts.callbacks.certificate_check = fail_certificate_check;
	cl_git_fail_with(GIT_ECERTIFICATE, git_clone(&g_repo, SSH_REPO_URL, "./bar", &g_options));

	git_str_dispose(&knownhostsfile);
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

static int cred_foo_bar(git_credential **cred, const char *url, const char *username_from_url,
				  unsigned int allowed_types, void *data)

{
	GIT_UNUSED(url); GIT_UNUSED(username_from_url); GIT_UNUSED(allowed_types); GIT_UNUSED(data);

	return git_credential_userpass_plaintext_new(cred, "foo", "bar");
}

void test_online_clone__ssh_cannot_change_username(void)
{
#ifndef GIT_SSH
	clar__skip();
#endif
	g_options.fetch_opts.callbacks.credentials = cred_foo_bar;

	cl_git_fail(git_clone(&g_repo, "ssh://git@github.com/libgit2/TestGitRepository", "./foo", &g_options));
}

static int ssh_certificate_check(git_cert *cert, int valid, const char *host, void *payload)
{
	git_cert_hostkey *key;
	git_oid expected = GIT_OID_SHA1_ZERO, actual = GIT_OID_SHA1_ZERO;

	GIT_UNUSED(valid);
	GIT_UNUSED(payload);

	cl_assert(_remote_ssh_fingerprint);

	cl_git_pass(git_oid__fromstrp(&expected, _remote_ssh_fingerprint, GIT_OID_SHA1));
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

static int ssh_memory_cred_cb(git_credential **cred, const char *url, const char *user_from_url,
		   unsigned int allowed_types, void *payload)
{
	GIT_UNUSED(url); GIT_UNUSED(user_from_url); GIT_UNUSED(payload);

	if (allowed_types & GIT_CREDENTIAL_USERNAME)
		return git_credential_username_new(cred, _remote_user);

	if (allowed_types & GIT_CREDENTIAL_SSH_KEY)
	{
		char *pubkey = read_key_file(_remote_ssh_pubkey);
		char *privkey = read_key_file(_remote_ssh_privkey);

		int ret = git_credential_ssh_key_memory_new(cred, _remote_user, pubkey, privkey, _remote_ssh_passphrase);

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
static int proxy_cred_cb(git_credential **out, const char *url, const char *username, unsigned int allowed, void *payload)
{
	GIT_UNUSED(url);
	GIT_UNUSED(username);
	GIT_UNUSED(allowed);
	GIT_UNUSED(payload);

	called_proxy_creds = 1;
	return git_credential_userpass_plaintext_new(out, _remote_proxy_user, _remote_proxy_pass);
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
	git_str url = GIT_STR_INIT;

	if (!_remote_proxy_host || !_remote_proxy_user || !_remote_proxy_pass)
		cl_skip();

	cl_git_pass(git_str_printf(&url, "%s://%s/",
		_remote_proxy_scheme ? _remote_proxy_scheme : "http",
		_remote_proxy_host));

	g_options.fetch_opts.proxy_opts.type = GIT_PROXY_SPECIFIED;
	g_options.fetch_opts.proxy_opts.url = url.ptr;
	g_options.fetch_opts.proxy_opts.credentials = proxy_cred_cb;
	g_options.fetch_opts.proxy_opts.certificate_check = proxy_cert_cb;
	called_proxy_creds = 0;
	cl_git_pass(git_clone(&g_repo, "http://github.com/libgit2/TestGitRepository", "./foo", &g_options));
	cl_assert(called_proxy_creds);

	git_str_dispose(&url);
}

void test_online_clone__proxy_credentials_in_url(void)
{
	git_str url = GIT_STR_INIT;

	if (!_remote_proxy_host || !_remote_proxy_user || !_remote_proxy_pass)
		cl_skip();

	cl_git_pass(git_str_printf(&url, "%s://%s:%s@%s/",
		_remote_proxy_scheme ? _remote_proxy_scheme : "http",
		_remote_proxy_user, _remote_proxy_pass, _remote_proxy_host));

	g_options.fetch_opts.proxy_opts.type = GIT_PROXY_SPECIFIED;
	g_options.fetch_opts.proxy_opts.url = url.ptr;
	g_options.fetch_opts.proxy_opts.certificate_check = proxy_cert_cb;
	called_proxy_creds = 0;
	cl_git_pass(git_clone(&g_repo, "http://github.com/libgit2/TestGitRepository", "./foo", &g_options));
	cl_assert(called_proxy_creds == 0);

	git_str_dispose(&url);
}

void test_online_clone__proxy_credentials_in_environment(void)
{
	git_str url = GIT_STR_INIT;

	if (!_remote_proxy_host || !_remote_proxy_user || !_remote_proxy_pass)
		cl_skip();

	_orig_http_proxy = cl_getenv("HTTP_PROXY");
	_orig_https_proxy = cl_getenv("HTTPS_PROXY");
	_orig_no_proxy = cl_getenv("NO_PROXY");
	_orig_proxies_need_reset = 1;

	g_options.fetch_opts.proxy_opts.type = GIT_PROXY_AUTO;
	g_options.fetch_opts.proxy_opts.certificate_check = proxy_cert_cb;

	cl_git_pass(git_str_printf(&url, "%s://%s:%s@%s/",
		_remote_proxy_scheme ? _remote_proxy_scheme : "http",
		_remote_proxy_user, _remote_proxy_pass, _remote_proxy_host));

	cl_setenv("HTTP_PROXY", url.ptr);
	cl_setenv("HTTPS_PROXY", url.ptr);
	cl_setenv("NO_PROXY", NULL);

	cl_git_pass(git_clone(&g_repo, "http://github.com/libgit2/TestGitRepository", "./foo", &g_options));

	git_str_dispose(&url);
}

void test_online_clone__proxy_credentials_in_url_https(void)
{
	git_str url = GIT_STR_INIT;

	if (!_remote_proxy_host || !_remote_proxy_user || !_remote_proxy_pass)
		cl_skip();

	cl_git_pass(git_str_printf(&url, "%s://%s:%s@%s/",
		_remote_proxy_scheme ? _remote_proxy_scheme : "http",
		_remote_proxy_user, _remote_proxy_pass, _remote_proxy_host));

	g_options.fetch_opts.proxy_opts.type = GIT_PROXY_SPECIFIED;
	g_options.fetch_opts.proxy_opts.url = url.ptr;
	g_options.fetch_opts.proxy_opts.certificate_check = proxy_cert_cb;
	g_options.fetch_opts.callbacks.certificate_check = ssl_cert;
	called_proxy_creds = 0;
	cl_git_pass(git_clone(&g_repo, "https://github.com/libgit2/TestGitRepository", "./foo", &g_options));
	cl_assert(called_proxy_creds == 0);

	git_str_dispose(&url);
}

void test_online_clone__proxy_auto_not_detected(void)
{
	g_options.fetch_opts.proxy_opts.type = GIT_PROXY_AUTO;

	cl_git_pass(git_clone(&g_repo, "http://github.com/libgit2/TestGitRepository", "./foo", &g_options));
}

void test_online_clone__proxy_cred_callback_after_failed_url_creds(void)
{
	git_str url = GIT_STR_INIT;

	if (!_remote_proxy_host || !_remote_proxy_user || !_remote_proxy_pass)
		cl_skip();

	cl_git_pass(git_str_printf(&url, "%s://invalid_user_name:INVALID_pass_WORD@%s/",
		_remote_proxy_scheme ? _remote_proxy_scheme : "http",
		_remote_proxy_host));

	g_options.fetch_opts.proxy_opts.type = GIT_PROXY_SPECIFIED;
	g_options.fetch_opts.proxy_opts.url = url.ptr;
	g_options.fetch_opts.proxy_opts.credentials = proxy_cred_cb;
	g_options.fetch_opts.proxy_opts.certificate_check = proxy_cert_cb;
	called_proxy_creds = 0;
	cl_git_pass(git_clone(&g_repo, "http://github.com/libgit2/TestGitRepository", "./foo", &g_options));
	cl_assert(called_proxy_creds);

	git_str_dispose(&url);
}

void test_online_clone__azurerepos(void)
{
	cl_git_pass(git_clone(&g_repo, "https://libgit2@dev.azure.com/libgit2/test/_git/test", "./foo", &g_options));
	cl_assert(git_fs_path_exists("./foo/master.txt"));
}

void test_online_clone__path_whitespace(void)
{
	cl_git_pass(git_clone(&g_repo, "https://libgit2@dev.azure.com/libgit2/test/_git/spaces%20in%20the%20name", "./foo", &g_options));
	cl_assert(git_fs_path_exists("./foo/master.txt"));
}

void test_online_clone__redirect_default_succeeds_for_initial(void)
{
	git_clone_options options = GIT_CLONE_OPTIONS_INIT;

	if (!_remote_redirect_initial || !_remote_redirect_subsequent)
		cl_skip();

	cl_git_pass(git_clone(&g_repo, _remote_redirect_initial, "./initial", &options));
}

void test_online_clone__redirect_default_fails_for_subsequent(void)
{
	git_clone_options options = GIT_CLONE_OPTIONS_INIT;

	if (!_remote_redirect_initial || !_remote_redirect_subsequent)
		cl_skip();

	cl_git_fail(git_clone(&g_repo, _remote_redirect_subsequent, "./fail", &options));
}

void test_online_clone__redirect_none(void)
{
	git_clone_options options = GIT_CLONE_OPTIONS_INIT;

	if (!_remote_redirect_initial)
		cl_skip();

	options.fetch_opts.follow_redirects = GIT_REMOTE_REDIRECT_NONE;

	cl_git_fail(git_clone(&g_repo, _remote_redirect_initial, "./fail", &options));
}

void test_online_clone__redirect_initial_succeeds_for_initial(void)
{
	git_clone_options options = GIT_CLONE_OPTIONS_INIT;

	if (!_remote_redirect_initial || !_remote_redirect_subsequent)
		cl_skip();

	options.fetch_opts.follow_redirects = GIT_REMOTE_REDIRECT_INITIAL;

	cl_git_pass(git_clone(&g_repo, _remote_redirect_initial, "./initial", &options));
}

void test_online_clone__redirect_initial_fails_for_subsequent(void)
{
	git_clone_options options = GIT_CLONE_OPTIONS_INIT;

	if (!_remote_redirect_initial || !_remote_redirect_subsequent)
		cl_skip();

	options.fetch_opts.follow_redirects = GIT_REMOTE_REDIRECT_INITIAL;

	cl_git_fail(git_clone(&g_repo, _remote_redirect_subsequent, "./fail", &options));
}

void test_online_clone__namespace_bare(void)
{
	git_clone_options options = GIT_CLONE_OPTIONS_INIT;
	git_reference *head;

	if (!_remote_url)
		cl_skip();

	options.bare = true;

	cl_git_pass(git_clone(&g_repo, _remote_url, "./namespaced.git", &options));

	cl_git_pass(git_reference_lookup(&head, g_repo, GIT_HEAD_FILE));
	cl_assert_equal_i(GIT_REFERENCE_SYMBOLIC, git_reference_type(head));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));

	git_reference_free(head);
}

void test_online_clone__namespace_with_specified_branch(void)
{
	git_clone_options options = GIT_CLONE_OPTIONS_INIT;
	git_reference *head;

	if (!_remote_url || !_remote_branch)
		cl_skip();

	options.checkout_branch = _remote_branch;

	cl_git_pass(git_clone(&g_repo, _remote_url, "./namespaced", &options));

	cl_git_pass(git_reference_lookup(&head, g_repo, GIT_HEAD_FILE));
	cl_assert_equal_i(GIT_REFERENCE_SYMBOLIC, git_reference_type(head));
	cl_assert_equal_strn("refs/heads/", git_reference_symbolic_target(head), 11);
	cl_assert_equal_s(_remote_branch, git_reference_symbolic_target(head) + 11);

	git_reference_free(head);
}

void test_online_clone__sha256(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	git_clone_options options = GIT_CLONE_OPTIONS_INIT;
	git_reference *head;

	if (!_remote_url)
		cl_skip();

	cl_git_pass(git_clone(&g_repo, _remote_url, "./sha256", &options));
	cl_git_pass(git_reference_lookup(&head, g_repo, GIT_HEAD_FILE));
	cl_assert_equal_i(GIT_REFERENCE_SYMBOLIC, git_reference_type(head));

	git_reference_free(head);
#endif
}
