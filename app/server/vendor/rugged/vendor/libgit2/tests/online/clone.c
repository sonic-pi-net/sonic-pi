#include "clar_libgit2.h"

#include "git2/clone.h"
#include "git2/cred_helpers.h"
#include "remote.h"
#include "fileops.h"
#include "refs.h"

#define LIVE_REPO_URL "http://github.com/libgit2/TestGitRepository"
#define LIVE_EMPTYREPO_URL "http://github.com/libgit2/TestEmptyRepository"
#define BB_REPO_URL "https://libgit3@bitbucket.org/libgit2/testgitrepository.git"
#define BB_REPO_URL_WITH_PASS "https://libgit3:libgit3@bitbucket.org/libgit2/testgitrepository.git"
#define BB_REPO_URL_WITH_WRONG_PASS "https://libgit3:wrong@bitbucket.org/libgit2/testgitrepository.git"

#define SSH_REPO_URL "ssh://github.com/libgit2/TestGitRepository"

static git_repository *g_repo;
static git_clone_options g_options;

void test_online_clone__initialize(void)
{
	git_checkout_options dummy_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_remote_callbacks dummy_callbacks = GIT_REMOTE_CALLBACKS_INIT;

	g_repo = NULL;

	memset(&g_options, 0, sizeof(git_clone_options));
	g_options.version = GIT_CLONE_OPTIONS_VERSION;
	g_options.checkout_opts = dummy_opts;
	g_options.checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;
	g_options.remote_callbacks = dummy_callbacks;
}

void test_online_clone__cleanup(void)
{
	if (g_repo) {
		git_repository_free(g_repo);
		g_repo = NULL;
	}
	cl_fixture_cleanup("./foo");
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
	cl_assert_equal_i(GIT_REF_SYMBOLIC, git_reference_type(head));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));

	git_reference_free(head);
}

static void checkout_progress(const char *path, size_t cur, size_t tot, void *payload)
{
	bool *was_called = (bool*)payload;
	GIT_UNUSED(path); GIT_UNUSED(cur); GIT_UNUSED(tot);
	(*was_called) = true;
}

static int fetch_progress(const git_transfer_progress *stats, void *payload)
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

	g_options.checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE_CREATE;
	g_options.checkout_opts.progress_cb = &checkout_progress;
	g_options.checkout_opts.progress_payload = &checkout_progress_cb_was_called;
	g_options.remote_callbacks.transfer_progress = &fetch_progress;
	g_options.remote_callbacks.payload = &fetch_progress_cb_was_called;

	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(g_repo), "master.txt"));
	cl_assert_equal_i(true, git_path_isfile(git_buf_cstr(&path)));

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_assert_equal_i(GIT_REF_SYMBOLIC, git_reference_type(head));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));

	cl_assert_equal_i(true, checkout_progress_cb_was_called);
	cl_assert_equal_i(true, fetch_progress_cb_was_called);

	git_reference_free(head);
	git_buf_free(&path);
}

static int remote_mirror_cb(git_remote **out, git_repository *repo,
			    const char *name, const char *url, void *payload)
{
	int error;
	git_remote *remote;
	git_remote_callbacks *callbacks = (git_remote_callbacks *) payload;


	if ((error = git_remote_create(&remote, repo, name, url)) < 0)
		return error;

	if ((error = git_remote_set_callbacks(remote, callbacks)) < 0) {
		git_remote_free(remote);
		return error;
	}

	git_remote_clear_refspecs(remote);

	if ((error = git_remote_add_fetch(remote, "+refs/*:refs/*")) < 0) {
		git_remote_free(remote);
		return error;
	}

	*out = remote;
	return 0;
}

void test_online_clone__clone_mirror(void)
{
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	git_reference *head;
	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;

	bool fetch_progress_cb_was_called = false;

	callbacks.transfer_progress = &fetch_progress;
	callbacks.payload = &fetch_progress_cb_was_called;

	opts.bare = true;
	opts.remote_cb = remote_mirror_cb;
	opts.remote_cb_payload = &callbacks;

	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo.git", &opts));

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_assert_equal_i(GIT_REF_SYMBOLIC, git_reference_type(head));
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

	g_options.remote_callbacks.update_tips = update_tips;
	g_options.remote_callbacks.payload = &callcount;

	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));
	cl_assert(callcount > 0);
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
	const char *remote_url = cl_getenv("GITTEST_REMOTE_URL");
	const char *remote_user = cl_getenv("GITTEST_REMOTE_USER");

	if (!remote_url || !remote_user)
		clar__skip();

	g_options.remote_callbacks.credentials = cred_failure_cb;

	cl_git_fail_with(-172, git_clone(&g_repo, remote_url, "./foo", &g_options));
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
	const char *remote_url = cl_getenv("GITTEST_REMOTE_URL");
	const char *remote_user = cl_getenv("GITTEST_REMOTE_USER");
	size_t counter = 0;

	if (!remote_url || !remote_user)
		clar__skip();

	g_options.remote_callbacks.credentials = cred_count_calls_cb;
	g_options.remote_callbacks.payload = &counter;

	cl_git_fail_with(GIT_EUSER, git_clone(&g_repo, remote_url, "./foo", &g_options));
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
	const char *remote_url = cl_getenv("GITTEST_REMOTE_URL");
	git_cred_userpass_payload user_pass = {
		cl_getenv("GITTEST_REMOTE_USER"),
		cl_getenv("GITTEST_REMOTE_PASS")
	};

	if (!remote_url) return;

	if (cl_getenv("GITTEST_REMOTE_DEFAULT")) {
		g_options.remote_callbacks.credentials = cred_default;
	} else {
		g_options.remote_callbacks.credentials = git_cred_userpass;
		g_options.remote_callbacks.payload = &user_pass;
	}

	cl_git_pass(git_clone(&g_repo, remote_url, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");
}

void test_online_clone__bitbucket_style(void)
{
	git_cred_userpass_payload user_pass = {
		"libgit2", "libgit2"
	};

	g_options.remote_callbacks.credentials = git_cred_userpass;
	g_options.remote_callbacks.payload = &user_pass;

	cl_git_pass(git_clone(&g_repo, BB_REPO_URL, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");

	/* User and pass from URL */
	user_pass.password = "wrong";
	cl_git_pass(git_clone(&g_repo, BB_REPO_URL_WITH_PASS, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");

	/* Wrong password in URL, fall back to user_pass */
	user_pass.password = "libgit2";
	cl_git_pass(git_clone(&g_repo, BB_REPO_URL_WITH_WRONG_PASS, "./foo", &g_options));
	git_repository_free(g_repo); g_repo = NULL;
	cl_fixture_cleanup("./foo");
}

static int cancel_at_half(const git_transfer_progress *stats, void *payload)
{
	GIT_UNUSED(payload);

	if (stats->received_objects > (stats->total_objects/2))
		return 4321;
	return 0;
}

void test_online_clone__can_cancel(void)
{
	g_options.remote_callbacks.transfer_progress = cancel_at_half;

	cl_git_fail_with(
		git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options), 4321);
}

static int cred_cb(git_cred **cred, const char *url, const char *user_from_url,
		   unsigned int allowed_types, void *payload)
{
	const char *remote_user = cl_getenv("GITTEST_REMOTE_USER");
	const char *pubkey = cl_getenv("GITTEST_REMOTE_SSH_PUBKEY");
	const char *privkey = cl_getenv("GITTEST_REMOTE_SSH_KEY");
	const char *passphrase = cl_getenv("GITTEST_REMOTE_SSH_PASSPHRASE");

	GIT_UNUSED(url); GIT_UNUSED(user_from_url); GIT_UNUSED(payload);

	if (allowed_types & GIT_CREDTYPE_USERNAME)
		return git_cred_username_new(cred, remote_user);

	if (allowed_types & GIT_CREDTYPE_SSH_KEY)
		return git_cred_ssh_key_new(cred, remote_user, pubkey, privkey, passphrase);

	giterr_set(GITERR_NET, "unexpected cred type");
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
	g_options.remote_callbacks.credentials = check_ssh_auth_methods;
	g_options.remote_callbacks.payload = &with_user;

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
	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;

	if ((error = git_remote_create(out, repo, name, url)) < 0)
		return error;

	if ((error = git_remote_set_transport(*out, git_transport_ssh_with_paths, payload)) < 0)
		return error;

	callbacks.credentials = cred_cb;
	git_remote_set_callbacks(*out, &callbacks);

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

	const char *remote_url = cl_getenv("GITTEST_REMOTE_URL");
	const char *remote_user = cl_getenv("GITTEST_REMOTE_USER");

#ifndef GIT_SSH
	clar__skip();
#endif
	if (!remote_url || !remote_user || strncmp(remote_url, "ssh://", 5) != 0)
		clar__skip();

	g_options.remote_cb = custom_remote_ssh_with_paths;
	g_options.remote_cb_payload = &arr;

	cl_git_fail(git_clone(&g_repo, remote_url, "./foo", &g_options));

	arr.strings = good_paths;
	cl_git_pass(git_clone(&g_repo, remote_url, "./foo", &g_options));
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
	g_options.remote_callbacks.credentials = cred_foo_bar;

	cl_git_fail(git_clone(&g_repo, "ssh://git@github.com/libgit2/TestGitRepository", "./foo", &g_options));
}

int ssh_certificate_check(git_cert *cert, int valid, const char *host, void *payload)
{
	git_cert_hostkey *key;
	git_oid expected = {{0}}, actual = {{0}};
	const char *expected_str;

	GIT_UNUSED(valid);
	GIT_UNUSED(payload);

	expected_str = cl_getenv("GITTEST_REMOTE_SSH_FINGERPRINT");
	cl_assert(expected_str);

	cl_git_pass(git_oid_fromstrp(&expected, expected_str));
	cl_assert_equal_i(GIT_CERT_HOSTKEY_LIBSSH2, cert->cert_type);
	key = (git_cert_hostkey *) cert;

	/*
	 * We need to figure out how long our input was to check for
	 * the type. Here we abuse the fact that both hashes fit into
	 * our git_oid type.
	 */
	if (strlen(expected_str) == 32 && key->type & GIT_CERT_SSH_MD5) {
		memcpy(&actual.id, key->hash_md5, 16);
	} else 	if (strlen(expected_str) == 40 && key->type & GIT_CERT_SSH_SHA1) {
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
	g_options.remote_callbacks.certificate_check = ssh_certificate_check;

	if (!cl_getenv("GITTEST_REMOTE_SSH_FINGERPRINT"))
		cl_skip();

	cl_git_fail_with(GIT_EUSER, git_clone(&g_repo, "ssh://localhost/foo", "./foo", &g_options));
}

void test_online_clone__url_with_no_path_returns_EINVALIDSPEC(void)
{
	cl_git_fail_with(git_clone(&g_repo, "http://github.com", "./foo", &g_options),
		GIT_EINVALIDSPEC);
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
	g_options.remote_callbacks.certificate_check = fail_certificate_check;

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
	g_options.remote_callbacks.certificate_check = succeed_certificate_check;

	cl_git_pass(git_clone(&g_repo, "https://github.com/libgit2/TestGitRepository", "./foo", &g_options));
}
