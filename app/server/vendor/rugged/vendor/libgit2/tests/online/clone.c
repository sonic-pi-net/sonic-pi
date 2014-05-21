#include "clar_libgit2.h"

#include "git2/clone.h"
#include "git2/cred_helpers.h"
#include "remote.h"
#include "fileops.h"
#include "refs.h"

#define LIVE_REPO_URL "http://github.com/libgit2/TestGitRepository"
#define LIVE_EMPTYREPO_URL "http://github.com/libgit2/TestEmptyRepository"
#define BB_REPO_URL "https://libgit2@bitbucket.org/libgit2/testgitrepository.git"
#define BB_REPO_URL_WITH_PASS "https://libgit2:libgit2@bitbucket.org/libgit2/testgitrepository.git"
#define BB_REPO_URL_WITH_WRONG_PASS "https://libgit2:wrong@bitbucket.org/libgit2/testgitrepository.git"
#define ASSEMBLA_REPO_URL "https://libgit2:_Libgit2@git.assembla.com/libgit2-test-repos.git"

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
	cl_git_pass(git_remote_load(&origin, g_repo, "origin"));

	cl_assert_equal_i(GIT_REMOTE_DOWNLOAD_TAGS_AUTO, origin->download_tags);

	git_remote_free(origin);
}

void test_online_clone__network_bare(void)
{
	git_remote *origin;

	g_options.bare = true;

	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));
	cl_assert(git_repository_is_bare(g_repo));
	cl_git_pass(git_remote_load(&origin, g_repo, "origin"));

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

void test_online_clone__clone_into(void)
{
	git_buf path = GIT_BUF_INIT;
	git_remote *remote;
	git_reference *head;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;

	bool checkout_progress_cb_was_called = false,
		  fetch_progress_cb_was_called = false;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE_CREATE;
	checkout_opts.progress_cb = &checkout_progress;
	checkout_opts.progress_payload = &checkout_progress_cb_was_called;

	cl_git_pass(git_repository_init(&g_repo, "./foo", false));
	cl_git_pass(git_remote_create(&remote, g_repo, "origin", LIVE_REPO_URL));

	callbacks.transfer_progress = &fetch_progress;
	callbacks.payload = &fetch_progress_cb_was_called;
	git_remote_set_callbacks(remote, &callbacks);

	cl_git_pass(git_clone_into(g_repo, remote, &checkout_opts, NULL, NULL));

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(g_repo), "master.txt"));
	cl_assert_equal_i(true, git_path_isfile(git_buf_cstr(&path)));

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_assert_equal_i(GIT_REF_SYMBOLIC, git_reference_type(head));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));

	cl_assert_equal_i(true, checkout_progress_cb_was_called);
	cl_assert_equal_i(true, fetch_progress_cb_was_called);

	git_remote_free(remote);
	git_reference_free(head);
	git_buf_free(&path);
}

void test_online_clone__clone_mirror(void)
{
	git_buf path = GIT_BUF_INIT;
	git_remote *remote;
	git_reference *head;
	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;

	bool fetch_progress_cb_was_called = false;

	cl_git_pass(git_repository_init(&g_repo, "./foo.git", true));
	cl_git_pass(git_remote_create(&remote, g_repo, "origin", LIVE_REPO_URL));

	callbacks.transfer_progress = &fetch_progress;
	callbacks.payload = &fetch_progress_cb_was_called;
	git_remote_set_callbacks(remote, &callbacks);

	git_remote_clear_refspecs(remote);
	cl_git_pass(git_remote_add_fetch(remote, "+refs/*:refs/*"));

	cl_git_pass(git_clone_into(g_repo, remote, NULL, NULL, NULL));

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_assert_equal_i(GIT_REF_SYMBOLIC, git_reference_type(head));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));

	cl_assert_equal_i(true, fetch_progress_cb_was_called);

	git_remote_free(remote);
	git_reference_free(head);
	git_buf_free(&path);
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

	/* TODO: this should expect -172. */
	cl_git_fail_with(git_clone(&g_repo, remote_url, "./foo", &g_options), -1);
}

void test_online_clone__credentials(void)
{
	/* Remote URL environment variable must be set.  User and password are optional.  */
	const char *remote_url = cl_getenv("GITTEST_REMOTE_URL");
	git_cred_userpass_payload user_pass = {
		cl_getenv("GITTEST_REMOTE_USER"),
		cl_getenv("GITTEST_REMOTE_PASS")
	};

	if (!remote_url) return;

	g_options.remote_callbacks.credentials = git_cred_userpass;
	g_options.remote_callbacks.payload = &user_pass;

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

void test_online_clone__assembla_style(void)
{
	cl_git_pass(git_clone(&g_repo, ASSEMBLA_REPO_URL, "./foo", NULL));
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






