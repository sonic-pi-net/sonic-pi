#include "clar_libgit2.h"

#include "git2/clone.h"
#include "remote.h"
#include "fileops.h"
#include "repository.h"

#define LIVE_REPO_URL "git://github.com/libgit2/TestGitRepository"

static git_clone_options g_options;
static git_repository *g_repo;
static git_reference* g_ref;
static git_remote* g_remote;

void test_clone_nonetwork__initialize(void)
{
	git_checkout_options dummy_opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_remote_callbacks dummy_callbacks = GIT_REMOTE_CALLBACKS_INIT;

	g_repo = NULL;

	memset(&g_options, 0, sizeof(git_clone_options));
	g_options.version = GIT_CLONE_OPTIONS_VERSION;
	g_options.checkout_opts = dummy_opts;
	g_options.checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE_CREATE;
	g_options.remote_callbacks = dummy_callbacks;
	cl_git_pass(git_signature_now(&g_options.signature, "Me", "foo@example.com"));
}

void test_clone_nonetwork__cleanup(void)
{
	if (g_repo) {
		git_repository_free(g_repo);
		g_repo = NULL;
	}

	if (g_ref) {
		git_reference_free(g_ref);
		g_ref = NULL;
	}

	if (g_remote) {
		git_remote_free(g_remote);
		g_remote = NULL;
	}

	git_signature_free(g_options.signature);
	cl_fixture_cleanup("./foo");
}

void test_clone_nonetwork__bad_urls(void)
{
	/* Clone should clean up the mess if the URL isn't a git repository */
	cl_git_fail(git_clone(&g_repo, "not_a_repo", "./foo", &g_options));
	cl_assert(!git_path_exists("./foo"));
	g_options.bare = true;
	cl_git_fail(git_clone(&g_repo, "not_a_repo", "./foo", &g_options));
	cl_assert(!git_path_exists("./foo"));

	cl_git_fail(git_clone(&g_repo, "git://example.com:asdf", "./foo", &g_options));
	cl_git_fail(git_clone(&g_repo, "https://example.com:asdf/foo", "./foo", &g_options));
	cl_git_fail(git_clone(&g_repo, "git://github.com/git://github.com/foo/bar.git.git",
				"./foo", &g_options));
	cl_git_fail(git_clone(&g_repo, "arrbee:my/bad:password@github.com:1111/strange:words.git",
				"./foo", &g_options));
}

void test_clone_nonetwork__do_not_clean_existing_directory(void)
{
	/* Clone should not remove the directory if it already exists, but
	 * Should clean up entries it creates. */
	p_mkdir("./foo", GIT_DIR_MODE);
	cl_git_fail(git_clone(&g_repo, "not_a_repo", "./foo", &g_options));
	cl_assert(git_path_is_empty_dir("./foo"));

	/* Try again with a bare repository. */
	g_options.bare = true;
	cl_git_fail(git_clone(&g_repo, "not_a_repo", "./foo", &g_options));
	cl_assert(git_path_is_empty_dir("./foo"));
}

void test_clone_nonetwork__local(void)
{
	cl_git_pass(git_clone(&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options));
}

void test_clone_nonetwork__local_absolute_path(void)
{
	const char *local_src;
	local_src = cl_fixture("testrepo.git");
	cl_git_pass(git_clone(&g_repo, local_src, "./foo", &g_options));
}

void test_clone_nonetwork__local_bare(void)
{
	g_options.bare = true;
	cl_git_pass(git_clone(&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options));
}

void test_clone_nonetwork__fail_when_the_target_is_a_file(void)
{
	cl_git_mkfile("./foo", "Bar!");
	cl_git_fail(git_clone(&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options));
}

void test_clone_nonetwork__fail_with_already_existing_but_non_empty_directory(void)
{
	p_mkdir("./foo", GIT_DIR_MODE);
	cl_git_mkfile("./foo/bar", "Baz!");
	cl_git_fail(git_clone(&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options));
}

void test_clone_nonetwork__custom_origin_name(void)
{
       g_options.remote_name = "my_origin";
       cl_git_pass(git_clone(&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options));

       cl_git_pass(git_remote_load(&g_remote, g_repo, "my_origin"));
}

void test_clone_nonetwork__defaults(void)
{
	cl_git_pass(git_clone(&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", NULL));
	cl_assert(g_repo);
	cl_git_pass(git_remote_load(&g_remote, g_repo, "origin"));
}

void test_clone_nonetwork__cope_with_already_existing_directory(void)
{
	p_mkdir("./foo", GIT_DIR_MODE);
	cl_git_pass(git_clone(&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options));
}

void test_clone_nonetwork__can_prevent_the_checkout_of_a_standard_repo(void)
{
	git_buf path = GIT_BUF_INIT;

	g_options.checkout_opts.checkout_strategy = 0;
	cl_git_pass(git_clone(&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options));

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(g_repo), "master.txt"));
	cl_assert_equal_i(false, git_path_isfile(git_buf_cstr(&path)));

	git_buf_free(&path);
}

void test_clone_nonetwork__can_checkout_given_branch(void)
{
	g_options.checkout_branch = "test";
	cl_git_pass(git_clone(&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options));

	cl_assert_equal_i(0, git_repository_head_unborn(g_repo));

	cl_git_pass(git_repository_head(&g_ref, g_repo));
	cl_assert_equal_s(git_reference_name(g_ref), "refs/heads/test");

	cl_assert(git_path_exists("foo/readme.txt"));
}

static int clone_cancel_fetch_transfer_progress_cb(
	const git_transfer_progress *stats, void *data)
{
	GIT_UNUSED(stats); GIT_UNUSED(data);
	return -54321;
}

void test_clone_nonetwork__can_cancel_clone_in_fetch(void)
{
	g_options.checkout_branch = "test";

	g_options.remote_callbacks.transfer_progress =
		clone_cancel_fetch_transfer_progress_cb;

	cl_git_fail_with(git_clone(
		&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options),
		-54321);

	cl_assert(!g_repo);
	cl_assert(!git_path_exists("foo/readme.txt"));
}

static int clone_cancel_checkout_cb(
	git_checkout_notify_t why,
	const char *path,
	const git_diff_file *b,
	const git_diff_file *t,
	const git_diff_file *w,
	void *payload)
{
	const char *at_file = payload;
	GIT_UNUSED(why); GIT_UNUSED(b); GIT_UNUSED(t); GIT_UNUSED(w);
	if (!strcmp(path, at_file))
		return -12345;
	return 0;
}

void test_clone_nonetwork__can_cancel_clone_in_checkout(void)
{
	g_options.checkout_branch = "test";

	g_options.checkout_opts.notify_flags = GIT_CHECKOUT_NOTIFY_UPDATED;
	g_options.checkout_opts.notify_cb = clone_cancel_checkout_cb;
	g_options.checkout_opts.notify_payload = "readme.txt";

	cl_git_fail_with(git_clone(
		&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options),
		-12345);

	cl_assert(!g_repo);
	cl_assert(!git_path_exists("foo/readme.txt"));
}

void test_clone_nonetwork__can_detached_head(void)
{
	git_object *obj;
	git_repository *cloned;
	git_reference *cloned_head;
	git_reflog *log;
	const git_reflog_entry *entry;

	cl_git_pass(git_clone(&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options));

	cl_git_pass(git_revparse_single(&obj, g_repo, "master~1"));
	cl_git_pass(git_repository_set_head_detached(g_repo, git_object_id(obj), NULL, NULL));

	cl_git_pass(git_clone(&cloned, "./foo", "./foo1", &g_options));

	cl_assert(git_repository_head_detached(cloned));

	cl_git_pass(git_repository_head(&cloned_head, cloned));
	cl_assert(!git_oid_cmp(git_object_id(obj), git_reference_target(cloned_head)));

	cl_git_pass(git_reflog_read(&log, cloned, "HEAD"));
	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s("foo@example.com", git_reflog_entry_committer(entry)->email);

	git_object_free(obj);
	git_reference_free(cloned_head);
	git_reflog_free(log);
	git_repository_free(cloned);

	cl_fixture_cleanup("./foo1");
}

static void assert_correct_reflog(const char *name)
{
	git_reflog *log;
	const git_reflog_entry *entry;
	char expected_log_message[128] = {0};

	sprintf(expected_log_message, "clone: from %s", cl_git_fixture_url("testrepo.git"));

	cl_git_pass(git_reflog_read(&log, g_repo, name));
	cl_assert_equal_i(1, git_reflog_entrycount(log));
	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s(expected_log_message, git_reflog_entry_message(entry));
	cl_assert_equal_s("foo@example.com", git_reflog_entry_committer(entry)->email);

	git_reflog_free(log);
}

void test_clone_nonetwork__clone_updates_reflog_properly(void)
{
	cl_git_pass(git_clone(&g_repo, cl_git_fixture_url("testrepo.git"), "./foo", &g_options));
	assert_correct_reflog("HEAD");
	assert_correct_reflog("refs/heads/master");
}

void test_clone_nonetwork__clone_into_updates_reflog_properly(void)
{
	git_remote *remote;
	git_signature *sig;
	cl_git_pass(git_signature_now(&sig, "Me", "foo@example.com"));

	cl_git_pass(git_repository_init(&g_repo, "./foo", false));
	cl_git_pass(git_remote_create(&remote, g_repo, "origin", cl_git_fixture_url("testrepo.git")));
	cl_git_pass(git_clone_into(g_repo, remote, NULL, NULL, sig));

	assert_correct_reflog("HEAD");
	assert_correct_reflog("refs/heads/master");

	git_remote_free(remote);
	git_signature_free(sig);
}

static void cleanup_repository(void *path)
{
	if (g_repo) {
		git_repository_free(g_repo);
		g_repo = NULL;
	}

	cl_fixture_cleanup((const char *)path);
}

void test_clone_nonetwork__clone_from_empty_sets_upstream(void)
{
	git_config *config;
	git_repository *repo;
	const char *str;

	/* Create an empty repo to clone from */
	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));
	cl_set_cleanup(&cleanup_repository, "./repowithunborn");
	cl_git_pass(git_clone(&repo, "./test1", "./repowithunborn", NULL));

	cl_git_pass(git_repository_config(&config, repo));

	cl_git_pass(git_config_get_string(&str, config, "branch.master.remote"));
	cl_assert_equal_s("origin", str);
	cl_git_pass(git_config_get_string(&str, config, "branch.master.merge"));
	cl_assert_equal_s("refs/heads/master", str);

	git_config_free(config);
	git_repository_free(repo);
	cl_fixture_cleanup("./repowithunborn");
}
