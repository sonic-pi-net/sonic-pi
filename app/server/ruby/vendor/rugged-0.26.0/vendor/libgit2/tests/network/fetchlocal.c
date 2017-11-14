#include "clar_libgit2.h"

#include "buffer.h"
#include "path.h"
#include "remote.h"

static const char* tagger_name = "Vicent Marti";
static const char* tagger_email = "vicent@github.com";
static const char* tagger_message = "This is my tag.\n\nThere are many tags, but this one is mine\n";

static int transfer_cb(const git_transfer_progress *stats, void *payload)
{
	int *callcount = (int*)payload;
	GIT_UNUSED(stats);
	(*callcount)++;
	return 0;
}

static void cleanup_local_repo(void *path)
{
	cl_fixture_cleanup((char *)path);
}

void test_network_fetchlocal__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_network_fetchlocal__complete(void)
{
	git_repository *repo;
	git_remote *origin;
	int callcount = 0;
	git_strarray refnames = {0};

	const char *url = cl_git_fixture_url("testrepo.git");
	git_fetch_options options = GIT_FETCH_OPTIONS_INIT;

	options.callbacks.transfer_progress = transfer_cb;
	options.callbacks.payload = &callcount;

	cl_set_cleanup(&cleanup_local_repo, "foo");
	cl_git_pass(git_repository_init(&repo, "foo", true));

	cl_git_pass(git_remote_create(&origin, repo, GIT_REMOTE_ORIGIN, url));
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));

	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(19, (int)refnames.count);
	cl_assert(callcount > 0);

	git_strarray_free(&refnames);
	git_remote_free(origin);
	git_repository_free(repo);
}

void test_network_fetchlocal__prune(void)
{
	git_repository *repo;
	git_remote *origin;
	int callcount = 0;
	git_strarray refnames = {0};
	git_reference *ref;
	git_repository *remote_repo = cl_git_sandbox_init("testrepo.git");
	const char *url = cl_git_path_url(git_repository_path(remote_repo));
	git_fetch_options options = GIT_FETCH_OPTIONS_INIT;

	options.callbacks.transfer_progress = transfer_cb;
	options.callbacks.payload = &callcount;

	cl_set_cleanup(&cleanup_local_repo, "foo");
	cl_git_pass(git_repository_init(&repo, "foo", true));

	cl_git_pass(git_remote_create(&origin, repo, GIT_REMOTE_ORIGIN, url));
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));

	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(19, (int)refnames.count);
	cl_assert(callcount > 0);
	git_strarray_free(&refnames);
	git_remote_free(origin);

	cl_git_pass(git_reference_lookup(&ref, remote_repo, "refs/heads/br2"));
	cl_git_pass(git_reference_delete(ref));
	git_reference_free(ref);

	cl_git_pass(git_remote_lookup(&origin, repo, GIT_REMOTE_ORIGIN));
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));
	cl_git_pass(git_remote_prune(origin, &options.callbacks));

	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(18, (int)refnames.count);
	git_strarray_free(&refnames);
	git_remote_free(origin);

	cl_git_pass(git_reference_lookup(&ref, remote_repo, "refs/heads/packed"));
	cl_git_pass(git_reference_delete(ref));
	git_reference_free(ref);

	cl_git_pass(git_remote_lookup(&origin, repo, GIT_REMOTE_ORIGIN));
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));
	cl_git_pass(git_remote_prune(origin, &options.callbacks));

	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(17, (int)refnames.count);
	git_strarray_free(&refnames);
	git_remote_free(origin);

	git_repository_free(repo);
}

int update_tips_fail_on_call(const char *ref, const git_oid *old, const git_oid *new, void *data)
{
	GIT_UNUSED(ref);
	GIT_UNUSED(old);
	GIT_UNUSED(new);
	GIT_UNUSED(data);

	cl_fail("update tips called");
	return 0;
}

void assert_ref_exists(git_repository *repo, const char *name)
{
	git_reference *ref;

	cl_git_pass(git_reference_lookup(&ref, repo, name));
	git_reference_free(ref);
}

void test_network_fetchlocal__prune_overlapping(void)
{
	git_repository *repo;
	git_remote *origin;
	int callcount = 0;
	git_strarray refnames = {0};
	git_reference *ref;
	git_config *config;
	git_oid target;

	git_repository *remote_repo = cl_git_sandbox_init("testrepo.git");
	const char *url = cl_git_path_url(git_repository_path(remote_repo));

	git_fetch_options options = GIT_FETCH_OPTIONS_INIT;
	options.callbacks.transfer_progress = transfer_cb;
	options.callbacks.payload = &callcount;

	cl_git_pass(git_reference_lookup(&ref, remote_repo, "refs/heads/master"));
	git_oid_cpy(&target, git_reference_target(ref));
	git_reference_free(ref);
	cl_git_pass(git_reference_create(&ref, remote_repo, "refs/pull/42/head", &target, 1, NULL));
	git_reference_free(ref);

	cl_set_cleanup(&cleanup_local_repo, "foo");
	cl_git_pass(git_repository_init(&repo, "foo", true));

	cl_git_pass(git_remote_create(&origin, repo, GIT_REMOTE_ORIGIN, url));

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_bool(config, "remote.origin.prune", true));
	cl_git_pass(git_config_set_multivar(config, "remote.origin.fetch", "^$", "refs/pull/*/head:refs/remotes/origin/pr/*"));

	git_remote_free(origin);
	cl_git_pass(git_remote_lookup(&origin, repo, GIT_REMOTE_ORIGIN));
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));

	assert_ref_exists(repo, "refs/remotes/origin/master");
	assert_ref_exists(repo, "refs/remotes/origin/pr/42");
	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(20, (int)refnames.count);
	git_strarray_free(&refnames);

	cl_git_pass(git_config_delete_multivar(config, "remote.origin.fetch", "refs"));
	cl_git_pass(git_config_set_multivar(config, "remote.origin.fetch", "^$", "refs/pull/*/head:refs/remotes/origin/pr/*"));
	cl_git_pass(git_config_set_multivar(config, "remote.origin.fetch", "^$", "refs/heads/*:refs/remotes/origin/*"));

	git_remote_free(origin);
	cl_git_pass(git_remote_lookup(&origin, repo, GIT_REMOTE_ORIGIN));
	options.callbacks.update_tips = update_tips_fail_on_call;
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));

	assert_ref_exists(repo, "refs/remotes/origin/master");
	assert_ref_exists(repo, "refs/remotes/origin/pr/42");
	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(20, (int)refnames.count);
	git_strarray_free(&refnames);

	cl_git_pass(git_config_delete_multivar(config, "remote.origin.fetch", "refs"));
	cl_git_pass(git_config_set_multivar(config, "remote.origin.fetch", "^$", "refs/heads/*:refs/remotes/origin/*"));
	cl_git_pass(git_config_set_multivar(config, "remote.origin.fetch", "^$", "refs/pull/*/head:refs/remotes/origin/pr/*"));

	git_remote_free(origin);
	cl_git_pass(git_remote_lookup(&origin, repo, GIT_REMOTE_ORIGIN));
	options.callbacks.update_tips = update_tips_fail_on_call;
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));

	git_config_free(config);
	git_strarray_free(&refnames);
	git_remote_free(origin);
	git_repository_free(repo);
}

void test_network_fetchlocal__fetchprune(void)
{
	git_repository *repo;
	git_remote *origin;
	int callcount = 0;
	git_strarray refnames = {0};
	git_reference *ref;
	git_config *config;
	git_repository *remote_repo = cl_git_sandbox_init("testrepo.git");
	const char *url = cl_git_path_url(git_repository_path(remote_repo));
	git_fetch_options options = GIT_FETCH_OPTIONS_INIT;

	options.callbacks.transfer_progress = transfer_cb;
	options.callbacks.payload = &callcount;

	cl_set_cleanup(&cleanup_local_repo, "foo");
	cl_git_pass(git_repository_init(&repo, "foo", true));

	cl_git_pass(git_remote_create(&origin, repo, GIT_REMOTE_ORIGIN, url));
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));

	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(19, (int)refnames.count);
	cl_assert(callcount > 0);
	git_strarray_free(&refnames);
	git_remote_free(origin);

	cl_git_pass(git_reference_lookup(&ref, remote_repo, "refs/heads/br2"));
	cl_git_pass(git_reference_delete(ref));
	git_reference_free(ref);

	cl_git_pass(git_remote_lookup(&origin, repo, GIT_REMOTE_ORIGIN));
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));
	cl_git_pass(git_remote_prune(origin, &options.callbacks));

	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(18, (int)refnames.count);
	git_strarray_free(&refnames);
	git_remote_free(origin);

	cl_git_pass(git_reference_lookup(&ref, remote_repo, "refs/heads/packed"));
	cl_git_pass(git_reference_delete(ref));
	git_reference_free(ref);

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_bool(config, "remote.origin.prune", 1));
	git_config_free(config);
	cl_git_pass(git_remote_lookup(&origin, repo, GIT_REMOTE_ORIGIN));
	cl_assert_equal_i(1, git_remote_prune_refs(origin));
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));

	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(17, (int)refnames.count);
	git_strarray_free(&refnames);
	git_remote_free(origin);

	git_repository_free(repo);
}

void test_network_fetchlocal__prune_tag(void)
{
	git_repository *repo;
	git_remote *origin;
	int callcount = 0;
	git_reference *ref;
	git_config *config;
	git_oid tag_id;
	git_signature *tagger;
	git_object *obj;

	git_repository *remote_repo = cl_git_sandbox_init("testrepo.git");
	const char *url = cl_git_path_url(git_repository_path(remote_repo));
	git_fetch_options options = GIT_FETCH_OPTIONS_INIT;

	options.callbacks.transfer_progress = transfer_cb;
	options.callbacks.payload = &callcount;

	cl_set_cleanup(&cleanup_local_repo, "foo");
	cl_git_pass(git_repository_init(&repo, "foo", true));

	cl_git_pass(git_remote_create(&origin, repo, GIT_REMOTE_ORIGIN, url));
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));
	git_remote_free(origin);

	cl_git_pass(git_revparse_single(&obj, repo, "origin/master"));

	cl_git_pass(git_reference_create(&ref, repo, "refs/remotes/origin/fake-remote", git_object_id(obj), 1, NULL));
	git_reference_free(ref);

	/* create signature */
	cl_git_pass(git_signature_new(&tagger, tagger_name, tagger_email, 123456789, 60));

	cl_git_pass(
		git_tag_create(&tag_id, repo,
		  "some-tag", obj, tagger, tagger_message, 0)
	);
	git_signature_free(tagger);

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_bool(config, "remote.origin.prune", 1));
	git_config_free(config);
	cl_git_pass(git_remote_lookup(&origin, repo, GIT_REMOTE_ORIGIN));
	cl_assert_equal_i(1, git_remote_prune_refs(origin));
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));

	assert_ref_exists(repo, "refs/tags/some-tag");
	cl_git_fail_with(GIT_ENOTFOUND, git_reference_lookup(&ref, repo, "refs/remotes/origin/fake-remote"));

	git_object_free(obj);
	git_remote_free(origin);

	git_repository_free(repo);
}

static void cleanup_sandbox(void *unused)
{
	GIT_UNUSED(unused);
	cl_git_sandbox_cleanup();
}

void test_network_fetchlocal__partial(void)
{
	git_repository *repo = cl_git_sandbox_init("partial-testrepo");
	git_remote *origin;
	int callcount = 0;
	git_strarray refnames = {0};
	const char *url;
	git_fetch_options options = GIT_FETCH_OPTIONS_INIT;

	options.callbacks.transfer_progress = transfer_cb;
	options.callbacks.payload = &callcount;

	cl_set_cleanup(&cleanup_sandbox, NULL);
	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(1, (int)refnames.count);

	url = cl_git_fixture_url("testrepo.git");
	cl_git_pass(git_remote_create(&origin, repo, GIT_REMOTE_ORIGIN, url));
	cl_git_pass(git_remote_fetch(origin, NULL, &options, NULL));

	git_strarray_free(&refnames);

	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(20, (int)refnames.count); /* 18 remote + 1 local */
	cl_assert(callcount > 0);

	git_strarray_free(&refnames);
	git_remote_free(origin);
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

void test_network_fetchlocal__clone_into_mirror(void)
{
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	git_repository *repo;
	git_reference *ref;

	opts.bare = true;
	opts.remote_cb = remote_mirror_cb;
	cl_git_pass(git_clone(&repo, cl_git_fixture_url("testrepo.git"), "./foo.git", &opts));

	cl_git_pass(git_reference_lookup(&ref, repo, "HEAD"));
	cl_assert_equal_i(GIT_REF_SYMBOLIC, git_reference_type(ref));
	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(ref));

	git_reference_free(ref);
	cl_git_pass(git_reference_lookup(&ref, repo, "refs/remotes/test/master"));

	git_reference_free(ref);
	git_repository_free(repo);
	cl_fixture_cleanup("./foo.git");
}

void test_network_fetchlocal__all_refs(void)
{
	git_repository *repo;
	git_remote *remote;
	git_reference *ref;
	char *allrefs = "+refs/*:refs/*";
	git_strarray refspecs = {
		&allrefs,
		1,
	};

	cl_git_pass(git_repository_init(&repo, "./foo.git", true));
	cl_git_pass(git_remote_create_anonymous(&remote, repo, cl_git_fixture_url("testrepo.git")));
	cl_git_pass(git_remote_fetch(remote, &refspecs, NULL, NULL));

	cl_git_pass(git_reference_lookup(&ref, repo, "refs/remotes/test/master"));
	git_reference_free(ref);

	cl_git_pass(git_reference_lookup(&ref, repo, "refs/tags/test"));
	git_reference_free(ref);

	git_remote_free(remote);
	git_repository_free(repo);
	cl_fixture_cleanup("./foo.git");
}

void test_network_fetchlocal__multi_remotes(void)
{
	git_repository *repo = cl_git_sandbox_init("testrepo.git");
	git_remote *test, *test2;
	git_strarray refnames = {0};
	git_fetch_options options = GIT_FETCH_OPTIONS_INIT;

	cl_set_cleanup(&cleanup_sandbox, NULL);
	options.callbacks.transfer_progress = transfer_cb;
	cl_git_pass(git_remote_set_url(repo, "test", cl_git_fixture_url("testrepo.git")));
	cl_git_pass(git_remote_lookup(&test, repo, "test"));
	cl_git_pass(git_remote_fetch(test, NULL, &options, NULL));

	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(32, (int)refnames.count);
	git_strarray_free(&refnames);

	cl_git_pass(git_remote_set_url(repo, "test_with_pushurl", cl_git_fixture_url("testrepo.git")));
	cl_git_pass(git_remote_lookup(&test2, repo, "test_with_pushurl"));
	cl_git_pass(git_remote_fetch(test2, NULL, &options, NULL));

	cl_git_pass(git_reference_list(&refnames, repo));
	cl_assert_equal_i(44, (int)refnames.count);

	git_strarray_free(&refnames);
	git_remote_free(test);
	git_remote_free(test2);
}

static int sideband_cb(const char *str, int len, void *payload)
{
	int *count = (int *) payload;

	GIT_UNUSED(str);
	GIT_UNUSED(len);

	(*count)++;
	return 0;
}

void test_network_fetchlocal__call_progress(void)
{
	git_repository *repo;
	git_remote *remote;
	git_fetch_options options = GIT_FETCH_OPTIONS_INIT;
	int callcount = 0;

	cl_git_pass(git_repository_init(&repo, "foo.git", true));
	cl_set_cleanup(cleanup_local_repo, "foo.git");

	cl_git_pass(git_remote_create_with_fetchspec(&remote, repo, "origin", cl_git_fixture_url("testrepo.git"), "+refs/heads/*:refs/heads/*"));

	options.callbacks.sideband_progress = sideband_cb;
	options.callbacks.payload = &callcount;

	cl_git_pass(git_remote_fetch(remote, NULL, &options, NULL));
	cl_assert(callcount != 0);

	git_remote_free(remote);
	git_repository_free(repo);
}

void test_network_fetchlocal__prune_load_remote_prune_config(void)
{
	git_repository *repo;
	git_remote *origin;
	git_config *config;
	git_repository *remote_repo = cl_git_sandbox_init("testrepo.git");
	const char *url = cl_git_path_url(git_repository_path(remote_repo));

	cl_set_cleanup(&cleanup_local_repo, "foo");
	cl_git_pass(git_repository_init(&repo, "foo", true));

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_bool(config, "remote.origin.prune", 1));

	cl_git_pass(git_remote_create(&origin, repo, GIT_REMOTE_ORIGIN, url));
	cl_assert_equal_i(1, git_remote_prune_refs(origin));

	git_config_free(config);
	git_remote_free(origin);
	git_repository_free(repo);
}

void test_network_fetchlocal__prune_load_fetch_prune_config(void)
{
	git_repository *repo;
	git_remote *origin;
	git_config *config;
	git_repository *remote_repo = cl_git_sandbox_init("testrepo.git");
	const char *url = cl_git_path_url(git_repository_path(remote_repo));

	cl_set_cleanup(&cleanup_local_repo, "foo");
	cl_git_pass(git_repository_init(&repo, "foo", true));

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_bool(config, "fetch.prune", 1));

	cl_git_pass(git_remote_create(&origin, repo, GIT_REMOTE_ORIGIN, url));
	cl_assert_equal_i(1, git_remote_prune_refs(origin));

	git_config_free(config);
	git_remote_free(origin);
	git_repository_free(repo);
}
