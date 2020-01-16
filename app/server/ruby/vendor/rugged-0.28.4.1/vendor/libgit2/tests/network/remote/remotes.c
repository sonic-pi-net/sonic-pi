#include "clar_libgit2.h"
#include "config/config_helpers.h"
#include "buffer.h"
#include "refspec.h"
#include "remote.h"

static git_remote *_remote;
static git_repository *_repo;
static const git_refspec *_refspec;

void test_network_remote_remotes__initialize(void)
{
	_repo = cl_git_sandbox_init("testrepo.git");

	cl_git_pass(git_remote_lookup(&_remote, _repo, "test"));

	_refspec = git_remote_get_refspec(_remote, 0);
	cl_assert(_refspec != NULL);
}

void test_network_remote_remotes__cleanup(void)
{
	git_remote_free(_remote);
	_remote = NULL;

	cl_git_sandbox_cleanup();
}

void test_network_remote_remotes__parsing(void)
{
	git_buf url = GIT_BUF_INIT;
	git_remote *_remote2 = NULL;

	cl_assert_equal_s(git_remote_name(_remote), "test");
	cl_assert_equal_s(git_remote_url(_remote), "git://github.com/libgit2/libgit2");
	cl_assert(git_remote_pushurl(_remote) == NULL);

	cl_git_pass(git_remote__urlfordirection(&url, _remote, GIT_DIRECTION_FETCH, NULL));
	cl_assert_equal_s(url.ptr, "git://github.com/libgit2/libgit2");

	cl_git_pass(git_remote__urlfordirection(&url, _remote, GIT_DIRECTION_PUSH, NULL));
	cl_assert_equal_s(url.ptr, "git://github.com/libgit2/libgit2");

	cl_git_pass(git_remote_lookup(&_remote2, _repo, "test_with_pushurl"));
	cl_assert_equal_s(git_remote_name(_remote2), "test_with_pushurl");
	cl_assert_equal_s(git_remote_url(_remote2), "git://github.com/libgit2/fetchlibgit2");
	cl_assert_equal_s(git_remote_pushurl(_remote2), "git://github.com/libgit2/pushlibgit2");

	cl_git_pass(git_remote__urlfordirection(&url, _remote2, GIT_DIRECTION_FETCH, NULL));
	cl_assert_equal_s(url.ptr, "git://github.com/libgit2/fetchlibgit2");

	cl_git_pass(git_remote__urlfordirection(&url, _remote2, GIT_DIRECTION_PUSH, NULL));
	cl_assert_equal_s(url.ptr, "git://github.com/libgit2/pushlibgit2");

	git_remote_free(_remote2);
	git_buf_dispose(&url);
}

static int urlresolve_callback(git_buf *url_resolved, const char *url, int direction, void *payload)
{
	cl_assert(strcmp(url, "git://github.com/libgit2/libgit2") == 0);
	cl_assert(strcmp(payload, "payload") == 0);
	cl_assert(url_resolved->size == 0);
	
	if (direction == GIT_DIRECTION_PUSH)
		git_buf_sets(url_resolved, "pushresolve");
	if (direction == GIT_DIRECTION_FETCH)
		git_buf_sets(url_resolved, "fetchresolve");

	return GIT_OK;
}

void test_network_remote_remotes__urlresolve(void)
{
	git_buf url = GIT_BUF_INIT;

	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;
	callbacks.resolve_url = urlresolve_callback;
	callbacks.payload = "payload";

	cl_assert_equal_s(git_remote_name(_remote), "test");
	cl_assert_equal_s(git_remote_url(_remote), "git://github.com/libgit2/libgit2");
	cl_assert(git_remote_pushurl(_remote) == NULL);

	cl_git_pass(git_remote__urlfordirection(&url, _remote, GIT_DIRECTION_FETCH, &callbacks));
	cl_assert_equal_s(url.ptr, "fetchresolve");

	cl_git_pass(git_remote__urlfordirection(&url, _remote, GIT_DIRECTION_PUSH, &callbacks));
	cl_assert_equal_s(url.ptr, "pushresolve");

	git_buf_dispose(&url);
}

static int urlresolve_passthrough_callback(git_buf *url_resolved, const char *url, int direction, void *payload)
{
	GIT_UNUSED(url_resolved);
	GIT_UNUSED(url);
	GIT_UNUSED(direction);
	GIT_UNUSED(payload);
	return GIT_PASSTHROUGH;
}

void test_network_remote_remotes__urlresolve_passthrough(void)
{
	git_buf url = GIT_BUF_INIT;
	const char *orig_url = "git://github.com/libgit2/libgit2";

	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;
	callbacks.resolve_url = urlresolve_passthrough_callback;

	cl_assert_equal_s(git_remote_name(_remote), "test");
	cl_assert_equal_s(git_remote_url(_remote), orig_url);
	cl_assert(git_remote_pushurl(_remote) == NULL);

	cl_git_pass(git_remote__urlfordirection(&url, _remote, GIT_DIRECTION_FETCH, &callbacks));
	cl_assert_equal_s(url.ptr, orig_url);

	cl_git_pass(git_remote__urlfordirection(&url, _remote, GIT_DIRECTION_PUSH, &callbacks));
	cl_assert_equal_s(url.ptr, orig_url);

	git_buf_dispose(&url);
}

void test_network_remote_remotes__pushurl(void)
{
	const char *name = git_remote_name(_remote);
	git_remote *mod;

	cl_git_pass(git_remote_set_pushurl(_repo, name, "git://github.com/libgit2/notlibgit2"));
	cl_git_pass(git_remote_lookup(&mod, _repo, name));
	cl_assert_equal_s(git_remote_pushurl(mod), "git://github.com/libgit2/notlibgit2");
	git_remote_free(mod);

	cl_git_pass(git_remote_set_pushurl(_repo, name, NULL));
	cl_git_pass(git_remote_lookup(&mod, _repo, name));
	cl_assert(git_remote_pushurl(mod) == NULL);
	git_remote_free(mod);
}

void test_network_remote_remotes__error_when_not_found(void)
{
	git_remote *r;
	cl_git_fail_with(git_remote_lookup(&r, _repo, "does-not-exist"), GIT_ENOTFOUND);

	cl_assert(git_error_last() != NULL);
	cl_assert(git_error_last()->klass == GIT_ERROR_CONFIG);
}

void test_network_remote_remotes__error_when_no_push_available(void)
{
	git_remote *r;
	git_remote_callbacks callbacks = GIT_REMOTE_CALLBACKS_INIT;
	char *specs = {
		"refs/heads/master",
	};
	git_strarray arr = {
		&specs,
		1,
	};


	cl_git_pass(git_remote_create_anonymous(&r, _repo, cl_fixture("testrepo.git")));

	callbacks.transport = git_transport_local;
	cl_git_pass(git_remote_connect(r, GIT_DIRECTION_PUSH, &callbacks, NULL, NULL));

	/* Make sure that push is really not available */
	r->transport->push = NULL;

	cl_git_fail_with(-1, git_remote_upload(r, &arr, NULL));

	git_remote_free(r);
}

void test_network_remote_remotes__refspec_parsing(void)
{
	cl_assert_equal_s(git_refspec_src(_refspec), "refs/heads/*");
	cl_assert_equal_s(git_refspec_dst(_refspec), "refs/remotes/test/*");
}

void test_network_remote_remotes__add_fetchspec(void)
{
	size_t size;

	size = git_remote_refspec_count(_remote);

	cl_git_pass(git_remote_add_fetch(_repo, "test", "refs/*:refs/*"));
	size++;

	git_remote_free(_remote);
	cl_git_pass(git_remote_lookup(&_remote, _repo, "test"));

	cl_assert_equal_i((int)size, (int)git_remote_refspec_count(_remote));

	_refspec = git_remote_get_refspec(_remote, size - 1);
	cl_assert_equal_s(git_refspec_src(_refspec), "refs/*");
	cl_assert_equal_s(git_refspec_dst(_refspec), "refs/*");
	cl_assert_equal_s(git_refspec_string(_refspec), "refs/*:refs/*");
	cl_assert_equal_b(_refspec->push, false);

	cl_git_fail_with(GIT_EINVALIDSPEC, git_remote_add_fetch(_repo, "test", "refs/*/foo/*:refs/*"));
}

void test_network_remote_remotes__dup(void)
{
	git_strarray array;
	git_remote *dup;

	cl_git_pass(git_remote_dup(&dup, _remote));

	cl_assert_equal_s(git_remote_name(dup), git_remote_name(_remote));
	cl_assert_equal_s(git_remote_url(dup), git_remote_url(_remote));
	cl_assert_equal_s(git_remote_pushurl(dup), git_remote_pushurl(_remote));

	cl_git_pass(git_remote_get_fetch_refspecs(&array, _remote));
	cl_assert_equal_i(1, (int)array.count);
	cl_assert_equal_s("+refs/heads/*:refs/remotes/test/*", array.strings[0]);
	git_strarray_free(&array);

	cl_git_pass(git_remote_get_push_refspecs(&array, _remote));
	cl_assert_equal_i(0, (int)array.count);
	git_strarray_free(&array);

	git_remote_free(dup);
}

void test_network_remote_remotes__add_pushspec(void)
{
	size_t size;

	size = git_remote_refspec_count(_remote);

	cl_git_pass(git_remote_add_push(_repo, "test", "refs/*:refs/*"));
	size++;

	git_remote_free(_remote);
	cl_git_pass(git_remote_lookup(&_remote, _repo, "test"));

	cl_assert_equal_i((int)size, (int)git_remote_refspec_count(_remote));

	_refspec = git_remote_get_refspec(_remote, size - 1);
	cl_assert_equal_s(git_refspec_src(_refspec), "refs/*");
	cl_assert_equal_s(git_refspec_dst(_refspec), "refs/*");
	cl_assert_equal_s(git_refspec_string(_refspec), "refs/*:refs/*");

	cl_assert_equal_b(_refspec->push, true);
}

void test_network_remote_remotes__fnmatch(void)
{
	cl_assert(git_refspec_src_matches(_refspec, "refs/heads/master"));
	cl_assert(git_refspec_src_matches(_refspec, "refs/heads/multi/level/branch"));
}

void test_network_remote_remotes__transform(void)
{
	git_buf ref = GIT_BUF_INIT;

	cl_git_pass(git_refspec_transform(&ref, _refspec, "refs/heads/master"));
	cl_assert_equal_s(ref.ptr, "refs/remotes/test/master");
	git_buf_dispose(&ref);
}

void test_network_remote_remotes__transform_destination_to_source(void)
{
	git_buf ref = GIT_BUF_INIT;

	cl_git_pass(git_refspec_rtransform(&ref, _refspec, "refs/remotes/test/master"));
	cl_assert_equal_s(ref.ptr, "refs/heads/master");
	git_buf_dispose(&ref);
}

void test_network_remote_remotes__missing_refspecs(void)
{
	git_config *cfg;

	git_remote_free(_remote);
	_remote = NULL;

	cl_git_pass(git_repository_config(&cfg, _repo));
	cl_git_pass(git_config_set_string(cfg, "remote.specless.url", "http://example.com"));
	cl_git_pass(git_remote_lookup(&_remote, _repo, "specless"));

	git_config_free(cfg);
}

void test_network_remote_remotes__nonmatch_upstream_refspec(void)
{
	git_config *config;
	git_remote *remote;
	char *specstr[] = {
		"refs/tags/*:refs/tags/*",
	};
	git_strarray specs = {
		specstr,
		1,
	};

	cl_git_pass(git_remote_create(&remote, _repo, "taggy", git_repository_path(_repo)));

	/*
	 * Set the current branch's upstream remote to a dummy ref so we call into the code
	 * which tries to check for the current branch's upstream in the refspecs
	 */
	cl_git_pass(git_repository_config(&config, _repo));
	cl_git_pass(git_config_set_string(config, "branch.master.remote", "taggy"));
	cl_git_pass(git_config_set_string(config, "branch.master.merge", "refs/heads/foo"));

	cl_git_pass(git_remote_fetch(remote, &specs, NULL, NULL));

	git_remote_free(remote);
}

void test_network_remote_remotes__list(void)
{
	git_strarray list;
	git_config *cfg;

	cl_git_pass(git_remote_list(&list, _repo));
	cl_assert(list.count == 5);
	git_strarray_free(&list);

	cl_git_pass(git_repository_config(&cfg, _repo));

	/* Create a new remote */
	cl_git_pass(git_config_set_string(cfg, "remote.specless.url", "http://example.com"));

	/* Update a remote (previously without any url/pushurl entry) */
	cl_git_pass(git_config_set_string(cfg, "remote.no-remote-url.pushurl", "http://example.com"));

	cl_git_pass(git_remote_list(&list, _repo));
	cl_assert(list.count == 7);
	git_strarray_free(&list);

	git_config_free(cfg);
}

void test_network_remote_remotes__loading_a_missing_remote_returns_ENOTFOUND(void)
{
	git_remote_free(_remote);
	_remote = NULL;

	cl_assert_equal_i(GIT_ENOTFOUND, git_remote_lookup(&_remote, _repo, "just-left-few-minutes-ago"));
}

void test_network_remote_remotes__loading_with_an_invalid_name_returns_EINVALIDSPEC(void)
{
	git_remote_free(_remote);
	_remote = NULL;

	cl_assert_equal_i(GIT_EINVALIDSPEC, git_remote_lookup(&_remote, _repo, "Inv@{id"));
}

/*
 * $ git remote add addtest http://github.com/libgit2/libgit2
 *
 * $ cat .git/config
 * [...]
 * [remote "addtest"]
 *         url = http://github.com/libgit2/libgit2
 *         fetch = +refs/heads/\*:refs/remotes/addtest/\*
 */
void test_network_remote_remotes__add(void)
{
	git_remote_free(_remote);
	_remote = NULL;

	cl_git_pass(git_remote_create(&_remote, _repo, "addtest", "http://github.com/libgit2/libgit2"));
	cl_assert_equal_i(GIT_REMOTE_DOWNLOAD_TAGS_AUTO, git_remote_autotag(_remote));

	git_remote_free(_remote);
	_remote = NULL;

	cl_git_pass(git_remote_lookup(&_remote, _repo, "addtest"));
	cl_assert_equal_i(GIT_REMOTE_DOWNLOAD_TAGS_AUTO, git_remote_autotag(_remote));

	_refspec = git_vector_get(&_remote->refspecs, 0);
	cl_assert_equal_s("refs/heads/*", git_refspec_src(_refspec));
	cl_assert(git_refspec_force(_refspec) == 1);
	cl_assert_equal_s("refs/remotes/addtest/*", git_refspec_dst(_refspec));
	cl_assert_equal_s(git_remote_url(_remote), "http://github.com/libgit2/libgit2");
}

void test_network_remote_remotes__tagopt(void)
{
	const char *name = git_remote_name(_remote);

	git_remote_set_autotag(_repo, name, GIT_REMOTE_DOWNLOAD_TAGS_ALL);
	assert_config_entry_value(_repo, "remote.test.tagopt", "--tags");

	git_remote_set_autotag(_repo, name, GIT_REMOTE_DOWNLOAD_TAGS_NONE);
	assert_config_entry_value(_repo, "remote.test.tagopt", "--no-tags");

	git_remote_set_autotag(_repo, name, GIT_REMOTE_DOWNLOAD_TAGS_AUTO);
	assert_config_entry_existence(_repo, "remote.test.tagopt", false);
}

void test_network_remote_remotes__can_load_with_an_empty_url(void)
{
	git_remote *remote = NULL;

	cl_git_pass(git_remote_lookup(&remote, _repo, "empty-remote-url"));

	cl_assert(remote->url == NULL);
	cl_assert(remote->pushurl == NULL);

	cl_git_fail(git_remote_connect(remote, GIT_DIRECTION_FETCH, NULL, NULL, NULL));

	cl_assert(git_error_last() != NULL);
	cl_assert(git_error_last()->klass == GIT_ERROR_INVALID);

	git_remote_free(remote);
}

void test_network_remote_remotes__can_load_with_only_an_empty_pushurl(void)
{
	git_remote *remote = NULL;

	cl_git_pass(git_remote_lookup(&remote, _repo, "empty-remote-pushurl"));

	cl_assert(remote->url == NULL);
	cl_assert(remote->pushurl == NULL);

	cl_git_fail(git_remote_connect(remote, GIT_DIRECTION_FETCH, NULL, NULL, NULL));

	git_remote_free(remote);
}

void test_network_remote_remotes__returns_ENOTFOUND_when_neither_url_nor_pushurl(void)
{
	git_remote *remote = NULL;

	cl_git_fail_with(
		git_remote_lookup(&remote, _repo, "no-remote-url"), GIT_ENOTFOUND);
}

static const char *fetch_refspecs[] = {
	"+refs/heads/*:refs/remotes/origin/*",
	"refs/tags/*:refs/tags/*",
	"+refs/pull/*:refs/pull/*",
};

static const char *push_refspecs[] = {
	"refs/heads/*:refs/heads/*",
	"refs/tags/*:refs/tags/*",
	"refs/notes/*:refs/notes/*",
};

void test_network_remote_remotes__query_refspecs(void)
{
	git_remote *remote;
	git_strarray array;
	int i;

	cl_git_pass(git_remote_create_with_fetchspec(&remote, _repo, "query", "git://github.com/libgit2/libgit2", NULL));
	git_remote_free(remote);

	for (i = 0; i < 3; i++) {
		cl_git_pass(git_remote_add_fetch(_repo, "query", fetch_refspecs[i]));
		cl_git_pass(git_remote_add_push(_repo, "query", push_refspecs[i]));
	}

	cl_git_pass(git_remote_lookup(&remote, _repo, "query"));

	cl_git_pass(git_remote_get_fetch_refspecs(&array, remote));
	for (i = 0; i < 3; i++) {
		cl_assert_equal_s(fetch_refspecs[i], array.strings[i]);
	}
	git_strarray_free(&array);

	cl_git_pass(git_remote_get_push_refspecs(&array, remote));
	for (i = 0; i < 3; i++) {
		cl_assert_equal_s(push_refspecs[i], array.strings[i]);
	}
	git_strarray_free(&array);

	git_remote_free(remote);
	git_remote_delete(_repo, "test");
}
