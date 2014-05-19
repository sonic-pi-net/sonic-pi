#include "clar_libgit2.h"
#include "buffer.h"
#include "refspec.h"
#include "remote.h"

static git_remote *_remote;
static git_repository *_repo;
static const git_refspec *_refspec;

void test_network_remote_remotes__initialize(void)
{
	_repo = cl_git_sandbox_init("testrepo.git");

	cl_git_pass(git_remote_load(&_remote, _repo, "test"));

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
	git_remote *_remote2 = NULL;

	cl_assert_equal_s(git_remote_name(_remote), "test");
	cl_assert_equal_s(git_remote_url(_remote), "git://github.com/libgit2/libgit2");
	cl_assert(git_remote_pushurl(_remote) == NULL);

	cl_assert_equal_s(git_remote__urlfordirection(_remote, GIT_DIRECTION_FETCH),
					  "git://github.com/libgit2/libgit2");
	cl_assert_equal_s(git_remote__urlfordirection(_remote, GIT_DIRECTION_PUSH),
					  "git://github.com/libgit2/libgit2");

	cl_git_pass(git_remote_load(&_remote2, _repo, "test_with_pushurl"));
	cl_assert_equal_s(git_remote_name(_remote2), "test_with_pushurl");
	cl_assert_equal_s(git_remote_url(_remote2), "git://github.com/libgit2/fetchlibgit2");
	cl_assert_equal_s(git_remote_pushurl(_remote2), "git://github.com/libgit2/pushlibgit2");

	cl_assert_equal_s(git_remote__urlfordirection(_remote2, GIT_DIRECTION_FETCH),
					  "git://github.com/libgit2/fetchlibgit2");
	cl_assert_equal_s(git_remote__urlfordirection(_remote2, GIT_DIRECTION_PUSH),
					  "git://github.com/libgit2/pushlibgit2");

	git_remote_free(_remote2);
}

void test_network_remote_remotes__pushurl(void)
{
	cl_git_pass(git_remote_set_pushurl(_remote, "git://github.com/libgit2/notlibgit2"));
	cl_assert_equal_s(git_remote_pushurl(_remote), "git://github.com/libgit2/notlibgit2");

	cl_git_pass(git_remote_set_pushurl(_remote, NULL));
	cl_assert(git_remote_pushurl(_remote) == NULL);
}

void test_network_remote_remotes__error_when_no_push_available(void)
{
	git_remote *r;
	git_transport *t;
	git_push *p;

	cl_git_pass(git_remote_create_anonymous(&r, _repo, cl_fixture("testrepo.git"), NULL));

	cl_git_pass(git_transport_local(&t,r,NULL));

	/* Make sure that push is really not available */
	t->push = NULL;
	cl_git_pass(git_remote_set_transport(r, t));

	cl_git_pass(git_remote_connect(r, GIT_DIRECTION_PUSH));
	cl_git_pass(git_push_new(&p, r));
	cl_git_pass(git_push_add_refspec(p, "refs/heads/master"));
	cl_git_fail_with(git_push_finish(p), GIT_ERROR);

	git_push_free(p);
	git_remote_free(r);
}

void test_network_remote_remotes__parsing_ssh_remote(void)
{
	cl_assert( git_remote_valid_url("git@github.com:libgit2/libgit2.git") );
}

void test_network_remote_remotes__parsing_local_path_fails_if_path_not_found(void)
{
	cl_assert( !git_remote_valid_url("/home/git/repos/libgit2.git") );
}

void test_network_remote_remotes__supported_transport_methods_are_supported(void)
{
	cl_assert( git_remote_supported_url("git://github.com/libgit2/libgit2") );
}

void test_network_remote_remotes__unsupported_transport_methods_are_unsupported(void)
{
#ifndef GIT_SSH
	cl_assert( !git_remote_supported_url("git@github.com:libgit2/libgit2.git") );
#endif
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

	cl_git_pass(git_remote_add_fetch(_remote, "refs/*:refs/*"));

	size++;
	cl_assert_equal_i((int)size, (int)git_remote_refspec_count(_remote));

	_refspec = git_remote_get_refspec(_remote, size - 1);
	cl_assert_equal_s(git_refspec_src(_refspec), "refs/*");
	cl_assert_equal_s(git_refspec_dst(_refspec), "refs/*");
	cl_assert_equal_s(git_refspec_string(_refspec), "refs/*:refs/*");
	cl_assert_equal_b(_refspec->push, false);
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

	cl_git_pass(git_remote_add_push(_remote, "refs/*:refs/*"));
	size++;
	cl_assert_equal_i((int)size, (int)git_remote_refspec_count(_remote));

	_refspec = git_remote_get_refspec(_remote, size - 1);
	cl_assert_equal_s(git_refspec_src(_refspec), "refs/*");
	cl_assert_equal_s(git_refspec_dst(_refspec), "refs/*");
	cl_assert_equal_s(git_refspec_string(_refspec), "refs/*:refs/*");

	cl_assert_equal_b(_refspec->push, true);
}

void test_network_remote_remotes__save(void)
{
	git_strarray array;
	const char *fetch_refspec1 = "refs/heads/ns1/*:refs/remotes/upstream/ns1/*";
	const char *fetch_refspec2 = "refs/heads/ns2/*:refs/remotes/upstream/ns2/*";
	const char *push_refspec1 = "refs/heads/ns1/*:refs/heads/ns1/*";
	const char *push_refspec2 = "refs/heads/ns2/*:refs/heads/ns2/*";

	git_remote_free(_remote);
	_remote = NULL;

	/* Set up the remote and save it to config */
	cl_git_pass(git_remote_create(&_remote, _repo, "upstream", "git://github.com/libgit2/libgit2"));
	git_remote_clear_refspecs(_remote);

	cl_git_pass(git_remote_add_fetch(_remote, fetch_refspec1));
	cl_git_pass(git_remote_add_fetch(_remote, fetch_refspec2));
	cl_git_pass(git_remote_add_push(_remote, push_refspec1));
	cl_git_pass(git_remote_add_push(_remote, push_refspec2));
	cl_git_pass(git_remote_set_pushurl(_remote, "git://github.com/libgit2/libgit2_push"));
	cl_git_pass(git_remote_save(_remote));
	git_remote_free(_remote);
	_remote = NULL;

	/* Load it from config and make sure everything matches */
	cl_git_pass(git_remote_load(&_remote, _repo, "upstream"));

	cl_git_pass(git_remote_get_fetch_refspecs(&array, _remote));
	cl_assert_equal_i(2, (int)array.count);
	cl_assert_equal_s(fetch_refspec1, array.strings[0]);
	cl_assert_equal_s(fetch_refspec2, array.strings[1]);
	git_strarray_free(&array);

	cl_git_pass(git_remote_get_push_refspecs(&array, _remote));
	cl_assert_equal_i(2, (int)array.count);
	cl_assert_equal_s(push_refspec1, array.strings[0]);
	cl_assert_equal_s(push_refspec2, array.strings[1]);
	git_strarray_free(&array);

	cl_assert_equal_s(git_remote_url(_remote), "git://github.com/libgit2/libgit2");
	cl_assert_equal_s(git_remote_pushurl(_remote), "git://github.com/libgit2/libgit2_push");

	/* remove the pushurl again and see if we can save that too */
	cl_git_pass(git_remote_set_pushurl(_remote, NULL));
	cl_git_pass(git_remote_save(_remote));
	git_remote_free(_remote);
	_remote = NULL;

	cl_git_pass(git_remote_load(&_remote, _repo, "upstream"));
	cl_assert(git_remote_pushurl(_remote) == NULL);
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
	git_buf_free(&ref);
}

void test_network_remote_remotes__transform_destination_to_source(void)
{
	git_buf ref = GIT_BUF_INIT;

	cl_git_pass(git_refspec_rtransform(&ref, _refspec, "refs/remotes/test/master"));
	cl_assert_equal_s(ref.ptr, "refs/heads/master");
	git_buf_free(&ref);
}

void test_network_remote_remotes__missing_refspecs(void)
{
	git_config *cfg;

	git_remote_free(_remote);
	_remote = NULL;

	cl_git_pass(git_repository_config(&cfg, _repo));
	cl_git_pass(git_config_set_string(cfg, "remote.specless.url", "http://example.com"));
	cl_git_pass(git_remote_load(&_remote, _repo, "specless"));

	git_config_free(cfg);
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

	cl_assert_equal_i(GIT_ENOTFOUND, git_remote_load(&_remote, _repo, "just-left-few-minutes-ago"));
}

void test_network_remote_remotes__loading_with_an_invalid_name_returns_EINVALIDSPEC(void)
{
	git_remote_free(_remote);
	_remote = NULL;

	cl_assert_equal_i(GIT_EINVALIDSPEC, git_remote_load(&_remote, _repo, "Inv@{id"));
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

	cl_git_pass(git_remote_load(&_remote, _repo, "addtest"));
	cl_assert_equal_i(GIT_REMOTE_DOWNLOAD_TAGS_AUTO, git_remote_autotag(_remote));

	_refspec = git_vector_get(&_remote->refspecs, 0);
	cl_assert_equal_s("refs/heads/*", git_refspec_src(_refspec));
	cl_assert(git_refspec_force(_refspec) == 1);
	cl_assert_equal_s("refs/remotes/addtest/*", git_refspec_dst(_refspec));
	cl_assert_equal_s(git_remote_url(_remote), "http://github.com/libgit2/libgit2");
}

void test_network_remote_remotes__cannot_add_a_nameless_remote(void)
{
	git_remote *remote;

	cl_assert_equal_i(
		GIT_EINVALIDSPEC,
		git_remote_create(&remote, _repo, NULL, "git://github.com/libgit2/libgit2"));
}

void test_network_remote_remotes__cannot_save_an_inmemory_remote(void)
{
	git_remote *remote;

	cl_git_pass(git_remote_create_anonymous(&remote, _repo, "git://github.com/libgit2/libgit2", NULL));

	cl_assert_equal_p(NULL, git_remote_name(remote));

	cl_git_fail(git_remote_save(remote));
	git_remote_free(remote);
}

void test_network_remote_remotes__cannot_add_a_remote_with_an_invalid_name(void)
{
	git_remote *remote = NULL;

	cl_assert_equal_i(
		GIT_EINVALIDSPEC,
		git_remote_create(&remote, _repo, "Inv@{id", "git://github.com/libgit2/libgit2"));
	cl_assert_equal_p(remote, NULL);

	cl_assert_equal_i(
		GIT_EINVALIDSPEC,
		git_remote_create(&remote, _repo, "", "git://github.com/libgit2/libgit2"));
	cl_assert_equal_p(remote, NULL);
}

void test_network_remote_remotes__tagopt(void)
{
	const char *opt;
	git_config *cfg;

	cl_git_pass(git_repository_config(&cfg, _repo));

	git_remote_set_autotag(_remote, GIT_REMOTE_DOWNLOAD_TAGS_ALL);
	cl_git_pass(git_remote_save(_remote));
	cl_git_pass(git_config_get_string(&opt, cfg, "remote.test.tagopt"));
	cl_assert_equal_s("--tags", opt);

	git_remote_set_autotag(_remote, GIT_REMOTE_DOWNLOAD_TAGS_NONE);
	cl_git_pass(git_remote_save(_remote));
	cl_git_pass(git_config_get_string(&opt, cfg, "remote.test.tagopt"));
	cl_assert_equal_s("--no-tags", opt);

	git_remote_set_autotag(_remote, GIT_REMOTE_DOWNLOAD_TAGS_AUTO);
	cl_git_pass(git_remote_save(_remote));
	cl_assert(git_config_get_string(&opt, cfg, "remote.test.tagopt") == GIT_ENOTFOUND);

	git_config_free(cfg);
}

void test_network_remote_remotes__can_load_with_an_empty_url(void)
{
	git_remote *remote = NULL;

	cl_git_pass(git_remote_load(&remote, _repo, "empty-remote-url"));

	cl_assert(remote->url == NULL);
	cl_assert(remote->pushurl == NULL);

	cl_git_fail(git_remote_connect(remote, GIT_DIRECTION_FETCH));

	cl_assert(giterr_last() != NULL);
	cl_assert(giterr_last()->klass == GITERR_INVALID);

	git_remote_free(remote);
}

void test_network_remote_remotes__can_load_with_only_an_empty_pushurl(void)
{
	git_remote *remote = NULL;

	cl_git_pass(git_remote_load(&remote, _repo, "empty-remote-pushurl"));

	cl_assert(remote->url == NULL);
	cl_assert(remote->pushurl == NULL);

	cl_git_fail(git_remote_connect(remote, GIT_DIRECTION_FETCH));

	git_remote_free(remote);
}

void test_network_remote_remotes__returns_ENOTFOUND_when_neither_url_nor_pushurl(void)
{
	git_remote *remote = NULL;

	cl_git_fail_with(
		git_remote_load(&remote, _repo, "no-remote-url"), GIT_ENOTFOUND);
}

void test_network_remote_remotes__check_structure_version(void)
{
	git_transport transport = GIT_TRANSPORT_INIT;
	const git_error *err;

	git_remote_free(_remote);
	_remote = NULL;
	cl_git_pass(git_remote_create_anonymous(&_remote, _repo, "test-protocol://localhost", NULL));

	transport.version = 0;
	cl_git_fail(git_remote_set_transport(_remote, &transport));
	err = giterr_last();
	cl_assert_equal_i(GITERR_INVALID, err->klass);

	giterr_clear();
	transport.version = 1024;
	cl_git_fail(git_remote_set_transport(_remote, &transport));
	err = giterr_last();
	cl_assert_equal_i(GITERR_INVALID, err->klass);
}

void assert_cannot_create_remote(const char *name, int expected_error)
{
	git_remote *remote = NULL;

	cl_git_fail_with(
		git_remote_create(&remote, _repo, name, "git://github.com/libgit2/libgit2"),
		expected_error);

	cl_assert_equal_p(remote, NULL);
}

void test_network_remote_remotes__cannot_create_a_remote_which_name_conflicts_with_an_existing_remote(void)
{
	assert_cannot_create_remote("test", GIT_EEXISTS);
}

void test_network_remote_remotes__cannot_create_a_remote_which_name_is_invalid(void)
{
	assert_cannot_create_remote("/", GIT_EINVALIDSPEC);
	assert_cannot_create_remote("//", GIT_EINVALIDSPEC);
	assert_cannot_create_remote(".lock", GIT_EINVALIDSPEC);
	assert_cannot_create_remote("a.lock", GIT_EINVALIDSPEC);
}

void test_network_remote_remote__git_remote_create_with_fetchspec(void)
{
	git_remote *remote;
	git_strarray array;

	cl_git_pass(git_remote_create_with_fetchspec(&remote, _repo, "test-new", "git://github.com/libgit2/libgit2", "+refs/*:refs/*"));
	git_remote_get_fetch_refspecs(&array, remote);
	cl_assert_equal_s("+refs/*:refs/*", array.strings[0]);
	git_remote_free(remote);
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

	cl_git_pass(git_remote_create_anonymous(&remote, _repo, "git://github.com/libgit2/libgit2", NULL));

	for (i = 0; i < 3; i++) {
		cl_git_pass(git_remote_add_fetch(remote, fetch_refspecs[i]));
		cl_git_pass(git_remote_add_push(remote, push_refspecs[i]));
	}

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
}
