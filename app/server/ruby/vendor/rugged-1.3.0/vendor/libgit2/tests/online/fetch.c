#include "clar_libgit2.h"

static git_repository *_repo;
static int counter;

static char *_remote_proxy_scheme = NULL;
static char *_remote_proxy_host = NULL;
static char *_remote_proxy_user = NULL;
static char *_remote_proxy_pass = NULL;

void test_online_fetch__initialize(void)
{
	cl_git_pass(git_repository_init(&_repo, "./fetch", 0));

    _remote_proxy_scheme = cl_getenv("GITTEST_REMOTE_PROXY_SCHEME");
    _remote_proxy_host = cl_getenv("GITTEST_REMOTE_PROXY_HOST");
    _remote_proxy_user = cl_getenv("GITTEST_REMOTE_PROXY_USER");
    _remote_proxy_pass = cl_getenv("GITTEST_REMOTE_PROXY_PASS");
}

void test_online_fetch__cleanup(void)
{
	git_repository_free(_repo);
	_repo = NULL;

	cl_fixture_cleanup("./fetch");

    git__free(_remote_proxy_scheme);
    git__free(_remote_proxy_host);
    git__free(_remote_proxy_user);
    git__free(_remote_proxy_pass);
}

static int update_tips(const char *refname, const git_oid *a, const git_oid *b, void *data)
{
	GIT_UNUSED(refname); GIT_UNUSED(a); GIT_UNUSED(b); GIT_UNUSED(data);

	++counter;

	return 0;
}

static int progress(const git_indexer_progress *stats, void *payload)
{
	size_t *bytes_received = (size_t *)payload;
	*bytes_received = stats->received_bytes;
	return 0;
}

static void do_fetch(const char *url, git_remote_autotag_option_t flag, int n)
{
	git_remote *remote;
	git_fetch_options options = GIT_FETCH_OPTIONS_INIT;
	size_t bytes_received = 0;

	options.callbacks.transfer_progress = progress;
	options.callbacks.update_tips = update_tips;
	options.callbacks.payload = &bytes_received;
	options.download_tags = flag;
	counter = 0;

	cl_git_pass(git_remote_create(&remote, _repo, "test", url));
	cl_git_pass(git_remote_fetch(remote, NULL, &options, NULL));
	cl_assert_equal_i(counter, n);
	cl_assert(bytes_received > 0);

	git_remote_free(remote);
}

void test_online_fetch__default_git(void)
{
	do_fetch("git://github.com/libgit2/TestGitRepository.git", GIT_REMOTE_DOWNLOAD_TAGS_AUTO, 6);
}

void test_online_fetch__default_http(void)
{
	do_fetch("http://github.com/libgit2/TestGitRepository.git", GIT_REMOTE_DOWNLOAD_TAGS_AUTO, 6);
}

void test_online_fetch__default_https(void)
{
	do_fetch("https://github.com/libgit2/TestGitRepository.git", GIT_REMOTE_DOWNLOAD_TAGS_AUTO, 6);
}

void test_online_fetch__no_tags_git(void)
{
	do_fetch("git://github.com/libgit2/TestGitRepository.git", GIT_REMOTE_DOWNLOAD_TAGS_NONE, 3);
}

void test_online_fetch__no_tags_http(void)
{
	do_fetch("http://github.com/libgit2/TestGitRepository.git", GIT_REMOTE_DOWNLOAD_TAGS_NONE, 3);
}

void test_online_fetch__fetch_twice(void)
{
	git_remote *remote;
	cl_git_pass(git_remote_create(&remote, _repo, "test", "git://github.com/libgit2/TestGitRepository.git"));
	cl_git_pass(git_remote_connect(remote, GIT_DIRECTION_FETCH, NULL, NULL, NULL));
	cl_git_pass(git_remote_download(remote, NULL, NULL));
    	git_remote_disconnect(remote);

	git_remote_connect(remote, GIT_DIRECTION_FETCH, NULL, NULL, NULL);
	cl_git_pass(git_remote_download(remote, NULL, NULL));
	git_remote_disconnect(remote);

	git_remote_free(remote);
}

static int transferProgressCallback(const git_indexer_progress *stats, void *payload)
{
	bool *invoked = (bool *)payload;

	GIT_UNUSED(stats);
	*invoked = true;
	return 0;
}

void test_online_fetch__doesnt_retrieve_a_pack_when_the_repository_is_up_to_date(void)
{
	git_repository *_repository;
	bool invoked = false;
	git_remote *remote;
	git_fetch_options options = GIT_FETCH_OPTIONS_INIT;
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	opts.bare = true;

	cl_git_pass(git_clone(&_repository, "https://github.com/libgit2/TestGitRepository.git",
				"./fetch/lg2", &opts));
	git_repository_free(_repository);

	cl_git_pass(git_repository_open(&_repository, "./fetch/lg2"));

	cl_git_pass(git_remote_lookup(&remote, _repository, "origin"));
	cl_git_pass(git_remote_connect(remote, GIT_DIRECTION_FETCH, NULL, NULL, NULL));

	cl_assert_equal_i(false, invoked);

	options.callbacks.transfer_progress = &transferProgressCallback;
	options.callbacks.payload = &invoked;
	cl_git_pass(git_remote_download(remote, NULL, &options));

	cl_assert_equal_i(false, invoked);

	cl_git_pass(git_remote_update_tips(remote, &options.callbacks, 1, options.download_tags, NULL));
	git_remote_disconnect(remote);

	git_remote_free(remote);
	git_repository_free(_repository);
}

static int cancel_at_half(const git_indexer_progress *stats, void *payload)
{
	GIT_UNUSED(payload);

	if (stats->received_objects > (stats->total_objects/2))
		return -4321;
	return 0;
}

void test_online_fetch__can_cancel(void)
{
	git_remote *remote;
	size_t bytes_received = 0;
	git_fetch_options options = GIT_FETCH_OPTIONS_INIT;

	cl_git_pass(git_remote_create(&remote, _repo, "test",
				"http://github.com/libgit2/TestGitRepository.git"));

	options.callbacks.transfer_progress = cancel_at_half;
	options.callbacks.payload = &bytes_received;

	cl_git_pass(git_remote_connect(remote, GIT_DIRECTION_FETCH, NULL, NULL, NULL));
	cl_git_fail_with(git_remote_download(remote, NULL, &options), -4321);
	git_remote_disconnect(remote);
	git_remote_free(remote);
}

void test_online_fetch__ls_disconnected(void)
{
	const git_remote_head **refs;
	size_t refs_len_before, refs_len_after;
	git_remote *remote;

	cl_git_pass(git_remote_create(&remote, _repo, "test",
				"http://github.com/libgit2/TestGitRepository.git"));
	cl_git_pass(git_remote_connect(remote, GIT_DIRECTION_FETCH, NULL, NULL, NULL));
	cl_git_pass(git_remote_ls(&refs, &refs_len_before, remote));
	git_remote_disconnect(remote);
	cl_git_pass(git_remote_ls(&refs, &refs_len_after, remote));

	cl_assert_equal_i(refs_len_before, refs_len_after);

	git_remote_free(remote);
}

void test_online_fetch__remote_symrefs(void)
{
	const git_remote_head **refs;
	size_t refs_len;
	git_remote *remote;

	cl_git_pass(git_remote_create(&remote, _repo, "test",
				"http://github.com/libgit2/TestGitRepository.git"));
	cl_git_pass(git_remote_connect(remote, GIT_DIRECTION_FETCH, NULL, NULL, NULL));
	git_remote_disconnect(remote);
	cl_git_pass(git_remote_ls(&refs, &refs_len, remote));

	cl_assert_equal_s("HEAD", refs[0]->name);
	cl_assert_equal_s("refs/heads/master", refs[0]->symref_target);

	git_remote_free(remote);
}

void test_online_fetch__twice(void)
{
	git_remote *remote;

	cl_git_pass(git_remote_create(&remote, _repo, "test", "http://github.com/libgit2/TestGitRepository.git"));
	cl_git_pass(git_remote_fetch(remote, NULL, NULL, NULL));
	cl_git_pass(git_remote_fetch(remote, NULL, NULL, NULL));

	git_remote_free(remote);
}

void test_online_fetch__proxy(void)
{
    git_remote *remote;
    git_buf url = GIT_BUF_INIT;
    git_fetch_options fetch_opts;

    if (!_remote_proxy_host || !_remote_proxy_user || !_remote_proxy_pass)
        cl_skip();

    cl_git_pass(git_buf_printf(&url, "%s://%s:%s@%s/",
        _remote_proxy_scheme ? _remote_proxy_scheme : "http",
        _remote_proxy_user, _remote_proxy_pass, _remote_proxy_host));

    cl_git_pass(git_fetch_options_init(&fetch_opts, GIT_FETCH_OPTIONS_VERSION));
    fetch_opts.proxy_opts.type = GIT_PROXY_SPECIFIED;
    fetch_opts.proxy_opts.url = url.ptr;

    cl_git_pass(git_remote_create(&remote, _repo, "test", "https://github.com/libgit2/TestGitRepository.git"));
    cl_git_pass(git_remote_connect(remote, GIT_DIRECTION_FETCH, NULL, &fetch_opts.proxy_opts, NULL));
    cl_git_pass(git_remote_fetch(remote, NULL, &fetch_opts, NULL));

    git_remote_free(remote);
    git_buf_dispose(&url);
}
