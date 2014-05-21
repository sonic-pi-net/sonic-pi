#include "clar_libgit2.h"

#include "fileops.h"
#include "fetchhead.h"
#include "../fetchhead/fetchhead_data.h"
#include "git2/clone.h"

#define LIVE_REPO_URL "git://github.com/libgit2/TestGitRepository"

static git_repository *g_repo;
static git_clone_options g_options;

void test_online_fetchhead__initialize(void)
{
	git_remote_callbacks dummy_callbacks = GIT_REMOTE_CALLBACKS_INIT;
	g_repo = NULL;

	memset(&g_options, 0, sizeof(git_clone_options));
	g_options.version = GIT_CLONE_OPTIONS_VERSION;
	g_options.remote_callbacks = dummy_callbacks;
}

void test_online_fetchhead__cleanup(void)
{
	if (g_repo) {
		git_repository_free(g_repo);
		g_repo = NULL;
	}

	cl_fixture_cleanup("./foo");
}

static void fetchhead_test_clone(void)
{
	cl_git_pass(git_clone(&g_repo, LIVE_REPO_URL, "./foo", &g_options));
}

static void fetchhead_test_fetch(const char *fetchspec, const char *expected_fetchhead)
{
	git_remote *remote;
	git_buf fetchhead_buf = GIT_BUF_INIT;
	int equals = 0;

	cl_git_pass(git_remote_load(&remote, g_repo, "origin"));
	git_remote_set_autotag(remote, GIT_REMOTE_DOWNLOAD_TAGS_AUTO);

	if(fetchspec != NULL) {
		git_remote_clear_refspecs(remote);
		git_remote_add_fetch(remote, fetchspec);
	}

	cl_git_pass(git_remote_connect(remote, GIT_DIRECTION_FETCH));
	cl_git_pass(git_remote_download(remote));
	cl_git_pass(git_remote_update_tips(remote, NULL, NULL));
	git_remote_disconnect(remote);
	git_remote_free(remote);

	cl_git_pass(git_futils_readbuffer(&fetchhead_buf, "./foo/.git/FETCH_HEAD"));

	equals = (strcmp(fetchhead_buf.ptr, expected_fetchhead) == 0);

	git_buf_free(&fetchhead_buf);

	cl_assert(equals);
}

void test_online_fetchhead__wildcard_spec(void)
{
	fetchhead_test_clone();
	fetchhead_test_fetch(NULL, FETCH_HEAD_WILDCARD_DATA);
}

void test_online_fetchhead__explicit_spec(void)
{
	fetchhead_test_clone();
	fetchhead_test_fetch("refs/heads/first-merge:refs/remotes/origin/first-merge", FETCH_HEAD_EXPLICIT_DATA);
}

void test_online_fetchhead__no_merges(void)
{
	git_config *config;

	fetchhead_test_clone();

	cl_git_pass(git_repository_config(&config, g_repo));
	cl_git_pass(git_config_delete_entry(config, "branch.master.remote"));
	cl_git_pass(git_config_delete_entry(config, "branch.master.merge"));
	git_config_free(config);

	fetchhead_test_fetch(NULL, FETCH_HEAD_NO_MERGE_DATA);
}
