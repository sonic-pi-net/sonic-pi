#include "clar_libgit2.h"

#include "futils.h"
#include "fetchhead.h"
#include "../fetchhead/fetchhead_data.h"
#include "git2/clone.h"

#define LIVE_REPO_URL "https://github.com/libgit2/TestGitRepository"

static git_repository *g_repo;
static git_clone_options g_options;

void test_online_fetchhead__initialize(void)
{
	git_fetch_options dummy_fetch = GIT_FETCH_OPTIONS_INIT;
	g_repo = NULL;

	memset(&g_options, 0, sizeof(git_clone_options));
	g_options.version = GIT_CLONE_OPTIONS_VERSION;
	g_options.fetch_opts = dummy_fetch;
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

static size_t count_references(void)
{
	git_strarray array;
	size_t refs;

	cl_git_pass(git_reference_list(&array, g_repo));
	refs = array.count;

	git_strarray_dispose(&array);

	return refs;
}

static void fetchhead_test_fetch(const char *fetchspec, const char *expected_fetchhead)
{
	git_remote *remote;
	git_fetch_options fetch_opts = GIT_FETCH_OPTIONS_INIT;
	git_str fetchhead_buf = GIT_STR_INIT;
	git_strarray array, *active_refs = NULL;

	cl_git_pass(git_remote_lookup(&remote, g_repo, "origin"));
	fetch_opts.download_tags = GIT_REMOTE_DOWNLOAD_TAGS_AUTO;

	if(fetchspec != NULL) {
		array.count = 1;
		array.strings = (char **) &fetchspec;
		active_refs = &array;
	}

	cl_git_pass(git_remote_fetch(remote, active_refs, &fetch_opts, NULL));
	git_remote_free(remote);

	cl_git_pass(git_futils_readbuffer(&fetchhead_buf, "./foo/.git/FETCH_HEAD"));

	cl_assert_equal_s(fetchhead_buf.ptr, expected_fetchhead);
	git_str_dispose(&fetchhead_buf);
}

void test_online_fetchhead__wildcard_spec(void)
{
	fetchhead_test_clone();
	fetchhead_test_fetch(NULL, FETCH_HEAD_WILDCARD_DATA2);
	cl_git_pass(git_tag_delete(g_repo, "annotated_tag"));
	cl_git_pass(git_tag_delete(g_repo, "blob"));
	cl_git_pass(git_tag_delete(g_repo, "commit_tree"));
	cl_git_pass(git_tag_delete(g_repo, "nearly-dangling"));
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

	fetchhead_test_fetch(NULL, FETCH_HEAD_NO_MERGE_DATA2);
	cl_git_pass(git_tag_delete(g_repo, "annotated_tag"));
	cl_git_pass(git_tag_delete(g_repo, "blob"));
	cl_git_pass(git_tag_delete(g_repo, "commit_tree"));
	cl_git_pass(git_tag_delete(g_repo, "nearly-dangling"));
	fetchhead_test_fetch(NULL, FETCH_HEAD_NO_MERGE_DATA);
	cl_git_pass(git_tag_delete(g_repo, "commit_tree"));
	fetchhead_test_fetch(NULL, FETCH_HEAD_NO_MERGE_DATA3);
}

void test_online_fetchhead__explicit_dst_refspec_creates_branch(void)
{
	git_reference *ref;
	size_t refs;

	fetchhead_test_clone();
	refs = count_references();
	fetchhead_test_fetch("refs/heads/first-merge:refs/heads/explicit-refspec", FETCH_HEAD_EXPLICIT_DATA);

	cl_git_pass(git_branch_lookup(&ref, g_repo, "explicit-refspec", GIT_BRANCH_ALL));
	cl_assert_equal_i(refs + 1, count_references());

	git_reference_free(ref);
}

void test_online_fetchhead__empty_dst_refspec_creates_no_branch(void)
{
	git_reference *ref;
	size_t refs;

	fetchhead_test_clone();
	refs = count_references();

	fetchhead_test_fetch("refs/heads/first-merge", FETCH_HEAD_EXPLICIT_DATA);
	cl_git_fail(git_branch_lookup(&ref, g_repo, "first-merge", GIT_BRANCH_ALL));

	cl_assert_equal_i(refs, count_references());
}

void test_online_fetchhead__colon_only_dst_refspec_creates_no_branch(void)
{
	size_t refs;

	fetchhead_test_clone();
	refs = count_references();
	fetchhead_test_fetch("refs/heads/first-merge:", FETCH_HEAD_EXPLICIT_DATA);

	cl_assert_equal_i(refs, count_references());
}

void test_online_fetchhead__creds_get_stripped(void)
{
	git_str buf = GIT_STR_INIT;
	git_remote *remote;

	cl_git_pass(git_repository_init(&g_repo, "./foo", 0));
	cl_git_pass(git_remote_create_anonymous(&remote, g_repo, "https://foo:bar@github.com/libgit2/TestGitRepository"));
	cl_git_pass(git_remote_fetch(remote, NULL, NULL, NULL));

	cl_git_pass(git_futils_readbuffer(&buf, "./foo/.git/FETCH_HEAD"));
	cl_assert_equal_s(buf.ptr,
		"49322bb17d3acc9146f98c97d078513228bbf3c0\t\thttps://github.com/libgit2/TestGitRepository\n");

	git_remote_free(remote);
	git_str_dispose(&buf);
}
