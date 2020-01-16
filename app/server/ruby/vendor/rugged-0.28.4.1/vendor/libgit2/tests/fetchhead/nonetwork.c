#include "clar_libgit2.h"

#include "futils.h"
#include "fetchhead.h"

#include "fetchhead_data.h"

#define DO_LOCAL_TEST 0

static git_repository *g_repo;

void test_fetchhead_nonetwork__initialize(void)
{
	g_repo = NULL;
}

static void cleanup_repository(void *path)
{
	if (g_repo) {
		git_repository_free(g_repo);
		g_repo = NULL;
	}

	cl_fixture_cleanup((const char *)path);
}

static void populate_fetchhead(git_vector *out, git_repository *repo)
{
	git_fetchhead_ref *fetchhead_ref;
	git_oid oid;

	cl_git_pass(git_oid_fromstr(&oid,
		"49322bb17d3acc9146f98c97d078513228bbf3c0"));
	cl_git_pass(git_fetchhead_ref_create(&fetchhead_ref, &oid, 1,
		"refs/heads/master",
		"git://github.com/libgit2/TestGitRepository"));
	cl_git_pass(git_vector_insert(out, fetchhead_ref));

	cl_git_pass(git_oid_fromstr(&oid,
		"0966a434eb1a025db6b71485ab63a3bfbea520b6"));
	cl_git_pass(git_fetchhead_ref_create(&fetchhead_ref, &oid, 0,
		"refs/heads/first-merge",
		"git://github.com/libgit2/TestGitRepository"));
	cl_git_pass(git_vector_insert(out, fetchhead_ref));

	cl_git_pass(git_oid_fromstr(&oid,
		"42e4e7c5e507e113ebbb7801b16b52cf867b7ce1"));
	cl_git_pass(git_fetchhead_ref_create(&fetchhead_ref, &oid, 0,
		"refs/heads/no-parent",
		"git://github.com/libgit2/TestGitRepository"));
	cl_git_pass(git_vector_insert(out, fetchhead_ref));

	cl_git_pass(git_oid_fromstr(&oid,
		"d96c4e80345534eccee5ac7b07fc7603b56124cb"));
	cl_git_pass(git_fetchhead_ref_create(&fetchhead_ref, &oid, 0,
		"refs/tags/annotated_tag",
		"git://github.com/libgit2/TestGitRepository"));
	cl_git_pass(git_vector_insert(out, fetchhead_ref));

	cl_git_pass(git_oid_fromstr(&oid,
		"55a1a760df4b86a02094a904dfa511deb5655905"));
	cl_git_pass(git_fetchhead_ref_create(&fetchhead_ref, &oid, 0,
		"refs/tags/blob",
		"git://github.com/libgit2/TestGitRepository"));
	cl_git_pass(git_vector_insert(out, fetchhead_ref));

	cl_git_pass(git_oid_fromstr(&oid,
		"8f50ba15d49353813cc6e20298002c0d17b0a9ee"));
	cl_git_pass(git_fetchhead_ref_create(&fetchhead_ref, &oid, 0,
		"refs/tags/commit_tree",
		"git://github.com/libgit2/TestGitRepository"));
	cl_git_pass(git_vector_insert(out, fetchhead_ref));

	cl_git_pass(git_fetchhead_write(repo, out));
}

void test_fetchhead_nonetwork__write(void)
{
	git_vector fetchhead_vector = GIT_VECTOR_INIT;
	git_fetchhead_ref *fetchhead_ref;
	git_buf fetchhead_buf = GIT_BUF_INIT;
	int equals = 0;
	size_t i;

	git_vector_init(&fetchhead_vector, 6, NULL);

	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	populate_fetchhead(&fetchhead_vector, g_repo);

	cl_git_pass(git_futils_readbuffer(&fetchhead_buf,
		"./test1/.git/FETCH_HEAD"));

	equals = (strcmp(fetchhead_buf.ptr, FETCH_HEAD_WILDCARD_DATA_LOCAL) == 0);

	git_buf_dispose(&fetchhead_buf);

	git_vector_foreach(&fetchhead_vector, i, fetchhead_ref) {
		git_fetchhead_ref_free(fetchhead_ref);
	}

	git_vector_free(&fetchhead_vector);

	cl_assert(equals);
}

typedef struct {
	git_vector *fetchhead_vector;
	size_t idx;
} fetchhead_ref_cb_data; 

static int fetchhead_ref_cb(const char *name, const char *url,
	const git_oid *oid, unsigned int is_merge, void *payload)
{
	fetchhead_ref_cb_data *cb_data = payload;
	git_fetchhead_ref *expected;

	cl_assert(payload);

	expected = git_vector_get(cb_data->fetchhead_vector, cb_data->idx);

	cl_assert_equal_oid(&expected->oid, oid);
	cl_assert(expected->is_merge == is_merge);

	if (expected->ref_name)
		cl_assert_equal_s(expected->ref_name, name);
	else
		cl_assert(name == NULL);

	if (expected->remote_url)
		cl_assert_equal_s(expected->remote_url, url);
	else
		cl_assert(url == NULL);

	cb_data->idx++;

	return 0;
}

void test_fetchhead_nonetwork__read(void)
{
	git_vector fetchhead_vector = GIT_VECTOR_INIT;
	git_fetchhead_ref *fetchhead_ref;
	fetchhead_ref_cb_data cb_data;
	size_t i;

	memset(&cb_data, 0x0, sizeof(fetchhead_ref_cb_data));

	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	populate_fetchhead(&fetchhead_vector, g_repo);

	cb_data.fetchhead_vector = &fetchhead_vector;

	cl_git_pass(git_repository_fetchhead_foreach(g_repo, fetchhead_ref_cb, &cb_data));

	git_vector_foreach(&fetchhead_vector, i, fetchhead_ref) {
		git_fetchhead_ref_free(fetchhead_ref);
	}

	git_vector_free(&fetchhead_vector);
}

static int read_old_style_cb(const char *name, const char *url,
	const git_oid *oid, unsigned int is_merge, void *payload)
{
	git_oid expected;

	GIT_UNUSED(payload);

	git_oid_fromstr(&expected, "49322bb17d3acc9146f98c97d078513228bbf3c0");

	cl_assert(name == NULL);
	cl_assert(url == NULL);
	cl_assert_equal_oid(&expected, oid);
	cl_assert(is_merge == 1);

	return 0;
}

void test_fetchhead_nonetwork__read_old_style(void)
{
	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	cl_git_rewritefile("./test1/.git/FETCH_HEAD", "49322bb17d3acc9146f98c97d078513228bbf3c0\n");

	cl_git_pass(git_repository_fetchhead_foreach(g_repo, read_old_style_cb, NULL));
}

static int read_type_missing(const char *ref_name, const char *remote_url,
	const git_oid *oid, unsigned int is_merge, void *payload)
{
	git_oid expected;

	GIT_UNUSED(payload);

	git_oid_fromstr(&expected, "49322bb17d3acc9146f98c97d078513228bbf3c0");

	cl_assert_equal_s("name", ref_name);
	cl_assert_equal_s("remote_url", remote_url);
	cl_assert_equal_oid(&expected, oid);
	cl_assert(is_merge == 0);

	return 0;
}

void test_fetchhead_nonetwork__type_missing(void)
{
	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	cl_git_rewritefile("./test1/.git/FETCH_HEAD", "49322bb17d3acc9146f98c97d078513228bbf3c0\tnot-for-merge\t'name' of remote_url\n");

	cl_git_pass(git_repository_fetchhead_foreach(g_repo, read_type_missing, NULL));
}

static int read_name_missing(const char *ref_name, const char *remote_url,
	const git_oid *oid, unsigned int is_merge, void *payload)
{
	git_oid expected;

	GIT_UNUSED(payload);

	git_oid_fromstr(&expected, "49322bb17d3acc9146f98c97d078513228bbf3c0");

	cl_assert(ref_name == NULL);
	cl_assert_equal_s("remote_url", remote_url);
	cl_assert_equal_oid(&expected, oid);
	cl_assert(is_merge == 0);

	return 0;
}

void test_fetchhead_nonetwork__name_missing(void)
{
	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	cl_git_rewritefile("./test1/.git/FETCH_HEAD", "49322bb17d3acc9146f98c97d078513228bbf3c0\tnot-for-merge\tremote_url\n");

	cl_git_pass(git_repository_fetchhead_foreach(g_repo, read_name_missing, NULL));
}

static int read_noop(const char *ref_name, const char *remote_url,
	const git_oid *oid, unsigned int is_merge, void *payload)
{
	GIT_UNUSED(ref_name);
	GIT_UNUSED(remote_url);
	GIT_UNUSED(oid);
	GIT_UNUSED(is_merge);
	GIT_UNUSED(payload);

	return 0;
}

void test_fetchhead_nonetwork__nonexistent(void)
{
	int error;

	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	cl_git_fail((error = git_repository_fetchhead_foreach(g_repo, read_noop, NULL)));
	cl_assert(error == GIT_ENOTFOUND);
}

void test_fetchhead_nonetwork__invalid_unterminated_last_line(void)
{
	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	cl_git_rewritefile("./test1/.git/FETCH_HEAD", "unterminated");
	cl_git_fail(git_repository_fetchhead_foreach(g_repo, read_noop, NULL));
}

void test_fetchhead_nonetwork__invalid_oid(void)
{
	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	cl_git_rewritefile("./test1/.git/FETCH_HEAD", "shortoid\n");
	cl_git_fail(git_repository_fetchhead_foreach(g_repo, read_noop, NULL));
}

void test_fetchhead_nonetwork__invalid_for_merge(void)
{
	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	cl_git_rewritefile("./test1/.git/FETCH_HEAD", "49322bb17d3acc9146f98c97d078513228bbf3c0\tinvalid-merge\t\n");
	cl_git_fail(git_repository_fetchhead_foreach(g_repo, read_noop, NULL));

	cl_assert(git__prefixcmp(git_error_last()->message, "invalid for-merge") == 0);
}

void test_fetchhead_nonetwork__invalid_description(void)
{
	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	cl_git_rewritefile("./test1/.git/FETCH_HEAD", "49322bb17d3acc9146f98c97d078513228bbf3c0\tnot-for-merge\n");
	cl_git_fail(git_repository_fetchhead_foreach(g_repo, read_noop, NULL));

	cl_assert(git__prefixcmp(git_error_last()->message, "invalid description") == 0);
}

static int assert_master_for_merge(const char *ref, const char *url, const git_oid *id, unsigned int is_merge, void *data)
{
	GIT_UNUSED(url);
	GIT_UNUSED(id);
	GIT_UNUSED(data);

	if (!strcmp("refs/heads/master", ref) && !is_merge)
		return -1;

	return 0;
}

void test_fetchhead_nonetwork__unborn_with_upstream(void)
{
	git_repository *repo;
	git_remote *remote;

	/* Create an empty repo to clone from */
	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));
	cl_set_cleanup(&cleanup_repository, "./repowithunborn");
	cl_git_pass(git_clone(&repo, "./test1", "./repowithunborn", NULL));

	/* Simulate someone pushing to it by changing to one that has stuff */
	cl_git_pass(git_remote_set_url(repo, "origin", cl_fixture("testrepo.git")));
	cl_git_pass(git_remote_lookup(&remote, repo, "origin"));

	cl_git_pass(git_remote_fetch(remote, NULL, NULL, NULL));
	git_remote_free(remote);

	cl_git_pass(git_repository_fetchhead_foreach(repo, assert_master_for_merge, NULL));

	git_repository_free(repo);
	cl_fixture_cleanup("./repowithunborn");
}

void test_fetchhead_nonetwork__fetch_into_repo_with_symrefs(void)
{
	git_repository *repo;
	git_remote *remote;
	git_reference *symref;

	repo = cl_git_sandbox_init("empty_standard_repo");

	/*
	 * Testing for a specific constellation where the repository has at
	 * least one symbolic reference in its refdb.
	 */
	cl_git_pass(git_reference_symbolic_create(&symref, repo, "refs/heads/symref", "refs/heads/master", 0, NULL));

	cl_git_pass(git_remote_set_url(repo, "origin", cl_fixture("testrepo.git")));
	cl_git_pass(git_remote_lookup(&remote, repo, "origin"));
	cl_git_pass(git_remote_fetch(remote, NULL, NULL, NULL));

	git_remote_free(remote);
	git_reference_free(symref);
	cl_git_sandbox_cleanup();
}

void test_fetchhead_nonetwork__quote_in_branch_name(void)
{
	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	cl_git_rewritefile("./test1/.git/FETCH_HEAD", FETCH_HEAD_QUOTE_DATA);
	cl_git_pass(git_repository_fetchhead_foreach(g_repo, read_noop, NULL));
}

static bool found_master;
static bool found_haacked;
static bool find_master_haacked_called;

int find_master_haacked(const char *ref_name, const char *remote_url, const git_oid *oid, unsigned int is_merge, void *payload)
{
	GIT_UNUSED(remote_url);
	GIT_UNUSED(oid);
	GIT_UNUSED(payload);

	find_master_haacked_called = true;

	if (!strcmp("refs/heads/master", ref_name)) {
		cl_assert(is_merge);
		found_master = true;
	}
	if (!strcmp("refs/heads/haacked", ref_name)) {
		cl_assert(is_merge);
		found_haacked = true;
	}

	return 0;
}

void test_fetchhead_nonetwork__create_when_refpecs_given(void)
{
	git_remote *remote;
	git_buf path = GIT_BUF_INIT;
	char *refspec1 = "refs/heads/master";
	char *refspec2 = "refs/heads/haacked";
	char *refspecs[] = { refspec1, refspec2 };
	git_strarray specs = {
		refspecs,
		2,
	};

	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	cl_git_pass(git_buf_joinpath(&path, git_repository_path(g_repo), "FETCH_HEAD"));
	cl_git_pass(git_remote_create(&remote, g_repo, "origin", cl_fixture("testrepo.git")));

	cl_assert(!git_path_exists(path.ptr));
	cl_git_pass(git_remote_fetch(remote, &specs, NULL, NULL));
	cl_assert(git_path_exists(path.ptr));

	cl_git_pass(git_repository_fetchhead_foreach(g_repo, find_master_haacked, NULL));
	cl_assert(find_master_haacked_called);
	cl_assert(found_master);
	cl_assert(found_haacked);

	git_remote_free(remote);
	git_buf_dispose(&path);
}

static bool count_refs_called;
struct prefix_count {
	const char *prefix;
	int count;
	int expected;
};

int count_refs(const char *ref_name, const char *remote_url, const git_oid *oid, unsigned int is_merge, void *payload)
{
	int i;
	struct prefix_count *prefix_counts = (struct prefix_count *) payload;

	GIT_UNUSED(remote_url);
	GIT_UNUSED(oid);
	GIT_UNUSED(is_merge);

	count_refs_called = true;

	for (i = 0; prefix_counts[i].prefix; i++) {
		if (!git__prefixcmp(ref_name, prefix_counts[i].prefix))
			prefix_counts[i].count++;
	}

	return 0;
}

void test_fetchhead_nonetwork__create_with_multiple_refspecs(void)
{
	git_remote *remote;
	git_buf path = GIT_BUF_INIT;

	cl_set_cleanup(&cleanup_repository, "./test1");
	cl_git_pass(git_repository_init(&g_repo, "./test1", 0));

	cl_git_pass(git_remote_create(&remote, g_repo, "origin", cl_fixture("testrepo.git")));
	git_remote_free(remote);
	cl_git_pass(git_remote_add_fetch(g_repo, "origin", "+refs/notes/*:refs/origin/notes/*"));
	/* Pick up the new refspec */
	cl_git_pass(git_remote_lookup(&remote, g_repo, "origin"));

	cl_git_pass(git_buf_joinpath(&path, git_repository_path(g_repo), "FETCH_HEAD"));
	cl_assert(!git_path_exists(path.ptr));
	cl_git_pass(git_remote_fetch(remote, NULL, NULL, NULL));
	cl_assert(git_path_exists(path.ptr));

	{
		int i;
		struct prefix_count prefix_counts[] = {
			{"refs/notes/", 0, 1},
			{"refs/heads/", 0, 12},
			{"refs/tags/", 0, 7},
			{NULL, 0, 0},
		};

		cl_git_pass(git_repository_fetchhead_foreach(g_repo, count_refs, &prefix_counts));
		cl_assert(count_refs_called);
		for (i = 0; prefix_counts[i].prefix; i++)
			cl_assert_equal_i(prefix_counts[i].expected, prefix_counts[i].count);
	}

	git_remote_free(remote);
	git_buf_dispose(&path);
}
