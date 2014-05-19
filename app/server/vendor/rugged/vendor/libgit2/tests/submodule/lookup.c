#include "clar_libgit2.h"
#include "submodule_helpers.h"
#include "git2/sys/repository.h"
#include "fileops.h"

static git_repository *g_repo = NULL;

void test_submodule_lookup__initialize(void)
{
	g_repo = setup_fixture_submod2();
}

void test_submodule_lookup__simple_lookup(void)
{
	assert_submodule_exists(g_repo, "sm_unchanged");

	/* lookup pending change in .gitmodules that is not in HEAD */
	assert_submodule_exists(g_repo, "sm_added_and_uncommited");

	/* lookup pending change in .gitmodules that is not in HEAD nor index */
	assert_submodule_exists(g_repo, "sm_gitmodules_only");

	/* lookup git repo subdir that is not added as submodule */
	refute_submodule_exists(g_repo, "not-submodule", GIT_EEXISTS);

	/* lookup existing directory that is not a submodule */
	refute_submodule_exists(g_repo, "just_a_dir", GIT_ENOTFOUND);

	/* lookup existing file that is not a submodule */
	refute_submodule_exists(g_repo, "just_a_file", GIT_ENOTFOUND);

	/* lookup non-existent item */
	refute_submodule_exists(g_repo, "no_such_file", GIT_ENOTFOUND);
}

void test_submodule_lookup__accessors(void)
{
	git_submodule *sm;
	const char *oid = "480095882d281ed676fe5b863569520e54a7d5c0";

	cl_git_pass(git_submodule_lookup(&sm, g_repo, "sm_unchanged"));
	cl_assert(git_submodule_owner(sm) == g_repo);
	cl_assert_equal_s("sm_unchanged", git_submodule_name(sm));
	cl_assert(git__suffixcmp(git_submodule_path(sm), "sm_unchanged") == 0);
	cl_assert(git__suffixcmp(git_submodule_url(sm), "/submod2_target") == 0);

	cl_assert(git_oid_streq(git_submodule_index_id(sm), oid) == 0);
	cl_assert(git_oid_streq(git_submodule_head_id(sm), oid) == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), oid) == 0);

	cl_assert(git_submodule_ignore(sm) == GIT_SUBMODULE_IGNORE_NONE);
	cl_assert(git_submodule_update(sm) == GIT_SUBMODULE_UPDATE_CHECKOUT);

	git_submodule_free(sm);


	cl_git_pass(git_submodule_lookup(&sm, g_repo, "sm_changed_head"));
	cl_assert_equal_s("sm_changed_head", git_submodule_name(sm));

	cl_assert(git_oid_streq(git_submodule_index_id(sm), oid) == 0);
	cl_assert(git_oid_streq(git_submodule_head_id(sm), oid) == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm),
		"3d9386c507f6b093471a3e324085657a3c2b4247") == 0);

	git_submodule_free(sm);


	cl_git_pass(git_submodule_lookup(&sm, g_repo, "sm_added_and_uncommited"));
	cl_assert_equal_s("sm_added_and_uncommited", git_submodule_name(sm));

	cl_assert(git_oid_streq(git_submodule_index_id(sm), oid) == 0);
	cl_assert(git_submodule_head_id(sm) == NULL);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), oid) == 0);

	git_submodule_free(sm);


	cl_git_pass(git_submodule_lookup(&sm, g_repo, "sm_missing_commits"));
	cl_assert_equal_s("sm_missing_commits", git_submodule_name(sm));

	cl_assert(git_oid_streq(git_submodule_index_id(sm), oid) == 0);
	cl_assert(git_oid_streq(git_submodule_head_id(sm), oid) == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm),
		"5e4963595a9774b90524d35a807169049de8ccad") == 0);

	git_submodule_free(sm);
}

typedef struct {
	int count;
} sm_lookup_data;

static int sm_lookup_cb(git_submodule *sm, const char *name, void *payload)
{
	sm_lookup_data *data = payload;
	data->count += 1;
	cl_assert_equal_s(git_submodule_name(sm), name);
	return 0;
}

void test_submodule_lookup__foreach(void)
{
	sm_lookup_data data;
	memset(&data, 0, sizeof(data));
	cl_git_pass(git_submodule_foreach(g_repo, sm_lookup_cb, &data));
	cl_assert_equal_i(8, data.count);
}

void test_submodule_lookup__lookup_even_with_unborn_head(void)
{
	git_reference *head;

	/* put us on an unborn branch */
	cl_git_pass(git_reference_symbolic_create(
		&head, g_repo, "HEAD", "refs/heads/garbage", 1, NULL, NULL));
	git_reference_free(head);

	test_submodule_lookup__simple_lookup(); /* baseline should still pass */
}

void test_submodule_lookup__lookup_even_with_missing_index(void)
{
	git_index *idx;

	/* give the repo an empty index */
	cl_git_pass(git_index_new(&idx));
	git_repository_set_index(g_repo, idx);
	git_index_free(idx);

	test_submodule_lookup__simple_lookup(); /* baseline should still pass */
}

static void baseline_tests(void)
{
	/* small baseline that should work even if we change the index or make
	 * commits from the index
	 */
	assert_submodule_exists(g_repo, "sm_unchanged");
	assert_submodule_exists(g_repo, "sm_gitmodules_only");
	refute_submodule_exists(g_repo, "not-submodule", GIT_EEXISTS);
}

static void add_submodule_with_commit(const char *name)
{
	git_submodule *sm;
	git_repository *smrepo;
	git_index *idx;
	git_buf p = GIT_BUF_INIT;

	cl_git_pass(git_submodule_add_setup(&sm, g_repo,
		"https://github.com/libgit2/libgit2.git", name, 1));

	assert_submodule_exists(g_repo, name);

	cl_git_pass(git_submodule_open(&smrepo, sm));
	cl_git_pass(git_repository_index(&idx, smrepo));

	cl_git_pass(git_buf_joinpath(&p, git_repository_workdir(smrepo), "file"));
	cl_git_mkfile(p.ptr, "new file");
	git_buf_free(&p);

	cl_git_pass(git_index_add_bypath(idx, "file"));
	cl_git_pass(git_index_write(idx));
	git_index_free(idx);

	cl_repo_commit_from_index(NULL, smrepo, NULL, 0, "initial commit");
	git_repository_free(smrepo);

	cl_git_pass(git_submodule_add_finalize(sm));

	git_submodule_free(sm);
}

void test_submodule_lookup__just_added(void)
{
	git_submodule *sm;
	git_buf snap1 = GIT_BUF_INIT, snap2 = GIT_BUF_INIT;
	git_reference *original_head = NULL;

	refute_submodule_exists(g_repo, "sm_just_added", GIT_ENOTFOUND);
	refute_submodule_exists(g_repo, "sm_just_added_2", GIT_ENOTFOUND);
	refute_submodule_exists(g_repo, "sm_just_added_idx", GIT_ENOTFOUND);
	refute_submodule_exists(g_repo, "sm_just_added_head", GIT_ENOTFOUND);
	refute_submodule_exists(g_repo, "mismatch_name", GIT_ENOTFOUND);
	refute_submodule_exists(g_repo, "mismatch_path", GIT_ENOTFOUND);
	baseline_tests();

	cl_git_pass(git_futils_readbuffer(&snap1, "submod2/.gitmodules"));
	cl_git_pass(git_repository_head(&original_head, g_repo));

	cl_git_pass(git_submodule_add_setup(&sm, g_repo,
		"https://github.com/libgit2/libgit2.git", "sm_just_added", 1));
	git_submodule_free(sm);
	assert_submodule_exists(g_repo, "sm_just_added");

	cl_git_pass(git_submodule_add_setup(&sm, g_repo,
		"https://github.com/libgit2/libgit2.git", "sm_just_added_2", 1));
	assert_submodule_exists(g_repo, "sm_just_added_2");
	cl_git_fail(git_submodule_add_finalize(sm)); /* fails if no HEAD */
	git_submodule_free(sm);

	add_submodule_with_commit("sm_just_added_head");
	cl_repo_commit_from_index(NULL, g_repo, NULL, 0, "commit new sm to head");
	assert_submodule_exists(g_repo, "sm_just_added_head");

	add_submodule_with_commit("sm_just_added_idx");
	assert_submodule_exists(g_repo, "sm_just_added_idx");

	cl_git_pass(git_futils_readbuffer(&snap2, "submod2/.gitmodules"));

	cl_git_append2file(
		"submod2/.gitmodules",
		"\n[submodule \"mismatch_name\"]\n"
		"\tpath = mismatch_path\n"
		"\turl = https://example.com/example.git\n\n");

	assert_submodule_exists(g_repo, "mismatch_name");
	assert_submodule_exists(g_repo, "mismatch_path");
	assert_submodule_exists(g_repo, "sm_just_added");
	assert_submodule_exists(g_repo, "sm_just_added_2");
	assert_submodule_exists(g_repo, "sm_just_added_idx");
	assert_submodule_exists(g_repo, "sm_just_added_head");
	baseline_tests();

	cl_git_rewritefile("submod2/.gitmodules", snap2.ptr);
	git_buf_free(&snap2);

	refute_submodule_exists(g_repo, "mismatch_name", GIT_ENOTFOUND);
	refute_submodule_exists(g_repo, "mismatch_path", GIT_ENOTFOUND);
	assert_submodule_exists(g_repo, "sm_just_added");
	assert_submodule_exists(g_repo, "sm_just_added_2");
	assert_submodule_exists(g_repo, "sm_just_added_idx");
	assert_submodule_exists(g_repo, "sm_just_added_head");
	baseline_tests();

	cl_git_rewritefile("submod2/.gitmodules", snap1.ptr);
	git_buf_free(&snap1);

	refute_submodule_exists(g_repo, "mismatch_name", GIT_ENOTFOUND);
	refute_submodule_exists(g_repo, "mismatch_path", GIT_ENOTFOUND);
	/* note error code change, because add_setup made a repo in the workdir */
	refute_submodule_exists(g_repo, "sm_just_added", GIT_EEXISTS);
	refute_submodule_exists(g_repo, "sm_just_added_2", GIT_EEXISTS);
	/* these still exist in index and head respectively */
	assert_submodule_exists(g_repo, "sm_just_added_idx");
	assert_submodule_exists(g_repo, "sm_just_added_head");
	baseline_tests();

	{
		git_index *idx;
		cl_git_pass(git_repository_index(&idx, g_repo));
		cl_git_pass(git_index_remove_bypath(idx, "sm_just_added_idx"));
		cl_git_pass(git_index_remove_bypath(idx, "sm_just_added_head"));
		cl_git_pass(git_index_write(idx));
		git_index_free(idx);
	}

	refute_submodule_exists(g_repo, "sm_just_added_idx", GIT_EEXISTS);
	assert_submodule_exists(g_repo, "sm_just_added_head");

	{
		git_signature *sig;
		cl_git_pass(git_signature_now(&sig, "resetter", "resetter@email.com"));
		cl_git_pass(git_reference_create(NULL, g_repo, "refs/heads/master", git_reference_target(original_head), 1, sig, "move head back"));
		git_signature_free(sig);
		git_reference_free(original_head);
	}

	refute_submodule_exists(g_repo, "sm_just_added_head", GIT_EEXISTS);
}

