#include "clar_libgit2.h"
#include "posix.h"
#include "path.h"
#include "submodule_helpers.h"
#include "futils.h"

static git_repository *g_repo = NULL;

void test_submodule_update__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_submodule_update__uninitialized_submodule_no_init(void)
{
	git_submodule *sm;
	git_submodule_update_options update_options = GIT_SUBMODULE_UPDATE_OPTIONS_INIT;

	g_repo = setup_fixture_submodule_simple();

	/* get the submodule */
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	/* updating an uninitialized repository throws */
	cl_git_fail_with(
		GIT_ERROR,
		git_submodule_update(sm, 0, &update_options));

	git_submodule_free(sm);
}

struct update_submodule_cb_payload {
	int update_tips_called;
	int checkout_progress_called;
	int checkout_notify_called;
};

static void checkout_progress_cb(
	const char *path,
	size_t completed_steps,
	size_t total_steps,
	void *payload)
{
	struct update_submodule_cb_payload *update_payload = payload;

	GIT_UNUSED(path);
	GIT_UNUSED(completed_steps);
	GIT_UNUSED(total_steps);

	update_payload->checkout_progress_called = 1;
}

static int checkout_notify_cb(
	git_checkout_notify_t why,
	const char *path,
	const git_diff_file *baseline,
	const git_diff_file *target,
	const git_diff_file *workdir,
	void *payload)
{
	struct update_submodule_cb_payload *update_payload = payload;

	GIT_UNUSED(why);
	GIT_UNUSED(path);
	GIT_UNUSED(baseline);
	GIT_UNUSED(target);
	GIT_UNUSED(workdir);

	update_payload->checkout_notify_called = 1;

	return 0;
}

static int update_tips(const char *refname, const git_oid *a, const git_oid *b, void *data)
{
	struct update_submodule_cb_payload *update_payload = data;

	GIT_UNUSED(refname);
	GIT_UNUSED(a);
	GIT_UNUSED(b);

	update_payload->update_tips_called = 1;

	return 1;
}

void test_submodule_update__update_submodule(void)
{
	git_submodule *sm;
	git_submodule_update_options update_options = GIT_SUBMODULE_UPDATE_OPTIONS_INIT;
	unsigned int submodule_status = 0;
	struct update_submodule_cb_payload update_payload = { 0 };

	g_repo = setup_fixture_submodule_simple();

	update_options.checkout_opts.progress_cb = checkout_progress_cb;
	update_options.checkout_opts.progress_payload = &update_payload;

	update_options.fetch_opts.callbacks.update_tips = update_tips;
	update_options.fetch_opts.callbacks.payload = &update_payload;

	/* get the submodule */
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	/* verify the initial state of the submodule */
	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "testrepo", GIT_SUBMODULE_IGNORE_UNSPECIFIED));
	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_WD_UNINITIALIZED);

	/* initialize and update the submodule */
	cl_git_pass(git_submodule_init(sm, 0));
	cl_git_pass(git_submodule_update(sm, 0, &update_options));

	/* verify state */
	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "testrepo", GIT_SUBMODULE_IGNORE_UNSPECIFIED));
	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_IN_WD);

	cl_assert(git_oid_streq(git_submodule_head_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);

	/* verify that the expected callbacks have been called. */
	cl_assert_equal_i(1, update_payload.checkout_progress_called);
	cl_assert_equal_i(1, update_payload.update_tips_called);

	git_submodule_free(sm);
}

void test_submodule_update__update_submodule_with_path(void)
{
	git_submodule *sm;
	git_submodule_update_options update_options = GIT_SUBMODULE_UPDATE_OPTIONS_INIT;
	unsigned int submodule_status = 0;
	struct update_submodule_cb_payload update_payload = { 0 };

	g_repo = setup_fixture_submodule_with_path();

	update_options.checkout_opts.progress_cb = checkout_progress_cb;
	update_options.checkout_opts.progress_payload = &update_payload;

	update_options.fetch_opts.callbacks.update_tips = update_tips;
	update_options.fetch_opts.callbacks.payload = &update_payload;

	/* get the submodule */
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	/* verify the initial state of the submodule */
	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "testrepo", GIT_SUBMODULE_IGNORE_UNSPECIFIED));
	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_WD_UNINITIALIZED);

	/* initialize and update the submodule */
	cl_git_pass(git_submodule_init(sm, 0));
	cl_git_pass(git_submodule_update(sm, 0, &update_options));

	/* verify state */
	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "testrepo", GIT_SUBMODULE_IGNORE_UNSPECIFIED));
	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_IN_WD);

	cl_assert(git_oid_streq(git_submodule_head_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);

	/* verify that the expected callbacks have been called. */
	cl_assert_equal_i(1, update_payload.checkout_progress_called);
	cl_assert_equal_i(1, update_payload.update_tips_called);

	git_submodule_free(sm);
}

void test_submodule_update__update_and_init_submodule(void)
{
	git_submodule *sm;
	git_submodule_update_options update_options = GIT_SUBMODULE_UPDATE_OPTIONS_INIT;
	unsigned int submodule_status = 0;

	g_repo = setup_fixture_submodule_simple();

	/* get the submodule */
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "testrepo", GIT_SUBMODULE_IGNORE_UNSPECIFIED));
	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_WD_UNINITIALIZED);

	/* update (with option to initialize sub repo) */
	cl_git_pass(git_submodule_update(sm, 1, &update_options));

	/* verify expected state */
	cl_assert(git_oid_streq(git_submodule_head_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);

	git_submodule_free(sm);
}

void test_submodule_update__update_skip_configured_missing_submodule(void)
{
	git_submodule *sm;
	git_submodule_update_options update_options = GIT_SUBMODULE_UPDATE_OPTIONS_INIT;
	unsigned int submodule_status = 0;

	g_repo = setup_fixture_submod2();

	/* get the submodule */
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "sm_gitmodules_only"));

	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "sm_gitmodules_only", GIT_SUBMODULE_IGNORE_UNSPECIFIED));
	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_CONFIG);

	/* update (with option to initialize sub repo) */
	cl_git_pass(git_submodule_update(sm, 1, &update_options));

	git_submodule_free(sm);
}

void test_submodule_update__update_already_checked_out_submodule(void)
{
	git_submodule *sm = NULL;
	git_checkout_options checkout_options = GIT_CHECKOUT_OPTIONS_INIT;
	git_submodule_update_options update_options = GIT_SUBMODULE_UPDATE_OPTIONS_INIT;
	unsigned int submodule_status = 0;
	git_reference *branch_reference = NULL;
	git_object *branch_commit = NULL;
	struct update_submodule_cb_payload update_payload = { 0 };

	g_repo = setup_fixture_submodule_simple();

	update_options.checkout_opts.progress_cb = checkout_progress_cb;
	update_options.checkout_opts.progress_payload = &update_payload;

	/* Initialize and update the sub repository */
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "testrepo", GIT_SUBMODULE_IGNORE_UNSPECIFIED));
	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_WD_UNINITIALIZED);

	cl_git_pass(git_submodule_update(sm, 1, &update_options));

	/* verify expected state */
	cl_assert(git_oid_streq(git_submodule_head_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);

	/* checkout the alternate_1 branch */
	checkout_options.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup(&branch_reference, g_repo, "refs/heads/alternate_1"));
	cl_git_pass(git_reference_peel(&branch_commit, branch_reference, GIT_OBJECT_COMMIT));
	cl_git_pass(git_checkout_tree(g_repo, branch_commit, &checkout_options));
	cl_git_pass(git_repository_set_head(g_repo, git_reference_name(branch_reference)));

	/*
	 * Verify state after checkout of parent repository. The submodule ID in the
	 * HEAD commit and index should be updated, but not the workdir.
	 */

	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "testrepo", GIT_SUBMODULE_IGNORE_UNSPECIFIED));

	git_submodule_free(sm);
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_IN_WD |
		GIT_SUBMODULE_STATUS_WD_MODIFIED);

	cl_assert(git_oid_streq(git_submodule_head_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);

	/*
	 * Update the submodule and verify the state.
	 * Now, the HEAD, index, and Workdir commits should all be updated to
	 * the new commit.
	 */
	cl_git_pass(git_submodule_update(sm, 0, &update_options));
	cl_assert(git_oid_streq(git_submodule_head_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);

	/* verify that the expected callbacks have been called. */
	cl_assert_equal_i(1, update_payload.checkout_progress_called);

	git_submodule_free(sm);
	git_object_free(branch_commit);
	git_reference_free(branch_reference);
}

void test_submodule_update__update_blocks_on_dirty_wd(void)
{
	git_submodule *sm = NULL;
	git_checkout_options checkout_options = GIT_CHECKOUT_OPTIONS_INIT;
	git_submodule_update_options update_options = GIT_SUBMODULE_UPDATE_OPTIONS_INIT;
	unsigned int submodule_status = 0;
	git_reference *branch_reference = NULL;
	git_object *branch_commit = NULL;
	struct update_submodule_cb_payload update_payload = { 0 };

	g_repo = setup_fixture_submodule_simple();

	update_options.checkout_opts.notify_flags = GIT_CHECKOUT_NOTIFY_CONFLICT;
	update_options.checkout_opts.notify_cb = checkout_notify_cb;
	update_options.checkout_opts.notify_payload = &update_payload;

	/* Initialize and update the sub repository */
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "testrepo", GIT_SUBMODULE_IGNORE_UNSPECIFIED));
	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_WD_UNINITIALIZED);

	cl_git_pass(git_submodule_update(sm, 1, &update_options));

	/* verify expected state */
	cl_assert(git_oid_streq(git_submodule_head_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);

	/* checkout the alternate_1 branch */
	checkout_options.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup(&branch_reference, g_repo, "refs/heads/alternate_1"));
	cl_git_pass(git_reference_peel(&branch_commit, branch_reference, GIT_OBJECT_COMMIT));
	cl_git_pass(git_checkout_tree(g_repo, branch_commit, &checkout_options));
	cl_git_pass(git_repository_set_head(g_repo, git_reference_name(branch_reference)));

	/*
	 * Verify state after checkout of parent repository. The submodule ID in the
	 * HEAD commit and index should be updated, but not the workdir.
	 */

	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "testrepo", GIT_SUBMODULE_IGNORE_UNSPECIFIED));

	git_submodule_free(sm);
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_IN_WD |
		GIT_SUBMODULE_STATUS_WD_MODIFIED);

	cl_assert(git_oid_streq(git_submodule_head_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);

	/*
	 * Create a conflicting edit in the subrepository to verify that
	 * the submodule update action is blocked.
	 */
	cl_git_write2file("submodule_simple/testrepo/branch_file.txt", "a conflicting edit", 0,
		O_WRONLY | O_CREAT | O_TRUNC, 0755);

	cl_git_fail(git_submodule_update(sm, 0, &update_options));

	/* verify that the expected callbacks have been called. */
	cl_assert_equal_i(1, update_payload.checkout_notify_called);

	/* verify that the submodule state has not changed. */
	cl_assert(git_oid_streq(git_submodule_head_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);

	git_submodule_free(sm);
	git_object_free(branch_commit);
	git_reference_free(branch_reference);
}

void test_submodule_update__can_force_update(void)
{
	git_submodule *sm = NULL;
	git_checkout_options checkout_options = GIT_CHECKOUT_OPTIONS_INIT;
	git_submodule_update_options update_options = GIT_SUBMODULE_UPDATE_OPTIONS_INIT;
	unsigned int submodule_status = 0;
	git_reference *branch_reference = NULL;
	git_object *branch_commit = NULL;

	g_repo = setup_fixture_submodule_simple();

	/* Initialize and update the sub repository */
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "testrepo", GIT_SUBMODULE_IGNORE_UNSPECIFIED));
	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_WD_UNINITIALIZED);

	cl_git_pass(git_submodule_update(sm, 1, &update_options));

	/* verify expected state */
	cl_assert(git_oid_streq(git_submodule_head_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);

	/* checkout the alternate_1 branch */
	checkout_options.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup(&branch_reference, g_repo, "refs/heads/alternate_1"));
	cl_git_pass(git_reference_peel(&branch_commit, branch_reference, GIT_OBJECT_COMMIT));
	cl_git_pass(git_checkout_tree(g_repo, branch_commit, &checkout_options));
	cl_git_pass(git_repository_set_head(g_repo, git_reference_name(branch_reference)));

	/*
	 * Verify state after checkout of parent repository. The submodule ID in the
	 * HEAD commit and index should be updated, but not the workdir.
	 */
	cl_git_pass(git_submodule_status(&submodule_status, g_repo, "testrepo", GIT_SUBMODULE_IGNORE_UNSPECIFIED));

	git_submodule_free(sm);
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	cl_assert_equal_i(submodule_status, GIT_SUBMODULE_STATUS_IN_HEAD |
		GIT_SUBMODULE_STATUS_IN_INDEX |
		GIT_SUBMODULE_STATUS_IN_CONFIG |
		GIT_SUBMODULE_STATUS_IN_WD |
		GIT_SUBMODULE_STATUS_WD_MODIFIED);

	cl_assert(git_oid_streq(git_submodule_head_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);

	/*
	 * Create a conflicting edit in the subrepository to verify that
	 * the submodule update action is blocked.
	 */
	cl_git_write2file("submodule_simple/testrepo/branch_file.txt", "a conflicting edit", 0,
		O_WRONLY | O_CREAT | O_TRUNC, 0777);

	/* forcefully checkout and verify the submodule state was updated. */
	update_options.checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	cl_git_pass(git_submodule_update(sm, 0, &update_options));
	cl_assert(git_oid_streq(git_submodule_head_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);
	cl_assert(git_oid_streq(git_submodule_wd_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);
	cl_assert(git_oid_streq(git_submodule_index_id(sm), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750") == 0);

	git_submodule_free(sm);
	git_object_free(branch_commit);
	git_reference_free(branch_reference);
}

