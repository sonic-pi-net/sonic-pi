#include "clar_libgit2.h"
#include "futils.h"
#include "stash_helpers.h"

static git_signature *signature;
static git_repository *repo;
static git_index *repo_index;

void test_stash_apply__initialize(void)
{
	git_oid oid;

	repo = cl_git_sandbox_init_new("stash");
	cl_git_pass(git_repository_index(&repo_index, repo));
	cl_git_pass(git_signature_new(&signature, "nulltoken", "emeric.fermas@gmail.com", 1323847743, 60)); /* Wed Dec 14 08:29:03 2011 +0100 */

	cl_git_mkfile("stash/what", "hello\n");
	cl_git_mkfile("stash/how", "small\n");
	cl_git_mkfile("stash/who", "world\n");
	cl_git_mkfile("stash/where", "meh\n");

	cl_git_pass(git_index_add_bypath(repo_index, "what"));
	cl_git_pass(git_index_add_bypath(repo_index, "how"));
	cl_git_pass(git_index_add_bypath(repo_index, "who"));

	cl_repo_commit_from_index(NULL, repo, signature, 0, "Initial commit");

	cl_git_rewritefile("stash/what", "goodbye\n");
	cl_git_rewritefile("stash/who", "funky world\n");
	cl_git_mkfile("stash/when", "tomorrow\n");
	cl_git_mkfile("stash/why", "would anybody use stash?\n");
	cl_git_mkfile("stash/where", "????\n");

	cl_git_pass(git_index_add_bypath(repo_index, "who"));
	cl_git_pass(git_index_add_bypath(repo_index, "why"));
	cl_git_pass(git_index_add_bypath(repo_index, "where"));
	cl_git_pass(git_index_write(repo_index));

	cl_git_rewritefile("stash/where", "....\n");

	/* Pre-stash state */
	assert_status(repo, "what", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_STATUS_INDEX_NEW);
	assert_status(repo, "where", GIT_STATUS_INDEX_NEW|GIT_STATUS_WT_MODIFIED);

	cl_git_pass(git_stash_save(&oid, repo, signature, NULL, GIT_STASH_INCLUDE_UNTRACKED));

	/* Post-stash state */
	assert_status(repo, "what", GIT_STATUS_CURRENT);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_CURRENT);
	assert_status(repo, "when", GIT_ENOTFOUND);
	assert_status(repo, "why", GIT_ENOTFOUND);
	assert_status(repo, "where", GIT_ENOTFOUND);
}

void test_stash_apply__cleanup(void)
{
	git_signature_free(signature);
	signature = NULL;

	git_index_free(repo_index);
	repo_index = NULL;

	cl_git_sandbox_cleanup();
}

void test_stash_apply__with_default(void)
{
	git_buf where = GIT_BUF_INIT;

	cl_git_pass(git_stash_apply(repo, 0, NULL));

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 0);
	assert_status(repo, "what", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_STATUS_INDEX_NEW);
	assert_status(repo, "where", GIT_STATUS_INDEX_NEW);

	cl_git_pass(git_futils_readbuffer(&where, "stash/where"));
	cl_assert_equal_s("....\n", where.ptr);

	git_buf_dispose(&where);
}

void test_stash_apply__with_existing_file(void)
{
	cl_git_mkfile("stash/where", "oops!\n");
	cl_git_fail(git_stash_apply(repo, 0, NULL));
}

void test_stash_apply__merges_new_file(void)
{
	const git_index_entry *ancestor, *our, *their;

	cl_git_mkfile("stash/where", "committed before stash\n");
	cl_git_pass(git_index_add_bypath(repo_index, "where"));
	cl_repo_commit_from_index(NULL, repo, signature, 0, "Other commit");

	cl_git_pass(git_stash_apply(repo, 0, NULL));

	cl_assert_equal_i(1, git_index_has_conflicts(repo_index));
	assert_status(repo, "what", GIT_STATUS_INDEX_MODIFIED);
	cl_git_pass(git_index_conflict_get(&ancestor, &our, &their, repo_index, "where")); /* unmerged */
	assert_status(repo, "who", GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_STATUS_INDEX_NEW);
}

void test_stash_apply__with_reinstate_index(void)
{
	git_buf where = GIT_BUF_INIT;
	git_stash_apply_options opts = GIT_STASH_APPLY_OPTIONS_INIT;

	opts.flags = GIT_STASH_APPLY_REINSTATE_INDEX;

	cl_git_pass(git_stash_apply(repo, 0, &opts));

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 0);
	assert_status(repo, "what", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_STATUS_INDEX_NEW);
	assert_status(repo, "where", GIT_STATUS_INDEX_NEW | GIT_STATUS_WT_MODIFIED);

	cl_git_pass(git_futils_readbuffer(&where, "stash/where"));
	cl_assert_equal_s("....\n", where.ptr);

	git_buf_dispose(&where);
}

void test_stash_apply__conflict_index_with_default(void)
{
	const git_index_entry *ancestor;
	const git_index_entry *our;
	const git_index_entry *their;

	cl_git_rewritefile("stash/who", "nothing\n");
	cl_git_pass(git_index_add_bypath(repo_index, "who"));
	cl_git_pass(git_index_write(repo_index));
	cl_repo_commit_from_index(NULL, repo, signature, 0, "Other commit");

	cl_git_pass(git_stash_apply(repo, 0, NULL));

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 1);
	assert_status(repo, "what", GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	cl_git_pass(git_index_conflict_get(&ancestor, &our, &their, repo_index, "who")); /* unmerged */
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_STATUS_INDEX_NEW);
}

void test_stash_apply__conflict_index_with_reinstate_index(void)
{
	git_stash_apply_options opts = GIT_STASH_APPLY_OPTIONS_INIT;

	opts.flags = GIT_STASH_APPLY_REINSTATE_INDEX;

	cl_git_rewritefile("stash/who", "nothing\n");
	cl_git_pass(git_index_add_bypath(repo_index, "who"));
	cl_git_pass(git_index_write(repo_index));
	cl_repo_commit_from_index(NULL, repo, signature, 0, "Other commit");

	cl_git_fail_with(git_stash_apply(repo, 0, &opts), GIT_ECONFLICT);

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 0);
	assert_status(repo, "what", GIT_STATUS_CURRENT);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_CURRENT);
	assert_status(repo, "when", GIT_ENOTFOUND);
	assert_status(repo, "why", GIT_ENOTFOUND);
}

void test_stash_apply__conflict_untracked_with_default(void)
{
	git_stash_apply_options opts = GIT_STASH_APPLY_OPTIONS_INIT;

	cl_git_mkfile("stash/when", "nothing\n");

	cl_git_fail_with(git_stash_apply(repo, 0, &opts), GIT_ECONFLICT);

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 0);
	assert_status(repo, "what", GIT_STATUS_CURRENT);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_CURRENT);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_ENOTFOUND);
}

void test_stash_apply__conflict_untracked_with_reinstate_index(void)
{
	git_stash_apply_options opts = GIT_STASH_APPLY_OPTIONS_INIT;

	opts.flags = GIT_STASH_APPLY_REINSTATE_INDEX;

	cl_git_mkfile("stash/when", "nothing\n");

	cl_git_fail_with(git_stash_apply(repo, 0, &opts), GIT_ECONFLICT);

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 0);
	assert_status(repo, "what", GIT_STATUS_CURRENT);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_CURRENT);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_ENOTFOUND);
}

void test_stash_apply__conflict_workdir_with_default(void)
{
	cl_git_rewritefile("stash/what", "ciao\n");

	cl_git_fail_with(git_stash_apply(repo, 0, NULL), GIT_ECONFLICT);

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 0);
	assert_status(repo, "what", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_CURRENT);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_ENOTFOUND);
}

void test_stash_apply__conflict_workdir_with_reinstate_index(void)
{
	git_stash_apply_options opts = GIT_STASH_APPLY_OPTIONS_INIT;

	opts.flags = GIT_STASH_APPLY_REINSTATE_INDEX;

	cl_git_rewritefile("stash/what", "ciao\n");

	cl_git_fail_with(git_stash_apply(repo, 0, &opts), GIT_ECONFLICT);

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 0);
	assert_status(repo, "what", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_CURRENT);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_ENOTFOUND);
}

void test_stash_apply__conflict_commit_with_default(void)
{
	const git_index_entry *ancestor;
	const git_index_entry *our;
	const git_index_entry *their;

	cl_git_rewritefile("stash/what", "ciao\n");
	cl_git_pass(git_index_add_bypath(repo_index, "what"));
	cl_repo_commit_from_index(NULL, repo, signature, 0, "Other commit");

	cl_git_pass(git_stash_apply(repo, 0, NULL));

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 1);
	cl_git_pass(git_index_conflict_get(&ancestor, &our, &their, repo_index, "what")); /* unmerged */
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_STATUS_INDEX_NEW);
}

void test_stash_apply__conflict_commit_with_reinstate_index(void)
{
	git_stash_apply_options opts = GIT_STASH_APPLY_OPTIONS_INIT;
	const git_index_entry *ancestor;
	const git_index_entry *our;
	const git_index_entry *their;

	opts.flags = GIT_STASH_APPLY_REINSTATE_INDEX;

	cl_git_rewritefile("stash/what", "ciao\n");
	cl_git_pass(git_index_add_bypath(repo_index, "what"));
	cl_repo_commit_from_index(NULL, repo, signature, 0, "Other commit");

	cl_git_pass(git_stash_apply(repo, 0, &opts));

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 1);
	cl_git_pass(git_index_conflict_get(&ancestor, &our, &their, repo_index, "what")); /* unmerged */
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_STATUS_INDEX_NEW);
}

void test_stash_apply__fails_with_uncommitted_changes_in_index(void)
{
	cl_git_rewritefile("stash/who", "nothing\n");
	cl_git_pass(git_index_add_bypath(repo_index, "who"));
	cl_git_pass(git_index_write(repo_index));

	cl_git_fail_with(git_stash_apply(repo, 0, NULL), GIT_EUNCOMMITTED);

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 0);
	assert_status(repo, "what", GIT_STATUS_CURRENT);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_INDEX_MODIFIED);
	assert_status(repo, "when", GIT_ENOTFOUND);
	assert_status(repo, "why", GIT_ENOTFOUND);
}

void test_stash_apply__pop(void)
{
	cl_git_pass(git_stash_pop(repo, 0, NULL));

	cl_git_fail_with(git_stash_pop(repo, 0, NULL), GIT_ENOTFOUND);
}

struct seen_paths {
	bool what;
	bool how;
	bool who;
	bool when;
};

int checkout_notify(
	git_checkout_notify_t why,
	const char *path,
	const git_diff_file *baseline,
	const git_diff_file *target,
	const git_diff_file *workdir,
	void *payload)
{
	struct seen_paths *seen_paths = (struct seen_paths *)payload;

	GIT_UNUSED(why);
	GIT_UNUSED(baseline);
	GIT_UNUSED(target);
	GIT_UNUSED(workdir);

	if (strcmp(path, "what") == 0)
		seen_paths->what = 1;
	else if (strcmp(path, "how") == 0)
		seen_paths->how = 1;
	else if (strcmp(path, "who") == 0)
		seen_paths->who = 1;
	else if (strcmp(path, "when") == 0)
		seen_paths->when = 1;

	return 0;
}

void test_stash_apply__executes_notify_cb(void)
{
	git_stash_apply_options opts = GIT_STASH_APPLY_OPTIONS_INIT;
	struct seen_paths seen_paths = {0};

	opts.checkout_options.notify_cb = checkout_notify;
	opts.checkout_options.notify_flags = GIT_CHECKOUT_NOTIFY_ALL;
	opts.checkout_options.notify_payload = &seen_paths;

	cl_git_pass(git_stash_apply(repo, 0, &opts));

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 0);
	assert_status(repo, "what", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_STATUS_INDEX_NEW);
	assert_status(repo, "where", GIT_STATUS_INDEX_NEW);

	cl_assert_equal_b(true, seen_paths.what);
	cl_assert_equal_b(false, seen_paths.how);
	cl_assert_equal_b(true, seen_paths.who);
	cl_assert_equal_b(true, seen_paths.when);
}

int progress_cb(
	git_stash_apply_progress_t progress,
	void *payload)
{
	git_stash_apply_progress_t *p = (git_stash_apply_progress_t *)payload;

	cl_assert_equal_i((*p)+1, progress);

	*p = progress;

	return 0;
}

void test_stash_apply__calls_progress_cb(void)
{
	git_stash_apply_options opts = GIT_STASH_APPLY_OPTIONS_INIT;
	git_stash_apply_progress_t progress = GIT_STASH_APPLY_PROGRESS_NONE;

	opts.progress_cb = progress_cb;
	opts.progress_payload = &progress;

	cl_git_pass(git_stash_apply(repo, 0, &opts));
	cl_assert_equal_i(progress, GIT_STASH_APPLY_PROGRESS_DONE);
}

int aborting_progress_cb(
	git_stash_apply_progress_t progress,
	void *payload)
{
	GIT_UNUSED(payload);

	if (progress == GIT_STASH_APPLY_PROGRESS_ANALYZE_MODIFIED)
		return -44;

	return 0;
}

void test_stash_apply__progress_cb_can_abort(void)
{
	git_stash_apply_options opts = GIT_STASH_APPLY_OPTIONS_INIT;

	opts.progress_cb = aborting_progress_cb;

	cl_git_fail_with(-44, git_stash_apply(repo, 0, &opts));
}

void test_stash_apply__uses_reflog_like_indices_1(void)
{
	git_oid oid;

	cl_git_mkfile("stash/untracked", "untracked\n");
	cl_git_pass(git_stash_save(&oid, repo, signature, NULL, GIT_STASH_INCLUDE_UNTRACKED));
	assert_status(repo, "untracked", GIT_ENOTFOUND);

	/* stash@{1} is the oldest (first) stash we made */
	cl_git_pass(git_stash_apply(repo, 1, NULL));
	cl_assert_equal_i(git_index_has_conflicts(repo_index), 0);
	assert_status(repo, "what", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "how", GIT_STATUS_CURRENT);
	assert_status(repo, "who", GIT_STATUS_WT_MODIFIED);
	assert_status(repo, "when", GIT_STATUS_WT_NEW);
	assert_status(repo, "why", GIT_STATUS_INDEX_NEW);
	assert_status(repo, "where", GIT_STATUS_INDEX_NEW);
}

void test_stash_apply__uses_reflog_like_indices_2(void)
{
	git_oid oid;

	cl_git_mkfile("stash/untracked", "untracked\n");
	cl_git_pass(git_stash_save(&oid, repo, signature, NULL, GIT_STASH_INCLUDE_UNTRACKED));
	assert_status(repo, "untracked", GIT_ENOTFOUND);

	/* stash@{0} is the newest stash we made immediately above */
	cl_git_pass(git_stash_apply(repo, 0, NULL));

	cl_assert_equal_i(git_index_has_conflicts(repo_index), 0);
	assert_status(repo, "untracked", GIT_STATUS_WT_NEW);
}
