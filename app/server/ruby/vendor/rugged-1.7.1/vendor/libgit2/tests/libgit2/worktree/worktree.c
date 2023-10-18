#include "clar_libgit2.h"
#include "worktree_helpers.h"
#include "submodule/submodule_helpers.h"

#include "checkout.h"
#include "repository.h"
#include "worktree.h"

#define COMMON_REPO "testrepo"
#define WORKTREE_REPO "testrepo-worktree"

static worktree_fixture fixture =
	WORKTREE_FIXTURE_INIT(COMMON_REPO, WORKTREE_REPO);

void test_worktree_worktree__initialize(void)
{
	setup_fixture_worktree(&fixture);
}

void test_worktree_worktree__cleanup(void)
{
	cleanup_fixture_worktree(&fixture);
}

void test_worktree_worktree__list(void)
{
	git_strarray wts;

	cl_git_pass(git_worktree_list(&wts, fixture.repo));
	cl_assert_equal_i(wts.count, 1);
	cl_assert_equal_s(wts.strings[0], "testrepo-worktree");

	git_strarray_dispose(&wts);
}

void test_worktree_worktree__list_with_invalid_worktree_dirs(void)
{
	const char *filesets[3][2] = {
		{ "gitdir", "commondir" },
		{ "gitdir", "HEAD" },
		{ "HEAD", "commondir" },
	};
	git_str path = GIT_STR_INIT;
	git_strarray wts;
	size_t i, j, len;

	cl_git_pass(git_str_joinpath(&path,
	            fixture.repo->commondir,
	            "worktrees/invalid"));
	cl_git_pass(p_mkdir(path.ptr, 0755));

	len = path.size;

	for (i = 0; i < ARRAY_SIZE(filesets); i++) {

		for (j = 0; j < ARRAY_SIZE(filesets[i]); j++) {
			git_str_truncate(&path, len);
			cl_git_pass(git_str_joinpath(&path, path.ptr, filesets[i][j]));
			cl_git_pass(p_close(p_creat(path.ptr, 0644)));
		}

		cl_git_pass(git_worktree_list(&wts, fixture.worktree));
		cl_assert_equal_i(wts.count, 1);
		cl_assert_equal_s(wts.strings[0], "testrepo-worktree");
		git_strarray_dispose(&wts);

		for (j = 0; j < ARRAY_SIZE(filesets[i]); j++) {
			git_str_truncate(&path, len);
			cl_git_pass(git_str_joinpath(&path, path.ptr, filesets[i][j]));
			p_unlink(path.ptr);
		}
	}

	git_str_dispose(&path);
}

void test_worktree_worktree__list_in_worktree_repo(void)
{
	git_strarray wts;

	cl_git_pass(git_worktree_list(&wts, fixture.worktree));
	cl_assert_equal_i(wts.count, 1);
	cl_assert_equal_s(wts.strings[0], "testrepo-worktree");

	git_strarray_dispose(&wts);
}

void test_worktree_worktree__list_without_worktrees(void)
{
	git_repository *repo;
	git_strarray wts;

	repo = cl_git_sandbox_init("testrepo2");
	cl_git_pass(git_worktree_list(&wts, repo));
	cl_assert_equal_i(wts.count, 0);

	git_repository_free(repo);
}

void test_worktree_worktree__lookup(void)
{
	git_worktree *wt;
	git_str gitdir_path = GIT_STR_INIT;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));

	cl_git_pass(git_str_joinpath(&gitdir_path, fixture.repo->commondir, "worktrees/testrepo-worktree/"));

	cl_assert_equal_s(wt->gitdir_path, gitdir_path.ptr);
	cl_assert_equal_s(wt->parent_path, fixture.repo->workdir);
	cl_assert_equal_s(wt->gitlink_path, fixture.worktree->gitlink);
	cl_assert_equal_s(wt->commondir_path, fixture.repo->gitdir);
	cl_assert_equal_s(wt->commondir_path, fixture.repo->commondir);

	git_str_dispose(&gitdir_path);
	git_worktree_free(wt);
}

void test_worktree_worktree__lookup_nonexistent_worktree(void)
{
	git_worktree *wt;

	cl_git_fail_with(GIT_ENOTFOUND, git_worktree_lookup(&wt, fixture.repo, "nonexistent"));
	cl_assert_equal_p(wt, NULL);
}

void test_worktree_worktree__open(void)
{
	git_worktree *wt;
	git_repository *repo;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));

	cl_git_pass(git_repository_open_from_worktree(&repo, wt));
	cl_assert_equal_s(git_repository_workdir(repo),
		git_repository_workdir(fixture.worktree));

	git_repository_free(repo);
	git_worktree_free(wt);
}

void test_worktree_worktree__open_invalid_commondir(void)
{
	git_worktree *wt;
	git_repository *repo;
	git_str buf = GIT_STR_INIT, path = GIT_STR_INIT;

	cl_git_pass(git_str_sets(&buf, "/path/to/nonexistent/commondir"));
	cl_git_pass(git_str_joinpath(&path,
	            fixture.repo->commondir,
	            "worktrees/testrepo-worktree/commondir"));
	cl_git_pass(git_futils_writebuffer(&buf, path.ptr, O_RDWR, 0644));

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_fail(git_repository_open_from_worktree(&repo, wt));

	git_str_dispose(&buf);
	git_str_dispose(&path);
	git_worktree_free(wt);
}

void test_worktree_worktree__open_invalid_gitdir(void)
{
	git_worktree *wt;
	git_repository *repo;
	git_str buf = GIT_STR_INIT, path = GIT_STR_INIT;

	cl_git_pass(git_str_sets(&buf, "/path/to/nonexistent/gitdir"));
	cl_git_pass(git_str_joinpath(&path,
	            fixture.repo->commondir,
	            "worktrees/testrepo-worktree/gitdir"));
	cl_git_pass(git_futils_writebuffer(&buf, path.ptr, O_RDWR, 0644));

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_fail(git_repository_open_from_worktree(&repo, wt));

	git_str_dispose(&buf);
	git_str_dispose(&path);
	git_worktree_free(wt);
}

void test_worktree_worktree__open_invalid_parent(void)
{
	git_worktree *wt;
	git_repository *repo;
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_str_sets(&buf, "/path/to/nonexistent/gitdir"));
	cl_git_pass(git_futils_writebuffer(&buf,
		    fixture.worktree->gitlink, O_RDWR, 0644));

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_fail(git_repository_open_from_worktree(&repo, wt));

	git_str_dispose(&buf);
	git_worktree_free(wt);
}

void test_worktree_worktree__init(void)
{
	git_worktree *wt;
	git_repository *repo;
	git_reference *branch;
	git_str path = GIT_STR_INIT;

	cl_git_pass(git_str_joinpath(&path, fixture.repo->workdir, "../worktree-new"));
	cl_git_pass(git_worktree_add(&wt, fixture.repo, "worktree-new", path.ptr, NULL));

	/* Open and verify created repo */
	cl_git_pass(git_repository_open(&repo, path.ptr));
	cl_assert(git__suffixcmp(git_repository_workdir(repo), "worktree-new/") == 0);
	cl_git_pass(git_branch_lookup(&branch, repo, "worktree-new", GIT_BRANCH_LOCAL));

	git_str_dispose(&path);
	git_worktree_free(wt);
	git_reference_free(branch);
	git_repository_free(repo);
}

void test_worktree_worktree__add_locked(void)
{
	git_worktree *wt;
	git_repository *repo;
	git_reference *branch;
	git_str path = GIT_STR_INIT;
	git_worktree_add_options opts = GIT_WORKTREE_ADD_OPTIONS_INIT;

	opts.lock = 1;

	cl_git_pass(git_str_joinpath(&path, fixture.repo->workdir, "../worktree-locked"));
	cl_git_pass(git_worktree_add(&wt, fixture.repo, "worktree-locked", path.ptr, &opts));

	/* Open and verify created repo */
	cl_assert(git_worktree_is_locked(NULL, wt));
	cl_git_pass(git_repository_open(&repo, path.ptr));
	cl_assert(git__suffixcmp(git_repository_workdir(repo), "worktree-locked/") == 0);
	cl_git_pass(git_branch_lookup(&branch, repo, "worktree-locked", GIT_BRANCH_LOCAL));

	git_str_dispose(&path);
	git_worktree_free(wt);
	git_reference_free(branch);
	git_repository_free(repo);
}

void test_worktree_worktree__init_existing_branch(void)
{
	git_reference *head, *branch;
	git_commit *commit;
	git_worktree *wt;
	git_str path = GIT_STR_INIT;

	cl_git_pass(git_repository_head(&head, fixture.repo));
	cl_git_pass(git_commit_lookup(&commit, fixture.repo, &head->target.oid));
	cl_git_pass(git_branch_create(&branch, fixture.repo, "worktree-new", commit, false));

	cl_git_pass(git_str_joinpath(&path, fixture.repo->workdir, "../worktree-new"));
	cl_git_fail(git_worktree_add(&wt, fixture.repo, "worktree-new", path.ptr, NULL));

	git_str_dispose(&path);
	git_commit_free(commit);
	git_reference_free(head);
	git_reference_free(branch);
}

void test_worktree_worktree__add_with_explicit_branch(void)
{
	git_reference *head, *branch, *wthead;
	git_commit *commit;
	git_worktree *wt;
	git_repository *wtrepo;
	git_str path = GIT_STR_INIT;
	git_worktree_add_options opts = GIT_WORKTREE_ADD_OPTIONS_INIT;

	cl_git_pass(git_repository_head(&head, fixture.repo));
	cl_git_pass(git_commit_lookup(&commit, fixture.repo, &head->target.oid));
	cl_git_pass(git_branch_create(&branch, fixture.repo, "worktree-with-ref", commit, false));

	opts.ref = branch;

	cl_git_pass(git_str_joinpath(&path, fixture.repo->workdir, "../worktree-with-different-name"));
	cl_git_pass(git_worktree_add(&wt, fixture.repo, "worktree-with-different-name", path.ptr, &opts));
	cl_git_pass(git_repository_open_from_worktree(&wtrepo, wt));
	cl_git_pass(git_repository_head(&wthead, wtrepo));
	cl_assert_equal_s(git_reference_name(wthead), "refs/heads/worktree-with-ref");

	git_str_dispose(&path);
	git_commit_free(commit);
	git_reference_free(head);
	git_reference_free(branch);
	git_reference_free(wthead);
	git_repository_free(wtrepo);
	git_worktree_free(wt);
}

void test_worktree_worktree__add_no_checkout(void)
{
	git_worktree *wt;
	git_repository *wtrepo;
	git_index *index;
	git_str path = GIT_STR_INIT;
	git_worktree_add_options opts = GIT_WORKTREE_ADD_OPTIONS_INIT;

	opts.checkout_options.checkout_strategy = GIT_CHECKOUT_NONE;

	cl_git_pass(git_str_joinpath(&path, fixture.repo->workdir, "../worktree-no-checkout"));
	cl_git_pass(git_worktree_add(&wt, fixture.repo, "worktree-no-checkout", path.ptr, &opts));

	cl_git_pass(git_repository_open(&wtrepo, path.ptr));
	cl_git_pass(git_repository_index(&index, wtrepo));
	cl_assert_equal_i(git_index_entrycount(index), 0);

	git_str_dispose(&path);
	git_worktree_free(wt);
	git_index_free(index);
	git_repository_free(wtrepo);
}

void test_worktree_worktree__init_existing_worktree(void)
{
	git_worktree *wt;
	git_str path = GIT_STR_INIT;

	cl_git_pass(git_str_joinpath(&path, fixture.repo->workdir, "../worktree-new"));
	cl_git_fail(git_worktree_add(&wt, fixture.repo, "testrepo-worktree", path.ptr, NULL));

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_assert_equal_s(wt->gitlink_path, fixture.worktree->gitlink);

	git_str_dispose(&path);
	git_worktree_free(wt);
}

void test_worktree_worktree__init_existing_path(void)
{
	const char *wtfiles[] = { "HEAD", "commondir", "gitdir", "index" };
	git_worktree *wt;
	git_str path = GIT_STR_INIT;
	unsigned i;

	/* Delete files to verify they have not been created by
	 * the init call */
	for (i = 0; i < ARRAY_SIZE(wtfiles); i++) {
		cl_git_pass(git_str_joinpath(&path,
			    fixture.worktree->gitdir, wtfiles[i]));
		cl_git_pass(p_unlink(path.ptr));
	}

	cl_git_pass(git_str_joinpath(&path, fixture.repo->workdir, "../testrepo-worktree"));
	cl_git_fail(git_worktree_add(&wt, fixture.repo, "worktree-new", path.ptr, NULL));

	/* Verify files have not been re-created */
	for (i = 0; i < ARRAY_SIZE(wtfiles); i++) {
		cl_git_pass(git_str_joinpath(&path,
			    fixture.worktree->gitdir, wtfiles[i]));
		cl_assert(!git_fs_path_exists(path.ptr));
	}

	git_str_dispose(&path);
}

void test_worktree_worktree__init_submodule(void)
{
	git_repository *repo, *sm, *wt;
	git_worktree *worktree;
	git_str path = GIT_STR_INIT;

	cleanup_fixture_worktree(&fixture);
	repo = setup_fixture_submod2();

	cl_git_pass(git_str_joinpath(&path, repo->workdir, "sm_unchanged"));
	cl_git_pass(git_repository_open(&sm, path.ptr));
	cl_git_pass(git_str_joinpath(&path, repo->workdir, "../worktree/"));
	cl_git_pass(git_worktree_add(&worktree, sm, "repo-worktree", path.ptr, NULL));
	cl_git_pass(git_repository_open_from_worktree(&wt, worktree));

	cl_git_pass(git_fs_path_prettify_dir(&path, path.ptr, NULL));
	cl_assert_equal_s(path.ptr, wt->workdir);
	cl_git_pass(git_fs_path_prettify_dir(&path, sm->commondir, NULL));
	cl_assert_equal_s(sm->commondir, wt->commondir);

	cl_git_pass(git_str_joinpath(&path, sm->gitdir, "worktrees/repo-worktree/"));
	cl_assert_equal_s(path.ptr, wt->gitdir);

	git_str_dispose(&path);
	git_worktree_free(worktree);
	git_repository_free(sm);
	git_repository_free(wt);
}

void test_worktree_worktree__validate(void)
{
	git_worktree *wt;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_pass(git_worktree_validate(wt));

	git_worktree_free(wt);
}

void test_worktree_worktree__name(void)
{
	git_worktree *wt;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_assert_equal_s(git_worktree_name(wt), "testrepo-worktree");

	git_worktree_free(wt);
}

void test_worktree_worktree__path(void)
{
	git_worktree *wt;
	git_str expected_path = GIT_STR_INIT;

	cl_git_pass(git_str_joinpath(&expected_path, clar_sandbox_path(), "testrepo-worktree"));
	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_assert_equal_s(git_worktree_path(wt), expected_path.ptr);

	git_str_dispose(&expected_path);
	git_worktree_free(wt);
}

void test_worktree_worktree__validate_invalid_commondir(void)
{
	git_worktree *wt;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	git__free(wt->commondir_path);
	wt->commondir_path = "/path/to/invalid/commondir";

	cl_git_fail(git_worktree_validate(wt));

	wt->commondir_path = NULL;
	git_worktree_free(wt);
}

void test_worktree_worktree__validate_invalid_gitdir(void)
{
	git_worktree *wt;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	git__free(wt->gitdir_path);
	wt->gitdir_path = "/path/to/invalid/gitdir";
	cl_git_fail(git_worktree_validate(wt));

	wt->gitdir_path = NULL;
	git_worktree_free(wt);
}

void test_worktree_worktree__validate_invalid_parent(void)
{
	git_worktree *wt;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	git__free(wt->parent_path);
	wt->parent_path = "/path/to/invalid/parent";
	cl_git_fail(git_worktree_validate(wt));

	wt->parent_path = NULL;
	git_worktree_free(wt);
}

void test_worktree_worktree__lock_with_reason(void)
{
	git_worktree *wt;
	git_buf reason = GIT_BUF_INIT;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));

	cl_assert(!git_worktree_is_locked(NULL, wt));
	cl_git_pass(git_worktree_lock(wt, "because"));
	cl_assert(git_worktree_is_locked(&reason, wt) > 0);
	cl_assert_equal_s(reason.ptr, "because");
	cl_assert(wt->locked);

	git_buf_dispose(&reason);
	git_worktree_free(wt);
}

void test_worktree_worktree__lock_without_reason(void)
{
	git_worktree *wt;
	git_buf reason = GIT_BUF_INIT;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));

	cl_assert(!git_worktree_is_locked(NULL, wt));
	cl_git_pass(git_worktree_lock(wt, NULL));
	cl_assert(git_worktree_is_locked(&reason, wt) > 0);
	cl_assert_equal_i(reason.size, 0);
	cl_assert(wt->locked);

	git_buf_dispose(&reason);
	git_worktree_free(wt);
}

void test_worktree_worktree__unlock_unlocked_worktree(void)
{
	git_worktree *wt;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_assert(!git_worktree_is_locked(NULL, wt));
	cl_assert_equal_i(1, git_worktree_unlock(wt));
	cl_assert(!wt->locked);

	git_worktree_free(wt);
}

void test_worktree_worktree__unlock_locked_worktree(void)
{
	git_worktree *wt;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_pass(git_worktree_lock(wt, NULL));
	cl_assert(git_worktree_is_locked(NULL, wt));
	cl_assert_equal_i(0, git_worktree_unlock(wt));
	cl_assert(!wt->locked);

	git_worktree_free(wt);
}

void test_worktree_worktree__prune_without_opts_fails(void)
{
	git_worktree *wt;
	git_repository *repo;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_fail(git_worktree_prune(wt, NULL));

	/* Assert the repository is still valid */
	cl_git_pass(git_repository_open_from_worktree(&repo, wt));

	git_worktree_free(wt);
	git_repository_free(repo);
}

void test_worktree_worktree__prune_valid(void)
{
	git_worktree_prune_options opts = GIT_WORKTREE_PRUNE_OPTIONS_INIT;
	git_worktree *wt;
	git_repository *repo;

	opts.flags = GIT_WORKTREE_PRUNE_VALID;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_pass(git_worktree_prune(wt, &opts));

	/* Assert the repository is not valid anymore */
	cl_git_fail(git_repository_open_from_worktree(&repo, wt));

	git_worktree_free(wt);
	git_repository_free(repo);
}

void test_worktree_worktree__prune_locked(void)
{
	git_worktree_prune_options opts = GIT_WORKTREE_PRUNE_OPTIONS_INIT;
	git_worktree *wt;
	git_repository *repo;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_pass(git_worktree_lock(wt, NULL));

	opts.flags = GIT_WORKTREE_PRUNE_VALID;
	cl_git_fail(git_worktree_prune(wt, &opts));
	/* Assert the repository is still valid */
	cl_git_pass(git_repository_open_from_worktree(&repo, wt));

	opts.flags = GIT_WORKTREE_PRUNE_VALID|GIT_WORKTREE_PRUNE_LOCKED;
	cl_git_pass(git_worktree_prune(wt, &opts));

	git_worktree_free(wt);
	git_repository_free(repo);
}

void test_worktree_worktree__prune_gitdir_only(void)
{
	git_worktree_prune_options opts = GIT_WORKTREE_PRUNE_OPTIONS_INIT;
	git_worktree *wt;

	opts.flags = GIT_WORKTREE_PRUNE_VALID;
	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_pass(git_worktree_prune(wt, &opts));

	cl_assert(!git_fs_path_exists(wt->gitdir_path));
	cl_assert(git_fs_path_exists(wt->gitlink_path));

	git_worktree_free(wt);
}

void test_worktree_worktree__prune_worktree(void)
{
	git_worktree_prune_options opts = GIT_WORKTREE_PRUNE_OPTIONS_INIT;
	git_worktree *wt;

	opts.flags = GIT_WORKTREE_PRUNE_VALID|GIT_WORKTREE_PRUNE_WORKING_TREE;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_pass(git_worktree_prune(wt, &opts));

	cl_assert(!git_fs_path_exists(wt->gitdir_path));
	cl_assert(!git_fs_path_exists(wt->gitlink_path));

	git_worktree_free(wt);
}

static int foreach_worktree_cb(git_repository *worktree, void *payload)
{
	int *counter = (int *)payload;

	switch (*counter) {
	case 0:
		cl_assert_equal_s(git_repository_path(fixture.repo),
				  git_repository_path(worktree));
		cl_assert(!git_repository_is_worktree(worktree));
		break;
	case 1:
		cl_assert_equal_s(git_repository_path(fixture.worktree),
				  git_repository_path(worktree));
		cl_assert(git_repository_is_worktree(worktree));
		break;
	default:
		cl_fail("more worktrees found than expected");
	}

	(*counter)++;

	return 0;
}

void test_worktree_worktree__foreach_worktree_lists_all_worktrees(void)
{
	int counter = 0;
	cl_git_pass(git_repository_foreach_worktree(fixture.repo, foreach_worktree_cb, &counter));
}

void test_worktree_worktree__validate_invalid_worktreedir(void)
{
	git_worktree *wt;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	p_rename("testrepo-worktree", "testrepo-worktree-tmp");
	cl_git_fail(git_worktree_validate(wt));
	p_rename("testrepo-worktree-tmp", "testrepo-worktree");

	git_worktree_free(wt);
}

void test_worktree_worktree__is_prunable_missing_repo(void)
{
	git_worktree *wt;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	p_rename("testrepo", "testrepo-tmp");
	/* Should not be prunable since the repository moved */
	cl_assert(!git_worktree_is_prunable(wt, NULL));
	p_rename("testrepo-tmp", "testrepo");

	git_worktree_free(wt);
}
