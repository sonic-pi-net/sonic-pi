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

	git_strarray_free(&wts);
}

void test_worktree_worktree__list_with_invalid_worktree_dirs(void)
{
	const char *filesets[3][2] = {
		{ "gitdir", "commondir" },
		{ "gitdir", "HEAD" },
		{ "HEAD", "commondir" },
	};
	git_buf path = GIT_BUF_INIT;
	git_strarray wts;
	unsigned i, j, len;

	cl_git_pass(git_buf_printf(&path, "%s/worktrees/invalid",
		    fixture.repo->commondir));
	cl_git_pass(p_mkdir(path.ptr, 0755));

	len = path.size;

	for (i = 0; i < ARRAY_SIZE(filesets); i++) {

		for (j = 0; j < ARRAY_SIZE(filesets[i]); j++) {
			git_buf_truncate(&path, len);
			cl_git_pass(git_buf_joinpath(&path, path.ptr, filesets[i][j]));
			cl_git_pass(p_close(p_creat(path.ptr, 0644)));
		}

		cl_git_pass(git_worktree_list(&wts, fixture.worktree));
		cl_assert_equal_i(wts.count, 1);
		cl_assert_equal_s(wts.strings[0], "testrepo-worktree");
		git_strarray_free(&wts);

		for (j = 0; j < ARRAY_SIZE(filesets[i]); j++) {
			git_buf_truncate(&path, len);
			cl_git_pass(git_buf_joinpath(&path, path.ptr, filesets[i][j]));
			p_unlink(path.ptr);
		}
	}

	git_buf_free(&path);
}

void test_worktree_worktree__list_in_worktree_repo(void)
{
	git_strarray wts;

	cl_git_pass(git_worktree_list(&wts, fixture.worktree));
	cl_assert_equal_i(wts.count, 1);
	cl_assert_equal_s(wts.strings[0], "testrepo-worktree");

	git_strarray_free(&wts);
}

void test_worktree_worktree__list_bare(void)
{
	git_repository *repo;
	git_strarray wts;

	repo = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_worktree_list(&wts, repo));
	cl_assert_equal_i(wts.count, 0);

	git_repository_free(repo);
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
	git_buf gitdir_path = GIT_BUF_INIT;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));

	cl_git_pass(git_buf_joinpath(&gitdir_path, fixture.repo->commondir, "worktrees/testrepo-worktree/"));

	cl_assert_equal_s(wt->gitdir_path, gitdir_path.ptr);
	cl_assert_equal_s(wt->parent_path, fixture.repo->workdir);
	cl_assert_equal_s(wt->gitlink_path, fixture.worktree->gitlink);
	cl_assert_equal_s(wt->commondir_path, fixture.repo->gitdir);
	cl_assert_equal_s(wt->commondir_path, fixture.repo->commondir);

	git_buf_free(&gitdir_path);
	git_worktree_free(wt);
}

void test_worktree_worktree__lookup_nonexistent_worktree(void)
{
	git_worktree *wt;

	cl_git_fail(git_worktree_lookup(&wt, fixture.repo, "nonexistent"));
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
	git_buf buf = GIT_BUF_INIT, path = GIT_BUF_INIT;

	cl_git_pass(git_buf_sets(&buf, "/path/to/nonexistent/commondir"));
	cl_git_pass(git_buf_printf(&path,
		    "%s/worktrees/testrepo-worktree/commondir",
		    fixture.repo->commondir));
	cl_git_pass(git_futils_writebuffer(&buf, path.ptr, O_RDWR, 0644));

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_fail(git_repository_open_from_worktree(&repo, wt));

	git_buf_free(&buf);
	git_buf_free(&path);
	git_worktree_free(wt);
}

void test_worktree_worktree__open_invalid_gitdir(void)
{
	git_worktree *wt;
	git_repository *repo;
	git_buf buf = GIT_BUF_INIT, path = GIT_BUF_INIT;

	cl_git_pass(git_buf_sets(&buf, "/path/to/nonexistent/gitdir"));
	cl_git_pass(git_buf_printf(&path,
		    "%s/worktrees/testrepo-worktree/gitdir",
		    fixture.repo->commondir));
	cl_git_pass(git_futils_writebuffer(&buf, path.ptr, O_RDWR, 0644));

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_fail(git_repository_open_from_worktree(&repo, wt));

	git_buf_free(&buf);
	git_buf_free(&path);
	git_worktree_free(wt);
}

void test_worktree_worktree__open_invalid_parent(void)
{
	git_worktree *wt;
	git_repository *repo;
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_buf_sets(&buf, "/path/to/nonexistent/gitdir"));
	cl_git_pass(git_futils_writebuffer(&buf,
		    fixture.worktree->gitlink, O_RDWR, 0644));

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_fail(git_repository_open_from_worktree(&repo, wt));

	git_buf_free(&buf);
	git_worktree_free(wt);
}

void test_worktree_worktree__init(void)
{
	git_worktree *wt;
	git_repository *repo;
	git_reference *branch;
	git_buf path = GIT_BUF_INIT;

	cl_git_pass(git_buf_joinpath(&path, fixture.repo->workdir, "../worktree-new"));
	cl_git_pass(git_worktree_add(&wt, fixture.repo, "worktree-new", path.ptr, NULL));

	/* Open and verify created repo */
	cl_git_pass(git_repository_open(&repo, path.ptr));
	cl_assert(git__suffixcmp(git_repository_workdir(repo), "worktree-new/") == 0);
	cl_git_pass(git_branch_lookup(&branch, repo, "worktree-new", GIT_BRANCH_LOCAL));

	git_buf_free(&path);
	git_worktree_free(wt);
	git_reference_free(branch);
	git_repository_free(repo);
}

void test_worktree_worktree__add_locked(void)
{
	git_worktree *wt;
	git_repository *repo;
	git_reference *branch;
	git_buf path = GIT_BUF_INIT;
	git_worktree_add_options opts = GIT_WORKTREE_ADD_OPTIONS_INIT;

	opts.lock = 1;

	cl_git_pass(git_buf_joinpath(&path, fixture.repo->workdir, "../worktree-locked"));
	cl_git_pass(git_worktree_add(&wt, fixture.repo, "worktree-locked", path.ptr, &opts));

	/* Open and verify created repo */
	cl_assert(git_worktree_is_locked(NULL, wt));
	cl_git_pass(git_repository_open(&repo, path.ptr));
	cl_assert(git__suffixcmp(git_repository_workdir(repo), "worktree-locked/") == 0);
	cl_git_pass(git_branch_lookup(&branch, repo, "worktree-locked", GIT_BRANCH_LOCAL));

	git_buf_free(&path);
	git_worktree_free(wt);
	git_reference_free(branch);
	git_repository_free(repo);
}

void test_worktree_worktree__init_existing_branch(void)
{
	git_reference *head, *branch;
	git_commit *commit;
	git_worktree *wt;
	git_buf path = GIT_BUF_INIT;

	cl_git_pass(git_repository_head(&head, fixture.repo));
	cl_git_pass(git_commit_lookup(&commit, fixture.repo, &head->target.oid));
	cl_git_pass(git_branch_create(&branch, fixture.repo, "worktree-new", commit, false));

	cl_git_pass(git_buf_joinpath(&path, fixture.repo->workdir, "../worktree-new"));
	cl_git_fail(git_worktree_add(&wt, fixture.repo, "worktree-new", path.ptr, NULL));

	git_buf_free(&path);
	git_commit_free(commit);
	git_reference_free(head);
	git_reference_free(branch);
}

void test_worktree_worktree__init_existing_worktree(void)
{
	git_worktree *wt;
	git_buf path = GIT_BUF_INIT;

	cl_git_pass(git_buf_joinpath(&path, fixture.repo->workdir, "../worktree-new"));
	cl_git_fail(git_worktree_add(&wt, fixture.repo, "testrepo-worktree", path.ptr, NULL));

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_assert_equal_s(wt->gitlink_path, fixture.worktree->gitlink);

	git_buf_free(&path);
	git_worktree_free(wt);
}

void test_worktree_worktree__init_existing_path(void)
{
	const char *wtfiles[] = { "HEAD", "commondir", "gitdir", "index" };
	git_worktree *wt;
	git_buf path = GIT_BUF_INIT;
	unsigned i;

	/* Delete files to verify they have not been created by
	 * the init call */
	for (i = 0; i < ARRAY_SIZE(wtfiles); i++) {
		cl_git_pass(git_buf_joinpath(&path,
			    fixture.worktree->gitdir, wtfiles[i]));
		cl_git_pass(p_unlink(path.ptr));
	}

	cl_git_pass(git_buf_joinpath(&path, fixture.repo->workdir, "../testrepo-worktree"));
	cl_git_fail(git_worktree_add(&wt, fixture.repo, "worktree-new", path.ptr, NULL));

	/* Verify files have not been re-created */
	for (i = 0; i < ARRAY_SIZE(wtfiles); i++) {
		cl_git_pass(git_buf_joinpath(&path,
			    fixture.worktree->gitdir, wtfiles[i]));
		cl_assert(!git_path_exists(path.ptr));
	}

	git_buf_free(&path);
}

void test_worktree_worktree__init_submodule(void)
{
	git_repository *repo, *sm, *wt;
	git_worktree *worktree;
	git_buf path = GIT_BUF_INIT;

	cleanup_fixture_worktree(&fixture);
	repo = setup_fixture_submod2();

	cl_git_pass(git_buf_joinpath(&path, repo->workdir, "sm_unchanged"));
	cl_git_pass(git_repository_open(&sm, path.ptr));
	cl_git_pass(git_buf_joinpath(&path, repo->workdir, "../worktree/"));
	cl_git_pass(git_worktree_add(&worktree, sm, "repo-worktree", path.ptr, NULL));
	cl_git_pass(git_repository_open_from_worktree(&wt, worktree));

	cl_git_pass(git_path_prettify_dir(&path, path.ptr, NULL));
	cl_assert_equal_s(path.ptr, wt->workdir);
	cl_git_pass(git_path_prettify_dir(&path, sm->commondir, NULL));
	cl_assert_equal_s(sm->commondir, wt->commondir);

	cl_git_pass(git_buf_joinpath(&path, sm->gitdir, "worktrees/repo-worktree/"));
	cl_assert_equal_s(path.ptr, wt->gitdir);

	git_buf_free(&path);
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

	git_buf_free(&reason);
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

	git_buf_free(&reason);
	git_worktree_free(wt);
}

void test_worktree_worktree__unlock_unlocked_worktree(void)
{
	git_worktree *wt;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_assert(!git_worktree_is_locked(NULL, wt));
	cl_assert(git_worktree_unlock(wt) == 0);
	cl_assert(!wt->locked);

	git_worktree_free(wt);
}

void test_worktree_worktree__unlock_locked_worktree(void)
{
	git_worktree *wt;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_pass(git_worktree_lock(wt, NULL));
	cl_assert(git_worktree_is_locked(NULL, wt));
	cl_git_pass(git_worktree_unlock(wt));
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

	cl_assert(!git_path_exists(wt->gitdir_path));
	cl_assert(git_path_exists(wt->gitlink_path));

	git_worktree_free(wt);
}

void test_worktree_worktree__prune_worktree(void)
{
	git_worktree_prune_options opts = GIT_WORKTREE_PRUNE_OPTIONS_INIT;
	git_worktree *wt;

	opts.flags = GIT_WORKTREE_PRUNE_VALID|GIT_WORKTREE_PRUNE_WORKING_TREE;

	cl_git_pass(git_worktree_lookup(&wt, fixture.repo, "testrepo-worktree"));
	cl_git_pass(git_worktree_prune(wt, &opts));

	cl_assert(!git_path_exists(wt->gitdir_path));
	cl_assert(!git_path_exists(wt->gitlink_path));

	git_worktree_free(wt);
}

static int read_head_ref(git_repository *repo, const char *path, void *payload)
{
	git_vector *refs = (git_vector *) payload;
	git_reference *head;

	GIT_UNUSED(repo);

	cl_git_pass(git_reference__read_head(&head, repo, path));

	git_vector_insert(refs, head);

	return 0;
}

void test_worktree_worktree__foreach_head_gives_same_results_in_wt_and_repo(void)
{
	git_vector repo_refs = GIT_VECTOR_INIT, worktree_refs = GIT_VECTOR_INIT;
	git_reference *heads[2];
	size_t i;

	cl_git_pass(git_reference_lookup(&heads[0], fixture.repo, GIT_HEAD_FILE));
	cl_git_pass(git_reference_lookup(&heads[1], fixture.worktree, GIT_HEAD_FILE));

	cl_git_pass(git_repository_foreach_head(fixture.repo, read_head_ref, &repo_refs));
	cl_git_pass(git_repository_foreach_head(fixture.worktree, read_head_ref, &worktree_refs));

	cl_assert_equal_i(repo_refs.length, ARRAY_SIZE(heads));
	cl_assert_equal_i(worktree_refs.length, ARRAY_SIZE(heads));

	for (i = 0; i < ARRAY_SIZE(heads); i++) {
		cl_assert_equal_s(heads[i]->name, ((git_reference *) repo_refs.contents[i])->name);
		cl_assert_equal_s(heads[i]->name, ((git_reference *) repo_refs.contents[i])->name);
		cl_assert_equal_s(heads[i]->name, ((git_reference *) worktree_refs.contents[i])->name);

		git_reference_free(heads[i]);
		git_reference_free(repo_refs.contents[i]);
		git_reference_free(worktree_refs.contents[i]);
	}

	git_vector_free(&repo_refs);
	git_vector_free(&worktree_refs);
}
