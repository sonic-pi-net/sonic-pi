#include "clar_libgit2.h"
#include "git2/sys/repository.h"

#include "buffer.h"
#include "posix.h"
#include "util.h"
#include "path.h"
#include "futils.h"

static git_repository *repo;

void test_repo_setters__initialize(void)
{
	cl_fixture_sandbox("testrepo.git");
	cl_git_pass(git_repository_open(&repo, "testrepo.git"));
	cl_must_pass(p_mkdir("new_workdir", 0777));
}

void test_repo_setters__cleanup(void)
{
	git_repository_free(repo);
	repo = NULL;

	cl_fixture_cleanup("testrepo.git");
	cl_fixture_cleanup("new_workdir");
}

void test_repo_setters__setting_a_workdir_turns_a_bare_repository_into_a_standard_one(void)
{
	cl_assert(git_repository_is_bare(repo) == 1);

	cl_assert(git_repository_workdir(repo) == NULL);
	cl_git_pass(git_repository_set_workdir(repo, "./new_workdir", false));

	cl_assert(git_repository_workdir(repo) != NULL);
	cl_assert(git_repository_is_bare(repo) == 0);
}

void test_repo_setters__setting_a_workdir_prettifies_its_path(void)
{
	cl_git_pass(git_repository_set_workdir(repo, "./new_workdir", false));

	cl_assert(git__suffixcmp(git_repository_workdir(repo), "new_workdir/") == 0);
}

void test_repo_setters__setting_a_workdir_creates_a_gitlink(void)
{
	git_config *cfg;
	git_buf buf = GIT_BUF_INIT;
	git_buf content = GIT_BUF_INIT;

	cl_git_pass(git_repository_set_workdir(repo, "./new_workdir", true));

	cl_assert(git_path_isfile("./new_workdir/.git"));

	cl_git_pass(git_futils_readbuffer(&content, "./new_workdir/.git"));
	cl_assert(git__prefixcmp(git_buf_cstr(&content), "gitdir: ") == 0);
	cl_assert(git__suffixcmp(git_buf_cstr(&content), "testrepo.git/") == 0);
	git_buf_dispose(&content);

	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "core.worktree"));
	cl_assert(git__suffixcmp(git_buf_cstr(&buf), "new_workdir/") == 0);

	git_buf_dispose(&buf);
	git_config_free(cfg);
}

void test_repo_setters__setting_a_new_index_on_a_repo_which_has_already_loaded_one_properly_honors_the_refcount(void)
{
	git_index *new_index;

	cl_git_pass(git_index_open(&new_index, "./my-index"));
	cl_assert(((git_refcount *)new_index)->refcount.val == 1);

	git_repository_set_index(repo, new_index);
	cl_assert(((git_refcount *)new_index)->refcount.val == 2);

	git_repository_free(repo);
	cl_assert(((git_refcount *)new_index)->refcount.val == 1);

	git_index_free(new_index);

	/* 
	 * Ensure the cleanup method won't try to free the repo as it's already been taken care of
	 */
	repo = NULL;
}

void test_repo_setters__setting_a_new_odb_on_a_repo_which_already_loaded_one_properly_honors_the_refcount(void)
{
	git_odb *new_odb;

	cl_git_pass(git_odb_open(&new_odb, "./testrepo.git/objects"));
	cl_assert(((git_refcount *)new_odb)->refcount.val == 1);

	git_repository_set_odb(repo, new_odb);
	cl_assert(((git_refcount *)new_odb)->refcount.val == 2);

	git_repository_free(repo);
	cl_assert(((git_refcount *)new_odb)->refcount.val == 1);

	git_odb_free(new_odb);

	/* 
	 * Ensure the cleanup method won't try to free the repo as it's already been taken care of
	 */
	repo = NULL;
}
