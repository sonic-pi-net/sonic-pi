#include "clar_libgit2.h"
#include "refs.h"
#include "repo/repo_helpers.h"
#include "path.h"
#include "fileops.h"

static git_repository *g_repo;

void test_checkout_head__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");
}

void test_checkout_head__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_checkout_head__unborn_head_returns_GIT_EUNBORNBRANCH(void)
{
	make_head_unborn(g_repo, NON_EXISTING_HEAD);

	cl_assert_equal_i(GIT_EUNBORNBRANCH, git_checkout_head(g_repo, NULL));
}

void test_checkout_head__with_index_only_tree(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;

	/* let's start by getting things into a known state */

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	cl_git_pass(git_checkout_head(g_repo, &opts));

	/* now let's stage some new stuff including a new directory */

	cl_git_pass(git_repository_index(&index, g_repo));

	p_mkdir("testrepo/newdir", 0777);
    cl_git_mkfile("testrepo/newdir/newfile.txt", "new file\n");

	cl_git_pass(git_index_add_bypath(index, "newdir/newfile.txt"));
	cl_git_pass(git_index_write(index));

	cl_assert(git_path_isfile("testrepo/newdir/newfile.txt"));
	cl_assert(git_index_get_bypath(index, "newdir/newfile.txt", 0) != NULL);

	git_index_free(index);

	/* okay, so now we have staged this new file; let's see if we can remove */

	opts.checkout_strategy = GIT_CHECKOUT_FORCE | GIT_CHECKOUT_REMOVE_UNTRACKED;
	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_assert(!git_path_isfile("testrepo/newdir/newfile.txt"));
	cl_assert(git_index_get_bypath(index, "newdir/newfile.txt", 0) == NULL);

	git_index_free(index);
}
