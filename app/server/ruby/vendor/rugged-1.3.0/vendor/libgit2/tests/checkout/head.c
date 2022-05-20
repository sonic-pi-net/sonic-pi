#include "clar_libgit2.h"
#include "refs.h"
#include "repo/repo_helpers.h"
#include "path.h"
#include "futils.h"

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

void test_checkout_head__do_not_remove_untracked_file(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;

	cl_git_pass(p_mkdir("testrepo/tracked", 0755));
	cl_git_mkfile("testrepo/tracked/tracked", "tracked\n");
	cl_git_mkfile("testrepo/tracked/untracked", "untracked\n");

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_add_bypath(index, "tracked/tracked"));
	cl_git_pass(git_index_write(index));

	git_index_free(index);

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_assert(!git_path_isfile("testrepo/tracked/tracked"));
	cl_assert(git_path_isfile("testrepo/tracked/untracked"));
}

void test_checkout_head__do_not_remove_untracked_file_in_subdir(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;

	cl_git_pass(p_mkdir("testrepo/tracked", 0755));
	cl_git_pass(p_mkdir("testrepo/tracked/subdir", 0755));
	cl_git_mkfile("testrepo/tracked/tracked", "tracked\n");
	cl_git_mkfile("testrepo/tracked/subdir/tracked", "tracked\n");
	cl_git_mkfile("testrepo/tracked/subdir/untracked", "untracked\n");

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_add_bypath(index, "tracked/tracked"));
	cl_git_pass(git_index_add_bypath(index, "tracked/subdir/tracked"));
	cl_git_pass(git_index_write(index));

	git_index_free(index);

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_assert(!git_path_isfile("testrepo/tracked/tracked"));
	cl_assert(!git_path_isfile("testrepo/tracked/subdir/tracked"));
	cl_assert(git_path_isfile("testrepo/tracked/subdir/untracked"));
}

void test_checkout_head__do_remove_untracked_paths(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;
	char *paths[] = {"tracked/untracked"};

	cl_git_pass(p_mkdir("testrepo/tracked", 0755));
	cl_git_pass(p_mkdir("testrepo/tracked/subdir", 0755));
	cl_git_mkfile("testrepo/tracked/tracked", "tracked\n");
	cl_git_mkfile("testrepo/tracked/untracked", "untracked\n");

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_add_bypath(index, "tracked/tracked"));
	cl_git_pass(git_index_write(index));

	git_index_free(index);

	opts.checkout_strategy = GIT_CHECKOUT_FORCE | GIT_CHECKOUT_REMOVE_UNTRACKED;
	opts.paths.strings = paths;
	opts.paths.count = 1;
	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_assert(git_path_isfile("testrepo/tracked/tracked"));
	cl_assert(!git_path_isfile("testrepo/tracked/untracked"));
}

void test_checkout_head__do_remove_tracked_subdir(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;

	cl_git_pass(p_mkdir("testrepo/subdir", 0755));
	cl_git_pass(p_mkdir("testrepo/subdir/tracked", 0755));
	cl_git_mkfile("testrepo/subdir/tracked-file", "tracked\n");
	cl_git_mkfile("testrepo/subdir/untracked-file", "untracked\n");
	cl_git_mkfile("testrepo/subdir/tracked/tracked1", "tracked\n");
	cl_git_mkfile("testrepo/subdir/tracked/tracked2", "tracked\n");

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_add_bypath(index, "subdir/tracked-file"));
	cl_git_pass(git_index_add_bypath(index, "subdir/tracked/tracked1"));
	cl_git_pass(git_index_add_bypath(index, "subdir/tracked/tracked2"));
	cl_git_pass(git_index_write(index));

	git_index_free(index);

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_assert(!git_path_isdir("testrepo/subdir/tracked"));
	cl_assert(!git_path_isfile("testrepo/subdir/tracked-file"));
	cl_assert(git_path_isfile("testrepo/subdir/untracked-file"));
}

void test_checkout_head__typechange_workdir(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_object *target;
	struct stat st;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_revparse_single(&target, g_repo, "HEAD"));
	cl_git_pass(git_reset(g_repo, target, GIT_RESET_HARD, NULL));

	cl_must_pass(p_chmod("testrepo/new.txt", 0755));
	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_git_pass(p_stat("testrepo/new.txt", &st));
	cl_assert(!GIT_PERMS_IS_EXEC(st.st_mode));

	git_object_free(target);
}

void test_checkout_head__typechange_index_and_workdir(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_object *target;
	git_index *index;
	struct stat st;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_revparse_single(&target, g_repo, "HEAD"));
	cl_git_pass(git_reset(g_repo, target, GIT_RESET_HARD, NULL));

	cl_must_pass(p_chmod("testrepo/new.txt", 0755));
	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_add_bypath(index, "new.txt"));
	cl_git_pass(git_index_write(index));
	cl_git_pass(git_checkout_head(g_repo, &opts));

	cl_git_pass(p_stat("testrepo/new.txt", &st));
	cl_assert(!GIT_PERMS_IS_EXEC(st.st_mode));

	git_object_free(target);
	git_index_free(index);
}

void test_checkout_head__workdir_filemode_is_simplified(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_object *target, *branch;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_revparse_single(&target, g_repo, "a38d028f71eaa590febb7d716b1ca32350cf70da"));
	cl_git_pass(git_reset(g_repo, target, GIT_RESET_HARD, NULL));

	cl_must_pass(p_chmod("testrepo/branch_file.txt", 0666));

	/*
	 * Checkout should not fail with a conflict; though the file mode
	 * on disk is literally different to the base (0666 vs 0644), Git
	 * ignores the actual mode and simply treats both as non-executable.
	 */
	cl_git_pass(git_revparse_single(&branch, g_repo, "099fabac3a9ea935598528c27f866e34089c2eff"));

	opts.checkout_strategy &= ~GIT_CHECKOUT_FORCE;
	opts.checkout_strategy |=  GIT_CHECKOUT_SAFE;
	cl_git_pass(git_checkout_tree(g_repo, branch, NULL));

	git_object_free(branch);
	git_object_free(target);
}

void test_checkout_head__obeys_filemode_true(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_object *target, *branch;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	/* In this commit, `README` is executable */
	cl_git_pass(git_revparse_single(&target, g_repo, "f9ed4af42472941da45a3c"));
	cl_git_pass(git_reset(g_repo, target, GIT_RESET_HARD, NULL));

	cl_repo_set_bool(g_repo, "core.filemode", true);
	cl_must_pass(p_chmod("testrepo/README", 0644));

	/*
	 * Checkout will fail with a conflict; the file mode is updated in
	 * the checkout target, but the contents have changed in our branch.
	 */
	cl_git_pass(git_revparse_single(&branch, g_repo, "099fabac3a9ea935598528c27f866e34089c2eff"));

	opts.checkout_strategy &= ~GIT_CHECKOUT_FORCE;
	opts.checkout_strategy |=  GIT_CHECKOUT_SAFE;
	cl_git_fail_with(GIT_ECONFLICT, git_checkout_tree(g_repo, branch, NULL));

	git_object_free(branch);
	git_object_free(target);
}

void test_checkout_head__obeys_filemode_false(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_object *target, *branch;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	/* In this commit, `README` is executable */
	cl_git_pass(git_revparse_single(&target, g_repo, "f9ed4af42472941da45a3c"));
	cl_git_pass(git_reset(g_repo, target, GIT_RESET_HARD, NULL));

	cl_repo_set_bool(g_repo, "core.filemode", false);
	cl_must_pass(p_chmod("testrepo/README", 0644));

	/*
	 * Checkout will fail with a conflict; the file contents are updated
	 * in the checkout target, but the filemode has changed in our branch.
	 */
	cl_git_pass(git_revparse_single(&branch, g_repo, "099fabac3a9ea935598528c27f866e34089c2eff"));

	opts.checkout_strategy &= ~GIT_CHECKOUT_FORCE;
	opts.checkout_strategy |=  GIT_CHECKOUT_SAFE;
	cl_git_pass(git_checkout_tree(g_repo, branch, NULL));

	git_object_free(branch);
	git_object_free(target);
}
