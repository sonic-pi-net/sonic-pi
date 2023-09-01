#include "clar_libgit2.h"
#include "submodule_helpers.h"
#include "path.h"

static git_repository *g_parent;
static git_repository *g_child;
static git_submodule *g_module;

void test_submodule_open__initialize(void)
{
	g_parent = setup_fixture_submod2();
}

void test_submodule_open__cleanup(void)
{
	git_submodule_free(g_module);
	git_repository_free(g_child);
	cl_git_sandbox_cleanup();
	g_parent = NULL;
	g_child = NULL;
	g_module = NULL;
}

static void assert_sm_valid(git_repository *parent, git_repository *child, const char *sm_name)
{
	git_str expected = GIT_STR_INIT, actual = GIT_STR_INIT;

	/* assert working directory */
	cl_git_pass(git_str_joinpath(&expected, git_repository_workdir(parent), sm_name));
	cl_git_pass(git_fs_path_prettify_dir(&expected, expected.ptr, NULL));
	cl_git_pass(git_str_sets(&actual, git_repository_workdir(child)));
	cl_git_pass(git_fs_path_prettify_dir(&actual, actual.ptr, NULL));
	cl_assert_equal_s(expected.ptr, actual.ptr);

	git_str_clear(&expected);
	git_str_clear(&actual);

	/* assert common directory */
	cl_git_pass(git_str_joinpath(&expected, git_repository_commondir(parent), "modules"));
	cl_git_pass(git_str_joinpath(&expected, expected.ptr, sm_name));
	cl_git_pass(git_fs_path_prettify_dir(&expected, expected.ptr, NULL));
	cl_git_pass(git_str_sets(&actual, git_repository_commondir(child)));
	cl_git_pass(git_fs_path_prettify_dir(&actual, actual.ptr, NULL));
	cl_assert_equal_s(expected.ptr, actual.ptr);

	/* assert git directory */
	cl_git_pass(git_str_sets(&actual, git_repository_path(child)));
	cl_git_pass(git_fs_path_prettify_dir(&actual, actual.ptr, NULL));
	cl_assert_equal_s(expected.ptr, actual.ptr);

	git_str_dispose(&expected);
	git_str_dispose(&actual);
}

void test_submodule_open__opening_via_lookup_succeeds(void)
{
	cl_git_pass(git_submodule_lookup(&g_module, g_parent, "sm_unchanged"));
	cl_git_pass(git_submodule_open(&g_child, g_module));
	assert_sm_valid(g_parent, g_child, "sm_unchanged");
}

void test_submodule_open__direct_open_succeeds(void)
{
	git_str path = GIT_STR_INIT;

	cl_git_pass(git_str_joinpath(&path, git_repository_workdir(g_parent), "sm_unchanged"));
	cl_git_pass(git_repository_open(&g_child, path.ptr));
	assert_sm_valid(g_parent, g_child, "sm_unchanged");

	git_str_dispose(&path);
}

void test_submodule_open__direct_open_succeeds_for_broken_sm_with_gitdir(void)
{
	git_str path = GIT_STR_INIT;

	/*
	 * This is actually not a valid submodule, but we
	 * encountered at least one occasion where the gitdir
	 * file existed inside of a submodule's gitdir. As we are
	 * now able to open these submodules correctly, we still
	 * add a test for this.
	 */
	cl_git_mkfile("submod2/.git/modules/sm_unchanged/gitdir", ".git");
	cl_git_pass(git_str_joinpath(&path, git_repository_workdir(g_parent), "sm_unchanged"));
	cl_git_pass(git_repository_open(&g_child, path.ptr));
	assert_sm_valid(g_parent, g_child, "sm_unchanged");

	git_str_dispose(&path);
}
