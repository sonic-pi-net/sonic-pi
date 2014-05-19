#include "clar_libgit2.h"
#include "posix.h"
#include "path.h"
#include "submodule_helpers.h"

static git_repository *g_repo = NULL;

void test_submodule_add__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void assert_submodule_url(const char* name, const char *url)
{
	git_config *cfg;
	const char *s;
	git_buf key = GIT_BUF_INIT;

	cl_git_pass(git_repository_config(&cfg, g_repo));

	cl_git_pass(git_buf_printf(&key, "submodule.%s.url", name));
	cl_git_pass(git_config_get_string(&s, cfg, git_buf_cstr(&key)));
	cl_assert_equal_s(s, url);

	git_config_free(cfg);
	git_buf_free(&key);
}

void test_submodule_add__url_absolute(void)
{
	git_submodule *sm;

	g_repo = setup_fixture_submod2();

	/* re-add existing submodule */
	cl_git_fail_with(
		GIT_EEXISTS,
		git_submodule_add_setup(NULL, g_repo, "whatever", "sm_unchanged", 1));

	/* add a submodule using a gitlink */

	cl_git_pass(
		git_submodule_add_setup(&sm, g_repo, "https://github.com/libgit2/libgit2.git", "sm_libgit2", 1)
		);
	git_submodule_free(sm);

	cl_assert(git_path_isfile("submod2/" "sm_libgit2" "/.git"));

	cl_assert(git_path_isdir("submod2/.git/modules"));
	cl_assert(git_path_isdir("submod2/.git/modules/" "sm_libgit2"));
	cl_assert(git_path_isfile("submod2/.git/modules/" "sm_libgit2" "/HEAD"));
	assert_submodule_url("sm_libgit2", "https://github.com/libgit2/libgit2.git");

	/* add a submodule not using a gitlink */

	cl_git_pass(
		git_submodule_add_setup(&sm, g_repo, "https://github.com/libgit2/libgit2.git", "sm_libgit2b", 0)
		);
	git_submodule_free(sm);

	cl_assert(git_path_isdir("submod2/" "sm_libgit2b" "/.git"));
	cl_assert(git_path_isfile("submod2/" "sm_libgit2b" "/.git/HEAD"));
	cl_assert(!git_path_exists("submod2/.git/modules/" "sm_libgit2b"));
	assert_submodule_url("sm_libgit2b", "https://github.com/libgit2/libgit2.git");
}

void test_submodule_add__url_relative(void)
{
	git_submodule *sm;
	git_remote *remote;

	/* default remote url is https://github.com/libgit2/false.git */
	g_repo = cl_git_sandbox_init("testrepo2");

	/* make sure we don't default to origin - rename origin -> test_remote */
	cl_git_pass(git_remote_load(&remote, g_repo, "origin"));
	cl_git_pass(git_remote_rename(remote, "test_remote", NULL, NULL));
	cl_git_fail(git_remote_load(&remote, g_repo, "origin"));
	git_remote_free(remote);

	cl_git_pass(
		git_submodule_add_setup(&sm, g_repo, "../TestGitRepository", "TestGitRepository", 1)
		);
	git_submodule_free(sm);

	assert_submodule_url("TestGitRepository", "https://github.com/libgit2/TestGitRepository");
}

void test_submodule_add__url_relative_to_origin(void)
{
	git_submodule *sm;

	/* default remote url is https://github.com/libgit2/false.git */
	g_repo = cl_git_sandbox_init("testrepo2");

	cl_git_pass(
		git_submodule_add_setup(&sm, g_repo, "../TestGitRepository", "TestGitRepository", 1)
		);
	git_submodule_free(sm);

	assert_submodule_url("TestGitRepository", "https://github.com/libgit2/TestGitRepository");
}

void test_submodule_add__url_relative_to_workdir(void)
{
	git_submodule *sm;

	/* In this repo, HEAD (master) has no remote tracking branc h*/
	g_repo = cl_git_sandbox_init("testrepo");

	cl_git_pass(
		git_submodule_add_setup(&sm, g_repo, "./", "TestGitRepository", 1)
		);
	git_submodule_free(sm);

	assert_submodule_url("TestGitRepository", git_repository_workdir(g_repo));
}
