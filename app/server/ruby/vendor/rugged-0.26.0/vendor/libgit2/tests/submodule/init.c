#include "clar_libgit2.h"
#include "posix.h"
#include "path.h"
#include "submodule_helpers.h"
#include "fileops.h"

static git_repository *g_repo = NULL;

void test_submodule_init__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_submodule_init__absolute_url(void)
{
	git_submodule *sm;
	git_config *cfg;
	git_buf absolute_url = GIT_BUF_INIT;
	const char *config_url;

	g_repo = setup_fixture_submodule_simple();

	cl_assert(git_path_dirname_r(&absolute_url, git_repository_workdir(g_repo)) > 0);
	cl_git_pass(git_buf_joinpath(&absolute_url, absolute_url.ptr, "testrepo.git"));

	/* write the absolute url to the .gitmodules file*/
	cl_git_pass(git_submodule_set_url(g_repo, "testrepo", absolute_url.ptr));

	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	/* verify that the .gitmodules is set with an absolute path*/
	cl_assert_equal_s(absolute_url.ptr, git_submodule_url(sm));

	/* init and verify that absolute path is written to .git/config */
	cl_git_pass(git_submodule_init(sm, false));

	cl_git_pass(git_repository_config_snapshot(&cfg, g_repo));

	cl_git_pass(git_config_get_string(&config_url, cfg, "submodule.testrepo.url"));
	cl_assert_equal_s(absolute_url.ptr, config_url);

	git_buf_free(&absolute_url);
	git_config_free(cfg);
	git_submodule_free(sm);
}

void test_submodule_init__relative_url(void)
{
	git_submodule *sm;
	git_config *cfg;
	git_buf absolute_url = GIT_BUF_INIT;
	const char *config_url;

	g_repo = setup_fixture_submodule_simple();

	cl_assert(git_path_dirname_r(&absolute_url, git_repository_workdir(g_repo)) > 0);
	cl_git_pass(git_buf_joinpath(&absolute_url, absolute_url.ptr, "testrepo.git"));

	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	/* verify that the .gitmodules is set with an absolute path*/
	cl_assert_equal_s("../testrepo.git", git_submodule_url(sm));

	/* init and verify that absolute path is written to .git/config */
	cl_git_pass(git_submodule_init(sm, false));

	cl_git_pass(git_repository_config_snapshot(&cfg, g_repo));

	cl_git_pass(git_config_get_string(&config_url, cfg, "submodule.testrepo.url"));
	cl_assert_equal_s(absolute_url.ptr, config_url);

	git_buf_free(&absolute_url);
	git_config_free(cfg);
	git_submodule_free(sm);
}

void test_submodule_init__relative_url_detached_head(void)
{
	git_submodule *sm;
	git_config *cfg;
	git_buf absolute_url = GIT_BUF_INIT;
	const char *config_url;
	git_reference *head_ref = NULL;
	git_object *head_commit = NULL;

	g_repo = setup_fixture_submodule_simple();

	/* Put the parent repository into a detached head state. */
	cl_git_pass(git_repository_head(&head_ref, g_repo));
	cl_git_pass(git_reference_peel(&head_commit, head_ref, GIT_OBJ_COMMIT));

	cl_git_pass(git_repository_set_head_detached(g_repo, git_commit_id((git_commit *)head_commit)));

	cl_assert(git_path_dirname_r(&absolute_url, git_repository_workdir(g_repo)) > 0);
	cl_git_pass(git_buf_joinpath(&absolute_url, absolute_url.ptr, "testrepo.git"));

	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));

	/* verify that the .gitmodules is set with an absolute path*/
	cl_assert_equal_s("../testrepo.git", git_submodule_url(sm));

	/* init and verify that absolute path is written to .git/config */
	cl_git_pass(git_submodule_init(sm, false));

	cl_git_pass(git_repository_config_snapshot(&cfg, g_repo));

	cl_git_pass(git_config_get_string(&config_url, cfg, "submodule.testrepo.url"));
	cl_assert_equal_s(absolute_url.ptr, config_url);

	git_buf_free(&absolute_url);
	git_config_free(cfg);
	git_object_free(head_commit);
	git_reference_free(head_ref);
	git_submodule_free(sm);
}
