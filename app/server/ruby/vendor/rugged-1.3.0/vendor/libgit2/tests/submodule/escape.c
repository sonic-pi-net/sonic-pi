#include "clar_libgit2.h"
#include "posix.h"
#include "path.h"
#include "submodule_helpers.h"
#include "futils.h"
#include "repository.h"

static git_repository *g_repo = NULL;

void test_submodule_escape__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

#define EVIL_SM_NAME "../../modules/evil"
#define EVIL_SM_NAME_WINDOWS "..\\\\..\\\\modules\\\\evil"
#define EVIL_SM_NAME_WINDOWS_UNESC "..\\..\\modules\\evil"

static int find_evil(git_submodule *sm, const char *name, void *payload)
{
	int *foundit = (int *) payload;

	GIT_UNUSED(sm);

	if (!git__strcmp(EVIL_SM_NAME, name) ||
	    !git__strcmp(EVIL_SM_NAME_WINDOWS_UNESC, name))
		*foundit = true;

	return 0;
}

void test_submodule_escape__from_gitdir(void)
{
	int foundit;
	git_submodule *sm;
	git_buf buf = GIT_BUF_INIT;
	unsigned int sm_location;

	g_repo = setup_fixture_submodule_simple();

	cl_git_pass(git_buf_joinpath(&buf, git_repository_workdir(g_repo), ".gitmodules"));
	cl_git_rewritefile(buf.ptr,
			   "[submodule \"" EVIL_SM_NAME "\"]\n"
			   "    path = testrepo\n"
			   "    url = ../testrepo.git\n");
	git_buf_dispose(&buf);

	/* Find it all the different ways we know about it */
	foundit = 0;
	cl_git_pass(git_submodule_foreach(g_repo, find_evil, &foundit));
	cl_assert_equal_i(0, foundit);
	cl_git_fail_with(GIT_ENOTFOUND, git_submodule_lookup(&sm, g_repo, EVIL_SM_NAME));
	/*
	 * We do know about this as it's in the index and HEAD, but the data is
	 * incomplete as there is no configured data for it (we pretend it
	 * doesn't exist). This leaves us with an odd situation but it's
	 * consistent with what we would do if we did add a submodule with no
	 * configuration.
	 */
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));
	cl_git_pass(git_submodule_location(&sm_location, sm));
	cl_assert_equal_i(GIT_SUBMODULE_STATUS_IN_INDEX | GIT_SUBMODULE_STATUS_IN_HEAD, sm_location);
	git_submodule_free(sm);
}

void test_submodule_escape__from_gitdir_windows(void)
{
	int foundit;
	git_submodule *sm;
	git_buf buf = GIT_BUF_INIT;
	unsigned int sm_location;

	g_repo = setup_fixture_submodule_simple();

	cl_git_pass(git_buf_joinpath(&buf, git_repository_workdir(g_repo), ".gitmodules"));
	cl_git_rewritefile(buf.ptr,
			   "[submodule \"" EVIL_SM_NAME_WINDOWS "\"]\n"
			   "    path = testrepo\n"
			   "    url = ../testrepo.git\n");
	git_buf_dispose(&buf);

	/* Find it all the different ways we know about it */
	foundit = 0;
	cl_git_pass(git_submodule_foreach(g_repo, find_evil, &foundit));
	cl_assert_equal_i(0, foundit);
	cl_git_fail_with(GIT_ENOTFOUND, git_submodule_lookup(&sm, g_repo, EVIL_SM_NAME_WINDOWS_UNESC));
	/*
	 * We do know about this as it's in the index and HEAD, but the data is
	 * incomplete as there is no configured data for it (we pretend it
	 * doesn't exist). This leaves us with an odd situation but it's
	 * consistent with what we would do if we did add a submodule with no
	 * configuration.
	 */
	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));
	cl_git_pass(git_submodule_location(&sm_location, sm));
	cl_assert_equal_i(GIT_SUBMODULE_STATUS_IN_INDEX | GIT_SUBMODULE_STATUS_IN_HEAD, sm_location);
	git_submodule_free(sm);
}
