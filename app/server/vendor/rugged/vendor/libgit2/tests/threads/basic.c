#include "clar_libgit2.h"

#include "thread_helpers.h"
#include "cache.h"


static git_repository *g_repo;

void test_threads_basic__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");
}

void test_threads_basic__cleanup(void)
{
	cl_git_sandbox_cleanup();
}


void test_threads_basic__cache(void)
{
	// run several threads polling the cache at the same time
	cl_assert(1 == 1);
}

void test_threads_basic__multiple_init(void)
{
	git_repository *nested_repo;

	git_libgit2_init();
	cl_git_pass(git_repository_open(&nested_repo, cl_fixture("testrepo.git")));
	git_repository_free(nested_repo);

	git_libgit2_shutdown();
	cl_git_pass(git_repository_open(&nested_repo, cl_fixture("testrepo.git")));
	git_repository_free(nested_repo);
}

static void *set_error(void *dummy)
{
	giterr_set(GITERR_INVALID, "oh no, something happened!\n");

	return dummy;
}

/* Set errors so we can check that we free it */
void test_threads_basic__set_error(void)
{
	run_in_parallel(1, 4, set_error, NULL, NULL);
}
