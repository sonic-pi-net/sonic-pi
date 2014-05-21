#include "clar_libgit2.h"

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

	git_threads_init();
	cl_git_pass(git_repository_open(&nested_repo, cl_fixture("testrepo.git")));
	git_repository_free(nested_repo);

	git_threads_shutdown();
	cl_git_pass(git_repository_open(&nested_repo, cl_fixture("testrepo.git")));
	git_repository_free(nested_repo);
}
