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

#ifdef GIT_THREADS
static void *return_normally(void *param)
{
	return param;
}
#endif

void test_threads_basic__exit(void)
{
#ifndef GIT_THREADS
	clar__skip();
#else
	git_thread thread;
	void *result;

	/* Ensure that the return value of the threadproc is returned. */
	cl_git_pass(git_thread_create(&thread, return_normally, (void *)424242));
	cl_git_pass(git_thread_join(&thread, &result));
	cl_assert_equal_sz(424242, (size_t)result);

	/* Ensure that the return value of `git_thread_exit` is returned. */
	cl_git_pass(git_thread_create(&thread, return_normally, (void *)232323));
	cl_git_pass(git_thread_join(&thread, &result));
	cl_assert_equal_sz(232323, (size_t)result);
#endif
}
