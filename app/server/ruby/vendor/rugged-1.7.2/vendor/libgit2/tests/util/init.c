#include "clar_libgit2.h"

void test_init__returns_count(void)
{
	/* libgit2_tests initializes us first, so we have an existing
	 * initialization.
	 */
	cl_assert_equal_i(2, git_libgit2_init());
	cl_assert_equal_i(3, git_libgit2_init());

	cl_assert_equal_i(2, git_libgit2_shutdown());
	cl_assert_equal_i(1, git_libgit2_shutdown());
}

void test_init__reinit_succeeds(void)
{
	cl_assert_equal_i(0, git_libgit2_shutdown());
	cl_assert_equal_i(1, git_libgit2_init());
	cl_sandbox_set_search_path_defaults();
}

#ifdef GIT_THREADS
static void *reinit(void *unused)
{
	unsigned i;

	for (i = 0; i < 20; i++) {
		cl_assert(git_libgit2_init() > 0);
		cl_assert(git_libgit2_shutdown() >= 0);
	}

	return unused;
}
#endif

void test_init__concurrent_init_succeeds(void)
{
#ifdef GIT_THREADS
	git_thread threads[10];
	unsigned i;

	cl_assert_equal_i(2, git_libgit2_init());

	for (i = 0; i < ARRAY_SIZE(threads); i++)
		git_thread_create(&threads[i], reinit, NULL);
	for (i = 0; i < ARRAY_SIZE(threads); i++)
		git_thread_join(&threads[i], NULL);

	cl_assert_equal_i(1, git_libgit2_shutdown());
	cl_sandbox_set_search_path_defaults();
#else
	cl_skip();
#endif
}
