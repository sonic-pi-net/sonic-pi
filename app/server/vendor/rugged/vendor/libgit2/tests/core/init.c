#include "clar_libgit2.h"

void test_core_init__returns_count(void)
{
	/* libgit2_clar initializes us first, so we have an existing
	 * initialization.
	 */
	cl_assert_equal_i(2, git_libgit2_init());
	cl_assert_equal_i(3, git_libgit2_init());

	cl_assert_equal_i(2, git_libgit2_shutdown());
	cl_assert_equal_i(1, git_libgit2_shutdown());
}

