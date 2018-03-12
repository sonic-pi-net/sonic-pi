#include "clar_libgit2.h"

void test_core_features__0(void)
{
	int major, minor, rev, caps;

	git_libgit2_version(&major, &minor, &rev);
	cl_assert_equal_i(LIBGIT2_VER_MAJOR, major);
	cl_assert_equal_i(LIBGIT2_VER_MINOR, minor);
	cl_assert_equal_i(LIBGIT2_VER_REVISION, rev);

	caps = git_libgit2_features();

#ifdef GIT_THREADS
	cl_assert((caps & GIT_FEATURE_THREADS) != 0);
#else
	cl_assert((caps & GIT_FEATURE_THREADS) == 0);
#endif

#ifdef GIT_HTTPS
	cl_assert((caps & GIT_FEATURE_HTTPS) != 0);
#endif

#if defined(GIT_SSH)
	cl_assert((caps & GIT_FEATURE_SSH) != 0);
#else
	cl_assert((caps & GIT_FEATURE_SSH) == 0);
#endif

#if defined(GIT_USE_NSEC)
	cl_assert((caps & GIT_FEATURE_NSEC) != 0);
#else
	cl_assert((caps & GIT_FEATURE_NSEC) == 0);
#endif
}
