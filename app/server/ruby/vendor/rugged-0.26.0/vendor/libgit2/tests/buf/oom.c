#include "clar_libgit2.h"
#include "buffer.h"

#if defined(GIT_ARCH_64)
#define TOOBIG 0xffffffffffffff00
#else
#define TOOBIG 0xffffff00
#endif

/**
 * If we make a ridiculously large request the first time we
 * actually allocate some space in the git_buf, the realloc()
 * will fail.  And because the git_buf_grow() wrapper always
 * sets mark_oom, the code in git_buf_try_grow() will free
 * the internal buffer and set it to git_buf__oom.
 * 
 * We initialized the internal buffer to (the static variable)
 * git_buf__initbuf.  The purpose of this test is to make sure
 * that we don't try to free the static buffer.
 */
void test_buf_oom__grow(void)
{
	git_buf buf = GIT_BUF_INIT;

	git_buf_clear(&buf);

	cl_assert(git_buf_grow(&buf, TOOBIG) == -1);
	cl_assert(git_buf_oom(&buf));

	git_buf_free(&buf);
}

void test_buf_oom__grow_by(void)
{
	git_buf buf = GIT_BUF_INIT;

	buf.size = SIZE_MAX-10;

	cl_assert(git_buf_grow_by(&buf, 50) == -1);
	cl_assert(git_buf_oom(&buf));
}
