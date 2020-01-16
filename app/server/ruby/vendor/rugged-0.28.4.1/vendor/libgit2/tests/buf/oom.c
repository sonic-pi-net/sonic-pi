#include "clar_libgit2.h"
#include "buffer.h"

/* Override default allocators with ones that will fail predictably. */

static git_allocator std_alloc;
static git_allocator oom_alloc;

static void *oom_malloc(size_t n, const char *file, int line)
{
	/* Reject any allocation of more than 100 bytes */
	return (n > 100) ? NULL : std_alloc.gmalloc(n, file, line);
}

static void *oom_realloc(void *p, size_t n, const char *file, int line)
{
	/* Reject any allocation of more than 100 bytes */
	return (n > 100) ? NULL : std_alloc.grealloc(p, n, file, line);
}

void test_buf_oom__initialize(void)
{
	git_stdalloc_init_allocator(&std_alloc);
	git_stdalloc_init_allocator(&oom_alloc);

	oom_alloc.gmalloc = oom_malloc;
	oom_alloc.grealloc = oom_realloc;

	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_ALLOCATOR, &oom_alloc));
}

void test_buf_oom__cleanup(void)
{
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_ALLOCATOR, NULL));
}

void test_buf_oom__grow(void)
{
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_buf_grow(&buf, 42));
	cl_assert(!git_buf_oom(&buf));

	cl_assert(git_buf_grow(&buf, 101) == -1);
	cl_assert(git_buf_oom(&buf));

	git_buf_dispose(&buf);
}

void test_buf_oom__grow_by(void)
{
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_buf_grow_by(&buf, 42));
	cl_assert(!git_buf_oom(&buf));

	cl_assert(git_buf_grow_by(&buf, 101) == -1);
	cl_assert(git_buf_oom(&buf));
}
