#include "clar_libgit2.h"
#include "clar_libgit2_alloc.h"

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

void test_str_oom__initialize(void)
{
	git_stdalloc_init_allocator(&std_alloc);
	git_stdalloc_init_allocator(&oom_alloc);

	oom_alloc.gmalloc = oom_malloc;
	oom_alloc.grealloc = oom_realloc;

	cl_git_pass(git_allocator_setup(&oom_alloc));
}

void test_str_oom__cleanup(void)
{
	cl_git_pass(git_allocator_setup(NULL));
}

void test_str_oom__grow(void)
{
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_str_grow(&buf, 42));
	cl_assert(!git_str_oom(&buf));

	cl_assert(git_str_grow(&buf, 101) == -1);
	cl_assert(git_str_oom(&buf));

	git_str_dispose(&buf);
}

void test_str_oom__grow_by(void)
{
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_str_grow_by(&buf, 42));
	cl_assert(!git_str_oom(&buf));

	cl_assert(git_str_grow_by(&buf, 101) == -1);
	cl_assert(git_str_oom(&buf));
}

void test_str_oom__allocation_failure(void)
{
	git_str buf = GIT_STR_INIT;

	cl_alloc_limit(10);

	cl_git_pass(git_str_puts(&buf, "foobar"));
	cl_git_fail(git_str_puts(&buf, "foobar"));

	cl_alloc_reset();
}
