#include "clar_libgit2.h"
#include "pool.h"
#include "git2/oid.h"

void test_pool__0(void)
{
	int i;
	git_pool p;
	void *ptr;

	git_pool_init(&p, 1);

	for (i = 1; i < 10000; i *= 2) {
		ptr = git_pool_malloc(&p, i);
		cl_assert(ptr != NULL);
		cl_assert(git_pool__ptr_in_pool(&p, ptr));
		cl_assert(!git_pool__ptr_in_pool(&p, &i));
	}

	git_pool_clear(&p);
}

void test_pool__1(void)
{
	int i;
	git_pool p;

	git_pool_init(&p, 1);
	p.page_size = 4000;

	for (i = 2010; i > 0; i--)
		cl_assert(git_pool_malloc(&p, i) != NULL);

#ifndef GIT_DEBUG_POOL
	/* with fixed page size, allocation must end up with these values */
	cl_assert_equal_i(591, git_pool__open_pages(&p));
#endif
	git_pool_clear(&p);

	git_pool_init(&p, 1);
	p.page_size = 4120;

	for (i = 2010; i > 0; i--)
		cl_assert(git_pool_malloc(&p, i) != NULL);

#ifndef GIT_DEBUG_POOL
	/* with fixed page size, allocation must end up with these values */
	cl_assert_equal_i(sizeof(void *) == 8 ? 575 : 573, git_pool__open_pages(&p));
#endif
	git_pool_clear(&p);
}

void test_pool__strndup_limit(void)
{
	git_pool p;

	git_pool_init(&p, 1);
	/* ensure 64 bit doesn't overflow */
	cl_assert(git_pool_strndup(&p, "foo", (size_t)-1) == NULL);
	git_pool_clear(&p);
}

