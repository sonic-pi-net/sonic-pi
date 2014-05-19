#include "clar_libgit2.h"
#include "pool.h"
#include "git2/oid.h"

void test_core_pool__0(void)
{
	int i;
	git_pool p;
	void *ptr;

	cl_git_pass(git_pool_init(&p, 1, 4000));

	for (i = 1; i < 10000; i *= 2) {
		ptr = git_pool_malloc(&p, i);
		cl_assert(ptr != NULL);
		cl_assert(git_pool__ptr_in_pool(&p, ptr));
		cl_assert(!git_pool__ptr_in_pool(&p, &i));
	}

	/* 1+2+4+8+16+32+64+128+256+512+1024 -> original block */
	/* 2048 -> 1 block */
	/* 4096 -> 1 block */
	/* 8192 -> 1 block */

	cl_assert(git_pool__open_pages(&p) + git_pool__full_pages(&p) == 4);

	git_pool_clear(&p);
}

void test_core_pool__1(void)
{
	int i;
	git_pool p;

	cl_git_pass(git_pool_init(&p, 1, 4000));

	for (i = 2010; i > 0; i--)
		cl_assert(git_pool_malloc(&p, i) != NULL);

	/* with fixed page size, allocation must end up with these values */
	cl_assert(git_pool__open_pages(&p) == 1);
	cl_assert(git_pool__full_pages(&p) == 505);

	git_pool_clear(&p);

	cl_git_pass(git_pool_init(&p, 1, 4100));

	for (i = 2010; i > 0; i--)
		cl_assert(git_pool_malloc(&p, i) != NULL);

	/* with fixed page size, allocation must end up with these values */
	cl_assert(git_pool__open_pages(&p) == 1);
	cl_assert(git_pool__full_pages(&p) == 492);

	git_pool_clear(&p);
}

static char to_hex[] = "0123456789abcdef";

void test_core_pool__2(void)
{
	git_pool p;
	char oid_hex[GIT_OID_HEXSZ];
	git_oid *oid;
	int i, j;

	memset(oid_hex, '0', sizeof(oid_hex));

	cl_git_pass(git_pool_init(&p, sizeof(git_oid), 100));

	for (i = 1000; i < 10000; i++) {
		oid = git_pool_malloc(&p, 1);
		cl_assert(oid != NULL);

		for (j = 0; j < 8; j++)
			oid_hex[j] = to_hex[(i >> (4 * j)) & 0x0f];
		cl_git_pass(git_oid_fromstr(oid, oid_hex));
	}

	/* with fixed page size, allocation must end up with these values */
	cl_assert(git_pool__open_pages(&p) == 0);
	cl_assert(git_pool__full_pages(&p) == 90);

	git_pool_clear(&p);
}

void test_core_pool__free_list(void)
{
	int i;
	git_pool p;
	void *ptr, *ptrs[50];

	cl_git_pass(git_pool_init(&p, 100, 100));

	for (i = 0; i < 10; ++i) {
		ptr = git_pool_malloc(&p, 1);
		cl_assert(ptr != NULL);
	}
	cl_assert_equal_i(10, (int)p.items);

	for (i = 0; i < 50; ++i) {
		ptrs[i] = git_pool_malloc(&p, 1);
		cl_assert(ptrs[i] != NULL);
	}
	cl_assert_equal_i(60, (int)p.items);

	git_pool_free(&p, ptr);
	cl_assert_equal_i(60, (int)p.items);

	git_pool_free_array(&p, 50, ptrs);
	cl_assert_equal_i(60, (int)p.items);

	for (i = 0; i < 50; ++i) {
		ptrs[i] = git_pool_malloc(&p, 1);
		cl_assert(ptrs[i] != NULL);
	}
	cl_assert_equal_i(60, (int)p.items);

	for (i = 0; i < 111; ++i) {
		ptr = git_pool_malloc(&p, 1);
		cl_assert(ptr != NULL);
	}
	cl_assert_equal_i(170, (int)p.items);

	git_pool_free_array(&p, 50, ptrs);
	cl_assert_equal_i(170, (int)p.items);

	for (i = 0; i < 50; ++i) {
		ptrs[i] = git_pool_malloc(&p, 1);
		cl_assert(ptrs[i] != NULL);
	}
	cl_assert_equal_i(170, (int)p.items);

	git_pool_clear(&p);
}

void test_core_pool__strndup_limit(void)
{
	git_pool p;

	cl_git_pass(git_pool_init(&p, 1, 100));
	/* ensure 64 bit doesn't overflow */
	cl_assert(git_pool_strndup(&p, "foo", (size_t)-1) == NULL);
	git_pool_clear(&p);
}

