#include "clar_libgit2.h"
#include "array.h"

static int int_lookup(const void *k, const void *a)
{
	const int *one = (const int *)k;
	int *two = (int *)a;

	return *one - *two;
}

#define expect_pos(k, n, ret) \
	key = (k); \
	cl_assert_equal_i((ret), \
		git_array_search(&p, integers, int_lookup, &key)); \
	cl_assert_equal_i((n), p);

void test_core_array__bsearch2(void)
{
	git_array_t(int) integers = GIT_ARRAY_INIT;
	int *i, key;
	size_t p;

	i = git_array_alloc(integers); *i = 2;
	i = git_array_alloc(integers); *i = 3;
	i = git_array_alloc(integers); *i = 5;
	i = git_array_alloc(integers); *i = 7;
	i = git_array_alloc(integers); *i = 7;
	i = git_array_alloc(integers); *i = 8;
	i = git_array_alloc(integers); *i = 13;
	i = git_array_alloc(integers); *i = 21;
	i = git_array_alloc(integers); *i = 25;
	i = git_array_alloc(integers); *i = 42;
	i = git_array_alloc(integers); *i = 69;
	i = git_array_alloc(integers); *i = 121;
	i = git_array_alloc(integers); *i = 256;
	i = git_array_alloc(integers); *i = 512;
	i = git_array_alloc(integers); *i = 513;
	i = git_array_alloc(integers); *i = 514;
	i = git_array_alloc(integers); *i = 516;
	i = git_array_alloc(integers); *i = 516;
	i = git_array_alloc(integers); *i = 517;

	/* value to search for, expected position, return code */
	expect_pos(3, 1, GIT_OK);
	expect_pos(2, 0, GIT_OK);
	expect_pos(1, 0, GIT_ENOTFOUND);
	expect_pos(25, 8, GIT_OK);
	expect_pos(26, 9, GIT_ENOTFOUND);
	expect_pos(42, 9, GIT_OK);
	expect_pos(50, 10, GIT_ENOTFOUND);
	expect_pos(68, 10, GIT_ENOTFOUND);
	expect_pos(256, 12, GIT_OK);

	git_array_clear(integers);
}

