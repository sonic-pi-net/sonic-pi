#include "clar_libgit2.h"
#include "clar_libgit2_alloc.h"
#include "alloc.h"

void test_alloc__cleanup(void)
{
	cl_alloc_reset();
}

void test_alloc__oom(void)
{
	void *ptr = NULL;

	cl_alloc_limit(0);

	cl_assert(git__malloc(1) == NULL);
	cl_assert(git__calloc(1, 1) == NULL);
	cl_assert(git__realloc(ptr, 1) == NULL);
	cl_assert(git__strdup("test") == NULL);
	cl_assert(git__strndup("test", 4) == NULL);
}

void test_alloc__single_byte_is_exhausted(void)
{
	void *ptr;

	cl_alloc_limit(1);

	cl_assert(ptr = git__malloc(1));
	cl_assert(git__malloc(1) == NULL);
	git__free(ptr);
}

void test_alloc__free_replenishes_byte(void)
{
	void *ptr;

	cl_alloc_limit(1);

	cl_assert(ptr = git__malloc(1));
	cl_assert(git__malloc(1) == NULL);
	git__free(ptr);
	cl_assert(ptr = git__malloc(1));
	git__free(ptr);
}

void test_alloc__realloc(void)
{
	char *ptr = NULL;

	cl_alloc_limit(3);

	cl_assert(ptr = git__realloc(ptr, 1));
	*ptr = 'x';

	cl_assert(ptr = git__realloc(ptr, 1));
	cl_assert_equal_i(*ptr, 'x');

	cl_assert(ptr = git__realloc(ptr, 2));
	cl_assert_equal_i(*ptr, 'x');

	cl_assert(git__realloc(ptr, 2) == NULL);

	cl_assert(ptr = git__realloc(ptr, 1));
	cl_assert_equal_i(*ptr, 'x');

	git__free(ptr);
}
