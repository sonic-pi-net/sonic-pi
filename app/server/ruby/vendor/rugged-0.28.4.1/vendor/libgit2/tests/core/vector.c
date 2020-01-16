#include <stdint.h>

#include "clar_libgit2.h"
#include "vector.h"

/* initial size of 1 would cause writing past array bounds */
void test_core_vector__0(void)
{
	git_vector x;
	int i;
	git_vector_init(&x, 1, NULL);
	for (i = 0; i < 10; ++i) {
		git_vector_insert(&x, (void*) 0xabc);
	}
	git_vector_free(&x);
}


/* don't read past array bounds on remove() */
void test_core_vector__1(void)
{
	git_vector x;
	/* make initial capacity exact for our insertions. */
	git_vector_init(&x, 3, NULL);
	git_vector_insert(&x, (void*) 0xabc);
	git_vector_insert(&x, (void*) 0xdef);
	git_vector_insert(&x, (void*) 0x123);

	git_vector_remove(&x, 0); /* used to read past array bounds. */
	git_vector_free(&x);
}


static int test_cmp(const void *a, const void *b)
{
	return *(const int *)a - *(const int *)b;
}

/* remove duplicates */
void test_core_vector__2(void)
{
	git_vector x;
	int *ptrs[2];

	ptrs[0] = git__malloc(sizeof(int));
	ptrs[1] = git__malloc(sizeof(int));

	*ptrs[0] = 2;
	*ptrs[1] = 1;

	cl_git_pass(git_vector_init(&x, 5, test_cmp));
	cl_git_pass(git_vector_insert(&x, ptrs[0]));
	cl_git_pass(git_vector_insert(&x, ptrs[1]));
	cl_git_pass(git_vector_insert(&x, ptrs[1]));
	cl_git_pass(git_vector_insert(&x, ptrs[0]));
	cl_git_pass(git_vector_insert(&x, ptrs[1]));
	cl_assert(x.length == 5);

	git_vector_uniq(&x, NULL);
	cl_assert(x.length == 2);

	git_vector_free(&x);

	git__free(ptrs[0]);
	git__free(ptrs[1]);
}


static int compare_them(const void *a, const void *b)
{
	return (int)((intptr_t)a - (intptr_t)b);
}

/* insert_sorted */
void test_core_vector__3(void)
{
	git_vector x;
	intptr_t i;
	git_vector_init(&x, 1, &compare_them);

	for (i = 0; i < 10; i += 2) {
		git_vector_insert_sorted(&x, (void*)(i + 1), NULL);
	}

	for (i = 9; i > 0; i -= 2) {
		git_vector_insert_sorted(&x, (void*)(i + 1), NULL);
	}

	cl_assert(x.length == 10);
	for (i = 0; i < 10; ++i) {
		cl_assert(git_vector_get(&x, i) == (void*)(i + 1));
	}

	git_vector_free(&x);
}

/* insert_sorted with duplicates */
void test_core_vector__4(void)
{
	git_vector x;
	intptr_t i;
	git_vector_init(&x, 1, &compare_them);

	for (i = 0; i < 10; i += 2) {
		git_vector_insert_sorted(&x, (void*)(i + 1), NULL);
	}

	for (i = 9; i > 0; i -= 2) {
		git_vector_insert_sorted(&x, (void*)(i + 1), NULL);
	}

	for (i = 0; i < 10; i += 2) {
		git_vector_insert_sorted(&x, (void*)(i + 1), NULL);
	}

	for (i = 9; i > 0; i -= 2) {
		git_vector_insert_sorted(&x, (void*)(i + 1), NULL);
	}

	cl_assert(x.length == 20);
	for (i = 0; i < 20; ++i) {
		cl_assert(git_vector_get(&x, i) == (void*)(i / 2 + 1));
	}

	git_vector_free(&x);
}

typedef struct {
	int content;
	int count;
} my_struct;

static int _struct_count = 0;

static int compare_structs(const void *a, const void *b)
{
	return ((const my_struct *)a)->content -
		((const my_struct *)b)->content;
}

static int merge_structs(void **old_raw, void *new)
{
	my_struct *old = *(my_struct **)old_raw;
	cl_assert(((my_struct *)old)->content == ((my_struct *)new)->content);
	((my_struct *)old)->count += 1;
	git__free(new);
	_struct_count--;
	return GIT_EEXISTS;
}

static my_struct *alloc_struct(int value)
{
	my_struct *st = git__malloc(sizeof(my_struct));
	st->content = value;
	st->count = 0;
	_struct_count++;
	return st;
}

/* insert_sorted with duplicates and special handling */
void test_core_vector__5(void)
{
	git_vector x;
	int i;

	git_vector_init(&x, 1, &compare_structs);

	for (i = 0; i < 10; i += 2)
		git_vector_insert_sorted(&x, alloc_struct(i), &merge_structs);

	for (i = 9; i > 0; i -= 2)
		git_vector_insert_sorted(&x, alloc_struct(i), &merge_structs);

	cl_assert(x.length == 10);
	cl_assert(_struct_count == 10);

	for (i = 0; i < 10; i += 2)
		git_vector_insert_sorted(&x, alloc_struct(i), &merge_structs);

	for (i = 9; i > 0; i -= 2)
		git_vector_insert_sorted(&x, alloc_struct(i), &merge_structs);

	cl_assert(x.length == 10);
	cl_assert(_struct_count == 10);

	for (i = 0; i < 10; ++i) {
		cl_assert(((my_struct *)git_vector_get(&x, i))->content == i);
		git__free(git_vector_get(&x, i));
		_struct_count--;
	}

	git_vector_free(&x);
}

static int remove_ones(const git_vector *v, size_t idx, void *p)
{
	GIT_UNUSED(p);
	return (git_vector_get(v, idx) == (void *)0x001);
}

/* Test removal based on callback */
void test_core_vector__remove_matching(void)
{
	git_vector x;
	size_t i;
	void *compare;

	git_vector_init(&x, 1, NULL);
	git_vector_insert(&x, (void*) 0x001);

	cl_assert(x.length == 1);
	git_vector_remove_matching(&x, remove_ones, NULL);
	cl_assert(x.length == 0);

	git_vector_insert(&x, (void*) 0x001);
	git_vector_insert(&x, (void*) 0x001);
	git_vector_insert(&x, (void*) 0x001);

	cl_assert(x.length == 3);
	git_vector_remove_matching(&x, remove_ones, NULL);
	cl_assert(x.length == 0);

	git_vector_insert(&x, (void*) 0x002);
	git_vector_insert(&x, (void*) 0x001);
	git_vector_insert(&x, (void*) 0x002);
	git_vector_insert(&x, (void*) 0x001);

	cl_assert(x.length == 4);
	git_vector_remove_matching(&x, remove_ones, NULL);
	cl_assert(x.length == 2);

	git_vector_foreach(&x, i, compare) {
		cl_assert(compare != (void *)0x001);
	}

	git_vector_clear(&x);

	git_vector_insert(&x, (void*) 0x001);
	git_vector_insert(&x, (void*) 0x002);
	git_vector_insert(&x, (void*) 0x002);
	git_vector_insert(&x, (void*) 0x001);

	cl_assert(x.length == 4);
	git_vector_remove_matching(&x, remove_ones, NULL);
	cl_assert(x.length == 2);

	git_vector_foreach(&x, i, compare) {
		cl_assert(compare != (void *)0x001);
	}

	git_vector_clear(&x);

	git_vector_insert(&x, (void*) 0x002);
	git_vector_insert(&x, (void*) 0x001);
	git_vector_insert(&x, (void*) 0x002);
	git_vector_insert(&x, (void*) 0x001);

	cl_assert(x.length == 4);
	git_vector_remove_matching(&x, remove_ones, NULL);
	cl_assert(x.length == 2);

	git_vector_foreach(&x, i, compare) {
		cl_assert(compare != (void *)0x001);
	}

	git_vector_clear(&x);

	git_vector_insert(&x, (void*) 0x002);
	git_vector_insert(&x, (void*) 0x003);
	git_vector_insert(&x, (void*) 0x002);
	git_vector_insert(&x, (void*) 0x003);

	cl_assert(x.length == 4);
	git_vector_remove_matching(&x, remove_ones, NULL);
	cl_assert(x.length == 4);

	git_vector_free(&x);
}

static void assert_vector(git_vector *x, void *expected[], size_t len)
{
	size_t i;

	cl_assert_equal_i(len, x->length);

	for (i = 0; i < len; i++)
		cl_assert(expected[i] == x->contents[i]);
}

void test_core_vector__grow_and_shrink(void)
{
	git_vector x = GIT_VECTOR_INIT;
	void *expected1[] = {
		(void *)0x02, (void *)0x03, (void *)0x04, (void *)0x05,
		(void *)0x06, (void *)0x07, (void *)0x08, (void *)0x09,
		(void *)0x0a
	};
	void *expected2[] = {
		(void *)0x02, (void *)0x04, (void *)0x05, (void *)0x06,
		(void *)0x07, (void *)0x08, (void *)0x09, (void *)0x0a
	};
	void *expected3[] = {
		(void *)0x02, (void *)0x04, (void *)0x05, (void *)0x06,
		(void *)0x0a
	};
	void *expected4[] = {
		(void *)0x02, (void *)0x04, (void *)0x05
	};
	void *expected5[] = {
		(void *)0x00, (void *)0x00, (void *)0x02, (void *)0x04,
		(void *)0x05
	};
	void *expected6[] = {
		(void *)0x00, (void *)0x00, (void *)0x02, (void *)0x04,
		(void *)0x05, (void *)0x00
	};
	void *expected7[] = {
		(void *)0x00, (void *)0x00, (void *)0x02, (void *)0x04,
		(void *)0x00, (void *)0x00, (void *)0x00, (void *)0x05,
		(void *)0x00
	};
	void *expected8[] = {
		(void *)0x04, (void *)0x00, (void *)0x00, (void *)0x00,
		(void *)0x05, (void *)0x00
	};
	void *expected9[] = {
		(void *)0x04, (void *)0x00, (void *)0x05, (void *)0x00
	};
	void *expectedA[] = { (void *)0x04, (void *)0x00 };
	void *expectedB[] = { (void *)0x04 };

	git_vector_insert(&x, (void *)0x01);
	git_vector_insert(&x, (void *)0x02);
	git_vector_insert(&x, (void *)0x03);
	git_vector_insert(&x, (void *)0x04);
	git_vector_insert(&x, (void *)0x05);
	git_vector_insert(&x, (void *)0x06);
	git_vector_insert(&x, (void *)0x07);
	git_vector_insert(&x, (void *)0x08);
	git_vector_insert(&x, (void *)0x09);
	git_vector_insert(&x, (void *)0x0a);

	git_vector_remove_range(&x, 0, 1);
	assert_vector(&x, expected1, ARRAY_SIZE(expected1));

	git_vector_remove_range(&x, 1, 1);
	assert_vector(&x, expected2, ARRAY_SIZE(expected2));

	git_vector_remove_range(&x, 4, 3);
	assert_vector(&x, expected3, ARRAY_SIZE(expected3));

	git_vector_remove_range(&x, 3, 2);
	assert_vector(&x, expected4, ARRAY_SIZE(expected4));

	git_vector_insert_null(&x, 0, 2);
	assert_vector(&x, expected5, ARRAY_SIZE(expected5));

	git_vector_insert_null(&x, 5, 1);
	assert_vector(&x, expected6, ARRAY_SIZE(expected6));

	git_vector_insert_null(&x, 4, 3);
	assert_vector(&x, expected7, ARRAY_SIZE(expected7));

	git_vector_remove_range(&x, 0, 3);
	assert_vector(&x, expected8, ARRAY_SIZE(expected8));

	git_vector_remove_range(&x, 1, 2);
	assert_vector(&x, expected9, ARRAY_SIZE(expected9));

	git_vector_remove_range(&x, 2, 2);
	assert_vector(&x, expectedA, ARRAY_SIZE(expectedA));

	git_vector_remove_range(&x, 1, 1);
	assert_vector(&x, expectedB, ARRAY_SIZE(expectedB));

	git_vector_remove_range(&x, 0, 1);
	assert_vector(&x, NULL, 0);

	git_vector_free(&x);
}

void test_core_vector__reverse(void)
{
	git_vector v = GIT_VECTOR_INIT;
	size_t i;

	void *in1[] = {(void *) 0x0, (void *) 0x1, (void *) 0x2, (void *) 0x3};
	void *out1[] = {(void *) 0x3, (void *) 0x2, (void *) 0x1, (void *) 0x0};

	void *in2[] = {(void *) 0x0, (void *) 0x1, (void *) 0x2, (void *) 0x3, (void *) 0x4};
	void *out2[] = {(void *) 0x4, (void *) 0x3, (void *) 0x2, (void *) 0x1, (void *) 0x0};

	for (i = 0; i < 4; i++)
		cl_git_pass(git_vector_insert(&v, in1[i]));

	git_vector_reverse(&v);

	for (i = 0; i < 4; i++)
		cl_assert_equal_p(out1[i], git_vector_get(&v, i));

	git_vector_clear(&v);
	for (i = 0; i < 5; i++)
		cl_git_pass(git_vector_insert(&v, in2[i]));

	git_vector_reverse(&v);

	for (i = 0; i < 5; i++)
		cl_assert_equal_p(out2[i], git_vector_get(&v, i));

	git_vector_free(&v);
}

void test_core_vector__dup_empty_vector(void)
{
	git_vector v = GIT_VECTOR_INIT;
	git_vector dup = GIT_VECTOR_INIT;
	int dummy;

	cl_assert_equal_i(0, v.length);

	cl_git_pass(git_vector_dup(&dup, &v, v._cmp));
	cl_assert_equal_i(0, dup._alloc_size);
	cl_assert_equal_i(0, dup.length);

	cl_git_pass(git_vector_insert(&dup, &dummy));
	cl_assert_equal_i(8, dup._alloc_size);
	cl_assert_equal_i(1, dup.length);

	git_vector_free(&dup);
}
