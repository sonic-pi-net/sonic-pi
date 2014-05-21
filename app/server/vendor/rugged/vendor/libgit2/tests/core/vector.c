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
	// make initial capacity exact for our insertions.
	git_vector_init(&x, 3, NULL);
	git_vector_insert(&x, (void*) 0xabc);
	git_vector_insert(&x, (void*) 0xdef);
	git_vector_insert(&x, (void*) 0x123);

	git_vector_remove(&x, 0); // used to read past array bounds.
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
	return (int)((long)a - (long)b);
}

/* insert_sorted */
void test_core_vector__3(void)
{
	git_vector x;
	long i;
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
	long i;
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
