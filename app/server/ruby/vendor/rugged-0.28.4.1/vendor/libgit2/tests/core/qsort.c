#include "clar_libgit2.h"

#define assert_sorted(a, cmp) \
	_assert_sorted(a, ARRAY_SIZE(a), sizeof(*a), cmp)

struct big_entries {
	char c[311];
};

static void _assert_sorted(void *els, size_t n, size_t elsize, git__sort_r_cmp cmp)
{
	int8_t *p = els;

	git__qsort_r(p, n, elsize, cmp, NULL);
	while (n-- > 1) {
		cl_assert(cmp(p, p + elsize, NULL) <= 0);
		p += elsize;
	}
}

static int cmp_big(const void *_a, const void *_b, void *payload)
{
	const struct big_entries *a = (const struct big_entries *)_a, *b = (const struct big_entries *)_b;
	GIT_UNUSED(payload);
	return (a->c[0] < b->c[0]) ? -1 : (a->c[0] > b->c[0]) ? +1 : 0;
}

static int cmp_int(const void *_a, const void *_b, void *payload)
{
	int a = *(const int *)_a, b = *(const int *)_b;
	GIT_UNUSED(payload);
	return (a < b) ? -1 : (a > b) ? +1 : 0;
}

static int cmp_str(const void *_a, const void *_b, void *payload)
{
	GIT_UNUSED(payload);
	return strcmp((const char *) _a, (const char *) _b);
}

void test_core_qsort__array_with_single_entry(void)
{
	int a[] = { 10 };
	assert_sorted(a, cmp_int);
}

void test_core_qsort__array_with_equal_entries(void)
{
	int a[] = { 4, 4, 4, 4 };
	assert_sorted(a, cmp_int);
}

void test_core_qsort__sorted_array(void)
{
	int a[] = { 1, 10 };
	assert_sorted(a, cmp_int);
}

void test_core_qsort__unsorted_array(void)
{
	int a[] = { 123, 9, 412938, 10, 234, 89 };
	assert_sorted(a, cmp_int);
}

void test_core_qsort__sorting_strings(void)
{
	char *a[] = { "foo", "bar", "baz" };
	assert_sorted(a, cmp_str);
}

void test_core_qsort__sorting_big_entries(void)
{
	struct big_entries a[5];

	memset(&a, 0, sizeof(a));

	memset(a[0].c, 'w', sizeof(a[0].c) - 1);
	memset(a[1].c, 'c', sizeof(a[1].c) - 1);
	memset(a[2].c, 'w', sizeof(a[2].c) - 1);
	memset(a[3].c, 'h', sizeof(a[3].c) - 1);
	memset(a[4].c, 'a', sizeof(a[4].c) - 1);

	assert_sorted(a, cmp_big);

	cl_assert_equal_i(strspn(a[0].c, "a"), sizeof(a[0].c) - 1);
	cl_assert_equal_i(strspn(a[1].c, "c"), sizeof(a[1].c) - 1);
	cl_assert_equal_i(strspn(a[2].c, "h"), sizeof(a[2].c) - 1);
	cl_assert_equal_i(strspn(a[3].c, "w"), sizeof(a[3].c) - 1);
	cl_assert_equal_i(strspn(a[4].c, "w"), sizeof(a[4].c) - 1);
}
