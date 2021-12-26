#include "clar_libgit2.h"

void test_threads_atomic__atomic32_set(void)
{
	git_atomic32 v = {0};
	git_atomic32_set(&v, 1);
	cl_assert_equal_i(v.val, 1);
}

void test_threads_atomic__atomic32_get(void)
{
	git_atomic32 v = {1};
	cl_assert_equal_i(git_atomic32_get(&v), 1);
}

void test_threads_atomic__atomic32_inc(void)
{
	git_atomic32 v = {0};
	cl_assert_equal_i(git_atomic32_inc(&v), 1);
	cl_assert_equal_i(v.val, 1);
}

void test_threads_atomic__atomic32_add(void)
{
	git_atomic32 v = {0};
	cl_assert_equal_i(git_atomic32_add(&v, 1), 1);
	cl_assert_equal_i(v.val, 1);
}

void test_threads_atomic__atomic32_dec(void)
{
	git_atomic32 v = {1};
	cl_assert_equal_i(git_atomic32_dec(&v), 0);
	cl_assert_equal_i(v.val, 0);
}

void test_threads_atomic__atomic64_set(void)
{
#ifndef GIT_ARCH_64
	cl_skip();
#else
	git_atomic64 v = {0};
	git_atomic64_set(&v, 1);
	cl_assert_equal_i(v.val, 1);
#endif
}

void test_threads_atomic__atomic64_get(void)
{
#ifndef GIT_ARCH_64
	cl_skip();
#else
	git_atomic64 v = {1};
	cl_assert_equal_i(git_atomic64_get(&v), 1);
#endif
}

void test_threads_atomic__atomic64_add(void)
{
#ifndef GIT_ARCH_64
	cl_skip();
#else
	git_atomic64 v = {0};
	cl_assert_equal_i(git_atomic64_add(&v, 1), 1);
	cl_assert_equal_i(v.val, 1);
#endif
}

void test_threads_atomic__cas_pointer(void)
{
	int *value = NULL;
	int newvalue1 = 1, newvalue2 = 2;

	/* value is updated */
	cl_assert_equal_p(git_atomic_compare_and_swap(&value, NULL, &newvalue1), NULL);
	cl_assert_equal_p(value, &newvalue1);

	/* value is not updated */
	cl_assert_equal_p(git_atomic_compare_and_swap(&value, NULL, &newvalue2), &newvalue1);
	cl_assert_equal_p(value, &newvalue1);
}

void test_threads_atomic__cas_intptr(void)
{
	intptr_t value = 0;
	intptr_t oldvalue;
	intptr_t newvalue;

	/* value is updated */
	oldvalue = 0;
	newvalue = 1;
	cl_assert_equal_i((intptr_t)git_atomic_compare_and_swap(&value, (void *)oldvalue, (void *)newvalue), 0);
	cl_assert_equal_i(value, 1);

	/* value is not updated */
	oldvalue = 0;
	newvalue = 2;
	cl_assert_equal_i((intptr_t)git_atomic_compare_and_swap(&value, (void *)oldvalue, (void *)newvalue), 1);
	cl_assert_equal_i(value, 1);
}

void test_threads_atomic__swap(void)
{
	int *value = NULL;
	int newvalue = 1;

	cl_assert_equal_p(git_atomic_swap(value, &newvalue), NULL);
	cl_assert_equal_p(value, &newvalue);

	cl_assert_equal_p(git_atomic_swap(value, NULL), &newvalue);
	cl_assert_equal_p(value, NULL);
}

void test_threads_atomic__load_ptr(void)
{
	int value = 1;
	int *ptr = &value;
	cl_assert_equal_p(git_atomic_load(ptr), &value);
}

void test_threads_atomic__load_intptr(void)
{
	intptr_t value = 1;
	cl_assert_equal_i((intptr_t)git_atomic_load(value), 1);
}
