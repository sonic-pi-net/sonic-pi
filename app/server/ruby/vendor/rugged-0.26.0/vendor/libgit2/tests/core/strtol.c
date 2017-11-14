#include "clar_libgit2.h"

void test_core_strtol__int32(void)
{
	int32_t i;

	cl_git_pass(git__strtol32(&i, "123", NULL, 10));
	cl_assert(i == 123);
	cl_git_pass(git__strtol32(&i, "  +123 ", NULL, 10));
	cl_assert(i == 123);
	cl_git_pass(git__strtol32(&i, "  +2147483647 ", NULL, 10));
	cl_assert(i == 2147483647);
	cl_git_pass(git__strtol32(&i, "  -2147483648 ", NULL, 10));
	cl_assert(i == -2147483648LL);
	
	cl_git_fail(git__strtol32(&i, "  2147483657 ", NULL, 10));
	cl_git_fail(git__strtol32(&i, "  -2147483657 ", NULL, 10));
}

void test_core_strtol__int64(void)
{
	int64_t i;

	cl_git_pass(git__strtol64(&i, "123", NULL, 10));
	cl_assert(i == 123);
	cl_git_pass(git__strtol64(&i, "  +123 ", NULL, 10));
	cl_assert(i == 123);
	cl_git_pass(git__strtol64(&i, "  +2147483647 ", NULL, 10));
	cl_assert(i == 2147483647);
	cl_git_pass(git__strtol64(&i, "  -2147483648 ", NULL, 10));
	cl_assert(i == -2147483648LL);
	cl_git_pass(git__strtol64(&i, "  2147483657 ", NULL, 10));
	cl_assert(i == 2147483657LL);
	cl_git_pass(git__strtol64(&i, "  -2147483657 ", NULL, 10));
	cl_assert(i == -2147483657LL);
	cl_git_pass(git__strtol64(&i, " 9223372036854775807  ", NULL, 10));
	cl_assert(i == INT64_MAX);
	cl_git_pass(git__strtol64(&i, "   -9223372036854775808  ", NULL, 10));
	cl_assert(i == INT64_MIN);
	cl_git_pass(git__strtol64(&i, "   0x7fffffffffffffff  ", NULL, 16));
	cl_assert(i == INT64_MAX);
	cl_git_pass(git__strtol64(&i, "   -0x8000000000000000   ", NULL, 16));
	cl_assert(i == INT64_MIN);
}

