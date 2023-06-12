#include "clar_libgit2.h"

static void assert_l32_parses(const char *string, int32_t expected, int base)
{
	int32_t i;
	cl_git_pass(git__strntol32(&i, string, strlen(string), NULL, base));
	cl_assert_equal_i(i, expected);
}

static void assert_l32_fails(const char *string, int base)
{
	int32_t i;
	cl_git_fail(git__strntol32(&i, string, strlen(string), NULL, base));
}

static void assert_l64_parses(const char *string, int64_t expected, int base)
{
	int64_t i;
	cl_git_pass(git__strntol64(&i, string, strlen(string), NULL, base));
	cl_assert_equal_i(i, expected);
}

static void assert_l64_fails(const char *string, int base)
{
	int64_t i;
	cl_git_fail(git__strntol64(&i, string, strlen(string), NULL, base));
}

void test_strtol__int32(void)
{
	assert_l32_parses("123", 123, 10);
	assert_l32_parses("  +123 ", 123, 10);
	assert_l32_parses("  -123 ", -123, 10);
	assert_l32_parses("  +2147483647 ", 2147483647, 10);
	assert_l32_parses("  -2147483648 ", INT64_C(-2147483648), 10);
	assert_l32_parses("A", 10, 16);
	assert_l32_parses("1x1", 1, 10);

	assert_l32_fails("", 10);
	assert_l32_fails("a", 10);
	assert_l32_fails("x10x", 10);
	assert_l32_fails("  2147483657 ", 10);
	assert_l32_fails("  -2147483657 ", 10);
}

void test_strtol__int64(void)
{
	assert_l64_parses("123", 123, 10);
	assert_l64_parses("  +123 ", 123, 10);
	assert_l64_parses("  -123 ", -123, 10);
	assert_l64_parses("  +2147483647 ", 2147483647, 10);
	assert_l64_parses("  -2147483648 ", INT64_C(-2147483648), 10);
	assert_l64_parses("  2147483657 ", INT64_C(2147483657), 10);
	assert_l64_parses("  -2147483657 ", INT64_C(-2147483657), 10);
	assert_l64_parses(" 9223372036854775807  ", INT64_MAX, 10);
	assert_l64_parses("   -9223372036854775808  ", INT64_MIN, 10);
	assert_l64_parses("   0x7fffffffffffffff  ", INT64_MAX, 16);
	assert_l64_parses("   -0x8000000000000000   ", INT64_MIN, 16);
	assert_l64_parses("1a", 26, 16);
	assert_l64_parses("1A", 26, 16);

	assert_l64_fails("", 10);
	assert_l64_fails("a", 10);
	assert_l64_fails("x10x", 10);
	assert_l64_fails("0x8000000000000000", 16);
	assert_l64_fails("-0x8000000000000001", 16);
}

void test_strtol__base_autodetection(void)
{
	assert_l64_parses("0", 0, 0);
	assert_l64_parses("00", 0, 0);
	assert_l64_parses("0x", 0, 0);
	assert_l64_parses("0foobar", 0, 0);
	assert_l64_parses("07", 7, 0);
	assert_l64_parses("017", 15, 0);
	assert_l64_parses("0x8", 8, 0);
	assert_l64_parses("0x18", 24, 0);
}

void test_strtol__buffer_length_with_autodetection_truncates(void)
{
	int64_t i64;

	cl_git_pass(git__strntol64(&i64, "011", 2, NULL, 0));
	cl_assert_equal_i(i64, 1);
	cl_git_pass(git__strntol64(&i64, "0x11", 3, NULL, 0));
	cl_assert_equal_i(i64, 1);
}

void test_strtol__buffer_length_truncates(void)
{
	int32_t i32;
	int64_t i64;

	cl_git_pass(git__strntol32(&i32, "11", 1, NULL, 10));
	cl_assert_equal_i(i32, 1);

	cl_git_pass(git__strntol64(&i64, "11", 1, NULL, 10));
	cl_assert_equal_i(i64, 1);
}

void test_strtol__buffer_length_with_leading_ws_truncates(void)
{
	int64_t i64;

	cl_git_fail(git__strntol64(&i64, " 1", 1, NULL, 10));

	cl_git_pass(git__strntol64(&i64, " 11", 2, NULL, 10));
	cl_assert_equal_i(i64, 1);
}

void test_strtol__buffer_length_with_leading_sign_truncates(void)
{
	int64_t i64;

	cl_git_fail(git__strntol64(&i64, "-1", 1, NULL, 10));

	cl_git_pass(git__strntol64(&i64, "-11", 2, NULL, 10));
	cl_assert_equal_i(i64, -1);
}

void test_strtol__error_message_cuts_off(void)
{
	assert_l32_fails("2147483657foobar", 10);
	cl_assert(strstr(git_error_last()->message, "2147483657") != NULL);
	cl_assert(strstr(git_error_last()->message, "foobar") == NULL);
}
