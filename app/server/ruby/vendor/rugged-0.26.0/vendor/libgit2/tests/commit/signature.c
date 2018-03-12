#include "clar_libgit2.h"

static int try_build_signature(const char *name, const char *email, git_time_t time, int offset)
{
	git_signature *sign;
	int error = 0;

	if ((error =  git_signature_new(&sign, name, email, time, offset)) < 0)
		return error;

	git_signature_free((git_signature *)sign);

	return error;
}

static void assert_name_and_email(
	const char *expected_name,
	const char *expected_email,
	const char *name,
	const char *email)
{
	git_signature *sign;

	cl_git_pass(git_signature_new(&sign, name, email, 1234567890, 60));
	cl_assert_equal_s(expected_name, sign->name);
	cl_assert_equal_s(expected_email, sign->email);

	git_signature_free(sign);
}

void test_commit_signature__leading_and_trailing_spaces_are_trimmed(void)
{
	assert_name_and_email("nulltoken", "emeric.fermas@gmail.com", "  nulltoken ", "   emeric.fermas@gmail.com     ");
	assert_name_and_email("nulltoken", "emeric.fermas@gmail.com", "  nulltoken ", "   emeric.fermas@gmail.com  \n");
	assert_name_and_email("nulltoken", "emeric.fermas@gmail.com", " \t nulltoken \n", " \n  emeric.fermas@gmail.com  \n");
}

void test_commit_signature__leading_and_trailing_crud_is_trimmed(void)
{
	assert_name_and_email("nulltoken", "emeric.fermas@gmail.com", "\"nulltoken\"", "\"emeric.fermas@gmail.com\"");
	assert_name_and_email("nulltoken w", "emeric.fermas@gmail.com", "nulltoken w.", "emeric.fermas@gmail.com");
	assert_name_and_email("nulltoken \xe2\x98\xba", "emeric.fermas@gmail.com", "nulltoken \xe2\x98\xba", "emeric.fermas@gmail.com");
}

void test_commit_signature__angle_brackets_in_names_are_not_supported(void)
{
	cl_git_fail(try_build_signature("<Phil Haack", "phil@haack", 1234567890, 60));
	cl_git_fail(try_build_signature("Phil>Haack", "phil@haack", 1234567890, 60));
	cl_git_fail(try_build_signature("<Phil Haack>", "phil@haack", 1234567890, 60));
}

void test_commit_signature__angle_brackets_in_email_are_not_supported(void)
{
	cl_git_fail(try_build_signature("Phil Haack", ">phil@haack", 1234567890, 60));
	cl_git_fail(try_build_signature("Phil Haack", "phil@>haack", 1234567890, 60));
	cl_git_fail(try_build_signature("Phil Haack", "<phil@haack>", 1234567890, 60));
}

void test_commit_signature__create_empties(void)
{
   // can not create a signature with empty name or email
	cl_git_pass(try_build_signature("nulltoken", "emeric.fermas@gmail.com", 1234567890, 60));

	cl_git_fail(try_build_signature("", "emeric.fermas@gmail.com", 1234567890, 60));
	cl_git_fail(try_build_signature("   ", "emeric.fermas@gmail.com", 1234567890, 60));
	cl_git_fail(try_build_signature("nulltoken", "", 1234567890, 60));
	cl_git_fail(try_build_signature("nulltoken", "  ", 1234567890, 60));
}

void test_commit_signature__create_one_char(void)
{
   // creating a one character signature
	assert_name_and_email("x", "foo@bar.baz", "x", "foo@bar.baz");
}

void test_commit_signature__create_two_char(void)
{
   // creating a two character signature
	assert_name_and_email("xx", "foo@bar.baz", "xx", "foo@bar.baz");
}

void test_commit_signature__create_zero_char(void)
{
   // creating a zero character signature
	git_signature *sign;
	cl_git_fail(git_signature_new(&sign, "", "x@y.z", 1234567890, 60));
	cl_assert(sign == NULL);
}

void test_commit_signature__from_buf(void)
{
	git_signature *sign;

	cl_git_pass(git_signature_from_buffer(&sign, "Test User <test@test.tt> 1461698487 +0200"));
	cl_assert_equal_s("Test User", sign->name);
	cl_assert_equal_s("test@test.tt", sign->email);
	cl_assert_equal_i(1461698487, sign->when.time);
	cl_assert_equal_i(120, sign->when.offset);
	git_signature_free(sign);
}

