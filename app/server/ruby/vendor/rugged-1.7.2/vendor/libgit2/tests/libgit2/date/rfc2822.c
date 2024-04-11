#include "clar_libgit2.h"

#include "date.h"

void test_date_rfc2822__format_rfc2822_no_offset(void)
{
	git_time t = {1397031663, 0};
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_date_rfc2822_fmt(&buf, t.time, t.offset));
	cl_assert_equal_s("Wed, 9 Apr 2014 08:21:03 +0000", buf.ptr);

	git_str_dispose(&buf);
}

void test_date_rfc2822__format_rfc2822_positive_offset(void)
{
	git_time t = {1397031663, 120};
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_date_rfc2822_fmt(&buf, t.time, t.offset));
	cl_assert_equal_s("Wed, 9 Apr 2014 10:21:03 +0200", buf.ptr);

	git_str_dispose(&buf);
}

void test_date_rfc2822__format_rfc2822_negative_offset(void)
{
	git_time t = {1397031663, -120};
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_date_rfc2822_fmt(&buf, t.time, t.offset));
	cl_assert_equal_s("Wed, 9 Apr 2014 06:21:03 -0200", buf.ptr);

	git_str_dispose(&buf);
}

