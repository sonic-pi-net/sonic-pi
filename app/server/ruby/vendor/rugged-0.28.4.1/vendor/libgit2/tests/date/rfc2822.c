#include "clar_libgit2.h"

#include "util.h"

void test_date_rfc2822__format_rfc2822_no_offset(void)
{
	git_time t = {1397031663, 0};
	char buf[GIT_DATE_RFC2822_SZ];

	cl_git_pass(git__date_rfc2822_fmt(buf, sizeof(buf), &t));
	cl_assert(strcmp(buf, "Wed, 9 Apr 2014 08:21:03 +0000") == 0);
}

void test_date_rfc2822__format_rfc2822_positive_offset(void)
{
	git_time t = {1397031663, 120};
	char buf[GIT_DATE_RFC2822_SZ];

	cl_git_pass(git__date_rfc2822_fmt(buf, sizeof(buf), &t));
	cl_assert(strcmp(buf, "Wed, 9 Apr 2014 10:21:03 +0200") == 0);
}

void test_date_rfc2822__format_rfc2822_negative_offset(void)
{
	git_time t = {1397031663, -120};
	char buf[GIT_DATE_RFC2822_SZ];

	cl_git_pass(git__date_rfc2822_fmt(buf, sizeof(buf), &t));
	cl_assert(strcmp(buf, "Wed, 9 Apr 2014 06:21:03 -0200") == 0);
}

void test_date_rfc2822__format_rfc2822_buffer_too_small(void)
{
	/* "Wed, 10 Apr 2014 08:21:03 +0000" */
	git_time t = {1397031663 + 86400, 0};
	char buf[GIT_DATE_RFC2822_SZ-1];

	cl_git_fail(git__date_rfc2822_fmt(buf, sizeof(buf), &t));
}

