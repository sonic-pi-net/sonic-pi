
#include "clar_libgit2.h"

#include "odb.h"

void test_object_raw_convert__succeed_on_oid_to_string_conversion(void)
{
	const char *exp = "16a0123456789abcdef4b775213c23a8bd74f5e0";
	git_oid in;
	char out[GIT_OID_SHA1_HEXSIZE + 1];
	char *str;
	int i;

	cl_git_pass(git_oid__fromstr(&in, exp, GIT_OID_SHA1));

	/* NULL buffer pointer, returns static empty string */
	str = git_oid_tostr(NULL, sizeof(out), &in);
	cl_assert(str && *str == '\0' && str != out);

	/* zero buffer size, returns static empty string */
	str = git_oid_tostr(out, 0, &in);
	cl_assert(str && *str == '\0' && str != out);

	/* NULL oid pointer, sets existing buffer to empty string */
	str = git_oid_tostr(out, sizeof(out), NULL);
	cl_assert(str && *str == '\0' && str == out);

	/* n == 1, returns out as an empty string */
	str = git_oid_tostr(out, 1, &in);
	cl_assert(str && *str == '\0' && str == out);

	for (i = 1; i < GIT_OID_SHA1_HEXSIZE; i++) {
		out[i+1] = 'Z';
		str = git_oid_tostr(out, i+1, &in);
		/* returns out containing c-string */
		cl_assert(str && str == out);
		/* must be '\0' terminated */
		cl_assert(*(str+i) == '\0');
		/* must not touch bytes past end of string */
		cl_assert(*(str+(i+1)) == 'Z');
		/* i == n-1 characters of string */
		cl_git_pass(strncmp(exp, out, i));
	}

	/* returns out as hex formatted c-string */
	str = git_oid_tostr(out, sizeof(out), &in);
	cl_assert(str && str == out && *(str+GIT_OID_SHA1_HEXSIZE) == '\0');
	cl_assert_equal_s(exp, out);
}

void test_object_raw_convert__succeed_on_oid_to_string_conversion_big(void)
{
	const char *exp = "16a0123456789abcdef4b775213c23a8bd74f5e0";
	git_oid in;
	char big[GIT_OID_SHA1_HEXSIZE + 1 + 3]; /* note + 4 => big buffer */
	char *str;

	cl_git_pass(git_oid__fromstr(&in, exp, GIT_OID_SHA1));

	/* place some tail material */
	big[GIT_OID_SHA1_HEXSIZE+0] = 'W'; /* should be '\0' afterwards */
	big[GIT_OID_SHA1_HEXSIZE+1] = 'X'; /* should remain untouched   */
	big[GIT_OID_SHA1_HEXSIZE+2] = 'Y'; /* ditto */
	big[GIT_OID_SHA1_HEXSIZE+3] = 'Z'; /* ditto */

	/* returns big as hex formatted c-string */
	str = git_oid_tostr(big, sizeof(big), &in);
	cl_assert(str && str == big && *(str+GIT_OID_SHA1_HEXSIZE) == '\0');
	cl_assert_equal_s(exp, big);

	/* check tail material is untouched */
	cl_assert(str && str == big && *(str+GIT_OID_SHA1_HEXSIZE+1) == 'X');
	cl_assert(str && str == big && *(str+GIT_OID_SHA1_HEXSIZE+2) == 'Y');
	cl_assert(str && str == big && *(str+GIT_OID_SHA1_HEXSIZE+3) == 'Z');
}

static void check_partial_oid(
	char *buffer, size_t count, const git_oid *oid, const char *expected)
{
	git_oid_nfmt(buffer, count, oid);
	buffer[count] = '\0';
	cl_assert_equal_s(expected, buffer);
}

void test_object_raw_convert__convert_oid_partially(void)
{
	const char *exp = "16a0123456789abcdef4b775213c23a8bd74f5e0";
	git_oid in;
	char big[GIT_OID_SHA1_HEXSIZE + 1 + 3]; /* note + 4 => big buffer */

	cl_git_pass(git_oid__fromstr(&in, exp, GIT_OID_SHA1));

	git_oid_nfmt(big, sizeof(big), &in);
	cl_assert_equal_s(exp, big);

	git_oid_nfmt(big, GIT_OID_SHA1_HEXSIZE + 1, &in);
	cl_assert_equal_s(exp, big);

	check_partial_oid(big, 1, &in, "1");
	check_partial_oid(big, 2, &in, "16");
	check_partial_oid(big, 3, &in, "16a");
	check_partial_oid(big, 4, &in, "16a0");
	check_partial_oid(big, 5, &in, "16a01");

	check_partial_oid(big, GIT_OID_SHA1_HEXSIZE, &in, exp);
	check_partial_oid(
		big, GIT_OID_SHA1_HEXSIZE - 1, &in, "16a0123456789abcdef4b775213c23a8bd74f5e");
	check_partial_oid(
		big, GIT_OID_SHA1_HEXSIZE - 2, &in, "16a0123456789abcdef4b775213c23a8bd74f5");
	check_partial_oid(
		big, GIT_OID_SHA1_HEXSIZE - 3, &in, "16a0123456789abcdef4b775213c23a8bd74f");
}
