#include "clar_libgit2.h"
#include "oid.h"

static git_oid id_sha1;
static git_oid idp_sha1;
static git_oid idm_sha1;

const char *str_oid_sha1 = "ae90f12eea699729ed24555e40b9fd669da12a12";
const char *str_oid_sha1_p = "ae90f12eea699729ed";
const char *str_oid_sha1_m = "ae90f12eea699729ed24555e40b9fd669da12a12THIS IS EXTRA TEXT THAT SHOULD GET IGNORED";

#ifdef GIT_EXPERIMENTAL_SHA256
static git_oid id_sha256;
static git_oid idp_sha256;
static git_oid idm_sha256;

const char *str_oid_sha256 = "d3e63d2f2e43d1fee23a74bf19a0ede156cba2d1bd602eba13de433cea1bb512";
const char *str_oid_sha256_p = "d3e63d2f2e43d1fee2";
const char *str_oid_sha256_m = "d3e63d2f2e43d1fee23a74bf19a0ede156cba2d1bd602eba13de433cea1bb512 GARBAGE EXTRA TEXT AT THE END";
#endif

void test_core_oid__initialize(void)
{
	cl_git_pass(git_oid__fromstr(&id_sha1, str_oid_sha1, GIT_OID_SHA1));
	cl_git_pass(git_oid__fromstrp(&idp_sha1, str_oid_sha1_p, GIT_OID_SHA1));
	cl_git_fail(git_oid__fromstrp(&idm_sha1, str_oid_sha1_m, GIT_OID_SHA1));

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_oid__fromstr(&id_sha256, str_oid_sha256, GIT_OID_SHA256));
	cl_git_pass(git_oid__fromstrp(&idp_sha256, str_oid_sha256_p, GIT_OID_SHA256));
	cl_git_fail(git_oid__fromstrp(&idm_sha256, str_oid_sha256_m, GIT_OID_SHA256));
#endif
}

void test_core_oid__streq_sha1(void)
{
	cl_assert_equal_i(0, git_oid_streq(&id_sha1, str_oid_sha1));
	cl_assert_equal_i(-1, git_oid_streq(&id_sha1, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"));

	cl_assert_equal_i(-1, git_oid_streq(&id_sha1, "deadbeef"));
	cl_assert_equal_i(-1, git_oid_streq(&id_sha1, "I'm not an oid.... :)"));

	cl_assert_equal_i(0, git_oid_streq(&idp_sha1, "ae90f12eea699729ed0000000000000000000000"));
	cl_assert_equal_i(0, git_oid_streq(&idp_sha1, "ae90f12eea699729ed"));
	cl_assert_equal_i(-1, git_oid_streq(&idp_sha1, "ae90f12eea699729ed1"));
	cl_assert_equal_i(-1, git_oid_streq(&idp_sha1, "ae90f12eea699729ec"));
	cl_assert_equal_i(-1, git_oid_streq(&idp_sha1, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"));

	cl_assert_equal_i(-1, git_oid_streq(&idp_sha1, "deadbeef"));
	cl_assert_equal_i(-1, git_oid_streq(&idp_sha1, "I'm not an oid.... :)"));
}

void test_core_oid__streq_sha256(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	cl_assert_equal_i(0, git_oid_streq(&id_sha256, str_oid_sha256));
	cl_assert_equal_i(-1, git_oid_streq(&id_sha256, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"));

	cl_assert_equal_i(-1, git_oid_streq(&id_sha256, "deadbeef"));
	cl_assert_equal_i(-1, git_oid_streq(&id_sha256, "I'm not an oid.... :)"));

	cl_assert_equal_i(0, git_oid_streq(&idp_sha256, "d3e63d2f2e43d1fee20000000000000000000000000000000000000000000000"));
	cl_assert_equal_i(0, git_oid_streq(&idp_sha256, "d3e63d2f2e43d1fee2"));
	cl_assert_equal_i(-1, git_oid_streq(&idp_sha1, "d3e63d2f2e43d1fee21"));
	cl_assert_equal_i(-1, git_oid_streq(&idp_sha1, "d3e63d2f2e43d1fee1"));
	cl_assert_equal_i(-1, git_oid_streq(&idp_sha256, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"));

	cl_assert_equal_i(-1, git_oid_streq(&idp_sha1, "deadbeef"));
	cl_assert_equal_i(-1, git_oid_streq(&idp_sha1, "I'm not an oid.... :)"));
#endif
}

void test_core_oid__strcmp_sha1(void)
{
	cl_assert_equal_i(0, git_oid_strcmp(&id_sha1, str_oid_sha1));
	cl_assert(git_oid_strcmp(&id_sha1, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef") < 0);

	cl_assert(git_oid_strcmp(&id_sha1, "deadbeef") < 0);
	cl_assert_equal_i(-1, git_oid_strcmp(&id_sha1, "I'm not an oid.... :)"));

	cl_assert_equal_i(0, git_oid_strcmp(&idp_sha1, "ae90f12eea699729ed0000000000000000000000"));
	cl_assert_equal_i(0, git_oid_strcmp(&idp_sha1, "ae90f12eea699729ed"));
	cl_assert(git_oid_strcmp(&idp_sha1, "ae90f12eea699729ed1") < 0);
	cl_assert(git_oid_strcmp(&idp_sha1, "ae90f12eea699729ec") > 0);
	cl_assert(git_oid_strcmp(&idp_sha1, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef") < 0);

	cl_assert(git_oid_strcmp(&idp_sha1, "deadbeef") < 0);
	cl_assert_equal_i(-1, git_oid_strcmp(&idp_sha1, "I'm not an oid.... :)"));
}

void test_core_oid__strcmp_sha256(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	cl_assert_equal_i(0, git_oid_strcmp(&id_sha256, str_oid_sha256));
	cl_assert(git_oid_strcmp(&id_sha256, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef") < 0);

	cl_assert(git_oid_strcmp(&id_sha256, "deadbeef") < 0);
	cl_assert_equal_i(-1, git_oid_strcmp(&id_sha256, "I'm not an oid.... :)"));

	cl_assert_equal_i(0, git_oid_strcmp(&idp_sha256, "d3e63d2f2e43d1fee20000000000000000000000"));
	cl_assert_equal_i(0, git_oid_strcmp(&idp_sha256, "d3e63d2f2e43d1fee2"));
	cl_assert(git_oid_strcmp(&idp_sha256, "d3e63d2f2e43d1fee21") < 0);
	cl_assert(git_oid_strcmp(&idp_sha256, "d3e63d2f2e43d1fee1") > 0);
	cl_assert(git_oid_strcmp(&idp_sha256, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef") < 0);

	cl_assert(git_oid_strcmp(&idp_sha256, "deadbeef") < 0);
	cl_assert_equal_i(-1, git_oid_strcmp(&idp_sha256, "I'm not an oid.... :)"));
#endif
}

void test_core_oid__ncmp_sha1(void)
{
	cl_assert(!git_oid_ncmp(&id_sha1, &idp_sha1, 0));
	cl_assert(!git_oid_ncmp(&id_sha1, &idp_sha1, 1));
	cl_assert(!git_oid_ncmp(&id_sha1, &idp_sha1, 2));
	cl_assert(!git_oid_ncmp(&id_sha1, &idp_sha1, 17));
	cl_assert(!git_oid_ncmp(&id_sha1, &idp_sha1, 18));
	cl_assert(git_oid_ncmp(&id_sha1, &idp_sha1, 19));
	cl_assert(git_oid_ncmp(&id_sha1, &idp_sha1, 40));
	cl_assert(git_oid_ncmp(&id_sha1, &idp_sha1, 41));
	cl_assert(git_oid_ncmp(&id_sha1, &idp_sha1, 42));

	cl_assert(!git_oid_ncmp(&id_sha1, &id_sha1, 0));
	cl_assert(!git_oid_ncmp(&id_sha1, &id_sha1, 1));
	cl_assert(!git_oid_ncmp(&id_sha1, &id_sha1, 39));
	cl_assert(!git_oid_ncmp(&id_sha1, &id_sha1, 40));
	cl_assert(!git_oid_ncmp(&id_sha1, &id_sha1, 41));
}

void test_core_oid__ncmp_sha256(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	cl_assert(!git_oid_ncmp(&id_sha256, &idp_sha256, 0));
	cl_assert(!git_oid_ncmp(&id_sha256, &idp_sha256, 1));
	cl_assert(!git_oid_ncmp(&id_sha256, &idp_sha256, 2));
	cl_assert(!git_oid_ncmp(&id_sha256, &idp_sha256, 17));
	cl_assert(!git_oid_ncmp(&id_sha256, &idp_sha256, 18));
	cl_assert(git_oid_ncmp(&id_sha256, &idp_sha256, 19));
	cl_assert(git_oid_ncmp(&id_sha256, &idp_sha256, 40));
	cl_assert(git_oid_ncmp(&id_sha256, &idp_sha256, 41));
	cl_assert(git_oid_ncmp(&id_sha256, &idp_sha256, 42));
	cl_assert(git_oid_ncmp(&id_sha256, &idp_sha256, 63));
	cl_assert(git_oid_ncmp(&id_sha256, &idp_sha256, 64));
	cl_assert(git_oid_ncmp(&id_sha256, &idp_sha256, 65));

	cl_assert(!git_oid_ncmp(&id_sha256, &id_sha256, 0));
	cl_assert(!git_oid_ncmp(&id_sha256, &id_sha256, 1));
	cl_assert(!git_oid_ncmp(&id_sha256, &id_sha256, 39));
	cl_assert(!git_oid_ncmp(&id_sha256, &id_sha256, 40));
	cl_assert(!git_oid_ncmp(&id_sha256, &id_sha256, 41));
	cl_assert(!git_oid_ncmp(&id_sha256, &id_sha256, 63));
	cl_assert(!git_oid_ncmp(&id_sha256, &id_sha256, 64));
	cl_assert(!git_oid_ncmp(&id_sha256, &id_sha256, 65));
#endif
}

void test_core_oid__is_hexstr(void)
{
	cl_assert(git_oid__is_hexstr("deadbeefdeadbeefdeadbeefdeadbeefdeadbeef", GIT_OID_SHA1));
	cl_assert(!git_oid__is_hexstr("deadbeefdeadbeef", GIT_OID_SHA1));
	cl_assert(!git_oid__is_hexstr("zeadbeefdeadbeefdeadbeefdeadbeefdeadbeef", GIT_OID_SHA1));
	cl_assert(!git_oid__is_hexstr("deadbeefdeadbeefdeadbeefdeadbeefdeadbeef1", GIT_OID_SHA1));
}

void test_core_oid__fmt_substr_sha1(void)
{
	char buf[GIT_OID_MAX_HEXSIZE + 1];

	memset(buf, 0, GIT_OID_MAX_HEXSIZE + 1);
	git_oid_fmt_substr(buf, &id_sha1, 0, 40);
	cl_assert_equal_s(buf, str_oid_sha1);

	memset(buf, 0, GIT_OID_MAX_HEXSIZE + 1);
	git_oid_fmt_substr(buf, &id_sha1, 0, 18);
	cl_assert_equal_s(buf, str_oid_sha1_p);

	memset(buf, 0, GIT_OID_MAX_HEXSIZE + 1);
	git_oid_fmt_substr(buf, &id_sha1, 0, 5);
	cl_assert_equal_s(buf, "ae90f");

	memset(buf, 0, GIT_OID_MAX_HEXSIZE + 1);
	git_oid_fmt_substr(buf, &id_sha1, 5, 5);
	cl_assert_equal_s(buf, "12eea");

	memset(buf, 0, GIT_OID_MAX_HEXSIZE + 1);
	git_oid_fmt_substr(buf, &id_sha1, 5, 6);
	cl_assert_equal_s(buf, "12eea6");
}

void test_core_oid__type_lookup(void)
{
	cl_assert_equal_i(GIT_OID_SHA1, git_oid_type_fromstr("sha1"));
	cl_assert_equal_i(GIT_OID_SHA1, git_oid_type_fromstrn("sha1...", 4));
	cl_assert_equal_s("sha1", git_oid_type_name(GIT_OID_SHA1));

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_assert_equal_i(GIT_OID_SHA256, git_oid_type_fromstr("sha256"));
	cl_assert_equal_i(GIT_OID_SHA256, git_oid_type_fromstrn("sha256...", 6));
	cl_assert_equal_s("sha256", git_oid_type_name(GIT_OID_SHA256));
#endif

	cl_assert_equal_i(0, git_oid_type_fromstr("sha42"));
	cl_assert_equal_i(0, git_oid_type_fromstrn("sha1", 3));
	cl_assert_equal_i(0, git_oid_type_fromstrn("sha1...", 5));
	cl_assert_equal_s("unknown", git_oid_type_name(0));
	cl_assert_equal_s("unknown", git_oid_type_name(42));
}
