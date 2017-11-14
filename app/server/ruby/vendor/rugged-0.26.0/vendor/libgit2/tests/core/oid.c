#include "clar_libgit2.h"

static git_oid id;
static git_oid idp;
static git_oid idm;
const char *str_oid = "ae90f12eea699729ed24555e40b9fd669da12a12";
const char *str_oid_p = "ae90f12eea699729ed";
const char *str_oid_m = "ae90f12eea699729ed24555e40b9fd669da12a12THIS IS EXTRA TEXT THAT SHOULD GET IGNORED";

void test_core_oid__initialize(void)
{
	cl_git_pass(git_oid_fromstr(&id, str_oid));
	cl_git_pass(git_oid_fromstrp(&idp, str_oid_p));
	cl_git_fail(git_oid_fromstrp(&idm, str_oid_m));
}

void test_core_oid__streq(void)
{
	cl_assert_equal_i(0, git_oid_streq(&id, str_oid));
	cl_assert_equal_i(-1, git_oid_streq(&id, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"));

	cl_assert_equal_i(-1, git_oid_streq(&id, "deadbeef"));
	cl_assert_equal_i(-1, git_oid_streq(&id, "I'm not an oid.... :)"));

	cl_assert_equal_i(0, git_oid_streq(&idp, "ae90f12eea699729ed0000000000000000000000"));
	cl_assert_equal_i(0, git_oid_streq(&idp, "ae90f12eea699729ed"));
	cl_assert_equal_i(-1, git_oid_streq(&idp, "ae90f12eea699729ed1"));
	cl_assert_equal_i(-1, git_oid_streq(&idp, "ae90f12eea699729ec"));
	cl_assert_equal_i(-1, git_oid_streq(&idp, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"));

	cl_assert_equal_i(-1, git_oid_streq(&idp, "deadbeef"));
	cl_assert_equal_i(-1, git_oid_streq(&idp, "I'm not an oid.... :)"));
}

void test_core_oid__strcmp(void)
{
	cl_assert_equal_i(0, git_oid_strcmp(&id, str_oid));
	cl_assert(git_oid_strcmp(&id, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef") < 0);

	cl_assert(git_oid_strcmp(&id, "deadbeef") < 0);
	cl_assert_equal_i(-1, git_oid_strcmp(&id, "I'm not an oid.... :)"));

	cl_assert_equal_i(0, git_oid_strcmp(&idp, "ae90f12eea699729ed0000000000000000000000"));
	cl_assert_equal_i(0, git_oid_strcmp(&idp, "ae90f12eea699729ed"));
	cl_assert(git_oid_strcmp(&idp, "ae90f12eea699729ed1") < 0);
	cl_assert(git_oid_strcmp(&idp, "ae90f12eea699729ec") > 0);
	cl_assert(git_oid_strcmp(&idp, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef") < 0);

	cl_assert(git_oid_strcmp(&idp, "deadbeef") < 0);
	cl_assert_equal_i(-1, git_oid_strcmp(&idp, "I'm not an oid.... :)"));
}

void test_core_oid__ncmp(void)
{
	cl_assert(!git_oid_ncmp(&id, &idp, 0));
	cl_assert(!git_oid_ncmp(&id, &idp, 1));
	cl_assert(!git_oid_ncmp(&id, &idp, 2));
	cl_assert(!git_oid_ncmp(&id, &idp, 17));
	cl_assert(!git_oid_ncmp(&id, &idp, 18));
	cl_assert(git_oid_ncmp(&id, &idp, 19));
	cl_assert(git_oid_ncmp(&id, &idp, 40));
	cl_assert(git_oid_ncmp(&id, &idp, 41));
	cl_assert(git_oid_ncmp(&id, &idp, 42));

	cl_assert(!git_oid_ncmp(&id, &id, 0));
	cl_assert(!git_oid_ncmp(&id, &id, 1));
	cl_assert(!git_oid_ncmp(&id, &id, 39));
	cl_assert(!git_oid_ncmp(&id, &id, 40));
	cl_assert(!git_oid_ncmp(&id, &id, 41));
}
