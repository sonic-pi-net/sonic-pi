
#include "clar_libgit2.h"

#include "odb.h"

void test_object_raw_fromstr__fail_on_invalid_oid_string(void)
{
	git_oid out;
	cl_git_fail(git_oid__fromstr(&out, "", GIT_OID_SHA1));
	cl_git_fail(git_oid__fromstr(&out, "moo", GIT_OID_SHA1));
	cl_git_fail(git_oid__fromstr(&out, "16a67770b7d8d72317c4b775213c23a8bd74f5ez", GIT_OID_SHA1));
}

void test_object_raw_fromstr__succeed_on_valid_oid_string(void)
{
	git_oid out;
	unsigned char exp[] = {
		0x16, 0xa6, 0x77, 0x70, 0xb7,
		0xd8, 0xd7, 0x23, 0x17, 0xc4,
		0xb7, 0x75, 0x21, 0x3c, 0x23,
		0xa8, 0xbd, 0x74, 0xf5, 0xe0,
	};

	cl_git_pass(git_oid__fromstr(&out, "16a67770b7d8d72317c4b775213c23a8bd74f5e0", GIT_OID_SHA1));
	cl_git_pass(memcmp(out.id, exp, GIT_OID_SHA1_SIZE));

	cl_git_pass(git_oid__fromstr(&out, "16A67770B7D8D72317C4b775213C23A8BD74F5E0", GIT_OID_SHA1));
	cl_git_pass(memcmp(out.id, exp, GIT_OID_SHA1_SIZE));
}
