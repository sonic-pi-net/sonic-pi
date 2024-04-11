#include "clar_libgit2.h"

void test_odb_open__initialize(void)
{
	cl_fixture_sandbox("testrepo.git");
}

void test_odb_open__cleanup(void)
{
	cl_fixture_cleanup("testrepo.git");
}

void test_odb_open__exists(void)
{
	git_odb *odb;
	git_oid one, two;

#ifdef GIT_EXPERIMENTAL_SHA256
	git_odb_options opts = GIT_ODB_OPTIONS_INIT;

	cl_git_pass(git_odb_open(&odb, "testrepo.git/objects", &opts));
	cl_git_pass(git_oid_fromstr(&one, "1385f264afb75a56a5bec74243be9b367ba4ca08", GIT_OID_SHA1));
	cl_git_pass(git_oid_fromstr(&two, "00112233445566778899aabbccddeeff00112233", GIT_OID_SHA1));
#else
	cl_git_pass(git_odb_open(&odb, "testrepo.git/objects"));
	cl_git_pass(git_oid_fromstr(&one, "1385f264afb75a56a5bec74243be9b367ba4ca08"));
	cl_git_pass(git_oid_fromstr(&two, "00112233445566778899aabbccddeeff00112233"));
#endif

	cl_assert(git_odb_exists(odb, &one));
	cl_assert(!git_odb_exists(odb, &two));

	git_odb_free(odb);
}
