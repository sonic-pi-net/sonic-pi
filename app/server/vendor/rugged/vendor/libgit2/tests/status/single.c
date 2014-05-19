#include "clar_libgit2.h"
#include "posix.h"

static void
cleanup__remove_file(void *_file)
{
	cl_must_pass(p_unlink((char *)_file));
}

/* test retrieving OID from a file apart from the ODB */
void test_status_single__hash_single_file(void)
{
	static const char file_name[] = "new_file";
	static const char file_contents[] = "new_file\n";
	static const char file_hash[] = "d4fa8600b4f37d7516bef4816ae2c64dbf029e3a";

	git_oid expected_id, actual_id;

	/* initialization */
	git_oid_fromstr(&expected_id, file_hash);
	cl_git_mkfile(file_name, file_contents);
	cl_set_cleanup(&cleanup__remove_file, (void *)file_name);

	cl_git_pass(git_odb_hashfile(&actual_id, file_name, GIT_OBJ_BLOB));
	cl_assert(git_oid_cmp(&expected_id, &actual_id) == 0);
}

/* test retrieving OID from an empty file apart from the ODB */
void test_status_single__hash_single_empty_file(void)
{
	static const char file_name[] = "new_empty_file";
	static const char file_contents[] = "";
	static const char file_hash[] = "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391";

	git_oid expected_id, actual_id;

	/* initialization */
	git_oid_fromstr(&expected_id, file_hash);
	cl_git_mkfile(file_name, file_contents);
	cl_set_cleanup(&cleanup__remove_file, (void *)file_name);

	cl_git_pass(git_odb_hashfile(&actual_id, file_name, GIT_OBJ_BLOB));
	cl_assert(git_oid_cmp(&expected_id, &actual_id) == 0);
}

