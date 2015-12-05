
#include "clar_libgit2.h"

#include "odb.h"
#include "hash.h"

void test_object_raw_type2string__convert_type_to_string(void)
{
	cl_assert_equal_s(git_object_type2string(GIT_OBJ_BAD), "");
	cl_assert_equal_s(git_object_type2string(GIT_OBJ__EXT1), "");
	cl_assert_equal_s(git_object_type2string(GIT_OBJ_COMMIT), "commit");
	cl_assert_equal_s(git_object_type2string(GIT_OBJ_TREE), "tree");
	cl_assert_equal_s(git_object_type2string(GIT_OBJ_BLOB), "blob");
	cl_assert_equal_s(git_object_type2string(GIT_OBJ_TAG), "tag");
	cl_assert_equal_s(git_object_type2string(GIT_OBJ__EXT2), "");
	cl_assert_equal_s(git_object_type2string(GIT_OBJ_OFS_DELTA), "OFS_DELTA");
	cl_assert_equal_s(git_object_type2string(GIT_OBJ_REF_DELTA), "REF_DELTA");

	cl_assert_equal_s(git_object_type2string(-2), "");
	cl_assert_equal_s(git_object_type2string(8), "");
	cl_assert_equal_s(git_object_type2string(1234), "");
}

void test_object_raw_type2string__convert_string_to_type(void)
{
	cl_assert(git_object_string2type(NULL) == GIT_OBJ_BAD);
	cl_assert(git_object_string2type("") == GIT_OBJ_BAD);
	cl_assert(git_object_string2type("commit") == GIT_OBJ_COMMIT);
	cl_assert(git_object_string2type("tree") == GIT_OBJ_TREE);
	cl_assert(git_object_string2type("blob") == GIT_OBJ_BLOB);
	cl_assert(git_object_string2type("tag") == GIT_OBJ_TAG);
	cl_assert(git_object_string2type("OFS_DELTA") == GIT_OBJ_OFS_DELTA);
	cl_assert(git_object_string2type("REF_DELTA") == GIT_OBJ_REF_DELTA);

	cl_assert(git_object_string2type("CoMmIt") == GIT_OBJ_BAD);
	cl_assert(git_object_string2type("hohoho") == GIT_OBJ_BAD);
}

void test_object_raw_type2string__check_type_is_loose(void)
{
	cl_assert(git_object_typeisloose(GIT_OBJ_BAD) == 0);
	cl_assert(git_object_typeisloose(GIT_OBJ__EXT1) == 0);
	cl_assert(git_object_typeisloose(GIT_OBJ_COMMIT) == 1);
	cl_assert(git_object_typeisloose(GIT_OBJ_TREE) == 1);
	cl_assert(git_object_typeisloose(GIT_OBJ_BLOB) == 1);
	cl_assert(git_object_typeisloose(GIT_OBJ_TAG) == 1);
	cl_assert(git_object_typeisloose(GIT_OBJ__EXT2) == 0);
	cl_assert(git_object_typeisloose(GIT_OBJ_OFS_DELTA) == 0);
	cl_assert(git_object_typeisloose(GIT_OBJ_REF_DELTA) == 0);

	cl_assert(git_object_typeisloose(-2) == 0);
	cl_assert(git_object_typeisloose(8) == 0);
	cl_assert(git_object_typeisloose(1234) == 0);
}
