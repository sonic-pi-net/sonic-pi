#include "clar_libgit2.h"

static int name_is_valid(const char *name)
{
	int valid;
	cl_git_pass(git_tag_name_is_valid(&valid, name));
	return valid;
}

void test_refs_tags_name__is_name_valid(void)
{
	cl_assert_equal_i(true, name_is_valid("sometag"));
	cl_assert_equal_i(true, name_is_valid("test/sometag"));

	cl_assert_equal_i(false, name_is_valid(""));
	cl_assert_equal_i(false, name_is_valid("-dash"));
}
