#include "clar_libgit2.h"

static int is_valid_name(const char *name)
{
	int valid = 0;
	cl_git_pass(git_remote_name_is_valid(&valid, name));
	return valid;
}

void test_network_remote_isvalidname__can_detect_invalid_formats(void)
{
	cl_assert_equal_i(false, is_valid_name("/"));
	cl_assert_equal_i(false, is_valid_name("//"));
	cl_assert_equal_i(false, is_valid_name(".lock"));
	cl_assert_equal_i(false, is_valid_name("a.lock"));
	cl_assert_equal_i(false, is_valid_name("/no/leading/slash"));
	cl_assert_equal_i(false, is_valid_name("no/trailing/slash/"));
}

void test_network_remote_isvalidname__wont_hopefully_choke_on_valid_formats(void)
{
	cl_assert_equal_i(true, is_valid_name("webmatrix"));
	cl_assert_equal_i(true, is_valid_name("yishaigalatzer/rules"));
}
