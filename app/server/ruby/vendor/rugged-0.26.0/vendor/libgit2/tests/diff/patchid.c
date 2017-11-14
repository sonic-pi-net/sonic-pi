#include "clar_libgit2.h"
#include "patch/patch_common.h"

static void verify_patch_id(const char *diff_content, const char *expected_id)
{
	git_oid expected_oid, actual_oid;
	git_diff *diff;

	cl_git_pass(git_oid_fromstr(&expected_oid, expected_id));
	cl_git_pass(git_diff_from_buffer(&diff, diff_content, strlen(diff_content)));
	cl_git_pass(git_diff_patchid(&actual_oid, diff, NULL));

	cl_assert_equal_oid(&expected_oid, &actual_oid);

	git_diff_free(diff);
}

void test_diff_patchid__simple_commit(void)
{
	verify_patch_id(PATCH_SIMPLE_COMMIT, "06094b1948b878b7d9ff7560b4eae672a014b0ec");
}

void test_diff_patchid__filename_with_spaces(void)
{
	verify_patch_id(PATCH_APPEND_NO_NL, "f0ba05413beaef743b630e796153839462ee477a");
}

void test_diff_patchid__multiple_hunks(void)
{
	verify_patch_id(PATCH_MULTIPLE_HUNKS, "81e26c34643d17f521e57c483a6a637e18ba1f57");
}

void test_diff_patchid__multiple_files(void)
{
	verify_patch_id(PATCH_MULTIPLE_FILES, "192d1f49d23f2004517963aecd3f8a6c467f50ff");
}

void test_diff_patchid__same_diff_with_differing_whitespace_has_same_id(void)
{
	const char *tabs =
	    "diff --git a/file.txt b/file.txt\n"
	    "index 8fecc09..1d43a92 100644\n"
	    "--- a/file.txt\n"
	    "+++ b/file.txt\n"
	    "@@ -1 +1 @@\n"
	    "-old text\n"
	    "+		new text\n";
	const char *spaces =
	    "diff --git a/file.txt b/file.txt\n"
	    "index 8fecc09..1d43a92 100644\n"
	    "--- a/file.txt\n"
	    "+++ b/file.txt\n"
	    "@@ -1 +1 @@\n"
	    "-old text\n"
	    "+        new text\n";
	const char *id = "11efdd13c30f7a1056eac2ae2fb952da475e2c23";

	verify_patch_id(tabs, id);
	verify_patch_id(spaces, id);
}
