#include "clar_libgit2.h"
#include "patch/patch_common.h"
#include "diff_helpers.h"

static void verify_patch_id(const char *diff_content, const char *expected_id)
{
	git_oid expected_oid, actual_oid;
	git_diff *diff;

	cl_git_pass(git_oid__fromstr(&expected_oid, expected_id, GIT_OID_SHA1));
	cl_git_pass(diff_from_buffer(&diff, diff_content, strlen(diff_content)));
	cl_git_pass(git_diff_patchid(&actual_oid, diff, NULL));

	cl_assert_equal_oid(&expected_oid, &actual_oid);

	git_diff_free(diff);
}

void test_diff_patchid__simple_commit(void)
{
	verify_patch_id(PATCH_SIMPLE_COMMIT, "06094b1948b878b7d9ff7560b4eae672a014b0ec");
}

void test_diff_patchid__deleted_file(void)
{
	verify_patch_id(PATCH_DELETE_ORIGINAL, "d18507fe189f49c028b32c8c34e1ad98dd6a1aad");
	verify_patch_id(PATCH_DELETED_FILE_2_HUNKS, "f31412498a17e6c3fbc635f2c5f9aa3ef4c1a9b7");
}

void test_diff_patchid__created_file(void)
{
	verify_patch_id(PATCH_ADD_ORIGINAL, "a7d39379308021465ae2ce65e338c048a3110db6");
}

void test_diff_patchid__binary_file(void)
{
	verify_patch_id(PATCH_ADD_BINARY_NOT_PRINTED, "2b31236b485faa30cf4dd33e4d6539829996739f");
}

void test_diff_patchid__renamed_file(void)
{
	verify_patch_id(PATCH_RENAME_EXACT, "4666d50cea4976f6f727448046d43461912058fd");
	verify_patch_id(PATCH_RENAME_SIMILAR, "a795087575fcb940227be524488bedd6b3d3f438");
}

void test_diff_patchid__modechange(void)
{
	verify_patch_id(PATCH_MODECHANGE_UNCHANGED, "dbf3423ee98375ef1c72a79fbd29a049a2bae771");
	verify_patch_id(PATCH_MODECHANGE_MODIFIED, "93aba696e1bbd2bbb73e3e3e62ed71f232137657");
}

void test_diff_patchid__shuffle_hunks(void)
{
	verify_patch_id(PATCH_DELETED_FILE_2_HUNKS_SHUFFLED, "f31412498a17e6c3fbc635f2c5f9aa3ef4c1a9b7");
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
