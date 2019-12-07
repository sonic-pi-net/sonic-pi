#include "clar_libgit2.h"
#include "patch.h"
#include "patch_parse.h"

#include "patch_common.h"

static void ensure_patch_validity(git_patch *patch)
{
	const git_diff_delta *delta;
	char idstr[GIT_OID_HEXSZ+1] = {0};

	cl_assert((delta = git_patch_get_delta(patch)) != NULL);
	cl_assert_equal_i(2, delta->nfiles);

	cl_assert_equal_s(delta->old_file.path, "file.txt");
	cl_assert(delta->old_file.mode == GIT_FILEMODE_BLOB);
	cl_assert_equal_i(7, delta->old_file.id_abbrev);
	git_oid_nfmt(idstr, delta->old_file.id_abbrev, &delta->old_file.id);
	cl_assert_equal_s(idstr, "9432026");
	cl_assert_equal_i(0, delta->old_file.size);

	cl_assert_equal_s(delta->new_file.path, "file.txt");
	cl_assert(delta->new_file.mode == GIT_FILEMODE_BLOB);
	cl_assert_equal_i(7, delta->new_file.id_abbrev);
	git_oid_nfmt(idstr, delta->new_file.id_abbrev, &delta->new_file.id);
	cl_assert_equal_s(idstr, "cd8fd12");
	cl_assert_equal_i(0, delta->new_file.size);
}

void test_patch_parse__original_to_change_middle(void)
{
	git_patch *patch;

	cl_git_pass(git_patch_from_buffer(
		&patch, PATCH_ORIGINAL_TO_CHANGE_MIDDLE,
		strlen(PATCH_ORIGINAL_TO_CHANGE_MIDDLE), NULL));
	ensure_patch_validity(patch);
	git_patch_free(patch);
}

void test_patch_parse__leading_and_trailing_garbage(void)
{
	git_patch *patch;
	const char *leading = "This is some leading garbage.\n"
		"Maybe it's email headers?\n"
		"\n"
		PATCH_ORIGINAL_TO_CHANGE_MIDDLE;
	const char *trailing = PATCH_ORIGINAL_TO_CHANGE_MIDDLE
		"\n"
		"This is some trailing garbage.\n"
		"Maybe it's an email signature?\n";
	const char *both = "Here's some leading garbage\n"
		PATCH_ORIGINAL_TO_CHANGE_MIDDLE
		"And here's some trailing.\n";

	cl_git_pass(git_patch_from_buffer(&patch, leading, strlen(leading),
		NULL));
	ensure_patch_validity(patch);
	git_patch_free(patch);

	cl_git_pass(git_patch_from_buffer(&patch, trailing, strlen(trailing),
		NULL));
	ensure_patch_validity(patch);
	git_patch_free(patch);

	cl_git_pass(git_patch_from_buffer(&patch, both, strlen(both),
		NULL));
	ensure_patch_validity(patch);
	git_patch_free(patch);
}

void test_patch_parse__nonpatches_fail_with_notfound(void)
{
	git_patch *patch;

	cl_git_fail_with(GIT_ENOTFOUND,
		git_patch_from_buffer(&patch, PATCH_NOT_A_PATCH,
		strlen(PATCH_NOT_A_PATCH), NULL));
}

void test_patch_parse__invalid_patches_fails(void)
{
	git_patch *patch;

	cl_git_fail_with(GIT_ERROR,
		git_patch_from_buffer(&patch, PATCH_CORRUPT_GIT_HEADER,
		strlen(PATCH_CORRUPT_GIT_HEADER), NULL));
	cl_git_fail_with(GIT_ERROR,
		git_patch_from_buffer(&patch,
		PATCH_CORRUPT_MISSING_NEW_FILE,
		strlen(PATCH_CORRUPT_MISSING_NEW_FILE), NULL));
	cl_git_fail_with(GIT_ERROR,
		git_patch_from_buffer(&patch,
		PATCH_CORRUPT_MISSING_OLD_FILE,
		strlen(PATCH_CORRUPT_MISSING_OLD_FILE), NULL));
	cl_git_fail_with(GIT_ERROR,
		git_patch_from_buffer(&patch, PATCH_CORRUPT_NO_CHANGES,
		strlen(PATCH_CORRUPT_NO_CHANGES), NULL));
	cl_git_fail_with(GIT_ERROR,
		git_patch_from_buffer(&patch,
		PATCH_CORRUPT_MISSING_HUNK_HEADER,
		strlen(PATCH_CORRUPT_MISSING_HUNK_HEADER), NULL));
}

