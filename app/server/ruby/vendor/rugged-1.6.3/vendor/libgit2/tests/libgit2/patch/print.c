#include "clar_libgit2.h"
#include "patch.h"
#include "patch_parse.h"

#include "patch_common.h"


/* sanity check the round-trip of patch parsing:  ensure that we can parse
 * and then print a variety of patch files.
 */

static void patch_print_from_patchfile(const char *data, size_t len)
{
	git_patch *patch;
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_patch_from_buffer(&patch, data, len, NULL));
	cl_git_pass(git_patch_to_buf(&buf, patch));

	cl_assert_equal_s(data, buf.ptr);

	git_patch_free(patch);
	git_buf_dispose(&buf);
}

void test_patch_print__change_middle(void)
{
	patch_print_from_patchfile(PATCH_ORIGINAL_TO_CHANGE_MIDDLE,
		strlen(PATCH_ORIGINAL_TO_CHANGE_MIDDLE));
}

void test_patch_print__change_middle_nocontext(void)
{
	patch_print_from_patchfile(PATCH_ORIGINAL_TO_CHANGE_MIDDLE_NOCONTEXT,
		strlen(PATCH_ORIGINAL_TO_CHANGE_MIDDLE_NOCONTEXT));
}

void test_patch_print__change_firstline(void)
{
	patch_print_from_patchfile(PATCH_ORIGINAL_TO_CHANGE_FIRSTLINE,
		strlen(PATCH_ORIGINAL_TO_CHANGE_FIRSTLINE));
}

void test_patch_print__change_lastline(void)
{
	patch_print_from_patchfile(PATCH_ORIGINAL_TO_CHANGE_LASTLINE,
		strlen(PATCH_ORIGINAL_TO_CHANGE_LASTLINE));
}

void test_patch_print__prepend(void)
{
	patch_print_from_patchfile(PATCH_ORIGINAL_TO_PREPEND,
		strlen(PATCH_ORIGINAL_TO_PREPEND));
}

void test_patch_print__prepend_nocontext(void)
{
	patch_print_from_patchfile(PATCH_ORIGINAL_TO_PREPEND_NOCONTEXT,
		strlen(PATCH_ORIGINAL_TO_PREPEND_NOCONTEXT));
}

void test_patch_print__append(void)
{
	patch_print_from_patchfile(PATCH_ORIGINAL_TO_APPEND,
		strlen(PATCH_ORIGINAL_TO_APPEND));
}

void test_patch_print__append_nocontext(void)
{
	patch_print_from_patchfile(PATCH_ORIGINAL_TO_APPEND_NOCONTEXT,
		strlen(PATCH_ORIGINAL_TO_APPEND_NOCONTEXT));
}

void test_patch_print__prepend_and_append(void)
{
	patch_print_from_patchfile(PATCH_ORIGINAL_TO_PREPEND_AND_APPEND,
		strlen(PATCH_ORIGINAL_TO_PREPEND_AND_APPEND));
}

void test_patch_print__to_empty_file(void)
{
	patch_print_from_patchfile(PATCH_ORIGINAL_TO_EMPTY_FILE,
		strlen(PATCH_ORIGINAL_TO_EMPTY_FILE));
}

void test_patch_print__from_empty_file(void)
{
	patch_print_from_patchfile(PATCH_EMPTY_FILE_TO_ORIGINAL,
		strlen(PATCH_EMPTY_FILE_TO_ORIGINAL));
}

void test_patch_print__add(void)
{
	patch_print_from_patchfile(PATCH_ADD_ORIGINAL,
		strlen(PATCH_ADD_ORIGINAL));
}

void test_patch_print__delete(void)
{
	patch_print_from_patchfile(PATCH_DELETE_ORIGINAL,
		strlen(PATCH_DELETE_ORIGINAL));
}

void test_patch_print__rename_exact(void)
{
	patch_print_from_patchfile(PATCH_RENAME_EXACT,
		strlen(PATCH_RENAME_EXACT));
}

void test_patch_print__rename_exact_with_mode(void)
{
	patch_print_from_patchfile(PATCH_RENAME_EXACT_WITH_MODE,
		strlen(PATCH_RENAME_EXACT_WITH_MODE));
}

void test_patch_print__rename_similar(void)
{
	patch_print_from_patchfile(PATCH_RENAME_SIMILAR,
		strlen(PATCH_RENAME_SIMILAR));
}

void test_patch_print__rename_exact_quotedname(void)
{
	patch_print_from_patchfile(PATCH_RENAME_EXACT_QUOTEDNAME,
		strlen(PATCH_RENAME_EXACT_QUOTEDNAME));
}

void test_patch_print__rename_similar_quotedname(void)
{
	patch_print_from_patchfile(PATCH_RENAME_SIMILAR_QUOTEDNAME,
		strlen(PATCH_RENAME_SIMILAR_QUOTEDNAME));
}

void test_patch_print__modechange_unchanged(void)
{
	patch_print_from_patchfile(PATCH_MODECHANGE_UNCHANGED,
		strlen(PATCH_MODECHANGE_UNCHANGED));
}

void test_patch_print__modechange_modified(void)
{
	patch_print_from_patchfile(PATCH_MODECHANGE_MODIFIED,
		strlen(PATCH_MODECHANGE_MODIFIED));
}

void test_patch_print__binary_literal(void)
{
	patch_print_from_patchfile(PATCH_BINARY_LITERAL,
		strlen(PATCH_BINARY_LITERAL));
}

void test_patch_print__binary_delta(void)
{
	patch_print_from_patchfile(PATCH_BINARY_DELTA,
		strlen(PATCH_BINARY_DELTA));
}

void test_patch_print__binary_add(void)
{
	patch_print_from_patchfile(PATCH_BINARY_ADD,
		strlen(PATCH_BINARY_ADD));
}

void test_patch_print__binary_delete(void)
{
	patch_print_from_patchfile(PATCH_BINARY_DELETE,
		strlen(PATCH_BINARY_DELETE));
}

void test_patch_print__not_reversible(void)
{
	patch_print_from_patchfile(PATCH_BINARY_NOT_REVERSIBLE,
		strlen(PATCH_BINARY_NOT_REVERSIBLE));
}

void test_patch_print__binary_not_shown(void)
{
	patch_print_from_patchfile(PATCH_BINARY_NOT_PRINTED,
		strlen(PATCH_BINARY_NOT_PRINTED));
}

void test_patch_print__binary_add_not_shown(void)
{
	patch_print_from_patchfile(PATCH_ADD_BINARY_NOT_PRINTED,
		strlen(PATCH_ADD_BINARY_NOT_PRINTED));
}
