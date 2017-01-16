#include "clar_libgit2.h"
#include "patch.h"
#include "patch_parse.h"
#include "diff_helpers.h"
#include "../src/diff.h"

#include "../patch/patch_common.h"

void test_diff_parse__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_diff_parse__nonpatches_fail_with_notfound(void)
{
	git_diff *diff;
	const char *not = PATCH_NOT_A_PATCH;
	const char *not_with_leading = "Leading text.\n" PATCH_NOT_A_PATCH;
	const char *not_with_trailing = PATCH_NOT_A_PATCH "Trailing text.\n";
	const char *not_with_both = "Lead.\n" PATCH_NOT_A_PATCH "Trail.\n";

	cl_git_fail_with(GIT_ENOTFOUND,
		git_diff_from_buffer(&diff,
		not,
		strlen(not)));
	cl_git_fail_with(GIT_ENOTFOUND,
		git_diff_from_buffer(&diff,
		not_with_leading,
		strlen(not_with_leading)));
	cl_git_fail_with(GIT_ENOTFOUND,
		git_diff_from_buffer(&diff,
		not_with_trailing,
		strlen(not_with_trailing)));
	cl_git_fail_with(GIT_ENOTFOUND,
		git_diff_from_buffer(&diff,
		not_with_both,
		strlen(not_with_both)));
}

static void test_parse_invalid_diff(const char *invalid_diff)
{
	git_diff *diff;
	git_buf buf = GIT_BUF_INIT;

	/* throw some random (legitimate) diffs in with the given invalid
	 * one.
	 */
	git_buf_puts(&buf, PATCH_ORIGINAL_TO_CHANGE_FIRSTLINE);
	git_buf_puts(&buf, PATCH_BINARY_DELTA);
	git_buf_puts(&buf, invalid_diff);
	git_buf_puts(&buf, PATCH_ORIGINAL_TO_CHANGE_MIDDLE);
	git_buf_puts(&buf, PATCH_BINARY_LITERAL);

	cl_git_fail_with(GIT_ERROR,
		git_diff_from_buffer(&diff, buf.ptr, buf.size));

	git_buf_free(&buf);
}

void test_diff_parse__invalid_patches_fails(void)
{
	test_parse_invalid_diff(PATCH_CORRUPT_MISSING_NEW_FILE);
	test_parse_invalid_diff(PATCH_CORRUPT_MISSING_OLD_FILE);
	test_parse_invalid_diff(PATCH_CORRUPT_NO_CHANGES);
	test_parse_invalid_diff(PATCH_CORRUPT_MISSING_HUNK_HEADER);
}

static void test_tree_to_tree_computed_to_parsed(
	const char *sandbox, const char *a_id, const char *b_id,
	uint32_t diff_flags, uint32_t find_flags)
{
	git_repository *repo;
	git_diff *computed, *parsed;
	git_tree *a, *b;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff_find_options findopts = GIT_DIFF_FIND_OPTIONS_INIT;
	git_buf computed_buf = GIT_BUF_INIT;

	repo = cl_git_sandbox_init(sandbox);

	opts.id_abbrev = GIT_OID_HEXSZ;
	opts.flags = GIT_DIFF_SHOW_BINARY | diff_flags;
	findopts.flags = find_flags;

	cl_assert((a = resolve_commit_oid_to_tree(repo, a_id)) != NULL);
	cl_assert((b = resolve_commit_oid_to_tree(repo, b_id)) != NULL);

	cl_git_pass(git_diff_tree_to_tree(&computed, repo, a, b, &opts));

	if (find_flags)
		cl_git_pass(git_diff_find_similar(computed, &findopts));

	cl_git_pass(git_diff_to_buf(&computed_buf,
		computed, GIT_DIFF_FORMAT_PATCH));

	cl_git_pass(git_diff_from_buffer(&parsed,
		computed_buf.ptr, computed_buf.size));

	diff_assert_equal(computed, parsed);

	git_tree_free(a);
	git_tree_free(b);

	git_diff_free(computed);
	git_diff_free(parsed);

	git_buf_free(&computed_buf);

	cl_git_sandbox_cleanup();
}

void test_diff_parse__can_parse_generated_diff(void)
{
	test_tree_to_tree_computed_to_parsed(
		"diff", "d70d245e", "7a9e0b02", 0, 0);
	test_tree_to_tree_computed_to_parsed(
		"unsymlinked.git", "806999", "a8595c", 0, 0);
	test_tree_to_tree_computed_to_parsed("diff",
		"d70d245ed97ed2aa596dd1af6536e4bfdb047b69",
		"7a9e0b02e63179929fed24f0a3e0f19168114d10", 0, 0);
	test_tree_to_tree_computed_to_parsed(
		"unsymlinked.git", "7fccd7", "806999", 0, 0);
	test_tree_to_tree_computed_to_parsed(
		"unsymlinked.git", "7fccd7", "a8595c", 0, 0);
	test_tree_to_tree_computed_to_parsed(
		"attr", "605812a", "370fe9ec22", 0, 0);
	test_tree_to_tree_computed_to_parsed(
		"attr", "f5b0af1fb4f5c", "370fe9ec22", 0, 0);
	test_tree_to_tree_computed_to_parsed(
		"diff", "d70d245e", "d70d245e", 0, 0);
	test_tree_to_tree_computed_to_parsed("diff_format_email",
		"873806f6f27e631eb0b23e4b56bea2bfac14a373",
		"897d3af16ca9e420cd071b1c4541bd2b91d04c8c",
		GIT_DIFF_SHOW_BINARY, 0);
	test_tree_to_tree_computed_to_parsed("diff_format_email",
		"897d3af16ca9e420cd071b1c4541bd2b91d04c8c",
		"873806f6f27e631eb0b23e4b56bea2bfac14a373",
		GIT_DIFF_SHOW_BINARY, 0);
	test_tree_to_tree_computed_to_parsed("renames",
		"31e47d8c1fa36d7f8d537b96158e3f024de0a9f2",
		"2bc7f351d20b53f1c72c16c4b036e491c478c49a",
		0, GIT_DIFF_FIND_RENAMES);
	test_tree_to_tree_computed_to_parsed("renames",
		"31e47d8c1fa36d7f8d537b96158e3f024de0a9f2",
		"2bc7f351d20b53f1c72c16c4b036e491c478c49a",
		GIT_DIFF_INCLUDE_UNMODIFIED,
		0);
	test_tree_to_tree_computed_to_parsed("renames",
		"31e47d8c1fa36d7f8d537b96158e3f024de0a9f2",
		"2bc7f351d20b53f1c72c16c4b036e491c478c49a",
		GIT_DIFF_INCLUDE_UNMODIFIED,
		GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED | GIT_DIFF_FIND_EXACT_MATCH_ONLY);
}

void test_diff_parse__get_patch_from_diff(void)
{
	git_repository *repo;
	git_diff *computed, *parsed;
	git_tree *a, *b;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_buf computed_buf = GIT_BUF_INIT;
	git_patch *patch_computed, *patch_parsed;

	repo = cl_git_sandbox_init("diff");

	opts.flags = GIT_DIFF_SHOW_BINARY;

	cl_assert((a = resolve_commit_oid_to_tree(repo,
		"d70d245ed97ed2aa596dd1af6536e4bfdb047b69")) != NULL);
	cl_assert((b = resolve_commit_oid_to_tree(repo,
		"7a9e0b02e63179929fed24f0a3e0f19168114d10")) != NULL);

	cl_git_pass(git_diff_tree_to_tree(&computed, repo, a, b, &opts));
	cl_git_pass(git_diff_to_buf(&computed_buf,
		computed, GIT_DIFF_FORMAT_PATCH));
	cl_git_pass(git_patch_from_diff(&patch_computed, computed, 0));

	cl_git_pass(git_diff_from_buffer(&parsed,
		computed_buf.ptr, computed_buf.size));
	cl_git_pass(git_patch_from_diff(&patch_parsed, parsed, 0));

	cl_assert_equal_i(
		git_patch_num_hunks(patch_computed),
		git_patch_num_hunks(patch_parsed));

	git_patch_free(patch_computed);
	git_patch_free(patch_parsed);

	git_tree_free(a);
	git_tree_free(b);

	git_diff_free(computed);
	git_diff_free(parsed);

	git_buf_free(&computed_buf);

	cl_git_sandbox_cleanup();
}
