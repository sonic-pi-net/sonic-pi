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

	git_buf_dispose(&buf);
}

void test_diff_parse__exact_rename(void)
{
	const char *content =
	    "---\n"
	    " old_name.c => new_name.c | 0\n"
	    " 1 file changed, 0 insertions(+), 0 deletions(-)\n"
	    " rename old_name.c => new_name.c  (100%)\n"
	    "\n"
	    "diff --git a/old_name.c b/new_name.c\n"
	    "similarity index 100%\n"
	    "rename from old_name.c\n"
	    "rename to new_name.c\n"
	    "-- \n"
	    "2.9.3\n";
	git_diff *diff;

	cl_git_pass(git_diff_from_buffer(
		&diff, content, strlen(content)));
	git_diff_free(diff);
}

void test_diff_parse__empty_file(void)
{
	const char *content =
	    "---\n"
	    " file | 0\n"
	    " 1 file changed, 0 insertions(+), 0 deletions(-)\n"
	    " created mode 100644 file\n"
	    "\n"
	    "diff --git a/file b/file\n"
	    "new file mode 100644\n"
	    "index 0000000..e69de29\n"
	    "-- \n"
	    "2.20.1\n";
	git_diff *diff;

	cl_git_pass(git_diff_from_buffer(
		&diff, content, strlen(content)));
	git_diff_free(diff);
}

void test_diff_parse__no_extended_headers(void)
{
	const char *content = PATCH_NO_EXTENDED_HEADERS;
	git_diff *diff;

	cl_git_pass(git_diff_from_buffer(
		&diff, content, strlen(content)));
	git_diff_free(diff);
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

	git_buf_dispose(&computed_buf);

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

	git_buf_dispose(&computed_buf);

	cl_git_sandbox_cleanup();
}

static int file_cb(const git_diff_delta *delta, float progress, void *payload)
{
    int *called = (int *) payload;
    GIT_UNUSED(delta);
    GIT_UNUSED(progress);
    (*called)++;
    return 0;
}

void test_diff_parse__foreach_works_with_parsed_patch(void)
{
	const char patch[] =
	    "diff --git a/obj1 b/obj2\n"
	    "index 1234567..7654321 10644\n"
	    "--- a/obj1\n"
	    "+++ b/obj2\n"
	    "@@ -1 +1 @@\n"
	    "-abcde\n"
	    "+12345\n";
	int called = 0;
	git_diff *diff;

	cl_git_pass(git_diff_from_buffer(&diff, patch, strlen(patch)));
	cl_git_pass(git_diff_foreach(diff, file_cb, NULL, NULL, NULL, &called));
	cl_assert_equal_i(called, 1);

	git_diff_free(diff);
}

void test_diff_parse__parsing_minimal_patch_succeeds(void)
{
	const char patch[] =
	    "diff --git a/obj1 b/obj2\n"
	    "index 1234567..7654321 10644\n"
	    "--- a/obj1\n"
	    "+++ b/obj2\n"
	    "@@ -1 +1 @@\n"
	    "-a\n"
	    "+\n";
	git_buf buf = GIT_BUF_INIT;
	git_diff *diff;

	cl_git_pass(git_diff_from_buffer(&diff, patch, strlen(patch)));
	cl_git_pass(git_diff_to_buf(&buf, diff, GIT_DIFF_FORMAT_PATCH));
	cl_assert_equal_s(patch, buf.ptr);

	git_diff_free(diff);
	git_buf_dispose(&buf);
}

void test_diff_parse__patch_roundtrip_succeeds(void)
{
	const char buf1[] = "a\n", buf2[] = "b\n";
	git_buf patchbuf = GIT_BUF_INIT, diffbuf = GIT_BUF_INIT;
	git_patch *patch;
	git_diff *diff;

	cl_git_pass(git_patch_from_buffers(&patch, buf1, strlen(buf1), "obj1", buf2, strlen(buf2), "obj2", NULL));
	cl_git_pass(git_patch_to_buf(&patchbuf, patch));

	cl_git_pass(git_diff_from_buffer(&diff, patchbuf.ptr, patchbuf.size));
	cl_git_pass(git_diff_to_buf(&diffbuf, diff, GIT_DIFF_FORMAT_PATCH));

	cl_assert_equal_s(patchbuf.ptr, diffbuf.ptr);

	git_patch_free(patch);
	git_diff_free(diff);
	git_buf_dispose(&patchbuf);
	git_buf_dispose(&diffbuf);
}

#define cl_assert_equal_i_src(i1,i2,file,line) clar__assert_equal(file,line,#i1 " != " #i2, 1, "%d", (int)(i1), (int)(i2))

static void cl_git_assert_lineinfo_(int old_lineno, int new_lineno, int num_lines, git_patch *patch, size_t hunk_idx, size_t line_idx, const char *file, int lineno)
{
	const git_diff_line *line;

	cl_git_expect(git_patch_get_line_in_hunk(&line, patch, hunk_idx, line_idx), 0, file, lineno);
	cl_assert_equal_i_src(old_lineno, line->old_lineno, file, lineno);
	cl_assert_equal_i_src(new_lineno, line->new_lineno, file, lineno);
	cl_assert_equal_i_src(num_lines, line->num_lines, file, lineno);
}

#define cl_git_assert_lineinfo(old, new, num, p, h, l) \
	cl_git_assert_lineinfo_(old,new,num,p,h,l,__FILE__,__LINE__)


void test_diff_parse__issue4672(void)
{
	const char *text = "diff --git a/a b/a\n"
	"index 7f129fd..af431f2 100644\n"
	"--- a/a\n"
	"+++ b/a\n"
	"@@ -3 +3 @@\n"
	"-a contents 2\n"
	"+a contents\n";

	git_diff *diff;
	git_patch *patch;
	const git_diff_hunk *hunk;
	size_t n, l = 0;

	cl_git_pass(git_diff_from_buffer(&diff, text, strlen(text)));
	cl_git_pass(git_patch_from_diff(&patch, diff, 0));
	cl_git_pass(git_patch_get_hunk(&hunk, &n, patch, 0));

	cl_git_assert_lineinfo(3, -1, 1, patch, 0, l++);
	cl_git_assert_lineinfo(-1, 3, 1, patch, 0, l++);

	cl_assert_equal_i(n, l);

	git_patch_free(patch);
	git_diff_free(diff);
}

void test_diff_parse__lineinfo(void)
{
	const char *text = PATCH_ORIGINAL_TO_CHANGE_MIDDLE;
	git_diff *diff;
	git_patch *patch;
	const git_diff_hunk *hunk;
	size_t n, l = 0;

	cl_git_pass(git_diff_from_buffer(&diff, text, strlen(text)));
	cl_git_pass(git_patch_from_diff(&patch, diff, 0));
	cl_git_pass(git_patch_get_hunk(&hunk, &n, patch, 0));

	cl_git_assert_lineinfo(3, 3, 1, patch, 0, l++);
	cl_git_assert_lineinfo(4, 4, 1, patch, 0, l++);
	cl_git_assert_lineinfo(5, 5, 1, patch, 0, l++);
	cl_git_assert_lineinfo(6, -1, 1, patch, 0, l++);
	cl_git_assert_lineinfo(-1, 6, 1, patch, 0, l++);
	cl_git_assert_lineinfo(7, 7, 1, patch, 0, l++);
	cl_git_assert_lineinfo(8, 8, 1, patch, 0, l++);
	cl_git_assert_lineinfo(9, 9, 1, patch, 0, l++);

	cl_assert_equal_i(n, l);

	git_patch_free(patch);
	git_diff_free(diff);
}


void test_diff_parse__new_file_with_space(void)
{
	const char *content = PATCH_ORIGINAL_NEW_FILE_WITH_SPACE;
	git_patch *patch;
	git_diff *diff;

	cl_git_pass(git_diff_from_buffer(&diff, content, strlen(content)));
	cl_git_pass(git_patch_from_diff((git_patch **) &patch, diff, 0));

	cl_assert_equal_p(patch->diff_opts.old_prefix, NULL);
	cl_assert_equal_p(patch->delta->old_file.path, NULL);
	cl_assert_equal_s(patch->diff_opts.new_prefix, "b/");
	cl_assert_equal_s(patch->delta->new_file.path, "sp ace.txt");

	git_patch_free(patch);
	git_diff_free(diff);
}

void test_diff_parse__crlf(void)
{
	const char *text = PATCH_CRLF;
	git_diff *diff;
	git_patch *patch;
	const git_diff_delta *delta;

	cl_git_pass(git_diff_from_buffer(&diff, text, strlen(text)));
	cl_git_pass(git_patch_from_diff(&patch, diff, 0));
	delta = git_patch_get_delta(patch);

	cl_assert_equal_s(delta->old_file.path, "test-file");
	cl_assert_equal_s(delta->new_file.path, "test-file");

	git_patch_free(patch);
	git_diff_free(diff);
}
