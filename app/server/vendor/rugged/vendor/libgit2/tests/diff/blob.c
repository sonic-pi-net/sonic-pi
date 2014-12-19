#include "clar_libgit2.h"
#include "diff_helpers.h"

static git_repository *g_repo = NULL;
static diff_expects expected;
static git_diff_options opts;
static git_blob *d, *alien;

static void quick_diff_blob_to_str(
	const git_blob *blob, const char *blob_path,
	const char *str, size_t len, const char *str_path)
{
	memset(&expected, 0, sizeof(expected));

	if (str && !len)
		len = strlen(str);

	cl_git_pass(git_diff_blob_to_buffer(
		blob, blob_path, str, len, str_path,
		&opts, diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
}

void test_diff_blob__initialize(void)
{
	git_oid oid;

	g_repo = cl_git_sandbox_init("attr");

	cl_git_pass(git_diff_init_options(&opts, GIT_DIFF_OPTIONS_VERSION));
	opts.context_lines = 1;

	memset(&expected, 0, sizeof(expected));

	/* tests/resources/attr/root_test4.txt */
	cl_git_pass(git_oid_fromstrn(&oid, "a0f7217a", 8));
	cl_git_pass(git_blob_lookup_prefix(&d, g_repo, &oid, 4));

	/* alien.png */
	cl_git_pass(git_oid_fromstrn(&oid, "edf3dcee", 8));
	cl_git_pass(git_blob_lookup_prefix(&alien, g_repo, &oid, 4));
}

void test_diff_blob__cleanup(void)
{
	git_blob_free(d);
	d = NULL;

	git_blob_free(alien);
	alien = NULL;

	cl_git_sandbox_cleanup();
}

static void assert_one_modified(
	int hunks, int lines, int ctxt, int adds, int dels, diff_expects *exp)
{
	cl_assert_equal_i(1, exp->files);
	cl_assert_equal_i(1, exp->file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(0, exp->files_binary);

	cl_assert_equal_i(hunks, exp->hunks);
	cl_assert_equal_i(lines, exp->lines);
	cl_assert_equal_i(ctxt,  exp->line_ctxt);
	cl_assert_equal_i(adds,  exp->line_adds);
	cl_assert_equal_i(dels,  exp->line_dels);
}

void test_diff_blob__can_compare_text_blobs(void)
{
	git_blob *a, *b, *c;
	git_oid a_oid, b_oid, c_oid;

	/* tests/resources/attr/root_test1 */
	cl_git_pass(git_oid_fromstrn(&a_oid, "45141a79", 8));
	cl_git_pass(git_blob_lookup_prefix(&a, g_repo, &a_oid, 4));

	/* tests/resources/attr/root_test2 */
	cl_git_pass(git_oid_fromstrn(&b_oid, "4d713dc4", 8));
	cl_git_pass(git_blob_lookup_prefix(&b, g_repo, &b_oid, 4));

	/* tests/resources/attr/root_test3 */
	cl_git_pass(git_oid_fromstrn(&c_oid, "c96bbb2c2557a832", 16));
	cl_git_pass(git_blob_lookup_prefix(&c, g_repo, &c_oid, 8));

	/* Doing the equivalent of a `git diff -U1` on these files */

	/* diff on tests/resources/attr/root_test1 */
	memset(&expected, 0, sizeof(expected));
	cl_git_pass(git_diff_blobs(
		a, NULL, b, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
	assert_one_modified(1, 6, 1, 5, 0, &expected);

	/* same diff but use direct buffers */
	memset(&expected, 0, sizeof(expected));
	cl_git_pass(git_diff_buffers(
		git_blob_rawcontent(a), (size_t)git_blob_rawsize(a), NULL,
		git_blob_rawcontent(b), (size_t)git_blob_rawsize(b), NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
	assert_one_modified(1, 6, 1, 5, 0, &expected);

	/* diff on tests/resources/attr/root_test2 */
	memset(&expected, 0, sizeof(expected));
	cl_git_pass(git_diff_blobs(
		b, NULL, c, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
	assert_one_modified(1, 15, 3, 9, 3, &expected);

	/* diff on tests/resources/attr/root_test3 */
	memset(&expected, 0, sizeof(expected));
	cl_git_pass(git_diff_blobs(
		a, NULL, c, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
	assert_one_modified(1, 13, 0, 12, 1, &expected);

	memset(&expected, 0, sizeof(expected));
	cl_git_pass(git_diff_blobs(
		c, NULL, d, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
	assert_one_modified(2, 14, 4, 6, 4, &expected);

	git_blob_free(a);
	git_blob_free(b);
	git_blob_free(c);
}

static void assert_patch_matches_blobs(
	git_patch *p, git_blob *a, git_blob *b,
	int hunks, int l0, int l1, int ctxt, int adds, int dels)
{
	const git_diff_delta *delta;
	size_t tc, ta, td;

	cl_assert(p != NULL);

	delta = git_patch_get_delta(p);
	cl_assert(delta != NULL);

	cl_assert_equal_i(GIT_DELTA_MODIFIED, delta->status);
	cl_assert_equal_oid(git_blob_id(a), &delta->old_file.id);
	cl_assert_equal_sz(git_blob_rawsize(a), delta->old_file.size);
	cl_assert_equal_oid(git_blob_id(b), &delta->new_file.id);
	cl_assert_equal_sz(git_blob_rawsize(b), delta->new_file.size);

	cl_assert_equal_i(hunks, (int)git_patch_num_hunks(p));

	if (hunks > 0)
		cl_assert_equal_i(l0, git_patch_num_lines_in_hunk(p, 0));
	if (hunks > 1)
		cl_assert_equal_i(l1, git_patch_num_lines_in_hunk(p, 1));

	cl_git_pass(git_patch_line_stats(&tc, &ta, &td, p));
	cl_assert_equal_i(ctxt, (int)tc);
	cl_assert_equal_i(adds, (int)ta);
	cl_assert_equal_i(dels, (int)td);
}

void test_diff_blob__can_compare_text_blobs_with_patch(void)
{
	git_blob *a, *b, *c;
	git_oid a_oid, b_oid, c_oid;
	git_patch *p;

	/* tests/resources/attr/root_test1 */
	cl_git_pass(git_oid_fromstrn(&a_oid, "45141a79", 8));
	cl_git_pass(git_blob_lookup_prefix(&a, g_repo, &a_oid, 4));

	/* tests/resources/attr/root_test2 */
	cl_git_pass(git_oid_fromstrn(&b_oid, "4d713dc4", 8));
	cl_git_pass(git_blob_lookup_prefix(&b, g_repo, &b_oid, 4));

	/* tests/resources/attr/root_test3 */
	cl_git_pass(git_oid_fromstrn(&c_oid, "c96bbb2c2557a832", 16));
	cl_git_pass(git_blob_lookup_prefix(&c, g_repo, &c_oid, 8));

	/* Doing the equivalent of a `git diff -U1` on these files */

	/* diff on tests/resources/attr/root_test1 */
	cl_git_pass(git_patch_from_blobs(&p, a, NULL, b, NULL, &opts));
	assert_patch_matches_blobs(p, a, b, 1, 6, 0, 1, 5, 0);
	git_patch_free(p);

	/* diff on tests/resources/attr/root_test2 */
	cl_git_pass(git_patch_from_blobs(&p, b, NULL, c, NULL, &opts));
	assert_patch_matches_blobs(p, b, c, 1, 15, 0, 3, 9, 3);
	git_patch_free(p);

	/* diff on tests/resources/attr/root_test3 */
	cl_git_pass(git_patch_from_blobs(&p, a, NULL, c, NULL, &opts));
	assert_patch_matches_blobs(p, a, c, 1, 13, 0, 0, 12, 1);
	git_patch_free(p);

	/* one more */
	cl_git_pass(git_patch_from_blobs(&p, c, NULL, d, NULL, &opts));
	assert_patch_matches_blobs(p, c, d, 2, 5, 9, 4, 6, 4);
	git_patch_free(p);

	git_blob_free(a);
	git_blob_free(b);
	git_blob_free(c);
}

void test_diff_blob__can_compare_against_null_blobs(void)
{
	git_blob *e = NULL;

	cl_git_pass(git_diff_blobs(
		d, NULL, e, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	cl_assert_equal_i(1, expected.files);
	cl_assert_equal_i(1, expected.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(0, expected.files_binary);

	cl_assert_equal_i(1, expected.hunks);
	cl_assert_equal_i(14, expected.hunk_old_lines);
	cl_assert_equal_i(14, expected.lines);
	cl_assert_equal_i(14, expected.line_dels);

	opts.flags |= GIT_DIFF_REVERSE;
	memset(&expected, 0, sizeof(expected));

	cl_git_pass(git_diff_blobs(
		d, NULL, e, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	cl_assert_equal_i(1, expected.files);
	cl_assert_equal_i(1, expected.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(0, expected.files_binary);

	cl_assert_equal_i(1, expected.hunks);
	cl_assert_equal_i(14, expected.hunk_new_lines);
	cl_assert_equal_i(14, expected.lines);
	cl_assert_equal_i(14, expected.line_adds);

	opts.flags ^= GIT_DIFF_REVERSE;
	memset(&expected, 0, sizeof(expected));

	cl_git_pass(git_diff_blobs(
		alien, NULL, NULL, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	cl_assert_equal_i(1, expected.files);
	cl_assert_equal_i(1, expected.files_binary);
	cl_assert_equal_i(1, expected.file_status[GIT_DELTA_DELETED]);
	cl_assert_equal_i(0, expected.hunks);
	cl_assert_equal_i(0, expected.lines);

	memset(&expected, 0, sizeof(expected));

	cl_git_pass(git_diff_blobs(
		NULL, NULL, alien, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	cl_assert_equal_i(1, expected.files);
	cl_assert_equal_i(1, expected.files_binary);
	cl_assert_equal_i(1, expected.file_status[GIT_DELTA_ADDED]);
	cl_assert_equal_i(0, expected.hunks);
	cl_assert_equal_i(0, expected.lines);
}

void test_diff_blob__can_compare_against_null_blobs_with_patch(void)
{
	git_blob *e = NULL;
	git_patch *p;
	const git_diff_delta *delta;
	const git_diff_line *line;
	int l, max_l;

	cl_git_pass(git_patch_from_blobs(&p, d, NULL, e, NULL, &opts));

	cl_assert(p != NULL);

	delta = git_patch_get_delta(p);
	cl_assert(delta != NULL);
	cl_assert_equal_i(GIT_DELTA_DELETED, delta->status);
	cl_assert_equal_oid(git_blob_id(d), &delta->old_file.id);
	cl_assert_equal_sz(git_blob_rawsize(d), delta->old_file.size);
	cl_assert(git_oid_iszero(&delta->new_file.id));
	cl_assert_equal_sz(0, delta->new_file.size);

	cl_assert_equal_i(1, (int)git_patch_num_hunks(p));
	cl_assert_equal_i(14, git_patch_num_lines_in_hunk(p, 0));

	max_l = git_patch_num_lines_in_hunk(p, 0);
	for (l = 0; l < max_l; ++l) {
		cl_git_pass(git_patch_get_line_in_hunk(&line, p, 0, l));
		cl_assert_equal_i(GIT_DIFF_LINE_DELETION, (int)line->origin);
	}

	git_patch_free(p);

	opts.flags |= GIT_DIFF_REVERSE;

	cl_git_pass(git_patch_from_blobs(&p, d, NULL, e, NULL, &opts));

	cl_assert(p != NULL);

	delta = git_patch_get_delta(p);
	cl_assert(delta != NULL);
	cl_assert_equal_i(GIT_DELTA_ADDED, delta->status);
	cl_assert(git_oid_iszero(&delta->old_file.id));
	cl_assert_equal_sz(0, delta->old_file.size);
	cl_assert_equal_oid(git_blob_id(d), &delta->new_file.id);
	cl_assert_equal_sz(git_blob_rawsize(d), delta->new_file.size);

	cl_assert_equal_i(1, (int)git_patch_num_hunks(p));
	cl_assert_equal_i(14, git_patch_num_lines_in_hunk(p, 0));

	max_l = git_patch_num_lines_in_hunk(p, 0);
	for (l = 0; l < max_l; ++l) {
		cl_git_pass(git_patch_get_line_in_hunk(&line, p, 0, l));
		cl_assert_equal_i(GIT_DIFF_LINE_ADDITION, (int)line->origin);
	}

	git_patch_free(p);

	opts.flags ^= GIT_DIFF_REVERSE;

	cl_git_pass(git_patch_from_blobs(&p, alien, NULL, NULL, NULL, &opts));

	cl_assert(p != NULL);

	delta = git_patch_get_delta(p);
	cl_assert(delta != NULL);
	cl_assert_equal_i(GIT_DELTA_DELETED, delta->status);
	cl_assert((delta->flags & GIT_DIFF_FLAG_BINARY) != 0);

	cl_assert_equal_i(0, (int)git_patch_num_hunks(p));

	git_patch_free(p);

	cl_git_pass(git_patch_from_blobs(&p, NULL, NULL, alien, NULL, &opts));

	cl_assert(p != NULL);

	delta = git_patch_get_delta(p);
	cl_assert(delta != NULL);
	cl_assert_equal_i(GIT_DELTA_ADDED, delta->status);
	cl_assert((delta->flags & GIT_DIFF_FLAG_BINARY) != 0);

	cl_assert_equal_i(0, (int)git_patch_num_hunks(p));

	git_patch_free(p);
}

static void assert_identical_blobs_comparison(diff_expects *expected)
{
	cl_assert_equal_i(1, expected->files);
	cl_assert_equal_i(1, expected->file_status[GIT_DELTA_UNMODIFIED]);
	cl_assert_equal_i(0, expected->hunks);
	cl_assert_equal_i(0, expected->lines);
}

void test_diff_blob__can_compare_identical_blobs(void)
{
	opts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;

	cl_git_pass(git_diff_blobs(
		d, NULL, d, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	assert_identical_blobs_comparison(&expected);
	cl_assert_equal_i(0, expected.files_binary);

	memset(&expected, 0, sizeof(expected));
	cl_git_pass(git_diff_blobs(
		NULL, NULL, NULL, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	assert_identical_blobs_comparison(&expected);
	cl_assert_equal_i(0, expected.files_binary);

	memset(&expected, 0, sizeof(expected));
	cl_git_pass(git_diff_blobs(
		alien, NULL, alien, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	assert_identical_blobs_comparison(&expected);
	cl_assert(expected.files_binary > 0);
}

void test_diff_blob__can_compare_identical_blobs_with_patch(void)
{
	git_patch *p;
	const git_diff_delta *delta;

	cl_git_pass(git_patch_from_blobs(&p, d, NULL, d, NULL, &opts));
	cl_assert(p != NULL);

	delta = git_patch_get_delta(p);
	cl_assert(delta != NULL);
	cl_assert_equal_i(GIT_DELTA_UNMODIFIED, delta->status);
	cl_assert_equal_sz(delta->old_file.size, git_blob_rawsize(d));
	cl_assert_equal_oid(git_blob_id(d), &delta->old_file.id);
	cl_assert_equal_sz(delta->new_file.size, git_blob_rawsize(d));
	cl_assert_equal_oid(git_blob_id(d), &delta->new_file.id);

	cl_assert_equal_i(0, (int)git_patch_num_hunks(p));
	git_patch_free(p);

	cl_git_pass(git_patch_from_blobs(&p, NULL, NULL, NULL, NULL, &opts));
	cl_assert(p != NULL);

	delta = git_patch_get_delta(p);
	cl_assert(delta != NULL);
	cl_assert_equal_i(GIT_DELTA_UNMODIFIED, delta->status);
	cl_assert_equal_sz(0, delta->old_file.size);
	cl_assert(git_oid_iszero(&delta->old_file.id));
	cl_assert_equal_sz(0, delta->new_file.size);
	cl_assert(git_oid_iszero(&delta->new_file.id));

	cl_assert_equal_i(0, (int)git_patch_num_hunks(p));
	git_patch_free(p);

	cl_git_pass(git_patch_from_blobs(&p, alien, NULL, alien, NULL, &opts));
	cl_assert(p != NULL);
	cl_assert_equal_i(GIT_DELTA_UNMODIFIED, git_patch_get_delta(p)->status);
	cl_assert_equal_i(0, (int)git_patch_num_hunks(p));
	git_patch_free(p);
}

static void assert_binary_blobs_comparison(diff_expects *expected)
{
	cl_assert(expected->files_binary > 0);

	cl_assert_equal_i(1, expected->files);
	cl_assert_equal_i(1, expected->file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(0, expected->hunks);
	cl_assert_equal_i(0, expected->lines);
}

void test_diff_blob__can_compare_two_binary_blobs(void)
{
	git_blob *heart;
	git_oid h_oid;

	/* heart.png */
	cl_git_pass(git_oid_fromstrn(&h_oid, "de863bff", 8));
	cl_git_pass(git_blob_lookup_prefix(&heart, g_repo, &h_oid, 4));

	cl_git_pass(git_diff_blobs(
		alien, NULL, heart, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	assert_binary_blobs_comparison(&expected);

	memset(&expected, 0, sizeof(expected));

	cl_git_pass(git_diff_blobs(
		heart, NULL, alien, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	assert_binary_blobs_comparison(&expected);

	git_blob_free(heart);
}

void test_diff_blob__can_compare_a_binary_blob_and_a_text_blob(void)
{
	cl_git_pass(git_diff_blobs(
		alien, NULL, d, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	assert_binary_blobs_comparison(&expected);

	memset(&expected, 0, sizeof(expected));

	cl_git_pass(git_diff_blobs(
		d, NULL, alien, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	assert_binary_blobs_comparison(&expected);
}

/*
 * $ git diff fe773770 a0f7217
 * diff --git a/fe773770 b/a0f7217
 * index fe77377..a0f7217 100644
 * --- a/fe773770
 * +++ b/a0f7217
 * @@ -1,6 +1,6 @@
 *  Here is some stuff at the start
 * 
 * -This should go in one hunk
 * +This should go in one hunk (first)
 * 
 *  Some additional lines
 * 
 * @@ -8,7 +8,7 @@ Down here below the other lines
 * 
 *  With even more at the end
 * 
 * -Followed by a second hunk of stuff
 * +Followed by a second hunk of stuff (second)
 * 
 *  That happens down here
 */
void test_diff_blob__comparing_two_text_blobs_honors_interhunkcontext(void)
{
	git_blob *old_d;
	git_oid old_d_oid;

	opts.context_lines = 3;

	/* tests/resources/attr/root_test1 from commit f5b0af1 */
	cl_git_pass(git_oid_fromstrn(&old_d_oid, "fe773770", 8));
	cl_git_pass(git_blob_lookup_prefix(&old_d, g_repo, &old_d_oid, 4));

	/* Test with default inter-hunk-context (not set) => default is 0 */
	cl_git_pass(git_diff_blobs(
		old_d, NULL, d, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	cl_assert_equal_i(2, expected.hunks);

	/* Test with inter-hunk-context explicitly set to 0 */
	opts.interhunk_lines = 0;
	memset(&expected, 0, sizeof(expected));
	cl_git_pass(git_diff_blobs(
		old_d, NULL, d, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	cl_assert_equal_i(2, expected.hunks);

	/* Test with inter-hunk-context explicitly set to 1 */
	opts.interhunk_lines = 1;
	memset(&expected, 0, sizeof(expected));
	cl_git_pass(git_diff_blobs(
		old_d, NULL, d, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));

	cl_assert_equal_i(1, expected.hunks);

	git_blob_free(old_d);
}

void test_diff_blob__checks_options_version_too_low(void)
{
	const git_error *err;

	opts.version = 0;
	cl_git_fail(git_diff_blobs(
		d, NULL, alien, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
	err = giterr_last();
	cl_assert_equal_i(GITERR_INVALID, err->klass);
}

void test_diff_blob__checks_options_version_too_high(void)
{
	const git_error *err;

	opts.version = 1024;
	cl_git_fail(git_diff_blobs(
		d, NULL, alien, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
	err = giterr_last();
	cl_assert_equal_i(GITERR_INVALID, err->klass);
}

void test_diff_blob__can_correctly_detect_a_binary_blob_as_binary(void)
{
	/* alien.png */
	cl_assert_equal_i(true, git_blob_is_binary(alien));
}

void test_diff_blob__can_correctly_detect_a_textual_blob_as_non_binary(void)
{
	/* tests/resources/attr/root_test4.txt */
	cl_assert_equal_i(false, git_blob_is_binary(d));
}

/*
 * git_diff_blob_to_buffer tests
 */

static void assert_changed_single_one_line_file(
	diff_expects *expected, git_delta_t mod)
{
	cl_assert_equal_i(1, expected->files);
	cl_assert_equal_i(1, expected->file_status[mod]);
	cl_assert_equal_i(1, expected->hunks);
	cl_assert_equal_i(1, expected->lines);

	if (mod == GIT_DELTA_ADDED)
		cl_assert_equal_i(1, expected->line_adds);
	else if (mod == GIT_DELTA_DELETED)
		cl_assert_equal_i(1, expected->line_dels);
}

void test_diff_blob__can_compare_blob_to_buffer(void)
{
	git_blob *a;
	git_oid a_oid;
	const char *a_content = "Hello from the root\n";
	const char *b_content = "Hello from the root\n\nSome additional lines\n\nDown here below\n\n";

	/* tests/resources/attr/root_test1 */
	cl_git_pass(git_oid_fromstrn(&a_oid, "45141a79", 8));
	cl_git_pass(git_blob_lookup_prefix(&a, g_repo, &a_oid, 4));

	/* diff from blob a to content of b */
	quick_diff_blob_to_str(a, NULL, b_content, 0, NULL);
	assert_one_modified(1, 6, 1, 5, 0, &expected);

	/* diff from blob a to content of a */
	opts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;
	quick_diff_blob_to_str(a, NULL, a_content, 0, NULL);
	assert_identical_blobs_comparison(&expected);

	/* diff from NULL blob to content of a */
	memset(&expected, 0, sizeof(expected));
	quick_diff_blob_to_str(NULL, NULL, a_content, 0, NULL);
	assert_changed_single_one_line_file(&expected, GIT_DELTA_ADDED);

	/* diff from blob a to NULL buffer */
	memset(&expected, 0, sizeof(expected));
	quick_diff_blob_to_str(a, NULL, NULL, 0, NULL);
	assert_changed_single_one_line_file(&expected, GIT_DELTA_DELETED);

	/* diff with reverse */
	opts.flags ^= GIT_DIFF_REVERSE;

	memset(&expected, 0, sizeof(expected));
	quick_diff_blob_to_str(a, NULL, NULL, 0, NULL);
	assert_changed_single_one_line_file(&expected, GIT_DELTA_ADDED);

	git_blob_free(a);
}

void test_diff_blob__can_compare_blob_to_buffer_with_patch(void)
{
	git_patch *p;
	git_blob *a;
	git_oid a_oid;
	const char *a_content = "Hello from the root\n";
	const char *b_content = "Hello from the root\n\nSome additional lines\n\nDown here below\n\n";
	size_t tc, ta, td;

	/* tests/resources/attr/root_test1 */
	cl_git_pass(git_oid_fromstrn(&a_oid, "45141a79", 8));
	cl_git_pass(git_blob_lookup_prefix(&a, g_repo, &a_oid, 4));

	/* diff from blob a to content of b */
	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, a, NULL, b_content, strlen(b_content), NULL, &opts));

	cl_assert(p != NULL);
	cl_assert_equal_i(GIT_DELTA_MODIFIED, git_patch_get_delta(p)->status);
	cl_assert_equal_i(1, (int)git_patch_num_hunks(p));
	cl_assert_equal_i(6, git_patch_num_lines_in_hunk(p, 0));

	cl_git_pass(git_patch_line_stats(&tc, &ta, &td, p));
	cl_assert_equal_i(1, (int)tc);
	cl_assert_equal_i(5, (int)ta);
	cl_assert_equal_i(0, (int)td);

	git_patch_free(p);

	/* diff from blob a to content of a */
	opts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;
	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, a, NULL, a_content, strlen(a_content), NULL, &opts));
	cl_assert(p != NULL);
	cl_assert_equal_i(GIT_DELTA_UNMODIFIED, git_patch_get_delta(p)->status);
	cl_assert_equal_i(0, (int)git_patch_num_hunks(p));
	git_patch_free(p);

	/* diff from NULL blob to content of a */
	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, NULL, NULL, a_content, strlen(a_content), NULL, &opts));
	cl_assert(p != NULL);
	cl_assert_equal_i(GIT_DELTA_ADDED, git_patch_get_delta(p)->status);
	cl_assert_equal_i(1, (int)git_patch_num_hunks(p));
	cl_assert_equal_i(1, git_patch_num_lines_in_hunk(p, 0));
	git_patch_free(p);

	/* diff from blob a to NULL buffer */
	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, a, NULL, NULL, 0, NULL, &opts));
	cl_assert(p != NULL);
	cl_assert_equal_i(GIT_DELTA_DELETED, git_patch_get_delta(p)->status);
	cl_assert_equal_i(1, (int)git_patch_num_hunks(p));
	cl_assert_equal_i(1, git_patch_num_lines_in_hunk(p, 0));
	git_patch_free(p);

	/* diff with reverse */
	opts.flags ^= GIT_DIFF_REVERSE;

	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, a, NULL, NULL, 0, NULL, &opts));
	cl_assert(p != NULL);
	cl_assert_equal_i(GIT_DELTA_ADDED, git_patch_get_delta(p)->status);
	cl_assert_equal_i(1, (int)git_patch_num_hunks(p));
	cl_assert_equal_i(1, git_patch_num_lines_in_hunk(p, 0));
	git_patch_free(p);

	git_blob_free(a);
}

static void assert_one_modified_with_lines(diff_expects *expected, int lines)
{
	cl_assert_equal_i(1, expected->files);
	cl_assert_equal_i(1, expected->file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(0, expected->files_binary);
	cl_assert_equal_i(lines, expected->lines);
}

void test_diff_blob__binary_data_comparisons(void)
{
	git_blob *bin, *nonbin;
	git_oid oid;
	const char *nonbin_content = "Hello from the root\n";
	size_t nonbin_len = 20;
	const char *bin_content = "0123456789\n\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00\n0123456789\n";
	size_t bin_len = 33;

	opts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;

	cl_git_pass(git_oid_fromstrn(&oid, "45141a79", 8));
	cl_git_pass(git_blob_lookup_prefix(&nonbin, g_repo, &oid, 4));

	cl_git_pass(git_oid_fromstrn(&oid, "b435cd56", 8));
	cl_git_pass(git_blob_lookup_prefix(&bin, g_repo, &oid, 4));

	/* non-binary to reference content */

	quick_diff_blob_to_str(nonbin, NULL, nonbin_content, nonbin_len, NULL);
	assert_identical_blobs_comparison(&expected);
	cl_assert_equal_i(0, expected.files_binary);

	/* binary to reference content */

	quick_diff_blob_to_str(bin, NULL, bin_content, bin_len, NULL);
	assert_identical_blobs_comparison(&expected);

	cl_assert_equal_i(1, expected.files_binary);

	/* non-binary to binary content */

	quick_diff_blob_to_str(nonbin, NULL, bin_content, bin_len, NULL);
	assert_binary_blobs_comparison(&expected);

	/* binary to non-binary content */

	quick_diff_blob_to_str(bin, NULL, nonbin_content, nonbin_len, NULL);
	assert_binary_blobs_comparison(&expected);

	/* non-binary to binary blob */

	memset(&expected, 0, sizeof(expected));
	cl_git_pass(git_diff_blobs(
		bin, NULL, nonbin, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
	assert_binary_blobs_comparison(&expected);

	/*
	 * repeat with FORCE_TEXT
	 */

	opts.flags |= GIT_DIFF_FORCE_TEXT;

	quick_diff_blob_to_str(bin, NULL, bin_content, bin_len, NULL);
	assert_identical_blobs_comparison(&expected);

	quick_diff_blob_to_str(nonbin, NULL, bin_content, bin_len, NULL);
	assert_one_modified_with_lines(&expected, 4);

	quick_diff_blob_to_str(bin, NULL, nonbin_content, nonbin_len, NULL);
	assert_one_modified_with_lines(&expected, 4);

	memset(&expected, 0, sizeof(expected));
	cl_git_pass(git_diff_blobs(
		bin, NULL, nonbin, NULL, &opts,
		diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
	assert_one_modified_with_lines(&expected, 4);

	/* cleanup */
	git_blob_free(bin);
	git_blob_free(nonbin);
}

void test_diff_blob__using_path_and_attributes(void)
{
	git_config *cfg;
	git_blob *bin, *nonbin;
	git_oid oid;
	const char *nonbin_content = "Hello from the root\n";
	const char *bin_content =
		"0123456789\n\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00\n0123456789\n";
	size_t bin_len = 33;
	const char *changed;
	git_patch *p;
	git_buf buf = GIT_BUF_INIT;

	/* set up custom diff drivers and 'diff' attribute mappings for them */

	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_pass(git_config_set_bool(cfg, "diff.iam_binary.binary", 1));
	cl_git_pass(git_config_set_bool(cfg, "diff.iam_text.binary", 0));
	cl_git_pass(git_config_set_string(
		cfg, "diff.iam_alphactx.xfuncname", "^[A-Za-z].*$"));
	cl_git_pass(git_config_set_bool(cfg, "diff.iam_textalpha.binary", 0));
	cl_git_pass(git_config_set_string(
		cfg, "diff.iam_textalpha.xfuncname", "^[A-Za-z].*$"));
	cl_git_pass(git_config_set_string(
		cfg, "diff.iam_numctx.funcname", "^[0-9][0-9]*"));
	cl_git_pass(git_config_set_bool(cfg, "diff.iam_textnum.binary", 0));
	cl_git_pass(git_config_set_string(
		cfg, "diff.iam_textnum.funcname", "^[0-9][0-9]*"));
	git_config_free(cfg);

	cl_git_append2file(
		"attr/.gitattributes",
		"\n\n# test_diff_blob__using_path_and_attributes extra\n\n"
		"*.binary  diff=iam_binary\n"
		"*.textary diff=iam_text\n"
		"*.alphary diff=iam_alphactx\n"
		"*.textalphary diff=iam_textalpha\n"
		"*.textnumary diff=iam_textnum\n"
		"*.numary  diff=iam_numctx\n\n");

	opts.context_lines = 0;
	opts.flags |= GIT_DIFF_INCLUDE_UNMODIFIED;

	cl_git_pass(git_oid_fromstrn(&oid, "45141a79", 8));
	cl_git_pass(git_blob_lookup_prefix(&nonbin, g_repo, &oid, 4));
	/* 20b: "Hello from the root\n" */

	cl_git_pass(git_oid_fromstrn(&oid, "b435cd56", 8));
	cl_git_pass(git_blob_lookup_prefix(&bin, g_repo, &oid, 4));
	/* 33b: "0123456789\n\x01\x02\x03\x04\x05\x06\x07\x08\x09\n0123456789\n" */

	/* non-binary to reference content */

	quick_diff_blob_to_str(nonbin, NULL, nonbin_content, 0, NULL);
	assert_identical_blobs_comparison(&expected);
	cl_assert_equal_i(0, expected.files_binary);

	/* binary to reference content */

	quick_diff_blob_to_str(bin, NULL, bin_content, bin_len, NULL);
	assert_identical_blobs_comparison(&expected);
	cl_assert_equal_i(1, expected.files_binary);

	/* add some text */

	changed = "Hello from the root\nMore lines\nAnd more\nGo here\n";

	quick_diff_blob_to_str(nonbin, NULL, changed, 0, NULL);
	assert_one_modified(1, 3, 0, 3, 0, &expected);

	quick_diff_blob_to_str(nonbin, "foo/bar.binary", changed, 0, NULL);
	cl_assert_equal_i(1, expected.files);
	cl_assert_equal_i(1, expected.file_status[GIT_DELTA_MODIFIED]);
	cl_assert_equal_i(1, expected.files_binary);
	cl_assert_equal_i(0, expected.hunks);
	cl_assert_equal_i(0, expected.lines);

	quick_diff_blob_to_str(nonbin, "foo/bar.textary", changed, 0, NULL);
	assert_one_modified(1, 3, 0, 3, 0, &expected);

	quick_diff_blob_to_str(nonbin, "foo/bar.alphary", changed, 0, NULL);
	assert_one_modified(1, 3, 0, 3, 0, &expected);

	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, nonbin, "zzz.normal", changed, strlen(changed), NULL, &opts));
	cl_git_pass(git_patch_to_buf(&buf, p));
	cl_assert_equal_s(
		"diff --git a/zzz.normal b/zzz.normal\n"
		"index 45141a7..75b0dbb 100644\n"
		"--- a/zzz.normal\n"
		"+++ b/zzz.normal\n"
		"@@ -1,0 +2,3 @@ Hello from the root\n"
		"+More lines\n"
		"+And more\n"
		"+Go here\n", buf.ptr);
	git_buf_clear(&buf);
	git_patch_free(p);

	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, nonbin, "zzz.binary", changed, strlen(changed), NULL, &opts));
	cl_git_pass(git_patch_to_buf(&buf, p));
	cl_assert_equal_s(
		"diff --git a/zzz.binary b/zzz.binary\n"
		"index 45141a7..75b0dbb 100644\n"
		"Binary files a/zzz.binary and b/zzz.binary differ\n", buf.ptr);
	git_buf_clear(&buf);
	git_patch_free(p);

	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, nonbin, "zzz.alphary", changed, strlen(changed), NULL, &opts));
	cl_git_pass(git_patch_to_buf(&buf, p));
	cl_assert_equal_s(
		"diff --git a/zzz.alphary b/zzz.alphary\n"
		"index 45141a7..75b0dbb 100644\n"
		"--- a/zzz.alphary\n"
		"+++ b/zzz.alphary\n"
		"@@ -1,0 +2,3 @@ Hello from the root\n"
		"+More lines\n"
		"+And more\n"
		"+Go here\n", buf.ptr);
	git_buf_clear(&buf);
	git_patch_free(p);

	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, nonbin, "zzz.numary", changed, strlen(changed), NULL, &opts));
	cl_git_pass(git_patch_to_buf(&buf, p));
	cl_assert_equal_s(
		"diff --git a/zzz.numary b/zzz.numary\n"
		"index 45141a7..75b0dbb 100644\n"
		"--- a/zzz.numary\n"
		"+++ b/zzz.numary\n"
		"@@ -1,0 +2,3 @@\n"
		"+More lines\n"
		"+And more\n"
		"+Go here\n", buf.ptr);
	git_buf_clear(&buf);
	git_patch_free(p);

	/* "0123456789\n\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00\n0123456789\n"
	 * 33 bytes
	 */

	changed = "0123456789\n\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00\nreplace a line\n";

	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, bin, "zzz.normal", changed, 37, NULL, &opts));
	cl_git_pass(git_patch_to_buf(&buf, p));
	cl_assert_equal_s(
		"diff --git a/zzz.normal b/zzz.normal\n"
		"index b435cd5..1604519 100644\n"
		"Binary files a/zzz.normal and b/zzz.normal differ\n", buf.ptr);
	git_buf_clear(&buf);
	git_patch_free(p);

	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, bin, "zzz.textary", changed, 37, NULL, &opts));
	cl_git_pass(git_patch_to_buf(&buf, p));
	cl_assert_equal_s(
		"diff --git a/zzz.textary b/zzz.textary\n"
		"index b435cd5..1604519 100644\n"
		"--- a/zzz.textary\n"
		"+++ b/zzz.textary\n"
		"@@ -3 +3 @@\n"
		"-0123456789\n"
		"+replace a line\n", buf.ptr);
	git_buf_clear(&buf);
	git_patch_free(p);

	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, bin, "zzz.textalphary", changed, 37, NULL, &opts));
	cl_git_pass(git_patch_to_buf(&buf, p));
	cl_assert_equal_s(
		"diff --git a/zzz.textalphary b/zzz.textalphary\n"
		"index b435cd5..1604519 100644\n"
		"--- a/zzz.textalphary\n"
		"+++ b/zzz.textalphary\n"
		"@@ -3 +3 @@\n"
		"-0123456789\n"
		"+replace a line\n", buf.ptr);
	git_buf_clear(&buf);
	git_patch_free(p);

	cl_git_pass(git_patch_from_blob_and_buffer(
		&p, bin, "zzz.textnumary", changed, 37, NULL, &opts));
	cl_git_pass(git_patch_to_buf(&buf, p));
	cl_assert_equal_s(
		"diff --git a/zzz.textnumary b/zzz.textnumary\n"
		"index b435cd5..1604519 100644\n"
		"--- a/zzz.textnumary\n"
		"+++ b/zzz.textnumary\n"
		"@@ -3 +3 @@ 0123456789\n"
		"-0123456789\n"
		"+replace a line\n", buf.ptr);
	git_buf_clear(&buf);
	git_patch_free(p);

	git_buf_free(&buf);
	git_blob_free(nonbin);
	git_blob_free(bin);
}

void test_diff_blob__can_compare_buffer_to_buffer(void)
{
	const char *a = "a\nb\nc\nd\ne\nf\ng\nh\ni\nj\n";
	const char *b = "a\nB\nc\nd\nE\nF\nh\nj\nk\n";

	opts.interhunk_lines = 0;
	opts.context_lines = 0;

	memset(&expected, 0, sizeof(expected));

	cl_git_pass(git_diff_buffers(
		a, strlen(a), NULL, b, strlen(b), NULL,
		&opts, diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
	assert_one_modified(4, 9, 0, 4, 5, &expected);

	opts.flags ^= GIT_DIFF_REVERSE;

	memset(&expected, 0, sizeof(expected));

	cl_git_pass(git_diff_buffers(
		a, strlen(a), NULL, b, strlen(b), NULL,
		&opts, diff_file_cb, diff_hunk_cb, diff_line_cb, &expected));
	assert_one_modified(4, 9, 0, 5, 4, &expected);
}
