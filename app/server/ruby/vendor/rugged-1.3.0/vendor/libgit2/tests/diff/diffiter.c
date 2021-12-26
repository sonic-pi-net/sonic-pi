#include "clar_libgit2.h"
#include "diff_helpers.h"

void test_diff_diffiter__initialize(void)
{
}

void test_diff_diffiter__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_diff_diffiter__create(void)
{
	git_repository *repo = cl_git_sandbox_init("attr");
	git_diff *diff;
	size_t d, num_d;

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, NULL));

	num_d = git_diff_num_deltas(diff);
	for (d = 0; d < num_d; ++d) {
		const git_diff_delta *delta = git_diff_get_delta(diff, d);
		cl_assert(delta != NULL);
	}

	cl_assert(!git_diff_get_delta(diff, num_d));

	git_diff_free(diff);
}

void test_diff_diffiter__iterate_files_1(void)
{
	git_repository *repo = cl_git_sandbox_init("attr");
	git_diff *diff;
	size_t d, num_d;
	diff_expects exp = { 0 };

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, NULL));

	num_d = git_diff_num_deltas(diff);

	for (d = 0; d < num_d; ++d) {
		const git_diff_delta *delta = git_diff_get_delta(diff, d);
		cl_assert(delta != NULL);

		diff_file_cb(delta, (float)d / (float)num_d, &exp);
	}
	cl_assert_equal_sz(6, exp.files);

	git_diff_free(diff);
}

void test_diff_diffiter__iterate_files_2(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_diff *diff;
	size_t d, num_d;
	int count = 0;

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, NULL));

	num_d = git_diff_num_deltas(diff);
	cl_assert_equal_i(8, (int)num_d);

	for (d = 0; d < num_d; ++d) {
		const git_diff_delta *delta = git_diff_get_delta(diff, d);
		cl_assert(delta != NULL);
		count++;
	}
	cl_assert_equal_i(8, count);

	git_diff_free(diff);
}

void test_diff_diffiter__iterate_files_and_hunks(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	size_t d, num_d;
	int file_count = 0, hunk_count = 0;

	opts.context_lines = 3;
	opts.interhunk_lines = 1;
	opts.flags |= GIT_DIFF_INCLUDE_IGNORED | GIT_DIFF_INCLUDE_UNTRACKED;

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, &opts));

	num_d = git_diff_num_deltas(diff);

	for (d = 0; d < num_d; ++d) {
		git_patch *patch;
		size_t h, num_h;

		cl_git_pass(git_patch_from_diff(&patch, diff, d));
		cl_assert(patch);

		file_count++;

		num_h = git_patch_num_hunks(patch);

		for (h = 0; h < num_h; h++) {
			const git_diff_hunk *hunk;

			cl_git_pass(git_patch_get_hunk(&hunk, NULL, patch, h));
			cl_assert(hunk);

			hunk_count++;
		}

		git_patch_free(patch);
	}

	cl_assert_equal_i(13, file_count);
	cl_assert_equal_i(8, hunk_count);

	git_diff_free(diff);
}

void test_diff_diffiter__max_size_threshold(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	int file_count = 0, binary_count = 0, hunk_count = 0;
	size_t d, num_d;

	opts.context_lines = 3;
	opts.interhunk_lines = 1;
	opts.flags |= GIT_DIFF_INCLUDE_IGNORED | GIT_DIFF_INCLUDE_UNTRACKED;

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, &opts));
	num_d = git_diff_num_deltas(diff);

	for (d = 0; d < num_d; ++d) {
		git_patch *patch;
		const git_diff_delta *delta;

		cl_git_pass(git_patch_from_diff(&patch, diff, d));
		cl_assert(patch);
		delta = git_patch_get_delta(patch);
		cl_assert(delta);

		file_count++;
		hunk_count += (int)git_patch_num_hunks(patch);

		assert((delta->flags & (GIT_DIFF_FLAG_BINARY|GIT_DIFF_FLAG_NOT_BINARY)) != 0);
		binary_count += ((delta->flags & GIT_DIFF_FLAG_BINARY) != 0);

		git_patch_free(patch);
	}

	cl_assert_equal_i(13, file_count);
	cl_assert_equal_i(0, binary_count);
	cl_assert_equal_i(8, hunk_count);

	git_diff_free(diff);

	/* try again with low file size threshold */

	file_count = binary_count = hunk_count = 0;

	opts.context_lines = 3;
	opts.interhunk_lines = 1;
	opts.flags |= GIT_DIFF_INCLUDE_IGNORED | GIT_DIFF_INCLUDE_UNTRACKED;
	opts.max_size = 50; /* treat anything over 50 bytes as binary! */

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, &opts));
	num_d = git_diff_num_deltas(diff);

	for (d = 0; d < num_d; ++d) {
		git_patch *patch;
		const git_diff_delta *delta;

		cl_git_pass(git_patch_from_diff(&patch, diff, d));
		delta = git_patch_get_delta(patch);

		file_count++;
		hunk_count += (int)git_patch_num_hunks(patch);

		assert((delta->flags & (GIT_DIFF_FLAG_BINARY|GIT_DIFF_FLAG_NOT_BINARY)) != 0);
		binary_count += ((delta->flags & GIT_DIFF_FLAG_BINARY) != 0);

		git_patch_free(patch);
	}

	cl_assert_equal_i(13, file_count);
	/* Three files are over the 50 byte threshold:
	 * - staged_changes_file_deleted
	 * - staged_changes_modified_file
	 * - staged_new_file_modified_file
	 */
	cl_assert_equal_i(3, binary_count);
	cl_assert_equal_i(5, hunk_count);

	git_diff_free(diff);
}


void test_diff_diffiter__iterate_all(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	diff_expects exp = {0};
	size_t d, num_d;

	opts.context_lines = 3;
	opts.interhunk_lines = 1;
	opts.flags |= GIT_DIFF_INCLUDE_IGNORED | GIT_DIFF_INCLUDE_UNTRACKED;

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, &opts));

	num_d = git_diff_num_deltas(diff);
	for (d = 0; d < num_d; ++d) {
		git_patch *patch;
		size_t h, num_h;

		cl_git_pass(git_patch_from_diff(&patch, diff, d));
		cl_assert(patch);
		exp.files++;

		num_h = git_patch_num_hunks(patch);
		for (h = 0; h < num_h; h++) {
			const git_diff_hunk *range;
			size_t l, num_l;

			cl_git_pass(git_patch_get_hunk(&range, &num_l, patch, h));
			cl_assert(range);
			exp.hunks++;

			for (l = 0; l < num_l; ++l) {
				const git_diff_line *line;

				cl_git_pass(git_patch_get_line_in_hunk(&line, patch, h, l));
				cl_assert(line && line->content);
				exp.lines++;
			}
		}

		git_patch_free(patch);
	}

	cl_assert_equal_i(13, exp.files);
	cl_assert_equal_i(8, exp.hunks);
	cl_assert_equal_i(14, exp.lines);

	git_diff_free(diff);
}

static void iterate_over_patch(git_patch *patch, diff_expects *exp)
{
	size_t h, num_h = git_patch_num_hunks(patch), num_l;

	exp->files++;
	exp->hunks += (int)num_h;

	/* let's iterate in reverse, just because we can! */
	for (h = 1, num_l = 0; h <= num_h; ++h)
		num_l += git_patch_num_lines_in_hunk(patch, num_h - h);

	exp->lines += (int)num_l;
}

#define PATCH_CACHE 5

void test_diff_diffiter__iterate_randomly_while_saving_state(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	diff_expects exp = {0};
	git_patch *patches[PATCH_CACHE];
	size_t p, d, num_d;

	memset(patches, 0, sizeof(patches));

	opts.context_lines = 3;
	opts.interhunk_lines = 1;
	opts.flags |= GIT_DIFF_INCLUDE_IGNORED | GIT_DIFF_INCLUDE_UNTRACKED;

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, &opts));

	num_d = git_diff_num_deltas(diff);

	/* To make sure that references counts work for diff and patch objects,
	 * this generates patches and randomly caches them.  Only when the patch
	 * is removed from the cache are hunks and lines counted.  At the end,
	 * there are still patches in the cache, so free the diff and try to
	 * process remaining patches after the diff is freed.
	 */

	srand(121212);
	p = rand() % PATCH_CACHE;

	for (d = 0; d < num_d; ++d) {
		/* take old patch */
		git_patch *patch = patches[p];
		patches[p] = NULL;

		/* cache new patch */
		cl_git_pass(git_patch_from_diff(&patches[p], diff, d));
		cl_assert(patches[p] != NULL);

		/* process old patch if non-NULL */
		if (patch != NULL) {
			iterate_over_patch(patch, &exp);
			git_patch_free(patch);
		}

		p = rand() % PATCH_CACHE;
	}

	/* free diff list now - refcounts should keep things safe */
	git_diff_free(diff);

	/* process remaining unprocessed patches */
	for (p = 0; p < PATCH_CACHE; p++) {
		git_patch *patch = patches[p];

		if (patch != NULL) {
			iterate_over_patch(patch, &exp);
			git_patch_free(patch);
		}
	}

	/* hopefully it all still added up right */
	cl_assert_equal_i(13, exp.files);
	cl_assert_equal_i(8, exp.hunks);
	cl_assert_equal_i(14, exp.lines);
}

/* This output is taken directly from `git diff` on the status test data */
static const char *expected_patch_text[8] = {
	/* 0 */
	"diff --git a/file_deleted b/file_deleted\n"
	"deleted file mode 100644\n"
	"index 5452d32..0000000\n"
	"--- a/file_deleted\n"
	"+++ /dev/null\n"
	"@@ -1 +0,0 @@\n"
	"-file_deleted\n",
	/* 1 */
	"diff --git a/modified_file b/modified_file\n"
	"index 452e424..0a53963 100644\n"
	"--- a/modified_file\n"
	"+++ b/modified_file\n"
	"@@ -1 +1,2 @@\n"
	" modified_file\n"
	"+modified_file\n",
	/* 2 */
	"diff --git a/staged_changes_file_deleted b/staged_changes_file_deleted\n"
	"deleted file mode 100644\n"
	"index a6be623..0000000\n"
	"--- a/staged_changes_file_deleted\n"
	"+++ /dev/null\n"
	"@@ -1,2 +0,0 @@\n"
	"-staged_changes_file_deleted\n"
	"-staged_changes_file_deleted\n",
	/* 3 */
	"diff --git a/staged_changes_modified_file b/staged_changes_modified_file\n"
	"index 906ee77..011c344 100644\n"
	"--- a/staged_changes_modified_file\n"
	"+++ b/staged_changes_modified_file\n"
	"@@ -1,2 +1,3 @@\n"
	" staged_changes_modified_file\n"
	" staged_changes_modified_file\n"
	"+staged_changes_modified_file\n",
	/* 4 */
	"diff --git a/staged_new_file_deleted_file b/staged_new_file_deleted_file\n"
	"deleted file mode 100644\n"
	"index 90b8c29..0000000\n"
	"--- a/staged_new_file_deleted_file\n"
	"+++ /dev/null\n"
	"@@ -1 +0,0 @@\n"
	"-staged_new_file_deleted_file\n",
	/* 5 */
	"diff --git a/staged_new_file_modified_file b/staged_new_file_modified_file\n"
	"index ed06290..8b090c0 100644\n"
	"--- a/staged_new_file_modified_file\n"
	"+++ b/staged_new_file_modified_file\n"
	"@@ -1 +1,2 @@\n"
	" staged_new_file_modified_file\n"
	"+staged_new_file_modified_file\n",
	/* 6 */
	"diff --git a/subdir/deleted_file b/subdir/deleted_file\n"
	"deleted file mode 100644\n"
	"index 1888c80..0000000\n"
	"--- a/subdir/deleted_file\n"
	"+++ /dev/null\n"
	"@@ -1 +0,0 @@\n"
	"-subdir/deleted_file\n",
	/* 7 */
	"diff --git a/subdir/modified_file b/subdir/modified_file\n"
	"index a619198..57274b7 100644\n"
	"--- a/subdir/modified_file\n"
	"+++ b/subdir/modified_file\n"
	"@@ -1 +1,2 @@\n"
	" subdir/modified_file\n"
	"+subdir/modified_file\n"
};

void test_diff_diffiter__iterate_and_generate_patch_text(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_diff *diff;
	size_t d, num_d;

	cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, NULL));

	num_d = git_diff_num_deltas(diff);
	cl_assert_equal_i(8, (int)num_d);

	for (d = 0; d < num_d; ++d) {
		git_patch *patch;
		git_buf buf = GIT_BUF_INIT;

		cl_git_pass(git_patch_from_diff(&patch, diff, d));
		cl_assert(patch != NULL);

		cl_git_pass(git_patch_to_buf(&buf, patch));

		cl_assert_equal_s(expected_patch_text[d], buf.ptr);

		git_buf_dispose(&buf);
		git_patch_free(patch);
	}

	git_diff_free(diff);
}

void test_diff_diffiter__checks_options_version(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	const git_error *err;

	opts.version = 0;
	opts.flags |= GIT_DIFF_INCLUDE_IGNORED | GIT_DIFF_INCLUDE_UNTRACKED;

	cl_git_fail(git_diff_index_to_workdir(&diff, repo, NULL, &opts));
	err = git_error_last();
	cl_assert_equal_i(GIT_ERROR_INVALID, err->klass);

	git_error_clear();
	opts.version = 1024;
	cl_git_fail(git_diff_index_to_workdir(&diff, repo, NULL, &opts));
	err = git_error_last();
	cl_assert_equal_i(GIT_ERROR_INVALID, err->klass);
}

