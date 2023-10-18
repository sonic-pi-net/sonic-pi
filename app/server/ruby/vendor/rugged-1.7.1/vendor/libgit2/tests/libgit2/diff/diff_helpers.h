#include "futils.h"
#include "git2/diff.h"

extern git_tree *resolve_commit_oid_to_tree(
	git_repository *repo, const char *partial_oid);

typedef struct {
	int files;
	int files_binary;

	int file_status[11]; /* indexed by git_delta_t value */

	int hunks;
	int hunk_new_lines;
	int hunk_old_lines;

	int lines;
	int line_ctxt;
	int line_adds;
	int line_dels;

	/* optional arrays of expected specific values */
	const char **names;
	int *statuses;

	int debug;

} diff_expects;

typedef struct {
	const char *path;
	const char *matched_pathspec;
} notify_expected;

extern int diff_file_cb(
	const git_diff_delta *delta,
	float progress,
	void *cb_data);

extern int diff_print_file_cb(
	const git_diff_delta *delta,
	float progress,
	void *cb_data);

extern int diff_binary_cb(
	const git_diff_delta *delta,
	const git_diff_binary *binary,
	void *cb_data);

extern int diff_hunk_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	void *cb_data);

extern int diff_line_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *cb_data);

extern int diff_foreach_via_iterator(
	git_diff *diff,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb line_cb,
	void *data);

extern void diff_print(FILE *fp, git_diff *diff);
extern void diff_print_raw(FILE *fp, git_diff *diff);

extern void diff_assert_equal(git_diff *a, git_diff *b);

extern int diff_from_buffer(
	git_diff **out,
	const char *content,
	size_t content_len);
