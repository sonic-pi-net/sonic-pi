#include "clar_libgit2.h"
#include "diff_helpers.h"
#include "git2/sys/diff.h"

git_tree *resolve_commit_oid_to_tree(
	git_repository *repo,
	const char *partial_oid)
{
	size_t len = strlen(partial_oid);
	git_oid oid;
	git_object *obj = NULL;
	git_tree *tree = NULL;

	if (git_oid_fromstrn(&oid, partial_oid, len) == 0)
		cl_git_pass(git_object_lookup_prefix(&obj, repo, &oid, len, GIT_OBJ_ANY));

	cl_git_pass(git_object_peel((git_object **) &tree, obj, GIT_OBJ_TREE));
	git_object_free(obj);
	return tree;
}

static char diff_pick_suffix(int mode)
{
	if (S_ISDIR(mode))
		return '/';
	else if (GIT_PERMS_IS_EXEC(mode))
		return '*';
	else
		return ' ';
}

static void fprintf_delta(FILE *fp, const git_diff_delta *delta, float progress)
{
	char code = git_diff_status_char(delta->status);
	char old_suffix = diff_pick_suffix(delta->old_file.mode);
	char new_suffix = diff_pick_suffix(delta->new_file.mode);

	fprintf(fp, "%c\t%s", code, delta->old_file.path);

	if ((delta->old_file.path != delta->new_file.path &&
		 strcmp(delta->old_file.path, delta->new_file.path) != 0) ||
		(delta->old_file.mode != delta->new_file.mode &&
		 delta->old_file.mode != 0 && delta->new_file.mode != 0))
		fprintf(fp, "%c %s%c", old_suffix, delta->new_file.path, new_suffix);
	else if (old_suffix != ' ')
		fprintf(fp, "%c", old_suffix);

	fprintf(fp, "\t[%.2f]\n", progress);
}

int diff_file_cb(
	const git_diff_delta *delta,
	float progress,
	void *payload)
{
	diff_expects *e = payload;

	if (e->debug)
		fprintf_delta(stderr, delta, progress);

	if (e->names)
		cl_assert_equal_s(e->names[e->files], delta->old_file.path);
	if (e->statuses)
		cl_assert_equal_i(e->statuses[e->files], (int)delta->status);

	e->files++;

	if ((delta->flags & GIT_DIFF_FLAG_BINARY) != 0)
		e->files_binary++;

	cl_assert(delta->status <= GIT_DELTA_CONFLICTED);

	e->file_status[delta->status] += 1;

	return 0;
}

int diff_print_file_cb(
	const git_diff_delta *delta,
	float progress,
	void *payload)
{
	if (!payload) {
		fprintf_delta(stderr, delta, progress);
		return 0;
	}

	if (!((diff_expects *)payload)->debug)
		fprintf_delta(stderr, delta, progress);

	return diff_file_cb(delta, progress, payload);
}

int diff_binary_cb(
	const git_diff_delta *delta,
	const git_diff_binary *binary,
	void *payload)
{
	GIT_UNUSED(delta);
	GIT_UNUSED(binary);
	GIT_UNUSED(payload);

	return 0;
}

int diff_hunk_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	void *payload)
{
	diff_expects *e = payload;
	const char *scan = hunk->header, *scan_end = scan + hunk->header_len;

	GIT_UNUSED(delta);

	/* confirm no NUL bytes in header text */
	while (scan < scan_end)
		cl_assert('\0' != *scan++);

	e->hunks++;
	e->hunk_old_lines += hunk->old_lines;
	e->hunk_new_lines += hunk->new_lines;
	return 0;
}

int diff_line_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *payload)
{
	diff_expects *e = payload;

	GIT_UNUSED(delta);
	GIT_UNUSED(hunk);

	e->lines++;
	switch (line->origin) {
	case GIT_DIFF_LINE_CONTEXT:
	case GIT_DIFF_LINE_CONTEXT_EOFNL: /* techically not a line */
		e->line_ctxt++;
		break;
	case GIT_DIFF_LINE_ADDITION:
	case GIT_DIFF_LINE_ADD_EOFNL: /* technically not a line add */
		e->line_adds++;
		break;
	case GIT_DIFF_LINE_DELETION:
	case GIT_DIFF_LINE_DEL_EOFNL: /* technically not a line delete */
		e->line_dels++;
		break;
	default:
		break;
	}
	return 0;
}

int diff_foreach_via_iterator(
	git_diff *diff,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb line_cb,
	void *data)
{
	size_t d, num_d = git_diff_num_deltas(diff);

	GIT_UNUSED(binary_cb);

	for (d = 0; d < num_d; ++d) {
		git_patch *patch;
		const git_diff_delta *delta;
		size_t h, num_h;

		cl_git_pass(git_patch_from_diff(&patch, diff, d));
		cl_assert((delta = git_patch_get_delta(patch)) != NULL);

		/* call file_cb for this file */
		if (file_cb != NULL && file_cb(delta, (float)d / num_d, data) != 0) {
			git_patch_free(patch);
			goto abort;
		}

		/* if there are no changes, then the patch will be NULL */
		if (!patch) {
			cl_assert(delta->status == GIT_DELTA_UNMODIFIED ||
					  (delta->flags & GIT_DIFF_FLAG_BINARY) != 0);
			continue;
		}

		if (!hunk_cb && !line_cb) {
			git_patch_free(patch);
			continue;
		}

		num_h = git_patch_num_hunks(patch);

		for (h = 0; h < num_h; h++) {
			const git_diff_hunk *hunk;
			size_t l, num_l;

			cl_git_pass(git_patch_get_hunk(&hunk, &num_l, patch, h));

			if (hunk_cb && hunk_cb(delta, hunk, data) != 0) {
				git_patch_free(patch);
				goto abort;
			}

			for (l = 0; l < num_l; ++l) {
				const git_diff_line *line;

				cl_git_pass(git_patch_get_line_in_hunk(&line, patch, h, l));

				if (line_cb &&
					line_cb(delta, hunk, line, data) != 0) {
					git_patch_free(patch);
					goto abort;
				}
			}
		}

		git_patch_free(patch);
	}

	return 0;

abort:
	giterr_clear();
	return GIT_EUSER;
}

void diff_print(FILE *fp, git_diff *diff)
{
	cl_git_pass(
		git_diff_print(diff, GIT_DIFF_FORMAT_PATCH,
			git_diff_print_callback__to_file_handle, fp ? fp : stderr));
}

void diff_print_raw(FILE *fp, git_diff *diff)
{
	cl_git_pass(
		git_diff_print(diff, GIT_DIFF_FORMAT_RAW,
			git_diff_print_callback__to_file_handle, fp ? fp : stderr));
}
