/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"
#include "diff.h"
#include "diff_patch.h"
#include "fileops.h"
#include "zstream.h"
#include "blob.h"
#include "delta.h"
#include "git2/sys/diff.h"

typedef struct {
	git_diff *diff;
	git_diff_format_t format;
	git_diff_line_cb print_cb;
	void *payload;
	git_buf *buf;
	uint32_t flags;
	int oid_strlen;
	git_diff_line line;
} diff_print_info;

static int diff_print_info_init(
	diff_print_info *pi,
	git_buf *out,
	git_diff *diff,
	git_diff_format_t format,
	git_diff_line_cb cb,
	void *payload)
{
	pi->diff     = diff;
	pi->format   = format;
	pi->print_cb = cb;
	pi->payload  = payload;
	pi->buf      = out;

	if (diff)
		pi->flags = diff->opts.flags;
	else
		pi->flags = 0;

	if (diff && diff->opts.id_abbrev != 0)
		pi->oid_strlen = diff->opts.id_abbrev;
	else if (!diff || !diff->repo)
		pi->oid_strlen = GIT_ABBREV_DEFAULT;
	else if (git_repository__cvar(
		&pi->oid_strlen, diff->repo, GIT_CVAR_ABBREV) < 0)
		return -1;

	pi->oid_strlen += 1; /* for NUL byte */

	if (pi->oid_strlen < 2)
		pi->oid_strlen = 2;
	else if (pi->oid_strlen > GIT_OID_HEXSZ + 1)
		pi->oid_strlen = GIT_OID_HEXSZ + 1;

	memset(&pi->line, 0, sizeof(pi->line));
	pi->line.old_lineno = -1;
	pi->line.new_lineno = -1;
	pi->line.num_lines  = 1;

	return 0;
}

static char diff_pick_suffix(int mode)
{
	if (S_ISDIR(mode))
		return '/';
	else if (GIT_PERMS_IS_EXEC(mode)) /* -V536 */
		/* in git, modes are very regular, so we must have 0100755 mode */
		return '*';
	else
		return ' ';
}

char git_diff_status_char(git_delta_t status)
{
	char code;

	switch (status) {
	case GIT_DELTA_ADDED:     code = 'A'; break;
	case GIT_DELTA_DELETED:   code = 'D'; break;
	case GIT_DELTA_MODIFIED:  code = 'M'; break;
	case GIT_DELTA_RENAMED:   code = 'R'; break;
	case GIT_DELTA_COPIED:    code = 'C'; break;
	case GIT_DELTA_IGNORED:   code = 'I'; break;
	case GIT_DELTA_UNTRACKED: code = '?'; break;
	default:                  code = ' '; break;
	}

	return code;
}

static int diff_print_one_name_only(
	const git_diff_delta *delta, float progress, void *data)
{
	diff_print_info *pi = data;
	git_buf *out = pi->buf;

	GIT_UNUSED(progress);

	if ((pi->flags & GIT_DIFF_SHOW_UNMODIFIED) == 0 &&
		delta->status == GIT_DELTA_UNMODIFIED)
		return 0;

	git_buf_clear(out);
	git_buf_puts(out, delta->new_file.path);
	git_buf_putc(out, '\n');
	if (git_buf_oom(out))
		return -1;

	pi->line.origin      = GIT_DIFF_LINE_FILE_HDR;
	pi->line.content     = git_buf_cstr(out);
	pi->line.content_len = git_buf_len(out);

	return pi->print_cb(delta, NULL, &pi->line, pi->payload);
}

static int diff_print_one_name_status(
	const git_diff_delta *delta, float progress, void *data)
{
	diff_print_info *pi = data;
	git_buf *out = pi->buf;
	char old_suffix, new_suffix, code = git_diff_status_char(delta->status);
	int (*strcomp)(const char *, const char *) =
		pi->diff ? pi->diff->strcomp : git__strcmp;

	GIT_UNUSED(progress);

	if ((pi->flags & GIT_DIFF_SHOW_UNMODIFIED) == 0 && code == ' ')
		return 0;

	old_suffix = diff_pick_suffix(delta->old_file.mode);
	new_suffix = diff_pick_suffix(delta->new_file.mode);

	git_buf_clear(out);

	if (delta->old_file.path != delta->new_file.path &&
		strcomp(delta->old_file.path,delta->new_file.path) != 0)
		git_buf_printf(out, "%c\t%s%c %s%c\n", code,
			delta->old_file.path, old_suffix, delta->new_file.path, new_suffix);
	else if (delta->old_file.mode != delta->new_file.mode &&
		delta->old_file.mode != 0 && delta->new_file.mode != 0)
		git_buf_printf(out, "%c\t%s%c %s%c\n", code,
			delta->old_file.path, old_suffix, delta->new_file.path, new_suffix);
	else if (old_suffix != ' ')
		git_buf_printf(out, "%c\t%s%c\n", code, delta->old_file.path, old_suffix);
	else
		git_buf_printf(out, "%c\t%s\n", code, delta->old_file.path);
	if (git_buf_oom(out))
		return -1;

	pi->line.origin      = GIT_DIFF_LINE_FILE_HDR;
	pi->line.content     = git_buf_cstr(out);
	pi->line.content_len = git_buf_len(out);

	return pi->print_cb(delta, NULL, &pi->line, pi->payload);
}

static int diff_print_one_raw(
	const git_diff_delta *delta, float progress, void *data)
{
	diff_print_info *pi = data;
	git_buf *out = pi->buf;
	char code = git_diff_status_char(delta->status);
	char start_oid[GIT_OID_HEXSZ+1], end_oid[GIT_OID_HEXSZ+1];

	GIT_UNUSED(progress);

	if ((pi->flags & GIT_DIFF_SHOW_UNMODIFIED) == 0 && code == ' ')
		return 0;

	git_buf_clear(out);

	git_oid_tostr(start_oid, pi->oid_strlen, &delta->old_file.id);
	git_oid_tostr(end_oid, pi->oid_strlen, &delta->new_file.id);

	git_buf_printf(
		out, (pi->oid_strlen <= GIT_OID_HEXSZ) ?
			":%06o %06o %s... %s... %c" : ":%06o %06o %s %s %c",
		delta->old_file.mode, delta->new_file.mode, start_oid, end_oid, code);

	if (delta->similarity > 0)
		git_buf_printf(out, "%03u", delta->similarity);

	if (delta->old_file.path != delta->new_file.path)
		git_buf_printf(
			out, "\t%s %s\n", delta->old_file.path, delta->new_file.path);
	else
		git_buf_printf(
			out, "\t%s\n", delta->old_file.path ?
			delta->old_file.path : delta->new_file.path);

	if (git_buf_oom(out))
		return -1;

	pi->line.origin      = GIT_DIFF_LINE_FILE_HDR;
	pi->line.content     = git_buf_cstr(out);
	pi->line.content_len = git_buf_len(out);

	return pi->print_cb(delta, NULL, &pi->line, pi->payload);
}

static int diff_print_oid_range(
	git_buf *out, const git_diff_delta *delta, int oid_strlen)
{
	char start_oid[GIT_OID_HEXSZ+1], end_oid[GIT_OID_HEXSZ+1];

	git_oid_tostr(start_oid, oid_strlen, &delta->old_file.id);
	git_oid_tostr(end_oid, oid_strlen, &delta->new_file.id);

	/* TODO: Match git diff more closely */
	if (delta->old_file.mode == delta->new_file.mode) {
		git_buf_printf(out, "index %s..%s %o\n",
			start_oid, end_oid, delta->old_file.mode);
	} else {
		if (delta->old_file.mode == 0) {
			git_buf_printf(out, "new file mode %o\n", delta->new_file.mode);
		} else if (delta->new_file.mode == 0) {
			git_buf_printf(out, "deleted file mode %o\n", delta->old_file.mode);
		} else {
			git_buf_printf(out, "old mode %o\n", delta->old_file.mode);
			git_buf_printf(out, "new mode %o\n", delta->new_file.mode);
		}
		git_buf_printf(out, "index %s..%s\n", start_oid, end_oid);
	}

	return git_buf_oom(out) ? -1 : 0;
}

static int diff_delta_format_with_paths(
	git_buf *out,
	const git_diff_delta *delta,
	const char *oldpfx,
	const char *newpfx,
	const char *template)
{
	const char *oldpath = delta->old_file.path;
	const char *newpath = delta->new_file.path;

	if (git_oid_iszero(&delta->old_file.id)) {
		oldpfx = "";
		oldpath = "/dev/null";
	}
	if (git_oid_iszero(&delta->new_file.id)) {
		newpfx = "";
		newpath = "/dev/null";
	}

	return git_buf_printf(out, template, oldpfx, oldpath, newpfx, newpath);
}

int git_diff_delta__format_file_header(
	git_buf *out,
	const git_diff_delta *delta,
	const char *oldpfx,
	const char *newpfx,
	int oid_strlen)
{
	if (!oldpfx)
		oldpfx = DIFF_OLD_PREFIX_DEFAULT;
	if (!newpfx)
		newpfx = DIFF_NEW_PREFIX_DEFAULT;
	if (!oid_strlen)
		oid_strlen = GIT_ABBREV_DEFAULT + 1;

	git_buf_clear(out);

	git_buf_printf(out, "diff --git %s%s %s%s\n",
		oldpfx, delta->old_file.path, newpfx, delta->new_file.path);

	GITERR_CHECK_ERROR(diff_print_oid_range(out, delta, oid_strlen));

	if ((delta->flags & GIT_DIFF_FLAG_BINARY) == 0)
		diff_delta_format_with_paths(
			out, delta, oldpfx, newpfx, "--- %s%s\n+++ %s%s\n");

	return git_buf_oom(out) ? -1 : 0;
}

static int print_binary_hunk(diff_print_info *pi, git_blob *old, git_blob *new)
{
	git_buf deflate = GIT_BUF_INIT, delta = GIT_BUF_INIT, *out = NULL;
	const void *old_data, *new_data;
	git_off_t old_data_len, new_data_len;
	unsigned long delta_data_len, inflated_len;
	const char *out_type = "literal";
	char *scan, *end;
	int error;

	old_data = old ? git_blob_rawcontent(old) : NULL;
	new_data = new ? git_blob_rawcontent(new) : NULL;

	old_data_len = old ? git_blob_rawsize(old) : 0;
	new_data_len = new ? git_blob_rawsize(new) : 0;

	/* The git_delta function accepts unsigned long only */
	if (!git__is_ulong(old_data_len) || !git__is_ulong(new_data_len))
		return GIT_EBUFS;

	out = &deflate;
	inflated_len = (unsigned long)new_data_len;

	if ((error = git_zstream_deflatebuf(
			out, new_data, (size_t)new_data_len)) < 0)
		goto done;

	/* The git_delta function accepts unsigned long only */
	if (!git__is_ulong((git_off_t)deflate.size)) {
		error = GIT_EBUFS;
		goto done;
	}

	if (old && new) {
		void *delta_data = git_delta(
			old_data, (unsigned long)old_data_len,
			new_data, (unsigned long)new_data_len,
			&delta_data_len, (unsigned long)deflate.size);

		if (delta_data) {
			error = git_zstream_deflatebuf(
				&delta, delta_data, (size_t)delta_data_len);

			git__free(delta_data);

			if (error < 0)
				goto done;

			if (delta.size < deflate.size) {
				out = &delta;
				out_type = "delta";
				inflated_len = delta_data_len;
			}
		}
	}

	git_buf_printf(pi->buf, "%s %lu\n", out_type, inflated_len);
	pi->line.num_lines++;

	for (scan = out->ptr, end = out->ptr + out->size; scan < end; ) {
		size_t chunk_len = end - scan;
		if (chunk_len > 52)
			chunk_len = 52;

		if (chunk_len <= 26)
			git_buf_putc(pi->buf, (char)chunk_len + 'A' - 1);
		else
			git_buf_putc(pi->buf, (char)chunk_len - 26 + 'a' - 1);

		git_buf_put_base85(pi->buf, scan, chunk_len);
		git_buf_putc(pi->buf, '\n');

		if (git_buf_oom(pi->buf)) {
			error = -1;
			goto done;
		}

		scan += chunk_len;
		pi->line.num_lines++;
	}

done:
	git_buf_free(&deflate);
	git_buf_free(&delta);

	return error;
}

/* git diff --binary 8d7523f~2 8d7523f~1 */
static int diff_print_patch_file_binary(
	diff_print_info *pi, const git_diff_delta *delta,
	const char *oldpfx, const char *newpfx)
{
	git_blob *old = NULL, *new = NULL;
	const git_oid *old_id, *new_id;
	int error;
	size_t pre_binary_size;

	if ((pi->flags & GIT_DIFF_SHOW_BINARY) == 0)
		goto noshow;

	pre_binary_size = pi->buf->size;
	git_buf_printf(pi->buf, "GIT binary patch\n");
	pi->line.num_lines++;

	old_id = (delta->status != GIT_DELTA_ADDED) ? &delta->old_file.id : NULL;
	new_id = (delta->status != GIT_DELTA_DELETED) ? &delta->new_file.id : NULL;

	if (old_id && (error = git_blob_lookup(&old, pi->diff->repo, old_id)) < 0)
		goto done;
	if (new_id && (error = git_blob_lookup(&new, pi->diff->repo,new_id)) < 0)
		goto done;

	if ((error = print_binary_hunk(pi, old, new)) < 0 ||
		(error = git_buf_putc(pi->buf, '\n')) < 0 ||
		(error = print_binary_hunk(pi, new, old)) < 0)
	{
		if (error == GIT_EBUFS) {
			giterr_clear();
			git_buf_truncate(pi->buf, pre_binary_size);
			goto noshow;
		}
	}

	pi->line.num_lines++;

done:
	git_blob_free(old);
	git_blob_free(new);

	return error;

noshow:
	pi->line.num_lines = 1;
	return diff_delta_format_with_paths(
		pi->buf, delta, oldpfx, newpfx,
		"Binary files %s%s and %s%s differ\n");
}

static int diff_print_patch_file(
	const git_diff_delta *delta, float progress, void *data)
{
	int error;
	diff_print_info *pi = data;
	const char *oldpfx =
		pi->diff ? pi->diff->opts.old_prefix : DIFF_OLD_PREFIX_DEFAULT;
	const char *newpfx =
		pi->diff ? pi->diff->opts.new_prefix : DIFF_NEW_PREFIX_DEFAULT;

	bool binary = !!(delta->flags & GIT_DIFF_FLAG_BINARY);
	bool show_binary = !!(pi->flags & GIT_DIFF_SHOW_BINARY);
	int oid_strlen = binary && show_binary ?
		GIT_OID_HEXSZ + 1 : pi->oid_strlen;

	GIT_UNUSED(progress);

	if (S_ISDIR(delta->new_file.mode) ||
		delta->status == GIT_DELTA_UNMODIFIED ||
		delta->status == GIT_DELTA_IGNORED ||
		(delta->status == GIT_DELTA_UNTRACKED &&
		 (pi->flags & GIT_DIFF_SHOW_UNTRACKED_CONTENT) == 0))
		return 0;

	if ((error = git_diff_delta__format_file_header(
			pi->buf, delta, oldpfx, newpfx, oid_strlen)) < 0)
		return error;

	pi->line.origin      = GIT_DIFF_LINE_FILE_HDR;
	pi->line.content     = git_buf_cstr(pi->buf);
	pi->line.content_len = git_buf_len(pi->buf);

	if ((error = pi->print_cb(delta, NULL, &pi->line, pi->payload)) != 0)
		return error;

	if (!binary)
		return 0;

	git_buf_clear(pi->buf);

	if ((error = diff_print_patch_file_binary(pi, delta, oldpfx, newpfx)) < 0)
		return error;

	pi->line.origin      = GIT_DIFF_LINE_BINARY;
	pi->line.content     = git_buf_cstr(pi->buf);
	pi->line.content_len = git_buf_len(pi->buf);

	return pi->print_cb(delta, NULL, &pi->line, pi->payload);
}

static int diff_print_patch_hunk(
	const git_diff_delta *d,
	const git_diff_hunk *h,
	void *data)
{
	diff_print_info *pi = data;

	if (S_ISDIR(d->new_file.mode))
		return 0;

	pi->line.origin      = GIT_DIFF_LINE_HUNK_HDR;
	pi->line.content     = h->header;
	pi->line.content_len = h->header_len;

	return pi->print_cb(d, h, &pi->line, pi->payload);
}

static int diff_print_patch_line(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *data)
{
	diff_print_info *pi = data;

	if (S_ISDIR(delta->new_file.mode))
		return 0;

	return pi->print_cb(delta, hunk, line, pi->payload);
}

/* print a git_diff to an output callback */
int git_diff_print(
	git_diff *diff,
	git_diff_format_t format,
	git_diff_line_cb print_cb,
	void *payload)
{
	int error;
	git_buf buf = GIT_BUF_INIT;
	diff_print_info pi;
	git_diff_file_cb print_file = NULL;
	git_diff_hunk_cb print_hunk = NULL;
	git_diff_line_cb print_line = NULL;

	switch (format) {
	case GIT_DIFF_FORMAT_PATCH:
		print_file = diff_print_patch_file;
		print_hunk = diff_print_patch_hunk;
		print_line = diff_print_patch_line;
		break;
	case GIT_DIFF_FORMAT_PATCH_HEADER:
		print_file = diff_print_patch_file;
		break;
	case GIT_DIFF_FORMAT_RAW:
		print_file = diff_print_one_raw;
		break;
	case GIT_DIFF_FORMAT_NAME_ONLY:
		print_file = diff_print_one_name_only;
		break;
	case GIT_DIFF_FORMAT_NAME_STATUS:
		print_file = diff_print_one_name_status;
		break;
	default:
		giterr_set(GITERR_INVALID, "Unknown diff output format (%d)", format);
		return -1;
	}

	if (!(error = diff_print_info_init(
			&pi, &buf, diff, format, print_cb, payload)))
	{
		error = git_diff_foreach(
			diff, print_file, print_hunk, print_line, &pi);

		if (error) /* make sure error message is set */
			giterr_set_after_callback_function(error, "git_diff_print");
	}

	git_buf_free(&buf);

	return error;
}

/* print a git_patch to an output callback */
int git_patch_print(
	git_patch *patch,
	git_diff_line_cb print_cb,
	void *payload)
{
	int error;
	git_buf temp = GIT_BUF_INIT;
	diff_print_info pi;

	assert(patch && print_cb);

	if (!(error = diff_print_info_init(
			&pi, &temp, git_patch__diff(patch),
			GIT_DIFF_FORMAT_PATCH, print_cb, payload)))
	{
		error = git_patch__invoke_callbacks(
			patch, diff_print_patch_file, diff_print_patch_hunk,
			diff_print_patch_line, &pi);

		if (error) /* make sure error message is set */
			giterr_set_after_callback_function(error, "git_patch_print");
	}

	git_buf_free(&temp);

	return error;
}

int git_diff_print_callback__to_buf(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *payload)
{
	git_buf *output = payload;
	GIT_UNUSED(delta); GIT_UNUSED(hunk);

	if (!output) {
		giterr_set(GITERR_INVALID, "Buffer pointer must be provided");
		return -1;
	}

	if (line->origin == GIT_DIFF_LINE_ADDITION ||
		line->origin == GIT_DIFF_LINE_DELETION ||
		line->origin == GIT_DIFF_LINE_CONTEXT)
		git_buf_putc(output, line->origin);

	return git_buf_put(output, line->content, line->content_len);
}

int git_diff_print_callback__to_file_handle(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk,
	const git_diff_line *line,
	void *payload)
{
	FILE *fp = payload ? payload : stdout;

	GIT_UNUSED(delta); GIT_UNUSED(hunk);

	if (line->origin == GIT_DIFF_LINE_CONTEXT ||
		line->origin == GIT_DIFF_LINE_ADDITION ||
		line->origin == GIT_DIFF_LINE_DELETION)
		fputc(line->origin, fp);
	fwrite(line->content, 1, line->content_len, fp);
	return 0;
}

/* print a git_patch to a git_buf */
int git_patch_to_buf(git_buf *out, git_patch *patch)
{
	assert(out && patch);
	git_buf_sanitize(out);
	return git_patch_print(patch, git_diff_print_callback__to_buf, out);
}
