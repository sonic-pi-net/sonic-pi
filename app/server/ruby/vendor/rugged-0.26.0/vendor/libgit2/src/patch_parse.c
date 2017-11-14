/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "patch_parse.h"

#include "git2/patch.h"
#include "patch.h"
#include "diff_parse.h"
#include "path.h"

#define parse_err(...) \
	( giterr_set(GITERR_PATCH, __VA_ARGS__), -1 )

typedef struct {
	git_patch base;

	git_patch_parse_ctx *ctx;

	/* the paths from the `diff --git` header, these will be used if this is not
	 * a rename (and rename paths are specified) or if no `+++`/`---` line specify
	 * the paths.
	 */
	char *header_old_path, *header_new_path;

	/* renamed paths are precise and are not prefixed */
	char *rename_old_path, *rename_new_path;

	/* the paths given in `---` and `+++` lines */
	char *old_path, *new_path;

	/* the prefixes from the old/new paths */
	char *old_prefix, *new_prefix;
} git_patch_parsed;


GIT_INLINE(bool) parse_ctx_contains(
	git_patch_parse_ctx *ctx, const char *str, size_t len)
{
	return (ctx->line_len >= len && memcmp(ctx->line, str, len) == 0);
}

#define parse_ctx_contains_s(ctx, str) \
	parse_ctx_contains(ctx, str, sizeof(str) - 1)

static void parse_advance_line(git_patch_parse_ctx *ctx)
{
	ctx->line += ctx->line_len;
	ctx->remain_len -= ctx->line_len;
	ctx->line_len = git__linenlen(ctx->line, ctx->remain_len);
	ctx->line_num++;
}

static void parse_advance_chars(git_patch_parse_ctx *ctx, size_t char_cnt)
{
	ctx->line += char_cnt;
	ctx->remain_len -= char_cnt;
	ctx->line_len -= char_cnt;
}

static int parse_advance_expected(
	git_patch_parse_ctx *ctx,
	const char *expected,
	size_t expected_len)
{
	if (ctx->line_len < expected_len)
		return -1;

	if (memcmp(ctx->line, expected, expected_len) != 0)
		return -1;

	parse_advance_chars(ctx, expected_len);
	return 0;
}

#define parse_advance_expected_str(ctx, str) \
	parse_advance_expected(ctx, str, strlen(str))

static int parse_advance_ws(git_patch_parse_ctx *ctx)
{
	int ret = -1;

	while (ctx->line_len > 0 &&
		ctx->line[0] != '\n' &&
		git__isspace(ctx->line[0])) {
		ctx->line++;
		ctx->line_len--;
		ctx->remain_len--;
		ret = 0;
	}

	return ret;
}

static int parse_advance_nl(git_patch_parse_ctx *ctx)
{
	if (ctx->line_len != 1 || ctx->line[0] != '\n')
		return -1;

	parse_advance_line(ctx);
	return 0;
}

static int header_path_len(git_patch_parse_ctx *ctx)
{
	bool inquote = 0;
	bool quoted = (ctx->line_len > 0 && ctx->line[0] == '"');
	size_t len;

	for (len = quoted; len < ctx->line_len; len++) {
		if (!quoted && git__isspace(ctx->line[len]))
			break;
		else if (quoted && !inquote && ctx->line[len] == '"') {
			len++;
			break;
		}

		inquote = (!inquote && ctx->line[len] == '\\');
	}

	return len;
}

static int parse_header_path_buf(git_buf *path, git_patch_parse_ctx *ctx)
{
	int path_len, error = 0;

	path_len = header_path_len(ctx);

	if ((error = git_buf_put(path, ctx->line, path_len)) < 0)
		goto done;

	parse_advance_chars(ctx, path_len);

	git_buf_rtrim(path);

	if (path->size > 0 && path->ptr[0] == '"')
		error = git_buf_unquote(path);

	if (error < 0)
		goto done;

	git_path_squash_slashes(path);

done:
	return error;
}

static int parse_header_path(char **out, git_patch_parse_ctx *ctx)
{
	git_buf path = GIT_BUF_INIT;
	int error = parse_header_path_buf(&path, ctx);

	*out = git_buf_detach(&path);

	return error;
}

static int parse_header_git_oldpath(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	return parse_header_path(&patch->old_path, ctx);
}

static int parse_header_git_newpath(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	return parse_header_path(&patch->new_path, ctx);
}

static int parse_header_mode(uint16_t *mode, git_patch_parse_ctx *ctx)
{
	const char *end;
	int32_t m;
	int ret;

	if (ctx->line_len < 1 || !git__isdigit(ctx->line[0]))
		return parse_err("invalid file mode at line %"PRIuZ, ctx->line_num);

	if ((ret = git__strntol32(&m, ctx->line, ctx->line_len, &end, 8)) < 0)
		return ret;

	if (m > UINT16_MAX)
		return -1;

	*mode = (uint16_t)m;

	parse_advance_chars(ctx, (end - ctx->line));

	return ret;
}

static int parse_header_oid(
	git_oid *oid,
	uint16_t *oid_len,
	git_patch_parse_ctx *ctx)
{
	size_t len;

	for (len = 0; len < ctx->line_len && len < GIT_OID_HEXSZ; len++) {
		if (!git__isxdigit(ctx->line[len]))
			break;
	}

	if (len < GIT_OID_MINPREFIXLEN || len > GIT_OID_HEXSZ ||
		git_oid_fromstrn(oid, ctx->line, len) < 0)
		return parse_err("invalid hex formatted object id at line %"PRIuZ,
			ctx->line_num);

	parse_advance_chars(ctx, len);

	*oid_len = (uint16_t)len;

	return 0;
}

static int parse_header_git_index(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	if (parse_header_oid(&patch->base.delta->old_file.id,
			&patch->base.delta->old_file.id_abbrev, ctx) < 0 ||
		parse_advance_expected_str(ctx, "..") < 0 ||
		parse_header_oid(&patch->base.delta->new_file.id,
			&patch->base.delta->new_file.id_abbrev, ctx) < 0)
		return -1;

	if (ctx->line_len > 0 && ctx->line[0] == ' ') {
		uint16_t mode;

		parse_advance_chars(ctx, 1);

		if (parse_header_mode(&mode, ctx) < 0)
			return -1;

		if (!patch->base.delta->new_file.mode)
			patch->base.delta->new_file.mode = mode;

		if (!patch->base.delta->old_file.mode)
			patch->base.delta->old_file.mode = mode;
	}

	return 0;
}

static int parse_header_git_oldmode(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	return parse_header_mode(&patch->base.delta->old_file.mode, ctx);
}

static int parse_header_git_newmode(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	return parse_header_mode(&patch->base.delta->new_file.mode, ctx);
}

static int parse_header_git_deletedfilemode(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	git__free((char *)patch->base.delta->old_file.path);

	patch->base.delta->old_file.path = NULL;
	patch->base.delta->status = GIT_DELTA_DELETED;
	patch->base.delta->nfiles = 1;

	return parse_header_mode(&patch->base.delta->old_file.mode, ctx);
}

static int parse_header_git_newfilemode(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	git__free((char *)patch->base.delta->new_file.path);

	patch->base.delta->new_file.path = NULL;
	patch->base.delta->status = GIT_DELTA_ADDED;
	patch->base.delta->nfiles = 1;

	return parse_header_mode(&patch->base.delta->new_file.mode, ctx);
}

static int parse_header_rename(
	char **out,
	git_patch_parse_ctx *ctx)
{
	git_buf path = GIT_BUF_INIT;

	if (parse_header_path_buf(&path, ctx) < 0)
		return -1;

	/* Note: the `rename from` and `rename to` lines include the literal
	 * filename.  They do *not* include the prefix.  (Who needs consistency?)
	 */
	*out = git_buf_detach(&path);
	return 0;
}

static int parse_header_renamefrom(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	patch->base.delta->status = GIT_DELTA_RENAMED;
	return parse_header_rename(&patch->rename_old_path, ctx);
}

static int parse_header_renameto(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	patch->base.delta->status = GIT_DELTA_RENAMED;
	return parse_header_rename(&patch->rename_new_path, ctx);
}

static int parse_header_copyfrom(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	patch->base.delta->status = GIT_DELTA_COPIED;
	return parse_header_rename(&patch->rename_old_path, ctx);
}

static int parse_header_copyto(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	patch->base.delta->status = GIT_DELTA_COPIED;
	return parse_header_rename(&patch->rename_new_path, ctx);
}

static int parse_header_percent(uint16_t *out, git_patch_parse_ctx *ctx)
{
	int32_t val;
	const char *end;

	if (ctx->line_len < 1 || !git__isdigit(ctx->line[0]) ||
		git__strntol32(&val, ctx->line, ctx->line_len, &end, 10) < 0)
		return -1;

	parse_advance_chars(ctx, (end - ctx->line));

	if (parse_advance_expected_str(ctx, "%") < 0)
		return -1;

	if (val > 100)
		return -1;

	*out = val;
	return 0;
}

static int parse_header_similarity(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	if (parse_header_percent(&patch->base.delta->similarity, ctx) < 0)
		return parse_err("invalid similarity percentage at line %"PRIuZ,
			ctx->line_num);

	return 0;
}

static int parse_header_dissimilarity(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	uint16_t dissimilarity;

	if (parse_header_percent(&dissimilarity, ctx) < 0)
		return parse_err("invalid similarity percentage at line %"PRIuZ,
			ctx->line_num);

	patch->base.delta->similarity = 100 - dissimilarity;

	return 0;
}

typedef struct {
	const char *str;
	int(*fn)(git_patch_parsed *, git_patch_parse_ctx *);
} header_git_op;

static const header_git_op header_git_ops[] = {
	{ "diff --git ", NULL },
	{ "@@ -", NULL },
	{ "GIT binary patch", NULL },
	{ "Binary files ", NULL },
	{ "--- ", parse_header_git_oldpath },
	{ "+++ ", parse_header_git_newpath },
	{ "index ", parse_header_git_index },
	{ "old mode ", parse_header_git_oldmode },
	{ "new mode ", parse_header_git_newmode },
	{ "deleted file mode ", parse_header_git_deletedfilemode },
	{ "new file mode ", parse_header_git_newfilemode },
	{ "rename from ", parse_header_renamefrom },
	{ "rename to ", parse_header_renameto },
	{ "rename old ", parse_header_renamefrom },
	{ "rename new ", parse_header_renameto },
	{ "copy from ", parse_header_copyfrom },
	{ "copy to ", parse_header_copyto },
	{ "similarity index ", parse_header_similarity },
	{ "dissimilarity index ", parse_header_dissimilarity },
};

static int parse_header_git(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	size_t i;
	int error = 0;

	/* Parse the diff --git line */
	if (parse_advance_expected_str(ctx, "diff --git ") < 0)
		return parse_err("corrupt git diff header at line %"PRIuZ, ctx->line_num);

	if (parse_header_path(&patch->header_old_path, ctx) < 0)
		return parse_err("corrupt old path in git diff header at line %"PRIuZ,
			ctx->line_num);

	if (parse_advance_ws(ctx) < 0 ||
		parse_header_path(&patch->header_new_path, ctx) < 0)
		return parse_err("corrupt new path in git diff header at line %"PRIuZ,
			ctx->line_num);

	/* Parse remaining header lines */
	for (parse_advance_line(ctx);
		ctx->remain_len > 0;
		parse_advance_line(ctx)) {

		bool found = false;

		if (ctx->line_len == 0 || ctx->line[ctx->line_len - 1] != '\n')
			break;

		for (i = 0; i < ARRAY_SIZE(header_git_ops); i++) {
			const header_git_op *op = &header_git_ops[i];
			size_t op_len = strlen(op->str);

			if (memcmp(ctx->line, op->str, min(op_len, ctx->line_len)) != 0)
				continue;

			/* Do not advance if this is the patch separator */
			if (op->fn == NULL)
				goto done;

			parse_advance_chars(ctx, op_len);

			if ((error = op->fn(patch, ctx)) < 0)
				goto done;

			parse_advance_ws(ctx);

			if (parse_advance_expected_str(ctx, "\n") < 0 ||
			    ctx->line_len > 0) {
				error = parse_err("trailing data at line %"PRIuZ, ctx->line_num);
				goto done;
			}

			found = true;
			break;
		}
		
		if (!found) {
			error = parse_err("invalid patch header at line %"PRIuZ,
				ctx->line_num);
			goto done;
		}
	}

done:
	return error;
}

static int parse_number(git_off_t *out, git_patch_parse_ctx *ctx)
{
	const char *end;
	int64_t num;

	if (!git__isdigit(ctx->line[0]))
		return -1;

	if (git__strntol64(&num, ctx->line, ctx->line_len, &end, 10) < 0)
		return -1;

	if (num < 0)
		return -1;

	*out = num;
	parse_advance_chars(ctx, (end - ctx->line));

	return 0;
}

static int parse_int(int *out, git_patch_parse_ctx *ctx)
{
	git_off_t num;

	if (parse_number(&num, ctx) < 0 || !git__is_int(num))
		return -1;

	*out = (int)num;
	return 0;
}

static int parse_hunk_header(
	git_patch_hunk *hunk,
	git_patch_parse_ctx *ctx)
{
	const char *header_start = ctx->line;

	hunk->hunk.old_lines = 1;
	hunk->hunk.new_lines = 1;

	if (parse_advance_expected_str(ctx, "@@ -") < 0 ||
		parse_int(&hunk->hunk.old_start, ctx) < 0)
		goto fail;

	if (ctx->line_len > 0 && ctx->line[0] == ',') {
		if (parse_advance_expected_str(ctx, ",") < 0 ||
			parse_int(&hunk->hunk.old_lines, ctx) < 0)
			goto fail;
	}

	if (parse_advance_expected_str(ctx, " +") < 0 ||
		parse_int(&hunk->hunk.new_start, ctx) < 0)
		goto fail;

	if (ctx->line_len > 0 && ctx->line[0] == ',') {
		if (parse_advance_expected_str(ctx, ",") < 0 ||
			parse_int(&hunk->hunk.new_lines, ctx) < 0)
			goto fail;
	}

	if (parse_advance_expected_str(ctx, " @@") < 0)
		goto fail;

	parse_advance_line(ctx);

	if (!hunk->hunk.old_lines && !hunk->hunk.new_lines)
		goto fail;

	hunk->hunk.header_len = ctx->line - header_start;
	if (hunk->hunk.header_len > (GIT_DIFF_HUNK_HEADER_SIZE - 1))
		return parse_err("oversized patch hunk header at line %"PRIuZ,
			ctx->line_num);

	memcpy(hunk->hunk.header, header_start, hunk->hunk.header_len);
	hunk->hunk.header[hunk->hunk.header_len] = '\0';

	return 0;

fail:
	giterr_set(GITERR_PATCH, "invalid patch hunk header at line %"PRIuZ,
		ctx->line_num);
	return -1;
}

static int parse_hunk_body(
	git_patch_parsed *patch,
	git_patch_hunk *hunk,
	git_patch_parse_ctx *ctx)
{
	git_diff_line *line;
	int error = 0;

	int oldlines = hunk->hunk.old_lines;
	int newlines = hunk->hunk.new_lines;

	for (;
		ctx->remain_len > 1 &&
		(oldlines || newlines) &&
		(ctx->remain_len <= 4 || memcmp(ctx->line, "@@ -", 4) != 0);
		parse_advance_line(ctx)) {

		int origin;
		int prefix = 1;

		if (ctx->line_len == 0 || ctx->line[ctx->line_len - 1] != '\n') {
			error = parse_err("invalid patch instruction at line %"PRIuZ,
				ctx->line_num);
			goto done;
		}

		switch (ctx->line[0]) {
		case '\n':
			prefix = 0;

		case ' ':
			origin = GIT_DIFF_LINE_CONTEXT;
			oldlines--;
			newlines--;
			break;

		case '-':
			origin = GIT_DIFF_LINE_DELETION;
			oldlines--;
			break;

		case '+':
			origin = GIT_DIFF_LINE_ADDITION;
			newlines--;
			break;

		default:
			error = parse_err("invalid patch hunk at line %"PRIuZ, ctx->line_num);
			goto done;
		}

		line = git_array_alloc(patch->base.lines);
		GITERR_CHECK_ALLOC(line);

		memset(line, 0x0, sizeof(git_diff_line));

		line->content = ctx->line + prefix;
		line->content_len = ctx->line_len - prefix;
		line->content_offset = ctx->content_len - ctx->remain_len;
		line->origin = origin;

		hunk->line_count++;
	}

	if (oldlines || newlines) {
		error = parse_err(
			"invalid patch hunk, expected %d old lines and %d new lines",
			hunk->hunk.old_lines, hunk->hunk.new_lines);
		goto done;
	}

	/* Handle "\ No newline at end of file".  Only expect the leading
	 * backslash, though, because the rest of the string could be
	 * localized.  Because `diff` optimizes for the case where you
	 * want to apply the patch by hand.
	 */
	if (parse_ctx_contains_s(ctx, "\\ ") &&
		git_array_size(patch->base.lines) > 0) {

		line = git_array_get(patch->base.lines, git_array_size(patch->base.lines) - 1);

		if (line->content_len < 1) {
			error = parse_err("cannot trim trailing newline of empty line");
			goto done;
		}

		line->content_len--;

		parse_advance_line(ctx);
	}

done:
	return error;
}

static int parse_patch_header(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	int error = 0;

	for (ctx->line = ctx->remain;
		ctx->remain_len > 0;
		parse_advance_line(ctx)) {

		/* This line is too short to be a patch header. */
		if (ctx->line_len < 6)
			continue;

		/* This might be a hunk header without a patch header, provide a
		 * sensible error message. */
		if (parse_ctx_contains_s(ctx, "@@ -")) {
			size_t line_num = ctx->line_num;
			git_patch_hunk hunk;

			/* If this cannot be parsed as a hunk header, it's just leading
			* noise, continue.
			*/
			if (parse_hunk_header(&hunk, ctx) < 0) {
				giterr_clear();
				continue;
			}

			error = parse_err("invalid hunk header outside patch at line %"PRIuZ,
				line_num);
			goto done;
		}

		/* This buffer is too short to contain a patch. */
		if (ctx->remain_len < ctx->line_len + 6)
			break;

		/* A proper git patch */
		if (parse_ctx_contains_s(ctx, "diff --git ")) {
			error = parse_header_git(patch, ctx);
			goto done;
		}

		error = 0;
		continue;
	}

	giterr_set(GITERR_PATCH, "no patch found");
	error = GIT_ENOTFOUND;

done:
	return error;
}

static int parse_patch_binary_side(
	git_diff_binary_file *binary,
	git_patch_parse_ctx *ctx)
{
	git_diff_binary_t type = GIT_DIFF_BINARY_NONE;
	git_buf base85 = GIT_BUF_INIT, decoded = GIT_BUF_INIT;
	git_off_t len;
	int error = 0;

	if (parse_ctx_contains_s(ctx, "literal ")) {
		type = GIT_DIFF_BINARY_LITERAL;
		parse_advance_chars(ctx, 8);
	} else if (parse_ctx_contains_s(ctx, "delta ")) {
		type = GIT_DIFF_BINARY_DELTA;
		parse_advance_chars(ctx, 6);
	} else {
		error = parse_err(
			"unknown binary delta type at line %"PRIuZ, ctx->line_num);
		goto done;
	}

	if (parse_number(&len, ctx) < 0 || parse_advance_nl(ctx) < 0 || len < 0) {
		error = parse_err("invalid binary size at line %"PRIuZ, ctx->line_num);
		goto done;
	}

	while (ctx->line_len) {
		char c = ctx->line[0];
		size_t encoded_len, decoded_len = 0, decoded_orig = decoded.size;

		if (c == '\n')
			break;
		else if (c >= 'A' && c <= 'Z')
			decoded_len = c - 'A' + 1;
		else if (c >= 'a' && c <= 'z')
			decoded_len = c - 'a' + (('z' - 'a') + 1) + 1;

		if (!decoded_len) {
			error = parse_err("invalid binary length at line %"PRIuZ, ctx->line_num);
			goto done;
		}

		parse_advance_chars(ctx, 1);

		encoded_len = ((decoded_len / 4) + !!(decoded_len % 4)) * 5;

		if (encoded_len > ctx->line_len - 1) {
			error = parse_err("truncated binary data at line %"PRIuZ, ctx->line_num);
			goto done;
		}

		if ((error = git_buf_decode_base85(
			&decoded, ctx->line, encoded_len, decoded_len)) < 0)
			goto done;

		if (decoded.size - decoded_orig != decoded_len) {
			error = parse_err("truncated binary data at line %"PRIuZ, ctx->line_num);
			goto done;
		}

		parse_advance_chars(ctx, encoded_len);

		if (parse_advance_nl(ctx) < 0) {
			error = parse_err("trailing data at line %"PRIuZ, ctx->line_num);
			goto done;
		}
	}

	binary->type = type;
	binary->inflatedlen = (size_t)len;
	binary->datalen = decoded.size;
	binary->data = git_buf_detach(&decoded);

done:
	git_buf_free(&base85);
	git_buf_free(&decoded);
	return error;
}

static int parse_patch_binary(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	int error;

	if (parse_advance_expected_str(ctx, "GIT binary patch") < 0 ||
		parse_advance_nl(ctx) < 0)
		return parse_err("corrupt git binary header at line %"PRIuZ, ctx->line_num);

	/* parse old->new binary diff */
	if ((error = parse_patch_binary_side(
			&patch->base.binary.new_file, ctx)) < 0)
		return error;

	if (parse_advance_nl(ctx) < 0)
		return parse_err("corrupt git binary separator at line %"PRIuZ,
			ctx->line_num);

	/* parse new->old binary diff */
	if ((error = parse_patch_binary_side(
			&patch->base.binary.old_file, ctx)) < 0)
		return error;

	if (parse_advance_nl(ctx) < 0)
		return parse_err("corrupt git binary patch separator at line %"PRIuZ,
			ctx->line_num);

	patch->base.binary.contains_data = 1;
	patch->base.delta->flags |= GIT_DIFF_FLAG_BINARY;
	return 0;
}

static int parse_patch_binary_nodata(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	if (parse_advance_expected_str(ctx, "Binary files ") < 0 ||
		parse_advance_expected_str(ctx, patch->header_old_path) < 0 ||
		parse_advance_expected_str(ctx, " and ") < 0 ||
		parse_advance_expected_str(ctx, patch->header_new_path) < 0 ||
		parse_advance_expected_str(ctx, " differ") < 0 ||
		parse_advance_nl(ctx) < 0)
		return parse_err("corrupt git binary header at line %"PRIuZ, ctx->line_num);

	patch->base.binary.contains_data = 0;
	patch->base.delta->flags |= GIT_DIFF_FLAG_BINARY;
	return 0;
}

static int parse_patch_hunks(
	git_patch_parsed *patch,
	git_patch_parse_ctx *ctx)
{
	git_patch_hunk *hunk;
	int error = 0;

	while (parse_ctx_contains_s(ctx, "@@ -")) {
		hunk = git_array_alloc(patch->base.hunks);
		GITERR_CHECK_ALLOC(hunk);

		memset(hunk, 0, sizeof(git_patch_hunk));

		hunk->line_start = git_array_size(patch->base.lines);
		hunk->line_count = 0;

		if ((error = parse_hunk_header(hunk, ctx)) < 0 ||
			(error = parse_hunk_body(patch, hunk, ctx)) < 0)
			goto done;
	}

	patch->base.delta->flags |= GIT_DIFF_FLAG_NOT_BINARY;

done:
	return error;
}

static int parse_patch_body(
	git_patch_parsed *patch, git_patch_parse_ctx *ctx)
{
	if (parse_ctx_contains_s(ctx, "GIT binary patch"))
		return parse_patch_binary(patch, ctx);
	else if (parse_ctx_contains_s(ctx, "Binary files "))
		return parse_patch_binary_nodata(patch, ctx);
	else
		return parse_patch_hunks(patch, ctx);
}

int check_header_names(
	const char *one,
	const char *two,
	const char *old_or_new,
	bool two_null)
{
	if (!one || !two)
		return 0;

	if (two_null && strcmp(two, "/dev/null") != 0)
		return parse_err("expected %s path of '/dev/null'", old_or_new);

	else if (!two_null && strcmp(one, two) != 0)
		return parse_err("mismatched %s path names", old_or_new);

	return 0;
}

static int check_prefix(
	char **out,
	size_t *out_len,
	git_patch_parsed *patch,
	const char *path_start)
{
	const char *path = path_start;
	size_t prefix_len = patch->ctx->opts.prefix_len;
	size_t remain_len = prefix_len;

	*out = NULL;
	*out_len = 0;

	if (prefix_len == 0)
		goto done;

	/* leading slashes do not count as part of the prefix in git apply */
	while (*path == '/')
		path++;

	while (*path && remain_len) {
		if (*path == '/')
			remain_len--;

		path++;
	}

	if (remain_len || !*path)
		return parse_err(
			"header filename does not contain %"PRIuZ" path components",
			prefix_len);

done:
	*out_len = (path - path_start);
	*out = git__strndup(path_start, *out_len);

	return (*out == NULL) ? -1 : 0;
}

static int check_filenames(git_patch_parsed *patch)
{
	const char *prefixed_new, *prefixed_old;
	size_t old_prefixlen = 0, new_prefixlen = 0;
	bool added = (patch->base.delta->status == GIT_DELTA_ADDED);
	bool deleted = (patch->base.delta->status == GIT_DELTA_DELETED);

	if (patch->old_path && !patch->new_path)
		return parse_err("missing new path");

	if (!patch->old_path && patch->new_path)
		return parse_err("missing old path");

	/* Ensure (non-renamed) paths match */
	if (check_header_names(
			patch->header_old_path, patch->old_path, "old", added) < 0 ||
		check_header_names(
			patch->header_new_path, patch->new_path, "new", deleted) < 0)
		return -1;

	prefixed_old = (!added && patch->old_path) ? patch->old_path :
		patch->header_old_path;
	prefixed_new = (!deleted && patch->new_path) ? patch->new_path :
		patch->header_new_path;

	if (check_prefix(
			&patch->old_prefix, &old_prefixlen, patch, prefixed_old) < 0 ||
		check_prefix(
			&patch->new_prefix, &new_prefixlen, patch, prefixed_new) < 0)
		return -1;

	/* Prefer the rename filenames as they are unambiguous and unprefixed */
	if (patch->rename_old_path)
		patch->base.delta->old_file.path = patch->rename_old_path;
	else
		patch->base.delta->old_file.path = prefixed_old + old_prefixlen;

	if (patch->rename_new_path)
		patch->base.delta->new_file.path = patch->rename_new_path;
	else
		patch->base.delta->new_file.path = prefixed_new + new_prefixlen;

	if (!patch->base.delta->old_file.path &&
		!patch->base.delta->new_file.path)
		return parse_err("git diff header lacks old / new paths");

	return 0;
}

static int check_patch(git_patch_parsed *patch)
{
	git_diff_delta *delta = patch->base.delta;

	if (check_filenames(patch) < 0)
		return -1;

	if (delta->old_file.path &&
			delta->status != GIT_DELTA_DELETED &&
			!delta->new_file.mode)
		delta->new_file.mode = delta->old_file.mode;

	if (delta->status == GIT_DELTA_MODIFIED &&
			!(delta->flags & GIT_DIFF_FLAG_BINARY) &&
			delta->new_file.mode == delta->old_file.mode &&
			git_array_size(patch->base.hunks) == 0)
		return parse_err("patch with no hunks");

	if (delta->status == GIT_DELTA_ADDED) {
		memset(&delta->old_file.id, 0x0, sizeof(git_oid));
		delta->old_file.id_abbrev = 0;
	}

	if (delta->status == GIT_DELTA_DELETED) {
		memset(&delta->new_file.id, 0x0, sizeof(git_oid));
		delta->new_file.id_abbrev = 0;
	}

	return 0;
}

git_patch_parse_ctx *git_patch_parse_ctx_init(
	const char *content,
	size_t content_len,
	const git_patch_options *opts)
{
	git_patch_parse_ctx *ctx;
	git_patch_options default_opts = GIT_PATCH_OPTIONS_INIT;

	if ((ctx = git__calloc(1, sizeof(git_patch_parse_ctx))) == NULL)
		return NULL;

	if (content_len) {
		if ((ctx->content = git__malloc(content_len)) == NULL) {
			git__free(ctx);
			return NULL;
		}

		memcpy((char *)ctx->content, content, content_len);
	}

	ctx->content_len = content_len;
	ctx->remain = ctx->content;
	ctx->remain_len = ctx->content_len;

	if (opts)
		memcpy(&ctx->opts, opts, sizeof(git_patch_options));
	else
		memcpy(&ctx->opts, &default_opts, sizeof(git_patch_options));

	GIT_REFCOUNT_INC(ctx);
	return ctx;
}

static void patch_parse_ctx_free(git_patch_parse_ctx *ctx)
{
	if (!ctx)
		return;

	git__free((char *)ctx->content);
	git__free(ctx);
}

void git_patch_parse_ctx_free(git_patch_parse_ctx *ctx)
{
	GIT_REFCOUNT_DEC(ctx, patch_parse_ctx_free);
}

int git_patch_parsed_from_diff(git_patch **out, git_diff *d, size_t idx)
{
	git_diff_parsed *diff = (git_diff_parsed *)d;
	git_patch *p;

	if ((p = git_vector_get(&diff->patches, idx)) == NULL)
		return -1;

	GIT_REFCOUNT_INC(p);
	*out = p;

	return 0;
}

static void patch_parsed__free(git_patch *p)
{
	git_patch_parsed *patch = (git_patch_parsed *)p;

	if (!patch)
		return;

	git_patch_parse_ctx_free(patch->ctx);

	git__free((char *)patch->base.binary.old_file.data);
	git__free((char *)patch->base.binary.new_file.data);
	git_array_clear(patch->base.hunks);
	git_array_clear(patch->base.lines);
	git__free(patch->base.delta);

	git__free(patch->old_prefix);
	git__free(patch->new_prefix);
	git__free(patch->header_old_path);
	git__free(patch->header_new_path);
	git__free(patch->rename_old_path);
	git__free(patch->rename_new_path);
	git__free(patch->old_path);
	git__free(patch->new_path);
	git__free(patch);
}

int git_patch_parse(
	git_patch **out,
	git_patch_parse_ctx *ctx)
{
	git_patch_parsed *patch;
	size_t start, used;
	int error = 0;

	assert(out && ctx);

	*out = NULL;

	patch = git__calloc(1, sizeof(git_patch_parsed));
	GITERR_CHECK_ALLOC(patch);

	patch->ctx = ctx;
	GIT_REFCOUNT_INC(patch->ctx);

	patch->base.free_fn = patch_parsed__free;

	patch->base.delta = git__calloc(1, sizeof(git_diff_delta));
	GITERR_CHECK_ALLOC(patch->base.delta);

	patch->base.delta->status = GIT_DELTA_MODIFIED;
	patch->base.delta->nfiles = 2;

	start = ctx->remain_len;

	if ((error = parse_patch_header(patch, ctx)) < 0 ||
		(error = parse_patch_body(patch, ctx)) < 0 ||
		(error = check_patch(patch)) < 0)
		goto done;

	used = start - ctx->remain_len;
	ctx->remain += used;

	patch->base.diff_opts.old_prefix = patch->old_prefix;
	patch->base.diff_opts.new_prefix = patch->new_prefix;
	patch->base.diff_opts.flags |= GIT_DIFF_SHOW_BINARY;

	GIT_REFCOUNT_INC(patch);
	*out = &patch->base;

done:
	if (error < 0)
		patch_parsed__free(&patch->base);

	return error;
}

int git_patch_from_buffer(
	git_patch **out,
	const char *content,
	size_t content_len,
	const git_patch_options *opts)
{
	git_patch_parse_ctx *ctx;
	int error;

	ctx = git_patch_parse_ctx_init(content, content_len, opts);
	GITERR_CHECK_ALLOC(ctx);

	error = git_patch_parse(out, ctx);

	git_patch_parse_ctx_free(ctx);
	return error;
}

