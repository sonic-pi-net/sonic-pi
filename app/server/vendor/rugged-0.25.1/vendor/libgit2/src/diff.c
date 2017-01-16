/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "git2/version.h"
#include "common.h"
#include "diff.h"
#include "diff_generate.h"
#include "patch.h"
#include "commit.h"
#include "index.h"

#define DIFF_FLAG_IS_SET(DIFF,FLAG) \
	(((DIFF)->opts.flags & (FLAG)) != 0)
#define DIFF_FLAG_ISNT_SET(DIFF,FLAG) \
	(((DIFF)->opts.flags & (FLAG)) == 0)
#define DIFF_FLAG_SET(DIFF,FLAG,VAL) (DIFF)->opts.flags = \
	(VAL) ? ((DIFF)->opts.flags | (FLAG)) : ((DIFF)->opts.flags & ~(VAL))

GIT_INLINE(const char *) diff_delta__path(const git_diff_delta *delta)
{
	const char *str = delta->old_file.path;

	if (!str ||
		delta->status == GIT_DELTA_ADDED ||
		delta->status == GIT_DELTA_RENAMED ||
		delta->status == GIT_DELTA_COPIED)
		str = delta->new_file.path;

	return str;
}

const char *git_diff_delta__path(const git_diff_delta *delta)
{
	return diff_delta__path(delta);
}

int git_diff_delta__cmp(const void *a, const void *b)
{
	const git_diff_delta *da = a, *db = b;
	int val = strcmp(diff_delta__path(da), diff_delta__path(db));
	return val ? val : ((int)da->status - (int)db->status);
}

int git_diff_delta__casecmp(const void *a, const void *b)
{
	const git_diff_delta *da = a, *db = b;
	int val = strcasecmp(diff_delta__path(da), diff_delta__path(db));
	return val ? val : ((int)da->status - (int)db->status);
}

int git_diff__entry_cmp(const void *a, const void *b)
{
	const git_index_entry *entry_a = a;
	const git_index_entry *entry_b = b;

	return strcmp(entry_a->path, entry_b->path);
}

int git_diff__entry_icmp(const void *a, const void *b)
{
	const git_index_entry *entry_a = a;
	const git_index_entry *entry_b = b;

	return strcasecmp(entry_a->path, entry_b->path);
}

void git_diff_free(git_diff *diff)
{
	if (!diff)
		return;

	GIT_REFCOUNT_DEC(diff, diff->free_fn);
}

void git_diff_addref(git_diff *diff)
{
	GIT_REFCOUNT_INC(diff);
}

size_t git_diff_num_deltas(const git_diff *diff)
{
	assert(diff);
	return diff->deltas.length;
}

size_t git_diff_num_deltas_of_type(const git_diff *diff, git_delta_t type)
{
	size_t i, count = 0;
	const git_diff_delta *delta;

	assert(diff);

	git_vector_foreach(&diff->deltas, i, delta) {
		count += (delta->status == type);
	}

	return count;
}

const git_diff_delta *git_diff_get_delta(const git_diff *diff, size_t idx)
{
	assert(diff);
	return git_vector_get(&diff->deltas, idx);
}

int git_diff_is_sorted_icase(const git_diff *diff)
{
	return (diff->opts.flags & GIT_DIFF_IGNORE_CASE) != 0;
}

int git_diff_get_perfdata(git_diff_perfdata *out, const git_diff *diff)
{
	assert(out);
	GITERR_CHECK_VERSION(out, GIT_DIFF_PERFDATA_VERSION, "git_diff_perfdata");
	out->stat_calls = diff->perf.stat_calls;
	out->oid_calculations = diff->perf.oid_calculations;
	return 0;
}

int git_diff_format_email__append_header_tobuf(
	git_buf *out,
	const git_oid *id,
	const git_signature *author,
	const char *summary,
	const char *body,
	size_t patch_no,
	size_t total_patches,
	bool exclude_patchno_marker)
{
	char idstr[GIT_OID_HEXSZ + 1];
	char date_str[GIT_DATE_RFC2822_SZ];
	int error = 0;

	git_oid_fmt(idstr, id);
	idstr[GIT_OID_HEXSZ] = '\0';

	if ((error = git__date_rfc2822_fmt(date_str, sizeof(date_str),
		&author->when)) < 0)
		return error;

	error = git_buf_printf(out,
				"From %s Mon Sep 17 00:00:00 2001\n" \
				"From: %s <%s>\n" \
				"Date: %s\n" \
				"Subject: ",
				idstr,
				author->name, author->email,
				date_str);

	if (error < 0)
		return error;

	if (!exclude_patchno_marker) {
		if (total_patches == 1) {
			error = git_buf_puts(out, "[PATCH] ");
		} else {
			error = git_buf_printf(out, "[PATCH %"PRIuZ"/%"PRIuZ"] ",
				patch_no, total_patches);
		}

		if (error < 0)
			return error;
	}

	error = git_buf_printf(out, "%s\n\n", summary);

	if (body) {
		git_buf_puts(out, body);

		if (out->ptr[out->size - 1] != '\n')
			git_buf_putc(out, '\n');
	}

	return error;
}

int git_diff_format_email__append_patches_tobuf(
	git_buf *out,
	git_diff *diff)
{
	size_t i, deltas;
	int error = 0;

	deltas = git_diff_num_deltas(diff);

	for (i = 0; i < deltas; ++i) {
		git_patch *patch = NULL;

		if ((error = git_patch_from_diff(&patch, diff, i)) >= 0)
			error = git_patch_to_buf(out, patch);

		git_patch_free(patch);

		if (error < 0)
			break;
	}

	return error;
}

int git_diff_format_email(
	git_buf *out,
	git_diff *diff,
	const git_diff_format_email_options *opts)
{
	git_diff_stats *stats = NULL;
	char *summary = NULL, *loc = NULL;
	bool ignore_marker;
	unsigned int format_flags = 0;
	size_t allocsize;
	int error;

	assert(out && diff && opts);
	assert(opts->summary && opts->id && opts->author);

	GITERR_CHECK_VERSION(opts,
		GIT_DIFF_FORMAT_EMAIL_OPTIONS_VERSION,
		"git_format_email_options");

	ignore_marker = (opts->flags &
		GIT_DIFF_FORMAT_EMAIL_EXCLUDE_SUBJECT_PATCH_MARKER) != 0;

	if (!ignore_marker) {
		if (opts->patch_no > opts->total_patches) {
			giterr_set(GITERR_INVALID,
				"patch %"PRIuZ" out of range. max %"PRIuZ,
				opts->patch_no, opts->total_patches);
			return -1;
		}

		if (opts->patch_no == 0) {
			giterr_set(GITERR_INVALID,
				"invalid patch no %"PRIuZ". should be >0", opts->patch_no);
			return -1;
		}
	}

	/* the summary we receive may not be clean.
	 * it could potentially contain new line characters
	 * or not be set, sanitize, */
	if ((loc = strpbrk(opts->summary, "\r\n")) != NULL) {
		size_t offset = 0;

		if ((offset = (loc - opts->summary)) == 0) {
			giterr_set(GITERR_INVALID, "summary is empty");
			error = -1;
			goto on_error;
		}

		GITERR_CHECK_ALLOC_ADD(&allocsize, offset, 1);
		summary = git__calloc(allocsize, sizeof(char));
		GITERR_CHECK_ALLOC(summary);

		strncpy(summary, opts->summary, offset);
	}

	error = git_diff_format_email__append_header_tobuf(out,
		opts->id, opts->author, summary == NULL ? opts->summary : summary,
		opts->body, opts->patch_no, opts->total_patches, ignore_marker);

	if (error < 0)
		goto on_error;

	format_flags = GIT_DIFF_STATS_FULL | GIT_DIFF_STATS_INCLUDE_SUMMARY;

	if ((error = git_buf_puts(out, "---\n")) < 0 ||
		(error = git_diff_get_stats(&stats, diff)) < 0 ||
		(error = git_diff_stats_to_buf(out, stats, format_flags, 0)) < 0 ||
		(error = git_buf_putc(out, '\n')) < 0 ||
		(error = git_diff_format_email__append_patches_tobuf(out, diff)) < 0)
			goto on_error;

	error = git_buf_puts(out, "--\nlibgit2 " LIBGIT2_VERSION "\n\n");

on_error:
	git__free(summary);
	git_diff_stats_free(stats);

	return error;
}

int git_diff_commit_as_email(
	git_buf *out,
	git_repository *repo,
	git_commit *commit,
	size_t patch_no,
	size_t total_patches,
	git_diff_format_email_flags_t flags,
	const git_diff_options *diff_opts)
{
	git_diff *diff = NULL;
	git_diff_format_email_options opts =
		GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT;
	int error;

	assert (out && repo && commit);

	opts.flags = flags;
	opts.patch_no = patch_no;
	opts.total_patches = total_patches;
	opts.id = git_commit_id(commit);
	opts.summary = git_commit_summary(commit);
	opts.body = git_commit_body(commit);
	opts.author = git_commit_author(commit);

	if ((error = git_diff__commit(&diff, repo, commit, diff_opts)) < 0)
		return error;

	error = git_diff_format_email(out, diff, &opts);

	git_diff_free(diff);
	return error;
}

int git_diff_init_options(git_diff_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_diff_options, GIT_DIFF_OPTIONS_INIT);
	return 0;
}

int git_diff_find_init_options(
	git_diff_find_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_diff_find_options, GIT_DIFF_FIND_OPTIONS_INIT);
	return 0;
}

int git_diff_format_email_init_options(
	git_diff_format_email_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_diff_format_email_options,
		GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT);
	return 0;
}

