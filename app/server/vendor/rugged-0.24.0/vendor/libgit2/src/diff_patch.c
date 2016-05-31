/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"
#include "git2/blob.h"
#include "diff.h"
#include "diff_file.h"
#include "diff_driver.h"
#include "diff_patch.h"
#include "diff_xdiff.h"
#include "delta.h"
#include "zstream.h"
#include "fileops.h"

static void diff_output_init(
	git_diff_output*, const git_diff_options*, git_diff_file_cb,
	git_diff_binary_cb, git_diff_hunk_cb, git_diff_line_cb, void*);

static void diff_output_to_patch(git_diff_output *, git_patch *);

static void diff_patch_update_binary(git_patch *patch)
{
	if ((patch->delta->flags & DIFF_FLAGS_KNOWN_BINARY) != 0)
		return;

	if ((patch->ofile.file->flags & GIT_DIFF_FLAG_BINARY) != 0 ||
		(patch->nfile.file->flags & GIT_DIFF_FLAG_BINARY) != 0)
		patch->delta->flags |= GIT_DIFF_FLAG_BINARY;

	else if (patch->ofile.file->size > GIT_XDIFF_MAX_SIZE ||
			 patch->nfile.file->size > GIT_XDIFF_MAX_SIZE)
		patch->delta->flags |= GIT_DIFF_FLAG_BINARY;

	else if ((patch->ofile.file->flags & DIFF_FLAGS_NOT_BINARY) != 0 &&
			 (patch->nfile.file->flags & DIFF_FLAGS_NOT_BINARY) != 0)
		patch->delta->flags |= GIT_DIFF_FLAG_NOT_BINARY;
}

static void diff_patch_init_common(git_patch *patch)
{
	diff_patch_update_binary(patch);

	patch->flags |= GIT_DIFF_PATCH_INITIALIZED;

	if (patch->diff)
		git_diff_addref(patch->diff);
}

static int diff_patch_normalize_options(
	git_diff_options *out,
	const git_diff_options *opts)
{
	if (opts) {
		GITERR_CHECK_VERSION(opts, GIT_DIFF_OPTIONS_VERSION, "git_diff_options");
		memcpy(out, opts, sizeof(git_diff_options));
	} else {
		git_diff_options default_opts = GIT_DIFF_OPTIONS_INIT;
		memcpy(out, &default_opts, sizeof(git_diff_options));
	}

	out->old_prefix = opts && opts->old_prefix ?
		git__strdup(opts->old_prefix) :
		git__strdup(DIFF_OLD_PREFIX_DEFAULT);

	out->new_prefix = opts && opts->new_prefix ?
		git__strdup(opts->new_prefix) :
		git__strdup(DIFF_NEW_PREFIX_DEFAULT);

	GITERR_CHECK_ALLOC(out->old_prefix);
	GITERR_CHECK_ALLOC(out->new_prefix);

	return 0;
}

static int diff_patch_init_from_diff(
	git_patch *patch, git_diff *diff, size_t delta_index)
{
	int error = 0;

	memset(patch, 0, sizeof(*patch));
	patch->diff  = diff;
	patch->delta = git_vector_get(&diff->deltas, delta_index);
	patch->delta_index = delta_index;

	if ((error = diff_patch_normalize_options(
			&patch->diff_opts, &diff->opts)) < 0 ||
		(error = git_diff_file_content__init_from_diff(
			&patch->ofile, diff, patch->delta, true)) < 0 ||
		(error = git_diff_file_content__init_from_diff(
			&patch->nfile, diff, patch->delta, false)) < 0)
		return error;

	diff_patch_init_common(patch);

	return 0;
}

static int diff_patch_alloc_from_diff(
	git_patch **out, git_diff *diff, size_t delta_index)
{
	int error;
	git_patch *patch = git__calloc(1, sizeof(git_patch));
	GITERR_CHECK_ALLOC(patch);

	if (!(error = diff_patch_init_from_diff(patch, diff, delta_index))) {
		patch->flags |= GIT_DIFF_PATCH_ALLOCATED;
		GIT_REFCOUNT_INC(patch);
	} else {
		git__free(patch);
		patch = NULL;
	}

	*out = patch;
	return error;
}

GIT_INLINE(bool) should_skip_binary(git_patch *patch, git_diff_file *file)
{
	if ((patch->diff_opts.flags & GIT_DIFF_SHOW_BINARY) != 0)
		return false;

	return (file->flags & GIT_DIFF_FLAG_BINARY) != 0;
}

static bool diff_patch_diffable(git_patch *patch)
{
	size_t olen, nlen;

	if (patch->delta->status == GIT_DELTA_UNMODIFIED)
		return false;

	/* if we've determined this to be binary (and we are not showing binary
	 * data) then we have skipped loading the map data.  instead, query the
	 * file data itself.
	 */
	if ((patch->delta->flags & GIT_DIFF_FLAG_BINARY) != 0 &&
		(patch->diff_opts.flags & GIT_DIFF_SHOW_BINARY) == 0) {
		olen = (size_t)patch->ofile.file->size;
		nlen = (size_t)patch->nfile.file->size;
	} else {
		olen = patch->ofile.map.len;
		nlen = patch->nfile.map.len;
	}

	/* if both sides are empty, files are identical */
	if (!olen && !nlen)
		return false;

	/* otherwise, check the file sizes and the oid */
	return (olen != nlen ||
		!git_oid_equal(&patch->ofile.file->id, &patch->nfile.file->id));
}

static int diff_patch_load(git_patch *patch, git_diff_output *output)
{
	int error = 0;
	bool incomplete_data;

	if ((patch->flags & GIT_DIFF_PATCH_LOADED) != 0)
		return 0;

	/* if no hunk and data callbacks and user doesn't care if data looks
	 * binary, then there is no need to actually load the data
	 */
	if ((patch->ofile.opts_flags & GIT_DIFF_SKIP_BINARY_CHECK) != 0 &&
		output && !output->binary_cb && !output->hunk_cb && !output->data_cb)
		return 0;

	incomplete_data =
		(((patch->ofile.flags & GIT_DIFF_FLAG__NO_DATA) != 0 ||
		  (patch->ofile.file->flags & GIT_DIFF_FLAG_VALID_ID) != 0) &&
		 ((patch->nfile.flags & GIT_DIFF_FLAG__NO_DATA) != 0 ||
		  (patch->nfile.file->flags & GIT_DIFF_FLAG_VALID_ID) != 0));

	/* always try to load workdir content first because filtering may
	 * need 2x data size and this minimizes peak memory footprint
	 */
	if (patch->ofile.src == GIT_ITERATOR_TYPE_WORKDIR) {
		if ((error = git_diff_file_content__load(
				&patch->ofile, &patch->diff_opts)) < 0 ||
			should_skip_binary(patch, patch->ofile.file))
			goto cleanup;
	}
	if (patch->nfile.src == GIT_ITERATOR_TYPE_WORKDIR) {
		if ((error = git_diff_file_content__load(
				&patch->nfile, &patch->diff_opts)) < 0 ||
			should_skip_binary(patch, patch->nfile.file))
			goto cleanup;
	}

	/* once workdir has been tried, load other data as needed */
	if (patch->ofile.src != GIT_ITERATOR_TYPE_WORKDIR) {
		if ((error = git_diff_file_content__load(
				&patch->ofile, &patch->diff_opts)) < 0 ||
			should_skip_binary(patch, patch->ofile.file))
			goto cleanup;
	}
	if (patch->nfile.src != GIT_ITERATOR_TYPE_WORKDIR) {
		if ((error = git_diff_file_content__load(
				&patch->nfile, &patch->diff_opts)) < 0 ||
			should_skip_binary(patch, patch->nfile.file))
			goto cleanup;
	}

	/* if previously missing an oid, and now that we have it the two sides
	 * are the same (and not submodules), update MODIFIED -> UNMODIFIED
	 */
	if (incomplete_data &&
		patch->ofile.file->mode == patch->nfile.file->mode &&
		patch->ofile.file->mode != GIT_FILEMODE_COMMIT &&
		git_oid_equal(&patch->ofile.file->id, &patch->nfile.file->id) &&
		patch->delta->status == GIT_DELTA_MODIFIED) /* not RENAMED/COPIED! */
		patch->delta->status = GIT_DELTA_UNMODIFIED;

cleanup:
	diff_patch_update_binary(patch);

	if (!error) {
		if (diff_patch_diffable(patch))
			patch->flags |= GIT_DIFF_PATCH_DIFFABLE;

		patch->flags |= GIT_DIFF_PATCH_LOADED;
	}

	return error;
}

static int diff_patch_invoke_file_callback(
	git_patch *patch, git_diff_output *output)
{
	float progress = patch->diff ?
		((float)patch->delta_index / patch->diff->deltas.length) : 1.0f;

	if (!output->file_cb)
		return 0;

	return giterr_set_after_callback_function(
		output->file_cb(patch->delta, progress, output->payload),
		"git_patch");
}

static int create_binary(
	git_diff_binary_t *out_type,
	char **out_data,
	size_t *out_datalen,
	size_t *out_inflatedlen,
	const char *a_data,
	size_t a_datalen,
	const char *b_data,
	size_t b_datalen)
{
	git_buf deflate = GIT_BUF_INIT, delta = GIT_BUF_INIT;
	unsigned long delta_data_len;
	int error;

	/* The git_delta function accepts unsigned long only */
	if (!git__is_ulong(a_datalen) || !git__is_ulong(b_datalen))
		return GIT_EBUFS;

	if ((error = git_zstream_deflatebuf(&deflate, b_data, b_datalen)) < 0)
		goto done;

	/* The git_delta function accepts unsigned long only */
	if (!git__is_ulong(deflate.size)) {
		error = GIT_EBUFS;
		goto done;
	}

	if (a_datalen && b_datalen) {
		void *delta_data = git_delta(
			a_data, (unsigned long)a_datalen,
			b_data, (unsigned long)b_datalen,
			&delta_data_len, (unsigned long)deflate.size);

		if (delta_data) {
			error = git_zstream_deflatebuf(
				&delta, delta_data, (size_t)delta_data_len);

			git__free(delta_data);

			if (error < 0)
				goto done;
		}
	}

	if (delta.size && delta.size < deflate.size) {
		*out_type = GIT_DIFF_BINARY_DELTA;
		*out_datalen = delta.size;
		*out_data = git_buf_detach(&delta);
		*out_inflatedlen = delta_data_len;
	} else {
		*out_type = GIT_DIFF_BINARY_LITERAL;
		*out_datalen = deflate.size;
		*out_data = git_buf_detach(&deflate);
		*out_inflatedlen = b_datalen;
	}

done:
	git_buf_free(&deflate);
	git_buf_free(&delta);

	return error;
}

static int diff_binary(git_diff_output *output, git_patch *patch)
{
	git_diff_binary binary = {{0}};
	const char *old_data = patch->ofile.map.data;
	const char *new_data = patch->nfile.map.data;
	size_t old_len = patch->ofile.map.len,
		new_len = patch->nfile.map.len;
	int error;

	/* Create the old->new delta (as the "new" side of the patch),
	 * and the new->old delta (as the "old" side)
	 */
	if ((error = create_binary(&binary.old_file.type,
			(char **)&binary.old_file.data,
			&binary.old_file.datalen,
			&binary.old_file.inflatedlen,
			new_data, new_len, old_data, old_len)) < 0 ||
		(error = create_binary(&binary.new_file.type,
			(char **)&binary.new_file.data,
			&binary.new_file.datalen,
			&binary.new_file.inflatedlen,
			old_data, old_len, new_data, new_len)) < 0)
		return error;

	error = giterr_set_after_callback_function(
		output->binary_cb(patch->delta, &binary, output->payload),
		"git_patch");

	git__free((char *) binary.old_file.data);
	git__free((char *) binary.new_file.data);

	return error;
}

static int diff_patch_generate(git_patch *patch, git_diff_output *output)
{
	int error = 0;

	if ((patch->flags & GIT_DIFF_PATCH_DIFFED) != 0)
		return 0;

	/* if we are not looking at the binary or text data, don't do the diff */
	if (!output->binary_cb && !output->hunk_cb && !output->data_cb)
		return 0;

	if ((patch->flags & GIT_DIFF_PATCH_LOADED) == 0 &&
		(error = diff_patch_load(patch, output)) < 0)
		return error;

	if ((patch->flags & GIT_DIFF_PATCH_DIFFABLE) == 0)
		return 0;

	if ((patch->delta->flags & GIT_DIFF_FLAG_BINARY) != 0) {
		if (output->binary_cb)
			error = diff_binary(output, patch);
	}
	else {
		if (output->diff_cb)
			error = output->diff_cb(output, patch);
	}

	patch->flags |= GIT_DIFF_PATCH_DIFFED;
	return error;
}

static void diff_patch_free(git_patch *patch)
{
	git_diff_file_content__clear(&patch->ofile);
	git_diff_file_content__clear(&patch->nfile);

	git_array_clear(patch->lines);
	git_array_clear(patch->hunks);

	git_diff_free(patch->diff); /* decrements refcount */
	patch->diff = NULL;

	git_pool_clear(&patch->flattened);

	git__free((char *)patch->diff_opts.old_prefix);
	git__free((char *)patch->diff_opts.new_prefix);

	git__free((char *)patch->binary.old_file.data);
	git__free((char *)patch->binary.new_file.data);

	if (patch->flags & GIT_DIFF_PATCH_ALLOCATED)
		git__free(patch);
}

static int diff_required(git_diff *diff, const char *action)
{
	if (diff)
		return 0;
	giterr_set(GITERR_INVALID, "Must provide valid diff to %s", action);
	return -1;
}

int git_diff_foreach(
	git_diff *diff,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb data_cb,
	void *payload)
{
	int error = 0;
	git_xdiff_output xo;
	size_t idx;
	git_patch patch;

	if ((error = diff_required(diff, "git_diff_foreach")) < 0)
		return error;

	memset(&xo, 0, sizeof(xo));
	memset(&patch, 0, sizeof(patch));
	diff_output_init(
		&xo.output, &diff->opts, file_cb, binary_cb, hunk_cb, data_cb, payload);
	git_xdiff_init(&xo, &diff->opts);

	git_vector_foreach(&diff->deltas, idx, patch.delta) {

		/* check flags against patch status */
		if (git_diff_delta__should_skip(&diff->opts, patch.delta))
			continue;

		if (binary_cb || hunk_cb || data_cb) {
			if ((error = diff_patch_init_from_diff(&patch, diff, idx)) != 0 ||
				(error = diff_patch_load(&patch, &xo.output)) != 0)
				return error;
		}

		if ((error = diff_patch_invoke_file_callback(&patch, &xo.output)) == 0) {
			if (binary_cb || hunk_cb || data_cb)
					error = diff_patch_generate(&patch, &xo.output);
		}

		git_patch_free(&patch);

		if (error)
			break;
	}

	return error;
}

typedef struct {
	git_patch patch;
	git_diff_delta delta;
	char paths[GIT_FLEX_ARRAY];
} diff_patch_with_delta;

static int diff_single_generate(diff_patch_with_delta *pd, git_xdiff_output *xo)
{
	int error = 0;
	git_patch *patch = &pd->patch;
	bool has_old = ((patch->ofile.flags & GIT_DIFF_FLAG__NO_DATA) == 0);
	bool has_new = ((patch->nfile.flags & GIT_DIFF_FLAG__NO_DATA) == 0);

	pd->delta.status = has_new ?
		(has_old ? GIT_DELTA_MODIFIED : GIT_DELTA_ADDED) :
		(has_old ? GIT_DELTA_DELETED : GIT_DELTA_UNTRACKED);

	if (git_oid_equal(&patch->nfile.file->id, &patch->ofile.file->id))
		pd->delta.status = GIT_DELTA_UNMODIFIED;

	patch->delta = &pd->delta;

	diff_patch_init_common(patch);

	if (pd->delta.status == GIT_DELTA_UNMODIFIED &&
		!(patch->ofile.opts_flags & GIT_DIFF_INCLUDE_UNMODIFIED))
		return error;

	error = diff_patch_invoke_file_callback(patch, (git_diff_output *)xo);

	if (!error)
		error = diff_patch_generate(patch, (git_diff_output *)xo);

	return error;
}

static int diff_patch_from_sources(
	diff_patch_with_delta *pd,
	git_xdiff_output *xo,
	git_diff_file_content_src *oldsrc,
	git_diff_file_content_src *newsrc,
	const git_diff_options *opts)
{
	int error = 0;
	git_repository *repo =
		oldsrc->blob ? git_blob_owner(oldsrc->blob) :
		newsrc->blob ? git_blob_owner(newsrc->blob) : NULL;
	git_diff_file *lfile = &pd->delta.old_file, *rfile = &pd->delta.new_file;
	git_diff_file_content *ldata = &pd->patch.ofile, *rdata = &pd->patch.nfile;

	if ((error = diff_patch_normalize_options(&pd->patch.diff_opts, opts)) < 0)
		return error;

	if (opts && (opts->flags & GIT_DIFF_REVERSE) != 0) {
		void *tmp = lfile; lfile = rfile; rfile = tmp;
		tmp = ldata; ldata = rdata; rdata = tmp;
	}

	pd->patch.delta = &pd->delta;

	if (!oldsrc->as_path) {
		if (newsrc->as_path)
			oldsrc->as_path = newsrc->as_path;
		else
			oldsrc->as_path = newsrc->as_path = "file";
	}
	else if (!newsrc->as_path)
		newsrc->as_path = oldsrc->as_path;

	lfile->path = oldsrc->as_path;
	rfile->path = newsrc->as_path;

	if ((error = git_diff_file_content__init_from_src(
			ldata, repo, opts, oldsrc, lfile)) < 0 ||
		(error = git_diff_file_content__init_from_src(
			rdata, repo, opts, newsrc, rfile)) < 0)
		return error;

	return diff_single_generate(pd, xo);
}

static int diff_patch_with_delta_alloc(
	diff_patch_with_delta **out,
	const char **old_path,
	const char **new_path)
{
	diff_patch_with_delta *pd;
	size_t old_len = *old_path ? strlen(*old_path) : 0;
	size_t new_len = *new_path ? strlen(*new_path) : 0;
	size_t alloc_len;

	GITERR_CHECK_ALLOC_ADD(&alloc_len, sizeof(*pd), old_len);
	GITERR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, new_len);
	GITERR_CHECK_ALLOC_ADD(&alloc_len, alloc_len, 2);

	*out = pd = git__calloc(1, alloc_len);
	GITERR_CHECK_ALLOC(pd);

	pd->patch.flags = GIT_DIFF_PATCH_ALLOCATED;

	if (*old_path) {
		memcpy(&pd->paths[0], *old_path, old_len);
		*old_path = &pd->paths[0];
	} else if (*new_path)
		*old_path = &pd->paths[old_len + 1];

	if (*new_path) {
		memcpy(&pd->paths[old_len + 1], *new_path, new_len);
		*new_path = &pd->paths[old_len + 1];
	} else if (*old_path)
		*new_path = &pd->paths[0];

	return 0;
}

static int diff_from_sources(
	git_diff_file_content_src *oldsrc,
	git_diff_file_content_src *newsrc,
	const git_diff_options *opts,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb data_cb,
	void *payload)
{
	int error = 0;
	diff_patch_with_delta pd;
	git_xdiff_output xo;

	memset(&xo, 0, sizeof(xo));
	diff_output_init(
		&xo.output, opts, file_cb, binary_cb, hunk_cb, data_cb, payload);
	git_xdiff_init(&xo, opts);

	memset(&pd, 0, sizeof(pd));

	error = diff_patch_from_sources(&pd, &xo, oldsrc, newsrc, opts);

	git_patch_free(&pd.patch);

	return error;
}

static int patch_from_sources(
	git_patch **out,
	git_diff_file_content_src *oldsrc,
	git_diff_file_content_src *newsrc,
	const git_diff_options *opts)
{
	int error = 0;
	diff_patch_with_delta *pd;
	git_xdiff_output xo;

	assert(out);
	*out = NULL;

	if ((error = diff_patch_with_delta_alloc(
			&pd, &oldsrc->as_path, &newsrc->as_path)) < 0)
		return error;

	memset(&xo, 0, sizeof(xo));
	diff_output_to_patch(&xo.output, &pd->patch);
	git_xdiff_init(&xo, opts);

	if (!(error = diff_patch_from_sources(pd, &xo, oldsrc, newsrc, opts)))
		*out = (git_patch *)pd;
	else
		git_patch_free((git_patch *)pd);

	return error;
}

int git_diff_blobs(
	const git_blob *old_blob,
	const char *old_path,
	const git_blob *new_blob,
	const char *new_path,
	const git_diff_options *opts,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb data_cb,
	void *payload)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(old_blob, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(new_blob, new_path);
	return diff_from_sources(
		&osrc, &nsrc, opts, file_cb, binary_cb, hunk_cb, data_cb, payload);
}

int git_patch_from_blobs(
	git_patch **out,
	const git_blob *old_blob,
	const char *old_path,
	const git_blob *new_blob,
	const char *new_path,
	const git_diff_options *opts)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(old_blob, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(new_blob, new_path);
	return patch_from_sources(out, &osrc, &nsrc, opts);
}

int git_diff_blob_to_buffer(
	const git_blob *old_blob,
	const char *old_path,
	const char *buf,
	size_t buflen,
	const char *buf_path,
	const git_diff_options *opts,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb data_cb,
	void *payload)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(old_blob, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(buf, buflen, buf_path);
	return diff_from_sources(
		&osrc, &nsrc, opts, file_cb, binary_cb, hunk_cb, data_cb, payload);
}

int git_patch_from_blob_and_buffer(
	git_patch **out,
	const git_blob *old_blob,
	const char *old_path,
	const char *buf,
	size_t buflen,
	const char *buf_path,
	const git_diff_options *opts)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BLOB(old_blob, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(buf, buflen, buf_path);
	return patch_from_sources(out, &osrc, &nsrc, opts);
}

int git_diff_buffers(
	const void *old_buf,
	size_t old_len,
	const char *old_path,
	const void *new_buf,
	size_t new_len,
	const char *new_path,
	const git_diff_options *opts,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb data_cb,
	void *payload)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(old_buf, old_len, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(new_buf, new_len, new_path);
	return diff_from_sources(
		&osrc, &nsrc, opts, file_cb, binary_cb, hunk_cb, data_cb, payload);
}

int git_patch_from_buffers(
	git_patch **out,
	const void *old_buf,
	size_t old_len,
	const char *old_path,
	const char *new_buf,
	size_t new_len,
	const char *new_path,
	const git_diff_options *opts)
{
	git_diff_file_content_src osrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(old_buf, old_len, old_path);
	git_diff_file_content_src nsrc =
		GIT_DIFF_FILE_CONTENT_SRC__BUF(new_buf, new_len, new_path);
	return patch_from_sources(out, &osrc, &nsrc, opts);
}

int git_patch_from_diff(
	git_patch **patch_ptr, git_diff *diff, size_t idx)
{
	int error = 0;
	git_xdiff_output xo;
	git_diff_delta *delta = NULL;
	git_patch *patch = NULL;

	if (patch_ptr) *patch_ptr = NULL;

	if (diff_required(diff, "git_patch_from_diff") < 0)
		return -1;

	delta = git_vector_get(&diff->deltas, idx);
	if (!delta) {
		giterr_set(GITERR_INVALID, "Index out of range for delta in diff");
		return GIT_ENOTFOUND;
	}

	if (git_diff_delta__should_skip(&diff->opts, delta))
		return 0;

	/* don't load the patch data unless we need it for binary check */
	if (!patch_ptr &&
		((delta->flags & DIFF_FLAGS_KNOWN_BINARY) != 0 ||
		 (diff->opts.flags & GIT_DIFF_SKIP_BINARY_CHECK) != 0))
		return 0;

	if ((error = diff_patch_alloc_from_diff(&patch, diff, idx)) < 0)
		return error;

	memset(&xo, 0, sizeof(xo));
	diff_output_to_patch(&xo.output, patch);
	git_xdiff_init(&xo, &diff->opts);

	error = diff_patch_invoke_file_callback(patch, &xo.output);

	if (!error)
		error = diff_patch_generate(patch, &xo.output);

	if (!error) {
		/* TODO: if cumulative diff size is < 0.5 total size, flatten patch */
		/* TODO: and unload the file content */
	}

	if (error || !patch_ptr)
		git_patch_free(patch);
	else
		*patch_ptr = patch;

	return error;
}

void git_patch_free(git_patch *patch)
{
	if (patch)
		GIT_REFCOUNT_DEC(patch, diff_patch_free);
}

const git_diff_delta *git_patch_get_delta(const git_patch *patch)
{
	assert(patch);
	return patch->delta;
}

size_t git_patch_num_hunks(const git_patch *patch)
{
	assert(patch);
	return git_array_size(patch->hunks);
}

int git_patch_line_stats(
	size_t *total_ctxt,
	size_t *total_adds,
	size_t *total_dels,
	const git_patch *patch)
{
	size_t totals[3], idx;

	memset(totals, 0, sizeof(totals));

	for (idx = 0; idx < git_array_size(patch->lines); ++idx) {
		git_diff_line *line = git_array_get(patch->lines, idx);
		if (!line)
			continue;

		switch (line->origin) {
		case GIT_DIFF_LINE_CONTEXT:  totals[0]++; break;
		case GIT_DIFF_LINE_ADDITION: totals[1]++; break;
		case GIT_DIFF_LINE_DELETION: totals[2]++; break;
		default:
			/* diff --stat and --numstat don't count EOFNL marks because
			 * they will always be paired with a ADDITION or DELETION line.
			 */
			break;
		}
	}

	if (total_ctxt)
		*total_ctxt = totals[0];
	if (total_adds)
		*total_adds = totals[1];
	if (total_dels)
		*total_dels = totals[2];

	return 0;
}

static int diff_error_outofrange(const char *thing)
{
	giterr_set(GITERR_INVALID, "Diff patch %s index out of range", thing);
	return GIT_ENOTFOUND;
}

int git_patch_get_hunk(
	const git_diff_hunk **out,
	size_t *lines_in_hunk,
	git_patch *patch,
	size_t hunk_idx)
{
	diff_patch_hunk *hunk;
	assert(patch);

	hunk = git_array_get(patch->hunks, hunk_idx);

	if (!hunk) {
		if (out) *out = NULL;
		if (lines_in_hunk) *lines_in_hunk = 0;
		return diff_error_outofrange("hunk");
	}

	if (out) *out = &hunk->hunk;
	if (lines_in_hunk) *lines_in_hunk = hunk->line_count;
	return 0;
}

int git_patch_num_lines_in_hunk(const git_patch *patch, size_t hunk_idx)
{
	diff_patch_hunk *hunk;
	assert(patch);

	if (!(hunk = git_array_get(patch->hunks, hunk_idx)))
		return diff_error_outofrange("hunk");
	return (int)hunk->line_count;
}

int git_patch_get_line_in_hunk(
	const git_diff_line **out,
	git_patch *patch,
	size_t hunk_idx,
	size_t line_of_hunk)
{
	diff_patch_hunk *hunk;
	git_diff_line *line;

	assert(patch);

	if (!(hunk = git_array_get(patch->hunks, hunk_idx))) {
		if (out) *out = NULL;
		return diff_error_outofrange("hunk");
	}

	if (line_of_hunk >= hunk->line_count ||
		!(line = git_array_get(
			patch->lines, hunk->line_start + line_of_hunk))) {
		if (out) *out = NULL;
		return diff_error_outofrange("line");
	}

	if (out) *out = line;
	return 0;
}

size_t git_patch_size(
	git_patch *patch,
	int include_context,
	int include_hunk_headers,
	int include_file_headers)
{
	size_t out;

	assert(patch);

	out = patch->content_size;

	if (!include_context)
		out -= patch->context_size;

	if (include_hunk_headers)
		out += patch->header_size;

	if (include_file_headers) {
		git_buf file_header = GIT_BUF_INIT;

		if (git_diff_delta__format_file_header(
				&file_header, patch->delta, NULL, NULL, 0) < 0)
			giterr_clear();
		else
			out += git_buf_len(&file_header);

		git_buf_free(&file_header);
	}

	return out;
}

git_diff *git_patch__diff(git_patch *patch)
{
	return patch->diff;
}

git_diff_driver *git_patch__driver(git_patch *patch)
{
	/* ofile driver is representative for whole patch */
	return patch->ofile.driver;
}

void git_patch__old_data(
	char **ptr, size_t *len, git_patch *patch)
{
	*ptr = patch->ofile.map.data;
	*len = patch->ofile.map.len;
}

void git_patch__new_data(
	char **ptr, size_t *len, git_patch *patch)
{
	*ptr = patch->nfile.map.data;
	*len = patch->nfile.map.len;
}

int git_patch__invoke_callbacks(
	git_patch *patch,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb line_cb,
	void *payload)
{
	int error = 0;
	uint32_t i, j;

	if (file_cb)
		error = file_cb(patch->delta, 0, payload);

	if ((patch->delta->flags & GIT_DIFF_FLAG_BINARY) != 0) {
		if (binary_cb)
			error = binary_cb(patch->delta, &patch->binary, payload);

		return error;
	}

	if (!hunk_cb && !line_cb)
		return error;

	for (i = 0; !error && i < git_array_size(patch->hunks); ++i) {
		diff_patch_hunk *h = git_array_get(patch->hunks, i);

		if (hunk_cb)
			error = hunk_cb(patch->delta, &h->hunk, payload);

		if (!line_cb)
			continue;

		for (j = 0; !error && j < h->line_count; ++j) {
			git_diff_line *l =
				git_array_get(patch->lines, h->line_start + j);

			error = line_cb(patch->delta, &h->hunk, l, payload);
		}
	}

	return error;
}


static int diff_patch_file_cb(
	const git_diff_delta *delta,
	float progress,
	void *payload)
{
	GIT_UNUSED(delta); GIT_UNUSED(progress); GIT_UNUSED(payload);
	return 0;
}

static int diff_patch_binary_cb(
	const git_diff_delta *delta,
	const git_diff_binary *binary,
	void *payload)
{
	git_patch *patch = payload;

	GIT_UNUSED(delta);

	memcpy(&patch->binary, binary, sizeof(git_diff_binary));

	if (binary->old_file.data) {
		patch->binary.old_file.data = git__malloc(binary->old_file.datalen);
		GITERR_CHECK_ALLOC(patch->binary.old_file.data);

		memcpy((char *)patch->binary.old_file.data,
			binary->old_file.data, binary->old_file.datalen);
	}

	if (binary->new_file.data) {
		patch->binary.new_file.data = git__malloc(binary->new_file.datalen);
		GITERR_CHECK_ALLOC(patch->binary.new_file.data);

		memcpy((char *)patch->binary.new_file.data,
			binary->new_file.data, binary->new_file.datalen);
	}

	return 0;
}

static int diff_patch_hunk_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk_,
	void *payload)
{
	git_patch *patch = payload;
	diff_patch_hunk *hunk;

	GIT_UNUSED(delta);

	hunk = git_array_alloc(patch->hunks);
	GITERR_CHECK_ALLOC(hunk);

	memcpy(&hunk->hunk, hunk_, sizeof(hunk->hunk));

	patch->header_size += hunk_->header_len;

	hunk->line_start = git_array_size(patch->lines);
	hunk->line_count = 0;

	return 0;
}

static int diff_patch_line_cb(
	const git_diff_delta *delta,
	const git_diff_hunk *hunk_,
	const git_diff_line *line_,
	void *payload)
{
	git_patch *patch = payload;
	diff_patch_hunk *hunk;
	git_diff_line   *line;

	GIT_UNUSED(delta);
	GIT_UNUSED(hunk_);

	hunk = git_array_last(patch->hunks);
	assert(hunk); /* programmer error if no hunk is available */

	line = git_array_alloc(patch->lines);
	GITERR_CHECK_ALLOC(line);

	memcpy(line, line_, sizeof(*line));

	/* do some bookkeeping so we can provide old/new line numbers */

	patch->content_size += line->content_len;

	if (line->origin == GIT_DIFF_LINE_ADDITION ||
		line->origin == GIT_DIFF_LINE_DELETION)
		patch->content_size += 1;
	else if (line->origin == GIT_DIFF_LINE_CONTEXT) {
		patch->content_size += 1;
		patch->context_size += line->content_len + 1;
	} else if (line->origin == GIT_DIFF_LINE_CONTEXT_EOFNL)
		patch->context_size += line->content_len;

	hunk->line_count++;

	return 0;
}

static void diff_output_init(
	git_diff_output *out,
	const git_diff_options *opts,
	git_diff_file_cb file_cb,
	git_diff_binary_cb binary_cb,
	git_diff_hunk_cb hunk_cb,
	git_diff_line_cb data_cb,
	void *payload)
{
	GIT_UNUSED(opts);

	memset(out, 0, sizeof(*out));

	out->file_cb = file_cb;
	out->binary_cb = binary_cb;
	out->hunk_cb = hunk_cb;
	out->data_cb = data_cb;
	out->payload = payload;
}

static void diff_output_to_patch(git_diff_output *out, git_patch *patch)
{
	diff_output_init(
		out,
		NULL,
		diff_patch_file_cb,
		diff_patch_binary_cb,
		diff_patch_hunk_cb,
		diff_patch_line_cb,
		patch);
}
