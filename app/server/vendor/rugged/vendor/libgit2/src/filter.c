/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "fileops.h"
#include "hash.h"
#include "filter.h"
#include "repository.h"
#include "global.h"
#include "git2/sys/filter.h"
#include "git2/config.h"
#include "blob.h"
#include "attr_file.h"
#include "array.h"

struct git_filter_source {
	git_repository *repo;
	const char     *path;
	git_oid         oid;  /* zero if unknown (which is likely) */
	uint16_t        filemode; /* zero if unknown */
	git_filter_mode_t mode;
	uint32_t        options;
};

typedef struct {
	git_filter *filter;
	void *payload;
} git_filter_entry;

struct git_filter_list {
	git_array_t(git_filter_entry) filters;
	git_filter_source source;
	char path[GIT_FLEX_ARRAY];
};

typedef struct {
	char *filter_name;
	git_filter *filter;
	int priority;
	int initialized;
	size_t nattrs, nmatches;
	char *attrdata;
	const char *attrs[GIT_FLEX_ARRAY];
} git_filter_def;

static int filter_def_priority_cmp(const void *a, const void *b)
{
	int pa = ((const git_filter_def *)a)->priority;
	int pb = ((const git_filter_def *)b)->priority;
	return (pa < pb) ? -1 : (pa > pb) ? 1 : 0;
}

struct filter_registry {
	git_vector filters;
};

static struct filter_registry *git__filter_registry = NULL;

static void filter_registry_shutdown(void)
{
	struct filter_registry *reg = NULL;
	size_t pos;
	git_filter_def *fdef;

	if ((reg = git__swap(git__filter_registry, NULL)) == NULL)
		return;

	git_vector_foreach(&reg->filters, pos, fdef) {
		if (fdef->filter && fdef->filter->shutdown) {
			fdef->filter->shutdown(fdef->filter);
			fdef->initialized = false;
		}

		git__free(fdef->filter_name);
		git__free(fdef->attrdata);
		git__free(fdef);
	}

	git_vector_free(&reg->filters);
	git__free(reg);
}

static int filter_registry_initialize(void)
{
	int error = 0;
	struct filter_registry *reg;

	if (git__filter_registry)
		return 0;

	reg = git__calloc(1, sizeof(struct filter_registry));
	GITERR_CHECK_ALLOC(reg);

	if ((error = git_vector_init(
			&reg->filters, 2, filter_def_priority_cmp)) < 0)
		goto cleanup;

	reg = git__compare_and_swap(&git__filter_registry, NULL, reg);
	if (reg != NULL)
		goto cleanup;

	git__on_shutdown(filter_registry_shutdown);

	/* try to register both default filters */
	{
		git_filter *crlf = git_crlf_filter_new();
		git_filter *ident = git_ident_filter_new();

		if (crlf && git_filter_register(
				GIT_FILTER_CRLF, crlf, GIT_FILTER_CRLF_PRIORITY) < 0)
			crlf = NULL;
		if (ident && git_filter_register(
				GIT_FILTER_IDENT, ident, GIT_FILTER_IDENT_PRIORITY) < 0)
			ident = NULL;

		if (!crlf || !ident)
			return -1;
	}

	return 0;

cleanup:
	git_vector_free(&reg->filters);
	git__free(reg);
	return error;
}

static int filter_def_scan_attrs(
	git_buf *attrs, size_t *nattr, size_t *nmatch, const char *attr_str)
{
	const char *start, *scan = attr_str;
	int has_eq;

	*nattr = *nmatch = 0;

	if (!scan)
		return 0;

	while (*scan) {
		while (git__isspace(*scan)) scan++;

		for (start = scan, has_eq = 0; *scan && !git__isspace(*scan); ++scan) {
			if (*scan == '=')
				has_eq = 1;
		}

		if (scan > start) {
			(*nattr)++;
			if (has_eq || *start == '-' || *start == '+' || *start == '!')
				(*nmatch)++;

			if (has_eq)
				git_buf_putc(attrs, '=');
			git_buf_put(attrs, start, scan - start);
			git_buf_putc(attrs, '\0');
		}
	}

	return 0;
}

static void filter_def_set_attrs(git_filter_def *fdef)
{
	char *scan = fdef->attrdata;
	size_t i;

	for (i = 0; i < fdef->nattrs; ++i) {
		const char *name, *value;

		switch (*scan) {
		case '=':
			name = scan + 1;
			for (scan++; *scan != '='; scan++) /* find '=' */;
			*scan++ = '\0';
			value = scan;
			break;
		case '-':
			name = scan + 1; value = git_attr__false; break;
		case '+':
			name = scan + 1; value = git_attr__true;  break;
		case '!':
			name = scan + 1; value = git_attr__unset; break;
		default:
			name = scan;     value = NULL; break;
		}

		fdef->attrs[i] = name;
		fdef->attrs[i + fdef->nattrs] = value;

		scan += strlen(scan) + 1;
	}
}

static int filter_def_name_key_check(const void *key, const void *fdef)
{
	const char *name =
		fdef ? ((const git_filter_def *)fdef)->filter_name : NULL;
	return name ? git__strcmp(key, name) : -1;
}

static int filter_def_filter_key_check(const void *key, const void *fdef)
{
	const void *filter = fdef ? ((const git_filter_def *)fdef)->filter : NULL;
	return (key == filter) ? 0 : -1;
}

static int filter_registry_find(size_t *pos, const char *name)
{
	return git_vector_search2(
		pos, &git__filter_registry->filters, filter_def_name_key_check, name);
}

static git_filter_def *filter_registry_lookup(size_t *pos, const char *name)
{
	git_filter_def *fdef = NULL;

	if (!filter_registry_find(pos, name))
		fdef = git_vector_get(&git__filter_registry->filters, *pos);

	return fdef;
}

int git_filter_register(
	const char *name, git_filter *filter, int priority)
{
	git_filter_def *fdef;
	size_t nattr = 0, nmatch = 0;
	git_buf attrs = GIT_BUF_INIT;

	assert(name && filter);

	if (filter_registry_initialize() < 0)
		return -1;

	if (!filter_registry_find(NULL, name)) {
		giterr_set(
			GITERR_FILTER, "Attempt to reregister existing filter '%s'", name);
		return GIT_EEXISTS;
	}

	if (filter_def_scan_attrs(&attrs, &nattr, &nmatch, filter->attributes) < 0)
		return -1;

	fdef = git__calloc(
		sizeof(git_filter_def) + 2 * nattr * sizeof(char *), 1);
	GITERR_CHECK_ALLOC(fdef);

	fdef->filter_name = git__strdup(name);
	GITERR_CHECK_ALLOC(fdef->filter_name);

	fdef->filter      = filter;
	fdef->priority    = priority;
	fdef->nattrs      = nattr;
	fdef->nmatches    = nmatch;
	fdef->attrdata    = git_buf_detach(&attrs);

	filter_def_set_attrs(fdef);

	if (git_vector_insert(&git__filter_registry->filters, fdef) < 0) {
		git__free(fdef->filter_name);
		git__free(fdef->attrdata);
		git__free(fdef);
		return -1;
	}

	git_vector_sort(&git__filter_registry->filters);
	return 0;
}

int git_filter_unregister(const char *name)
{
	size_t pos;
	git_filter_def *fdef;

	assert(name);

	/* cannot unregister default filters */
	if (!strcmp(GIT_FILTER_CRLF, name) || !strcmp(GIT_FILTER_IDENT, name)) {
		giterr_set(GITERR_FILTER, "Cannot unregister filter '%s'", name);
		return -1;
	}

	if ((fdef = filter_registry_lookup(&pos, name)) == NULL) {
		giterr_set(GITERR_FILTER, "Cannot find filter '%s' to unregister", name);
		return GIT_ENOTFOUND;
	}

	(void)git_vector_remove(&git__filter_registry->filters, pos);

	if (fdef->initialized && fdef->filter && fdef->filter->shutdown) {
		fdef->filter->shutdown(fdef->filter);
		fdef->initialized = false;
	}

	git__free(fdef->filter_name);
	git__free(fdef->attrdata);
	git__free(fdef);

	return 0;
}

static int filter_initialize(git_filter_def *fdef)
{
	int error = 0;

	if (!fdef->initialized &&
		fdef->filter &&
		fdef->filter->initialize &&
		(error = fdef->filter->initialize(fdef->filter)) < 0)
	{
		/* auto-unregister if initialize fails */
		git_filter_unregister(fdef->filter_name);
		return error;
	}

	fdef->initialized = true;
	return 0;
}

git_filter *git_filter_lookup(const char *name)
{
	size_t pos;
	git_filter_def *fdef;

	if (filter_registry_initialize() < 0)
		return NULL;

	if ((fdef = filter_registry_lookup(&pos, name)) == NULL)
		return NULL;

	if (!fdef->initialized && filter_initialize(fdef) < 0)
		return NULL;

	return fdef->filter;
}

void git_filter_free(git_filter *filter)
{
	git__free(filter);
}

git_repository *git_filter_source_repo(const git_filter_source *src)
{
	return src->repo;
}

const char *git_filter_source_path(const git_filter_source *src)
{
	return src->path;
}

uint16_t git_filter_source_filemode(const git_filter_source *src)
{
	return src->filemode;
}

const git_oid *git_filter_source_id(const git_filter_source *src)
{
	return git_oid_iszero(&src->oid) ? NULL : &src->oid;
}

git_filter_mode_t git_filter_source_mode(const git_filter_source *src)
{
	return src->mode;
}

uint32_t git_filter_source_options(const git_filter_source *src)
{
	return src->options;
}

static int filter_list_new(
	git_filter_list **out, const git_filter_source *src)
{
	git_filter_list *fl = NULL;
	size_t pathlen = src->path ? strlen(src->path) : 0;

	fl = git__calloc(1, sizeof(git_filter_list) + pathlen + 1);
	GITERR_CHECK_ALLOC(fl);

	if (src->path)
		memcpy(fl->path, src->path, pathlen);
	fl->source.repo = src->repo;
	fl->source.path = fl->path;
	fl->source.mode = src->mode;
	fl->source.options = src->options;

	*out = fl;
	return 0;
}

static int filter_list_check_attributes(
	const char ***out, git_filter_def *fdef, const git_filter_source *src)
{
	int error;
	size_t i;
	const char **strs = git__calloc(fdef->nattrs, sizeof(const char *));
	GITERR_CHECK_ALLOC(strs);

	error = git_attr_get_many(
		strs, src->repo, 0, src->path, fdef->nattrs, fdef->attrs);

	/* if no values were found but no matches are needed, it's okay! */
	if (error == GIT_ENOTFOUND && !fdef->nmatches) {
		giterr_clear();
		git__free((void *)strs);
		return 0;
	}

	for (i = 0; !error && i < fdef->nattrs; ++i) {
		const char *want = fdef->attrs[fdef->nattrs + i];
		git_attr_t want_type, found_type;

		if (!want)
			continue;

		want_type  = git_attr_value(want);
		found_type = git_attr_value(strs[i]);

		if (want_type != found_type ||
			(want_type == GIT_ATTR_VALUE_T && strcmp(want, strs[i])))
			error = GIT_ENOTFOUND;
	}

	if (error)
		git__free((void *)strs);
	else
		*out = strs;

	return error;
}

int git_filter_list_new(
	git_filter_list **out,
	git_repository *repo,
	git_filter_mode_t mode,
	uint32_t options)
{
	git_filter_source src = { 0 };
	src.repo = repo;
	src.path = NULL;
	src.mode = mode;
	src.options = options;
	return filter_list_new(out, &src);
}

int git_filter_list_load(
	git_filter_list **filters,
	git_repository *repo,
	git_blob *blob, /* can be NULL */
	const char *path,
	git_filter_mode_t mode,
	uint32_t options)
{
	int error = 0;
	git_filter_list *fl = NULL;
	git_filter_source src = { 0 };
	git_filter_entry *fe;
	size_t idx;
	git_filter_def *fdef;

	if (filter_registry_initialize() < 0)
		return -1;

	src.repo = repo;
	src.path = path;
	src.mode = mode;
	src.options = options;
	if (blob)
		git_oid_cpy(&src.oid, git_blob_id(blob));

	git_vector_foreach(&git__filter_registry->filters, idx, fdef) {
		const char **values = NULL;
		void *payload = NULL;

		if (!fdef || !fdef->filter)
			continue;

		if (fdef->nattrs > 0) {
			error = filter_list_check_attributes(&values, fdef, &src);
			if (error == GIT_ENOTFOUND) {
				error = 0;
				continue;
			} else if (error < 0)
				break;
		}

		if (!fdef->initialized && (error = filter_initialize(fdef)) < 0)
			break;

		if (fdef->filter->check)
			error = fdef->filter->check(
				fdef->filter, &payload, &src, values);

		git__free((void *)values);

		if (error == GIT_PASSTHROUGH)
			error = 0;
		else if (error < 0)
			break;
		else {
			if (!fl && (error = filter_list_new(&fl, &src)) < 0)
				return error;

			fe = git_array_alloc(fl->filters);
			GITERR_CHECK_ALLOC(fe);
			fe->filter  = fdef->filter;
			fe->payload = payload;
		}
	}

	if (error && fl != NULL) {
		git_array_clear(fl->filters);
		git__free(fl);
		fl = NULL;
	}

	*filters = fl;
	return error;
}

void git_filter_list_free(git_filter_list *fl)
{
	uint32_t i;

	if (!fl)
		return;

	for (i = 0; i < git_array_size(fl->filters); ++i) {
		git_filter_entry *fe = git_array_get(fl->filters, i);
		if (fe->filter->cleanup)
			fe->filter->cleanup(fe->filter, fe->payload);
	}

	git_array_clear(fl->filters);
	git__free(fl);
}

int git_filter_list_push(
	git_filter_list *fl, git_filter *filter, void *payload)
{
	int error = 0;
	size_t pos;
	git_filter_def *fdef;
	git_filter_entry *fe;

	assert(fl && filter);

	if (git_vector_search2(
			&pos, &git__filter_registry->filters,
			filter_def_filter_key_check, filter) < 0) {
		giterr_set(GITERR_FILTER, "Cannot use an unregistered filter");
		return -1;
	}

	fdef = git_vector_get(&git__filter_registry->filters, pos);

	if (!fdef->initialized && (error = filter_initialize(fdef)) < 0)
		return error;

	fe = git_array_alloc(fl->filters);
	GITERR_CHECK_ALLOC(fe);
	fe->filter  = filter;
	fe->payload = payload;

	return 0;
}

size_t git_filter_list_length(const git_filter_list *fl)
{
	return fl ? git_array_size(fl->filters) : 0;
}

static int filter_list_out_buffer_from_raw(
	git_buf *out, const void *ptr, size_t size)
{
	if (git_buf_is_allocated(out))
		git_buf_free(out);

	if (!size) {
		git_buf_init(out, 0);
	} else {
		out->ptr   = (char *)ptr;
		out->asize = 0;
		out->size  = size;
	}

	return 0;
}

int git_filter_list_apply_to_data(
	git_buf *tgt, git_filter_list *fl, git_buf *src)
{
	int error = 0;
	uint32_t i;
	git_buf *dbuffer[2], local = GIT_BUF_INIT;
	unsigned int si = 0;

	git_buf_sanitize(tgt);
	git_buf_sanitize(src);

	if (!fl)
		return filter_list_out_buffer_from_raw(tgt, src->ptr, src->size);

	dbuffer[0] = src;
	dbuffer[1] = tgt;

	/* if `src` buffer is reallocable, then use it, otherwise copy it */
	if (!git_buf_is_allocated(src)) {
		if (git_buf_set(&local, src->ptr, src->size) < 0)
			return -1;
		dbuffer[0] = &local;
	}

	for (i = 0; i < git_array_size(fl->filters); ++i) {
		unsigned int di = 1 - si;
		uint32_t fidx = (fl->source.mode == GIT_FILTER_TO_WORKTREE) ?
			i : git_array_size(fl->filters) - 1 - i;
		git_filter_entry *fe = git_array_get(fl->filters, fidx);

		dbuffer[di]->size = 0;

		/* Apply the filter from dbuffer[src] to the other buffer;
		 * if the filtering is canceled by the user mid-filter,
		 * we skip to the next filter without changing the source
		 * of the double buffering (so that the text goes through
		 * cleanly).
		 */

		error = fe->filter->apply(
			fe->filter, &fe->payload, dbuffer[di], dbuffer[si], &fl->source);

		if (error == GIT_PASSTHROUGH) {
			/* PASSTHROUGH means filter decided not to process the buffer */
			error = 0;
		} else if (!error) {
			git_buf_sanitize(dbuffer[di]); /* force NUL termination */
			si = di; /* swap buffers */
		} else {
			tgt->size = 0;
			goto cleanup;
		}
	}

	/* Ensure that the output ends up in dbuffer[1] (i.e. the dest) */
	if (si != 1)
		git_buf_swap(dbuffer[0], dbuffer[1]);

cleanup:
	git_buf_free(&local); /* don't leak if we allocated locally */

	return error;
}

int git_filter_list_apply_to_file(
	git_buf *out,
	git_filter_list *filters,
	git_repository *repo,
	const char *path)
{
	int error;
	const char *base = repo ? git_repository_workdir(repo) : NULL;
	git_buf abspath = GIT_BUF_INIT, raw = GIT_BUF_INIT;

	if (!(error = git_path_join_unrooted(&abspath, path, base, NULL)) &&
		!(error = git_futils_readbuffer(&raw, abspath.ptr)))
	{
		error = git_filter_list_apply_to_data(out, filters, &raw);

		git_buf_free(&raw);
	}

	git_buf_free(&abspath);
	return error;
}

int git_filter_list_apply_to_blob(
	git_buf *out,
	git_filter_list *filters,
	git_blob *blob)
{
	git_buf in = GIT_BUF_INIT;
	git_off_t rawsize = git_blob_rawsize(blob);

	if (!git__is_sizet(rawsize)) {
		giterr_set(GITERR_OS, "Blob is too large to filter");
		return -1;
	}

	in.ptr   = (char *)git_blob_rawcontent(blob);
	in.asize = 0;
	in.size  = (size_t)rawsize;

	if (filters)
		git_oid_cpy(&filters->source.oid, git_blob_id(blob));

	return git_filter_list_apply_to_data(out, filters, &in);
}
