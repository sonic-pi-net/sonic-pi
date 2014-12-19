/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2/attr.h"
#include "git2/blob.h"
#include "git2/index.h"
#include "git2/sys/filter.h"

#include "common.h"
#include "fileops.h"
#include "hash.h"
#include "filter.h"
#include "buf_text.h"
#include "repository.h"

struct crlf_attrs {
	int crlf_action;
	int eol;
	int auto_crlf;
	int safe_crlf;
};

struct crlf_filter {
	git_filter f;
};

static int check_crlf(const char *value)
{
	if (GIT_ATTR_TRUE(value))
		return GIT_CRLF_TEXT;

	if (GIT_ATTR_FALSE(value))
		return GIT_CRLF_BINARY;

	if (GIT_ATTR_UNSPECIFIED(value))
		return GIT_CRLF_GUESS;

	if (strcmp(value, "input") == 0)
		return GIT_CRLF_INPUT;

	if (strcmp(value, "auto") == 0)
		return GIT_CRLF_AUTO;

	return GIT_CRLF_GUESS;
}

static int check_eol(const char *value)
{
	if (GIT_ATTR_UNSPECIFIED(value))
		return GIT_EOL_UNSET;

	if (strcmp(value, "lf") == 0)
		return GIT_EOL_LF;

	if (strcmp(value, "crlf") == 0)
		return GIT_EOL_CRLF;

	return GIT_EOL_UNSET;
}

static int crlf_input_action(struct crlf_attrs *ca)
{
	if (ca->crlf_action == GIT_CRLF_BINARY)
		return GIT_CRLF_BINARY;

	if (ca->eol == GIT_EOL_LF)
		return GIT_CRLF_INPUT;

	if (ca->eol == GIT_EOL_CRLF)
		return GIT_CRLF_CRLF;

	return ca->crlf_action;
}

static int has_cr_in_index(const git_filter_source *src)
{
	git_repository *repo = git_filter_source_repo(src);
	const char *path = git_filter_source_path(src);
	git_index *index;
	const git_index_entry *entry;
	git_blob *blob;
	const void *blobcontent;
	git_off_t blobsize;
	bool found_cr;

	if (!path)
		return false;

	if (git_repository_index__weakptr(&index, repo) < 0) {
		giterr_clear();
		return false;
	}

	if (!(entry = git_index_get_bypath(index, path, 0)) &&
		!(entry = git_index_get_bypath(index, path, 1)))
		return false;

	if (!S_ISREG(entry->mode)) /* don't crlf filter non-blobs */
		return true;

	if (git_blob_lookup(&blob, repo, &entry->id) < 0)
		return false;

	blobcontent = git_blob_rawcontent(blob);
	blobsize    = git_blob_rawsize(blob);
	if (!git__is_sizet(blobsize))
		blobsize = (size_t)-1;

	found_cr = (blobcontent != NULL &&
		blobsize > 0 &&
		memchr(blobcontent, '\r', (size_t)blobsize) != NULL);

	git_blob_free(blob);
	return found_cr;
}

static int crlf_apply_to_odb(
	struct crlf_attrs *ca,
	git_buf *to,
	const git_buf *from,
	const git_filter_source *src)
{
	/* Empty file? Nothing to do */
	if (!git_buf_len(from))
		return 0;

	/* Heuristics to see if we can skip the conversion.
	 * Straight from Core Git.
	 */
	if (ca->crlf_action == GIT_CRLF_AUTO || ca->crlf_action == GIT_CRLF_GUESS) {
		git_buf_text_stats stats;

		/* Check heuristics for binary vs text - returns true if binary */
		if (git_buf_text_gather_stats(&stats, from, false))
			return GIT_PASSTHROUGH;

		/* If there are no CR characters to filter out, then just pass */
		if (!stats.cr)
			return GIT_PASSTHROUGH;

		/* If safecrlf is enabled, sanity-check the result. */
		if (stats.cr != stats.crlf || stats.lf != stats.crlf) {
			switch (ca->safe_crlf) {
			case GIT_SAFE_CRLF_FAIL:
				giterr_set(
					GITERR_FILTER, "LF would be replaced by CRLF in '%s'",
					git_filter_source_path(src));
				return -1;
			case GIT_SAFE_CRLF_WARN:
				/* TODO: issue warning when warning API is available */;
				break;
			default:
				break;
			}
		}

		/*
		 * We're currently not going to even try to convert stuff
		 * that has bare CR characters. Does anybody do that crazy
		 * stuff?
		 */
		if (stats.cr != stats.crlf)
			return GIT_PASSTHROUGH;

		if (ca->crlf_action == GIT_CRLF_GUESS) {
			/*
			 * If the file in the index has any CR in it, do not convert.
			 * This is the new safer autocrlf handling.
			 */
			if (has_cr_in_index(src))
				return GIT_PASSTHROUGH;
		}

		if (!stats.cr)
			return GIT_PASSTHROUGH;
	}

	/* Actually drop the carriage returns */
	return git_buf_text_crlf_to_lf(to, from);
}

static const char *line_ending(struct crlf_attrs *ca)
{
	switch (ca->crlf_action) {
	case GIT_CRLF_BINARY:
	case GIT_CRLF_INPUT:
		return "\n";

	case GIT_CRLF_CRLF:
		return "\r\n";

	case GIT_CRLF_AUTO:
	case GIT_CRLF_TEXT:
	case GIT_CRLF_GUESS:
		break;

	default:
		goto line_ending_error;
	}

	switch (ca->eol) {
	case GIT_EOL_UNSET:
		return GIT_EOL_NATIVE == GIT_EOL_CRLF ? "\r\n" : "\n";

	case GIT_EOL_CRLF:
		return "\r\n";

	case GIT_EOL_LF:
		return "\n";

	default:
		goto line_ending_error;
	}

line_ending_error:
	giterr_set(GITERR_INVALID, "Invalid input to line ending filter");
	return NULL;
}

static int crlf_apply_to_workdir(
	struct crlf_attrs *ca, git_buf *to, const git_buf *from)
{
	const char *workdir_ending = NULL;

	/* Empty file? Nothing to do. */
	if (git_buf_len(from) == 0)
		return 0;

	/* Don't filter binary files */
	if (git_buf_text_is_binary(from))
		return GIT_PASSTHROUGH;

	/* Determine proper line ending */
	workdir_ending = line_ending(ca);
	if (!workdir_ending)
		return -1;

	/* only LF->CRLF conversion is supported, do nothing on LF platforms */
	if (strcmp(workdir_ending, "\r\n") != 0)
		return GIT_PASSTHROUGH;

	return git_buf_text_lf_to_crlf(to, from);
}

static int crlf_check(
	git_filter        *self,
	void              **payload, /* points to NULL ptr on entry, may be set */
	const git_filter_source *src,
	const char **attr_values)
{
	int error;
	struct crlf_attrs ca;

	GIT_UNUSED(self);

	if (!attr_values) {
		ca.crlf_action = GIT_CRLF_GUESS;
		ca.eol = GIT_EOL_UNSET;
	} else {
		ca.crlf_action = check_crlf(attr_values[2]); /* text */
		if (ca.crlf_action == GIT_CRLF_GUESS)
			ca.crlf_action = check_crlf(attr_values[0]); /* clrf */
		ca.eol = check_eol(attr_values[1]); /* eol */
	}
	ca.auto_crlf = GIT_AUTO_CRLF_DEFAULT;

	/*
	 * Use the core Git logic to see if we should perform CRLF for this file
	 * based on its attributes & the value of `core.autocrlf`
	 */
	ca.crlf_action = crlf_input_action(&ca);

	if (ca.crlf_action == GIT_CRLF_BINARY)
		return GIT_PASSTHROUGH;

	if (ca.crlf_action == GIT_CRLF_GUESS ||
		(ca.crlf_action == GIT_CRLF_AUTO &&
		git_filter_source_mode(src) == GIT_FILTER_SMUDGE)) {

		error = git_repository__cvar(
			&ca.auto_crlf, git_filter_source_repo(src), GIT_CVAR_AUTO_CRLF);
		if (error < 0)
			return error;

		if (ca.crlf_action == GIT_CRLF_GUESS &&
			ca.auto_crlf == GIT_AUTO_CRLF_FALSE)
			return GIT_PASSTHROUGH;

		if (ca.auto_crlf == GIT_AUTO_CRLF_INPUT &&
			git_filter_source_mode(src) == GIT_FILTER_SMUDGE)
			return GIT_PASSTHROUGH;
	}

	if (git_filter_source_mode(src) == GIT_FILTER_CLEAN) {
		error = git_repository__cvar(
			&ca.safe_crlf, git_filter_source_repo(src), GIT_CVAR_SAFE_CRLF);
		if (error < 0)
			return error;

		/* downgrade FAIL to WARN if ALLOW_UNSAFE option is used */
		if ((git_filter_source_options(src) & GIT_FILTER_OPT_ALLOW_UNSAFE) &&
			ca.safe_crlf == GIT_SAFE_CRLF_FAIL)
			ca.safe_crlf = GIT_SAFE_CRLF_WARN;
	}

	*payload = git__malloc(sizeof(ca));
	GITERR_CHECK_ALLOC(*payload);
	memcpy(*payload, &ca, sizeof(ca));

	return 0;
}

static int crlf_apply(
	git_filter    *self,
	void         **payload, /* may be read and/or set */
	git_buf       *to,
	const git_buf *from,
	const git_filter_source *src)
{
	/* initialize payload in case `check` was bypassed */
	if (!*payload) {
		int error = crlf_check(self, payload, src, NULL);
		if (error < 0 && error != GIT_PASSTHROUGH)
			return error;
	}

	if (git_filter_source_mode(src) == GIT_FILTER_SMUDGE)
		return crlf_apply_to_workdir(*payload, to, from);
	else
		return crlf_apply_to_odb(*payload, to, from, src);
}

static void crlf_cleanup(
	git_filter *self,
	void       *payload)
{
	GIT_UNUSED(self);
	git__free(payload);
}

git_filter *git_crlf_filter_new(void)
{
	struct crlf_filter *f = git__calloc(1, sizeof(struct crlf_filter));

	f->f.version = GIT_FILTER_VERSION;
	f->f.attributes = "crlf eol text";
	f->f.initialize = NULL;
	f->f.shutdown = git_filter_free;
	f->f.check    = crlf_check;
	f->f.apply    = crlf_apply;
	f->f.cleanup  = crlf_cleanup;

	return (git_filter *)f;
}
