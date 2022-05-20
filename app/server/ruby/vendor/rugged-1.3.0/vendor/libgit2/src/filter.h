/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_filter_h__
#define INCLUDE_filter_h__

#include "common.h"

#include "attr_file.h"
#include "git2/filter.h"
#include "git2/sys/filter.h"

/* Amount of file to examine for NUL byte when checking binary-ness */
#define GIT_FILTER_BYTES_TO_CHECK_NUL 8000

typedef struct {
	git_filter_options options;
	git_attr_session *attr_session;
	git_buf *temp_buf;
} git_filter_session;

#define GIT_FILTER_SESSION_INIT {GIT_FILTER_OPTIONS_INIT, 0}

extern int git_filter_global_init(void);

extern void git_filter_free(git_filter *filter);

extern int git_filter_list__load(
	git_filter_list **filters,
	git_repository *repo,
	git_blob *blob, /* can be NULL */
	const char *path,
	git_filter_mode_t mode,
	git_filter_session *filter_session);

/*
 * The given input buffer will be converted to the given output buffer.
 * The input buffer will be freed (_if_ it was allocated).
 */
extern int git_filter_list__convert_buf(
	git_buf *out,
	git_filter_list *filters,
	git_buf *in);

/*
 * Available filters
 */

extern git_filter *git_crlf_filter_new(void);
extern git_filter *git_ident_filter_new(void);

extern int git_filter_buffered_stream_new(
	git_writestream **out,
	git_filter *filter,
	int (*write_fn)(git_filter *, void **, git_buf *, const git_buf *, const git_filter_source *),
	git_buf *temp_buf,
	void **payload,
	const git_filter_source *source,
	git_writestream *target);

#endif
