/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_trace_h__
#define INCLUDE_trace_h__

#include "common.h"

#include <git2/trace.h>
#include "buffer.h"

#ifdef GIT_TRACE

struct git_trace_data {
	git_trace_level_t level;
	git_trace_callback callback;
};

extern struct git_trace_data git_trace__data;

GIT_INLINE(void) git_trace__write_fmt(
	git_trace_level_t level,
	const char *fmt, ...)
{
	git_trace_callback callback = git_trace__data.callback;
	git_buf message = GIT_BUF_INIT;
	va_list ap;

	va_start(ap, fmt);
	git_buf_vprintf(&message, fmt, ap);
	va_end(ap);

	callback(level, git_buf_cstr(&message));

	git_buf_free(&message);
}

#define git_trace_level()		(git_trace__data.level)
#define git_trace(l, ...)		{ \
									if (git_trace__data.level >= l && \
										git_trace__data.callback != NULL) { \
										git_trace__write_fmt(l, __VA_ARGS__); \
									} \
								}

#else

GIT_INLINE(void) git_trace__null(
	git_trace_level_t level,
	const char *fmt, ...)
{
	GIT_UNUSED(level);
	GIT_UNUSED(fmt);
}

#define git_trace_level()		((void)0)
#define git_trace			git_trace__null

#endif

#endif
