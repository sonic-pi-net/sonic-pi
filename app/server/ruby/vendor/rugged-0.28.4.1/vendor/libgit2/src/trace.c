/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "trace.h"

#include "buffer.h"
#include "global.h"
#include "git2/trace.h"

#ifdef GIT_TRACE

struct git_trace_data git_trace__data = {0};

#endif

int git_trace_set(git_trace_level_t level, git_trace_cb callback)
{
#ifdef GIT_TRACE
	assert(level == 0 || callback != NULL);

	git_trace__data.level = level;
	git_trace__data.callback = callback;
	GIT_MEMORY_BARRIER;

	return 0;
#else
	GIT_UNUSED(level);
	GIT_UNUSED(callback);

	git_error_set(GIT_ERROR_INVALID,
		"this version of libgit2 was not built with tracing.");
	return -1;
#endif
}
