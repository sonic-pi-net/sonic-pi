/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_w32_stack_h__
#define INCLUDE_w32_stack_h__

#include "common.h"

#if defined(GIT_MSVC_CRTDBG)

/**
 * This type defines a callback to be used to augment a C stacktrace
 * with "aux" data. This can be used, for example, to allow LibGit2Sharp
 * (or other interpreted consumer libraries) to give us C# stacktrace
 * data for the PInvoke.
 *
 * This callback will be called during crtdbg-instrumented allocs.
 *
 * @param aux_id [out] A returned "aux_id" representing a unique
 * (de-duped at the C# layer) stacktrace.  "aux_id" 0 is reserved
 * to mean no aux stacktrace data.
 */
typedef void (*git_win32__stack__aux_cb_alloc)(unsigned int *aux_id);

/**
 * This type defines a callback to be used to augment the output of
 * a stacktrace.  This will be used to request the C# layer format
 * the C# stacktrace associated with "aux_id" into the provided
 * buffer.
 *
 * This callback will be called during leak reporting.
 *
 * @param aux_id The "aux_id" key associated with a stacktrace.
 * @param aux_msg A buffer where a formatted message should be written.
 * @param aux_msg_len The size of the buffer.
 */
typedef void (*git_win32__stack__aux_cb_lookup)(unsigned int aux_id, char *aux_msg, unsigned int aux_msg_len);

/**
 * Register an "aux" data provider to augment our C stacktrace data.
 *
 * This can be used, for example, to allow LibGit2Sharp (or other
 * interpreted consumer libraries) to give us the C# stacktrace of
 * the PInvoke.
 *
 * If you choose to use this feature, it should be registered during
 * initialization and not changed for the duration of the process.
 */
GIT_EXTERN(int) git_win32__stack__set_aux_cb(
	git_win32__stack__aux_cb_alloc cb_alloc,
	git_win32__stack__aux_cb_lookup cb_lookup);

/**
 * Maximum number of stackframes to record for a
 * single stacktrace.
 */
#define GIT_WIN32__STACK__MAX_FRAMES 30

/**
 * Wrapper containing the raw unprocessed stackframe
 * data for a single stacktrace and any "aux_id".
 *
 * I put the aux_id first so leaks will be sorted by it.
 * So, for example, if a specific callstack in C# leaks
 * a repo handle, all of the pointers within the associated
 * repo pointer will be grouped together.
 */
typedef struct {
	unsigned int aux_id;
	unsigned int nr_frames;
	void *frames[GIT_WIN32__STACK__MAX_FRAMES];
} git_win32__stack__raw_data;


/**
 * Load symbol table data.  This should be done in the primary
 * thread at startup (under a lock if there are other threads
 * active).
 */
void git_win32__stack_init(void);

/**
 * Cleanup symbol table data.  This should be done in the
 * primary thead at shutdown (under a lock if there are other
 * threads active).
 */
void git_win32__stack_cleanup(void);


/**
 * Capture raw stack trace data for the current process/thread.
 *
 * @param skip Number of initial frames to skip.  Pass 0 to
 * begin with the caller of this routine. Pass 1 to begin
 * with its caller.  And so on.
 */
int git_win32__stack_capture(git_win32__stack__raw_data *pdata, int skip);

/**
 * Compare 2 raw stacktraces with the usual -1,0,+1 result.
 * This includes any "aux_id" values in the comparison, so that
 * our de-dup is also "aux" context relative.
 */
int git_win32__stack_compare(
	git_win32__stack__raw_data *d1,
	git_win32__stack__raw_data *d2);

/**
 * Format raw stacktrace data into buffer WITHOUT using any mallocs.
 *
 * @param prefix String written before each frame; defaults to "\t".
 * @param suffix String written after each frame; defaults to "\n".
 */
int git_win32__stack_format(
	char *pbuf, int buf_len,
	const git_win32__stack__raw_data *pdata,
	const char *prefix, const char *suffix);

/**
 * Convenience routine to capture and format stacktrace into
 * a buffer WITHOUT using any mallocs.  This is primarily a
 * wrapper for testing.
 *
 * @param skip Number of initial frames to skip. Pass 0 to
 * begin with the caller of this routine.  Pass 1 to begin
 * with its caller.  And so on.
 * @param prefix String written before each frame; defaults to "\t".
 * @param suffix String written after each frame; defaults to "\n".
 */
int git_win32__stack(
	char * pbuf, int buf_len,
	int skip,
	const char *prefix, const char *suffix);

#endif /* GIT_MSVC_CRTDBG */
#endif /* INCLUDE_w32_stack_h__ */
