/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_errors_h__
#define INCLUDE_errors_h__

#include "common.h"

/*
 * Set the error message for this thread, formatting as needed.
 */
void git_error_set(int error_class, const char *fmt, ...) GIT_FORMAT_PRINTF(2, 3);
void git_error_vset(int error_class, const char *fmt, va_list ap);

/**
 * Set error message for user callback if needed.
 *
 * If the error code in non-zero and no error message is set, this
 * sets a generic error message.
 *
 * @return This always returns the `error_code` parameter.
 */
GIT_INLINE(int) git_error_set_after_callback_function(
	int error_code, const char *action)
{
	if (error_code) {
		const git_error *e = git_error_last();
		if (!e || !e->message)
			git_error_set(e ? e->klass : GIT_ERROR_CALLBACK,
				"%s callback returned %d", action, error_code);
	}
	return error_code;
}

#ifdef GIT_WIN32
#define git_error_set_after_callback(code) \
	git_error_set_after_callback_function((code), __FUNCTION__)
#else
#define git_error_set_after_callback(code) \
	git_error_set_after_callback_function((code), __func__)
#endif

/**
 * Gets the system error code for this thread.
 */
int git_error_system_last(void);

/**
 * Sets the system error code for this thread.
 */
void git_error_system_set(int code);

/**
 * Structure to preserve libgit2 error state
 */
typedef struct {
	int error_code;
	unsigned int oom : 1;
	git_error error_msg;
} git_error_state;

/**
 * Capture current error state to restore later, returning error code.
 * If `error_code` is zero, this does not clear the current error state.
 * You must either restore this error state, or free it.
 */
extern int git_error_state_capture(git_error_state *state, int error_code);

/**
 * Restore error state to a previous value, returning saved error code.
 */
extern int git_error_state_restore(git_error_state *state);

/** Free an error state. */
extern void git_error_state_free(git_error_state *state);

#endif
