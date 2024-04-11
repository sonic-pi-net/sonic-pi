/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "threadstate.h"
#include "posix.h"
#include "str.h"
#include "libgit2.h"

/********************************************
 * New error handling
 ********************************************/

static git_error oom_error = {
	"Out of memory",
	GIT_ERROR_NOMEMORY
};

static git_error uninitialized_error = {
	"libgit2 has not been initialized; you must call git_libgit2_init",
	GIT_ERROR_INVALID
};

static git_error tlsdata_error = {
	"thread-local data initialization failure",
	GIT_ERROR
};

static void set_error_from_buffer(int error_class)
{
	git_threadstate *threadstate = git_threadstate_get();
	git_error *error;
	git_str *buf;

	if (!threadstate)
		return;

	error = &threadstate->error_t;
	buf = &threadstate->error_buf;

	error->message = buf->ptr;
	error->klass = error_class;

	threadstate->last_error = error;
}

static void set_error(int error_class, char *string)
{
	git_threadstate *threadstate = git_threadstate_get();
	git_str *buf;

	if (!threadstate)
		return;

	buf = &threadstate->error_buf;

	git_str_clear(buf);

	if (string) {
		git_str_puts(buf, string);
		git__free(string);
	}

	set_error_from_buffer(error_class);
}

void git_error_set_oom(void)
{
	git_threadstate *threadstate = git_threadstate_get();

	if (!threadstate)
		return;

	threadstate->last_error = &oom_error;
}

void git_error_set(int error_class, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	git_error_vset(error_class, fmt, ap);
	va_end(ap);
}

void git_error_vset(int error_class, const char *fmt, va_list ap)
{
#ifdef GIT_WIN32
	DWORD win32_error_code = (error_class == GIT_ERROR_OS) ? GetLastError() : 0;
#endif

	git_threadstate *threadstate = git_threadstate_get();
	int error_code = (error_class == GIT_ERROR_OS) ? errno : 0;
	git_str *buf;

	if (!threadstate)
		return;

	buf = &threadstate->error_buf;

	git_str_clear(buf);

	if (fmt) {
		git_str_vprintf(buf, fmt, ap);
		if (error_class == GIT_ERROR_OS)
			git_str_PUTS(buf, ": ");
	}

	if (error_class == GIT_ERROR_OS) {
#ifdef GIT_WIN32
		char *win32_error = git_win32_get_error_message(win32_error_code);
		if (win32_error) {
			git_str_puts(buf, win32_error);
			git__free(win32_error);

			SetLastError(0);
		}
		else
#endif
		if (error_code)
			git_str_puts(buf, strerror(error_code));

		if (error_code)
			errno = 0;
	}

	if (!git_str_oom(buf))
		set_error_from_buffer(error_class);
}

int git_error_set_str(int error_class, const char *string)
{
	git_threadstate *threadstate = git_threadstate_get();
	git_str *buf;

	GIT_ASSERT_ARG(string);

	if (!threadstate)
		return -1;

	buf = &threadstate->error_buf;

	git_str_clear(buf);
	git_str_puts(buf, string);

	if (git_str_oom(buf))
		return -1;

	set_error_from_buffer(error_class);
	return 0;
}

void git_error_clear(void)
{
	git_threadstate *threadstate = git_threadstate_get();

	if (!threadstate)
		return;

	if (threadstate->last_error != NULL) {
		set_error(0, NULL);
		threadstate->last_error = NULL;
	}

	errno = 0;
#ifdef GIT_WIN32
	SetLastError(0);
#endif
}

const git_error *git_error_last(void)
{
	git_threadstate *threadstate;

	/* If the library is not initialized, return a static error. */
	if (!git_libgit2_init_count())
		return &uninitialized_error;

	if ((threadstate = git_threadstate_get()) == NULL)
		return &tlsdata_error;

	return threadstate->last_error;
}

int git_error_state_capture(git_error_state *state, int error_code)
{
	git_threadstate *threadstate = git_threadstate_get();
	git_error *error;
	git_str *error_buf;

	if (!threadstate)
		return -1;

	error = threadstate->last_error;
	error_buf = &threadstate->error_buf;

	memset(state, 0, sizeof(git_error_state));

	if (!error_code)
		return 0;

	state->error_code = error_code;
	state->oom = (error == &oom_error);

	if (error) {
		state->error_msg.klass = error->klass;

		if (state->oom)
			state->error_msg.message = oom_error.message;
		else
			state->error_msg.message = git_str_detach(error_buf);
	}

	git_error_clear();
	return error_code;
}

int git_error_state_restore(git_error_state *state)
{
	int ret = 0;

	git_error_clear();

	if (state && state->error_msg.message) {
		if (state->oom)
			git_error_set_oom();
		else
			set_error(state->error_msg.klass, state->error_msg.message);

		ret = state->error_code;
		memset(state, 0, sizeof(git_error_state));
	}

	return ret;
}

void git_error_state_free(git_error_state *state)
{
	if (!state)
		return;

	if (!state->oom)
		git__free(state->error_msg.message);

	memset(state, 0, sizeof(git_error_state));
}

int git_error_system_last(void)
{
#ifdef GIT_WIN32
	return GetLastError();
#else
	return errno;
#endif
}

void git_error_system_set(int code)
{
#ifdef GIT_WIN32
	SetLastError(code);
#else
	errno = code;
#endif
}

/* Deprecated error values and functions */

#ifndef GIT_DEPRECATE_HARD
const git_error *giterr_last(void)
{
	return git_error_last();
}

void giterr_clear(void)
{
	git_error_clear();
}

void giterr_set_str(int error_class, const char *string)
{
	git_error_set_str(error_class, string);
}

void giterr_set_oom(void)
{
	git_error_set_oom();
}
#endif
