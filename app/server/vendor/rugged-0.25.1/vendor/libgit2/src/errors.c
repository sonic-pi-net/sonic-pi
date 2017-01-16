/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"
#include "global.h"
#include "posix.h"
#include "buffer.h"

/********************************************
 * New error handling
 ********************************************/

static git_error g_git_oom_error = {
	"Out of memory",
	GITERR_NOMEMORY
};

static void set_error_from_buffer(int error_class)
{
	git_error *error = &GIT_GLOBAL->error_t;
	git_buf *buf = &GIT_GLOBAL->error_buf;

	error->message = buf->ptr;
	error->klass = error_class;

	GIT_GLOBAL->last_error = error;
}

static void set_error(int error_class, char *string)
{
	git_buf *buf = &GIT_GLOBAL->error_buf;

	git_buf_clear(buf);
	if (string) {
		git_buf_puts(buf, string);
		git__free(string);
	}

	set_error_from_buffer(error_class);
}

void giterr_set_oom(void)
{
	GIT_GLOBAL->last_error = &g_git_oom_error;
}

void giterr_set(int error_class, const char *string, ...)
{
	va_list arglist;
#ifdef GIT_WIN32
	DWORD win32_error_code = (error_class == GITERR_OS) ? GetLastError() : 0;
#endif
	int error_code = (error_class == GITERR_OS) ? errno : 0;
	git_buf *buf = &GIT_GLOBAL->error_buf;

	git_buf_clear(buf);
	if (string) {
		va_start(arglist, string);
		git_buf_vprintf(buf, string, arglist);
		va_end(arglist);

		if (error_class == GITERR_OS)
			git_buf_PUTS(buf, ": ");
	}

	if (error_class == GITERR_OS) {
#ifdef GIT_WIN32
		char * win32_error = git_win32_get_error_message(win32_error_code);
		if (win32_error) {
			git_buf_puts(buf, win32_error);
			git__free(win32_error);

			SetLastError(0);
		}
		else
#endif
		if (error_code)
			git_buf_puts(buf, strerror(error_code));

		if (error_code)
			errno = 0;
	}

	if (!git_buf_oom(buf))
		set_error_from_buffer(error_class);
}

void giterr_set_str(int error_class, const char *string)
{
	git_buf *buf = &GIT_GLOBAL->error_buf;

	assert(string);

	if (!string)
		return;

	git_buf_clear(buf);
	git_buf_puts(buf, string);
	if (!git_buf_oom(buf))
		set_error_from_buffer(error_class);
}

int giterr_set_regex(const regex_t *regex, int error_code)
{
	char error_buf[1024];

	assert(error_code);

	regerror(error_code, regex, error_buf, sizeof(error_buf));
	giterr_set_str(GITERR_REGEX, error_buf);

	if (error_code == REG_NOMATCH)
		return GIT_ENOTFOUND;

	return GIT_EINVALIDSPEC;
}

void giterr_clear(void)
{
	if (GIT_GLOBAL->last_error != NULL) {
		set_error(0, NULL);
		GIT_GLOBAL->last_error = NULL;
	}

	errno = 0;
#ifdef GIT_WIN32
	SetLastError(0);
#endif
}

const git_error *giterr_last(void)
{
	return GIT_GLOBAL->last_error;
}

int giterr_state_capture(git_error_state *state, int error_code)
{
	git_error *error = GIT_GLOBAL->last_error;
	git_buf *error_buf = &GIT_GLOBAL->error_buf;

	memset(state, 0, sizeof(git_error_state));

	if (!error_code)
		return 0;

	state->error_code = error_code;
	state->oom = (error == &g_git_oom_error);

	if (error) {
		state->error_msg.klass = error->klass;

		if (state->oom)
			state->error_msg.message = g_git_oom_error.message;
		else
			state->error_msg.message = git_buf_detach(error_buf);
	}

	giterr_clear();
	return error_code;
}

int giterr_state_restore(git_error_state *state)
{
	int ret = 0;

	giterr_clear();

	if (state && state->error_msg.message) {
		if (state->oom)
			giterr_set_oom();
		else
			set_error(state->error_msg.klass, state->error_msg.message);

		ret = state->error_code;
		memset(state, 0, sizeof(git_error_state));
	}

	return ret;
}

void giterr_state_free(git_error_state *state)
{
	if (!state)
		return;

	if (!state->oom)
		git__free(state->error_msg.message);

	memset(state, 0, sizeof(git_error_state));
}

int giterr_system_last(void)
{
#ifdef GIT_WIN32
	return GetLastError();
#else
	return errno;
#endif
}

void giterr_system_set(int code)
{
#ifdef GIT_WIN32
	SetLastError(code);
#else
	errno = code;
#endif
}
