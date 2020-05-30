/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_global_h__
#define INCLUDE_global_h__

#include "common.h"

#include "mwindow.h"
#include "hash.h"

typedef struct {
	git_error *last_error;
	git_error error_t;
	git_buf error_buf;
	char oid_fmt[GIT_OID_HEXSZ+1];

	/* On Windows, this is the current child thread that was started by
	 * `git_thread_create`.  This is used to set the thread's exit code
	 * when terminated by `git_thread_exit`.  It is unused on POSIX.
	 */
	git_thread *current_thread;
} git_global_st;

git_global_st *git__global_state(void);

extern git_mutex git__mwindow_mutex;

#define GIT_GLOBAL (git__global_state())

typedef void (*git_global_shutdown_fn)(void);

extern void git__on_shutdown(git_global_shutdown_fn callback);

extern const char *git_libgit2__user_agent(void);
extern const char *git_libgit2__ssl_ciphers(void);

#endif
