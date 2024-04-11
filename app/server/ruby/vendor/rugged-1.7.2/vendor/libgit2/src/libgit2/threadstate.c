/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "threadstate.h"
#include "runtime.h"

/**
 * Handle the thread-local state
 *
 * `git_threadstate_global_init` will be called as part
 * of `git_libgit2_init` (which itself must be called
 * before calling any other function in the library).
 *
 * This function allocates a TLS index to store the per-
 * thread state.
 *
 * Any internal method that requires thread-local state
 * will then call `git_threadstate_get()` which returns a
 * pointer to the thread-local state structure; this
 * structure is lazily allocated on each thread.
 *
 * This mechanism will register a shutdown handler
 * (`git_threadstate_global_shutdown`) which will free the
 * TLS index.  This shutdown handler will be called by
 * `git_libgit2_shutdown`.
 */

static git_tlsdata_key tls_key;

static void threadstate_dispose(git_threadstate *threadstate)
{
	if (!threadstate)
		return;

	if (threadstate->error_t.message != git_str__initstr)
		git__free(threadstate->error_t.message);
	threadstate->error_t.message = NULL;
}

static void GIT_SYSTEM_CALL threadstate_free(void *threadstate)
{
	threadstate_dispose(threadstate);
	git__free(threadstate);
}

static void git_threadstate_global_shutdown(void)
{
	git_threadstate *threadstate;

	threadstate = git_tlsdata_get(tls_key);
	git_tlsdata_set(tls_key, NULL);

	threadstate_dispose(threadstate);
	git__free(threadstate);

	git_tlsdata_dispose(tls_key);
}

int git_threadstate_global_init(void)
{
	if (git_tlsdata_init(&tls_key, &threadstate_free) != 0)
		return -1;

	return git_runtime_shutdown_register(git_threadstate_global_shutdown);
}

git_threadstate *git_threadstate_get(void)
{
	git_threadstate *threadstate;

	if ((threadstate = git_tlsdata_get(tls_key)) != NULL)
		return threadstate;

	/*
	 * Avoid git__malloc here, since if it fails, it sets an error
	 * message, which requires thread state, which would allocate
	 * here, which would fail, which would set an error message...
	 */

	if ((threadstate = git__allocator.gmalloc(sizeof(git_threadstate),
			__FILE__, __LINE__)) == NULL)
		return NULL;

	memset(threadstate, 0, sizeof(git_threadstate));

	if (git_str_init(&threadstate->error_buf, 0) < 0) {
		git__allocator.gfree(threadstate);
		return NULL;
	}

	git_tlsdata_set(tls_key, threadstate);
	return threadstate;
}
