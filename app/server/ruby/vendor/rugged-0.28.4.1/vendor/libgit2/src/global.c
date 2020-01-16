/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "global.h"

#include "alloc.h"
#include "hash.h"
#include "sysdir.h"
#include "filter.h"
#include "merge_driver.h"
#include "streams/registry.h"
#include "streams/mbedtls.h"
#include "streams/openssl.h"
#include "thread-utils.h"
#include "git2/global.h"
#include "transports/ssh.h"

#if defined(GIT_MSVC_CRTDBG)
#include "win32/w32_stack.h"
#include "win32/w32_crtdbg_stacktrace.h"
#endif

git_mutex git__mwindow_mutex;

typedef int (*git_global_init_fn)(void);

static git_global_init_fn git__init_callbacks[] = {
	git_allocator_global_init,
	git_hash_global_init,
	git_sysdir_global_init,
	git_filter_global_init,
	git_merge_driver_global_init,
	git_transport_ssh_global_init,
	git_stream_registry_global_init,
	git_openssl_stream_global_init,
	git_mbedtls_stream_global_init,
	git_mwindow_global_init
};

static git_global_shutdown_fn git__shutdown_callbacks[ARRAY_SIZE(git__init_callbacks)];

static git_atomic git__n_shutdown_callbacks;
static git_atomic git__n_inits;
char *git__user_agent;
char *git__ssl_ciphers;

void git__on_shutdown(git_global_shutdown_fn callback)
{
	int count = git_atomic_inc(&git__n_shutdown_callbacks);
	assert(count <= (int) ARRAY_SIZE(git__shutdown_callbacks) && count > 0);
	git__shutdown_callbacks[count - 1] = callback;
}

static void git__global_state_cleanup(git_global_st *st)
{
	if (!st)
		return;

	git__free(st->error_t.message);
	st->error_t.message = NULL;
}

static int init_common(void)
{
	size_t i;
	int ret;

	/* Initialize the CRT debug allocator first, before our first malloc */
#if defined(GIT_MSVC_CRTDBG)
	git_win32__crtdbg_stacktrace_init();
	git_win32__stack_init();
#endif

	/* Initialize subsystems that have global state */
	for (i = 0; i < ARRAY_SIZE(git__init_callbacks); i++)
		if ((ret = git__init_callbacks[i]()) != 0)
			break;

	GIT_MEMORY_BARRIER;

	return ret;
}

static void shutdown_common(void)
{
	int pos;

	/* Shutdown subsystems that have registered */
	for (pos = git_atomic_get(&git__n_shutdown_callbacks);
		pos > 0;
		pos = git_atomic_dec(&git__n_shutdown_callbacks)) {

		git_global_shutdown_fn cb = git__swap(
			git__shutdown_callbacks[pos - 1], NULL);

		if (cb != NULL)
			cb();
	}

	git__free(git__user_agent);
	git__free(git__ssl_ciphers);
}

/**
 * Handle the global state with TLS
 *
 * If libgit2 is built with GIT_THREADS enabled,
 * the `git_libgit2_init()` function must be called
 * before calling any other function of the library.
 *
 * This function allocates a TLS index (using pthreads
 * or the native Win32 API) to store the global state
 * on a per-thread basis.
 *
 * Any internal method that requires global state will
 * then call `git__global_state()` which returns a pointer
 * to the global state structure; this pointer is lazily
 * allocated on each thread.
 *
 * Before shutting down the library, the
 * `git_libgit2_shutdown` method must be called to free
 * the previously reserved TLS index.
 *
 * If libgit2 is built without threading support, the
 * `git__global_statestate()` call returns a pointer to a single,
 * statically allocated global state. The `git_thread_`
 * functions are not available in that case.
 */

/*
 * `git_libgit2_init()` allows subsystems to perform global setup,
 * which may take place in the global scope.  An explicit memory
 * fence exists at the exit of `git_libgit2_init()`.  Without this,
 * CPU cores are free to reorder cache invalidation of `_tls_init`
 * before cache invalidation of the subsystems' newly written global
 * state.
 */
#if defined(GIT_THREADS) && defined(GIT_WIN32)

static DWORD _fls_index;
static volatile LONG _mutex = 0;

static void WINAPI fls_free(void *st)
{
	git__global_state_cleanup(st);
	git__free(st);
}

static int synchronized_threads_init(void)
{
	int error;

	if ((_fls_index = FlsAlloc(fls_free)) == FLS_OUT_OF_INDEXES)
		return -1;

	git_threads_init();

	if (git_mutex_init(&git__mwindow_mutex))
		return -1;

	error = init_common();

	return error;
}

int git_libgit2_init(void)
{
	int ret;

	/* Enter the lock */
	while (InterlockedCompareExchange(&_mutex, 1, 0)) { Sleep(0); }

	/* Only do work on a 0 -> 1 transition of the refcount */
	if ((ret = git_atomic_inc(&git__n_inits)) == 1) {
		if (synchronized_threads_init() < 0)
			ret = -1;
	}

	/* Exit the lock */
	InterlockedExchange(&_mutex, 0);

	return ret;
}

int git_libgit2_shutdown(void)
{
	int ret;

	/* Enter the lock */
	while (InterlockedCompareExchange(&_mutex, 1, 0)) { Sleep(0); }

	/* Only do work on a 1 -> 0 transition of the refcount */
	if ((ret = git_atomic_dec(&git__n_inits)) == 0) {
		shutdown_common();

		FlsFree(_fls_index);
		git_mutex_free(&git__mwindow_mutex);

#if defined(GIT_MSVC_CRTDBG)
		git_win32__crtdbg_stacktrace_cleanup();
		git_win32__stack_cleanup();
#endif
	}

	/* Exit the lock */
	InterlockedExchange(&_mutex, 0);

	return ret;
}

git_global_st *git__global_state(void)
{
	git_global_st *ptr;

	assert(git_atomic_get(&git__n_inits) > 0);

	if ((ptr = FlsGetValue(_fls_index)) != NULL)
		return ptr;

	ptr = git__calloc(1, sizeof(git_global_st));
	if (!ptr)
		return NULL;

	git_buf_init(&ptr->error_buf, 0);

	FlsSetValue(_fls_index, ptr);
	return ptr;
}

#elif defined(GIT_THREADS) && defined(_POSIX_THREADS)

static pthread_key_t _tls_key;
static pthread_mutex_t _init_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_once_t _once_init = PTHREAD_ONCE_INIT;
int init_error = 0;

static void cb__free_status(void *st)
{
	git__global_state_cleanup(st);
	git__free(st);
}

static void init_once(void)
{
	if ((init_error = git_mutex_init(&git__mwindow_mutex)) != 0)
		return;

	pthread_key_create(&_tls_key, &cb__free_status);

	init_error = init_common();
}

int git_libgit2_init(void)
{
	int ret, err;

	if ((err = pthread_mutex_lock(&_init_mutex)) != 0)
		return err;

	ret = git_atomic_inc(&git__n_inits);
	err = pthread_once(&_once_init, init_once);
	err |= pthread_mutex_unlock(&_init_mutex);

	if (err || init_error)
		return err | init_error;

	return ret;
}

int git_libgit2_shutdown(void)
{
	void *ptr = NULL;
	pthread_once_t new_once = PTHREAD_ONCE_INIT;
	int error, ret;

	if ((error = pthread_mutex_lock(&_init_mutex)) != 0)
		return error;

	if ((ret = git_atomic_dec(&git__n_inits)) != 0)
		goto out;

	/* Shut down any subsystems that have global state */
	shutdown_common();

	ptr = pthread_getspecific(_tls_key);
	pthread_setspecific(_tls_key, NULL);

	git__global_state_cleanup(ptr);
	git__free(ptr);

	pthread_key_delete(_tls_key);
	git_mutex_free(&git__mwindow_mutex);
	_once_init = new_once;

out:
	if ((error = pthread_mutex_unlock(&_init_mutex)) != 0)
		return error;

	return ret;
}

git_global_st *git__global_state(void)
{
	git_global_st *ptr;

	assert(git_atomic_get(&git__n_inits) > 0);

	if ((ptr = pthread_getspecific(_tls_key)) != NULL)
		return ptr;

	ptr = git__calloc(1, sizeof(git_global_st));
	if (!ptr)
		return NULL;

	git_buf_init(&ptr->error_buf, 0);
	pthread_setspecific(_tls_key, ptr);
	return ptr;
}

#else

static git_global_st __state;

int git_libgit2_init(void)
{
	int ret;

	/* Only init subsystems the first time */
	if ((ret = git_atomic_inc(&git__n_inits)) != 1)
		return ret;

	if ((ret = init_common()) < 0)
		return ret;

	return 1;
}

int git_libgit2_shutdown(void)
{
	int ret;

	/* Shut down any subsystems that have global state */
	if ((ret = git_atomic_dec(&git__n_inits)) == 0) {
		shutdown_common();
		git__global_state_cleanup(&__state);
		memset(&__state, 0, sizeof(__state));
	}

	return ret;
}

git_global_st *git__global_state(void)
{
	return &__state;
}

#endif /* GIT_THREADS */
