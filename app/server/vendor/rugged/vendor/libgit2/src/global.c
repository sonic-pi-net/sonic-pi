/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"
#include "global.h"
#include "hash.h"
#include "sysdir.h"
#include "git2/threads.h"
#include "thread-utils.h"


git_mutex git__mwindow_mutex;

#define MAX_SHUTDOWN_CB 8

#ifdef GIT_SSL
# include <openssl/ssl.h>
SSL_CTX *git__ssl_ctx;
static git_mutex *openssl_locks;
#endif

static git_global_shutdown_fn git__shutdown_callbacks[MAX_SHUTDOWN_CB];
static git_atomic git__n_shutdown_callbacks;
static git_atomic git__n_inits;

void git__on_shutdown(git_global_shutdown_fn callback)
{
	int count = git_atomic_inc(&git__n_shutdown_callbacks);
	assert(count <= MAX_SHUTDOWN_CB && count > 0);
	git__shutdown_callbacks[count - 1] = callback;
}

static void git__shutdown(void)
{
	int pos;

	for (pos = git_atomic_get(&git__n_shutdown_callbacks); pos > 0; pos = git_atomic_dec(&git__n_shutdown_callbacks)) {
		git_global_shutdown_fn cb = git__swap(git__shutdown_callbacks[pos - 1], NULL);
		if (cb != NULL)
			cb();
	}

}

#if defined(GIT_THREADS) && defined(GIT_SSL)
void openssl_locking_function(int mode, int n, const char *file, int line)
{
	int lock;

	GIT_UNUSED(file);
	GIT_UNUSED(line);

	lock = mode & CRYPTO_LOCK;

	if (lock) {
		git_mutex_lock(&openssl_locks[n]);
	} else {
		git_mutex_unlock(&openssl_locks[n]);
	}
}

static void shutdown_ssl(void)
{
	git__free(openssl_locks);
}
#endif

static void init_ssl(void)
{
#ifdef GIT_SSL
	SSL_load_error_strings();
	OpenSSL_add_ssl_algorithms();
	/*
	 * Load SSLv{2,3} and TLSv1 so that we can talk with servers
	 * which use the SSL hellos, which are often used for
	 * compatibility. We then disable SSL so we only allow OpenSSL
	 * to speak TLSv1 to perform the encryption itself.
	 */
	git__ssl_ctx = SSL_CTX_new(SSLv23_method());
	SSL_CTX_set_options(git__ssl_ctx,
			    SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3
	/* Older OpenSSL and MacOS OpenSSL doesn't have this */
# ifdef SSL_OP_NO_COMPRESSION
			    | SSL_OP_NO_COMPRESSION
# endif
		);
	SSL_CTX_set_mode(git__ssl_ctx, SSL_MODE_AUTO_RETRY);
	SSL_CTX_set_verify(git__ssl_ctx, SSL_VERIFY_NONE, NULL);
	if (!SSL_CTX_set_default_verify_paths(git__ssl_ctx)) {
		SSL_CTX_free(git__ssl_ctx);
		git__ssl_ctx = NULL;
	}

# ifdef GIT_THREADS
	{
		int num_locks, i;

		num_locks = CRYPTO_num_locks();
		openssl_locks = git__calloc(num_locks, sizeof(git_mutex));
		if (openssl_locks == NULL) {
			SSL_CTX_free(git__ssl_ctx);
			git__ssl_ctx = NULL;
		}

		for (i = 0; i < num_locks; i++) {
			if (git_mutex_init(&openssl_locks[i]) != 0) {
				SSL_CTX_free(git__ssl_ctx);
				git__ssl_ctx = NULL;
			}
		}

		CRYPTO_set_locking_callback(openssl_locking_function);
	}

	git__on_shutdown(shutdown_ssl);
# endif
#endif
}

/**
 * Handle the global state with TLS
 *
 * If libgit2 is built with GIT_THREADS enabled,
 * the `git_threads_init()` function must be called
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
 * `git_threads_shutdown` method must be called to free
 * the previously reserved TLS index.
 *
 * If libgit2 is built without threading support, the
 * `git__global_statestate()` call returns a pointer to a single,
 * statically allocated global state. The `git_thread_`
 * functions are not available in that case.
 */

/*
 * `git_threads_init()` allows subsystems to perform global setup,
 * which may take place in the global scope.  An explicit memory
 * fence exists at the exit of `git_threads_init()`.  Without this,
 * CPU cores are free to reorder cache invalidation of `_tls_init`
 * before cache invalidation of the subsystems' newly written global
 * state.
 */
#if defined(GIT_THREADS) && defined(GIT_WIN32)

static DWORD _tls_index;
static volatile LONG _mutex = 0;

static int synchronized_threads_init(void)
{
	int error;

	_tls_index = TlsAlloc();
	if (git_mutex_init(&git__mwindow_mutex))
		return -1;

	/* Initialize any other subsystems that have global state */
	if ((error = git_hash_global_init()) >= 0)
		error = git_sysdir_global_init();

	win32_pthread_initialize();

	return error;
}

int git_threads_init(void)
{
	int error = 0;

	/* Enter the lock */
	while (InterlockedCompareExchange(&_mutex, 1, 0)) { Sleep(0); }

	/* Only do work on a 0 -> 1 transition of the refcount */
	if (1 == git_atomic_inc(&git__n_inits))
		error = synchronized_threads_init();

	/* Exit the lock */
	InterlockedExchange(&_mutex, 0);

	return error;
}

static void synchronized_threads_shutdown(void)
{
	/* Shut down any subsystems that have global state */
	git__shutdown();
	TlsFree(_tls_index);
	git_mutex_free(&git__mwindow_mutex);
}

void git_threads_shutdown(void)
{
	/* Enter the lock */
	while (InterlockedCompareExchange(&_mutex, 1, 0)) { Sleep(0); }

	/* Only do work on a 1 -> 0 transition of the refcount */
	if (0 == git_atomic_dec(&git__n_inits))
		synchronized_threads_shutdown();

	/* Exit the lock */
	InterlockedExchange(&_mutex, 0);
}

git_global_st *git__global_state(void)
{
	void *ptr;

	assert(git_atomic_get(&git__n_inits) > 0);

	if ((ptr = TlsGetValue(_tls_index)) != NULL)
		return ptr;

	ptr = git__malloc(sizeof(git_global_st));
	if (!ptr)
		return NULL;

	memset(ptr, 0x0, sizeof(git_global_st));
	TlsSetValue(_tls_index, ptr);
	return ptr;
}

#elif defined(GIT_THREADS) && defined(_POSIX_THREADS)

static pthread_key_t _tls_key;
static pthread_once_t _once_init = PTHREAD_ONCE_INIT;
int init_error = 0;

static void cb__free_status(void *st)
{
	git_global_st *state = (git_global_st *) st;
	git__free(state->error_t.message);

	git__free(st);
}

static void init_once(void)
{
	if ((init_error = git_mutex_init(&git__mwindow_mutex)) != 0)
		return;
	pthread_key_create(&_tls_key, &cb__free_status);


	/* Initialize any other subsystems that have global state */
	if ((init_error = git_hash_global_init()) >= 0)
		init_error = git_sysdir_global_init();

	/* OpenSSL needs to be initialized from the main thread */
	init_ssl();

	GIT_MEMORY_BARRIER;
}

int git_threads_init(void)
{
	pthread_once(&_once_init, init_once);
	git_atomic_inc(&git__n_inits);
	return init_error;
}

void git_threads_shutdown(void)
{
	void *ptr = NULL;
	pthread_once_t new_once = PTHREAD_ONCE_INIT;

	if (git_atomic_dec(&git__n_inits) > 0) return;

	/* Shut down any subsystems that have global state */
	git__shutdown();

	ptr = pthread_getspecific(_tls_key);
	pthread_setspecific(_tls_key, NULL);
	git__free(ptr);

	pthread_key_delete(_tls_key);
	git_mutex_free(&git__mwindow_mutex);
	_once_init = new_once;
}

git_global_st *git__global_state(void)
{
	void *ptr;

	assert(git_atomic_get(&git__n_inits) > 0);

	if ((ptr = pthread_getspecific(_tls_key)) != NULL)
		return ptr;

	ptr = git__malloc(sizeof(git_global_st));
	if (!ptr)
		return NULL;

	memset(ptr, 0x0, sizeof(git_global_st));
	pthread_setspecific(_tls_key, ptr);
	return ptr;
}

#else

static git_global_st __state;

int git_threads_init(void)
{
	static int ssl_inited = 0;

	if (!ssl_inited) {
		init_ssl();
		ssl_inited = 1;
	}

	git_atomic_inc(&git__n_inits);
	return 0;
}

void git_threads_shutdown(void)
{
	/* Shut down any subsystems that have global state */
	if (0 == git_atomic_dec(&git__n_inits))
		git__shutdown();
}

git_global_st *git__global_state(void)
{
	return &__state;
}

#endif /* GIT_THREADS */
