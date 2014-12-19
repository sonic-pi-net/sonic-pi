/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef GIT_PTHREAD_H
#define GIT_PTHREAD_H

#include "../common.h"

#if defined (_MSC_VER)
#	define GIT_RESTRICT __restrict
#else
#	define GIT_RESTRICT __restrict__
#endif

typedef struct {
	HANDLE thread;
	void *(*proc)(void *);
	void *param;
	void *result;
} git_win32_thread;

typedef int pthread_mutexattr_t;
typedef int pthread_condattr_t;
typedef int pthread_attr_t;
typedef int pthread_rwlockattr_t;

typedef CRITICAL_SECTION pthread_mutex_t;
typedef HANDLE pthread_cond_t;

typedef struct { void *Ptr; } GIT_SRWLOCK;

typedef struct {
	union {
		GIT_SRWLOCK srwl;
		CRITICAL_SECTION csec;
	} native;
} pthread_rwlock_t;

#define PTHREAD_MUTEX_INITIALIZER  {(void*)-1}

int git_win32__thread_create(
	git_win32_thread *GIT_RESTRICT,
	const pthread_attr_t *GIT_RESTRICT,
	void *(*) (void *),
	void *GIT_RESTRICT);

int git_win32__thread_join(
	git_win32_thread *,
	void **);

#ifdef GIT_THREADS

typedef git_win32_thread git_thread;

#define git_thread_create(git_thread_ptr, attr, start_routine, arg) \
	git_win32__thread_create(git_thread_ptr, attr, start_routine, arg)
#define git_thread_join(git_thread_ptr, status) \
	git_win32__thread_join(git_thread_ptr, status)

#endif

int pthread_mutex_init(
	pthread_mutex_t *GIT_RESTRICT mutex,
	const pthread_mutexattr_t *GIT_RESTRICT mutexattr);
int pthread_mutex_destroy(pthread_mutex_t *);
int pthread_mutex_lock(pthread_mutex_t *);
int pthread_mutex_unlock(pthread_mutex_t *);

int pthread_cond_init(pthread_cond_t *, const pthread_condattr_t *);
int pthread_cond_destroy(pthread_cond_t *);
int pthread_cond_wait(pthread_cond_t *, pthread_mutex_t *);
int pthread_cond_signal(pthread_cond_t *);
/* pthread_cond_broadcast is not supported on Win32 yet. */

int pthread_num_processors_np(void);

int pthread_rwlock_init(
	pthread_rwlock_t *GIT_RESTRICT lock,
	const pthread_rwlockattr_t *GIT_RESTRICT attr);
int pthread_rwlock_rdlock(pthread_rwlock_t *);
int pthread_rwlock_rdunlock(pthread_rwlock_t *);
int pthread_rwlock_wrlock(pthread_rwlock_t *);
int pthread_rwlock_wrunlock(pthread_rwlock_t *);
int pthread_rwlock_destroy(pthread_rwlock_t *);

extern int win32_pthread_initialize(void);

#endif
