/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "pthread.h"
#include "../global.h"

#define CLEAN_THREAD_EXIT 0x6F012842

/* The thread procedure stub used to invoke the caller's procedure
 * and capture the return value for later collection. Windows will
 * only hold a DWORD, but we need to be able to store an entire
 * void pointer. This requires the indirection. */
static DWORD WINAPI git_win32__threadproc(LPVOID lpParameter)
{
	git_win32_thread *thread = lpParameter;

	thread->result = thread->proc(thread->param);

	git__free_tls_data();

	return CLEAN_THREAD_EXIT;
}

int git_win32__thread_create(
	git_win32_thread *GIT_RESTRICT thread,
	const pthread_attr_t *GIT_RESTRICT attr,
	void *(*start_routine)(void*),
	void *GIT_RESTRICT arg)
{
	GIT_UNUSED(attr);

	thread->result = NULL;
	thread->param = arg;
	thread->proc = start_routine;
	thread->thread = CreateThread(
		NULL, 0, git_win32__threadproc, thread, 0, NULL);

	return thread->thread ? 0 : -1;
}

int git_win32__thread_join(
	git_win32_thread *thread,
	void **value_ptr)
{
	DWORD exit;

	if (WaitForSingleObject(thread->thread, INFINITE) != WAIT_OBJECT_0)
		return -1;

	if (!GetExitCodeThread(thread->thread, &exit)) {
		CloseHandle(thread->thread);
		return -1;
	}

	/* Check for the thread having exited uncleanly. If exit was unclean,
	 * then we don't have a return value to give back to the caller. */
	if (exit != CLEAN_THREAD_EXIT) {
		assert(false);
		thread->result = NULL;
	}

	if (value_ptr)
		*value_ptr = thread->result;

	CloseHandle(thread->thread);
	return 0;
}

int pthread_mutex_init(
	pthread_mutex_t *GIT_RESTRICT mutex,
	const pthread_mutexattr_t *GIT_RESTRICT mutexattr)
{
	GIT_UNUSED(mutexattr);
	InitializeCriticalSection(mutex);
	return 0;
}

int pthread_mutex_destroy(pthread_mutex_t *mutex)
{
	DeleteCriticalSection(mutex);
	return 0;
}

int pthread_mutex_lock(pthread_mutex_t *mutex)
{
	EnterCriticalSection(mutex);
	return 0;
}

int pthread_mutex_unlock(pthread_mutex_t *mutex)
{
	LeaveCriticalSection(mutex);
	return 0;
}

int pthread_cond_init(pthread_cond_t *cond, const pthread_condattr_t *attr)
{
	/* We don't support non-default attributes. */
	if (attr)
		return EINVAL;

	/* This is an auto-reset event. */
	*cond = CreateEventW(NULL, FALSE, FALSE, NULL);
	assert(*cond);

	/* If we can't create the event, claim that the reason was out-of-memory.
	 * The actual reason can be fetched with GetLastError(). */
	return *cond ? 0 : ENOMEM;
}

int pthread_cond_destroy(pthread_cond_t *cond)
{
	BOOL closed;

	if (!cond)
		return EINVAL;

	closed = CloseHandle(*cond);
	assert(closed);
	GIT_UNUSED(closed);

	*cond = NULL;
	return 0;
}

int pthread_cond_wait(pthread_cond_t *cond, pthread_mutex_t *mutex)
{
	int error;
	DWORD wait_result;

	if (!cond || !mutex)
		return EINVAL;

	/* The caller must be holding the mutex. */
	error = pthread_mutex_unlock(mutex);

	if (error)
		return error;

	wait_result = WaitForSingleObject(*cond, INFINITE);
	assert(WAIT_OBJECT_0 == wait_result);
	GIT_UNUSED(wait_result);

	return pthread_mutex_lock(mutex);
}

int pthread_cond_signal(pthread_cond_t *cond)
{
	BOOL signaled;

	if (!cond)
		return EINVAL;

	signaled = SetEvent(*cond);
	assert(signaled);
	GIT_UNUSED(signaled);

	return 0;
}

/* pthread_cond_broadcast is not implemented because doing so with just
 * Win32 events is quite complicated, and no caller in libgit2 uses it
 * yet.
 */
int pthread_num_processors_np(void)
{
	DWORD_PTR p, s;
	int n = 0;

	if (GetProcessAffinityMask(GetCurrentProcess(), &p, &s))
		for (; p; p >>= 1)
			n += p&1;

	return n ? n : 1;
}

typedef void (WINAPI *win32_srwlock_fn)(GIT_SRWLOCK *);

static win32_srwlock_fn win32_srwlock_initialize;
static win32_srwlock_fn win32_srwlock_acquire_shared;
static win32_srwlock_fn win32_srwlock_release_shared;
static win32_srwlock_fn win32_srwlock_acquire_exclusive;
static win32_srwlock_fn win32_srwlock_release_exclusive;

int pthread_rwlock_init(
	pthread_rwlock_t *GIT_RESTRICT lock,
	const pthread_rwlockattr_t *GIT_RESTRICT attr)
{
	GIT_UNUSED(attr);

	if (win32_srwlock_initialize)
		win32_srwlock_initialize(&lock->native.srwl);
	else
		InitializeCriticalSection(&lock->native.csec);

	return 0;
}

int pthread_rwlock_rdlock(pthread_rwlock_t *lock)
{
	if (win32_srwlock_acquire_shared)
		win32_srwlock_acquire_shared(&lock->native.srwl);
	else
		EnterCriticalSection(&lock->native.csec);

	return 0;
}

int pthread_rwlock_rdunlock(pthread_rwlock_t *lock)
{
	if (win32_srwlock_release_shared)
		win32_srwlock_release_shared(&lock->native.srwl);
	else
		LeaveCriticalSection(&lock->native.csec);

	return 0;
}

int pthread_rwlock_wrlock(pthread_rwlock_t *lock)
{
	if (win32_srwlock_acquire_exclusive)
		win32_srwlock_acquire_exclusive(&lock->native.srwl);
	else
		EnterCriticalSection(&lock->native.csec);

	return 0;
}

int pthread_rwlock_wrunlock(pthread_rwlock_t *lock)
{
	if (win32_srwlock_release_exclusive)
		win32_srwlock_release_exclusive(&lock->native.srwl);
	else
		LeaveCriticalSection(&lock->native.csec);

	return 0;
}

int pthread_rwlock_destroy(pthread_rwlock_t *lock)
{
	if (!win32_srwlock_initialize)
		DeleteCriticalSection(&lock->native.csec);
	git__memzero(lock, sizeof(*lock));
	return 0;
}

int win32_pthread_initialize(void)
{
	HMODULE hModule = GetModuleHandleW(L"kernel32");

	if (hModule) {
		win32_srwlock_initialize = (win32_srwlock_fn)
			GetProcAddress(hModule, "InitializeSRWLock");
		win32_srwlock_acquire_shared = (win32_srwlock_fn)
			GetProcAddress(hModule, "AcquireSRWLockShared");
		win32_srwlock_release_shared = (win32_srwlock_fn)
			GetProcAddress(hModule, "ReleaseSRWLockShared");
		win32_srwlock_acquire_exclusive = (win32_srwlock_fn)
			GetProcAddress(hModule, "AcquireSRWLockExclusive");
		win32_srwlock_release_exclusive = (win32_srwlock_fn)
			GetProcAddress(hModule, "ReleaseSRWLockExclusive");
	}

	return 0;
}
