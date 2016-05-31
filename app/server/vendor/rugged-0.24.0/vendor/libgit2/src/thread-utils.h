/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_thread_utils_h__
#define INCLUDE_thread_utils_h__

/* Common operations even if threading has been disabled */
typedef struct {
#if defined(GIT_WIN32)
	volatile long val;
#else
	volatile int val;
#endif
} git_atomic;

#ifdef GIT_ARCH_64

typedef struct {
#if defined(GIT_WIN32)
	__int64 val;
#else
	int64_t val;
#endif
} git_atomic64;

typedef git_atomic64 git_atomic_ssize;

#define git_atomic_ssize_add git_atomic64_add

#else

typedef git_atomic git_atomic_ssize;

#define git_atomic_ssize_add git_atomic_add

#endif

#ifdef GIT_THREADS

#if !defined(GIT_WIN32)

typedef struct {
	pthread_t thread;
} git_thread;

#define git_thread_create(git_thread_ptr, attr, start_routine, arg) \
	pthread_create(&(git_thread_ptr)->thread, attr, start_routine, arg)
#define git_thread_join(git_thread_ptr, status) \
	pthread_join((git_thread_ptr)->thread, status)

#endif

/* Pthreads Mutex */
#define git_mutex pthread_mutex_t
#define git_mutex_init(a)	pthread_mutex_init(a, NULL)
#define git_mutex_lock(a)	pthread_mutex_lock(a)
#define git_mutex_unlock(a) pthread_mutex_unlock(a)
#define git_mutex_free(a)	pthread_mutex_destroy(a)

/* Pthreads condition vars */
#define git_cond pthread_cond_t
#define git_cond_init(c)	pthread_cond_init(c, NULL)
#define git_cond_free(c) 	pthread_cond_destroy(c)
#define git_cond_wait(c, l)	pthread_cond_wait(c, l)
#define git_cond_signal(c)	pthread_cond_signal(c)
#define git_cond_broadcast(c)	pthread_cond_broadcast(c)

/* Pthread (-ish) rwlock
 *
 * This differs from normal pthreads rwlocks in two ways:
 * 1. Separate APIs for releasing read locks and write locks (as
 *    opposed to the pure POSIX API which only has one unlock fn)
 * 2. You should not use recursive read locks (i.e. grabbing a read
 *    lock in a thread that already holds a read lock) because the
 *    Windows implementation doesn't support it
 */
#define git_rwlock pthread_rwlock_t
#define git_rwlock_init(a)		pthread_rwlock_init(a, NULL)
#define git_rwlock_rdlock(a)	pthread_rwlock_rdlock(a)
#define git_rwlock_rdunlock(a)	pthread_rwlock_rdunlock(a)
#define git_rwlock_wrlock(a)	pthread_rwlock_wrlock(a)
#define git_rwlock_wrunlock(a)	pthread_rwlock_wrunlock(a)
#define git_rwlock_free(a)		pthread_rwlock_destroy(a)
#define GIT_RWLOCK_STATIC_INIT	PTHREAD_RWLOCK_INITIALIZER

#ifndef GIT_WIN32
#define pthread_rwlock_rdunlock pthread_rwlock_unlock
#define pthread_rwlock_wrunlock pthread_rwlock_unlock
#endif


GIT_INLINE(void) git_atomic_set(git_atomic *a, int val)
{
#if defined(GIT_WIN32)
	InterlockedExchange(&a->val, (LONG)val);
#elif defined(__GNUC__)
	__sync_lock_test_and_set(&a->val, val);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

GIT_INLINE(int) git_atomic_inc(git_atomic *a)
{
#if defined(GIT_WIN32)
	return InterlockedIncrement(&a->val);
#elif defined(__GNUC__)
	return __sync_add_and_fetch(&a->val, 1);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

GIT_INLINE(int) git_atomic_add(git_atomic *a, int32_t addend)
{
#if defined(GIT_WIN32)
	return InterlockedExchangeAdd(&a->val, addend);
#elif defined(__GNUC__)
	return __sync_add_and_fetch(&a->val, addend);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

GIT_INLINE(int) git_atomic_dec(git_atomic *a)
{
#if defined(GIT_WIN32)
	return InterlockedDecrement(&a->val);
#elif defined(__GNUC__)
	return __sync_sub_and_fetch(&a->val, 1);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

GIT_INLINE(void *) git___compare_and_swap(
	void * volatile *ptr, void *oldval, void *newval)
{
	volatile void *foundval;
#if defined(GIT_WIN32)
	foundval = InterlockedCompareExchangePointer((volatile PVOID *)ptr, newval, oldval);
#elif defined(__GNUC__)
	foundval = __sync_val_compare_and_swap(ptr, oldval, newval);
#else
#	error "Unsupported architecture for atomic operations"
#endif
	return (foundval == oldval) ? oldval : newval;
}

GIT_INLINE(volatile void *) git___swap(
	void * volatile *ptr, void *newval)
{
#if defined(GIT_WIN32)
	return InterlockedExchangePointer(ptr, newval);
#else
	return __sync_lock_test_and_set(ptr, newval);
#endif
}

#ifdef GIT_ARCH_64

GIT_INLINE(int64_t) git_atomic64_add(git_atomic64 *a, int64_t addend)
{
#if defined(GIT_WIN32)
	return InterlockedExchangeAdd64(&a->val, addend);
#elif defined(__GNUC__)
	return __sync_add_and_fetch(&a->val, addend);
#else
#	error "Unsupported architecture for atomic operations"
#endif
}

#endif

#else

#define git_thread unsigned int
#define git_thread_create(thread, attr, start_routine, arg) 0
#define git_thread_join(id, status) (void)0

/* Pthreads Mutex */
#define git_mutex unsigned int
GIT_INLINE(int) git_mutex_init(git_mutex *mutex) \
	{ GIT_UNUSED(mutex); return 0; }
GIT_INLINE(int) git_mutex_lock(git_mutex *mutex) \
	{ GIT_UNUSED(mutex); return 0; }
#define git_mutex_unlock(a) (void)0
#define git_mutex_free(a) (void)0

/* Pthreads condition vars */
#define git_cond unsigned int
#define git_cond_init(c, a)	(void)0
#define git_cond_free(c) (void)0
#define git_cond_wait(c, l)	(void)0
#define git_cond_signal(c) (void)0
#define git_cond_broadcast(c) (void)0

/* Pthreads rwlock */
#define git_rwlock unsigned int
#define git_rwlock_init(a)		0
#define git_rwlock_rdlock(a)	0
#define git_rwlock_rdunlock(a)	(void)0
#define git_rwlock_wrlock(a)	0
#define git_rwlock_wrunlock(a)	(void)0
#define git_rwlock_free(a)		(void)0
#define GIT_RWLOCK_STATIC_INIT	0


GIT_INLINE(void) git_atomic_set(git_atomic *a, int val)
{
	a->val = val;
}

GIT_INLINE(int) git_atomic_inc(git_atomic *a)
{
	return ++a->val;
}

GIT_INLINE(int) git_atomic_add(git_atomic *a, int32_t addend)
{
	a->val += addend;
	return a->val;
}

GIT_INLINE(int) git_atomic_dec(git_atomic *a)
{
	return --a->val;
}

GIT_INLINE(void *) git___compare_and_swap(
	void * volatile *ptr, void *oldval, void *newval)
{
	if (*ptr == oldval)
		*ptr = newval;
	else
		oldval = newval;
	return oldval;
}

GIT_INLINE(volatile void *) git___swap(
	void * volatile *ptr, void *newval)
{
	volatile void *old = *ptr;
	*ptr = newval;
	return old;
}

#ifdef GIT_ARCH_64

GIT_INLINE(int64_t) git_atomic64_add(git_atomic64 *a, int64_t addend)
{
	a->val += addend;
	return a->val;
}

#endif

#endif

GIT_INLINE(int) git_atomic_get(git_atomic *a)
{
	return (int)a->val;
}

/* Atomically replace oldval with newval
 * @return oldval if it was replaced or newval if it was not
 */
#define git__compare_and_swap(P,O,N) \
	git___compare_and_swap((void * volatile *)P, O, N)

#define git__swap(ptr, val) (void *)git___swap((void * volatile *)&ptr, val)

extern int git_online_cpus(void);

#if defined(GIT_THREADS) && defined(_MSC_VER)
# define GIT_MEMORY_BARRIER MemoryBarrier()
#elif defined(GIT_THREADS)
# define GIT_MEMORY_BARRIER __sync_synchronize()
#else
# define GIT_MEMORY_BARRIER /* noop */
#endif

#endif /* INCLUDE_thread_utils_h__ */
