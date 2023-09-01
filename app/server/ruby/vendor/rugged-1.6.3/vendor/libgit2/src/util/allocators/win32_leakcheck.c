/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "win32_leakcheck.h"

#if defined(GIT_WIN32_LEAKCHECK)

#include "win32/w32_leakcheck.h"

static void *leakcheck_malloc(size_t len, const char *file, int line)
{
	void *ptr = _malloc_dbg(len, _NORMAL_BLOCK, git_win32_leakcheck_stacktrace(1,file), line);
	if (!ptr) git_error_set_oom();
	return ptr;
}

static void *leakcheck_calloc(size_t nelem, size_t elsize, const char *file, int line)
{
	void *ptr = _calloc_dbg(nelem, elsize, _NORMAL_BLOCK, git_win32_leakcheck_stacktrace(1,file), line);
	if (!ptr) git_error_set_oom();
	return ptr;
}

static char *leakcheck_strdup(const char *str, const char *file, int line)
{
	char *ptr = _strdup_dbg(str, _NORMAL_BLOCK, git_win32_leakcheck_stacktrace(1,file), line);
	if (!ptr) git_error_set_oom();
	return ptr;
}

static char *leakcheck_strndup(const char *str, size_t n, const char *file, int line)
{
	size_t length = 0, alloclength;
	char *ptr;

	length = p_strnlen(str, n);

	if (GIT_ADD_SIZET_OVERFLOW(&alloclength, length, 1) ||
		!(ptr = leakcheck_malloc(alloclength, file, line)))
		return NULL;

	if (length)
		memcpy(ptr, str, length);

	ptr[length] = '\0';

	return ptr;
}

static char *leakcheck_substrdup(const char *start, size_t n, const char *file, int line)
{
	char *ptr;
	size_t alloclen;

	if (GIT_ADD_SIZET_OVERFLOW(&alloclen, n, 1) ||
		!(ptr = leakcheck_malloc(alloclen, file, line)))
		return NULL;

	memcpy(ptr, start, n);
	ptr[n] = '\0';
	return ptr;
}

static void *leakcheck_realloc(void *ptr, size_t size, const char *file, int line)
{
	void *new_ptr = _realloc_dbg(ptr, size, _NORMAL_BLOCK, git_win32_leakcheck_stacktrace(1,file), line);
	if (!new_ptr) git_error_set_oom();
	return new_ptr;
}

static void *leakcheck_reallocarray(void *ptr, size_t nelem, size_t elsize, const char *file, int line)
{
	size_t newsize;

	if (GIT_MULTIPLY_SIZET_OVERFLOW(&newsize, nelem, elsize))
		return NULL;

	return leakcheck_realloc(ptr, newsize, file, line);
}

static void *leakcheck_mallocarray(size_t nelem, size_t elsize, const char *file, int line)
{
	return leakcheck_reallocarray(NULL, nelem, elsize, file, line);
}

static void leakcheck_free(void *ptr)
{
	free(ptr);
}

int git_win32_leakcheck_init_allocator(git_allocator *allocator)
{
	allocator->gmalloc = leakcheck_malloc;
	allocator->gcalloc = leakcheck_calloc;
	allocator->gstrdup = leakcheck_strdup;
	allocator->gstrndup = leakcheck_strndup;
	allocator->gsubstrdup = leakcheck_substrdup;
	allocator->grealloc = leakcheck_realloc;
	allocator->greallocarray = leakcheck_reallocarray;
	allocator->gmallocarray = leakcheck_mallocarray;
	allocator->gfree = leakcheck_free;
	return 0;
}

#else

int git_win32_leakcheck_init_allocator(git_allocator *allocator)
{
	GIT_UNUSED(allocator);
	git_error_set(GIT_EINVALID, "leakcheck memory allocator not available");
	return -1;
}

#endif
