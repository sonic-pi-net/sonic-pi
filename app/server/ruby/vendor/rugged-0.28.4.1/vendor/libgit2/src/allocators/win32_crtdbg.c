/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "win32_crtdbg.h"

#if defined(GIT_MSVC_CRTDBG)

#include "win32/w32_crtdbg_stacktrace.h"

static void *crtdbg__malloc(size_t len, const char *file, int line)
{
	void *ptr = _malloc_dbg(len, _NORMAL_BLOCK, git_win32__crtdbg_stacktrace(1,file), line);
	if (!ptr) git_error_set_oom();
	return ptr;
}

static void *crtdbg__calloc(size_t nelem, size_t elsize, const char *file, int line)
{
	void *ptr = _calloc_dbg(nelem, elsize, _NORMAL_BLOCK, git_win32__crtdbg_stacktrace(1,file), line);
	if (!ptr) git_error_set_oom();
	return ptr;
}

static char *crtdbg__strdup(const char *str, const char *file, int line)
{
	char *ptr = _strdup_dbg(str, _NORMAL_BLOCK, git_win32__crtdbg_stacktrace(1,file), line);
	if (!ptr) git_error_set_oom();
	return ptr;
}

static char *crtdbg__strndup(const char *str, size_t n, const char *file, int line)
{
	size_t length = 0, alloclength;
	char *ptr;

	length = p_strnlen(str, n);

	if (GIT_ADD_SIZET_OVERFLOW(&alloclength, length, 1) ||
		!(ptr = crtdbg__malloc(alloclength, file, line)))
		return NULL;

	if (length)
		memcpy(ptr, str, length);

	ptr[length] = '\0';

	return ptr;
}

static char *crtdbg__substrdup(const char *start, size_t n, const char *file, int line)
{
	char *ptr;
	size_t alloclen;

	if (GIT_ADD_SIZET_OVERFLOW(&alloclen, n, 1) ||
		!(ptr = crtdbg__malloc(alloclen, file, line)))
		return NULL;

	memcpy(ptr, start, n);
	ptr[n] = '\0';
	return ptr;
}

static void *crtdbg__realloc(void *ptr, size_t size, const char *file, int line)
{
	void *new_ptr = _realloc_dbg(ptr, size, _NORMAL_BLOCK, git_win32__crtdbg_stacktrace(1,file), line);
	if (!new_ptr) git_error_set_oom();
	return new_ptr;
}

static void *crtdbg__reallocarray(void *ptr, size_t nelem, size_t elsize, const char *file, int line)
{
	size_t newsize;

	if (GIT_MULTIPLY_SIZET_OVERFLOW(&newsize, nelem, elsize))
		return NULL;

	return crtdbg__realloc(ptr, newsize, file, line);
}

static void *crtdbg__mallocarray(size_t nelem, size_t elsize, const char *file, int line)
{
	return crtdbg__reallocarray(NULL, nelem, elsize, file, line);
}

static void crtdbg__free(void *ptr)
{
	free(ptr);
}

int git_win32_crtdbg_init_allocator(git_allocator *allocator)
{
	allocator->gmalloc = crtdbg__malloc;
	allocator->gcalloc = crtdbg__calloc;
	allocator->gstrdup = crtdbg__strdup;
	allocator->gstrndup = crtdbg__strndup;
	allocator->gsubstrdup = crtdbg__substrdup;
	allocator->grealloc = crtdbg__realloc;
	allocator->greallocarray = crtdbg__reallocarray;
	allocator->gmallocarray = crtdbg__mallocarray;
	allocator->gfree = crtdbg__free;
	return 0;
}

#else

int git_win32_crtdbg_init_allocator(git_allocator *allocator)
{
	GIT_UNUSED(allocator);
	git_error_set(GIT_EINVALID, "crtdbg memory allocator not available");
	return -1;
}

#endif
