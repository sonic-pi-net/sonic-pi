/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "stdalloc.h"

static void *stdalloc__malloc(size_t len, const char *file, int line)
{
	void *ptr = malloc(len);

	GIT_UNUSED(file);
	GIT_UNUSED(line);

	if (!ptr) git_error_set_oom();
	return ptr;
}

static void *stdalloc__calloc(size_t nelem, size_t elsize, const char *file, int line)
{
	void *ptr = calloc(nelem, elsize);

	GIT_UNUSED(file);
	GIT_UNUSED(line);

	if (!ptr) git_error_set_oom();
	return ptr;
}

static char *stdalloc__strdup(const char *str, const char *file, int line)
{
	char *ptr = strdup(str);

	GIT_UNUSED(file);
	GIT_UNUSED(line);

	if (!ptr) git_error_set_oom();
	return ptr;
}

static char *stdalloc__strndup(const char *str, size_t n, const char *file, int line)
{
	size_t length = 0, alloclength;
	char *ptr;

	length = p_strnlen(str, n);

	if (GIT_ADD_SIZET_OVERFLOW(&alloclength, length, 1) ||
		!(ptr = stdalloc__malloc(alloclength, file, line)))
		return NULL;

	if (length)
		memcpy(ptr, str, length);

	ptr[length] = '\0';

	return ptr;
}

static char *stdalloc__substrdup(const char *start, size_t n, const char *file, int line)
{
	char *ptr;
	size_t alloclen;

	if (GIT_ADD_SIZET_OVERFLOW(&alloclen, n, 1) ||
		!(ptr = stdalloc__malloc(alloclen, file, line)))
		return NULL;

	memcpy(ptr, start, n);
	ptr[n] = '\0';
	return ptr;
}

static void *stdalloc__realloc(void *ptr, size_t size, const char *file, int line)
{
	void *new_ptr = realloc(ptr, size);

	GIT_UNUSED(file);
	GIT_UNUSED(line);

	if (!new_ptr) git_error_set_oom();
	return new_ptr;
}

static void *stdalloc__reallocarray(void *ptr, size_t nelem, size_t elsize, const char *file, int line)
{
	size_t newsize;

	if (GIT_MULTIPLY_SIZET_OVERFLOW(&newsize, nelem, elsize))
		return NULL;

	return stdalloc__realloc(ptr, newsize, file, line);
}

static void *stdalloc__mallocarray(size_t nelem, size_t elsize, const char *file, int line)
{
	return stdalloc__reallocarray(NULL, nelem, elsize, file, line);
}

static void stdalloc__free(void *ptr)
{
	free(ptr);
}

int git_stdalloc_init_allocator(git_allocator *allocator)
{
	allocator->gmalloc = stdalloc__malloc;
	allocator->gcalloc = stdalloc__calloc;
	allocator->gstrdup = stdalloc__strdup;
	allocator->gstrndup = stdalloc__strndup;
	allocator->gsubstrdup = stdalloc__substrdup;
	allocator->grealloc = stdalloc__realloc;
	allocator->greallocarray = stdalloc__reallocarray;
	allocator->gmallocarray = stdalloc__mallocarray;
	allocator->gfree = stdalloc__free;
	return 0;
}
