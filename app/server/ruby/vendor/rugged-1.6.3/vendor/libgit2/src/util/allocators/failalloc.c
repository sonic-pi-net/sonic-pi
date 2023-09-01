/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "failalloc.h"

void *git_failalloc_malloc(size_t len, const char *file, int line)
{
	GIT_UNUSED(len);
	GIT_UNUSED(file);
	GIT_UNUSED(line);

	return NULL;
}

void *git_failalloc_calloc(size_t nelem, size_t elsize, const char *file, int line)
{
	GIT_UNUSED(nelem);
	GIT_UNUSED(elsize);
	GIT_UNUSED(file);
	GIT_UNUSED(line);

	return NULL;
}

char *git_failalloc_strdup(const char *str, const char *file, int line)
{
	GIT_UNUSED(str);
	GIT_UNUSED(file);
	GIT_UNUSED(line);

	return NULL;
}

char *git_failalloc_strndup(const char *str, size_t n, const char *file, int line)
{
	GIT_UNUSED(str);
	GIT_UNUSED(n);
	GIT_UNUSED(file);
	GIT_UNUSED(line);

	return NULL;
}

char *git_failalloc_substrdup(const char *start, size_t n, const char *file, int line)
{
	GIT_UNUSED(start);
	GIT_UNUSED(n);
	GIT_UNUSED(file);
	GIT_UNUSED(line);

	return NULL;
}

void *git_failalloc_realloc(void *ptr, size_t size, const char *file, int line)
{
	GIT_UNUSED(ptr);
	GIT_UNUSED(size);
	GIT_UNUSED(file);
	GIT_UNUSED(line);

	return NULL;
}

void *git_failalloc_reallocarray(void *ptr, size_t nelem, size_t elsize, const char *file, int line)
{
	GIT_UNUSED(ptr);
	GIT_UNUSED(nelem);
	GIT_UNUSED(elsize);
	GIT_UNUSED(file);
	GIT_UNUSED(line);

	return NULL;
}

void *git_failalloc_mallocarray(size_t nelem, size_t elsize, const char *file, int line)
{
	GIT_UNUSED(nelem);
	GIT_UNUSED(elsize);
	GIT_UNUSED(file);
	GIT_UNUSED(line);

	return NULL;
}

void git_failalloc_free(void *ptr)
{
	GIT_UNUSED(ptr);
}
