/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_allocators_failalloc_h__
#define INCLUDE_allocators_failalloc_h__

#include "common.h"

extern void *git_failalloc_malloc(size_t len, const char *file, int line);
extern void *git_failalloc_calloc(size_t nelem, size_t elsize, const char *file, int line);
extern char *git_failalloc_strdup(const char *str, const char *file, int line);
extern char *git_failalloc_strndup(const char *str, size_t n, const char *file, int line);
extern char *git_failalloc_substrdup(const char *start, size_t n, const char *file, int line);
extern void *git_failalloc_realloc(void *ptr, size_t size, const char *file, int line);
extern void *git_failalloc_reallocarray(void *ptr, size_t nelem, size_t elsize, const char *file, int line);
extern void *git_failalloc_mallocarray(size_t nelem, size_t elsize, const char *file, int line);
extern void git_failalloc_free(void *ptr);

#endif
