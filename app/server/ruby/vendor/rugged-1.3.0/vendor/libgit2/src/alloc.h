/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_alloc_h__
#define INCLUDE_alloc_h__

#include "git2/sys/alloc.h"

extern git_allocator git__allocator;

#define git__malloc(len)                      git__allocator.gmalloc(len, __FILE__, __LINE__)
#define git__calloc(nelem, elsize)            git__allocator.gcalloc(nelem, elsize, __FILE__, __LINE__)
#define git__strdup(str)                      git__allocator.gstrdup(str, __FILE__, __LINE__)
#define git__strndup(str, n)                  git__allocator.gstrndup(str, n, __FILE__, __LINE__)
#define git__substrdup(str, n)                git__allocator.gsubstrdup(str, n, __FILE__, __LINE__)
#define git__realloc(ptr, size)               git__allocator.grealloc(ptr, size, __FILE__, __LINE__)
#define git__reallocarray(ptr, nelem, elsize) git__allocator.greallocarray(ptr, nelem, elsize, __FILE__, __LINE__)
#define git__mallocarray(nelem, elsize)       git__allocator.gmallocarray(nelem, elsize, __FILE__, __LINE__)
#define git__free                             git__allocator.gfree

/**
 * This function is being called by our global setup routines to
 * initialize the standard allocator.
 */
int git_allocator_global_init(void);

/**
 * Switch out libgit2's global memory allocator
 *
 * @param allocator The new allocator that should be used. All function pointers
 *                  of it need to be set correctly.
 * @return An error code or 0.
 */
int git_allocator_setup(git_allocator *allocator);

#endif
