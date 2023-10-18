/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "clar_libgit2_alloc.h"

static size_t bytes_available;

/*
 * The clar allocator uses a tagging mechanism for pointers that
 * prepends the actual pointer's number bytes as `size_t`.
 *
 * First, this is required in order to be able to implement
 * proper bookkeeping of allocated bytes in both `free` and
 * `realloc`.
 *
 * Second, it may also be able to spot bugs that are
 * otherwise hard to grasp, as the returned pointer cannot be
 * free'd directly via free(3P). Instead, one is forced to use
 * the tandem of `cl__malloc` and `cl__free`, as otherwise the
 * code is going to crash hard. This is considered to be a
 * feature, as it helps e.g. in finding cases where by accident
 * malloc(3P) and free(3P) were used instead of git__malloc and
 * git__free, respectively.
 *
 * The downside is obviously that each allocation grows by
 * sizeof(size_t) bytes. As the allocator is for testing purposes
 * only, this tradeoff is considered to be perfectly fine,
 * though.
 */

static void *cl__malloc(size_t len, const char *file, int line)
{
	char *ptr = NULL;
	size_t alloclen;

	GIT_UNUSED(file);
	GIT_UNUSED(line);

	if (len > bytes_available)
		goto out;

	if (GIT_ADD_SIZET_OVERFLOW(&alloclen, len, sizeof(size_t)) ||
	    (ptr = malloc(alloclen)) == NULL)
		goto out;
	memcpy(ptr, &len, sizeof(size_t));

	bytes_available -= len;

out:
	return ptr ? ptr + sizeof(size_t) : NULL;
}

static void cl__free(void *ptr)
{
	if (ptr) {
		char *p = ptr;
		size_t len;
		memcpy(&len, p - sizeof(size_t), sizeof(size_t));
		free(p - sizeof(size_t));
		bytes_available += len;
	}
}

static void *cl__realloc(void *ptr, size_t size, const char *file, int line)
{
	size_t copybytes = 0;
	char *p = ptr;
	void *new;

	if (p)
		memcpy(&copybytes, p - sizeof(size_t), sizeof(size_t));

	if (copybytes > size)
		copybytes = size;

	if ((new = cl__malloc(size, file, line)) == NULL)
		goto out;

	if (p) {
		memcpy(new, p, copybytes);
		cl__free(p);
	}

out:
	return new;
}

void cl_alloc_limit(size_t bytes)
{
	git_allocator alloc;

	alloc.gmalloc = cl__malloc;
	alloc.grealloc = cl__realloc;
	alloc.gfree = cl__free;

	git_allocator_setup(&alloc);

	bytes_available = bytes;
}

void cl_alloc_reset(void)
{
	git_allocator stdalloc;
	git_stdalloc_init_allocator(&stdalloc);
	git_allocator_setup(&stdalloc);
}
