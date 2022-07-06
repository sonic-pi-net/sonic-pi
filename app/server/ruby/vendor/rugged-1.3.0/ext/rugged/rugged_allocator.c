/*
 * Copyright (C) the Rugged contributors.  All rights reserved.
 *
 * This file is part of Rugged, distributed under the MIT license.
 * For full terms see the included LICENSE file.
 */

#include "rugged.h"
#include <git2/sys/alloc.h>

static void *rugged_gmalloc(size_t n, const char *file, int line)
{
	return xmalloc(n);
}

static void *rugged_gcalloc(size_t nelem, size_t elsize, const char *file, int line)
{
	return xcalloc(nelem, elsize);
}

static char *rugged_gstrdup(const char *str, const char *file, int line)
{
	return ruby_strdup(str);
}

static char *rugged_gstrndup(const char *str, size_t n, const char *file, int line)
{
	size_t len;
	char *newstr;

	len = strnlen(str, n);
	if (len < n)
		n = len;

	newstr = xmalloc(n+1);
	memcpy(newstr, str, n);
	newstr[n] = '\0';

	return newstr;
}

static char *rugged_gsubstrdup(const char *str, size_t n, const char *file, int line)
{
	char *newstr;

	newstr = xmalloc(n+1);
	memcpy(newstr, str, n);
	newstr[n] = '\0';

	return newstr;
}

static void *rugged_grealloc(void *ptr, size_t size, const char *file, int line)
{
	return xrealloc(ptr, size);
}

static void *rugged_greallocarray(void *ptr, size_t nelem, size_t elsize, const char *file, int line)
{
	return xrealloc2(ptr, nelem, elsize);
}

static void *rugged_gmallocarray(size_t nelem, size_t elsize, const char *file, int line)
{
	return xmalloc2(nelem, elsize);
}

static void rugged_gfree(void *ptr)
{
	xfree(ptr);
}

void rugged_set_allocator(void)
{
	git_allocator allocator;

	allocator.gmalloc = rugged_gmalloc;
	allocator.gcalloc = rugged_gcalloc;
	allocator.gstrdup = rugged_gstrdup;
	allocator.gstrndup = rugged_gstrndup;
	allocator.gstrndup = rugged_gstrndup;
	allocator.gsubstrdup = rugged_gsubstrdup;
	allocator.grealloc = rugged_grealloc;
	allocator.greallocarray = rugged_greallocarray;
	allocator.gmallocarray = rugged_gmallocarray;
	allocator.gfree = rugged_gfree;

	git_libgit2_opts(GIT_OPT_SET_ALLOCATOR, &allocator);
}
