/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "alloc.h"
#include "runtime.h"

#include "allocators/failalloc.h"
#include "allocators/stdalloc.h"
#include "allocators/win32_leakcheck.h"

/* Fail any allocation until git_libgit2_init is called. */
git_allocator git__allocator = {
	git_failalloc_malloc,
	git_failalloc_calloc,
	git_failalloc_strdup,
	git_failalloc_strndup,
	git_failalloc_substrdup,
	git_failalloc_realloc,
	git_failalloc_reallocarray,
	git_failalloc_mallocarray,
	git_failalloc_free
};

static int setup_default_allocator(void)
{
#if defined(GIT_WIN32_LEAKCHECK)
	return git_win32_leakcheck_init_allocator(&git__allocator);
#else
	return git_stdalloc_init_allocator(&git__allocator);
#endif
}

int git_allocator_global_init(void)
{
	/*
	 * We don't want to overwrite any allocator which has been set
	 * before the init function is called.
	 */
	if (git__allocator.gmalloc != git_failalloc_malloc)
		return 0;

	return setup_default_allocator();
}

int git_allocator_setup(git_allocator *allocator)
{
	if (!allocator)
		return setup_default_allocator();

	memcpy(&git__allocator, allocator, sizeof(*allocator));
	return 0;
}
