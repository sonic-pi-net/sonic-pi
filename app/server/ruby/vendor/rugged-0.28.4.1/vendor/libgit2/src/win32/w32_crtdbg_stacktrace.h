/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_win32_w32_crtdbg_stacktrace_h__
#define INCLUDE_win32_w32_crtdbg_stacktrace_h__

#include "common.h"

#if defined(GIT_MSVC_CRTDBG)

#include <stdlib.h>
#include <crtdbg.h>

#include "git2/errors.h"
#include "strnlen.h"

/* MSVC CRTDBG memory leak reporting.
 *
 * We DO NOT use the "_CRTDBG_MAP_ALLOC" macro described in the MSVC
 * documentation because all allocs/frees in libgit2 already go through
 * the "git__" routines defined in this file.  Simply using the normal
 * reporting mechanism causes all leaks to be attributed to a routine
 * here in util.h (ie, the actual call to calloc()) rather than the
 * caller of git__calloc().
 *
 * Therefore, we declare a set of "git__crtdbg__" routines to replace
 * the corresponding "git__" routines and re-define the "git__" symbols
 * as macros.  This allows us to get and report the file:line info of
 * the real caller.
 *
 * We DO NOT replace the "git__free" routine because it needs to remain
 * a function pointer because it is used as a function argument when
 * setting up various structure "destructors".
 *
 * We also DO NOT use the "_CRTDBG_MAP_ALLOC" macro because it causes
 * "free" to be remapped to "_free_dbg" and this causes problems for
 * structures which define a field named "free".
 *
 * Finally, CRTDBG must be explicitly enabled and configured at program
 * startup.  See tests/main.c for an example.
 */

/**
 * Initialize our memory leak tracking and de-dup data structures.
 * This should ONLY be called by git_libgit2_init().
 */
void git_win32__crtdbg_stacktrace_init(void);

/**
 * Shutdown our memory leak tracking and dump summary data.
 * This should ONLY be called by git_libgit2_shutdown().
 *
 * We explicitly call _CrtDumpMemoryLeaks() during here so
 * that we can compute summary data for the leaks. We print
 * the stacktrace of each unique leak.
 *
 * This cleanup does not happen if the app calls exit()
 * without calling the libgit2 shutdown code.
 *
 * This info we print here is independent of any automatic
 * reporting during exit() caused by _CRTDBG_LEAK_CHECK_DF.
 * Set it in your app if you also want traditional reporting.
 */
void git_win32__crtdbg_stacktrace_cleanup(void);

/**
 * Checkpoint options.
 */
typedef enum git_win32__crtdbg_stacktrace_options {
	/**
	 * Set checkpoint marker.
	 */
	GIT_WIN32__CRTDBG_STACKTRACE__SET_MARK         = (1 << 0),

	/**
	 * Dump leaks since last checkpoint marker.
	 * May not be combined with __LEAKS_TOTAL.
	 *
	 * Note that this may generate false positives for global TLS
	 * error state and other global caches that aren't cleaned up
	 * until the thread/process terminates.  So when using this
	 * around a region of interest, also check the final (at exit)
	 * dump before digging into leaks reported here.
	 */
	GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_SINCE_MARK = (1 << 1),

	/**
	 * Dump leaks since init.  May not be combined
	 * with __LEAKS_SINCE_MARK.
	 */
	GIT_WIN32__CRTDBG_STACKTRACE__LEAKS_TOTAL      = (1 << 2),

	/**
	 * Suppress printing during dumps.
	 * Just return leak count.
	 */
	GIT_WIN32__CRTDBG_STACKTRACE__QUIET            = (1 << 3),

} git_win32__crtdbg_stacktrace_options;

/**
 * Checkpoint memory state and/or dump unique stack traces of
 * current memory leaks.
 *
 * @return number of unique leaks (relative to requested starting
 * point) or error.
 */
GIT_EXTERN(int) git_win32__crtdbg_stacktrace__dump(
	git_win32__crtdbg_stacktrace_options opt,
	const char *label);

/**
 * Construct stacktrace and append it to the global buffer.
 * Return pointer to start of this string.  On any error or
 * lack of buffer space, just return the given file buffer
 * so it will behave as usual.
 *
 * This should ONLY be called by our internal memory allocations
 * routines.
 */
const char *git_win32__crtdbg_stacktrace(int skip, const char *file);

#endif
#endif
