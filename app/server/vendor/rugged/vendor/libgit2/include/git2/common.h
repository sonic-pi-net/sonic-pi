/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_git_common_h__
#define INCLUDE_git_common_h__

#include <time.h>
#include <stdlib.h>

#ifdef _MSC_VER
#	include "inttypes.h"
#else
#	include <inttypes.h>
#endif

#ifdef __cplusplus
# define GIT_BEGIN_DECL extern "C" {
# define GIT_END_DECL	}
#else
 /** Start declarations in C mode */
# define GIT_BEGIN_DECL /* empty */
 /** End declarations in C mode */
# define GIT_END_DECL	/* empty */
#endif

/** Declare a public function exported for application use. */
#if __GNUC__ >= 4
# define GIT_EXTERN(type) extern \
			 __attribute__((visibility("default"))) \
			 type
#elif defined(_MSC_VER)
# define GIT_EXTERN(type) __declspec(dllexport) type
#else
# define GIT_EXTERN(type) extern type
#endif

/** Declare a function's takes printf style arguments. */
#ifdef __GNUC__
# define GIT_FORMAT_PRINTF(a,b) __attribute__((format (printf, a, b)))
#else
# define GIT_FORMAT_PRINTF(a,b) /* empty */
#endif

#if (defined(_WIN32)) && !defined(__CYGWIN__)
#define GIT_WIN32 1
#endif

#ifdef __amigaos4__
#include <netinet/in.h>
#endif

/**
 * @file git2/common.h
 * @brief Git common platform definitions
 * @defgroup git_common Git common platform definitions
 * @ingroup Git
 * @{
 */

GIT_BEGIN_DECL

/**
 * The separator used in path list strings (ie like in the PATH
 * environment variable). A semi-colon ";" is used on Windows, and
 * a colon ":" for all other systems.
 */
#ifdef GIT_WIN32
#define GIT_PATH_LIST_SEPARATOR ';'
#else
#define GIT_PATH_LIST_SEPARATOR ':'
#endif

/**
 * The maximum length of a valid git path.
 */
#define GIT_PATH_MAX 4096

/**
 * The string representation of the null object ID.
 */
#define GIT_OID_HEX_ZERO "0000000000000000000000000000000000000000"

/**
 * Return the version of the libgit2 library
 * being currently used.
 *
 * @param major Store the major version number
 * @param minor Store the minor version number
 * @param rev Store the revision (patch) number
 */
GIT_EXTERN(void) git_libgit2_version(int *major, int *minor, int *rev);

/**
 * Combinations of these values describe the features with which libgit2
 * was compiled
 */
typedef enum {
	GIT_FEATURE_THREADS	= (1 << 0),
	GIT_FEATURE_HTTPS = (1 << 1),
	GIT_FEATURE_SSH = (1 << 2),
} git_feature_t;

/**
 * Query compile time options for libgit2.
 *
 * @return A combination of GIT_FEATURE_* values.
 *
 * - GIT_FEATURE_THREADS
 *   Libgit2 was compiled with thread support. Note that thread support is
 *   still to be seen as a 'work in progress' - basic object lookups are
 *   believed to be threadsafe, but other operations may not be.
 *
 * - GIT_FEATURE_HTTPS
 *   Libgit2 supports the https:// protocol. This requires the openssl
 *   library to be found when compiling libgit2.
 *
 * - GIT_FEATURE_SSH
 *   Libgit2 supports the SSH protocol for network operations. This requires
 *   the libssh2 library to be found when compiling libgit2
 */
GIT_EXTERN(int) git_libgit2_features(void);

/**
 * Global library options
 *
 * These are used to select which global option to set or get and are
 * used in `git_libgit2_opts()`.
 */
typedef enum {
	GIT_OPT_GET_MWINDOW_SIZE,
	GIT_OPT_SET_MWINDOW_SIZE,
	GIT_OPT_GET_MWINDOW_MAPPED_LIMIT,
	GIT_OPT_SET_MWINDOW_MAPPED_LIMIT,
	GIT_OPT_GET_SEARCH_PATH,
	GIT_OPT_SET_SEARCH_PATH,
	GIT_OPT_SET_CACHE_OBJECT_LIMIT,
	GIT_OPT_SET_CACHE_MAX_SIZE,
	GIT_OPT_ENABLE_CACHING,
	GIT_OPT_GET_CACHED_MEMORY,
	GIT_OPT_GET_TEMPLATE_PATH,
	GIT_OPT_SET_TEMPLATE_PATH,
	GIT_OPT_SET_SSL_CERT_LOCATIONS,
} git_libgit2_opt_t;

/**
 * Set or query a library global option
 *
 * Available options:
 *
 *	* opts(GIT_OPT_GET_MWINDOW_SIZE, size_t *):
 *
 *		> Get the maximum mmap window size
 *
 *	* opts(GIT_OPT_SET_MWINDOW_SIZE, size_t):
 *
 *		> Set the maximum mmap window size
 *
 *	* opts(GIT_OPT_GET_MWINDOW_MAPPED_LIMIT, size_t *):
 *
 *		> Get the maximum memory that will be mapped in total by the library
 *
 *	* opts(GIT_OPT_SET_MWINDOW_MAPPED_LIMIT, size_t):
 *
 *		>Set the maximum amount of memory that can be mapped at any time
 *		by the library
 *
 *	* opts(GIT_OPT_GET_SEARCH_PATH, int level, git_buf *buf)
 *
 *		> Get the search path for a given level of config data.  "level" must
 *		> be one of `GIT_CONFIG_LEVEL_SYSTEM`, `GIT_CONFIG_LEVEL_GLOBAL`, or
 *		> `GIT_CONFIG_LEVEL_XDG`.  The search path is written to the `out`
 *		> buffer.
 *
 *	* opts(GIT_OPT_SET_SEARCH_PATH, int level, const char *path)
 *
 *		> Set the search path for a level of config data.  The search path
 *		> applied to shared attributes and ignore files, too.
 *		>
 *		> - `path` lists directories delimited by GIT_PATH_LIST_SEPARATOR.
 *		>   Pass NULL to reset to the default (generally based on environment
 *		>   variables).  Use magic path `$PATH` to include the old value
 *		>   of the path (if you want to prepend or append, for instance).
 *		>
 *		> - `level` must be GIT_CONFIG_LEVEL_SYSTEM, GIT_CONFIG_LEVEL_GLOBAL,
 *		>   or GIT_CONFIG_LEVEL_XDG.
 *
 *	* opts(GIT_OPT_SET_CACHE_OBJECT_LIMIT, git_otype type, size_t size)
 *
 *		> Set the maximum data size for the given type of object to be
 *		> considered eligible for caching in memory.  Setting to value to
 *		> zero means that that type of object will not be cached.
 *		> Defaults to 0 for GIT_OBJ_BLOB (i.e. won't cache blobs) and 4k
 *		> for GIT_OBJ_COMMIT, GIT_OBJ_TREE, and GIT_OBJ_TAG.
 *
 *	* opts(GIT_OPT_SET_CACHE_MAX_SIZE, ssize_t max_storage_bytes)
 *
 *		> Set the maximum total data size that will be cached in memory
 *		> across all repositories before libgit2 starts evicting objects
 *		> from the cache.  This is a soft limit, in that the library might
 *		> briefly exceed it, but will start aggressively evicting objects
 *		> from cache when that happens.  The default cache size is 256MB.
 *
 *	* opts(GIT_OPT_ENABLE_CACHING, int enabled)
 *
 *		> Enable or disable caching completely.
 *		>
 *		> Because caches are repository-specific, disabling the cache
 *		> cannot immediately clear all cached objects, but each cache will
 *		> be cleared on the next attempt to update anything in it.
 *
 *	* opts(GIT_OPT_GET_CACHED_MEMORY, ssize_t *current, ssize_t *allowed)
 *
 *		> Get the current bytes in cache and the maximum that would be
 *		> allowed in the cache.
 *
 *	* opts(GIT_OPT_GET_TEMPLATE_PATH, git_buf *out)
 *
 *		> Get the default template path.
 *		> The path is written to the `out` buffer.
 *
 *	* opts(GIT_OPT_SET_TEMPLATE_PATH, const char *path)
 *
 *		> Set the default template path.
 *		>
 *		> - `path` directory of template.
 *
 *	* opts(GIT_OPT_SET_SSL_CERT_LOCATIONS, const char *file, const char *path)
 *
 *		> Set the SSL certificate-authority locations.
 *		>
 *		> - `file` is the location of a file containing several
 *		>   certificates concatenated together.
 *		> - `path` is the location of a directory holding several
 *		>   certificates, one per file.
 *		>
 * 		> Either parameter may be `NULL`, but not both.
 *
 * @param option Option key
 * @param ... value to set the option
 * @return 0 on success, <0 on failure
 */
GIT_EXTERN(int) git_libgit2_opts(int option, ...);

/** @} */
GIT_END_DECL

#endif
