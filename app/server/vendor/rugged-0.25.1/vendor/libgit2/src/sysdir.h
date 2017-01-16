/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_sysdir_h__
#define INCLUDE_sysdir_h__

#include "common.h"
#include "posix.h"
#include "buffer.h"

/**
 * Find a "global" file (i.e. one in a user's home directory).
 *
 * @param path buffer to write the full path into
 * @param filename name of file to find in the home directory
 * @return 0 if found, GIT_ENOTFOUND if not found, or -1 on other OS error
 */
extern int git_sysdir_find_global_file(git_buf *path, const char *filename);

/**
 * Find an "XDG" file (i.e. one in user's XDG config path).
 *
 * @param path buffer to write the full path into
 * @param filename name of file to find in the home directory
 * @return 0 if found, GIT_ENOTFOUND if not found, or -1 on other OS error
 */
extern int git_sysdir_find_xdg_file(git_buf *path, const char *filename);

/**
 * Find a "system" file (i.e. one shared for all users of the system).
 *
 * @param path buffer to write the full path into
 * @param filename name of file to find in the home directory
 * @return 0 if found, GIT_ENOTFOUND if not found, or -1 on other OS error
 */
extern int git_sysdir_find_system_file(git_buf *path, const char *filename);

/**
 * Find a "ProgramData" file (i.e. one in %PROGRAMDATA%)
 *
 * @param path buffer to write the full path into
 * @param filename name of file to find in the ProgramData directory
 * @return 0 if found, GIT_ENOTFOUND if not found, or -1 on other OS error
 */
extern int git_sysdir_find_programdata_file(git_buf *path, const char *filename);

/**
 * Find template directory.
 *
 * @param path buffer to write the full path into
 * @return 0 if found, GIT_ENOTFOUND if not found, or -1 on other OS error
 */
extern int git_sysdir_find_template_dir(git_buf *path);

typedef enum {
	GIT_SYSDIR_SYSTEM = 0,
	GIT_SYSDIR_GLOBAL = 1,
	GIT_SYSDIR_XDG    = 2,
	GIT_SYSDIR_PROGRAMDATA = 3,
	GIT_SYSDIR_TEMPLATE = 4,
	GIT_SYSDIR__MAX   = 5,
} git_sysdir_t;

/**
 * Configures global data for configuration file search paths.
 *
 * @return 0 on success, <0 on failure
 */
extern int git_sysdir_global_init(void);

/**
 * Get the search path for global/system/xdg files
 *
 * @param out pointer to git_buf containing search path
 * @param which which list of paths to return
 * @return 0 on success, <0 on failure
 */
extern int git_sysdir_get(const git_buf **out, git_sysdir_t which);

/**
 * Get search path into a preallocated buffer
 *
 * @param out String buffer to write into
 * @param outlen Size of string buffer
 * @param which Which search path to return
 * @return 0 on success, GIT_EBUFS if out is too small, <0 on other failure
 */

extern int git_sysdir_get_str(char *out, size_t outlen, git_sysdir_t which);

/**
 * Set search paths for global/system/xdg files
 *
 * The first occurrence of the magic string "$PATH" in the new value will
 * be replaced with the old value of the search path.
 *
 * @param which Which search path to modify
 * @param paths New search path (separated by GIT_PATH_LIST_SEPARATOR)
 * @return 0 on success, <0 on failure (allocation error)
 */
extern int git_sysdir_set(git_sysdir_t which, const char *paths);

#endif /* INCLUDE_sysdir_h__ */
