/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_w32_util_h__
#define INCLUDE_w32_util_h__

#include "utf-conv.h"
#include "posix.h"
#include "path_w32.h"

/*

#include "common.h"
#include "path.h"
#include "path_w32.h"
#include "utf-conv.h"
#include "posix.h"
#include "reparse.h"
#include "dir.h"
*/


GIT_INLINE(bool) git_win32__isalpha(wchar_t c)
{
	return ((c >= L'A' && c <= L'Z') || (c >= L'a' && c <= L'z'));
}

/**
 * Creates a FindFirstFile(Ex) filter string from a UTF-8 path.
 * The filter string enumerates all items in the directory.
 *
 * @param dest The buffer to receive the filter string.
 * @param src The UTF-8 path of the directory to enumerate.
 * @return True if the filter string was created successfully; false otherwise
 */
bool git_win32__findfirstfile_filter(git_win32_path dest, const char *src);

/**
 * Ensures the given path (file or folder) has the +H (hidden) attribute set.
 *
 * @param path The path which should receive the +H bit.
 * @return 0 on success; -1 on failure
 */
int git_win32__sethidden(const char *path);

/**
 * Removes any trailing backslashes from a path, except in the case of a drive
 * letter path (C:\, D:\, etc.). This function cannot fail.
 *
 * @param path The path which should be trimmed.
 * @return The length of the modified string (<= the input length)
 */
size_t git_win32__path_trim_end(wchar_t *str, size_t len);

/**
 * Removes any of the following namespace prefixes from a path,
 * if found: "\??\", "\\?\", "\\?\UNC\". This function cannot fail.
 *
 * @param path The path which should be converted.
 * @return The length of the modified string (<= the input length)
 */
size_t git_win32__canonicalize_path(wchar_t *str, size_t len);

/**
 * Converts a FILETIME structure to a time_t.
 *
 * @param FILETIME A pointer to a FILETIME
 * @return A time_t containing the same time
 */
GIT_INLINE(time_t) git_win32__filetime_to_time_t(const FILETIME *ft)
{
	long long winTime = ((long long)ft->dwHighDateTime << 32) + ft->dwLowDateTime;
	winTime -= 116444736000000000LL; /* Windows to Unix Epoch conversion */
	winTime /= 10000000;             /* Nano to seconds resolution */
	return (time_t)winTime;
}

GIT_INLINE(void) git_win32__timeval_to_filetime(
	FILETIME *ft, const struct timeval tv)
{
	long long ticks = (tv.tv_sec * 10000000LL) +
		(tv.tv_usec * 10LL) + 116444736000000000LL;

	ft->dwHighDateTime = ((ticks >> 32) & 0xffffffffLL);
	ft->dwLowDateTime = (ticks & 0xffffffffLL);
}

GIT_INLINE(int) git_win32__file_attribute_to_stat(
	struct stat *st,
	const WIN32_FILE_ATTRIBUTE_DATA *attrdata,
	const wchar_t *path)
{
	mode_t mode = S_IREAD;

	if (attrdata->dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
		mode |= S_IFDIR;
	else
		mode |= S_IFREG;

	if ((attrdata->dwFileAttributes & FILE_ATTRIBUTE_READONLY) == 0)
		mode |= S_IWRITE;

	st->st_ino = 0;
	st->st_gid = 0;
	st->st_uid = 0;
	st->st_nlink = 1;
	st->st_mode = mode;
	st->st_size = ((git_off_t)attrdata->nFileSizeHigh << 32) + attrdata->nFileSizeLow;
	st->st_dev = _getdrive() - 1;
	st->st_rdev = st->st_dev;
	st->st_atime = git_win32__filetime_to_time_t(&(attrdata->ftLastAccessTime));
	st->st_mtime = git_win32__filetime_to_time_t(&(attrdata->ftLastWriteTime));
	st->st_ctime = git_win32__filetime_to_time_t(&(attrdata->ftCreationTime));

	if (attrdata->dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT && path) {
		git_win32_path target;

		if (git_win32_path_readlink_w(target, path) >= 0) {
			st->st_mode = (st->st_mode & ~S_IFMT) | S_IFLNK;

			/* st_size gets the UTF-8 length of the target name, in bytes,
				* not counting the NULL terminator */
			if ((st->st_size = git__utf16_to_8(NULL, 0, target)) < 0) {
				giterr_set(GITERR_OS, "Could not convert reparse point name for '%s'", path);
				return -1;
			}
		}
	}

	return 0;
}

#endif
