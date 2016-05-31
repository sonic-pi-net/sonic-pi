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
 * Ensures the given path (file or folder) has the +H (hidden) attribute set
 * or unset.
 *
 * @param path The path that should receive the +H bit.
 * @param hidden true to set +H, false to unset it
 * @return 0 on success; -1 on failure
 */
extern int git_win32__set_hidden(const char *path, bool hidden);

/**
 * Determines if the given file or folder has the hidden attribute set.
 * @param hidden pointer to store hidden value
 * @param path The path that should be queried for hiddenness.
 * @return 0 on success or an error code.
 */
extern int git_win32__hidden(bool *hidden, const char *path);

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
 * Converts a FILETIME structure to a struct timespec.
 *
 * @param FILETIME A pointer to a FILETIME
 * @param ts A pointer to the timespec structure to fill in
 */
GIT_INLINE(void) git_win32__filetime_to_timespec(
	const FILETIME *ft,
	struct timespec *ts)
{
	long long winTime = ((long long)ft->dwHighDateTime << 32) + ft->dwLowDateTime;
	winTime -= 116444736000000000LL; /* Windows to Unix Epoch conversion */
	ts->tv_sec = (time_t)(winTime / 10000000);
#ifdef GIT_USE_NSEC
	ts->tv_nsec = (winTime % 10000000) * 100;
#else
	ts->tv_nsec = 0;
#endif
}

GIT_INLINE(void) git_win32__timeval_to_filetime(
	FILETIME *ft, const struct p_timeval tv)
{
	long long ticks = (tv.tv_sec * 10000000LL) +
		(tv.tv_usec * 10LL) + 116444736000000000LL;

	ft->dwHighDateTime = ((ticks >> 32) & 0xffffffffLL);
	ft->dwLowDateTime = (ticks & 0xffffffffLL);
}

GIT_INLINE(void) git_win32__stat_init(
	struct stat *st,
	DWORD dwFileAttributes,
	DWORD nFileSizeHigh,
	DWORD nFileSizeLow,
	FILETIME ftCreationTime,
	FILETIME ftLastAccessTime,
	FILETIME ftLastWriteTime)
{
	mode_t mode = S_IREAD;

	memset(st, 0, sizeof(struct stat));

	if (dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
		mode |= S_IFDIR;
	else
		mode |= S_IFREG;

	if ((dwFileAttributes & FILE_ATTRIBUTE_READONLY) == 0)
		mode |= S_IWRITE;

	st->st_ino = 0;
	st->st_gid = 0;
	st->st_uid = 0;
	st->st_nlink = 1;
	st->st_mode = mode;
	st->st_size = ((git_off_t)nFileSizeHigh << 32) + nFileSizeLow;
	st->st_dev = _getdrive() - 1;
	st->st_rdev = st->st_dev;
	git_win32__filetime_to_timespec(&ftLastAccessTime, &(st->st_atim));
	git_win32__filetime_to_timespec(&ftLastWriteTime, &(st->st_mtim));
	git_win32__filetime_to_timespec(&ftCreationTime, &(st->st_ctim));
}

GIT_INLINE(void) git_win32__file_information_to_stat(
	struct stat *st,
	const BY_HANDLE_FILE_INFORMATION *fileinfo)
{
	git_win32__stat_init(st,
		fileinfo->dwFileAttributes,
		fileinfo->nFileSizeHigh,
		fileinfo->nFileSizeLow,
		fileinfo->ftCreationTime,
		fileinfo->ftLastAccessTime,
		fileinfo->ftLastWriteTime);
}

GIT_INLINE(int) git_win32__file_attribute_to_stat(
	struct stat *st,
	const WIN32_FILE_ATTRIBUTE_DATA *attrdata,
	const wchar_t *path)
{
	git_win32__stat_init(st,
		attrdata->dwFileAttributes,
		attrdata->nFileSizeHigh,
		attrdata->nFileSizeLow,
		attrdata->ftCreationTime,
		attrdata->ftLastAccessTime,
		attrdata->ftLastWriteTime);

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
