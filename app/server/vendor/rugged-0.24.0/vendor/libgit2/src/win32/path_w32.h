/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_git_path_w32_h__
#define INCLUDE_git_path_w32_h__

#include "common.h"
#include "vector.h"

/*
 * Provides a large enough buffer to support Windows paths:  MAX_PATH is
 * 260, corresponding to a maximum path length of 259 characters plus a
 * NULL terminator.  Prefixing with "\\?\" adds 4 characters, but if the
 * original was a UNC path, then we turn "\\server\share" into
 * "\\?\UNC\server\share".  So we replace the first two characters with
 * 8 characters, a net gain of 6, so the maximum length is MAX_PATH+6.
 */
#define GIT_WIN_PATH_UTF16		MAX_PATH+6

/* Maximum size of a UTF-8 Win32 path.  We remove the "\\?\" or "\\?\UNC\"
 * prefixes for presentation, bringing us back to 259 (non-NULL)
 * characters.  UTF-8 does have 4-byte sequences, but they are encoded in
 * UTF-16 using surrogate pairs, which takes up the space of two characters.
 * Two characters in the range U+0800 -> U+FFFF take up more space in UTF-8
 * (6 bytes) than one surrogate pair (4 bytes).
 */
#define GIT_WIN_PATH_UTF8		(259 * 3 + 1)

/*
 * The length of a Windows "shortname", for 8.3 compatibility.
 */
#define GIT_WIN_PATH_SHORTNAME  13

/* Win32 path types */
typedef wchar_t git_win32_path[GIT_WIN_PATH_UTF16];
typedef char git_win32_utf8_path[GIT_WIN_PATH_UTF8];

/**
 * Create a Win32 path (in UCS-2 format) from a UTF-8 string.
 *
 * @param dest The buffer to receive the wide string.
 * @param src The UTF-8 string to convert.
 * @return The length of the wide string, in characters (not counting the NULL terminator), or < 0 for failure
 */
extern int git_win32_path_from_utf8(git_win32_path dest, const char *src);

/**
 * Canonicalize a Win32 UCS-2 path so that it is suitable for delivery to the
 * Win32 APIs: remove multiple directory separators, squashing to a single one,
 * strip trailing directory separators, ensure directory separators are all
 * canonical (always backslashes, never forward slashes) and process any
 * directory entries of '.' or '..'.
 *
 * This processes the buffer in place.
 *
 * @param path The buffer to process
 * @return The new length of the buffer, in wchar_t's (not counting the NULL terminator)
 */
extern int git_win32_path_canonicalize(git_win32_path path);

/**
 * Create an internal format (posix-style) UTF-8 path from a Win32 UCS-2 path.
 *
 * @param dest The buffer to receive the UTF-8 string.
 * @param src The wide string to convert.
 * @return The length of the UTF-8 string, in bytes (not counting the NULL terminator), or < 0 for failure
 */
extern int git_win32_path_to_utf8(git_win32_utf8_path dest, const wchar_t *src);

/**
 * Get the short name for the terminal path component in the given path.
 * For example, given "C:\Foo\Bar\Asdf.txt", this will return the short name
 * for the file "Asdf.txt".
 *
 * @param path The given path in UTF-8
 * @return The name of the shortname for the given path
 */
extern char *git_win32_path_8dot3_name(const char *path);

extern int git_win32_path_readlink_w(git_win32_path dest, const git_win32_path path);

#endif
