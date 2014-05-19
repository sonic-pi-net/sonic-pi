/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_git_utfconv_h__
#define INCLUDE_git_utfconv_h__

#include <wchar.h>
#include "common.h"

/* Equal to the Win32 MAX_PATH constant. The maximum path length is 259
 * characters plus a NULL terminator. */
#define GIT_WIN_PATH_UTF16		260

/* Maximum size of a UTF-8 Win32 path. UTF-8 does have 4-byte sequences,
 * but they are encoded in UTF-16 using surrogate pairs, which takes up
 * the space of two characters. Two characters in the range U+0800 ->
 * U+FFFF take up more space in UTF-8 (6 bytes) than one surrogate pair
 * (4 bytes). */
#define GIT_WIN_PATH_UTF8		(259 * 3 + 1)

/* Win32 path types */
typedef wchar_t git_win32_path[GIT_WIN_PATH_UTF16];
typedef char git_win32_utf8_path[GIT_WIN_PATH_UTF8];

/**
 * Converts a UTF-8 string to wide characters.
 *
 * @param dest The buffer to receive the wide string.
 * @param dest_size The size of the buffer, in characters.
 * @param src The UTF-8 string to convert.
 * @return The length of the wide string, in characters (not counting the NULL terminator), or < 0 for failure
 */
int git__utf8_to_16(wchar_t *dest, size_t dest_size, const char *src);

/**
 * Converts a wide string to UTF-8.
 *
 * @param dest The buffer to receive the UTF-8 string.
 * @param dest_size The size of the buffer, in bytes.
 * @param src The wide string to convert.
 * @return The length of the UTF-8 string, in bytes (not counting the NULL terminator), or < 0 for failure
 */
int git__utf16_to_8(char *dest, size_t dest_size, const wchar_t *src);

/**
 * Converts a UTF-8 string to wide characters.
 * Memory is allocated to hold the converted string.
 * The caller is responsible for freeing the string with git__free.
 *
 * @param dest Receives a pointer to the wide string.
 * @param src The UTF-8 string to convert.
 * @return The length of the wide string, in characters (not counting the NULL terminator), or < 0 for failure
 */
int git__utf8_to_16_alloc(wchar_t **dest, const char *src);

/**
 * Converts a wide string to UTF-8.
 * Memory is allocated to hold the converted string.
 * The caller is responsible for freeing the string with git__free.
 *
 * @param dest Receives a pointer to the UTF-8 string.
 * @param src The wide string to convert.
 * @return The length of the UTF-8 string, in bytes (not counting the NULL terminator), or < 0 for failure
 */
int git__utf16_to_8_alloc(char **dest, const wchar_t *src);

/**
 * Converts a UTF-8 Win32 path to wide characters.
 *
 * @param dest The buffer to receive the wide string.
 * @param src The UTF-8 string to convert.
 * @return The length of the wide string, in characters (not counting the NULL terminator), or < 0 for failure
 */
GIT_INLINE(int) git_win32_path_from_utf8(git_win32_path dest, const char *src)
{
	return git__utf8_to_16(dest, GIT_WIN_PATH_UTF16, src);
}

/**
 * Converts a wide Win32 path to UTF-8.
 *
 * @param dest The buffer to receive the UTF-8 string.
 * @param src The wide string to convert.
 * @return The length of the UTF-8 string, in bytes (not counting the NULL terminator), or < 0 for failure
 */
GIT_INLINE(int) git_win32_path_to_utf8(git_win32_utf8_path dest, const wchar_t *src)
{
	return git__utf16_to_8(dest, GIT_WIN_PATH_UTF8, src);
}

#endif
