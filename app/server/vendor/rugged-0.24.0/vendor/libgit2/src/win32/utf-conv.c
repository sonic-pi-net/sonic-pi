/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "utf-conv.h"

GIT_INLINE(void) git__set_errno(void)
{
	if (GetLastError() == ERROR_INSUFFICIENT_BUFFER)
		errno = ENAMETOOLONG;
	else
		errno = EINVAL;
}

/**
 * Converts a UTF-8 string to wide characters.
 *
 * @param dest The buffer to receive the wide string.
 * @param dest_size The size of the buffer, in characters.
 * @param src The UTF-8 string to convert.
 * @return The length of the wide string, in characters (not counting the NULL terminator), or < 0 for failure
 */
int git__utf8_to_16(wchar_t *dest, size_t dest_size, const char *src)
{
	int len;

	/* Length of -1 indicates NULL termination of the input string. Subtract 1 from the result to
	* turn 0 into -1 (an error code) and to not count the NULL terminator as part of the string's
	* length. MultiByteToWideChar never returns int's minvalue, so underflow is not possible */
	if ((len = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, src, -1, dest, (int)dest_size) - 1) < 0)
		git__set_errno();

	return len;
}

/**
 * Converts a wide string to UTF-8.
 *
 * @param dest The buffer to receive the UTF-8 string.
 * @param dest_size The size of the buffer, in bytes.
 * @param src The wide string to convert.
 * @return The length of the UTF-8 string, in bytes (not counting the NULL terminator), or < 0 for failure
 */
int git__utf16_to_8(char *dest, size_t dest_size, const wchar_t *src)
{
	int len;

	/* Length of -1 indicates NULL termination of the input string. Subtract 1 from the result to
	 * turn 0 into -1 (an error code) and to not count the NULL terminator as part of the string's
	 * length. WideCharToMultiByte never returns int's minvalue, so underflow is not possible */
	if ((len = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, src, -1, dest, (int)dest_size, NULL, NULL) - 1) < 0)
		git__set_errno();

	return len;
}

/**
 * Converts a UTF-8 string to wide characters.
 * Memory is allocated to hold the converted string.
 * The caller is responsible for freeing the string with git__free.
 *
 * @param dest Receives a pointer to the wide string.
 * @param src The UTF-8 string to convert.
 * @return The length of the wide string, in characters (not counting the NULL terminator), or < 0 for failure
 */
int git__utf8_to_16_alloc(wchar_t **dest, const char *src)
{
	int utf16_size;

	*dest = NULL;

	/* Length of -1 indicates NULL termination of the input string */
	utf16_size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, src, -1, NULL, 0);

	if (!utf16_size) {
		git__set_errno();
		return -1;
	}

	if (!(*dest = git__mallocarray(utf16_size, sizeof(wchar_t)))) {
		errno = ENOMEM;
		return -1;
	}

	utf16_size = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, src, -1, *dest, utf16_size);

	if (!utf16_size) {
		git__set_errno();

		git__free(*dest);
		*dest = NULL;
	}

	/* Subtract 1 from the result to turn 0 into -1 (an error code) and to not count the NULL
	 * terminator as part of the string's length. MultiByteToWideChar never returns int's minvalue,
	 * so underflow is not possible */
	return utf16_size - 1;
}

/**
 * Converts a wide string to UTF-8.
 * Memory is allocated to hold the converted string.
 * The caller is responsible for freeing the string with git__free.
 *
 * @param dest Receives a pointer to the UTF-8 string.
 * @param src The wide string to convert.
 * @return The length of the UTF-8 string, in bytes (not counting the NULL terminator), or < 0 for failure
 */
int git__utf16_to_8_alloc(char **dest, const wchar_t *src)
{
	int utf8_size;

	*dest = NULL;

	/* Length of -1 indicates NULL termination of the input string */
	utf8_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, src, -1, NULL, 0, NULL, NULL);

	if (!utf8_size) {
		git__set_errno();
		return -1;
	}

	*dest = git__malloc(utf8_size);

	if (!*dest) {
		errno = ENOMEM;
		return -1;
	}

	utf8_size = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, src, -1, *dest, utf8_size, NULL, NULL);

	if (!utf8_size) {
		git__set_errno();

		git__free(*dest);
		*dest = NULL;
	}

	/* Subtract 1 from the result to turn 0 into -1 (an error code) and to not count the NULL
	 * terminator as part of the string's length. MultiByteToWideChar never returns int's minvalue,
	 * so underflow is not possible */
	return utf8_size - 1;
}
