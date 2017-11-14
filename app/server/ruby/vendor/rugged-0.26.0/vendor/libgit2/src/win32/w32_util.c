/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "w32_util.h"

/**
 * Creates a FindFirstFile(Ex) filter string from a UTF-8 path.
 * The filter string enumerates all items in the directory.
 *
 * @param dest The buffer to receive the filter string.
 * @param src The UTF-8 path of the directory to enumerate.
 * @return True if the filter string was created successfully; false otherwise
 */
bool git_win32__findfirstfile_filter(git_win32_path dest, const char *src)
{
	static const wchar_t suffix[] = L"\\*";
	int len = git_win32_path_from_utf8(dest, src);

	/* Ensure the path was converted */
	if (len < 0)
		return false;

	/* Ensure that the path does not end with a trailing slash,
	 * because we're about to add one. Don't rely our trim_end
	 * helper, because we want to remove the backslash even for
	 * drive letter paths, in this case. */
	if (len > 0 &&
		(dest[len - 1] == L'/' || dest[len - 1] == L'\\')) {
		dest[len - 1] = L'\0';
		len--;
	}

	/* Ensure we have enough room to add the suffix */
	if ((size_t)len >= GIT_WIN_PATH_UTF16 - CONST_STRLEN(suffix))
		return false;

	wcscat(dest, suffix);
	return true;
}

/**
 * Ensures the given path (file or folder) has the +H (hidden) attribute set.
 *
 * @param path The path which should receive the +H bit.
 * @return 0 on success; -1 on failure
 */
int git_win32__set_hidden(const char *path, bool hidden)
{
	git_win32_path buf;
	DWORD attrs, newattrs;

	if (git_win32_path_from_utf8(buf, path) < 0)
		return -1;

	attrs = GetFileAttributesW(buf);

	/* Ensure the path exists */
	if (attrs == INVALID_FILE_ATTRIBUTES)
		return -1;

	if (hidden)
		newattrs = attrs | FILE_ATTRIBUTE_HIDDEN;
	else
		newattrs = attrs & ~FILE_ATTRIBUTE_HIDDEN;

	if (attrs != newattrs && !SetFileAttributesW(buf, newattrs)) {
		giterr_set(GITERR_OS, "failed to %s hidden bit for '%s'",
			hidden ? "set" : "unset", path);
		return -1;
	}

	return 0;
}

int git_win32__hidden(bool *out, const char *path)
{
	git_win32_path buf;
	DWORD attrs;

	if (git_win32_path_from_utf8(buf, path) < 0)
		return -1;

	attrs = GetFileAttributesW(buf);

	/* Ensure the path exists */
	if (attrs == INVALID_FILE_ATTRIBUTES)
		return -1;

	*out = (attrs & FILE_ATTRIBUTE_HIDDEN) ? true : false;
	return 0;
}

/**
 * Removes any trailing backslashes from a path, except in the case of a drive
 * letter path (C:\, D:\, etc.). This function cannot fail.
 *
 * @param path The path which should be trimmed.
 * @return The length of the modified string (<= the input length)
 */
size_t git_win32__path_trim_end(wchar_t *str, size_t len)
{
	while (1) {
		if (!len || str[len - 1] != L'\\')
			break;

		/* Don't trim backslashes from drive letter paths, which
		 * are 3 characters long and of the form C:\, D:\, etc. */
		if (len == 3 && git_win32__isalpha(str[0]) && str[1] == ':')
			break;

		len--;
	}

	str[len] = L'\0';

	return len;
}

/**
 * Removes any of the following namespace prefixes from a path,
 * if found: "\??\", "\\?\", "\\?\UNC\". This function cannot fail.
 *
 * @param path The path which should be converted.
 * @return The length of the modified string (<= the input length)
 */
size_t git_win32__canonicalize_path(wchar_t *str, size_t len)
{
	static const wchar_t dosdevices_prefix[] = L"\\\?\?\\";
	static const wchar_t nt_prefix[] = L"\\\\?\\";
	static const wchar_t unc_prefix[] = L"UNC\\";
	size_t to_advance = 0;

	/* "\??\" -- DOS Devices prefix */
	if (len >= CONST_STRLEN(dosdevices_prefix) &&
		!wcsncmp(str, dosdevices_prefix, CONST_STRLEN(dosdevices_prefix))) {
		to_advance += CONST_STRLEN(dosdevices_prefix);
		len -= CONST_STRLEN(dosdevices_prefix);
	}
	/* "\\?\" -- NT namespace prefix */
	else if (len >= CONST_STRLEN(nt_prefix) &&
		!wcsncmp(str, nt_prefix, CONST_STRLEN(nt_prefix))) {
		to_advance += CONST_STRLEN(nt_prefix);
		len -= CONST_STRLEN(nt_prefix);
	}

	/* "\??\UNC\", "\\?\UNC\" -- UNC prefix */
	if (to_advance && len >= CONST_STRLEN(unc_prefix) &&
		!wcsncmp(str + to_advance, unc_prefix, CONST_STRLEN(unc_prefix))) {
		to_advance += CONST_STRLEN(unc_prefix);
		len -= CONST_STRLEN(unc_prefix);
	}

	if (to_advance) {
		memmove(str, str + to_advance, len * sizeof(wchar_t));
		str[len] = L'\0';
	}

	return git_win32__path_trim_end(str, len);
}
