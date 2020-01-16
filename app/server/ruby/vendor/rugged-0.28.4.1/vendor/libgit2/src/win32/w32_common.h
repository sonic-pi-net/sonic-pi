/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_win32_w32_common_h__
#define INCLUDE_win32_w32_common_h__

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

#endif
