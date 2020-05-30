/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_win32_msvc_compat_h__
#define INCLUDE_win32_msvc_compat_h__

#if defined(_MSC_VER)

typedef unsigned short mode_t;
typedef SSIZE_T ssize_t;

#ifdef _WIN64
# define SSIZE_MAX _I64_MAX
#else
# define SSIZE_MAX LONG_MAX
#endif

#define strcasecmp(s1, s2) _stricmp(s1, s2)
#define strncasecmp(s1, s2, c) _strnicmp(s1, s2, c)

#endif

#define GIT_STDLIB_CALL __cdecl

#endif
