/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_msvc_compat__
#define INCLUDE_msvc_compat__

#if defined(_MSC_VER)

/* 64-bit stat information, regardless of USE_32BIT_TIME_T define */
#define stat __stat64

typedef unsigned short mode_t;
typedef SSIZE_T ssize_t;

#define strcasecmp(s1, s2) _stricmp(s1, s2)
#define strncasecmp(s1, s2, c) _strnicmp(s1, s2, c)

#endif

#define GIT_STDLIB_CALL __cdecl

#endif /* INCLUDE_msvc_compat__ */
