/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_mingw_compat__
#define INCLUDE_mingw_compat__

#if defined(__MINGW32__)

#if _WIN32_WINNT >= 0x0601
#define stat __stat64
#else
#define stat _stati64
#endif

#endif

#endif /* INCLUDE_mingw_compat__ */
