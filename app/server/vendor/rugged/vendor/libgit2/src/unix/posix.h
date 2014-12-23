/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_posix__w32_h__
#define INCLUDE_posix__w32_h__

#include <stdio.h>
#include <sys/param.h>

#define p_lstat(p,b) lstat(p,b)
#define p_readlink(a, b, c) readlink(a, b, c)
#define p_symlink(o,n) symlink(o, n)
#define p_link(o,n) link(o, n)
#define p_unlink(p) unlink(p)
#define p_mkdir(p,m) mkdir(p, m)
#define p_fsync(fd) fsync(fd)

/* The OpenBSD realpath function behaves differently */
#if !defined(__OpenBSD__)
# define p_realpath(p, po) realpath(p, po)
#else
char *p_realpath(const char *, char *);
#endif

#define p_vsnprintf(b, c, f, a) vsnprintf(b, c, f, a)
#define p_snprintf(b, c, f, ...) snprintf(b, c, f, __VA_ARGS__)
#define p_mkstemp(p) mkstemp(p)
#define p_inet_pton(a, b, c) inet_pton(a, b, c)

/* see win32/posix.h for explanation about why this exists */
#define p_lstat_posixly(p,b) lstat(p,b)

#endif
