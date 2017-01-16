/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_posix__unix_h__
#define INCLUDE_posix__unix_h__

#include <stdio.h>
#include <dirent.h>
#include <sys/param.h>
#include <sys/time.h>
#include <sys/stat.h>

typedef int GIT_SOCKET;
#define INVALID_SOCKET -1

#define p_lseek(f,n,w) lseek(f, n, w)
#define p_fstat(f,b) fstat(f, b)
#define p_lstat(p,b) lstat(p,b)
#define p_stat(p,b) stat(p, b)

#if defined(GIT_USE_STAT_MTIMESPEC)
# define st_atime_nsec st_atimespec.tv_nsec
# define st_mtime_nsec st_mtimespec.tv_nsec
# define st_ctime_nsec st_ctimespec.tv_nsec
#elif defined(GIT_USE_STAT_MTIM)
# define st_atime_nsec st_atim.tv_nsec
# define st_mtime_nsec st_mtim.tv_nsec
# define st_ctime_nsec st_ctim.tv_nsec
#elif !defined(GIT_USE_STAT_MTIME_NSEC) && defined(GIT_USE_NEC)
# error GIT_USE_NSEC defined but unknown struct stat nanosecond type
#endif

#define p_utimes(f, t) utimes(f, t)

#define p_readlink(a, b, c) readlink(a, b, c)
#define p_symlink(o,n) symlink(o, n)
#define p_link(o,n) link(o, n)
#define p_unlink(p) unlink(p)
#define p_mkdir(p,m) mkdir(p, m)
#define p_fsync(fd) fsync(fd)
extern char *p_realpath(const char *, char *);

#define p_recv(s,b,l,f) recv(s,b,l,f)
#define p_send(s,b,l,f) send(s,b,l,f)
#define p_inet_pton(a, b, c) inet_pton(a, b, c)

#define p_strcasecmp(s1, s2) strcasecmp(s1, s2)
#define p_strncasecmp(s1, s2, c) strncasecmp(s1, s2, c)
#define p_vsnprintf(b, c, f, a) vsnprintf(b, c, f, a)
#define p_snprintf(b, c, f, ...) snprintf(b, c, f, __VA_ARGS__)
#define p_mkstemp(p) mkstemp(p)
#define p_chdir(p) chdir(p)
#define p_chmod(p,m) chmod(p, m)
#define p_rmdir(p) rmdir(p)
#define p_access(p,m) access(p,m)
#define p_ftruncate(fd, sz) ftruncate(fd, sz)

/* see win32/posix.h for explanation about why this exists */
#define p_lstat_posixly(p,b) lstat(p,b)

#define p_localtime_r(c, r) localtime_r(c, r)
#define p_gmtime_r(c, r) gmtime_r(c, r)

#define p_timeval timeval

#ifdef HAVE_FUTIMENS
GIT_INLINE(int) p_futimes(int f, const struct p_timeval t[2])
{
	struct timespec s[2];
	s[0].tv_sec = t[0].tv_sec;
	s[0].tv_nsec = t[0].tv_usec * 1000;
	s[1].tv_sec = t[1].tv_sec;
	s[1].tv_nsec = t[1].tv_usec * 1000;
	return futimens(f, s);
}
#else
# define p_futimes futimes
#endif

#ifdef HAVE_REGCOMP_L
#include <xlocale.h>
GIT_INLINE(int) p_regcomp(regex_t *preg, const char *pattern, int cflags)
{
	return regcomp_l(preg, pattern, cflags, (locale_t) 0);
}
#else
# define p_regcomp regcomp
#endif

#endif
