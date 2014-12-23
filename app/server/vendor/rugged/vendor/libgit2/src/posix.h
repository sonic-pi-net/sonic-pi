/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_posix_h__
#define INCLUDE_posix_h__

#include "common.h"
#include <fcntl.h>
#include <time.h>
#include "fnmatch.h"

#ifndef S_IFGITLINK
#define S_IFGITLINK 0160000
#define S_ISGITLINK(m) (((m) & S_IFMT) == S_IFGITLINK)
#endif

/* if S_ISGID is not defined, then don't try to set it */
#ifndef S_ISGID
#define S_ISGID 0
#endif

#if !defined(O_BINARY)
#define O_BINARY 0
#endif
#if !defined(O_CLOEXEC)
#define O_CLOEXEC 0
#endif

/* Determine whether an errno value indicates that a read or write failed
 * because the descriptor is blocked.
 */
#if defined(EWOULDBLOCK)
#define GIT_ISBLOCKED(e) ((e) == EAGAIN || (e) == EWOULDBLOCK)
#else
#define GIT_ISBLOCKED(e) ((e) == EAGAIN)
#endif

typedef int git_file;

/**
 * Standard POSIX Methods
 *
 * All the methods starting with the `p_` prefix are
 * direct ports of the standard POSIX methods.
 *
 * Some of the methods are slightly wrapped to provide
 * saner defaults. Some of these methods are emulated
 * in Windows platforms.
 *
 * Use your manpages to check the docs on these.
 */

extern int p_read(git_file fd, void *buf, size_t cnt);
extern int p_write(git_file fd, const void *buf, size_t cnt);

#define p_fstat(f,b) fstat(f, b)
#define p_lseek(f,n,w) lseek(f, n, w)
#define p_close(fd) close(fd)
#define p_umask(m) umask(m)

extern int p_open(const char *path, int flags, ...);
extern int p_creat(const char *path, mode_t mode);
extern int p_getcwd(char *buffer_out, size_t size);
extern int p_rename(const char *from, const char *to);

#ifndef GIT_WIN32

#define p_stat(p,b) stat(p, b)
#define p_chdir(p) chdir(p)
#define p_rmdir(p) rmdir(p)
#define p_chmod(p,m) chmod(p, m)
#define p_access(p,m) access(p,m)
#define p_ftruncate(fd, sz) ftruncate(fd, sz)
#define p_recv(s,b,l,f) recv(s,b,l,f)
#define p_send(s,b,l,f) send(s,b,l,f)
typedef int GIT_SOCKET;
#define INVALID_SOCKET -1

#define p_localtime_r localtime_r
#define p_gmtime_r gmtime_r

#else

typedef SOCKET GIT_SOCKET;
extern struct tm * p_localtime_r (const time_t *timer, struct tm *result);
extern struct tm * p_gmtime_r (const time_t *timer, struct tm *result);

#endif

/**
 * Platform-dependent methods
 */
#ifdef GIT_WIN32
#	include "win32/posix.h"
#else
#	include "unix/posix.h"
#endif

#include "strnlen.h"

#ifdef NO_READDIR_R
#	include <dirent.h>
GIT_INLINE(int) p_readdir_r(DIR *dirp, struct dirent *entry, struct dirent **result)
{
	GIT_UNUSED(entry);
	*result = readdir(dirp);
	return 0;
}
#else /* NO_READDIR_R */
#	define p_readdir_r(d,e,r) readdir_r(d,e,r)
#endif

#ifdef NO_ADDRINFO
#	include <netdb.h>
struct addrinfo {
	struct hostent *ai_hostent;
	struct servent *ai_servent;
	struct sockaddr_in ai_addr_in;
	struct sockaddr *ai_addr;
	size_t ai_addrlen;
	int ai_family;
	int ai_socktype;
	int ai_protocol;
	long ai_port;
	struct addrinfo *ai_next;
};

extern int p_getaddrinfo(const char *host, const char *port,
	struct addrinfo *hints, struct addrinfo **info);
extern void p_freeaddrinfo(struct addrinfo *info);
extern const char *p_gai_strerror(int ret);
#else
#	define p_getaddrinfo(a, b, c, d) getaddrinfo(a, b, c, d)
#	define p_freeaddrinfo(a) freeaddrinfo(a)
#	define p_gai_strerror(c) gai_strerror(c)
#endif /* NO_ADDRINFO */

#endif
