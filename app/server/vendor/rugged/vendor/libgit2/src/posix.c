/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"
#include "posix.h"
#include "path.h"
#include <stdio.h>
#include <ctype.h>

#ifndef GIT_WIN32

#ifdef NO_ADDRINFO

int p_getaddrinfo(
	const char *host,
	const char *port,
	struct addrinfo *hints,
	struct addrinfo **info)
{
	struct addrinfo *ainfo, *ai;
	int p = 0;

	GIT_UNUSED(hints);

	if ((ainfo = malloc(sizeof(struct addrinfo))) == NULL)
		return -1;

	if ((ainfo->ai_hostent = gethostbyname(host)) == NULL) {
		free(ainfo);
		return -2;
	}

	ainfo->ai_servent = getservbyname(port, 0);

	if (ainfo->ai_servent)
		ainfo->ai_port = ainfo->ai_servent->s_port;
	else
		ainfo->ai_port = atol(port);

	memcpy(&ainfo->ai_addr_in.sin_addr,
			ainfo->ai_hostent->h_addr_list[0],
			ainfo->ai_hostent->h_length);

	ainfo->ai_protocol = 0;
	ainfo->ai_socktype = hints->ai_socktype;
	ainfo->ai_family = ainfo->ai_hostent->h_addrtype;
	ainfo->ai_addr_in.sin_family = ainfo->ai_family;
	ainfo->ai_addr_in.sin_port = ainfo->ai_port;
	ainfo->ai_addr = (struct addrinfo *)&ainfo->ai_addr_in;
	ainfo->ai_addrlen = sizeof(struct sockaddr_in);

	*info = ainfo;

	if (ainfo->ai_hostent->h_addr_list[1] == NULL) {
		ainfo->ai_next = NULL;
		return 0;
	}

	ai = ainfo;

	for (p = 1; ainfo->ai_hostent->h_addr_list[p] != NULL; p++) {
		ai->ai_next = malloc(sizeof(struct addrinfo));
		memcpy(&ai->ai_next, ainfo, sizeof(struct addrinfo));
		memcpy(&ai->ai_next->ai_addr_in.sin_addr,
			ainfo->ai_hostent->h_addr_list[p],
			ainfo->ai_hostent->h_length);
		ai->ai_next->ai_addr = (struct addrinfo *)&ai->ai_next->ai_addr_in;
		ai = ai->ai_next;
	}

	ai->ai_next = NULL;
	return 0;
}

void p_freeaddrinfo(struct addrinfo *info)
{
	struct addrinfo *p, *next;

	p = info;

	while(p != NULL) {
		next = p->ai_next;
		free(p);
		p = next;
	}
}

const char *p_gai_strerror(int ret)
{
	switch(ret) {
	case -1: return "Out of memory"; break;
	case -2: return "Address lookup failed"; break;
	default: return "Unknown error"; break;
	}
}

#endif /* NO_ADDRINFO */

int p_open(const char *path, volatile int flags, ...)
{
	mode_t mode = 0;

	if (flags & O_CREAT) {
		va_list arg_list;

		va_start(arg_list, flags);
		mode = (mode_t)va_arg(arg_list, int);
		va_end(arg_list);
	}

	return open(path, flags | O_BINARY | O_CLOEXEC, mode);
}

int p_creat(const char *path, mode_t mode)
{
	return open(path, O_WRONLY | O_CREAT | O_TRUNC | O_BINARY | O_CLOEXEC, mode);
}

int p_getcwd(char *buffer_out, size_t size)
{
	char *cwd_buffer;

	assert(buffer_out && size > 0);

	cwd_buffer = getcwd(buffer_out, size);

	if (cwd_buffer == NULL)
		return -1;

	git_path_mkposix(buffer_out);
	git_path_string_to_dir(buffer_out, size); /* append trailing slash */

	return 0;
}

int p_rename(const char *from, const char *to)
{
	if (!link(from, to)) {
		p_unlink(from);
		return 0;
	}

	if (!rename(from, to))
		return 0;

	return -1;
}

#endif /* GIT_WIN32 */

ssize_t p_read(git_file fd, void *buf, size_t cnt)
{
	char *b = buf;

	while (cnt) {
		ssize_t r;
#ifdef GIT_WIN32
		r = read(fd, b, cnt > INT_MAX ? INT_MAX : (unsigned int)cnt);
#else
		r = read(fd, b, cnt);
#endif
		if (r < 0) {
			if (errno == EINTR || errno == EAGAIN)
				continue;
			return -1;
		}
		if (!r)
			break;
		cnt -= r;
		b += r;
	}
	return (b - (char *)buf);
}

int p_write(git_file fd, const void *buf, size_t cnt)
{
	const char *b = buf;

	while (cnt) {
		ssize_t r;
#ifdef GIT_WIN32
		assert((size_t)((unsigned int)cnt) == cnt);
		r = write(fd, b, (unsigned int)cnt);
#else
		r = write(fd, b, cnt);
#endif
		if (r < 0) {
			if (errno == EINTR || GIT_ISBLOCKED(errno))
				continue;
			return -1;
		}
		if (!r) {
			errno = EPIPE;
			return -1;
		}
		cnt -= r;
		b += r;
	}
	return 0;
}

#ifdef NO_MMAP

#include "map.h"

int git__page_size(size_t *page_size)
{
	/* dummy; here we don't need any alignment anyway */
	*page_size = 4096;
	return 0;
}


int p_mmap(git_map *out, size_t len, int prot, int flags, int fd, git_off_t offset)
{
	GIT_MMAP_VALIDATE(out, len, prot, flags);

	out->data = NULL;
	out->len = 0;

	if ((prot & GIT_PROT_WRITE) && ((flags & GIT_MAP_TYPE) == GIT_MAP_SHARED)) {
		giterr_set(GITERR_OS, "Trying to map shared-writeable");
		return -1;
	}

	out->data = malloc(len);
	GITERR_CHECK_ALLOC(out->data);

	if ((p_lseek(fd, offset, SEEK_SET) < 0) || ((size_t)p_read(fd, out->data, len) != len)) {
		giterr_set(GITERR_OS, "mmap emulation failed");
		return -1;
	}

	out->len = len;
	return 0;
}

int p_munmap(git_map *map)
{
	assert(map != NULL);
	free(map->data);

	return 0;
}

#endif
