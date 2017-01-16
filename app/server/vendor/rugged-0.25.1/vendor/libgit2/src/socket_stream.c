/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "posix.h"
#include "netops.h"
#include "stream.h"
#include "socket_stream.h"

#ifndef _WIN32
#	include <sys/types.h>
#	include <sys/socket.h>
#	include <sys/select.h>
#	include <sys/time.h>
#	include <netdb.h>
#	include <netinet/in.h>
#       include <arpa/inet.h>
#else
#	include <winsock2.h>
#	include <ws2tcpip.h>
#	ifdef _MSC_VER
#		pragma comment(lib, "ws2_32")
#	endif
#endif

#ifdef GIT_WIN32
static void net_set_error(const char *str)
{
	int error = WSAGetLastError();
	char * win32_error = git_win32_get_error_message(error);

	if (win32_error) {
		giterr_set(GITERR_NET, "%s: %s", str, win32_error);
		git__free(win32_error);
	} else {
		giterr_set(GITERR_NET, str);
	}
}
#else
static void net_set_error(const char *str)
{
	giterr_set(GITERR_NET, "%s: %s", str, strerror(errno));
}
#endif

static int close_socket(GIT_SOCKET s)
{
	if (s == INVALID_SOCKET)
		return 0;

#ifdef GIT_WIN32
	if (SOCKET_ERROR == closesocket(s))
		return -1;

	if (0 != WSACleanup()) {
		giterr_set(GITERR_OS, "Winsock cleanup failed");
		return -1;
	}

	return 0;
#else
	return close(s);
#endif

}

int socket_connect(git_stream *stream)
{
	struct addrinfo *info = NULL, *p;
	struct addrinfo hints;
	git_socket_stream *st = (git_socket_stream *) stream;
	GIT_SOCKET s = INVALID_SOCKET;
	int ret;

#ifdef GIT_WIN32
	/* on win32, the WSA context needs to be initialized
	 * before any socket calls can be performed */
	WSADATA wsd;

	if (WSAStartup(MAKEWORD(2,2), &wsd) != 0) {
		giterr_set(GITERR_OS, "Winsock init failed");
		return -1;
	}

	if (LOBYTE(wsd.wVersion) != 2 || HIBYTE(wsd.wVersion) != 2) {
		WSACleanup();
		giterr_set(GITERR_OS, "Winsock init failed");
		return -1;
	}
#endif

	memset(&hints, 0x0, sizeof(struct addrinfo));
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_family = AF_UNSPEC;

	if ((ret = p_getaddrinfo(st->host, st->port, &hints, &info)) != 0) {
		giterr_set(GITERR_NET,
			   "Failed to resolve address for %s: %s", st->host, p_gai_strerror(ret));
		return -1;
	}

	for (p = info; p != NULL; p = p->ai_next) {
		s = socket(p->ai_family, p->ai_socktype, p->ai_protocol);

		if (s == INVALID_SOCKET) {
			net_set_error("error creating socket");
			break;
		}

		if (connect(s, p->ai_addr, (socklen_t)p->ai_addrlen) == 0)
			break;

		/* If we can't connect, try the next one */
		close_socket(s);
		s = INVALID_SOCKET;
	}

	/* Oops, we couldn't connect to any address */
	if (s == INVALID_SOCKET && p == NULL) {
		giterr_set(GITERR_OS, "Failed to connect to %s", st->host);
		p_freeaddrinfo(info);
		return -1;
	}

	st->s = s;
	p_freeaddrinfo(info);
	return 0;
}

ssize_t socket_write(git_stream *stream, const char *data, size_t len, int flags)
{
	ssize_t ret;
	size_t off = 0;
	git_socket_stream *st = (git_socket_stream *) stream;

	while (off < len) {
		errno = 0;
		ret = p_send(st->s, data + off, len - off, flags);
		if (ret < 0) {
			net_set_error("Error sending data");
			return -1;
		}

		off += ret;
	}

	return off;
}

ssize_t socket_read(git_stream *stream, void *data, size_t len)
{
	ssize_t ret;
	git_socket_stream *st = (git_socket_stream *) stream;

	if ((ret = p_recv(st->s, data, len, 0)) < 0)
		net_set_error("Error receiving socket data");

	return ret;
}

int socket_close(git_stream *stream)
{
	git_socket_stream *st = (git_socket_stream *) stream;
	int error;

	error = close_socket(st->s);
	st->s = INVALID_SOCKET;

	return error;
}

void socket_free(git_stream *stream)
{
	git_socket_stream *st = (git_socket_stream *) stream;

	git__free(st->host);
	git__free(st->port);
	git__free(st);
}

int git_socket_stream_new(git_stream **out, const char *host, const char *port)
{
	git_socket_stream *st;

	assert(out && host);

	st = git__calloc(1, sizeof(git_socket_stream));
	GITERR_CHECK_ALLOC(st);

	st->host = git__strdup(host);
	GITERR_CHECK_ALLOC(st->host);

	if (port) {
		st->port = git__strdup(port);
		GITERR_CHECK_ALLOC(st->port);
	}

	st->parent.version = GIT_STREAM_VERSION;
	st->parent.connect = socket_connect;
	st->parent.write = socket_write;
	st->parent.read = socket_read;
	st->parent.close = socket_close;
	st->parent.free = socket_free;
	st->s = INVALID_SOCKET;

	*out = (git_stream *) st;
	return 0;
}
