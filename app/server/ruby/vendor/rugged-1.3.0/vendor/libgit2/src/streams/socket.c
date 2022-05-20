/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "streams/socket.h"

#include "posix.h"
#include "netops.h"
#include "registry.h"
#include "stream.h"

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
		git_error_set(GIT_ERROR_NET, "%s: %s", str, win32_error);
		git__free(win32_error);
	} else {
		git_error_set(GIT_ERROR_NET, "%s", str);
	}
}
#else
static void net_set_error(const char *str)
{
	git_error_set(GIT_ERROR_NET, "%s: %s", str, strerror(errno));
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
		git_error_set(GIT_ERROR_OS, "winsock cleanup failed");
		return -1;
	}

	return 0;
#else
	return close(s);
#endif

}

static int socket_connect(git_stream *stream)
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
		git_error_set(GIT_ERROR_OS, "winsock init failed");
		return -1;
	}

	if (LOBYTE(wsd.wVersion) != 2 || HIBYTE(wsd.wVersion) != 2) {
		WSACleanup();
		git_error_set(GIT_ERROR_OS, "winsock init failed");
		return -1;
	}
#endif

	memset(&hints, 0x0, sizeof(struct addrinfo));
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_family = AF_UNSPEC;

	if ((ret = p_getaddrinfo(st->host, st->port, &hints, &info)) != 0) {
		git_error_set(GIT_ERROR_NET,
			   "failed to resolve address for %s: %s", st->host, p_gai_strerror(ret));
		return -1;
	}

	for (p = info; p != NULL; p = p->ai_next) {
		s = socket(p->ai_family, p->ai_socktype | SOCK_CLOEXEC, p->ai_protocol);

		if (s == INVALID_SOCKET)
			continue;

		if (connect(s, p->ai_addr, (socklen_t)p->ai_addrlen) == 0)
			break;

		/* If we can't connect, try the next one */
		close_socket(s);
		s = INVALID_SOCKET;
	}

	/* Oops, we couldn't connect to any address */
	if (s == INVALID_SOCKET && p == NULL) {
		git_error_set(GIT_ERROR_OS, "failed to connect to %s", st->host);
		p_freeaddrinfo(info);
		return -1;
	}

	st->s = s;
	p_freeaddrinfo(info);
	return 0;
}

static ssize_t socket_write(git_stream *stream, const char *data, size_t len, int flags)
{
	git_socket_stream *st = (git_socket_stream *) stream;
	ssize_t written;

	errno = 0;

	if ((written = p_send(st->s, data, len, flags)) < 0) {
		net_set_error("error sending data");
		return -1;
	}

	return written;
}

static ssize_t socket_read(git_stream *stream, void *data, size_t len)
{
	ssize_t ret;
	git_socket_stream *st = (git_socket_stream *) stream;

	if ((ret = p_recv(st->s, data, len, 0)) < 0)
		net_set_error("error receiving socket data");

	return ret;
}

static int socket_close(git_stream *stream)
{
	git_socket_stream *st = (git_socket_stream *) stream;
	int error;

	error = close_socket(st->s);
	st->s = INVALID_SOCKET;

	return error;
}

static void socket_free(git_stream *stream)
{
	git_socket_stream *st = (git_socket_stream *) stream;

	git__free(st->host);
	git__free(st->port);
	git__free(st);
}

static int default_socket_stream_new(
	git_stream **out,
	const char *host,
	const char *port)
{
	git_socket_stream *st;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(host);
	GIT_ASSERT_ARG(port);

	st = git__calloc(1, sizeof(git_socket_stream));
	GIT_ERROR_CHECK_ALLOC(st);

	st->host = git__strdup(host);
	GIT_ERROR_CHECK_ALLOC(st->host);

	if (port) {
		st->port = git__strdup(port);
		GIT_ERROR_CHECK_ALLOC(st->port);
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

int git_socket_stream_new(
	git_stream **out,
	const char *host,
	const char *port)
{
	int (*init)(git_stream **, const char *, const char *) = NULL;
	git_stream_registration custom = {0};
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(host);
	GIT_ASSERT_ARG(port);

	if ((error = git_stream_registry_lookup(&custom, GIT_STREAM_STANDARD)) == 0)
		init = custom.init;
	else if (error == GIT_ENOTFOUND)
		init = default_socket_stream_new;
	else
		return error;

	if (!init) {
		git_error_set(GIT_ERROR_NET, "there is no socket stream available");
		return -1;
	}

	return init(out, host, port);
}
