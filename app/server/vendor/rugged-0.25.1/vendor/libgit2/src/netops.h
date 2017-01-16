/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_netops_h__
#define INCLUDE_netops_h__

#include "posix.h"
#include "common.h"
#include "stream.h"

#ifdef GIT_OPENSSL
# include <openssl/ssl.h>
#endif

typedef struct gitno_ssl {
#ifdef GIT_OPENSSL
	SSL *ssl;
#else
	size_t dummy;
#endif
} gitno_ssl;

/* Represents a socket that may or may not be using SSL */
typedef struct gitno_socket {
	GIT_SOCKET socket;
	gitno_ssl ssl;
} gitno_socket;

typedef struct gitno_buffer {
	char *data;
	size_t len;
	size_t offset;
	int (*recv)(struct gitno_buffer *buffer);
	void *cb_data;
} gitno_buffer;

/* Flags to gitno_connect */
enum {
	/* Attempt to create an SSL connection. */
	GITNO_CONNECT_SSL = 1,
};

/**
 * Check if the name in a cert matches the wanted hostname
 *
 * Check if a pattern from a certificate matches the hostname we
 * wanted to connect to according to RFC2818 rules (which specifies
 * HTTP over TLS). Mainly, an asterisk matches anything, but is
 * limited to a single url component.
 *
 * Note that this does not set an error message. It expects the user
 * to provide the message for the user.
 */
int gitno__match_host(const char *pattern, const char *host);

void gitno_buffer_setup_fromstream(git_stream *st, gitno_buffer *buf, char *data, size_t len);
void gitno_buffer_setup_callback(gitno_buffer *buf, char *data, size_t len, int (*recv)(gitno_buffer *buf), void *cb_data);
int gitno_recv(gitno_buffer *buf);

void gitno_consume(gitno_buffer *buf, const char *ptr);
void gitno_consume_n(gitno_buffer *buf, size_t cons);

typedef struct gitno_connection_data {
	char *host;
	char *port;
	char *path;
	char *user;
	char *pass;
	bool use_ssl;
} gitno_connection_data;

/*
 * This replaces all the pointers in `data` with freshly-allocated strings,
 * that the caller is responsible for freeing.
 * `gitno_connection_data_free_ptrs` is good for this.
 */

int gitno_connection_data_from_url(
		gitno_connection_data *data,
		const char *url,
		const char *service_suffix);

/* This frees all the pointers IN the struct, but not the struct itself. */
void gitno_connection_data_free_ptrs(gitno_connection_data *data);

int gitno_extract_url_parts(
		char **host,
		char **port,
		char **path,
		char **username,
		char **password,
		const char *url,
		const char *default_port);

#endif
