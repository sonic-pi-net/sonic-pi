/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "netops.h"

#include <ctype.h>
#include "git2/errors.h"

#include "posix.h"
#include "str.h"
#include "runtime.h"

int gitno_recv(gitno_buffer *buf)
{
	return buf->recv(buf);
}

void gitno_buffer_setup_callback(
	gitno_buffer *buf,
	char *data,
	size_t len,
	int (*recv)(gitno_buffer *buf), void *cb_data)
{
	memset(data, 0x0, len);
	buf->data = data;
	buf->len = len;
	buf->offset = 0;
	buf->recv = recv;
	buf->cb_data = cb_data;
}

static int recv_stream(gitno_buffer *buf)
{
	git_stream *io = (git_stream *) buf->cb_data;
	size_t readlen = buf->len - buf->offset;
	ssize_t ret;

	readlen = min(readlen, INT_MAX);

	ret = git_stream_read(io, buf->data + buf->offset, (int)readlen);
	if (ret < 0)
		return -1;

	buf->offset += ret;
	return (int)ret;
}

void gitno_buffer_setup_fromstream(git_stream *st, gitno_buffer *buf, char *data, size_t len)
{
	memset(data, 0x0, len);
	buf->data = data;
	buf->len = len;
	buf->offset = 0;
	buf->recv = recv_stream;
	buf->cb_data = st;
}

/* Consume up to ptr and move the rest of the buffer to the beginning */
int gitno_consume(gitno_buffer *buf, const char *ptr)
{
	size_t consumed;

	GIT_ASSERT(ptr - buf->data >= 0);
	GIT_ASSERT(ptr - buf->data <= (int) buf->len);

	consumed = ptr - buf->data;

	memmove(buf->data, ptr, buf->offset - consumed);
	memset(buf->data + buf->offset, 0x0, buf->len - buf->offset);
	buf->offset -= consumed;

	return 0;
}

/* Consume const bytes and move the rest of the buffer to the beginning */
void gitno_consume_n(gitno_buffer *buf, size_t cons)
{
	memmove(buf->data, buf->data + cons, buf->len - buf->offset);
	memset(buf->data + cons, 0x0, buf->len - buf->offset);
	buf->offset -= cons;
}

/* Match host names according to RFC 2818 rules */
int gitno__match_host(const char *pattern, const char *host)
{
	for (;;) {
		char c = git__tolower(*pattern++);

		if (c == '\0')
			return *host ? -1 : 0;

		if (c == '*') {
			c = *pattern;
			/* '*' at the end matches everything left */
			if (c == '\0')
				return 0;

	/*
	 * We've found a pattern, so move towards the next matching
	 * char. The '.' is handled specially because wildcards aren't
	 * allowed to cross subdomains.
	 */

			while(*host) {
				char h = git__tolower(*host);
				if (c == h)
					return gitno__match_host(pattern, host++);
				if (h == '.')
					return gitno__match_host(pattern, host);
				host++;
			}
			return -1;
		}

		if (c != git__tolower(*host++))
			return -1;
	}

	return -1;
}
