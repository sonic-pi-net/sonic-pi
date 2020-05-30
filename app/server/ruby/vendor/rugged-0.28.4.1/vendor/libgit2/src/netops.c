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
#include "buffer.h"
#include "http_parser.h"
#include "global.h"

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
void gitno_consume(gitno_buffer *buf, const char *ptr)
{
	size_t consumed;

	assert(ptr - buf->data >= 0);
	assert(ptr - buf->data <= (int) buf->len);

	consumed = ptr - buf->data;

	memmove(buf->data, ptr, buf->offset - consumed);
	memset(buf->data + buf->offset, 0x0, buf->len - buf->offset);
	buf->offset -= consumed;
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

int gitno_connection_data_handle_redirect(
		git_net_url *url,
		const char *redirect_str,
		const char *service_suffix)
{
	git_net_url tmp = GIT_NET_URL_INIT;
	int error = 0;

	assert(url && redirect_str);

	if (redirect_str[0] == '/') {
		git__free(url->path);

		if ((url->path = git__strdup(redirect_str)) == NULL) {
			error = -1;
			goto done;
		}
	} else {
		git_net_url *original = url;

		if ((error = git_net_url_parse(&tmp, redirect_str)) < 0)
			goto done;

		/* Validate that this is a legal redirection */

		if (original->scheme &&
		    strcmp(original->scheme, tmp.scheme) != 0 &&
			strcmp(tmp.scheme, "https") != 0) {
			git_error_set(GIT_ERROR_NET, "cannot redirect from '%s' to '%s'",
				original->scheme, tmp.scheme);

			error = -1;
			goto done;
		}

		if (original->host &&
		    git__strcasecmp(original->host, tmp.host) != 0) {
			git_error_set(GIT_ERROR_NET, "cannot redirect from '%s' to '%s'",
				original->host, tmp.host);

			error = -1;
			goto done;
		}

		git_net_url_swap(url, &tmp);
	}

	/* Remove the service suffix if it was given to us */
	if (service_suffix) {
		const char *service_query = strchr(service_suffix, '?');
		size_t suffix_len = service_query ?
			(size_t)(service_query - service_suffix) : strlen(service_suffix);
		size_t path_len = strlen(url->path);

		if (suffix_len && path_len >= suffix_len) {
			size_t suffix_offset = path_len - suffix_len;

			if (git__strncmp(url->path + suffix_offset, service_suffix, suffix_len) == 0 &&
			    (!service_query || git__strcmp(url->query, service_query + 1) == 0)) {
				/* Ensure we leave a minimum of '/' as the path */
				if (suffix_offset == 0)
					suffix_offset++;

				url->path[suffix_offset] = '\0';

				git__free(url->query);
				url->query = NULL;
			}
		}
	}

done:
	git_net_url_dispose(&tmp);
	return error;
}

