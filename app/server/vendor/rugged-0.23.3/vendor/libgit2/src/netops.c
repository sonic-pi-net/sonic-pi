/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <ctype.h>
#include "git2/errors.h"

#include "common.h"
#include "netops.h"
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
	int ret;

	ret = git_stream_read(io, buf->data + buf->offset, buf->len - buf->offset);
	if (ret < 0)
		return -1;

	buf->offset += ret;
	return ret;
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

static const char *prefix_http = "http://";
static const char *prefix_https = "https://";

int gitno_connection_data_from_url(
		gitno_connection_data *data,
		const char *url,
		const char *service_suffix)
{
	int error = -1;
	const char *default_port = NULL, *path_search_start = NULL;
	char *original_host = NULL;

	/* service_suffix is optional */
	assert(data && url);

	/* Save these for comparison later */
	original_host = data->host;
	data->host = NULL;
	gitno_connection_data_free_ptrs(data);

	if (!git__prefixcmp(url, prefix_http)) {
		path_search_start = url + strlen(prefix_http);
		default_port = "80";

		if (data->use_ssl) {
			giterr_set(GITERR_NET, "Redirect from HTTPS to HTTP is not allowed");
			goto cleanup;
		}
	} else if (!git__prefixcmp(url, prefix_https)) {
		path_search_start = url + strlen(prefix_https);
		default_port = "443";
		data->use_ssl = true;
	} else if (url[0] == '/')
		default_port = data->use_ssl ? "443" : "80";

	if (!default_port) {
		giterr_set(GITERR_NET, "Unrecognized URL prefix");
		goto cleanup;
	}

	error = gitno_extract_url_parts(
		&data->host, &data->port, &data->path, &data->user, &data->pass,
		url, default_port);

	if (url[0] == '/') {
		/* Relative redirect; reuse original host name and port */
		path_search_start = url;
		git__free(data->host);
		data->host = original_host;
		original_host = NULL;
	}

	if (!error) {
		const char *path = strchr(path_search_start, '/');
		size_t pathlen = strlen(path);
		size_t suffixlen = service_suffix ? strlen(service_suffix) : 0;

		if (suffixlen &&
		    !memcmp(path + pathlen - suffixlen, service_suffix, suffixlen)) {
			git__free(data->path);
			data->path = git__strndup(path, pathlen - suffixlen);
		} else {
			git__free(data->path);
			data->path = git__strdup(path);
		}

		/* Check for errors in the resulting data */
		if (original_host && url[0] != '/' && strcmp(original_host, data->host)) {
			giterr_set(GITERR_NET, "Cross host redirect not allowed");
			error = -1;
		}
	}

cleanup:
	if (original_host) git__free(original_host);
	return error;
}

void gitno_connection_data_free_ptrs(gitno_connection_data *d)
{
	git__free(d->host); d->host = NULL;
	git__free(d->port); d->port = NULL;
	git__free(d->path); d->path = NULL;
	git__free(d->user); d->user = NULL;
	git__free(d->pass); d->pass = NULL;
}

#define hex2c(c) ((c | 32) % 39 - 9)
static char* unescape(char *str)
{
	int x, y;
	int len = (int)strlen(str);

	for (x=y=0; str[y]; ++x, ++y) {
		if ((str[x] = str[y]) == '%') {
			if (y < len-2 && isxdigit(str[y+1]) && isxdigit(str[y+2])) {
				str[x] = (hex2c(str[y+1]) << 4) + hex2c(str[y+2]);
				y += 2;
			}
		}
	}
	str[x] = '\0';
	return str;
}

int gitno_extract_url_parts(
		char **host,
		char **port,
		char **path,
		char **username,
		char **password,
		const char *url,
		const char *default_port)
{
	struct http_parser_url u = {0};
	const char *_host, *_port, *_path, *_userinfo;

	if (http_parser_parse_url(url, strlen(url), false, &u)) {
		giterr_set(GITERR_NET, "Malformed URL '%s'", url);
		return GIT_EINVALIDSPEC;
	}

	_host = url+u.field_data[UF_HOST].off;
	_port = url+u.field_data[UF_PORT].off;
	_path = url+u.field_data[UF_PATH].off;
	_userinfo = url+u.field_data[UF_USERINFO].off;

	if (u.field_set & (1 << UF_HOST)) {
		*host = git__substrdup(_host, u.field_data[UF_HOST].len);
		GITERR_CHECK_ALLOC(*host);
	}

	if (u.field_set & (1 << UF_PORT))
		*port = git__substrdup(_port, u.field_data[UF_PORT].len);
	else
		*port = git__strdup(default_port);
	GITERR_CHECK_ALLOC(*port);

	if (u.field_set & (1 << UF_PATH)) {
		*path = git__substrdup(_path, u.field_data[UF_PATH].len);
		GITERR_CHECK_ALLOC(*path);
	} else {
		giterr_set(GITERR_NET, "invalid url, missing path");
		return GIT_EINVALIDSPEC;
	}

	if (u.field_set & (1 << UF_USERINFO)) {
		const char *colon = memchr(_userinfo, ':', u.field_data[UF_USERINFO].len);
		if (colon) {
			*username = unescape(git__substrdup(_userinfo, colon - _userinfo));
			*password = unescape(git__substrdup(colon+1, u.field_data[UF_USERINFO].len - (colon+1-_userinfo)));
			GITERR_CHECK_ALLOC(*password);
		} else {
			*username = git__substrdup(_userinfo, u.field_data[UF_USERINFO].len);
		}
		GITERR_CHECK_ALLOC(*username);

	}

	return 0;
}
