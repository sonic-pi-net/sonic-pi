/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifdef GIT_CURL

#include <curl/curl.h>

#include "stream.h"
#include "git2/transport.h"
#include "buffer.h"
#include "vector.h"

typedef struct {
	git_stream parent;
	CURL *handle;
	curl_socket_t socket;
	char curl_error[CURL_ERROR_SIZE + 1];
	git_cert_x509 cert_info;
	git_strarray cert_info_strings;
} curl_stream;

static int seterr_curl(curl_stream *s)
{
	giterr_set(GITERR_NET, "curl error: %s\n", s->curl_error);
	return -1;
}

static int curls_connect(git_stream *stream)
{
	curl_stream *s = (curl_stream *) stream;
	long sockextr;
	int failed_cert = 0;
	CURLcode res;
	res = curl_easy_perform(s->handle);

	if (res != CURLE_OK && res != CURLE_PEER_FAILED_VERIFICATION)
		return seterr_curl(s);
	if (res == CURLE_PEER_FAILED_VERIFICATION)
		failed_cert = 1;

	if ((res = curl_easy_getinfo(s->handle, CURLINFO_LASTSOCKET, &sockextr)) != CURLE_OK)
		return seterr_curl(s);

	s->socket = sockextr;

	if (s->parent.encrypted && failed_cert)
		return GIT_ECERTIFICATE;

	return 0;
}

static int curls_certificate(git_cert **out, git_stream *stream)
{
	int error;
	CURLcode res;
	struct curl_slist *slist;
	struct curl_certinfo *certinfo;
	git_vector strings = GIT_VECTOR_INIT;
	curl_stream *s = (curl_stream *) stream;

	if ((res = curl_easy_getinfo(s->handle, CURLINFO_CERTINFO, &certinfo)) != CURLE_OK)
		return seterr_curl(s);

	/* No information is available, can happen with SecureTransport */
	if (certinfo->num_of_certs == 0) {
		s->cert_info.parent.cert_type = GIT_CERT_NONE;
		s->cert_info.data             = NULL;
		s->cert_info.len              = 0;
		return 0;
	}

	if ((error = git_vector_init(&strings, 8, NULL)) < 0)
		return error;

	for (slist = certinfo->certinfo[0]; slist; slist = slist->next) {
		char *str = git__strdup(slist->data);
		GITERR_CHECK_ALLOC(str);
		git_vector_insert(&strings, str);
	}

	/* Copy the contents of the vector into a strarray so we can expose them */
	s->cert_info_strings.strings = (char **) strings.contents;
	s->cert_info_strings.count   = strings.length;

	s->cert_info.parent.cert_type = GIT_CERT_STRARRAY;
	s->cert_info.data             = &s->cert_info_strings;
	s->cert_info.len              = strings.length;

	*out = &s->cert_info.parent;

	return 0;
}

static int curls_set_proxy(git_stream *stream, const char *proxy_url)
{
	CURLcode res;
	curl_stream *s = (curl_stream *) stream;

	if ((res = curl_easy_setopt(s->handle, CURLOPT_PROXY, proxy_url)) != CURLE_OK)
		return seterr_curl(s);

	return 0;
}

static int wait_for(curl_socket_t fd, bool reading)
{
	int ret;
	fd_set infd, outfd, errfd;

	FD_ZERO(&infd);
	FD_ZERO(&outfd);
	FD_ZERO(&errfd);

	FD_SET(fd, &errfd);
	if (reading)
		FD_SET(fd, &infd);
	else
		FD_SET(fd, &outfd);

	if ((ret = select(fd + 1, &infd, &outfd, &errfd, NULL)) < 0) {
		giterr_set(GITERR_OS, "error in select");
		return -1;
	}

	return 0;
}

static ssize_t curls_write(git_stream *stream, const char *data, size_t len, int flags)
{
	int error;
	size_t off = 0, sent;
	CURLcode res;
	curl_stream *s = (curl_stream *) stream;

	GIT_UNUSED(flags);

	do {
		if ((error = wait_for(s->socket, false)) < 0)
			return error;

		res = curl_easy_send(s->handle, data + off, len - off, &sent);
		if (res == CURLE_OK)
			off += sent;
	} while ((res == CURLE_OK || res == CURLE_AGAIN) && off < len);

	if (res != CURLE_OK)
		return seterr_curl(s);

	return len;
}

static ssize_t curls_read(git_stream *stream, void *data, size_t len)
{
	int error;
	size_t read;
	CURLcode res;
	curl_stream *s = (curl_stream *) stream;

	do {
		if ((error = wait_for(s->socket, true)) < 0)
			return error;

		res = curl_easy_recv(s->handle, data, len, &read);
	} while (res == CURLE_AGAIN);

	if (res != CURLE_OK)
		return seterr_curl(s);

	return read;
}

static int curls_close(git_stream *stream)
{
	curl_stream *s = (curl_stream *) stream;

	if (!s->handle)
		return 0;

	curl_easy_cleanup(s->handle);
	s->handle = NULL;
	s->socket = 0;

	return 0;
}

static void curls_free(git_stream *stream)
{
	curl_stream *s = (curl_stream *) stream;

	curls_close(stream);
	git_strarray_free(&s->cert_info_strings);
	git__free(s);
}

int git_curl_stream_new(git_stream **out, const char *host, const char *port)
{
	curl_stream *st;
	CURL *handle;
	int iport = 0, error;

	st = git__calloc(1, sizeof(curl_stream));
	GITERR_CHECK_ALLOC(st);

	handle = curl_easy_init();
	if (handle == NULL) {
		giterr_set(GITERR_NET, "failed to create curl handle");
		git__free(st);
		return -1;
	}

	if ((error = git__strtol32(&iport, port, NULL, 10)) < 0) {
		git__free(st);
		return error;
	}

	curl_easy_setopt(handle, CURLOPT_URL, host);
	curl_easy_setopt(handle, CURLOPT_ERRORBUFFER, st->curl_error);
	curl_easy_setopt(handle, CURLOPT_PORT, iport);
	curl_easy_setopt(handle, CURLOPT_CONNECT_ONLY, 1);
	curl_easy_setopt(handle, CURLOPT_SSL_VERIFYPEER, 1);
	curl_easy_setopt(handle, CURLOPT_CERTINFO, 1);
	curl_easy_setopt(handle, CURLOPT_HTTPPROXYTUNNEL, 1);
	curl_easy_setopt(handle, CURLOPT_PROXYAUTH, CURLAUTH_ANY);

	/* curl_easy_setopt(handle, CURLOPT_VERBOSE, 1); */

	st->parent.version = GIT_STREAM_VERSION;
	st->parent.encrypted = 0; /* we don't encrypt ourselves */
	st->parent.proxy_support = 1;
	st->parent.connect = curls_connect;
	st->parent.certificate = curls_certificate;
	st->parent.set_proxy = curls_set_proxy;
	st->parent.read = curls_read;
	st->parent.write = curls_write;
	st->parent.close = curls_close;
	st->parent.free = curls_free;
	st->handle = handle;

	*out = (git_stream *) st;
	return 0;
}

#else

#include "stream.h"

int git_curl_stream_new(git_stream **out, const char *host, const char *port)
{
	GIT_UNUSED(out);
	GIT_UNUSED(host);
	GIT_UNUSED(port);

	giterr_set(GITERR_NET, "curl is not supported in this version");
	return -1;
}


#endif
