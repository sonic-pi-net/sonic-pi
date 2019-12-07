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
#include "proxy.h"

/* This is for backwards compatibility with curl<7.45.0. */
#ifndef CURLINFO_ACTIVESOCKET
# define CURLINFO_ACTIVESOCKET CURLINFO_LASTSOCKET
# define GIT_CURL_BADSOCKET -1
# define git_activesocket_t long
#else
# define GIT_CURL_BADSOCKET CURL_SOCKET_BAD
# define git_activesocket_t curl_socket_t
#endif

typedef struct {
	git_stream parent;
	CURL *handle;
	curl_socket_t socket;
	char curl_error[CURL_ERROR_SIZE + 1];
	git_cert_x509 cert_info;
	git_strarray cert_info_strings;
	git_proxy_options proxy;
	git_cred *proxy_cred;
} curl_stream;

static int seterr_curl(curl_stream *s)
{
	giterr_set(GITERR_NET, "curl error: %s\n", s->curl_error);
	return -1;
}

GIT_INLINE(int) error_no_credentials(void)
{
	giterr_set(GITERR_NET, "proxy authentication required, but no callback provided");
	return GIT_EAUTH;
}

static int apply_proxy_creds(curl_stream *s)
{
	CURLcode res;
	git_cred_userpass_plaintext *userpass;

	if (!s->proxy_cred)
		return GIT_ENOTFOUND;

	userpass = (git_cred_userpass_plaintext *) s->proxy_cred;
	if ((res = curl_easy_setopt(s->handle, CURLOPT_PROXYUSERNAME, userpass->username)) != CURLE_OK)
		return seterr_curl(s);
	if ((res = curl_easy_setopt(s->handle, CURLOPT_PROXYPASSWORD, userpass->password)) != CURLE_OK)
		return seterr_curl(s);

	return 0;
}

static int ask_and_apply_proxy_creds(curl_stream *s)
{
	int error;
	git_proxy_options *opts = &s->proxy;

	if (!opts->credentials)
		return error_no_credentials();

	/* TODO: see if PROXYAUTH_AVAIL helps us here */
	git_cred_free(s->proxy_cred);
	s->proxy_cred = NULL;
	giterr_clear();
	error = opts->credentials(&s->proxy_cred, opts->url, NULL, GIT_CREDTYPE_USERPASS_PLAINTEXT, opts->payload);
	if (error == GIT_PASSTHROUGH)
		return error_no_credentials();
	if (error < 0) {
		if (!giterr_last())
			giterr_set(GITERR_NET, "proxy authentication was aborted by the user");
		return error;
	}

	if (s->proxy_cred->credtype != GIT_CREDTYPE_USERPASS_PLAINTEXT) {
		giterr_set(GITERR_NET, "credentials callback returned invalid credential type");
		return -1;
	}

	return apply_proxy_creds(s);
}

static int curls_connect(git_stream *stream)
{
	curl_stream *s = (curl_stream *) stream;
	git_activesocket_t sockextr;
	long connect_last = 0;
	int failed_cert = 0, error;
	bool retry_connect;
	CURLcode res;

	/* Apply any credentials we've already established */
	error = apply_proxy_creds(s);
	if (error < 0 && error != GIT_ENOTFOUND)
		return seterr_curl(s);

	do {
		retry_connect = 0;
		res = curl_easy_perform(s->handle);

		curl_easy_getinfo(s->handle, CURLINFO_HTTP_CONNECTCODE, &connect_last);

		/* HTTP 407 Proxy Authentication Required */
		if (connect_last == 407) {
			if ((error = ask_and_apply_proxy_creds(s)) < 0)
				return error;

			retry_connect = true;
		}
	} while (retry_connect);

	if (res != CURLE_OK && res != CURLE_PEER_FAILED_VERIFICATION)
		return seterr_curl(s);
	if (res == CURLE_PEER_FAILED_VERIFICATION)
		failed_cert = 1;

	if ((res = curl_easy_getinfo(s->handle, CURLINFO_ACTIVESOCKET, &sockextr)) != CURLE_OK) {
		return seterr_curl(s);
	}

	if (sockextr == GIT_CURL_BADSOCKET) {
		giterr_set(GITERR_NET, "curl socket is no longer valid");
		return -1;
	}

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

static int curls_set_proxy(git_stream *stream, const git_proxy_options *proxy_opts)
{
	int error;
	CURLcode res;
	curl_stream *s = (curl_stream *) stream;

	if ((error = git_proxy_options_dup(&s->proxy, proxy_opts)) < 0)
		return error;

	if ((res = curl_easy_setopt(s->handle, CURLOPT_PROXY, s->proxy.url)) != CURLE_OK)
		return seterr_curl(s);

	if ((res = curl_easy_setopt(s->handle, CURLOPT_PROXYAUTH, CURLAUTH_ANY)) != CURLE_OK)
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

	assert(fd >= 0);
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
