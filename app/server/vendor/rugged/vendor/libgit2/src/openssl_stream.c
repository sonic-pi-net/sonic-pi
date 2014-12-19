/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifdef GIT_SSL

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509v3.h>

#include <ctype.h>

#include "global.h"
#include "posix.h"
#include "stream.h"
#include "socket_stream.h"
#include "netops.h"
#include "git2/transport.h"

static int ssl_set_error(SSL *ssl, int error)
{
	int err;
	unsigned long e;

	err = SSL_get_error(ssl, error);

	assert(err != SSL_ERROR_WANT_READ);
	assert(err != SSL_ERROR_WANT_WRITE);

	switch (err) {
	case SSL_ERROR_WANT_CONNECT:
	case SSL_ERROR_WANT_ACCEPT:
		giterr_set(GITERR_NET, "SSL error: connection failure\n");
		break;
	case SSL_ERROR_WANT_X509_LOOKUP:
		giterr_set(GITERR_NET, "SSL error: x509 error\n");
		break;
	case SSL_ERROR_SYSCALL:
		e = ERR_get_error();
		if (e > 0) {
			giterr_set(GITERR_NET, "SSL error: %s",
					ERR_error_string(e, NULL));
			break;
		} else if (error < 0) {
			giterr_set(GITERR_OS, "SSL error: syscall failure");
			break;
		}
		giterr_set(GITERR_NET, "SSL error: received early EOF");
		break;
	case SSL_ERROR_SSL:
		e = ERR_get_error();
		giterr_set(GITERR_NET, "SSL error: %s",
				ERR_error_string(e, NULL));
		break;
	case SSL_ERROR_NONE:
	case SSL_ERROR_ZERO_RETURN:
	default:
		giterr_set(GITERR_NET, "SSL error: unknown error");
		break;
	}
	return -1;
}

static int ssl_teardown(SSL *ssl)
{
	int ret;

	ret = SSL_shutdown(ssl);
	if (ret < 0)
		ret = ssl_set_error(ssl, ret);
	else
		ret = 0;

	SSL_free(ssl);
	return ret;
}

static int check_host_name(const char *name, const char *host)
{
	if (!strcasecmp(name, host))
		return 0;

	if (gitno__match_host(name, host) < 0)
		return -1;

	return 0;
}

static int verify_server_cert(SSL *ssl, const char *host)
{
	X509 *cert;
	X509_NAME *peer_name;
	ASN1_STRING *str;
	unsigned char *peer_cn = NULL;
	int matched = -1, type = GEN_DNS;
	GENERAL_NAMES *alts;
	struct in6_addr addr6;
	struct in_addr addr4;
	void *addr;
	int i = -1,j;

	if (SSL_get_verify_result(ssl) != X509_V_OK) {
		giterr_set(GITERR_SSL, "The SSL certificate is invalid");
		return GIT_ECERTIFICATE;
	}

	/* Try to parse the host as an IP address to see if it is */
	if (p_inet_pton(AF_INET, host, &addr4)) {
		type = GEN_IPADD;
		addr = &addr4;
	} else {
		if(p_inet_pton(AF_INET6, host, &addr6)) {
			type = GEN_IPADD;
			addr = &addr6;
		}
	}


	cert = SSL_get_peer_certificate(ssl);
	if (!cert) {
		giterr_set(GITERR_SSL, "the server did not provide a certificate");
		return -1;
	}

	/* Check the alternative names */
	alts = X509_get_ext_d2i(cert, NID_subject_alt_name, NULL, NULL);
	if (alts) {
		int num;

		num = sk_GENERAL_NAME_num(alts);
		for (i = 0; i < num && matched != 1; i++) {
			const GENERAL_NAME *gn = sk_GENERAL_NAME_value(alts, i);
			const char *name = (char *) ASN1_STRING_data(gn->d.ia5);
			size_t namelen = (size_t) ASN1_STRING_length(gn->d.ia5);

			/* Skip any names of a type we're not looking for */
			if (gn->type != type)
				continue;

			if (type == GEN_DNS) {
				/* If it contains embedded NULs, don't even try */
				if (memchr(name, '\0', namelen))
					continue;

				if (check_host_name(name, host) < 0)
					matched = 0;
				else
					matched = 1;
			} else if (type == GEN_IPADD) {
				/* Here name isn't so much a name but a binary representation of the IP */
				matched = !!memcmp(name, addr, namelen);
			}
		}
	}
	GENERAL_NAMES_free(alts);

	if (matched == 0)
		goto cert_fail_name;

	if (matched == 1)
		return 0;

	/* If no alternative names are available, check the common name */
	peer_name = X509_get_subject_name(cert);
	if (peer_name == NULL)
		goto on_error;

	if (peer_name) {
		/* Get the index of the last CN entry */
		while ((j = X509_NAME_get_index_by_NID(peer_name, NID_commonName, i)) >= 0)
			i = j;
	}

	if (i < 0)
		goto on_error;

	str = X509_NAME_ENTRY_get_data(X509_NAME_get_entry(peer_name, i));
	if (str == NULL)
		goto on_error;

	/* Work around a bug in OpenSSL whereby ASN1_STRING_to_UTF8 fails if it's already in utf-8 */
	if (ASN1_STRING_type(str) == V_ASN1_UTF8STRING) {
		int size = ASN1_STRING_length(str);

		if (size > 0) {
			peer_cn = OPENSSL_malloc(size + 1);
			GITERR_CHECK_ALLOC(peer_cn);
			memcpy(peer_cn, ASN1_STRING_data(str), size);
			peer_cn[size] = '\0';
		}
	} else {
		int size = ASN1_STRING_to_UTF8(&peer_cn, str);
		GITERR_CHECK_ALLOC(peer_cn);
		if (memchr(peer_cn, '\0', size))
			goto cert_fail_name;
	}

	if (check_host_name((char *)peer_cn, host) < 0)
		goto cert_fail_name;

	OPENSSL_free(peer_cn);

	return 0;

on_error:
	OPENSSL_free(peer_cn);
	return ssl_set_error(ssl, 0);

cert_fail_name:
	OPENSSL_free(peer_cn);
	giterr_set(GITERR_SSL, "hostname does not match certificate");
	return GIT_ECERTIFICATE;
}

typedef struct {
	git_stream parent;
	git_socket_stream *socket;
	SSL *ssl;
	git_cert_x509 cert_info;
} openssl_stream;

int openssl_close(git_stream *stream);

int openssl_connect(git_stream *stream)
{
	int ret;
	openssl_stream *st = (openssl_stream *) stream;

	if ((ret = git_stream_connect((git_stream *)st->socket)) < 0)
		return ret;

	if ((ret = SSL_set_fd(st->ssl, st->socket->s)) <= 0) {
		openssl_close((git_stream *) st);
		return ssl_set_error(st->ssl, ret);
	}

	if ((ret = SSL_connect(st->ssl)) <= 0)
		return ssl_set_error(st->ssl, ret);

	return verify_server_cert(st->ssl, st->socket->host);
}

int openssl_certificate(git_cert **out, git_stream *stream)
{
	openssl_stream *st = (openssl_stream *) stream;
	int len;
	X509 *cert = SSL_get_peer_certificate(st->ssl);
	unsigned char *guard, *encoded_cert;

	/* Retrieve the length of the certificate first */
	len = i2d_X509(cert, NULL);
	if (len < 0) {
		giterr_set(GITERR_NET, "failed to retrieve certificate information");
		return -1;
	}

	encoded_cert = git__malloc(len);
	GITERR_CHECK_ALLOC(encoded_cert);
	/* i2d_X509 makes 'guard' point to just after the data */
	guard = encoded_cert;

	len = i2d_X509(cert, &guard);
	if (len < 0) {
		git__free(encoded_cert);
		giterr_set(GITERR_NET, "failed to retrieve certificate information");
		return -1;
	}

	st->cert_info.cert_type = GIT_CERT_X509;
	st->cert_info.data = encoded_cert;
	st->cert_info.len = len;

	*out = (git_cert *)&st->cert_info;
	return 0;
}

ssize_t openssl_write(git_stream *stream, const char *data, size_t len, int flags)
{
	openssl_stream *st = (openssl_stream *) stream;
	int ret;
	size_t off = 0;

	GIT_UNUSED(flags);

	while (off < len) {
		ret = SSL_write(st->ssl, data + off, len - off);
		if (ret <= 0 && ret != SSL_ERROR_WANT_WRITE)
			return ssl_set_error(st->ssl, ret);

		off += ret;
	}	

	return off;
}

ssize_t openssl_read(git_stream *stream, void *data, size_t len)
{
	openssl_stream *st = (openssl_stream *) stream;
	int ret;

	do {
		ret = SSL_read(st->ssl, data, len);
	} while (SSL_get_error(st->ssl, ret) == SSL_ERROR_WANT_READ);

	if (ret < 0) {
		ssl_set_error(st->ssl, ret);
		return -1;
	}

	return ret;
}

int openssl_close(git_stream *stream)
{
	openssl_stream *st = (openssl_stream *) stream;
	int ret;

	if ((ret = ssl_teardown(st->ssl)) < 0)
		return -1;

	return git_stream_close((git_stream *)st->socket);
}

void openssl_free(git_stream *stream)
{
	openssl_stream *st = (openssl_stream *) stream;

	git__free(st->cert_info.data);
	git_stream_free((git_stream *) st->socket);
	git__free(st);
}

int git_openssl_stream_new(git_stream **out, const char *host, const char *port)
{
	openssl_stream *st;

	st = git__calloc(1, sizeof(openssl_stream));
	GITERR_CHECK_ALLOC(st);

	if (git_socket_stream_new((git_stream **) &st->socket, host, port))
		return -1;

	st->ssl = SSL_new(git__ssl_ctx);
	if (st->ssl == NULL) {
		giterr_set(GITERR_SSL, "failed to create ssl object");
		return -1;
	}

	st->parent.version = GIT_STREAM_VERSION;
	st->parent.encrypted = 1;
	st->parent.connect = openssl_connect;
	st->parent.certificate = openssl_certificate;
	st->parent.read = openssl_read;
	st->parent.write = openssl_write;
	st->parent.close = openssl_close;
	st->parent.free = openssl_free;

	*out = (git_stream *) st;
	return 0;
}

#else

#include "stream.h"

int git_openssl_stream_new(git_stream **out, const char *host, const char *port)
{
	giterr_set(GITERR_SSL, "openssl is not supported in this version");
	return -1;
}

#endif
