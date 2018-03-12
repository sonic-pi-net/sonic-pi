/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "openssl_stream.h"

#ifdef GIT_OPENSSL

#include <ctype.h>

#include "global.h"
#include "posix.h"
#include "stream.h"
#include "socket_stream.h"
#include "netops.h"
#include "git2/transport.h"
#include "git2/sys/openssl.h"

#ifdef GIT_CURL
# include "curl_stream.h"
#endif

#ifndef GIT_WIN32
# include <sys/types.h>
# include <sys/socket.h>
# include <netinet/in.h>
#endif

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509v3.h>
#include <openssl/bio.h>

SSL_CTX *git__ssl_ctx;

#define GIT_SSL_DEFAULT_CIPHERS "ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384:DHE-DSS-AES256-GCM-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA:ECDHE-RSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-RSA-AES128-SHA:DHE-RSA-AES256-SHA:DHE-DSS-AES128-SHA256:DHE-DSS-AES256-SHA256:DHE-DSS-AES128-SHA:DHE-DSS-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA"

#if defined(GIT_THREADS) && OPENSSL_VERSION_NUMBER < 0x10100000L

static git_mutex *openssl_locks;

static void openssl_locking_function(
	int mode, int n, const char *file, int line)
{
	int lock;

	GIT_UNUSED(file);
	GIT_UNUSED(line);

	lock = mode & CRYPTO_LOCK;

	if (lock) {
		git_mutex_lock(&openssl_locks[n]);
	} else {
		git_mutex_unlock(&openssl_locks[n]);
	}
}

static void shutdown_ssl_locking(void)
{
	int num_locks, i;

	num_locks = CRYPTO_num_locks();
	CRYPTO_set_locking_callback(NULL);

	for (i = 0; i < num_locks; ++i)
		git_mutex_free(&openssl_locks[i]);
	git__free(openssl_locks);
}

#endif /* GIT_THREADS && OPENSSL_VERSION_NUMBER < 0x10100000L */

static BIO_METHOD *git_stream_bio_method;
static int init_bio_method(void);

/**
 * This function aims to clean-up the SSL context which
 * we allocated.
 */
static void shutdown_ssl(void)
{
	if (git_stream_bio_method) {
		BIO_meth_free(git_stream_bio_method);
		git_stream_bio_method = NULL;
	}

	if (git__ssl_ctx) {
		SSL_CTX_free(git__ssl_ctx);
		git__ssl_ctx = NULL;
	}
}

int git_openssl_stream_global_init(void)
{
#ifdef GIT_OPENSSL
	long ssl_opts = SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3;
	const char *ciphers = git_libgit2__ssl_ciphers();

	/* Older OpenSSL and MacOS OpenSSL doesn't have this */
#ifdef SSL_OP_NO_COMPRESSION
	ssl_opts |= SSL_OP_NO_COMPRESSION;
#endif

#if OPENSSL_VERSION_NUMBER < 0x10100000L || defined(LIBRESSL_VERSION_NUMBER)
	SSL_load_error_strings();
	OpenSSL_add_ssl_algorithms();
#else
	OPENSSL_init_ssl(0, NULL);
#endif

	/*
	 * Load SSLv{2,3} and TLSv1 so that we can talk with servers
	 * which use the SSL hellos, which are often used for
	 * compatibility. We then disable SSL so we only allow OpenSSL
	 * to speak TLSv1 to perform the encryption itself.
	 */
	git__ssl_ctx = SSL_CTX_new(SSLv23_method());
	SSL_CTX_set_options(git__ssl_ctx, ssl_opts);
	SSL_CTX_set_mode(git__ssl_ctx, SSL_MODE_AUTO_RETRY);
	SSL_CTX_set_verify(git__ssl_ctx, SSL_VERIFY_NONE, NULL);
	if (!SSL_CTX_set_default_verify_paths(git__ssl_ctx)) {
		SSL_CTX_free(git__ssl_ctx);
		git__ssl_ctx = NULL;
		return -1;
	}

	if (!ciphers) {
		ciphers = GIT_SSL_DEFAULT_CIPHERS;
	}

	if(!SSL_CTX_set_cipher_list(git__ssl_ctx, ciphers)) {
		SSL_CTX_free(git__ssl_ctx);
		git__ssl_ctx = NULL;
		return -1;
	}

	if (init_bio_method() < 0) {
		SSL_CTX_free(git__ssl_ctx);
		git__ssl_ctx = NULL;
		return -1;
	}

#endif

	git__on_shutdown(shutdown_ssl);

	return 0;
}

int git_openssl_set_locking(void)
{
#if defined(GIT_THREADS) && OPENSSL_VERSION_NUMBER < 0x10100000L
	int num_locks, i;

	num_locks = CRYPTO_num_locks();
	openssl_locks = git__calloc(num_locks, sizeof(git_mutex));
	GITERR_CHECK_ALLOC(openssl_locks);

	for (i = 0; i < num_locks; i++) {
		if (git_mutex_init(&openssl_locks[i]) != 0) {
			giterr_set(GITERR_SSL, "failed to initialize openssl locks");
			return -1;
		}
	}

	CRYPTO_set_locking_callback(openssl_locking_function);
	git__on_shutdown(shutdown_ssl_locking);
	return 0;
#elif OPENSSL_VERSION_NUMBER >= 0x10100000L
	return 0;
#else
	giterr_set(GITERR_THREAD, "libgit2 was not built with threads");
	return -1;
#endif
}


static int bio_create(BIO *b)
{
	BIO_set_init(b, 1);
	BIO_set_data(b, NULL);

	return 1;
}

static int bio_destroy(BIO *b)
{
	if (!b)
		return 0;

	BIO_set_data(b, NULL);

	return 1;
}

static int bio_read(BIO *b, char *buf, int len)
{
	git_stream *io = (git_stream *) BIO_get_data(b);

	return (int) git_stream_read(io, buf, len);
}

static int bio_write(BIO *b, const char *buf, int len)
{
	git_stream *io = (git_stream *) BIO_get_data(b);

	return (int) git_stream_write(io, buf, len, 0);
}

static long bio_ctrl(BIO *b, int cmd, long num, void *ptr)
{
	GIT_UNUSED(b);
	GIT_UNUSED(num);
	GIT_UNUSED(ptr);

	if (cmd == BIO_CTRL_FLUSH)
		return 1;

	return 0;
}

static int bio_gets(BIO *b, char *buf, int len)
{
	GIT_UNUSED(b);
	GIT_UNUSED(buf);
	GIT_UNUSED(len);
	return -1;
}

static int bio_puts(BIO *b, const char *str)
{
	return bio_write(b, str, strlen(str));
}

static int init_bio_method(void)
{
	/* Set up the BIO_METHOD we use for wrapping our own stream implementations */
	git_stream_bio_method = BIO_meth_new(BIO_TYPE_SOURCE_SINK | BIO_get_new_index(), "git_stream");
	GITERR_CHECK_ALLOC(git_stream_bio_method);

	BIO_meth_set_write(git_stream_bio_method, bio_write);
	BIO_meth_set_read(git_stream_bio_method, bio_read);
	BIO_meth_set_puts(git_stream_bio_method, bio_puts);
	BIO_meth_set_gets(git_stream_bio_method, bio_gets);
	BIO_meth_set_ctrl(git_stream_bio_method, bio_ctrl);
	BIO_meth_set_create(git_stream_bio_method, bio_create);
	BIO_meth_set_destroy(git_stream_bio_method, bio_destroy);

	return 0;
}

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
		giterr_set(GITERR_NET, "SSL error: connection failure");
		break;
	case SSL_ERROR_WANT_X509_LOOKUP:
		giterr_set(GITERR_NET, "SSL error: x509 error");
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
		return GIT_EEOF;
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
		giterr_set(GITERR_SSL, "the SSL certificate is invalid");
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
			const char *name = (char *) ASN1_STRING_get0_data(gn->d.ia5);
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
			memcpy(peer_cn, ASN1_STRING_get0_data(str), size);
			peer_cn[size] = '\0';
		} else {
			goto cert_fail_name;
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
	git_stream *io;
	bool connected;
	char *host;
	SSL *ssl;
	git_cert_x509 cert_info;
} openssl_stream;

int openssl_close(git_stream *stream);

int openssl_connect(git_stream *stream)
{
	int ret;
	BIO *bio;
	openssl_stream *st = (openssl_stream *) stream;

	if ((ret = git_stream_connect(st->io)) < 0)
		return ret;

	st->connected = true;

	bio = BIO_new(git_stream_bio_method);
	GITERR_CHECK_ALLOC(bio);

	BIO_set_data(bio, st->io);
	SSL_set_bio(st->ssl, bio, bio);

	/* specify the host in case SNI is needed */
#ifdef SSL_CTRL_SET_TLSEXT_HOSTNAME
	SSL_set_tlsext_host_name(st->ssl, st->host);
#endif

	if ((ret = SSL_connect(st->ssl)) <= 0)
		return ssl_set_error(st->ssl, ret);

	return verify_server_cert(st->ssl, st->host);
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

	st->cert_info.parent.cert_type = GIT_CERT_X509;
	st->cert_info.data = encoded_cert;
	st->cert_info.len = len;

	*out = &st->cert_info.parent;

	return 0;
}

static int openssl_set_proxy(git_stream *stream, const git_proxy_options *proxy_opts)
{
	openssl_stream *st = (openssl_stream *) stream;

	return git_stream_set_proxy(st->io, proxy_opts);
}

ssize_t openssl_write(git_stream *stream, const char *data, size_t len, int flags)
{
	openssl_stream *st = (openssl_stream *) stream;
	int ret;

	GIT_UNUSED(flags);

	if ((ret = SSL_write(st->ssl, data, len)) <= 0) {
		return ssl_set_error(st->ssl, ret);
	}

	return ret;
}

ssize_t openssl_read(git_stream *stream, void *data, size_t len)
{
	openssl_stream *st = (openssl_stream *) stream;
	int ret;

	if ((ret = SSL_read(st->ssl, data, len)) <= 0)
		return ssl_set_error(st->ssl, ret);

	return ret;
}

int openssl_close(git_stream *stream)
{
	openssl_stream *st = (openssl_stream *) stream;
	int ret;

	if (st->connected && (ret = ssl_teardown(st->ssl)) < 0)
		return -1;

	st->connected = false;

	return git_stream_close(st->io);
}

void openssl_free(git_stream *stream)
{
	openssl_stream *st = (openssl_stream *) stream;

	SSL_free(st->ssl);
	git__free(st->host);
	git__free(st->cert_info.data);
	git_stream_free(st->io);
	git__free(st);
}

int git_openssl_stream_new(git_stream **out, const char *host, const char *port)
{
	int error;
	openssl_stream *st;

	st = git__calloc(1, sizeof(openssl_stream));
	GITERR_CHECK_ALLOC(st);

	st->io = NULL;
#ifdef GIT_CURL
	error = git_curl_stream_new(&st->io, host, port);
#else
	error = git_socket_stream_new(&st->io, host, port);
#endif

	if (error < 0)
		goto out_err;

	st->ssl = SSL_new(git__ssl_ctx);
	if (st->ssl == NULL) {
		giterr_set(GITERR_SSL, "failed to create ssl object");
		error = -1;
		goto out_err;
	}

	st->host = git__strdup(host);
	GITERR_CHECK_ALLOC(st->host);

	st->parent.version = GIT_STREAM_VERSION;
	st->parent.encrypted = 1;
	st->parent.proxy_support = git_stream_supports_proxy(st->io);
	st->parent.connect = openssl_connect;
	st->parent.certificate = openssl_certificate;
	st->parent.set_proxy = openssl_set_proxy;
	st->parent.read = openssl_read;
	st->parent.write = openssl_write;
	st->parent.close = openssl_close;
	st->parent.free = openssl_free;

	*out = (git_stream *) st;
	return 0;

out_err:
	git_stream_free(st->io);
	git__free(st);

	return error;
}

#else

#include "stream.h"
#include "git2/sys/openssl.h"

int git_openssl_stream_global_init(void)
{
	return 0;
}

int git_openssl_set_locking(void)
{
	giterr_set(GITERR_SSL, "libgit2 was not built with OpenSSL support");
	return -1;
}

int git_openssl_stream_new(git_stream **out, const char *host, const char *port)
{
	GIT_UNUSED(out);
	GIT_UNUSED(host);
	GIT_UNUSED(port);

	giterr_set(GITERR_SSL, "openssl is not supported in this version");
	return -1;
}

#endif
