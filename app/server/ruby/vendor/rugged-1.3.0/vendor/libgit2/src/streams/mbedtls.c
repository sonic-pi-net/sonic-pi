/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "streams/mbedtls.h"

#ifdef GIT_MBEDTLS

#include <ctype.h>

#include "runtime.h"
#include "stream.h"
#include "streams/socket.h"
#include "netops.h"
#include "git2/transport.h"
#include "util.h"

#ifndef GIT_DEFAULT_CERT_LOCATION
#define GIT_DEFAULT_CERT_LOCATION NULL
#endif

/* Work around C90-conformance issues */
#if defined(_MSC_VER)
# define inline __inline
#elif defined(__GNUC__)
# define inline __inline__
#else
# define inline
#endif

#include <mbedtls/config.h>
#include <mbedtls/ssl.h>
#include <mbedtls/error.h>
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>

#undef inline

#define GIT_SSL_DEFAULT_CIPHERS "TLS-ECDHE-ECDSA-WITH-AES-128-GCM-SHA256:TLS-ECDHE-RSA-WITH-AES-128-GCM-SHA256:TLS-ECDHE-ECDSA-WITH-AES-256-GCM-SHA384:TLS-ECDHE-RSA-WITH-AES-256-GCM-SHA384:TLS-DHE-RSA-WITH-AES-128-GCM-SHA256:TLS-DHE-DSS-WITH-AES-128-GCM-SHA256:TLS-DHE-RSA-WITH-AES-256-GCM-SHA384:TLS-DHE-DSS-WITH-AES-256-GCM-SHA384:TLS-ECDHE-ECDSA-WITH-AES-128-CBC-SHA256:TLS-ECDHE-RSA-WITH-AES-128-CBC-SHA256:TLS-ECDHE-ECDSA-WITH-AES-128-CBC-SHA:TLS-ECDHE-RSA-WITH-AES-128-CBC-SHA:TLS-ECDHE-ECDSA-WITH-AES-256-CBC-SHA384:TLS-ECDHE-RSA-WITH-AES-256-CBC-SHA384:TLS-ECDHE-ECDSA-WITH-AES-256-CBC-SHA:TLS-ECDHE-RSA-WITH-AES-256-CBC-SHA:TLS-DHE-RSA-WITH-AES-128-CBC-SHA256:TLS-DHE-RSA-WITH-AES-256-CBC-SHA256:TLS-DHE-RSA-WITH-AES-128-CBC-SHA:TLS-DHE-RSA-WITH-AES-256-CBC-SHA:TLS-DHE-DSS-WITH-AES-128-CBC-SHA256:TLS-DHE-DSS-WITH-AES-256-CBC-SHA256:TLS-DHE-DSS-WITH-AES-128-CBC-SHA:TLS-DHE-DSS-WITH-AES-256-CBC-SHA:TLS-RSA-WITH-AES-128-GCM-SHA256:TLS-RSA-WITH-AES-256-GCM-SHA384:TLS-RSA-WITH-AES-128-CBC-SHA256:TLS-RSA-WITH-AES-256-CBC-SHA256:TLS-RSA-WITH-AES-128-CBC-SHA:TLS-RSA-WITH-AES-256-CBC-SHA"
#define GIT_SSL_DEFAULT_CIPHERS_COUNT 30

static mbedtls_ssl_config *git__ssl_conf;
static int ciphers_list[GIT_SSL_DEFAULT_CIPHERS_COUNT];
static mbedtls_entropy_context *mbedtls_entropy;

/**
 * This function aims to clean-up the SSL context which
 * we allocated.
 */
static void shutdown_ssl(void)
{
	if (git__ssl_conf) {
		mbedtls_x509_crt_free(git__ssl_conf->ca_chain);
		git__free(git__ssl_conf->ca_chain);
		mbedtls_ctr_drbg_free(git__ssl_conf->p_rng);
		git__free(git__ssl_conf->p_rng);
		mbedtls_ssl_config_free(git__ssl_conf);
		git__free(git__ssl_conf);
		git__ssl_conf = NULL;
	}
	if (mbedtls_entropy) {
		mbedtls_entropy_free(mbedtls_entropy);
		git__free(mbedtls_entropy);
		mbedtls_entropy = NULL;
	}
}

int git_mbedtls_stream_global_init(void)
{
	int loaded = 0;
	char *crtpath = GIT_DEFAULT_CERT_LOCATION;
	struct stat statbuf;
	mbedtls_ctr_drbg_context *ctr_drbg = NULL;

	size_t ciphers_known = 0;
	char *cipher_name = NULL;
	char *cipher_string = NULL;
	char *cipher_string_tmp = NULL;

	git__ssl_conf = git__malloc(sizeof(mbedtls_ssl_config));
	GIT_ERROR_CHECK_ALLOC(git__ssl_conf);

	mbedtls_ssl_config_init(git__ssl_conf);
	if (mbedtls_ssl_config_defaults(git__ssl_conf,
		                            MBEDTLS_SSL_IS_CLIENT,
		                            MBEDTLS_SSL_TRANSPORT_STREAM,
		                            MBEDTLS_SSL_PRESET_DEFAULT) != 0) {
		git_error_set(GIT_ERROR_SSL, "failed to initialize mbedTLS");
		goto cleanup;
	}

	/* configure TLSv1 */
	mbedtls_ssl_conf_min_version(git__ssl_conf, MBEDTLS_SSL_MAJOR_VERSION_3, MBEDTLS_SSL_MINOR_VERSION_0);

	/* verify_server_cert is responsible for making the check.
	 * OPTIONAL because REQUIRED drops the certificate as soon as the check
	 * is made, so we can never see the certificate and override it. */
	mbedtls_ssl_conf_authmode(git__ssl_conf, MBEDTLS_SSL_VERIFY_OPTIONAL);

	/* set the list of allowed ciphersuites */
	ciphers_known = 0;
	cipher_string = cipher_string_tmp = git__strdup(GIT_SSL_DEFAULT_CIPHERS);
	GIT_ERROR_CHECK_ALLOC(cipher_string);

	while ((cipher_name = git__strtok(&cipher_string_tmp, ":")) != NULL) {
		int cipherid = mbedtls_ssl_get_ciphersuite_id(cipher_name);
		if (cipherid == 0) continue;

		if (ciphers_known >= ARRAY_SIZE(ciphers_list)) {
			git_error_set(GIT_ERROR_SSL, "out of cipher list space");
			goto cleanup;
		}

		ciphers_list[ciphers_known++] = cipherid;
	}
	git__free(cipher_string);

	if (!ciphers_known) {
		git_error_set(GIT_ERROR_SSL, "no cipher could be enabled");
		goto cleanup;
	}
	mbedtls_ssl_conf_ciphersuites(git__ssl_conf, ciphers_list);

	/* Seeding the random number generator */
	mbedtls_entropy = git__malloc(sizeof(mbedtls_entropy_context));
	GIT_ERROR_CHECK_ALLOC(mbedtls_entropy);

	mbedtls_entropy_init(mbedtls_entropy);

	ctr_drbg = git__malloc(sizeof(mbedtls_ctr_drbg_context));
	GIT_ERROR_CHECK_ALLOC(ctr_drbg);

	mbedtls_ctr_drbg_init(ctr_drbg);

	if (mbedtls_ctr_drbg_seed(ctr_drbg,
		                      mbedtls_entropy_func,
		                      mbedtls_entropy, NULL, 0) != 0) {
		git_error_set(GIT_ERROR_SSL, "failed to initialize mbedTLS entropy pool");
		goto cleanup;
	}

	mbedtls_ssl_conf_rng(git__ssl_conf, mbedtls_ctr_drbg_random, ctr_drbg);

	/* load default certificates */
	if (crtpath != NULL && stat(crtpath, &statbuf) == 0 && S_ISREG(statbuf.st_mode))
		loaded = (git_mbedtls__set_cert_location(crtpath, NULL) == 0);
	if (!loaded && crtpath != NULL && stat(crtpath, &statbuf) == 0 && S_ISDIR(statbuf.st_mode))
		loaded = (git_mbedtls__set_cert_location(NULL, crtpath) == 0);

	return git_runtime_shutdown_register(shutdown_ssl);

cleanup:
	mbedtls_ctr_drbg_free(ctr_drbg);
	git__free(ctr_drbg);
	mbedtls_ssl_config_free(git__ssl_conf);
	git__free(git__ssl_conf);
	git__ssl_conf = NULL;

	return -1;
}

static int bio_read(void *b, unsigned char *buf, size_t len)
{
	git_stream *io = (git_stream *) b;
	return (int) git_stream_read(io, buf, min(len, INT_MAX));
}

static int bio_write(void *b, const unsigned char *buf, size_t len)
{
	git_stream *io = (git_stream *) b;
	return (int) git_stream_write(io, (const char *)buf, min(len, INT_MAX), 0);
}

static int ssl_set_error(mbedtls_ssl_context *ssl, int error)
{
	char errbuf[512];
	int ret = -1;

	GIT_ASSERT(error != MBEDTLS_ERR_SSL_WANT_READ);
	GIT_ASSERT(error != MBEDTLS_ERR_SSL_WANT_WRITE);

	if (error != 0)
		mbedtls_strerror( error, errbuf, 512 );

	switch(error) {
		case 0:
		git_error_set(GIT_ERROR_SSL, "SSL error: unknown error");
		break;

	case MBEDTLS_ERR_X509_CERT_VERIFY_FAILED:
		git_error_set(GIT_ERROR_SSL, "SSL error: %#04x [%x] - %s", error, ssl->session_negotiate->verify_result, errbuf);
		ret = GIT_ECERTIFICATE;
		break;

	default:
		git_error_set(GIT_ERROR_SSL, "SSL error: %#04x - %s", error, errbuf);
	}

	return ret;
}

static int ssl_teardown(mbedtls_ssl_context *ssl)
{
	int ret = 0;

	ret = mbedtls_ssl_close_notify(ssl);
	if (ret < 0)
		ret = ssl_set_error(ssl, ret);

	mbedtls_ssl_free(ssl);
	return ret;
}

static int verify_server_cert(mbedtls_ssl_context *ssl)
{
	int ret = -1;

	if ((ret = mbedtls_ssl_get_verify_result(ssl)) != 0) {
		char vrfy_buf[512];
		int len = mbedtls_x509_crt_verify_info(vrfy_buf, sizeof(vrfy_buf), "", ret);
		if (len >= 1) vrfy_buf[len - 1] = '\0'; /* Remove trailing \n */
		git_error_set(GIT_ERROR_SSL, "the SSL certificate is invalid: %#04x - %s", ret, vrfy_buf);
		return GIT_ECERTIFICATE;
	}

	return 0;
}

typedef struct {
	git_stream parent;
	git_stream *io;
	int owned;
	bool connected;
	char *host;
	mbedtls_ssl_context *ssl;
	git_cert_x509 cert_info;
} mbedtls_stream;


static int mbedtls_connect(git_stream *stream)
{
	int ret;
	mbedtls_stream *st = (mbedtls_stream *) stream;

	if (st->owned && (ret = git_stream_connect(st->io)) < 0)
		return ret;

	st->connected = true;

	mbedtls_ssl_set_hostname(st->ssl, st->host);

	mbedtls_ssl_set_bio(st->ssl, st->io, bio_write, bio_read, NULL);

	if ((ret = mbedtls_ssl_handshake(st->ssl)) != 0)
		return ssl_set_error(st->ssl, ret);

	return verify_server_cert(st->ssl);
}

static int mbedtls_certificate(git_cert **out, git_stream *stream)
{
	unsigned char *encoded_cert;
	mbedtls_stream *st = (mbedtls_stream *) stream;

	const mbedtls_x509_crt *cert = mbedtls_ssl_get_peer_cert(st->ssl);
	if (!cert) {
		git_error_set(GIT_ERROR_SSL, "the server did not provide a certificate");
		return -1;
	}

	/* Retrieve the length of the certificate first */
	if (cert->raw.len == 0) {
		git_error_set(GIT_ERROR_NET, "failed to retrieve certificate information");
		return -1;
	}

	encoded_cert = git__malloc(cert->raw.len);
	GIT_ERROR_CHECK_ALLOC(encoded_cert);
	memcpy(encoded_cert, cert->raw.p, cert->raw.len);

	st->cert_info.parent.cert_type = GIT_CERT_X509;
	st->cert_info.data = encoded_cert;
	st->cert_info.len = cert->raw.len;

	*out = &st->cert_info.parent;

	return 0;
}

static int mbedtls_set_proxy(git_stream *stream, const git_proxy_options *proxy_options)
{
	mbedtls_stream *st = (mbedtls_stream *) stream;

	return git_stream_set_proxy(st->io, proxy_options);
}

static ssize_t mbedtls_stream_write(git_stream *stream, const char *data, size_t len, int flags)
{
	mbedtls_stream *st = (mbedtls_stream *) stream;
	int written;

	GIT_UNUSED(flags);

	/*
	 * `mbedtls_ssl_write` can only represent INT_MAX bytes
	 * written via its return value. We thus need to clamp
	 * the maximum number of bytes written.
	 */
	len = min(len, INT_MAX);

	if ((written = mbedtls_ssl_write(st->ssl, (const unsigned char *)data, len)) <= 0)
		return ssl_set_error(st->ssl, written);

	return written;
}

static ssize_t mbedtls_stream_read(git_stream *stream, void *data, size_t len)
{
	mbedtls_stream *st = (mbedtls_stream *) stream;
	int ret;

	if ((ret = mbedtls_ssl_read(st->ssl, (unsigned char *)data, len)) <= 0)
		ssl_set_error(st->ssl, ret);

	return ret;
}

static int mbedtls_stream_close(git_stream *stream)
{
	mbedtls_stream *st = (mbedtls_stream *) stream;
	int ret = 0;

	if (st->connected && (ret = ssl_teardown(st->ssl)) != 0)
		return -1;

	st->connected = false;

	return st->owned ? git_stream_close(st->io) : 0;
}

static void mbedtls_stream_free(git_stream *stream)
{
	mbedtls_stream *st = (mbedtls_stream *) stream;

	if (st->owned)
		git_stream_free(st->io);

	git__free(st->host);
	git__free(st->cert_info.data);
	mbedtls_ssl_free(st->ssl);
	git__free(st->ssl);
	git__free(st);
}

static int mbedtls_stream_wrap(
	git_stream **out,
	git_stream *in,
	const char *host,
	int owned)
{
	mbedtls_stream *st;
	int error;

	st = git__calloc(1, sizeof(mbedtls_stream));
	GIT_ERROR_CHECK_ALLOC(st);

	st->io = in;
	st->owned = owned;

	st->ssl = git__malloc(sizeof(mbedtls_ssl_context));
	GIT_ERROR_CHECK_ALLOC(st->ssl);
	mbedtls_ssl_init(st->ssl);
	if (mbedtls_ssl_setup(st->ssl, git__ssl_conf)) {
		git_error_set(GIT_ERROR_SSL, "failed to create ssl object");
		error = -1;
		goto out_err;
	}

	st->host = git__strdup(host);
	GIT_ERROR_CHECK_ALLOC(st->host);

	st->parent.version = GIT_STREAM_VERSION;
	st->parent.encrypted = 1;
	st->parent.proxy_support = git_stream_supports_proxy(st->io);
	st->parent.connect = mbedtls_connect;
	st->parent.certificate = mbedtls_certificate;
	st->parent.set_proxy = mbedtls_set_proxy;
	st->parent.read = mbedtls_stream_read;
	st->parent.write = mbedtls_stream_write;
	st->parent.close = mbedtls_stream_close;
	st->parent.free = mbedtls_stream_free;

	*out = (git_stream *) st;
	return 0;

out_err:
	mbedtls_ssl_free(st->ssl);
	git_stream_close(st->io);
	git_stream_free(st->io);
	git__free(st);

	return error;
}

int git_mbedtls_stream_wrap(
	git_stream **out,
	git_stream *in,
	const char *host)
{
	return mbedtls_stream_wrap(out, in, host, 0);
}

int git_mbedtls_stream_new(
	git_stream **out,
	const char *host,
	const char *port)
{
	git_stream *stream;
	int error;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(host);
	GIT_ASSERT_ARG(port);

	if ((error = git_socket_stream_new(&stream, host, port)) < 0)
		return error;

	if ((error = mbedtls_stream_wrap(out, stream, host, 1)) < 0) {
		git_stream_close(stream);
		git_stream_free(stream);
	}

	return error;
}

int git_mbedtls__set_cert_location(const char *file, const char *path)
{
	int ret = 0;
	char errbuf[512];
	mbedtls_x509_crt *cacert;

	GIT_ASSERT_ARG(file || path);

	cacert = git__malloc(sizeof(mbedtls_x509_crt));
	GIT_ERROR_CHECK_ALLOC(cacert);

	mbedtls_x509_crt_init(cacert);
	if (file)
		ret = mbedtls_x509_crt_parse_file(cacert, file);
	if (ret >= 0 && path)
		ret = mbedtls_x509_crt_parse_path(cacert, path);
	/* mbedtls_x509_crt_parse_path returns the number of invalid certs on success */
	if (ret < 0) {
		mbedtls_x509_crt_free(cacert);
		git__free(cacert);
		mbedtls_strerror( ret, errbuf, 512 );
		git_error_set(GIT_ERROR_SSL, "failed to load CA certificates: %#04x - %s", ret, errbuf);
		return -1;
	}

	mbedtls_x509_crt_free(git__ssl_conf->ca_chain);
	git__free(git__ssl_conf->ca_chain);
	mbedtls_ssl_conf_ca_chain(git__ssl_conf, cacert, NULL);

	return 0;
}

#else

#include "stream.h"

int git_mbedtls_stream_global_init(void)
{
	return 0;
}

#endif
