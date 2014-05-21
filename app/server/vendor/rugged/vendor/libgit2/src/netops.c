/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef _WIN32
#	include <sys/types.h>
#	include <sys/socket.h>
#	include <sys/select.h>
#	include <sys/time.h>
#	include <netdb.h>
#	include <netinet/in.h>
#       include <arpa/inet.h>
#else
#	include <ws2tcpip.h>
#	ifdef _MSC_VER
#		pragma comment(lib, "ws2_32")
#	endif
#endif

#ifdef GIT_SSL
# include <openssl/ssl.h>
# include <openssl/err.h>
# include <openssl/x509v3.h>
#endif

#include <ctype.h>
#include "git2/errors.h"

#include "common.h"
#include "netops.h"
#include "posix.h"
#include "buffer.h"
#include "http_parser.h"

#ifdef GIT_WIN32
static void net_set_error(const char *str)
{
	int error = WSAGetLastError();
	char * win32_error = git_win32_get_error_message(error);

	if (win32_error) {
		giterr_set(GITERR_NET, "%s: %s", str, win32_error);
		git__free(win32_error);
	} else {
		giterr_set(GITERR_NET, str);
	}
}
#else
static void net_set_error(const char *str)
{
	giterr_set(GITERR_NET, "%s: %s", str, strerror(errno));
}
#endif

#ifdef GIT_SSL
static int ssl_set_error(gitno_ssl *ssl, int error)
{
	int err;
	unsigned long e;

	err = SSL_get_error(ssl->ssl, error);

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
#endif

int gitno_recv(gitno_buffer *buf)
{
	return buf->recv(buf);
}

#ifdef GIT_SSL
static int gitno__recv_ssl(gitno_buffer *buf)
{
	int ret;

	do {
		ret = SSL_read(buf->socket->ssl.ssl, buf->data + buf->offset, buf->len - buf->offset);
	} while (SSL_get_error(buf->socket->ssl.ssl, ret) == SSL_ERROR_WANT_READ);

	if (ret < 0) {
		net_set_error("Error receiving socket data");
		return -1;
	}

	buf->offset += ret;
	return ret;
}
#endif

static int gitno__recv(gitno_buffer *buf)
{
	int ret;

	ret = p_recv(buf->socket->socket, buf->data + buf->offset, buf->len - buf->offset, 0);
	if (ret < 0) {
		net_set_error("Error receiving socket data");
		return -1;
	}

	buf->offset += ret;
	return ret;
}

void gitno_buffer_setup_callback(
	gitno_socket *socket,
	gitno_buffer *buf,
	char *data,
	size_t len,
	int (*recv)(gitno_buffer *buf), void *cb_data)
{
	memset(data, 0x0, len);
	buf->data = data;
	buf->len = len;
	buf->offset = 0;
	buf->socket = socket;
	buf->recv = recv;
	buf->cb_data = cb_data;
}

void gitno_buffer_setup(gitno_socket *socket, gitno_buffer *buf, char *data, size_t len)
{
#ifdef GIT_SSL
	if (socket->ssl.ctx) {
		gitno_buffer_setup_callback(socket, buf, data, len, gitno__recv_ssl, NULL);
		return;
	}
#endif

	gitno_buffer_setup_callback(socket, buf, data, len, gitno__recv, NULL);
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

#ifdef GIT_SSL

static int gitno_ssl_teardown(gitno_ssl *ssl)
{
	int ret;

	ret = SSL_shutdown(ssl->ssl);
	if (ret < 0)
		ret = ssl_set_error(ssl, ret);
	else
		ret = 0;

	SSL_free(ssl->ssl);
	SSL_CTX_free(ssl->ctx);
	return ret;
}

#endif

/* Match host names according to RFC 2818 rules */
int gitno__match_host(const char *pattern, const char *host)
{
	for (;;) {
		char c = tolower(*pattern++);

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
				char h = tolower(*host);
				if (c == h)
					return gitno__match_host(pattern, host++);
				if (h == '.')
					return gitno__match_host(pattern, host);
				host++;
			}
			return -1;
		}

		if (c != tolower(*host++))
			return -1;
	}

	return -1;
}

static int check_host_name(const char *name, const char *host)
{
	if (!strcasecmp(name, host))
		return 0;

	if (gitno__match_host(name, host) < 0)
		return -1;

	return 0;
}

#ifdef GIT_SSL

static int verify_server_cert(gitno_ssl *ssl, const char *host)
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

	if (SSL_get_verify_result(ssl->ssl) != X509_V_OK) {
		giterr_set(GITERR_SSL, "The SSL certificate is invalid");
		return -1;
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


	cert = SSL_get_peer_certificate(ssl->ssl);
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
	return -1;
}

static int ssl_setup(gitno_socket *socket, const char *host, int flags)
{
	int ret;

	SSL_library_init();
	SSL_load_error_strings();
	socket->ssl.ctx = SSL_CTX_new(SSLv23_method());
	if (socket->ssl.ctx == NULL)
		return ssl_set_error(&socket->ssl, 0);

	SSL_CTX_set_mode(socket->ssl.ctx, SSL_MODE_AUTO_RETRY);
	SSL_CTX_set_verify(socket->ssl.ctx, SSL_VERIFY_NONE, NULL);
	if (!SSL_CTX_set_default_verify_paths(socket->ssl.ctx))
		return ssl_set_error(&socket->ssl, 0);

	socket->ssl.ssl = SSL_new(socket->ssl.ctx);
	if (socket->ssl.ssl == NULL)
		return ssl_set_error(&socket->ssl, 0);

	if((ret = SSL_set_fd(socket->ssl.ssl, socket->socket)) == 0)
		return ssl_set_error(&socket->ssl, ret);

	if ((ret = SSL_connect(socket->ssl.ssl)) <= 0)
		return ssl_set_error(&socket->ssl, ret);

	if (GITNO_CONNECT_SSL_NO_CHECK_CERT & flags)
		return 0;

	return verify_server_cert(&socket->ssl, host);
}
#endif

static int gitno__close(GIT_SOCKET s)
{
#ifdef GIT_WIN32
	if (SOCKET_ERROR == closesocket(s))
		return -1;

	if (0 != WSACleanup()) {
		giterr_set(GITERR_OS, "Winsock cleanup failed");
		return -1;
	}

	return 0;
#else
	return close(s);
#endif
}

int gitno_connect(gitno_socket *s_out, const char *host, const char *port, int flags)
{
	struct addrinfo *info = NULL, *p;
	struct addrinfo hints;
	GIT_SOCKET s = INVALID_SOCKET;
	int ret;

#ifdef GIT_WIN32
	/* on win32, the WSA context needs to be initialized
	 * before any socket calls can be performed */
	WSADATA wsd;

	if (WSAStartup(MAKEWORD(2,2), &wsd) != 0) {
		giterr_set(GITERR_OS, "Winsock init failed");
		return -1;
	}

	if (LOBYTE(wsd.wVersion) != 2 || HIBYTE(wsd.wVersion) != 2) {
		WSACleanup();
		giterr_set(GITERR_OS, "Winsock init failed");
		return -1;
	}
#endif

	/* Zero the socket structure provided */
	memset(s_out, 0x0, sizeof(gitno_socket));

	memset(&hints, 0x0, sizeof(struct addrinfo));
	hints.ai_socktype = SOCK_STREAM;
	hints.ai_family = AF_UNSPEC;

	if ((ret = p_getaddrinfo(host, port, &hints, &info)) < 0) {
		giterr_set(GITERR_NET,
			"Failed to resolve address for %s: %s", host, p_gai_strerror(ret));
		return -1;
	}

	for (p = info; p != NULL; p = p->ai_next) {
		s = socket(p->ai_family, p->ai_socktype, p->ai_protocol);

		if (s == INVALID_SOCKET) {
			net_set_error("error creating socket");
			break;
		}

		if (connect(s, p->ai_addr, (socklen_t)p->ai_addrlen) == 0)
			break;

		/* If we can't connect, try the next one */
		gitno__close(s);
		s = INVALID_SOCKET;
	}

	/* Oops, we couldn't connect to any address */
	if (s == INVALID_SOCKET && p == NULL) {
		giterr_set(GITERR_OS, "Failed to connect to %s", host);
		p_freeaddrinfo(info);
		return -1;
	}

	s_out->socket = s;
	p_freeaddrinfo(info);

#ifdef GIT_SSL
	if ((flags & GITNO_CONNECT_SSL) && ssl_setup(s_out, host, flags) < 0)
		return -1;
#else
	/* SSL is not supported */
	if (flags & GITNO_CONNECT_SSL) {
		giterr_set(GITERR_OS, "SSL is not supported by this copy of libgit2.");
		return -1;
	}
#endif

	return 0;
}

#ifdef GIT_SSL
static int gitno_send_ssl(gitno_ssl *ssl, const char *msg, size_t len, int flags)
{
	int ret;
	size_t off = 0;

	GIT_UNUSED(flags);

	while (off < len) {
		ret = SSL_write(ssl->ssl, msg + off, len - off);
		if (ret <= 0 && ret != SSL_ERROR_WANT_WRITE)
			return ssl_set_error(ssl, ret);

		off += ret;
	}	

	return off;
}
#endif

int gitno_send(gitno_socket *socket, const char *msg, size_t len, int flags)
{
	int ret;
	size_t off = 0;

#ifdef GIT_SSL
	if (socket->ssl.ctx)
		return gitno_send_ssl(&socket->ssl, msg, len, flags);
#endif

	while (off < len) {
		errno = 0;
		ret = p_send(socket->socket, msg + off, len - off, flags);
		if (ret < 0) {
			net_set_error("Error sending data");
			return -1;
		}

		off += ret;
	}

	return (int)off;
}

int gitno_close(gitno_socket *s)
{
#ifdef GIT_SSL
	if (s->ssl.ctx &&
		gitno_ssl_teardown(&s->ssl) < 0)
		return -1;
#endif

	return gitno__close(s->socket);
}

int gitno_select_in(gitno_buffer *buf, long int sec, long int usec)
{
	fd_set fds;
	struct timeval tv;

	tv.tv_sec = sec;
	tv.tv_usec = usec;

	FD_ZERO(&fds);
	FD_SET(buf->socket->socket, &fds);

	/* The select(2) interface is silly */
	return select((int)buf->socket->socket + 1, &fds, NULL, NULL, &tv);
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
