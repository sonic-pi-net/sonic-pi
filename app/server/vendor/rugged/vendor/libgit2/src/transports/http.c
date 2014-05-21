/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef GIT_WINHTTP

#include "git2.h"
#include "http_parser.h"
#include "buffer.h"
#include "netops.h"
#include "smart.h"

static const char *upload_pack_service = "upload-pack";
static const char *upload_pack_ls_service_url = "/info/refs?service=git-upload-pack";
static const char *upload_pack_service_url = "/git-upload-pack";
static const char *receive_pack_service = "receive-pack";
static const char *receive_pack_ls_service_url = "/info/refs?service=git-receive-pack";
static const char *receive_pack_service_url = "/git-receive-pack";
static const char *get_verb = "GET";
static const char *post_verb = "POST";
static const char *basic_authtype = "Basic";

#define OWNING_SUBTRANSPORT(s) ((http_subtransport *)(s)->parent.subtransport)

#define PARSE_ERROR_GENERIC	-1
#define PARSE_ERROR_REPLAY	-2

#define CHUNK_SIZE	4096

enum last_cb {
	NONE,
	FIELD,
	VALUE
};

typedef enum {
	GIT_HTTP_AUTH_BASIC = 1,
} http_authmechanism_t;

typedef struct {
	git_smart_subtransport_stream parent;
	const char *service;
	const char *service_url;
	char *redirect_url;
	const char *verb;
	char *chunk_buffer;
	unsigned chunk_buffer_len;
	unsigned sent_request : 1,
		received_response : 1,
		chunked : 1,
		redirect_count : 3;
} http_stream;

typedef struct {
	git_smart_subtransport parent;
	transport_smart *owner;
	gitno_socket socket;
	gitno_connection_data connection_data;
	git_cred *cred;
	git_cred *url_cred;
	http_authmechanism_t auth_mechanism;
	bool connected;

	/* Parser structures */
	http_parser parser;
	http_parser_settings settings;
	gitno_buffer parse_buffer;
	git_buf parse_header_name;
	git_buf parse_header_value;
	char parse_buffer_data[2048];
	char *content_type;
	char *location;
	git_vector www_authenticate;
	enum last_cb last_cb;
	int parse_error;
	unsigned parse_finished : 1;
} http_subtransport;

typedef struct {
	http_stream *s;
	http_subtransport *t;

	/* Target buffer details from read() */
	char *buffer;
	size_t buf_size;
	size_t *bytes_read;
} parser_context;

static int apply_basic_credential(git_buf *buf, git_cred *cred)
{
	git_cred_userpass_plaintext *c = (git_cred_userpass_plaintext *)cred;
	git_buf raw = GIT_BUF_INIT;
	int error = -1;

	git_buf_printf(&raw, "%s:%s", c->username, c->password);

	if (git_buf_oom(&raw) ||
		git_buf_puts(buf, "Authorization: Basic ") < 0 ||
		git_buf_put_base64(buf, git_buf_cstr(&raw), raw.size) < 0 ||
		git_buf_puts(buf, "\r\n") < 0)
		goto on_error;

	error = 0;

on_error:
	if (raw.size)
		memset(raw.ptr, 0x0, raw.size);

	git_buf_free(&raw);
	return error;
}

static int gen_request(
	git_buf *buf,
	http_stream *s,
	size_t content_length)
{
	http_subtransport *t = OWNING_SUBTRANSPORT(s);
	const char *path = t->connection_data.path ? t->connection_data.path : "/";

	git_buf_printf(buf, "%s %s%s HTTP/1.1\r\n", s->verb, path, s->service_url);

	git_buf_puts(buf, "User-Agent: git/1.0 (libgit2 " LIBGIT2_VERSION ")\r\n");
	git_buf_printf(buf, "Host: %s\r\n", t->connection_data.host);

	if (s->chunked || content_length > 0) {
		git_buf_printf(buf, "Accept: application/x-git-%s-result\r\n", s->service);
		git_buf_printf(buf, "Content-Type: application/x-git-%s-request\r\n", s->service);

		if (s->chunked)
			git_buf_puts(buf, "Transfer-Encoding: chunked\r\n");
		else
			git_buf_printf(buf, "Content-Length: %"PRIuZ "\r\n", content_length);
	} else
		git_buf_puts(buf, "Accept: */*\r\n");

	/* Apply credentials to the request */
	if (t->cred && t->cred->credtype == GIT_CREDTYPE_USERPASS_PLAINTEXT &&
		t->auth_mechanism == GIT_HTTP_AUTH_BASIC &&
		apply_basic_credential(buf, t->cred) < 0)
		return -1;

	/* Use url-parsed basic auth if username and password are both provided */
	if (!t->cred && t->connection_data.user && t->connection_data.pass) {
		if (!t->url_cred && git_cred_userpass_plaintext_new(&t->url_cred,
					t->connection_data.user, t->connection_data.pass) < 0)
			return -1;
		if (apply_basic_credential(buf, t->url_cred) < 0) return -1;
	}

	git_buf_puts(buf, "\r\n");

	if (git_buf_oom(buf))
		return -1;

	return 0;
}

static int parse_unauthorized_response(
	git_vector *www_authenticate,
	int *allowed_types,
	http_authmechanism_t *auth_mechanism)
{
	unsigned i;
	char *entry;

	git_vector_foreach(www_authenticate, i, entry) {
		if (!strncmp(entry, basic_authtype, 5) &&
			(entry[5] == '\0' || entry[5] == ' ')) {
			*allowed_types |= GIT_CREDTYPE_USERPASS_PLAINTEXT;
			*auth_mechanism = GIT_HTTP_AUTH_BASIC;
		}
	}

	return 0;
}

static int on_header_ready(http_subtransport *t)
{
	git_buf *name = &t->parse_header_name;
	git_buf *value = &t->parse_header_value;

	if (!strcasecmp("Content-Type", git_buf_cstr(name))) {
		if (!t->content_type) {
			t->content_type = git__strdup(git_buf_cstr(value));
			GITERR_CHECK_ALLOC(t->content_type);
		}
	}
	else if (!strcmp("WWW-Authenticate", git_buf_cstr(name))) {
		char *dup = git__strdup(git_buf_cstr(value));
		GITERR_CHECK_ALLOC(dup);

		git_vector_insert(&t->www_authenticate, dup);
	}
	else if (!strcasecmp("Location", git_buf_cstr(name))) {
		if (!t->location) {
			t->location = git__strdup(git_buf_cstr(value));
			GITERR_CHECK_ALLOC(t->location);
		}
	}

	return 0;
}

static int on_header_field(http_parser *parser, const char *str, size_t len)
{
	parser_context *ctx = (parser_context *) parser->data;
	http_subtransport *t = ctx->t;

	/* Both parse_header_name and parse_header_value are populated
	 * and ready for consumption */
	if (VALUE == t->last_cb)
		if (on_header_ready(t) < 0)
			return t->parse_error = PARSE_ERROR_GENERIC;

	if (NONE == t->last_cb || VALUE == t->last_cb)
		git_buf_clear(&t->parse_header_name);

	if (git_buf_put(&t->parse_header_name, str, len) < 0)
		return t->parse_error = PARSE_ERROR_GENERIC;

	t->last_cb = FIELD;
	return 0;
}

static int on_header_value(http_parser *parser, const char *str, size_t len)
{
	parser_context *ctx = (parser_context *) parser->data;
	http_subtransport *t = ctx->t;

	assert(NONE != t->last_cb);

	if (FIELD == t->last_cb)
		git_buf_clear(&t->parse_header_value);

	if (git_buf_put(&t->parse_header_value, str, len) < 0)
		return t->parse_error = PARSE_ERROR_GENERIC;

	t->last_cb = VALUE;
	return 0;
}

static int on_headers_complete(http_parser *parser)
{
	parser_context *ctx = (parser_context *) parser->data;
	http_subtransport *t = ctx->t;
	http_stream *s = ctx->s;
	git_buf buf = GIT_BUF_INIT;
	int error = 0, no_callback = 0;

	/* Both parse_header_name and parse_header_value are populated
	 * and ready for consumption. */
	if (VALUE == t->last_cb)
		if (on_header_ready(t) < 0)
			return t->parse_error = PARSE_ERROR_GENERIC;

	/* Check for an authentication failure. */

	if (parser->status_code == 401 &&
	    get_verb == s->verb) {
		if (!t->owner->cred_acquire_payload) {
			no_callback = 1;
		} else {
			int allowed_types = 0;

			if (parse_unauthorized_response(&t->www_authenticate,
							&allowed_types, &t->auth_mechanism) < 0)
				return t->parse_error = PARSE_ERROR_GENERIC;

			if (allowed_types &&
			    (!t->cred || 0 == (t->cred->credtype & allowed_types))) {

				error = t->owner->cred_acquire_cb(&t->cred,
								  t->owner->url,
								  t->connection_data.user,
								  allowed_types,
								  t->owner->cred_acquire_payload);

				if (error == GIT_PASSTHROUGH) {
					no_callback = 1;
				} else if (error < 0) {
					return PARSE_ERROR_GENERIC;
				} else {
					assert(t->cred);

					/* Successfully acquired a credential. */
					return t->parse_error = PARSE_ERROR_REPLAY;
				}
			}
		}

		if (no_callback) {
			giterr_set(GITERR_NET, "authentication required but no callback set");
			return t->parse_error = PARSE_ERROR_GENERIC;
		}
	}

	/* Check for a redirect.
	 * Right now we only permit a redirect to the same hostname. */
	if ((parser->status_code == 301 ||
	     parser->status_code == 302 ||
	     (parser->status_code == 303 && get_verb == s->verb) ||
	     parser->status_code == 307) &&
	    t->location) {

		if (s->redirect_count >= 7) {
			giterr_set(GITERR_NET, "Too many redirects");
			return t->parse_error = PARSE_ERROR_GENERIC;
		}

		if (gitno_connection_data_from_url(&t->connection_data, t->location, s->service_url) < 0)
			return t->parse_error = PARSE_ERROR_GENERIC;

		/* Set the redirect URL on the stream. This is a transfer of
		 * ownership of the memory. */
		if (s->redirect_url)
			git__free(s->redirect_url);

		s->redirect_url = t->location;
		t->location = NULL;

		t->connected = 0;
		s->redirect_count++;

		return t->parse_error = PARSE_ERROR_REPLAY;
	}

	/* Check for a 200 HTTP status code. */
	if (parser->status_code != 200) {
		giterr_set(GITERR_NET,
			"Unexpected HTTP status code: %d",
			parser->status_code);
		return t->parse_error = PARSE_ERROR_GENERIC;
	}

	/* The response must contain a Content-Type header. */
	if (!t->content_type) {
		giterr_set(GITERR_NET, "No Content-Type header in response");
		return t->parse_error = PARSE_ERROR_GENERIC;
	}

	/* The Content-Type header must match our expectation. */
	if (get_verb == s->verb)
		git_buf_printf(&buf,
			"application/x-git-%s-advertisement",
			ctx->s->service);
	else
		git_buf_printf(&buf,
			"application/x-git-%s-result",
			ctx->s->service);

	if (git_buf_oom(&buf))
		return t->parse_error = PARSE_ERROR_GENERIC;

	if (strcmp(t->content_type, git_buf_cstr(&buf))) {
		git_buf_free(&buf);
		giterr_set(GITERR_NET,
			"Invalid Content-Type: %s",
			t->content_type);
		return t->parse_error = PARSE_ERROR_GENERIC;
	}

	git_buf_free(&buf);

	return 0;
}

static int on_message_complete(http_parser *parser)
{
	parser_context *ctx = (parser_context *) parser->data;
	http_subtransport *t = ctx->t;

	t->parse_finished = 1;

	return 0;
}

static int on_body_fill_buffer(http_parser *parser, const char *str, size_t len)
{
	parser_context *ctx = (parser_context *) parser->data;
	http_subtransport *t = ctx->t;

	if (ctx->buf_size < len) {
		giterr_set(GITERR_NET, "Can't fit data in the buffer");
		return t->parse_error = PARSE_ERROR_GENERIC;
	}

	memcpy(ctx->buffer, str, len);
	*(ctx->bytes_read) += len;
	ctx->buffer += len;
	ctx->buf_size -= len;

	return 0;
}

static void clear_parser_state(http_subtransport *t)
{
	http_parser_init(&t->parser, HTTP_RESPONSE);
	gitno_buffer_setup(&t->socket,
		&t->parse_buffer,
		t->parse_buffer_data,
		sizeof(t->parse_buffer_data));

	t->last_cb = NONE;
	t->parse_error = 0;
	t->parse_finished = 0;

	git_buf_free(&t->parse_header_name);
	git_buf_init(&t->parse_header_name, 0);

	git_buf_free(&t->parse_header_value);
	git_buf_init(&t->parse_header_value, 0);

	git__free(t->content_type);
	t->content_type = NULL;

	git__free(t->location);
	t->location = NULL;

	git_vector_free_deep(&t->www_authenticate);
}

static int write_chunk(gitno_socket *socket, const char *buffer, size_t len)
{
	git_buf buf = GIT_BUF_INIT;

	/* Chunk header */
	git_buf_printf(&buf, "%X\r\n", (unsigned)len);

	if (git_buf_oom(&buf))
		return -1;

	if (gitno_send(socket, buf.ptr, buf.size, 0) < 0) {
		git_buf_free(&buf);
		return -1;
	}

	git_buf_free(&buf);

	/* Chunk body */
	if (len > 0 && gitno_send(socket, buffer, len, 0) < 0)
		return -1;

	/* Chunk footer */
	if (gitno_send(socket, "\r\n", 2, 0) < 0)
		return -1;

	return 0;
}

static int http_connect(http_subtransport *t)
{
	int flags = 0;

	if (t->connected &&
		http_should_keep_alive(&t->parser) &&
		http_body_is_final(&t->parser))
		return 0;

	if (t->socket.socket)
		gitno_close(&t->socket);

	if (t->connection_data.use_ssl) {
		int tflags;

		if (t->owner->parent.read_flags(&t->owner->parent, &tflags) < 0)
			return -1;

		flags |= GITNO_CONNECT_SSL;

		if (GIT_TRANSPORTFLAGS_NO_CHECK_CERT & tflags)
			flags |= GITNO_CONNECT_SSL_NO_CHECK_CERT;
	}

	if (gitno_connect(&t->socket, t->connection_data.host, t->connection_data.port, flags) < 0)
		return -1;

	t->connected = 1;
	return 0;
}

static int http_stream_read(
	git_smart_subtransport_stream *stream,
	char *buffer,
	size_t buf_size,
	size_t *bytes_read)
{
	http_stream *s = (http_stream *)stream;
	http_subtransport *t = OWNING_SUBTRANSPORT(s);
	parser_context ctx;
	size_t bytes_parsed;

replay:
	*bytes_read = 0;

	assert(t->connected);

	if (!s->sent_request) {
		git_buf request = GIT_BUF_INIT;

		clear_parser_state(t);

		if (gen_request(&request, s, 0) < 0) {
			giterr_set(GITERR_NET, "Failed to generate request");
			return -1;
		}

		if (gitno_send(&t->socket, request.ptr, request.size, 0) < 0) {
			git_buf_free(&request);
			return -1;
		}

		git_buf_free(&request);

		s->sent_request = 1;
	}

	if (!s->received_response) {
		if (s->chunked) {
			assert(s->verb == post_verb);

			/* Flush, if necessary */
			if (s->chunk_buffer_len > 0 &&
				write_chunk(&t->socket, s->chunk_buffer, s->chunk_buffer_len) < 0)
				return -1;

			s->chunk_buffer_len = 0;

			/* Write the final chunk. */
			if (gitno_send(&t->socket, "0\r\n\r\n", 5, 0) < 0)
				return -1;
		}

		s->received_response = 1;
	}

	while (!*bytes_read && !t->parse_finished) {
		t->parse_buffer.offset = 0;

		if (gitno_recv(&t->parse_buffer) < 0)
			return -1;

		/* This call to http_parser_execute will result in invocations of the
		 * on_* family of callbacks. The most interesting of these is
		 * on_body_fill_buffer, which is called when data is ready to be copied
		 * into the target buffer. We need to marshal the buffer, buf_size, and
		 * bytes_read parameters to this callback. */
		ctx.t = t;
		ctx.s = s;
		ctx.buffer = buffer;
		ctx.buf_size = buf_size;
		ctx.bytes_read = bytes_read;

		/* Set the context, call the parser, then unset the context. */
		t->parser.data = &ctx;

		bytes_parsed = http_parser_execute(&t->parser,
			&t->settings,
			t->parse_buffer.data,
			t->parse_buffer.offset);

		t->parser.data = NULL;

		/* If there was a handled authentication failure, then parse_error
		 * will have signaled us that we should replay the request. */
		if (PARSE_ERROR_REPLAY == t->parse_error) {
			s->sent_request = 0;

			if (http_connect(t) < 0)
				return -1;

			goto replay;
		}

		if (t->parse_error < 0)
			return -1;

		if (bytes_parsed != t->parse_buffer.offset) {
			giterr_set(GITERR_NET,
				"HTTP parser error: %s",
				http_errno_description((enum http_errno)t->parser.http_errno));
			return -1;
		}
	}

	return 0;
}

static int http_stream_write_chunked(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	http_stream *s = (http_stream *)stream;
	http_subtransport *t = OWNING_SUBTRANSPORT(s);

	assert(t->connected);

	/* Send the request, if necessary */
	if (!s->sent_request) {
		git_buf request = GIT_BUF_INIT;

		clear_parser_state(t);

		if (gen_request(&request, s, 0) < 0) {
			giterr_set(GITERR_NET, "Failed to generate request");
			return -1;
		}

		if (gitno_send(&t->socket, request.ptr, request.size, 0) < 0) {
			git_buf_free(&request);
			return -1;
		}

		git_buf_free(&request);

		s->sent_request = 1;
	}

	if (len > CHUNK_SIZE) {
		/* Flush, if necessary */
		if (s->chunk_buffer_len > 0) {
			if (write_chunk(&t->socket, s->chunk_buffer, s->chunk_buffer_len) < 0)
				return -1;

			s->chunk_buffer_len = 0;
		}

		/* Write chunk directly */
		if (write_chunk(&t->socket, buffer, len) < 0)
			return -1;
	}
	else {
		/* Append as much to the buffer as we can */
		int count = min(CHUNK_SIZE - s->chunk_buffer_len, len);

		if (!s->chunk_buffer)
			s->chunk_buffer = git__malloc(CHUNK_SIZE);

		memcpy(s->chunk_buffer + s->chunk_buffer_len, buffer, count);
		s->chunk_buffer_len += count;
		buffer += count;
		len -= count;

		/* Is the buffer full? If so, then flush */
		if (CHUNK_SIZE == s->chunk_buffer_len) {
			if (write_chunk(&t->socket, s->chunk_buffer, s->chunk_buffer_len) < 0)
				return -1;

			s->chunk_buffer_len = 0;

			if (len > 0) {
				memcpy(s->chunk_buffer, buffer, len);
				s->chunk_buffer_len = len;
			}
		}
	}

	return 0;
}

static int http_stream_write_single(
	git_smart_subtransport_stream *stream,
	const char *buffer,
	size_t len)
{
	http_stream *s = (http_stream *)stream;
	http_subtransport *t = OWNING_SUBTRANSPORT(s);
	git_buf request = GIT_BUF_INIT;

	assert(t->connected);

	if (s->sent_request) {
		giterr_set(GITERR_NET, "Subtransport configured for only one write");
		return -1;
	}

	clear_parser_state(t);

	if (gen_request(&request, s, len) < 0) {
		giterr_set(GITERR_NET, "Failed to generate request");
		return -1;
	}

	if (gitno_send(&t->socket, request.ptr, request.size, 0) < 0)
		goto on_error;

	if (len && gitno_send(&t->socket, buffer, len, 0) < 0)
		goto on_error;

	git_buf_free(&request);
	s->sent_request = 1;

	return 0;

on_error:
	git_buf_free(&request);
	return -1;
}

static void http_stream_free(git_smart_subtransport_stream *stream)
{
	http_stream *s = (http_stream *)stream;

	if (s->chunk_buffer)
		git__free(s->chunk_buffer);

	if (s->redirect_url)
		git__free(s->redirect_url);

	git__free(s);
}

static int http_stream_alloc(http_subtransport *t,
	git_smart_subtransport_stream **stream)
{
	http_stream *s;

	if (!stream)
		return -1;

	s = git__calloc(sizeof(http_stream), 1);
	GITERR_CHECK_ALLOC(s);

	s->parent.subtransport = &t->parent;
	s->parent.read = http_stream_read;
	s->parent.write = http_stream_write_single;
	s->parent.free = http_stream_free;

	*stream = (git_smart_subtransport_stream *)s;
	return 0;
}

static int http_uploadpack_ls(
	http_subtransport *t,
	git_smart_subtransport_stream **stream)
{
	http_stream *s;

	if (http_stream_alloc(t, stream) < 0)
		return -1;

	s = (http_stream *)*stream;

	s->service = upload_pack_service;
	s->service_url = upload_pack_ls_service_url;
	s->verb = get_verb;

	return 0;
}

static int http_uploadpack(
	http_subtransport *t,
	git_smart_subtransport_stream **stream)
{
	http_stream *s;

	if (http_stream_alloc(t, stream) < 0)
		return -1;

	s = (http_stream *)*stream;

	s->service = upload_pack_service;
	s->service_url = upload_pack_service_url;
	s->verb = post_verb;

	return 0;
}

static int http_receivepack_ls(
	http_subtransport *t,
	git_smart_subtransport_stream **stream)
{
	http_stream *s;

	if (http_stream_alloc(t, stream) < 0)
		return -1;

	s = (http_stream *)*stream;

	s->service = receive_pack_service;
	s->service_url = receive_pack_ls_service_url;
	s->verb = get_verb;

	return 0;
}

static int http_receivepack(
	http_subtransport *t,
	git_smart_subtransport_stream **stream)
{
	http_stream *s;

	if (http_stream_alloc(t, stream) < 0)
		return -1;

	s = (http_stream *)*stream;

	/* Use Transfer-Encoding: chunked for this request */
	s->chunked = 1;
	s->parent.write = http_stream_write_chunked;

	s->service = receive_pack_service;
	s->service_url = receive_pack_service_url;
	s->verb = post_verb;

	return 0;
}

static int http_action(
	git_smart_subtransport_stream **stream,
	git_smart_subtransport *subtransport,
	const char *url,
	git_smart_service_t action)
{
	http_subtransport *t = (http_subtransport *)subtransport;
	int ret;

	if (!stream)
		return -1;

	if ((!t->connection_data.host || !t->connection_data.port || !t->connection_data.path) &&
		 (ret = gitno_connection_data_from_url(&t->connection_data, url, NULL)) < 0)
		return ret;

	if (http_connect(t) < 0)
		return -1;

	switch (action) {
	case GIT_SERVICE_UPLOADPACK_LS:
		return http_uploadpack_ls(t, stream);

	case GIT_SERVICE_UPLOADPACK:
		return http_uploadpack(t, stream);

	case GIT_SERVICE_RECEIVEPACK_LS:
		return http_receivepack_ls(t, stream);

	case GIT_SERVICE_RECEIVEPACK:
		return http_receivepack(t, stream);
	}

	*stream = NULL;
	return -1;
}

static int http_close(git_smart_subtransport *subtransport)
{
	http_subtransport *t = (http_subtransport *) subtransport;

	clear_parser_state(t);

	if (t->socket.socket) {
		gitno_close(&t->socket);
		memset(&t->socket, 0x0, sizeof(gitno_socket));
	}

	if (t->cred) {
		t->cred->free(t->cred);
		t->cred = NULL;
	}

	if (t->url_cred) {
		t->url_cred->free(t->url_cred);
		t->url_cred = NULL;
	}

	gitno_connection_data_free_ptrs(&t->connection_data);

	return 0;
}

static void http_free(git_smart_subtransport *subtransport)
{
	http_subtransport *t = (http_subtransport *) subtransport;

	http_close(subtransport);

	git__free(t);
}

int git_smart_subtransport_http(git_smart_subtransport **out, git_transport *owner)
{
	http_subtransport *t;

	if (!out)
		return -1;

	t = git__calloc(sizeof(http_subtransport), 1);
	GITERR_CHECK_ALLOC(t);

	t->owner = (transport_smart *)owner;
	t->parent.action = http_action;
	t->parent.close = http_close;
	t->parent.free = http_free;

	t->settings.on_header_field = on_header_field;
	t->settings.on_header_value = on_header_value;
	t->settings.on_headers_complete = on_headers_complete;
	t->settings.on_body = on_body_fill_buffer;
	t->settings.on_message_complete = on_message_complete;

	*out = (git_smart_subtransport *) t;
	return 0;
}

#endif /* !GIT_WINHTTP */
