/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "git2.h"
#include "smart.h"
#include "refs.h"
#include "refspec.h"

static int git_smart__recv_cb(gitno_buffer *buf)
{
	transport_smart *t = (transport_smart *) buf->cb_data;
	size_t old_len, bytes_read;
	int error;

	assert(t->current_stream);

	old_len = buf->offset;

	if ((error = t->current_stream->read(t->current_stream, buf->data + buf->offset, buf->len - buf->offset, &bytes_read)) < 0)
		return error;

	buf->offset += bytes_read;

	if (t->packetsize_cb && !t->cancelled.val) {
		error = t->packetsize_cb(bytes_read, t->packetsize_payload);
		if (error) {
			git_atomic_set(&t->cancelled, 1);
			return GIT_EUSER;
		}
	}

	return (int)(buf->offset - old_len);
}

GIT_INLINE(int) git_smart__reset_stream(transport_smart *t, bool close_subtransport)
{
	if (t->current_stream) {
		t->current_stream->free(t->current_stream);
		t->current_stream = NULL;
	}

	if (close_subtransport &&
		t->wrapped->close(t->wrapped) < 0)
		return -1;

	return 0;
}

static int git_smart__set_callbacks(
	git_transport *transport,
	git_transport_message_cb progress_cb,
	git_transport_message_cb error_cb,
	git_transport_certificate_check_cb certificate_check_cb,
	void *message_cb_payload)
{
	transport_smart *t = (transport_smart *)transport;

	t->progress_cb = progress_cb;
	t->error_cb = error_cb;
	t->certificate_check_cb = certificate_check_cb;
	t->message_cb_payload = message_cb_payload;

	return 0;
}

static int http_header_name_length(const char *http_header)
{
	const char *colon = strchr(http_header, ':');
	if (!colon)
		return 0;
	return colon - http_header;
}

static bool is_malformed_http_header(const char *http_header)
{
	const char *c;
	int name_len;

	// Disallow \r and \n
	c = strchr(http_header, '\r');
	if (c)
		return true;
	c = strchr(http_header, '\n');
	if (c)
		return true;

	// Require a header name followed by :
	name_len = http_header_name_length(http_header);
	if (name_len < 1)
		return true;

	return false;
}

static char *forbidden_custom_headers[] = {
	"User-Agent",
	"Host",
	"Accept",
	"Content-Type",
	"Transfer-Encoding",
	"Content-Length",
};

static bool is_forbidden_custom_header(const char *custom_header)
{
	unsigned long i;
	int name_len = http_header_name_length(custom_header);

	// Disallow headers that we set
	for (i = 0; i < ARRAY_SIZE(forbidden_custom_headers); i++)
		if (strncmp(forbidden_custom_headers[i], custom_header, name_len) == 0)
			return true;

	return false;
}

static int git_smart__set_custom_headers(
	git_transport *transport,
	const git_strarray *custom_headers)
{
	transport_smart *t = (transport_smart *)transport;
	size_t i;

	if (t->custom_headers.count)
		git_strarray_free(&t->custom_headers);

	if (!custom_headers)
		return 0;

	for (i = 0; i < custom_headers->count; i++) {
		if (is_malformed_http_header(custom_headers->strings[i])) {
			giterr_set(GITERR_INVALID, "custom HTTP header '%s' is malformed", custom_headers->strings[i]);
			return -1;
		}
		if (is_forbidden_custom_header(custom_headers->strings[i])) {
			giterr_set(GITERR_INVALID, "custom HTTP header '%s' is already set by libgit2", custom_headers->strings[i]);
			return -1;
		}
	}

	return git_strarray_copy(&t->custom_headers, custom_headers);
}

int git_smart__update_heads(transport_smart *t, git_vector *symrefs)
{
	size_t i;
	git_pkt *pkt;

	git_vector_clear(&t->heads);
	git_vector_foreach(&t->refs, i, pkt) {
		git_pkt_ref *ref = (git_pkt_ref *) pkt;
		if (pkt->type != GIT_PKT_REF)
			continue;

		if (symrefs) {
			git_refspec *spec;
			git_buf buf = GIT_BUF_INIT;
			size_t j;
			int error = 0;

			git_vector_foreach(symrefs, j, spec) {
				git_buf_clear(&buf);
				if (git_refspec_src_matches(spec, ref->head.name) &&
				    !(error = git_refspec_transform(&buf, spec, ref->head.name)))
					ref->head.symref_target = git_buf_detach(&buf);
			}

			git_buf_free(&buf);

			if (error < 0)
				return error;
		}

		if (git_vector_insert(&t->heads, &ref->head) < 0)
			return -1;
	}

	return 0;
}

static void free_symrefs(git_vector *symrefs)
{
	git_refspec *spec;
	size_t i;

	git_vector_foreach(symrefs, i, spec) {
		git_refspec__free(spec);
		git__free(spec);
	}

	git_vector_free(symrefs);
}

static int git_smart__connect(
	git_transport *transport,
	const char *url,
	git_cred_acquire_cb cred_acquire_cb,
	void *cred_acquire_payload,
	int direction,
	int flags)
{
	transport_smart *t = (transport_smart *)transport;
	git_smart_subtransport_stream *stream;
	int error;
	git_pkt *pkt;
	git_pkt_ref *first;
	git_vector symrefs;
	git_smart_service_t service;

	if (git_smart__reset_stream(t, true) < 0)
		return -1;

	t->url = git__strdup(url);
	GITERR_CHECK_ALLOC(t->url);

	t->direction = direction;
	t->flags = flags;
	t->cred_acquire_cb = cred_acquire_cb;
	t->cred_acquire_payload = cred_acquire_payload;

	if (GIT_DIRECTION_FETCH == t->direction)
		service = GIT_SERVICE_UPLOADPACK_LS;
	else if (GIT_DIRECTION_PUSH == t->direction)
		service = GIT_SERVICE_RECEIVEPACK_LS;
	else {
		giterr_set(GITERR_NET, "Invalid direction");
		return -1;
	}

	if ((error = t->wrapped->action(&stream, t->wrapped, t->url, service)) < 0)
		return error;

	/* Save off the current stream (i.e. socket) that we are working with */
	t->current_stream = stream;

	gitno_buffer_setup_callback(&t->buffer, t->buffer_data, sizeof(t->buffer_data), git_smart__recv_cb, t);

	/* 2 flushes for RPC; 1 for stateful */
	if ((error = git_smart__store_refs(t, t->rpc ? 2 : 1)) < 0)
		return error;

	/* Strip the comment packet for RPC */
	if (t->rpc) {
		pkt = (git_pkt *)git_vector_get(&t->refs, 0);

		if (!pkt || GIT_PKT_COMMENT != pkt->type) {
			giterr_set(GITERR_NET, "Invalid response");
			return -1;
		} else {
			/* Remove the comment pkt from the list */
			git_vector_remove(&t->refs, 0);
			git__free(pkt);
		}
	}

	/* We now have loaded the refs. */
	t->have_refs = 1;

	first = (git_pkt_ref *)git_vector_get(&t->refs, 0);

	if ((error = git_vector_init(&symrefs, 1, NULL)) < 0)
		return error;

	/* Detect capabilities */
	if (git_smart__detect_caps(first, &t->caps, &symrefs) < 0)
		return -1;

	/* If the only ref in the list is capabilities^{} with OID_ZERO, remove it */
	if (1 == t->refs.length && !strcmp(first->head.name, "capabilities^{}") &&
		git_oid_iszero(&first->head.oid)) {
		git_vector_clear(&t->refs);
		git_pkt_free((git_pkt *)first);
	}

	/* Keep a list of heads for _ls */
	git_smart__update_heads(t, &symrefs);

	free_symrefs(&symrefs);

	if (t->rpc && git_smart__reset_stream(t, false) < 0)
		return -1;

	/* We're now logically connected. */
	t->connected = 1;

	return 0;
}

static int git_smart__ls(const git_remote_head ***out, size_t *size, git_transport *transport)
{
	transport_smart *t = (transport_smart *)transport;

	if (!t->have_refs) {
		giterr_set(GITERR_NET, "The transport has not yet loaded the refs");
		return -1;
	}

	*out = (const git_remote_head **) t->heads.contents;
	*size = t->heads.length;

	return 0;
}

int git_smart__negotiation_step(git_transport *transport, void *data, size_t len)
{
	transport_smart *t = (transport_smart *)transport;
	git_smart_subtransport_stream *stream;
	int error;

	if (t->rpc && git_smart__reset_stream(t, false) < 0)
		return -1;

	if (GIT_DIRECTION_FETCH != t->direction) {
		giterr_set(GITERR_NET, "This operation is only valid for fetch");
		return -1;
	}

	if ((error = t->wrapped->action(&stream, t->wrapped, t->url, GIT_SERVICE_UPLOADPACK)) < 0)
		return error;

	/* If this is a stateful implementation, the stream we get back should be the same */
	assert(t->rpc || t->current_stream == stream);

	/* Save off the current stream (i.e. socket) that we are working with */
	t->current_stream = stream;

	if ((error = stream->write(stream, (const char *)data, len)) < 0)
		return error;

	gitno_buffer_setup_callback(&t->buffer, t->buffer_data, sizeof(t->buffer_data), git_smart__recv_cb, t);

	return 0;
}

int git_smart__get_push_stream(transport_smart *t, git_smart_subtransport_stream **stream)
{
	int error;

	if (t->rpc && git_smart__reset_stream(t, false) < 0)
		return -1;

	if (GIT_DIRECTION_PUSH != t->direction) {
		giterr_set(GITERR_NET, "This operation is only valid for push");
		return -1;
	}

	if ((error = t->wrapped->action(stream, t->wrapped, t->url, GIT_SERVICE_RECEIVEPACK)) < 0)
		return error;

	/* If this is a stateful implementation, the stream we get back should be the same */
	assert(t->rpc || t->current_stream == *stream);

	/* Save off the current stream (i.e. socket) that we are working with */
	t->current_stream = *stream;

	gitno_buffer_setup_callback(&t->buffer, t->buffer_data, sizeof(t->buffer_data), git_smart__recv_cb, t);

	return 0;
}

static void git_smart__cancel(git_transport *transport)
{
	transport_smart *t = (transport_smart *)transport;

	git_atomic_set(&t->cancelled, 1);
}

static int git_smart__is_connected(git_transport *transport)
{
	transport_smart *t = (transport_smart *)transport;

	return t->connected;
}

static int git_smart__read_flags(git_transport *transport, int *flags)
{
	transport_smart *t = (transport_smart *)transport;

	*flags = t->flags;

	return 0;
}

static int git_smart__close(git_transport *transport)
{
	transport_smart *t = (transport_smart *)transport;
	git_vector *common = &t->common;
	unsigned int i;
	git_pkt *p;
	int ret;
	git_smart_subtransport_stream *stream;
	const char flush[] = "0000";

	/*
	 * If we're still connected at this point and not using RPC,
	 * we should say goodbye by sending a flush, or git-daemon
	 * will complain that we disconnected unexpectedly.
	 */
	if (t->connected && !t->rpc &&
	    !t->wrapped->action(&stream, t->wrapped, t->url, GIT_SERVICE_UPLOADPACK)) {
		t->current_stream->write(t->current_stream, flush, 4);
	}

	ret = git_smart__reset_stream(t, true);

	git_vector_foreach(common, i, p)
		git_pkt_free(p);

	git_vector_free(common);

	if (t->url) {
		git__free(t->url);
		t->url = NULL;
	}

	t->connected = 0;

	return ret;
}

static void git_smart__free(git_transport *transport)
{
	transport_smart *t = (transport_smart *)transport;
	git_vector *refs = &t->refs;
	unsigned int i;
	git_pkt *p;

	/* Make sure that the current stream is closed, if we have one. */
	git_smart__close(transport);

	/* Free the subtransport */
	t->wrapped->free(t->wrapped);

	git_vector_free(&t->heads);
	git_vector_foreach(refs, i, p)
		git_pkt_free(p);

	git_vector_free(refs);

	git_strarray_free(&t->custom_headers);

	git__free(t);
}

static int ref_name_cmp(const void *a, const void *b)
{
	const git_pkt_ref *ref_a = a, *ref_b = b;

	return strcmp(ref_a->head.name, ref_b->head.name);
}

int git_transport_smart_certificate_check(git_transport *transport, git_cert *cert, int valid, const char *hostname)
{
	transport_smart *t = (transport_smart *)transport;

	return t->certificate_check_cb(cert, valid, hostname, t->message_cb_payload);
}

int git_transport_smart_credentials(git_cred **out, git_transport *transport, const char *user, int methods)
{
	transport_smart *t = (transport_smart *)transport;

	return t->cred_acquire_cb(out, t->url, user, methods, t->cred_acquire_payload);
}

int git_transport_smart(git_transport **out, git_remote *owner, void *param)
{
	transport_smart *t;
	git_smart_subtransport_definition *definition = (git_smart_subtransport_definition *)param;

	if (!param)
		return -1;

	t = git__calloc(1, sizeof(transport_smart));
	GITERR_CHECK_ALLOC(t);

	t->parent.version = GIT_TRANSPORT_VERSION;
	t->parent.set_callbacks = git_smart__set_callbacks;
	t->parent.set_custom_headers = git_smart__set_custom_headers;
	t->parent.connect = git_smart__connect;
	t->parent.close = git_smart__close;
	t->parent.free = git_smart__free;
	t->parent.negotiate_fetch = git_smart__negotiate_fetch;
	t->parent.download_pack = git_smart__download_pack;
	t->parent.push = git_smart__push;
	t->parent.ls = git_smart__ls;
	t->parent.is_connected = git_smart__is_connected;
	t->parent.read_flags = git_smart__read_flags;
	t->parent.cancel = git_smart__cancel;

	t->owner = owner;
	t->rpc = definition->rpc;

	if (git_vector_init(&t->refs, 16, ref_name_cmp) < 0) {
		git__free(t);
		return -1;
	}

	if (git_vector_init(&t->heads, 16, ref_name_cmp) < 0) {
		git__free(t);
		return -1;
	}

	if (definition->callback(&t->wrapped, &t->parent, definition->param) < 0) {
		git__free(t);
		return -1;
	}

	*out = (git_transport *) t;
	return 0;
}
