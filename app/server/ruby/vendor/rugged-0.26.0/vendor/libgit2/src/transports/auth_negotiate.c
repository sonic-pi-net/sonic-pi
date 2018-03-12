/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "auth_negotiate.h"

#ifdef GIT_GSSAPI

#include "git2.h"
#include "buffer.h"
#include "auth.h"

#include <gssapi.h>
#include <krb5.h>

static gss_OID_desc negotiate_oid_spnego =
	{ 6, (void *) "\x2b\x06\x01\x05\x05\x02" };
static gss_OID_desc negotiate_oid_krb5 =
	{ 9, (void *) "\x2a\x86\x48\x86\xf7\x12\x01\x02\x02" };

static gss_OID negotiate_oids[] =
	{ &negotiate_oid_spnego, &negotiate_oid_krb5, NULL };

typedef struct {
	git_http_auth_context parent;
	unsigned configured : 1,
		complete : 1;
	git_buf target;
	char *challenge;
	gss_ctx_id_t gss_context;
	gss_OID oid;
} http_auth_negotiate_context;

static void negotiate_err_set(
	OM_uint32 status_major,
	OM_uint32 status_minor,
	const char *message)
{
	gss_buffer_desc buffer = GSS_C_EMPTY_BUFFER;
	OM_uint32 status_display, context = 0;

	if (gss_display_status(&status_display, status_major, GSS_C_GSS_CODE,
		GSS_C_NO_OID, &context, &buffer) == GSS_S_COMPLETE) {
		giterr_set(GITERR_NET, "%s: %.*s (%d.%d)",
			message, (int)buffer.length, (const char *)buffer.value,
			status_major, status_minor);
		gss_release_buffer(&status_minor, &buffer);
	} else {
		giterr_set(GITERR_NET, "%s: unknown negotiate error (%d.%d)",
			message, status_major, status_minor);
	}
}

static int negotiate_set_challenge(
	git_http_auth_context *c,
	const char *challenge)
{
	http_auth_negotiate_context *ctx = (http_auth_negotiate_context *)c;

	assert(ctx && ctx->configured && challenge);

	git__free(ctx->challenge);

	ctx->challenge = git__strdup(challenge);
	GITERR_CHECK_ALLOC(ctx->challenge);

	return 0;
}

static int negotiate_next_token(
	git_buf *buf,
	git_http_auth_context *c,
	git_cred *cred)
{
	http_auth_negotiate_context *ctx = (http_auth_negotiate_context *)c;
	OM_uint32 status_major, status_minor;
	gss_buffer_desc target_buffer = GSS_C_EMPTY_BUFFER,
		input_token = GSS_C_EMPTY_BUFFER,
		output_token = GSS_C_EMPTY_BUFFER;
	gss_buffer_t input_token_ptr = GSS_C_NO_BUFFER;
	git_buf input_buf = GIT_BUF_INIT;
	gss_name_t server = NULL;
	gss_OID mech;
	size_t challenge_len;
	int error = 0;

	assert(buf && ctx && ctx->configured && cred && cred->credtype == GIT_CREDTYPE_DEFAULT);

	if (ctx->complete)
		return 0;

	target_buffer.value = (void *)ctx->target.ptr;
	target_buffer.length = ctx->target.size;

	status_major = gss_import_name(&status_minor, &target_buffer,
		GSS_C_NT_HOSTBASED_SERVICE, &server);

	if (GSS_ERROR(status_major)) {
		negotiate_err_set(status_major, status_minor,
			"Could not parse principal");
		error = -1;
		goto done;
	}

	challenge_len = ctx->challenge ? strlen(ctx->challenge) : 0;

	if (challenge_len < 9) {
		giterr_set(GITERR_NET, "no negotiate challenge sent from server");
		error = -1;
		goto done;
	} else if (challenge_len > 9) {
		if (git_buf_decode_base64(&input_buf,
				ctx->challenge + 10, challenge_len - 10) < 0) {
			giterr_set(GITERR_NET, "invalid negotiate challenge from server");
			error = -1;
			goto done;
		}

		input_token.value = input_buf.ptr;
		input_token.length = input_buf.size;
		input_token_ptr = &input_token;
	} else if (ctx->gss_context != GSS_C_NO_CONTEXT) {
		giterr_set(GITERR_NET, "could not restart authentication");
		error = -1;
		goto done;
	}

	mech = &negotiate_oid_spnego;

	if (GSS_ERROR(status_major = gss_init_sec_context(
		&status_minor,
		GSS_C_NO_CREDENTIAL,
		&ctx->gss_context,
		server,
		mech,
		GSS_C_DELEG_FLAG | GSS_C_MUTUAL_FLAG,
		GSS_C_INDEFINITE,
		GSS_C_NO_CHANNEL_BINDINGS,
		input_token_ptr,
		NULL,
		&output_token,
		NULL,
		NULL))) {
		negotiate_err_set(status_major, status_minor, "Negotiate failure");
		error = -1;
		goto done;
	}

	/* This message merely told us auth was complete; we do not respond. */
	if (status_major == GSS_S_COMPLETE) {
		ctx->complete = 1;
		goto done;
	}

	git_buf_puts(buf, "Authorization: Negotiate ");
	git_buf_encode_base64(buf, output_token.value, output_token.length);
	git_buf_puts(buf, "\r\n");

	if (git_buf_oom(buf))
		error = -1;

done:
	gss_release_name(&status_minor, &server);
	gss_release_buffer(&status_minor, (gss_buffer_t) &output_token);
	git_buf_free(&input_buf);
	return error;
}

static void negotiate_context_free(git_http_auth_context *c)
{
	http_auth_negotiate_context *ctx = (http_auth_negotiate_context *)c;
	OM_uint32 status_minor;

	if (ctx->gss_context != GSS_C_NO_CONTEXT) {
		gss_delete_sec_context(
			&status_minor, &ctx->gss_context, GSS_C_NO_BUFFER);
		ctx->gss_context = GSS_C_NO_CONTEXT;
	}

	git_buf_free(&ctx->target);

	git__free(ctx->challenge);

	ctx->configured = 0;
	ctx->complete = 0;
	ctx->oid = NULL;

	git__free(ctx);
}

static int negotiate_init_context(
	http_auth_negotiate_context *ctx,
	const gitno_connection_data *connection_data)
{
	OM_uint32 status_major, status_minor;
	gss_OID item, *oid;
	gss_OID_set mechanism_list;
	size_t i;

	/* Query supported mechanisms looking for SPNEGO) */
	if (GSS_ERROR(status_major =
		gss_indicate_mechs(&status_minor, &mechanism_list))) {
		negotiate_err_set(status_major, status_minor,
			"could not query mechanisms");
		return -1;
	}

	if (mechanism_list) {
		for (oid = negotiate_oids; *oid; oid++) {
			for (i = 0; i < mechanism_list->count; i++) {
				item = &mechanism_list->elements[i];

				if (item->length == (*oid)->length &&
					memcmp(item->elements, (*oid)->elements, item->length) == 0) {
					ctx->oid = *oid;
					break;
				}

			}

			if (ctx->oid)
				break;
		}
	}

	gss_release_oid_set(&status_minor, &mechanism_list);

	if (!ctx->oid) {
		giterr_set(GITERR_NET, "negotiate authentication is not supported");
		return -1;
	}

	git_buf_puts(&ctx->target, "HTTP@");
	git_buf_puts(&ctx->target, connection_data->host);

	if (git_buf_oom(&ctx->target))
		return -1;

	ctx->gss_context = GSS_C_NO_CONTEXT;
	ctx->configured = 1;

	return 0;
}

int git_http_auth_negotiate(
	git_http_auth_context **out,
	const gitno_connection_data *connection_data)
{
	http_auth_negotiate_context *ctx;

	*out = NULL;

	ctx = git__calloc(1, sizeof(http_auth_negotiate_context));
	GITERR_CHECK_ALLOC(ctx);

	if (negotiate_init_context(ctx, connection_data) < 0) {
		git__free(ctx);
		return -1;
	}

	ctx->parent.type = GIT_AUTHTYPE_NEGOTIATE;
	ctx->parent.credtypes = GIT_CREDTYPE_DEFAULT;
	ctx->parent.set_challenge = negotiate_set_challenge;
	ctx->parent.next_token = negotiate_next_token;
	ctx->parent.free = negotiate_context_free;

	*out = (git_http_auth_context *)ctx;

	return 0;
}

#endif /* GIT_GSSAPI */

