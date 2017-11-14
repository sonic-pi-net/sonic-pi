/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "auth.h"

#include "git2.h"
#include "buffer.h"

static int basic_next_token(
	git_buf *out, git_http_auth_context *ctx, git_cred *c)
{
	git_cred_userpass_plaintext *cred;
	git_buf raw = GIT_BUF_INIT;
	int error = -1;

	GIT_UNUSED(ctx);

	if (c->credtype != GIT_CREDTYPE_USERPASS_PLAINTEXT) {
		giterr_set(GITERR_INVALID, "invalid credential type for basic auth");
		goto on_error;
	}

	cred = (git_cred_userpass_plaintext *)c;

	git_buf_printf(&raw, "%s:%s", cred->username, cred->password);

	if (git_buf_oom(&raw) ||
		git_buf_puts(out, "Authorization: Basic ") < 0 ||
		git_buf_encode_base64(out, git_buf_cstr(&raw), raw.size) < 0 ||
		git_buf_puts(out, "\r\n") < 0)
		goto on_error;

	error = 0;

on_error:
	if (raw.size)
		git__memzero(raw.ptr, raw.size);

	git_buf_free(&raw);
	return error;
}

static git_http_auth_context basic_context = {
	GIT_AUTHTYPE_BASIC,
	GIT_CREDTYPE_USERPASS_PLAINTEXT,
	NULL,
	basic_next_token,
	NULL
};

int git_http_auth_basic(
	git_http_auth_context **out, const gitno_connection_data *connection_data)
{
	GIT_UNUSED(connection_data);

	*out = &basic_context;
	return 0;
}

int git_http_auth_dummy(
	git_http_auth_context **out, const gitno_connection_data *connection_data)
{
	GIT_UNUSED(connection_data);

	*out = NULL;
	return 0;
}

