/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_http_auth_h__
#define INCLUDE_http_auth_h__

#include "git2.h"
#include "netops.h"

typedef enum {
	GIT_AUTHTYPE_BASIC = 1,
	GIT_AUTHTYPE_NEGOTIATE = 2,
} git_http_authtype_t;

typedef struct git_http_auth_context git_http_auth_context;

struct git_http_auth_context {
	/** Type of scheme */
	git_http_authtype_t type;

	/** Supported credentials */
	git_credtype_t credtypes;

	/** Sets the challenge on the authentication context */
	int (*set_challenge)(git_http_auth_context *ctx, const char *challenge);

	/** Gets the next authentication token from the context */
	int (*next_token)(git_buf *out, git_http_auth_context *ctx, git_cred *cred);

	/** Frees the authentication context */
	void (*free)(git_http_auth_context *ctx);
};

typedef struct {
	/** Type of scheme */
	git_http_authtype_t type;

	/** Name of the scheme (as used in the Authorization header) */
	const char *name;

	/** Credential types this scheme supports */
	git_credtype_t credtypes;

	/** Function to initialize an authentication context */
	int (*init_context)(
		git_http_auth_context **out,
		const gitno_connection_data *connection_data);
} git_http_auth_scheme;

int git_http_auth_dummy(
	git_http_auth_context **out,
	const gitno_connection_data *connection_data);

int git_http_auth_basic(
	git_http_auth_context **out,
	const gitno_connection_data *connection_data);

#endif

