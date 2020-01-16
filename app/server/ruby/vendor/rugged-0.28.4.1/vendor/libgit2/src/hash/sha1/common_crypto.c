/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common_crypto.h"

#define CC_LONG_MAX ((CC_LONG)-1)

int git_hash_sha1_global_init(void)
{
	return 0;
}

int git_hash_sha1_ctx_init(git_hash_sha1_ctx *ctx)
{
	return git_hash_sha1_init(ctx);
}

void git_hash_sha1_ctx_cleanup(git_hash_sha1_ctx *ctx)
{
	GIT_UNUSED(ctx);
}

int git_hash_sha1_init(git_hash_sha1_ctx *ctx)
{
	assert(ctx);
	CC_SHA1_Init(&ctx->c);
	return 0;
}

int git_hash_sha1_update(git_hash_sha1_ctx *ctx, const void *_data, size_t len)
{
	const unsigned char *data = _data;

	assert(ctx);

	while (len > 0) {
		CC_LONG chunk = (len > CC_LONG_MAX) ? CC_LONG_MAX : (CC_LONG)len;

		CC_SHA1_Update(&ctx->c, data, chunk);

		data += chunk;
		len -= chunk;
	}

	return 0;
}

int git_hash_sha1_final(git_oid *out, git_hash_sha1_ctx *ctx)
{
	assert(ctx);
	CC_SHA1_Final(out->id, &ctx->c);
	return 0;
}
