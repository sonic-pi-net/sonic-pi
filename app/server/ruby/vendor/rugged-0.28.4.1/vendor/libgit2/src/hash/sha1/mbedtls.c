/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "mbedtls.h"

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
    assert(ctx);
    mbedtls_sha1_free(&ctx->c);
}

int git_hash_sha1_init(git_hash_sha1_ctx *ctx)
{
    assert(ctx);
    mbedtls_sha1_init(&ctx->c);
    mbedtls_sha1_starts(&ctx->c);
    return 0;
}

int git_hash_sha1_update(git_hash_sha1_ctx *ctx, const void *data, size_t len)
{
    assert(ctx);
    mbedtls_sha1_update(&ctx->c, data, len);
    return 0;
}

int git_hash_sha1_final(git_oid *out, git_hash_sha1_ctx *ctx)
{
    assert(ctx);
    mbedtls_sha1_finish(&ctx->c, out->id);
    return 0;
}
