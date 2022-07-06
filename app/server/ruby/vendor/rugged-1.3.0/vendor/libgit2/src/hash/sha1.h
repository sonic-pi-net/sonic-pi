/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_hash_sha1_h__
#define INCLUDE_hash_sha1_h__

#include "common.h"

typedef struct git_hash_sha1_ctx git_hash_sha1_ctx;

#if defined(GIT_SHA1_COLLISIONDETECT)
# include "sha1/collisiondetect.h"
#elif defined(GIT_SHA1_COMMON_CRYPTO)
# include "sha1/common_crypto.h"
#elif defined(GIT_SHA1_OPENSSL)
# include "sha1/openssl.h"
#elif defined(GIT_SHA1_WIN32)
# include "sha1/win32.h"
#elif defined(GIT_SHA1_MBEDTLS)
# include "sha1/mbedtls.h"
#else
# include "sha1/generic.h"
#endif

int git_hash_sha1_global_init(void);

int git_hash_sha1_ctx_init(git_hash_sha1_ctx *ctx);
void git_hash_sha1_ctx_cleanup(git_hash_sha1_ctx *ctx);

int git_hash_sha1_init(git_hash_sha1_ctx *c);
int git_hash_sha1_update(git_hash_sha1_ctx *c, const void *data, size_t len);
int git_hash_sha1_final(git_oid *out, git_hash_sha1_ctx *c);

#endif
