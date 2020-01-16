/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_hash_h__
#define INCLUDE_hash_h__

#include "common.h"

#include "git2/oid.h"

typedef struct {
	void *data;
	size_t len;
} git_buf_vec;

typedef enum {
	GIT_HASH_ALGO_UNKNOWN = 0,
	GIT_HASH_ALGO_SHA1,
} git_hash_algo_t;

#include "hash/sha1.h"

typedef struct git_hash_ctx {
	union {
		git_hash_sha1_ctx sha1;
	};
	git_hash_algo_t algo;
} git_hash_ctx;

int git_hash_global_init(void);

int git_hash_ctx_init(git_hash_ctx *ctx);
void git_hash_ctx_cleanup(git_hash_ctx *ctx);

int git_hash_init(git_hash_ctx *c);
int git_hash_update(git_hash_ctx *c, const void *data, size_t len);
int git_hash_final(git_oid *out, git_hash_ctx *c);

int git_hash_buf(git_oid *out, const void *data, size_t len);
int git_hash_vec(git_oid *out, git_buf_vec *vec, size_t n);

#endif
