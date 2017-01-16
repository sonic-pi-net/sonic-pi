/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_hash_generic_h__
#define INCLUDE_hash_generic_h__

#include "hash.h"

struct git_hash_ctx {
	unsigned long long size;
	unsigned int H[5];
	unsigned int W[16];
};

#define git_hash_global_init() 0
#define git_hash_ctx_init(ctx) git_hash_init(ctx)
#define git_hash_ctx_cleanup(ctx)

#endif /* INCLUDE_hash_generic_h__ */
