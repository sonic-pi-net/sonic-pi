/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_hash_sha1_generic_h__
#define INCLUDE_hash_sha1_generic_h__

#include "hash/sha1.h"

struct git_hash_sha1_ctx {
	uint64_t size;
	unsigned int H[5];
	unsigned int W[16];
};

#endif
