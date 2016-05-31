/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "hash.h"

int git_hash_buf(git_oid *out, const void *data, size_t len)
{
	git_hash_ctx ctx;
	int error = 0;

	if (git_hash_ctx_init(&ctx) < 0)
		return -1;

	if ((error = git_hash_update(&ctx, data, len)) >= 0)
		error = git_hash_final(out, &ctx);

	git_hash_ctx_cleanup(&ctx);
	
	return error;
}

int git_hash_vec(git_oid *out, git_buf_vec *vec, size_t n)
{
	git_hash_ctx ctx;
	size_t i;
	int error = 0;

	if (git_hash_ctx_init(&ctx) < 0)
		return -1;

	for (i = 0; i < n; i++) {
		if ((error = git_hash_update(&ctx, vec[i].data, vec[i].len)) < 0)
			goto done;
	}

	error = git_hash_final(out, &ctx);

done:
	git_hash_ctx_cleanup(&ctx);

	return error;
}
