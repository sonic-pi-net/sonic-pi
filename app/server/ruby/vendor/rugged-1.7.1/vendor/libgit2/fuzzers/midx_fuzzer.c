/*
 * libgit2 multi-pack-index fuzzer target.
 *
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <stdio.h>

#include "git2.h"

#include "common.h"
#include "futils.h"
#include "hash.h"
#include "midx.h"

#include "standalone_driver.h"

int LLVMFuzzerInitialize(int *argc, char ***argv)
{
	GIT_UNUSED(argc);
	GIT_UNUSED(argv);

	if (git_libgit2_init() < 0) {
		fprintf(stderr, "Failed to initialize libgit2\n");
		abort();
	}
	return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
	git_midx_file idx = {{0}};
	git_midx_entry e;
	git_str midx_buf = GIT_STR_INIT;
	unsigned char hash[GIT_HASH_SHA1_SIZE];
	git_oid oid = GIT_OID_NONE;
	bool append_hash = false;

	if (size < 4)
		return 0;

	/*
	 * If the first byte in the stream has the high bit set, append the
	 * SHA1 hash so that the packfile is somewhat valid.
	 */
	append_hash = *data & 0x80;
	/* Keep a 4-byte alignment to avoid unaligned accesses. */
	data += 4;
	size -= 4;

	if (append_hash) {
		if (git_str_init(&midx_buf, size + GIT_HASH_SHA1_SIZE) < 0)
			goto cleanup;
		if (git_hash_buf(hash, data, size, GIT_HASH_ALGORITHM_SHA1) < 0) {
			fprintf(stderr, "Failed to compute the SHA1 hash\n");
			abort();
		}
		memcpy(midx_buf.ptr, data, size);
		memcpy(midx_buf.ptr + size, hash, GIT_HASH_SHA1_SIZE);

		memcpy(oid.id, hash, GIT_OID_SHA1_SIZE);
	} else {
		git_str_attach_notowned(&midx_buf, (char *)data, size);
	}

	if (git_midx_parse(&idx, (const unsigned char *)git_str_cstr(&midx_buf), git_str_len(&midx_buf)) < 0)
		goto cleanup;

	/* Search for any oid, just to exercise that codepath. */
	if (git_midx_entry_find(&e, &idx, &oid, GIT_OID_SHA1_HEXSIZE) < 0)
		goto cleanup;

cleanup:
	git_midx_close(&idx);
	git_str_dispose(&midx_buf);
	return 0;
}
