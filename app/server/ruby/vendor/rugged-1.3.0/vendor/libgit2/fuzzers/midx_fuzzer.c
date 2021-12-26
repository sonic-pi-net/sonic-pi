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

#include "buffer.h"
#include "common.h"
#include "futils.h"
#include "hash.h"
#include "midx.h"

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
	git_buf midx_buf = GIT_BUF_INIT;
	git_oid oid = {{0}};
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
		if (git_buf_init(&midx_buf, size + sizeof(oid)) < 0)
			goto cleanup;
		if (git_hash_buf(&oid, data, size) < 0) {
			fprintf(stderr, "Failed to compute the SHA1 hash\n");
			abort();
		}
		memcpy(midx_buf.ptr, data, size);
		memcpy(midx_buf.ptr + size, &oid, sizeof(oid));
	} else {
		git_buf_attach_notowned(&midx_buf, (char *)data, size);
	}

	if (git_midx_parse(&idx, (const unsigned char *)git_buf_cstr(&midx_buf), git_buf_len(&midx_buf)) < 0)
		goto cleanup;

	/* Search for any oid, just to exercise that codepath. */
	if (git_midx_entry_find(&e, &idx, &oid, GIT_OID_HEXSZ) < 0)
		goto cleanup;

cleanup:
	git_midx_close(&idx);
	git_buf_dispose(&midx_buf);
	return 0;
}
