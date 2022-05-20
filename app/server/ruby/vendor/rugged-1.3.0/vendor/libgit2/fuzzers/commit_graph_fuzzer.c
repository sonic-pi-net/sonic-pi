/*
 * libgit2 commit-graph fuzzer target.
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
#include "commit_graph.h"

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
	git_commit_graph_file file = {{0}};
	git_commit_graph_entry e;
	git_buf commit_graph_buf = GIT_BUF_INIT;
	git_oid oid = {{0}};
	bool append_hash = false;

	if (size < 4)
		return 0;

	/*
	 * If the first byte in the stream has the high bit set, append the
	 * SHA1 hash so that the file is somewhat valid.
	 */
	append_hash = *data & 0x80;
	/* Keep a 4-byte alignment to avoid unaligned accesses. */
	data += 4;
	size -= 4;

	if (append_hash) {
		if (git_buf_init(&commit_graph_buf, size + sizeof(oid)) < 0)
			goto cleanup;
		if (git_hash_buf(&oid, data, size) < 0) {
			fprintf(stderr, "Failed to compute the SHA1 hash\n");
			abort();
		}
		memcpy(commit_graph_buf.ptr, data, size);
		memcpy(commit_graph_buf.ptr + size, &oid, sizeof(oid));
	} else {
		git_buf_attach_notowned(&commit_graph_buf, (char *)data, size);
	}

	if (git_commit_graph_file_parse(
			    &file,
			    (const unsigned char *)git_buf_cstr(&commit_graph_buf),
			    git_buf_len(&commit_graph_buf))
	    < 0)
		goto cleanup;

	/* Search for any oid, just to exercise that codepath. */
	if (git_commit_graph_entry_find(&e, &file, &oid, GIT_OID_HEXSZ) < 0)
		goto cleanup;

cleanup:
	git_commit_graph_file_close(&file);
	git_buf_dispose(&commit_graph_buf);
	return 0;
}
