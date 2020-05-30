/*
 * libgit2 packfile fuzzer target.
 *
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <stdio.h>

#include "git2.h"
#include "git2/sys/mempack.h"
#include "common.h"
#include "buffer.h"

static git_odb *odb = NULL;
static git_odb_backend *mempack = NULL;

/* Arbitrary object to seed the ODB. */
static const unsigned char base_obj[] = { 07, 076 };
static const unsigned int base_obj_len = 2;

int LLVMFuzzerInitialize(int *argc, char ***argv)
{
	GIT_UNUSED(argc);
	GIT_UNUSED(argv);

	if (git_libgit2_init() < 0) {
		fprintf(stderr, "Failed to initialize libgit2\n");
		abort();
	}
	if (git_libgit2_opts(GIT_OPT_SET_PACK_MAX_OBJECTS, 10000000) < 0) {
		fprintf(stderr, "Failed to limit maximum pack object count\n");
		abort();
	}
	if (git_odb_new(&odb) < 0) {
		fprintf(stderr, "Failed to create the odb\n");
		abort();
	}
	if (git_mempack_new(&mempack) < 0) {
		fprintf(stderr, "Failed to create the mempack\n");
		abort();
	}
	if (git_odb_add_backend(odb, mempack, 999) < 0) {
		fprintf(stderr, "Failed to add the mempack\n");
		abort();
	}
	return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
	git_indexer_progress stats = {0, 0};
	git_indexer *indexer = NULL;
	git_buf path = GIT_BUF_INIT;
	git_oid oid;
	bool append_hash = false;

	if (size == 0)
		return 0;

	if (!odb || !mempack) {
		fprintf(stderr, "Global state not initialized\n");
		abort();
	}
	git_mempack_reset(mempack);

	if (git_odb_write(&oid, odb, base_obj, base_obj_len, GIT_OBJECT_BLOB) < 0) {
		fprintf(stderr, "Failed to add an object to the odb\n");
		abort();
	}

	if (git_indexer_new(&indexer, ".", 0, odb, NULL) < 0) {
		fprintf(stderr, "Failed to create the indexer: %s\n",
			git_error_last()->message);
		abort();
	}

	/*
	 * If the first byte in the stream has the high bit set, append the
	 * SHA1 hash so that the packfile is somewhat valid.
	 */
	append_hash = *data & 0x80;
	++data;
	--size;

	if (git_indexer_append(indexer, data, size, &stats) < 0)
		goto cleanup;
	if (append_hash) {
		if (git_odb_hash(&oid, data, size, GIT_OBJECT_BLOB) < 0) {
			fprintf(stderr, "Failed to compute the SHA1 hash\n");
			abort();
		}
		if (git_indexer_append(indexer, &oid, sizeof(oid), &stats) < 0) {
			goto cleanup;
		}
	}
	if (git_indexer_commit(indexer, &stats) < 0)
		goto cleanup;

	if (git_buf_printf(&path, "pack-%s.idx", git_oid_tostr_s(git_indexer_hash(indexer))) < 0)
		goto cleanup;
	p_unlink(git_buf_cstr(&path));

	git_buf_clear(&path);

	if (git_buf_printf(&path, "pack-%s.pack", git_oid_tostr_s(git_indexer_hash(indexer))) < 0)
		goto cleanup;
	p_unlink(git_buf_cstr(&path));

cleanup:
	git_mempack_reset(mempack);
	git_indexer_free(indexer);
	git_buf_dispose(&path);
	return 0;
}
