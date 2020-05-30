/*
 * libgit2 patch parser fuzzer target.
 *
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2.h"
#include "patch.h"
#include "patch_parse.h"

#define UNUSED(x) (void)(x)

int LLVMFuzzerInitialize(int *argc, char ***argv)
{
	UNUSED(argc);
	UNUSED(argv);

	if (git_libgit2_init() < 0)
		abort();

	return 0;
}

int LLVMFuzzerTestOneInput(const uint8_t *data, size_t size)
{
	if (size) {
		git_patch *patch = NULL;
		git_patch_options opts = GIT_PATCH_OPTIONS_INIT;
		opts.prefix_len = (uint32_t)data[0];
		git_patch_from_buffer(&patch, (const char *)data + 1, size - 1,
		                      &opts);
		git_patch_free(patch);
	}
	return 0;
}
