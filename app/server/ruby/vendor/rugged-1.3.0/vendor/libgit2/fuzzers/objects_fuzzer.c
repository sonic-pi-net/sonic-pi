/*
 * libgit2 packfile fuzzer target.
 *
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2.h"
#include "object.h"

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
	const git_object_t types[] = {
		GIT_OBJECT_BLOB, GIT_OBJECT_TREE, GIT_OBJECT_COMMIT, GIT_OBJECT_TAG
	};
	git_object *object = NULL;
	size_t i;

	/*
	 * Brute-force parse this as every object type. We want
	 * to stress the parsing logic anyway, so this is fine
	 * to do.
	 */
	for (i = 0; i < ARRAY_SIZE(types); i++) {
		if (git_object__from_raw(&object, (const char *) data, size, types[i]) < 0)
			continue;
		git_object_free(object);
		object = NULL;
	}

	return 0;
}
