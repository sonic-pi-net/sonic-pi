/*
 * libgit2 config file parser fuzz target.
 *
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2.h"
#include "config_backend.h"

#define UNUSED(x) (void)(x)

int foreach_cb(const git_config_entry *entry, void *payload)
{
	UNUSED(entry);
	UNUSED(payload);

	return 0;
}

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
	git_config *cfg = NULL;
	git_config_backend *backend = NULL;
	int err = 0;

	if ((err = git_config_new(&cfg)) != 0) {
		goto out;
	}

	if ((err = git_config_backend_from_string(&backend, (const char*)data, size)) != 0) {
		goto out;
	}
	if ((err = git_config_add_backend(cfg, backend, 0, NULL, 0)) != 0) {
		goto out;
	}
	/* Now owned by the config */
	backend = NULL;

	git_config_foreach(cfg, foreach_cb, NULL);
 out:
	git_config_backend_free(backend);
	git_config_free(cfg);

	return 0;
}
