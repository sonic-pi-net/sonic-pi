/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_push_h__
#define INCLUDE_push_h__

#include "git2.h"
#include "refspec.h"

typedef struct push_spec {
	struct git_refspec refspec;

	git_oid loid;
	git_oid roid;
} push_spec;

typedef struct push_status {
	bool ok;

	char *ref;
	char *msg;
} push_status;

struct git_push {
	git_repository *repo;
	git_packbuilder *pb;
	git_remote *remote;
	git_vector specs;
	bool report_status;

	/* report-status */
	bool unpack_ok;
	git_vector status;

	/* options */
	unsigned pb_parallelism;

	git_packbuilder_progress pack_progress_cb;
	void *pack_progress_cb_payload;
	git_push_transfer_progress transfer_progress_cb;
	void *transfer_progress_cb_payload;
};

/**
 * Free the given push status object
 *
 * @param status The push status object
 */
void git_push_status_free(push_status *status);

#endif
