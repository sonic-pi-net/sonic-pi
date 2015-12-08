/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_annotated_commit_h__
#define INCLUDE_annotated_commit_h__

#include "git2/oid.h"

/** Internal structure for merge inputs */
struct git_annotated_commit {
	git_commit *commit;

	char *ref_name;
	char *remote_url;

	char id_str[GIT_OID_HEXSZ+1];
};

#endif
