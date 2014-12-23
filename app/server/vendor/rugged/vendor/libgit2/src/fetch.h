/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_fetch_h__
#define INCLUDE_fetch_h__

#include "netops.h"

int git_fetch_negotiate(git_remote *remote);

int git_fetch_download_pack(git_remote *remote);

int git_fetch__download_pack(
		git_transport *t,
		git_repository *repo,
		git_transfer_progress *stats,
		git_transfer_progress_cb progress_cb,
		void *progress_payload);

int git_fetch_setup_walk(git_revwalk **out, git_repository *repo);

#endif
