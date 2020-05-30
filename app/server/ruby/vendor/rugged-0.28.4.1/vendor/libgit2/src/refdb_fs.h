/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_refdb_fs_h__
#define INCLUDE_refdb_fs_h__

#include "common.h"

#include "strmap.h"

typedef struct {
	git_strmap *packfile;
	time_t packfile_time;
} git_refcache;

#endif
