/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_oidarray_h__
#define INCLUDE_oidarray_h__

#include "common.h"

#include "git2/oidarray.h"
#include "array.h"

typedef git_array_t(git_oid) git_array_oid_t;

extern void git_oidarray__from_array(git_oidarray *arr, git_array_oid_t *array);

#endif
