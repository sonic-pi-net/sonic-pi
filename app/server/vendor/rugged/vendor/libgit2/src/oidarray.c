/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2/oidarray.h"
#include "oidarray.h"
#include "array.h"

void git_oidarray_free(git_oidarray *arr)
{
	git__free(arr->ids);
}

void git_oidarray__from_array(git_oidarray *arr, git_array_oid_t *array)
{
	arr->count = array->size;
	arr->ids = array->ptr;
}
