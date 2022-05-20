/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "oidarray.h"

#include "git2/oidarray.h"
#include "array.h"

void git_oidarray_dispose(git_oidarray *arr)
{
	git__free(arr->ids);
}

void git_oidarray__from_array(git_oidarray *arr, git_array_oid_t *array)
{
	arr->count = array->size;
	arr->ids = array->ptr;
}

void git_oidarray__reverse(git_oidarray *arr)
{
	size_t i;
	git_oid tmp;

	for (i = 0; i < arr->count / 2; i++) {
		git_oid_cpy(&tmp, &arr->ids[i]);
		git_oid_cpy(&arr->ids[i], &arr->ids[(arr->count-1)-i]);
		git_oid_cpy(&arr->ids[(arr->count-1)-i], &tmp);
	}
}

#ifndef GIT_DEPRECATE_HARD

void git_oidarray_free(git_oidarray *arr)
{
	git_oidarray_dispose(arr);
}

#endif
