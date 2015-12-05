/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_array_h__
#define INCLUDE_array_h__

#include "common.h"

/*
 * Use this to declare a typesafe resizable array of items, a la:
 *
 *     git_array_t(int) my_ints = GIT_ARRAY_INIT;
 *     ...
 *     int *i = git_array_alloc(my_ints);
 *     GITERR_CHECK_ALLOC(i);
 *     ...
 *     git_array_clear(my_ints);
 *
 * You may also want to do things like:
 *
 *     typedef git_array_t(my_struct) my_struct_array_t;
 */
#define git_array_t(type) struct { type *ptr; size_t size, asize; }

#define GIT_ARRAY_INIT { NULL, 0, 0 }

#define git_array_init(a) \
	do { (a).size = (a).asize = 0; (a).ptr = NULL; } while (0)

#define git_array_init_to_size(a, desired) \
	do { (a).size = 0; (a).asize = desired; (a).ptr = git__calloc(desired, sizeof(*(a).ptr)); } while (0)

#define git_array_clear(a) \
	do { git__free((a).ptr); git_array_init(a); } while (0)

#define GITERR_CHECK_ARRAY(a) GITERR_CHECK_ALLOC((a).ptr)


typedef git_array_t(char) git_array_generic_t;

/* use a generic array for growth so this can return the new item */
GIT_INLINE(void *) git_array_grow(void *_a, size_t item_size)
{
	volatile git_array_generic_t *a = _a;
	size_t new_size;
	char *new_array;

	if (a->size < 8) {
		new_size = 8;
	} else {
		if (GIT_MULTIPLY_SIZET_OVERFLOW(&new_size, a->size, 3))
			goto on_oom;
		new_size /= 2;
	}

	if ((new_array = git__reallocarray(a->ptr, new_size, item_size)) == NULL)
		goto on_oom;

	a->ptr = new_array; a->asize = new_size; a->size++;
	return a->ptr + (a->size - 1) * item_size;

on_oom:
	git_array_clear(*a);
	return NULL;
}

#define git_array_alloc(a) \
	(((a).size >= (a).asize) ? \
	git_array_grow(&(a), sizeof(*(a).ptr)) : \
	((a).ptr ? &(a).ptr[(a).size++] : NULL))

#define git_array_last(a) ((a).size ? &(a).ptr[(a).size - 1] : NULL)

#define git_array_pop(a) ((a).size ? &(a).ptr[--(a).size] : NULL)

#define git_array_get(a, i) (((i) < (a).size) ? &(a).ptr[(i)] : NULL)

#define git_array_size(a) (a).size

#define git_array_valid_index(a, i) ((i) < (a).size)

#endif
