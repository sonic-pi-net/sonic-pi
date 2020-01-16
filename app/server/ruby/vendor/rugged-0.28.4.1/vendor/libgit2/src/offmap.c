/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "offmap.h"

#define kmalloc git__malloc
#define kcalloc git__calloc
#define krealloc git__realloc
#define kreallocarray git__reallocarray
#define kfree git__free
#include "khash.h"

__KHASH_TYPE(off, off64_t, void *)

__KHASH_IMPL(off, static kh_inline, off64_t, void *, 1, kh_int64_hash_func, kh_int64_hash_equal)


int git_offmap_new(git_offmap **out)
{
	*out = kh_init(off);
	GIT_ERROR_CHECK_ALLOC(*out);

	return 0;
}

void git_offmap_free(git_offmap *map)
{
	kh_destroy(off, map);
}

void git_offmap_clear(git_offmap *map)
{
	kh_clear(off, map);
}

size_t git_offmap_size(git_offmap *map)
{
	return kh_size(map);
}

void *git_offmap_get(git_offmap *map, const off64_t key)
{
	size_t idx = kh_get(off, map, key);
	if (idx == kh_end(map) || !kh_exist(map, idx))
		return NULL;
	return kh_val(map, idx);
}

int git_offmap_set(git_offmap *map, const off64_t key, void *value)
{
	size_t idx;
	int rval;

	idx = kh_put(off, map, key, &rval);
	if (rval < 0)
		return -1;

	if (rval == 0)
		kh_key(map, idx) = key;

	kh_val(map, idx) = value;

	return 0;
}

int git_offmap_delete(git_offmap *map, const off64_t key)
{
	khiter_t idx = kh_get(off, map, key);
	if (idx == kh_end(map))
		return GIT_ENOTFOUND;
	kh_del(off, map, idx);
	return 0;
}

int git_offmap_exists(git_offmap *map, const off64_t key)
{
	return kh_get(off, map, key) != kh_end(map);
}

int git_offmap_iterate(void **value, git_offmap *map, size_t *iter, off64_t *key)
{
	size_t i = *iter;

	while (i < map->n_buckets && !kh_exist(map, i))
		i++;

	if (i >= map->n_buckets)
		return GIT_ITEROVER;

	if (key)
		*key = kh_key(map, i);
	if (value)
		*value = kh_value(map, i);
	*iter = ++i;

	return 0;
}
