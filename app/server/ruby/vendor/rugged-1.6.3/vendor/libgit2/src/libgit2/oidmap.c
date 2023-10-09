/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "oidmap.h"

#define kmalloc git__malloc
#define kcalloc git__calloc
#define krealloc git__realloc
#define kreallocarray git__reallocarray
#define kfree git__free
#include "khash.h"

__KHASH_TYPE(oid, const git_oid *, void *)

GIT_INLINE(khint_t) git_oidmap_hash(const git_oid *oid)
{
	khint_t h;
	memcpy(&h, oid->id, sizeof(khint_t));
	return h;
}

__KHASH_IMPL(oid, static kh_inline, const git_oid *, void *, 1, git_oidmap_hash, git_oid_equal)

int git_oidmap_new(git_oidmap **out)
{
	*out = kh_init(oid);
	GIT_ERROR_CHECK_ALLOC(*out);

	return 0;
}

void git_oidmap_free(git_oidmap *map)
{
	kh_destroy(oid, map);
}

void git_oidmap_clear(git_oidmap *map)
{
	kh_clear(oid, map);
}

size_t git_oidmap_size(git_oidmap *map)
{
	return kh_size(map);
}

void *git_oidmap_get(git_oidmap *map, const git_oid *key)
{
	size_t idx = kh_get(oid, map, key);
	if (idx == kh_end(map) || !kh_exist(map, idx))
		return NULL;
	return kh_val(map, idx);
}

int git_oidmap_set(git_oidmap *map, const git_oid *key, void *value)
{
	size_t idx;
	int rval;

	idx = kh_put(oid, map, key, &rval);
	if (rval < 0)
		return -1;

	if (rval == 0)
		kh_key(map, idx) = key;

	kh_val(map, idx) = value;

	return 0;
}

int git_oidmap_delete(git_oidmap *map, const git_oid *key)
{
	khiter_t idx = kh_get(oid, map, key);
	if (idx == kh_end(map))
		return GIT_ENOTFOUND;
	kh_del(oid, map, idx);
	return 0;
}

int git_oidmap_exists(git_oidmap *map, const git_oid *key)
{
	return kh_get(oid, map, key) != kh_end(map);
}

int git_oidmap_iterate(void **value, git_oidmap *map, size_t *iter, const git_oid **key)
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
