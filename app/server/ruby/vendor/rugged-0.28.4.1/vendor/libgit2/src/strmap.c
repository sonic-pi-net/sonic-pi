/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "strmap.h"

#define kmalloc git__malloc
#define kcalloc git__calloc
#define krealloc git__realloc
#define kreallocarray git__reallocarray
#define kfree git__free
#include "khash.h"

__KHASH_TYPE(str, const char *, void *)

__KHASH_IMPL(str, static kh_inline, const char *, void *, 1, kh_str_hash_func, kh_str_hash_equal)

int git_strmap_new(git_strmap **out)
{
	*out = kh_init(str);
	GIT_ERROR_CHECK_ALLOC(*out);

	return 0;
}

void git_strmap_free(git_strmap *map)
{
	kh_destroy(str, map);
}

void git_strmap_clear(git_strmap *map)
{
	kh_clear(str, map);
}

size_t git_strmap_size(git_strmap *map)
{
	return kh_size(map);
}

void *git_strmap_get(git_strmap *map, const char *key)
{
	size_t idx = kh_get(str, map, key);
	if (idx == kh_end(map) || !kh_exist(map, idx))
		return NULL;
	return kh_val(map, idx);
}

int git_strmap_set(git_strmap *map, const char *key, void *value)
{
	size_t idx;
	int rval;

	idx = kh_put(str, map, key, &rval);
	if (rval < 0)
		return -1;

	if (rval == 0)
		kh_key(map, idx) = key;

	kh_val(map, idx) = value;

	return 0;
}

int git_strmap_delete(git_strmap *map, const char *key)
{
	khiter_t idx = kh_get(str, map, key);
	if (idx == kh_end(map))
		return GIT_ENOTFOUND;
	kh_del(str, map, idx);
	return 0;
}

int git_strmap_exists(git_strmap *map, const char *key)
{
	return kh_get(str, map, key) != kh_end(map);
}

int git_strmap_iterate(void **value, git_strmap *map, size_t *iter, const char **key)
{
	size_t i = *iter;

	while (i < map->n_buckets && !kh_exist(map, i))
		i++;

	if (i >= map->n_buckets)
		return GIT_ITEROVER;

	if (key)
		*key = kh_key(map, i);
	if (value)
		*value = kh_val(map, i);
	*iter = ++i;

	return 0;
}
