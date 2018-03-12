/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "offmap.h"

__KHASH_IMPL(off, static kh_inline, git_off_t, void *, 1, kh_int64_hash_func, kh_int64_hash_equal)

git_offmap *git_offmap_alloc(void)
{
	return kh_init(off);
}

void git_offmap_free(git_offmap *map)
{
	kh_destroy(off, map);
}

void git_offmap_clear(git_offmap *map)
{
	kh_clear(off, map);
}

size_t git_offmap_num_entries(git_offmap *map)
{
	return kh_size(map);
}

size_t git_offmap_lookup_index(git_offmap *map, const git_off_t key)
{
	return kh_get(off, map, key);
}

int git_offmap_valid_index(git_offmap *map, size_t idx)
{
	return idx != kh_end(map);
}

int git_offmap_exists(git_offmap *map, const git_off_t key)
{
	return kh_get(off, map, key) != kh_end(map);
}

void *git_offmap_value_at(git_offmap *map, size_t idx)
{
	return kh_val(map, idx);
}

void git_offmap_set_value_at(git_offmap *map, size_t idx, void *value)
{
	kh_val(map, idx) = value;
}

void git_offmap_delete_at(git_offmap *map, size_t idx)
{
	kh_del(off, map, idx);
}

int git_offmap_put(git_offmap *map, const git_off_t key, int *err)
{
	return kh_put(off, map, key, err);
}

void git_offmap_insert(git_offmap *map, const git_off_t key, void *value, int *rval)
{
	khiter_t idx = kh_put(off, map, key, rval);

	if ((*rval) >= 0) {
		if ((*rval) == 0)
			kh_key(map, idx) = key;
		kh_val(map, idx) = value;
	}
}

void git_offmap_delete(git_offmap *map, const git_off_t key)
{
	khiter_t idx = git_offmap_lookup_index(map, key);
	if (git_offmap_valid_index(map, idx))
		git_offmap_delete_at(map, idx);
}
