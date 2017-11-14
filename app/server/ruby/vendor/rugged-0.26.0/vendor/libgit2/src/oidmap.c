/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "oidmap.h"

GIT_INLINE(khint_t) git_oidmap_hash(const git_oid *oid)
{
	khint_t h;
	memcpy(&h, oid, sizeof(khint_t));
	return h;
}

__KHASH_IMPL(oid, static kh_inline, const git_oid *, void *, 1, git_oidmap_hash, git_oid_equal)

git_oidmap *git_oidmap_alloc()
{
	return kh_init(oid);
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

size_t git_oidmap_lookup_index(git_oidmap *map, const git_oid *key)
{
	return kh_get(oid, map, key);
}

int git_oidmap_valid_index(git_oidmap *map, size_t idx)
{
	return idx != kh_end(map);
}

int git_oidmap_exists(git_oidmap *map, const git_oid *key)
{
	return kh_get(oid, map, key) != kh_end(map);
}

int git_oidmap_has_data(git_oidmap *map, size_t idx)
{
	return kh_exist(map, idx);
}

const git_oid *git_oidmap_key(git_oidmap *map, size_t idx)
{
	return kh_key(map, idx);
}

void git_oidmap_set_key_at(git_oidmap *map, size_t idx, git_oid *key)
{
	kh_key(map, idx) = key;
}

void *git_oidmap_value_at(git_oidmap *map, size_t idx)
{
	return kh_val(map, idx);
}

void git_oidmap_set_value_at(git_oidmap *map, size_t idx, void *value)
{
	kh_val(map, idx) = value;
}

void git_oidmap_delete_at(git_oidmap *map, size_t idx)
{
	kh_del(oid, map, idx);
}

int git_oidmap_put(git_oidmap *map, const git_oid *key, int *err)
{
	return kh_put(oid, map, key, err);
}

void git_oidmap_insert(git_oidmap *map, const git_oid *key, void *value, int *rval)
{
	khiter_t idx = kh_put(oid, map, key, rval);

	if ((*rval) >= 0) {
		if ((*rval) == 0)
			kh_key(map, idx) = key;
		kh_val(map, idx) = value;
	}
}

void git_oidmap_delete(git_oidmap *map, const git_oid *key)
{
	khiter_t idx = git_oidmap_lookup_index(map, key);
	if (git_oidmap_valid_index(map, idx))
		git_oidmap_delete_at(map, idx);
}
