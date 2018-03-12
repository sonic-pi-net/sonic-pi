/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "idxmap.h"

/* This is __ac_X31_hash_string but with tolower and it takes the entry's stage into account */
static kh_inline khint_t idxentry_hash(const git_index_entry *e)
{
	const char *s = e->path;
	khint_t h = (khint_t)git__tolower(*s);
	if (h) for (++s ; *s; ++s) h = (h << 5) - h + (khint_t)git__tolower(*s);
	return h + GIT_IDXENTRY_STAGE(e);
}

#define idxentry_equal(a, b) (GIT_IDXENTRY_STAGE(a) == GIT_IDXENTRY_STAGE(b) && strcmp(a->path, b->path) == 0)
#define idxentry_icase_equal(a, b) (GIT_IDXENTRY_STAGE(a) == GIT_IDXENTRY_STAGE(b) && strcasecmp(a->path, b->path) == 0)

__KHASH_IMPL(idx, static kh_inline, const git_index_entry *, git_index_entry *, 1, idxentry_hash, idxentry_equal)
__KHASH_IMPL(idxicase, static kh_inline, const git_index_entry *, git_index_entry *, 1, idxentry_hash, idxentry_icase_equal)

int git_idxmap_alloc(git_idxmap **map)
{
	if ((*map = kh_init(idx)) == NULL) {
		giterr_set_oom();
		return -1;
	}

	return 0;
}

int git_idxmap_icase_alloc(git_idxmap_icase **map)
{
	if ((*map = kh_init(idxicase)) == NULL) {
		giterr_set_oom();
		return -1;
	}

	return 0;
}

void git_idxmap_insert(git_idxmap *map, const git_index_entry *key, void *value, int *rval)
{
	khiter_t idx = kh_put(idx, map, key, rval);

	if ((*rval) >= 0) {
		if ((*rval) == 0)
			kh_key(map, idx) = key;
		kh_val(map, idx) = value;
	}
}

void git_idxmap_icase_insert(git_idxmap_icase *map, const git_index_entry *key, void *value, int *rval)
{
	khiter_t idx = kh_put(idxicase, map, key, rval);

	if ((*rval) >= 0) {
		if ((*rval) == 0)
			kh_key(map, idx) = key;
		kh_val(map, idx) = value;
	}
}

size_t git_idxmap_lookup_index(git_idxmap *map, const git_index_entry *key)
{
	return kh_get(idx, map, key);
}

size_t git_idxmap_icase_lookup_index(git_idxmap_icase *map, const git_index_entry *key)
{
	return kh_get(idxicase, map, key);
}

void *git_idxmap_value_at(git_idxmap *map, size_t idx)
{
	return kh_val(map, idx);
}

int git_idxmap_valid_index(git_idxmap *map, size_t idx)
{
	return idx != kh_end(map);
}

int git_idxmap_has_data(git_idxmap *map, size_t idx)
{
	return kh_exist(map, idx);
}

void git_idxmap_resize(git_idxmap *map, size_t size)
{
	kh_resize(idx, map, size);
}

void git_idxmap_icase_resize(git_idxmap_icase *map, size_t size)
{
	kh_resize(idxicase, map, size);
}

void git_idxmap_free(git_idxmap *map)
{
	kh_destroy(idx, map);
}

void git_idxmap_clear(git_idxmap *map)
{
	kh_clear(idx, map);
}

void git_idxmap_delete_at(git_idxmap *map, size_t idx)
{
	kh_del(idx, map, idx);
}

void git_idxmap_icase_delete_at(git_idxmap_icase *map, size_t idx)
{
	kh_del(idxicase, map, idx);
}

void git_idxmap_delete(git_idxmap *map, const git_index_entry *key)
{
	khiter_t idx = git_idxmap_lookup_index(map, key);
	if (git_idxmap_valid_index(map, idx))
		git_idxmap_delete_at(map, idx);
}
void git_idxmap_icase_delete(git_idxmap_icase *map, const git_index_entry *key)
{
	khiter_t idx = git_idxmap_icase_lookup_index(map, key);
	if (git_idxmap_valid_index((git_idxmap *)map, idx))
		git_idxmap_icase_delete_at(map, idx);
}
