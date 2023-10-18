/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "idxmap.h"

#define kmalloc git__malloc
#define kcalloc git__calloc
#define krealloc git__realloc
#define kreallocarray git__reallocarray
#define kfree git__free
#include "khash.h"

__KHASH_TYPE(idx, const git_index_entry *, git_index_entry *)
__KHASH_TYPE(idxicase, const git_index_entry *, git_index_entry *)

/* This is __ac_X31_hash_string but with tolower and it takes the entry's stage into account */
static kh_inline khint_t idxentry_hash(const git_index_entry *e)
{
	const char *s = e->path;
	khint_t h = (khint_t)git__tolower(*s);
	if (h) for (++s ; *s; ++s) h = (h << 5) - h + (khint_t)git__tolower(*s);
	return h + GIT_INDEX_ENTRY_STAGE(e);
}

#define idxentry_equal(a, b) (GIT_INDEX_ENTRY_STAGE(a) == GIT_INDEX_ENTRY_STAGE(b) && strcmp(a->path, b->path) == 0)
#define idxentry_icase_equal(a, b) (GIT_INDEX_ENTRY_STAGE(a) == GIT_INDEX_ENTRY_STAGE(b) && strcasecmp(a->path, b->path) == 0)

__KHASH_IMPL(idx, static kh_inline, const git_index_entry *, git_index_entry *, 1, idxentry_hash, idxentry_equal)
__KHASH_IMPL(idxicase, static kh_inline, const git_index_entry *, git_index_entry *, 1, idxentry_hash, idxentry_icase_equal)

int git_idxmap_new(git_idxmap **out)
{
	*out = kh_init(idx);
	GIT_ERROR_CHECK_ALLOC(*out);

	return 0;
}

int git_idxmap_icase_new(git_idxmap_icase **out)
{
	*out = kh_init(idxicase);
	GIT_ERROR_CHECK_ALLOC(*out);

	return 0;
}

void git_idxmap_free(git_idxmap *map)
{
	kh_destroy(idx, map);
}

void git_idxmap_icase_free(git_idxmap_icase *map)
{
	kh_destroy(idxicase, map);
}

void git_idxmap_clear(git_idxmap *map)
{
	kh_clear(idx, map);
}

void git_idxmap_icase_clear(git_idxmap_icase *map)
{
	kh_clear(idxicase, map);
}

int git_idxmap_resize(git_idxmap *map, size_t size)
{
	if (!git__is_uint32(size) ||
	    kh_resize(idx, map, (khiter_t)size) < 0) {
		git_error_set_oom();
		return -1;
	}
	return 0;
}

int git_idxmap_icase_resize(git_idxmap_icase *map, size_t size)
{
	if (!git__is_uint32(size) ||
	    kh_resize(idxicase, map, (khiter_t)size) < 0) {
		git_error_set_oom();
		return -1;
	}
	return 0;
}

void *git_idxmap_get(git_idxmap *map, const git_index_entry *key)
{
	size_t idx = kh_get(idx, map, key);
	if (idx == kh_end(map) || !kh_exist(map, idx))
		return NULL;
	return kh_val(map, idx);
}

int git_idxmap_set(git_idxmap *map, const git_index_entry *key, void *value)
{
	size_t idx;
	int rval;

	idx = kh_put(idx, map, key, &rval);
	if (rval < 0)
		return -1;

	if (rval == 0)
		kh_key(map, idx) = key;

	kh_val(map, idx) = value;

	return 0;
}

int git_idxmap_icase_set(git_idxmap_icase *map, const git_index_entry *key, void *value)
{
	size_t idx;
	int rval;

	idx = kh_put(idxicase, map, key, &rval);
	if (rval < 0)
		return -1;

	if (rval == 0)
		kh_key(map, idx) = key;

	kh_val(map, idx) = value;

	return 0;
}

void *git_idxmap_icase_get(git_idxmap_icase *map, const git_index_entry *key)
{
	size_t idx = kh_get(idxicase, map, key);
	if (idx == kh_end(map) || !kh_exist(map, idx))
		return NULL;
	return kh_val(map, idx);
}

int git_idxmap_delete(git_idxmap *map, const git_index_entry *key)
{
	khiter_t idx = kh_get(idx, map, key);
	if (idx == kh_end(map))
		return GIT_ENOTFOUND;
	kh_del(idx, map, idx);
	return 0;
}

int git_idxmap_icase_delete(git_idxmap_icase *map, const git_index_entry *key)
{
	khiter_t idx = kh_get(idxicase, map, key);
	if (idx == kh_end(map))
		return GIT_ENOTFOUND;
	kh_del(idxicase, map, idx);
	return 0;
}
