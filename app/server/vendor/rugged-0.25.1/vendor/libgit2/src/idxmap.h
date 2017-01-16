/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_idxmap_h__
#define INCLUDE_idxmap_h__

#include <ctype.h>
#include "common.h"
#include "git2/index.h"

#define kmalloc git__malloc
#define kcalloc git__calloc
#define krealloc git__realloc
#define kreallocarray git__reallocarray
#define kfree git__free
#include "khash.h"

__KHASH_TYPE(idx, const git_index_entry *, git_index_entry *)
__KHASH_TYPE(idxicase, const git_index_entry *, git_index_entry *)

typedef khash_t(idx) git_idxmap;
typedef khash_t(idxicase) git_idxmap_icase;

typedef khiter_t git_idxmap_iter;

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

#define GIT__USE_IDXMAP \
	__KHASH_IMPL(idx, static kh_inline, const git_index_entry *, git_index_entry *, 1, idxentry_hash, idxentry_equal)

#define GIT__USE_IDXMAP_ICASE \
	__KHASH_IMPL(idxicase, static kh_inline, const git_index_entry *, git_index_entry *, 1, idxentry_hash, idxentry_icase_equal)

#define git_idxmap_alloc(hp) \
	((*(hp) = kh_init(idx)) == NULL) ? giterr_set_oom(), -1 : 0

#define git_idxmap_icase_alloc(hp) \
	((*(hp) = kh_init(idxicase)) == NULL) ? giterr_set_oom(), -1 : 0

#define git_idxmap_insert(h, key, val, rval) do { \
	khiter_t __pos = kh_put(idx, h, key, &rval); \
	if (rval >= 0) { \
		if (rval == 0) kh_key(h, __pos) = key; \
		kh_val(h, __pos) = val; \
	} } while (0)

#define git_idxmap_icase_insert(h, key, val, rval) do { \
	khiter_t __pos = kh_put(idxicase, h, key, &rval); \
	if (rval >= 0) { \
		if (rval == 0) kh_key(h, __pos) = key; \
		kh_val(h, __pos) = val; \
	} } while (0)

#define git_idxmap_lookup_index(h, k)  kh_get(idx, h, k)
#define git_idxmap_icase_lookup_index(h, k)  kh_get(idxicase, h, k)
#define git_idxmap_value_at(h, idx)        kh_val(h, idx)
#define git_idxmap_valid_index(h, idx) (idx != kh_end(h))
#define git_idxmap_has_data(h, idx) kh_exist(h, idx)

#define git_idxmap_resize(h,s)  kh_resize(idx, h, s)
#define git_idxmap_free(h)  kh_destroy(idx, h), h = NULL
#define git_idxmap_clear(h) kh_clear(idx, h)

#define git_idxmap_delete_at(h, id)       kh_del(idx, h, id)
#define git_idxmap_icase_delete_at(h, id)       kh_del(idxicase, h, id)

#define git_idxmap_delete(h, key) do { \
	khiter_t __pos = git_idxmap_lookup_index(h, key); \
	if (git_idxmap_valid_index(h, __pos)) \
		git_idxmap_delete_at(h, __pos); } while (0)

#define git_idxmap_icase_delete(h, key) do { \
	khiter_t __pos = git_idxmap_icase_lookup_index(h, key); \
	if (git_idxmap_valid_index(h, __pos)) \
		git_idxmap_icase_delete_at(h, __pos); } while (0)

#define git_idxmap_begin		kh_begin
#define git_idxmap_end		kh_end

#endif
