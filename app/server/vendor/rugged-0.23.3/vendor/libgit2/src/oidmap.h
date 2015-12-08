/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_oidmap_h__
#define INCLUDE_oidmap_h__

#include "common.h"
#include "git2/oid.h"

#define kmalloc git__malloc
#define kcalloc git__calloc
#define krealloc git__realloc
#define kreallocarray git__reallocarray
#define kfree git__free
#include "khash.h"

__KHASH_TYPE(oid, const git_oid *, void *)
typedef khash_t(oid) git_oidmap;

GIT_INLINE(khint_t) git_oidmap_hash(const git_oid *oid)
{
	khint_t h;
	memcpy(&h, oid, sizeof(khint_t));
	return h;
}

#define GIT__USE_OIDMAP \
	__KHASH_IMPL(oid, static kh_inline, const git_oid *, void *, 1, git_oidmap_hash, git_oid_equal)

#define git_oidmap_alloc() kh_init(oid)
#define git_oidmap_free(h) kh_destroy(oid,h), h = NULL

#define git_oidmap_lookup_index(h, k) kh_get(oid, h, k)
#define git_oidmap_valid_index(h, idx) (idx != kh_end(h))

#define git_oidmap_value_at(h, idx) kh_val(h, idx)

#define git_oidmap_insert(h, key, val, rval) do { \
	khiter_t __pos = kh_put(oid, h, key, &rval); \
	if (rval >= 0) { \
		if (rval == 0) kh_key(h, __pos) = key; \
		kh_val(h, __pos) = val; \
	} } while (0)

#define git_oidmap_foreach_value kh_foreach_value

#define git_oidmap_size(h) kh_size(h)

#endif
