/*
 * Copyright (C) 2012 the libgit2 contributors
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_offmap_h__
#define INCLUDE_offmap_h__

#include "common.h"
#include "git2/types.h"

#define kmalloc git__malloc
#define kcalloc git__calloc
#define krealloc git__realloc
#define kreallocarray git__reallocarray
#define kfree git__free
#include "khash.h"

__KHASH_TYPE(off, git_off_t, void *)
typedef khash_t(off) git_offmap;

#define GIT__USE_OFFMAP \
	__KHASH_IMPL(off, static kh_inline, git_off_t, void *, 1, kh_int64_hash_func, kh_int64_hash_equal)

#define git_offmap_alloc()  kh_init(off)
#define git_offmap_free(h)  kh_destroy(off, h), h = NULL
#define git_offmap_clear(h) kh_clear(off, h)

#define git_offmap_num_entries(h) kh_size(h)

#define git_offmap_lookup_index(h, k)  kh_get(off, h, k)
#define git_offmap_valid_index(h, idx) (idx != kh_end(h))

#define git_offmap_exists(h, k) (kh_get(off, h, k) != kh_end(h))

#define git_offmap_value_at(h, idx)        kh_val(h, idx)
#define git_offmap_set_value_at(h, idx, v) kh_val(h, idx) = v
#define git_offmap_delete_at(h, idx)       kh_del(off, h, idx)

#define git_offmap_insert(h, key, val, rval) do { \
	khiter_t __pos = kh_put(off, h, key, &rval); \
	if (rval >= 0) { \
		if (rval == 0) kh_key(h, __pos) = key; \
		kh_val(h, __pos) = val; \
	} } while (0)

#define git_offmap_insert2(h, key, val, oldv, rval) do { \
	khiter_t __pos = kh_put(off, h, key, &rval); \
	if (rval >= 0) { \
		if (rval == 0) { \
			oldv = kh_val(h, __pos); \
			kh_key(h, __pos) = key; \
		} else { oldv = NULL; } \
		kh_val(h, __pos) = val; \
	} } while (0)

#define git_offmap_delete(h, key) do { \
	khiter_t __pos = git_offmap_lookup_index(h, key); \
	if (git_offmap_valid_index(h, __pos)) \
		git_offmap_delete_at(h, __pos); } while (0)

#define git_offmap_foreach		kh_foreach
#define git_offmap_foreach_value	kh_foreach_value

#endif
