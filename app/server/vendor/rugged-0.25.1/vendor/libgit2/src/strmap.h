/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_strmap_h__
#define INCLUDE_strmap_h__

#include "common.h"

#define kmalloc git__malloc
#define kcalloc git__calloc
#define krealloc git__realloc
#define kreallocarray git__reallocarray
#define kfree git__free
#include "khash.h"

__KHASH_TYPE(str, const char *, void *)
typedef khash_t(str) git_strmap;
typedef khiter_t git_strmap_iter;

#define GIT__USE_STRMAP \
	__KHASH_IMPL(str, static kh_inline, const char *, void *, 1, kh_str_hash_func, kh_str_hash_equal)

#define git_strmap_alloc(hp) \
	((*(hp) = kh_init(str)) == NULL) ? giterr_set_oom(), -1 : 0

#define git_strmap_free(h)  kh_destroy(str, h), h = NULL
#define git_strmap_clear(h) kh_clear(str, h)

#define git_strmap_num_entries(h) kh_size(h)

#define git_strmap_lookup_index(h, k)  kh_get(str, h, k)
#define git_strmap_valid_index(h, idx) (idx != kh_end(h))

#define git_strmap_exists(h, k) (kh_get(str, h, k) != kh_end(h))
#define git_strmap_has_data(h, idx) kh_exist(h, idx)

#define git_strmap_key(h, idx)             kh_key(h, idx)
#define git_strmap_value_at(h, idx)        kh_val(h, idx)
#define git_strmap_set_value_at(h, idx, v) kh_val(h, idx) = v
#define git_strmap_delete_at(h, idx)       kh_del(str, h, idx)

#define git_strmap_insert(h, key, val, rval) do { \
	khiter_t __pos = kh_put(str, h, key, &rval); \
	if (rval >= 0) { \
		if (rval == 0) kh_key(h, __pos) = key; \
		kh_val(h, __pos) = val; \
	} } while (0)

#define git_strmap_insert2(h, key, val, oldv, rval) do { \
	khiter_t __pos = kh_put(str, h, key, &rval); \
	if (rval >= 0) { \
		if (rval == 0) { \
			oldv = kh_val(h, __pos); \
			kh_key(h, __pos) = key; \
		} else { oldv = NULL; } \
		kh_val(h, __pos) = val; \
	} } while (0)

#define git_strmap_delete(h, key) do { \
	khiter_t __pos = git_strmap_lookup_index(h, key); \
	if (git_strmap_valid_index(h, __pos)) \
		git_strmap_delete_at(h, __pos); } while (0)

#define git_strmap_foreach		kh_foreach
#define git_strmap_foreach_value	kh_foreach_value

#define git_strmap_begin		kh_begin
#define git_strmap_end		kh_end

int git_strmap_next(
	void **data,
	git_strmap_iter* iter,
	git_strmap *map);

#endif
