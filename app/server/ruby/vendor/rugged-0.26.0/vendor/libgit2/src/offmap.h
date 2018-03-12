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

git_offmap *git_offmap_alloc(void);
void git_offmap_free(git_offmap *map);
void git_offmap_clear(git_offmap *map);

size_t git_offmap_num_entries(git_offmap *map);

size_t git_offmap_lookup_index(git_offmap *map, const git_off_t key);
int git_offmap_valid_index(git_offmap *map, size_t idx);

int git_offmap_exists(git_offmap *map, const git_off_t key);

void *git_offmap_value_at(git_offmap *map, size_t idx);
void git_offmap_set_value_at(git_offmap *map, size_t idx, void *value);
void git_offmap_delete_at(git_offmap *map, size_t idx);

int git_offmap_put(git_offmap *map, const git_off_t key, int *err);
void git_offmap_insert(git_offmap *map, const git_off_t key, void *value, int *rval);
void git_offmap_delete(git_offmap *map, const git_off_t key);

#define git_offmap_foreach		kh_foreach
#define git_offmap_foreach_value	kh_foreach_value

#endif
