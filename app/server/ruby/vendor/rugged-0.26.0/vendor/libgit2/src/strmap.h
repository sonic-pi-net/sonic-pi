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

int git_strmap_alloc(git_strmap **map);
void git_strmap_free(git_strmap *map);
void git_strmap_clear(git_strmap *map);

size_t git_strmap_num_entries(git_strmap *map);

size_t git_strmap_lookup_index(git_strmap *map, const char *key);
int git_strmap_valid_index(git_strmap *map, size_t idx);

int git_strmap_exists(git_strmap *map, const char *key);
int git_strmap_has_data(git_strmap *map, size_t idx);

const char *git_strmap_key(git_strmap *map, size_t idx);
void git_strmap_set_key_at(git_strmap *map, size_t idx, char *key);
void *git_strmap_value_at(git_strmap *map, size_t idx);
void git_strmap_set_value_at(git_strmap *map, size_t idx, void *value);
void git_strmap_delete_at(git_strmap *map, size_t idx);

int git_strmap_put(git_strmap *map, const char *key, int *err);
void git_strmap_insert(git_strmap *map, const char *key, void *value, int *rval);
void git_strmap_delete(git_strmap *map, const char *key);

#define git_strmap_foreach		kh_foreach
#define git_strmap_foreach_value	kh_foreach_value

#define git_strmap_begin		kh_begin
#define git_strmap_end		kh_end

int git_strmap_next(
	void **data,
	git_strmap_iter* iter,
	git_strmap *map);

#endif
