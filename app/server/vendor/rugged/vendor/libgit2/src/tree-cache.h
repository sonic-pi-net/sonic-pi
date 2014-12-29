/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_tree_cache_h__
#define INCLUDE_tree_cache_h__

#include "common.h"
#include "git2/oid.h"

struct git_tree_cache {
	struct git_tree_cache *parent;
	struct git_tree_cache **children;
	size_t children_count;

	ssize_t entries;
	git_oid oid;
	size_t namelen;
	char name[GIT_FLEX_ARRAY];
};

typedef struct git_tree_cache git_tree_cache;

int git_tree_cache_read(git_tree_cache **tree, const char *buffer, size_t buffer_size);
void git_tree_cache_invalidate_path(git_tree_cache *tree, const char *path);
const git_tree_cache *git_tree_cache_get(const git_tree_cache *tree, const char *path);
void git_tree_cache_free(git_tree_cache *tree);

#endif
