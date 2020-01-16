/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_tree_h__
#define INCLUDE_tree_h__

#include "common.h"

#include "git2/tree.h"
#include "repository.h"
#include "odb.h"
#include "vector.h"
#include "strmap.h"
#include "pool.h"

struct git_tree_entry {
	uint16_t attr;
	uint16_t filename_len;
	const git_oid *oid;
	const char *filename;
};

struct git_tree {
	git_object object;
	git_odb_object *odb_obj;
	git_array_t(git_tree_entry) entries;
};

struct git_treebuilder {
	git_repository *repo;
	git_strmap *map;
};

GIT_INLINE(bool) git_tree_entry__is_tree(const struct git_tree_entry *e)
{
	return (S_ISDIR(e->attr) && !S_ISGITLINK(e->attr));
}

void git_tree__free(void *tree);
int git_tree__parse(void *tree, git_odb_object *obj);
int git_tree__parse_raw(void *_tree, const char *data, size_t size);

/**
 * Write a tree to the given repository
 */
int git_tree__write_index(
	git_oid *oid, git_index *index, git_repository *repo);

/**
 * Obsolete mode kept for compatibility reasons
 */
#define GIT_FILEMODE_BLOB_GROUP_WRITABLE 0100664

#endif
