/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_tree_h__
#define INCLUDE_tree_h__

#include "git2/tree.h"
#include "repository.h"
#include "odb.h"
#include "vector.h"
#include "strmap.h"

struct git_tree_entry {
	uint16_t attr;
	git_oid oid;
	size_t filename_len;
	char filename[1];
};

struct git_tree {
	git_object object;
	git_vector entries;
};

struct git_treebuilder {
	git_repository *repo;
	git_strmap *map;
};

GIT_INLINE(bool) git_tree_entry__is_tree(const struct git_tree_entry *e)
{
	return (S_ISDIR(e->attr) && !S_ISGITLINK(e->attr));
}

extern int git_tree_entry_icmp(const git_tree_entry *e1, const git_tree_entry *e2);

void git_tree__free(void *tree);
int git_tree__parse(void *tree, git_odb_object *obj);

/**
 * Lookup the first position in the tree with a given prefix.
 *
 * @param tree a previously loaded tree.
 * @param prefix the beginning of a path to find in the tree.
 * @return index of the first item at or after the given prefix.
 */
int git_tree__prefix_position(const git_tree *tree, const char *prefix);


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
