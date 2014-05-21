/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "tree-cache.h"

static git_tree_cache *find_child(
	const git_tree_cache *tree, const char *path, const char *end)
{
	size_t i, dirlen = end ? (size_t)(end - path) : strlen(path);

	for (i = 0; i < tree->children_count; ++i) {
		git_tree_cache *child = tree->children[i];

		if (child->namelen == dirlen && !memcmp(path, child->name, dirlen))
			return child;
	}

	return NULL;
}

void git_tree_cache_invalidate_path(git_tree_cache *tree, const char *path)
{
	const char *ptr = path, *end;

	if (tree == NULL)
		return;

	tree->entries = -1;

	while (ptr != NULL) {
		end = strchr(ptr, '/');

		if (end == NULL) /* End of path */
			break;

		tree = find_child(tree, ptr, end);
		if (tree == NULL) /* We don't have that tree */
			return;

		tree->entries = -1;
		ptr = end + 1;
	}
}

const git_tree_cache *git_tree_cache_get(const git_tree_cache *tree, const char *path)
{
	const char *ptr = path, *end;

	if (tree == NULL) {
		return NULL;
	}

	while (1) {
		end = strchr(ptr, '/');

		tree = find_child(tree, ptr, end);
		if (tree == NULL) /* Can't find it */
			return NULL;

		if (end == NULL || *end + 1 == '\0')
			return tree;

		ptr = end + 1;
	}
}

static int read_tree_internal(git_tree_cache **out,
		const char **buffer_in, const char *buffer_end, git_tree_cache *parent)
{
	git_tree_cache *tree = NULL;
	const char *name_start, *buffer;
	int count;
	size_t name_len;

	buffer = name_start = *buffer_in;

	if ((buffer = memchr(buffer, '\0', buffer_end - buffer)) == NULL)
		goto corrupted;

	if (++buffer >= buffer_end)
		goto corrupted;

	name_len = strlen(name_start);
	tree = git__malloc(sizeof(git_tree_cache) + name_len + 1);
	GITERR_CHECK_ALLOC(tree);

	memset(tree, 0x0, sizeof(git_tree_cache));
	tree->parent = parent;

	/* NUL-terminated tree name */
	tree->namelen = name_len;
	memcpy(tree->name, name_start, name_len);
	tree->name[name_len] = '\0';

	/* Blank-terminated ASCII decimal number of entries in this tree */
	if (git__strtol32(&count, buffer, &buffer, 10) < 0)
		goto corrupted;

	tree->entries = count;

	if (*buffer != ' ' || ++buffer >= buffer_end)
		goto corrupted;

	 /* Number of children of the tree, newline-terminated */
	if (git__strtol32(&count, buffer, &buffer, 10) < 0 || count < 0)
		goto corrupted;

	tree->children_count = count;

	if (*buffer != '\n' || ++buffer > buffer_end)
		goto corrupted;

	/* The SHA1 is only there if it's not invalidated */
	if (tree->entries >= 0) {
		/* 160-bit SHA-1 for this tree and it's children */
		if (buffer + GIT_OID_RAWSZ > buffer_end)
			goto corrupted;

		git_oid_fromraw(&tree->oid, (const unsigned char *)buffer);
		buffer += GIT_OID_RAWSZ;
	}

	/* Parse children: */
	if (tree->children_count > 0) {
		unsigned int i;

		tree->children = git__malloc(tree->children_count * sizeof(git_tree_cache *));
		GITERR_CHECK_ALLOC(tree->children);

		memset(tree->children, 0x0, tree->children_count * sizeof(git_tree_cache *));

		for (i = 0; i < tree->children_count; ++i) {
			if (read_tree_internal(&tree->children[i], &buffer, buffer_end, tree) < 0)
				goto corrupted;
		}
	}

	*buffer_in = buffer;
	*out = tree;
	return 0;

 corrupted:
	git_tree_cache_free(tree);
	giterr_set(GITERR_INDEX, "Corrupted TREE extension in index");
	return -1;
}

int git_tree_cache_read(git_tree_cache **tree, const char *buffer, size_t buffer_size)
{
	const char *buffer_end = buffer + buffer_size;

	if (read_tree_internal(tree, &buffer, buffer_end, NULL) < 0)
		return -1;

	if (buffer < buffer_end) {
		giterr_set(GITERR_INDEX, "Corrupted TREE extension in index (unexpected trailing data)");
		return -1;
	}

	return 0;
}

void git_tree_cache_free(git_tree_cache *tree)
{
	unsigned int i;

	if (tree == NULL)
		return;

	if (tree->children != NULL) {
		for (i = 0; i < tree->children_count; ++i)
			git_tree_cache_free(tree->children[i]);

		git__free(tree->children);
	}

	git__free(tree);
}
