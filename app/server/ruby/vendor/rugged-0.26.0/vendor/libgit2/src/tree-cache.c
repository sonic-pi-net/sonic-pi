/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "tree-cache.h"

#include "pool.h"
#include "tree.h"

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

	tree->entry_count = -1;

	while (ptr != NULL) {
		end = strchr(ptr, '/');

		if (end == NULL) /* End of path */
			break;

		tree = find_child(tree, ptr, end);
		if (tree == NULL) /* We don't have that tree */
			return;

		tree->entry_count = -1;
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
			      const char **buffer_in, const char *buffer_end,
			      git_pool *pool)
{
	git_tree_cache *tree = NULL;
	const char *name_start, *buffer;
	int count;

	buffer = name_start = *buffer_in;

	if ((buffer = memchr(buffer, '\0', buffer_end - buffer)) == NULL)
		goto corrupted;

	if (++buffer >= buffer_end)
		goto corrupted;

	if (git_tree_cache_new(&tree, name_start, pool) < 0)
		return -1;

	/* Blank-terminated ASCII decimal number of entries in this tree */
	if (git__strtol32(&count, buffer, &buffer, 10) < 0)
		goto corrupted;

	tree->entry_count = count;

	if (*buffer != ' ' || ++buffer >= buffer_end)
		goto corrupted;

	 /* Number of children of the tree, newline-terminated */
	if (git__strtol32(&count, buffer, &buffer, 10) < 0 || count < 0)
		goto corrupted;

	tree->children_count = count;

	if (*buffer != '\n' || ++buffer > buffer_end)
		goto corrupted;

	/* The SHA1 is only there if it's not invalidated */
	if (tree->entry_count >= 0) {
		/* 160-bit SHA-1 for this tree and it's children */
		if (buffer + GIT_OID_RAWSZ > buffer_end)
			goto corrupted;

		git_oid_fromraw(&tree->oid, (const unsigned char *)buffer);
		buffer += GIT_OID_RAWSZ;
	}

	/* Parse children: */
	if (tree->children_count > 0) {
		unsigned int i;

		tree->children = git_pool_malloc(pool, tree->children_count * sizeof(git_tree_cache *));
		GITERR_CHECK_ALLOC(tree->children);

		memset(tree->children, 0x0, tree->children_count * sizeof(git_tree_cache *));

		for (i = 0; i < tree->children_count; ++i) {
			if (read_tree_internal(&tree->children[i], &buffer, buffer_end, pool) < 0)
				goto corrupted;
		}
	}

	*buffer_in = buffer;
	*out = tree;
	return 0;

 corrupted:
	giterr_set(GITERR_INDEX, "corrupted TREE extension in index");
	return -1;
}

int git_tree_cache_read(git_tree_cache **tree, const char *buffer, size_t buffer_size, git_pool *pool)
{
	const char *buffer_end = buffer + buffer_size;

	if (read_tree_internal(tree, &buffer, buffer_end, pool) < 0)
		return -1;

	if (buffer < buffer_end) {
		giterr_set(GITERR_INDEX, "corrupted TREE extension in index (unexpected trailing data)");
		return -1;
	}

	return 0;
}

static int read_tree_recursive(git_tree_cache *cache, const git_tree *tree, git_pool *pool)
{
	git_repository *repo;
	size_t i, j, nentries, ntrees;
	int error;

	repo = git_tree_owner(tree);

	git_oid_cpy(&cache->oid, git_tree_id(tree));
	nentries = git_tree_entrycount(tree);

	/*
	 * We make sure we know how many trees we need to allocate for
	 * so we don't have to realloc and change the pointers for the
	 * parents.
	 */
	ntrees = 0;
	for (i = 0; i < nentries; i++) {
		const git_tree_entry *entry;

		entry = git_tree_entry_byindex(tree, i);
		if (git_tree_entry_filemode(entry) == GIT_FILEMODE_TREE)
			ntrees++;
	}

	cache->children_count = ntrees;
	cache->children = git_pool_mallocz(pool, ntrees * sizeof(git_tree_cache *));
	GITERR_CHECK_ALLOC(cache->children);

	j = 0;
	for (i = 0; i < nentries; i++) {
		const git_tree_entry *entry;
		git_tree *subtree;

		entry = git_tree_entry_byindex(tree, i);
		if (git_tree_entry_filemode(entry) != GIT_FILEMODE_TREE) {
			cache->entry_count++;
			continue;
		}

		if ((error = git_tree_cache_new(&cache->children[j], git_tree_entry_name(entry), pool)) < 0)
			return error;

		if ((error = git_tree_lookup(&subtree, repo, git_tree_entry_id(entry))) < 0)
			return error;

		error = read_tree_recursive(cache->children[j], subtree, pool);
		git_tree_free(subtree);
		cache->entry_count += cache->children[j]->entry_count;
		j++;

		if (error < 0)
			return error;
	}

	return 0;
}

int git_tree_cache_read_tree(git_tree_cache **out, const git_tree *tree, git_pool *pool)
{
	int error;
	git_tree_cache *cache;

	if ((error = git_tree_cache_new(&cache, "", pool)) < 0)
		return error;

	if ((error = read_tree_recursive(cache, tree, pool)) < 0)
		return error;

	*out = cache;
	return 0;
}

int git_tree_cache_new(git_tree_cache **out, const char *name, git_pool *pool)
{
	size_t name_len;
	git_tree_cache *tree;

	name_len = strlen(name);
	tree = git_pool_malloc(pool, sizeof(git_tree_cache) + name_len + 1);
	GITERR_CHECK_ALLOC(tree);

	memset(tree, 0x0, sizeof(git_tree_cache));
	/* NUL-terminated tree name */
	tree->namelen = name_len;
	memcpy(tree->name, name, name_len);
	tree->name[name_len] = '\0';

	*out = tree;
	return 0;
}

static void write_tree(git_buf *out, git_tree_cache *tree)
{
	size_t i;

	git_buf_printf(out, "%s%c%"PRIdZ" %"PRIuZ"\n", tree->name, 0, tree->entry_count, tree->children_count);

	if (tree->entry_count != -1)
		git_buf_put(out, (const char *) &tree->oid, GIT_OID_RAWSZ);

	for (i = 0; i < tree->children_count; i++)
		write_tree(out, tree->children[i]);
}

int git_tree_cache_write(git_buf *out, git_tree_cache *tree)
{
	write_tree(out, tree);

	return git_buf_oom(out) ? -1 : 0;
}
