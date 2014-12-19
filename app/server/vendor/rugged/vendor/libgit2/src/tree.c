/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "commit.h"
#include "tree.h"
#include "git2/repository.h"
#include "git2/object.h"
#include "fileops.h"
#include "tree-cache.h"
#include "index.h"

#define DEFAULT_TREE_SIZE 16
#define MAX_FILEMODE_BYTES 6

GIT__USE_STRMAP;

static bool valid_filemode(const int filemode)
{
	return (filemode == GIT_FILEMODE_TREE
		|| filemode == GIT_FILEMODE_BLOB
		|| filemode == GIT_FILEMODE_BLOB_EXECUTABLE
		|| filemode == GIT_FILEMODE_LINK
		|| filemode == GIT_FILEMODE_COMMIT);
}

GIT_INLINE(git_filemode_t) normalize_filemode(git_filemode_t filemode)
{
	/* Tree bits set, but it's not a commit */
	if (GIT_MODE_TYPE(filemode) == GIT_FILEMODE_TREE)
		return GIT_FILEMODE_TREE;

	/* If any of the x bits are set */
	if (GIT_PERMS_IS_EXEC(filemode))
		return GIT_FILEMODE_BLOB_EXECUTABLE;

	/* 16XXXX means commit */
	if (GIT_MODE_TYPE(filemode) == GIT_FILEMODE_COMMIT)
		return GIT_FILEMODE_COMMIT;

	/* 12XXXX means commit */
	if (GIT_MODE_TYPE(filemode) == GIT_FILEMODE_LINK)
		return GIT_FILEMODE_LINK;

	/* Otherwise, return a blob */
	return GIT_FILEMODE_BLOB;
}

static int valid_entry_name(git_repository *repo, const char *filename)
{
	return *filename != '\0' &&
		git_path_isvalid(repo, filename,
		GIT_PATH_REJECT_TRAVERSAL | GIT_PATH_REJECT_DOT_GIT | GIT_PATH_REJECT_SLASH);
}

static int entry_sort_cmp(const void *a, const void *b)
{
	const git_tree_entry *e1 = (const git_tree_entry *)a;
	const git_tree_entry *e2 = (const git_tree_entry *)b;

	return git_path_cmp(
		e1->filename, e1->filename_len, git_tree_entry__is_tree(e1),
		e2->filename, e2->filename_len, git_tree_entry__is_tree(e2),
		git__strncmp);
}

int git_tree_entry_cmp(const git_tree_entry *e1, const git_tree_entry *e2)
{
	return entry_sort_cmp(e1, e2);
}

int git_tree_entry_icmp(const git_tree_entry *e1, const git_tree_entry *e2)
{
	return git_path_cmp(
		e1->filename, e1->filename_len, git_tree_entry__is_tree(e1),
		e2->filename, e2->filename_len, git_tree_entry__is_tree(e2),
		git__strncasecmp);
}

static git_tree_entry *alloc_entry(const char *filename)
{
	git_tree_entry *entry = NULL;
	size_t filename_len = strlen(filename);

	entry = git__malloc(sizeof(git_tree_entry) + filename_len + 1);
	if (!entry)
		return NULL;

	memset(entry, 0x0, sizeof(git_tree_entry));
	memcpy(entry->filename, filename, filename_len);
	entry->filename[filename_len] = 0;
	entry->filename_len = filename_len;

	return entry;
}

struct tree_key_search {
	const char *filename;
	size_t filename_len;
};

static int homing_search_cmp(const void *key, const void *array_member)
{
	const struct tree_key_search *ksearch = key;
	const git_tree_entry *entry = array_member;

	const size_t len1 = ksearch->filename_len;
	const size_t len2 = entry->filename_len;

	return memcmp(
		ksearch->filename,
		entry->filename,
		len1 < len2 ? len1 : len2
	);
}

/*
 * Search for an entry in a given tree.
 *
 * Note that this search is performed in two steps because
 * of the way tree entries are sorted internally in git:
 *
 * Entries in a tree are not sorted alphabetically; two entries
 * with the same root prefix will have different positions
 * depending on whether they are folders (subtrees) or normal files.
 *
 * Consequently, it is not possible to find an entry on the tree
 * with a binary search if you don't know whether the filename
 * you're looking for is a folder or a normal file.
 *
 * To work around this, we first perform a homing binary search
 * on the tree, using the minimal length root prefix of our filename.
 * Once the comparisons for this homing search start becoming
 * ambiguous because of folder vs file sorting, we look linearly
 * around the area for our target file.
 */
static int tree_key_search(
	size_t *at_pos, git_vector *entries, const char *filename, size_t filename_len)
{
	struct tree_key_search ksearch;
	const git_tree_entry *entry;
	size_t homing, i;

	ksearch.filename = filename;
	ksearch.filename_len = filename_len;

	/* Initial homing search; find an entry on the tree with
	 * the same prefix as the filename we're looking for */
	if (git_vector_bsearch2(&homing, entries, &homing_search_cmp, &ksearch) < 0)
		return GIT_ENOTFOUND; /* just a signal error; not passed back to user */

	/* We found a common prefix. Look forward as long as
	 * there are entries that share the common prefix */
	for (i = homing; i < entries->length; ++i) {
		entry = entries->contents[i];

		if (homing_search_cmp(&ksearch, entry) < 0)
			break;

		if (entry->filename_len == filename_len &&
			memcmp(filename, entry->filename, filename_len) == 0) {
			if (at_pos)
				*at_pos = i;

			return 0;
		}
	}

	/* If we haven't found our filename yet, look backwards
	 * too as long as we have entries with the same prefix */
	if (homing > 0) {
		i = homing - 1;

		do {
			entry = entries->contents[i];

			if (homing_search_cmp(&ksearch, entry) > 0)
				break;

			if (entry->filename_len == filename_len &&
				memcmp(filename, entry->filename, filename_len) == 0) {
				if (at_pos)
					*at_pos = i;

				return 0;
			}
		} while (i-- > 0);
	}

	/* The filename doesn't exist at all */
	return GIT_ENOTFOUND;
}

void git_tree_entry_free(git_tree_entry *entry)
{
	if (entry == NULL)
		return;

	git__free(entry);
}

int git_tree_entry_dup(git_tree_entry **dest, const git_tree_entry *source)
{
	size_t total_size;
	git_tree_entry *copy;

	assert(source);

	total_size = sizeof(git_tree_entry) + source->filename_len + 1;

	copy = git__malloc(total_size);
	GITERR_CHECK_ALLOC(copy);

	memcpy(copy, source, total_size);

	*dest = copy;
	return 0;
}

void git_tree__free(void *_tree)
{
	git_tree *tree = _tree;
	size_t i;
	git_tree_entry *e;

	git_vector_foreach(&tree->entries, i, e)
		git_tree_entry_free(e);

	git_vector_free(&tree->entries);
	git__free(tree);
}

git_filemode_t git_tree_entry_filemode(const git_tree_entry *entry)
{
	return normalize_filemode(entry->attr);
}

git_filemode_t git_tree_entry_filemode_raw(const git_tree_entry *entry)
{
	return entry->attr;
}

const char *git_tree_entry_name(const git_tree_entry *entry)
{
	assert(entry);
	return entry->filename;
}

const git_oid *git_tree_entry_id(const git_tree_entry *entry)
{
	assert(entry);
	return &entry->oid;
}

git_otype git_tree_entry_type(const git_tree_entry *entry)
{
	assert(entry);

	if (S_ISGITLINK(entry->attr))
		return GIT_OBJ_COMMIT;
	else if (S_ISDIR(entry->attr))
		return GIT_OBJ_TREE;
	else
		return GIT_OBJ_BLOB;
}

int git_tree_entry_to_object(
	git_object **object_out,
	git_repository *repo,
	const git_tree_entry *entry)
{
	assert(entry && object_out);
	return git_object_lookup(object_out, repo, &entry->oid, GIT_OBJ_ANY);
}

static const git_tree_entry *entry_fromname(
	const git_tree *tree, const char *name, size_t name_len)
{
	size_t idx;

	/* be safe when we cast away constness - i.e. don't trigger a sort */
	assert(git_vector_is_sorted(&tree->entries));

	if (tree_key_search(&idx, (git_vector *)&tree->entries, name, name_len) < 0)
		return NULL;

	return git_vector_get(&tree->entries, idx);
}

const git_tree_entry *git_tree_entry_byname(
	const git_tree *tree, const char *filename)
{
	assert(tree && filename);
	return entry_fromname(tree, filename, strlen(filename));
}

const git_tree_entry *git_tree_entry_byindex(
	const git_tree *tree, size_t idx)
{
	assert(tree);
	return git_vector_get(&tree->entries, idx);
}

const git_tree_entry *git_tree_entry_byid(
	const git_tree *tree, const git_oid *id)
{
	size_t i;
	const git_tree_entry *e;

	assert(tree);

	git_vector_foreach(&tree->entries, i, e) {
		if (memcmp(&e->oid.id, &id->id, sizeof(id->id)) == 0)
			return e;
	}

	return NULL;
}

int git_tree__prefix_position(const git_tree *tree, const char *path)
{
	const git_vector *entries = &tree->entries;
	struct tree_key_search ksearch;
	size_t at_pos;

	if (!path)
		return 0;

	ksearch.filename = path;
	ksearch.filename_len = strlen(path);

	/* be safe when we cast away constness - i.e. don't trigger a sort */
	assert(git_vector_is_sorted(&tree->entries));

	/* Find tree entry with appropriate prefix */
	git_vector_bsearch2(
		&at_pos, (git_vector *)entries, &homing_search_cmp, &ksearch);

	for (; at_pos < entries->length; ++at_pos) {
		const git_tree_entry *entry = entries->contents[at_pos];
		if (homing_search_cmp(&ksearch, entry) < 0)
			break;
	}

	for (; at_pos > 0; --at_pos) {
		const git_tree_entry *entry = entries->contents[at_pos - 1];
		if (homing_search_cmp(&ksearch, entry) > 0)
			break;
	}

	return (int)at_pos;
}

size_t git_tree_entrycount(const git_tree *tree)
{
	assert(tree);
	return tree->entries.length;
}

unsigned int git_treebuilder_entrycount(git_treebuilder *bld)
{
	assert(bld);

	return git_strmap_num_entries(bld->map);
}

static int tree_error(const char *str, const char *path)
{
	if (path)
		giterr_set(GITERR_TREE, "%s - %s", str, path);
	else
		giterr_set(GITERR_TREE, "%s", str);
	return -1;
}

int git_tree__parse(void *_tree, git_odb_object *odb_obj)
{
	git_tree *tree = _tree;
	const char *buffer = git_odb_object_data(odb_obj);
	const char *buffer_end = buffer + git_odb_object_size(odb_obj);

	if (git_vector_init(&tree->entries, DEFAULT_TREE_SIZE, entry_sort_cmp) < 0)
		return -1;

	while (buffer < buffer_end) {
		git_tree_entry *entry;
		int attr;

		if (git__strtol32(&attr, buffer, &buffer, 8) < 0 || !buffer)
			return tree_error("Failed to parse tree. Can't parse filemode", NULL);

		if (*buffer++ != ' ')
			return tree_error("Failed to parse tree. Object is corrupted", NULL);

		if (memchr(buffer, 0, buffer_end - buffer) == NULL)
			return tree_error("Failed to parse tree. Object is corrupted", NULL);

		/** Allocate the entry and store it in the entries vector */
		{
			entry = alloc_entry(buffer);
			GITERR_CHECK_ALLOC(entry);

			if (git_vector_insert(&tree->entries, entry) < 0) {
				git__free(entry);
				return -1;
			}

			entry->attr = attr;
		}

		while (buffer < buffer_end && *buffer != 0)
			buffer++;

		buffer++;

		git_oid_fromraw(&entry->oid, (const unsigned char *)buffer);
		buffer += GIT_OID_RAWSZ;
	}

	git_vector_sort(&tree->entries);

	return 0;
}

static size_t find_next_dir(const char *dirname, git_index *index, size_t start)
{
	size_t dirlen, i, entries = git_index_entrycount(index);

	dirlen = strlen(dirname);
	for (i = start; i < entries; ++i) {
		const git_index_entry *entry = git_index_get_byindex(index, i);
		if (strlen(entry->path) < dirlen ||
		    memcmp(entry->path, dirname, dirlen) ||
			(dirlen > 0 && entry->path[dirlen] != '/')) {
			break;
		}
	}

	return i;
}

static int append_entry(
	git_treebuilder *bld,
	const char *filename,
	const git_oid *id,
	git_filemode_t filemode)
{
	git_tree_entry *entry;
	int error = 0;

	if (!valid_entry_name(bld->repo, filename))
		return tree_error("Failed to insert entry. Invalid name for a tree entry", filename);

	entry = alloc_entry(filename);
	GITERR_CHECK_ALLOC(entry);

	git_oid_cpy(&entry->oid, id);
	entry->attr = (uint16_t)filemode;

	git_strmap_insert(bld->map, entry->filename, entry, error);
	if (error < 0) {
		git_tree_entry_free(entry);
		giterr_set(GITERR_TREE, "failed to append entry %s to the tree builder", filename);
		return -1;
	}

	return 0;
}

static int write_tree(
	git_oid *oid,
	git_repository *repo,
	git_index *index,
	const char *dirname,
	size_t start)
{
	git_treebuilder *bld = NULL;
	size_t i, entries = git_index_entrycount(index);
	int error;
	size_t dirname_len = strlen(dirname);
	const git_tree_cache *cache;

	cache = git_tree_cache_get(index->tree, dirname);
	if (cache != NULL && cache->entry_count >= 0){
		git_oid_cpy(oid, &cache->oid);
		return (int)find_next_dir(dirname, index, start);
	}

	if ((error = git_treebuilder_create(&bld, repo, NULL)) < 0 || bld == NULL)
		return -1;

	/*
	 * This loop is unfortunate, but necessary. The index doesn't have
	 * any directores, so we need to handle that manually, and we
	 * need to keep track of the current position.
	 */
	for (i = start; i < entries; ++i) {
		const git_index_entry *entry = git_index_get_byindex(index, i);
		const char *filename, *next_slash;

	/*
	 * If we've left our (sub)tree, exit the loop and return. The
	 * first check is an early out (and security for the
	 * third). The second check is a simple prefix comparison. The
	 * third check catches situations where there is a directory
	 * win32/sys and a file win32mmap.c. Without it, the following
	 * code believes there is a file win32/mmap.c
	 */
		if (strlen(entry->path) < dirname_len ||
		    memcmp(entry->path, dirname, dirname_len) ||
		    (dirname_len > 0 && entry->path[dirname_len] != '/')) {
			break;
		}

		filename = entry->path + dirname_len;
		if (*filename == '/')
			filename++;
		next_slash = strchr(filename, '/');
		if (next_slash) {
			git_oid sub_oid;
			int written;
			char *subdir, *last_comp;

			subdir = git__strndup(entry->path, next_slash - entry->path);
			GITERR_CHECK_ALLOC(subdir);

			/* Write out the subtree */
			written = write_tree(&sub_oid, repo, index, subdir, i);
			if (written < 0) {
				git__free(subdir);
				goto on_error;
			} else {
				i = written - 1; /* -1 because of the loop increment */
			}

			/*
			 * We need to figure out what we want toinsert
			 * into this tree. If we're traversing
			 * deps/zlib/, then we only want to write
			 * 'zlib' into the tree.
			 */
			last_comp = strrchr(subdir, '/');
			if (last_comp) {
				last_comp++; /* Get rid of the '/' */
			} else {
				last_comp = subdir;
			}

			error = append_entry(bld, last_comp, &sub_oid, S_IFDIR);
			git__free(subdir);
			if (error < 0)
				goto on_error;
		} else {
			error = append_entry(bld, filename, &entry->id, entry->mode);
			if (error < 0)
				goto on_error;
		}
	}

	if (git_treebuilder_write(oid, bld) < 0)
		goto on_error;

	git_treebuilder_free(bld);
	return (int)i;

on_error:
	git_treebuilder_free(bld);
	return -1;
}

int git_tree__write_index(
	git_oid *oid, git_index *index, git_repository *repo)
{
	int ret;
	git_tree *tree;
	bool old_ignore_case = false;

	assert(oid && index && repo);

	if (git_index_has_conflicts(index)) {
		giterr_set(GITERR_INDEX,
			"Cannot create a tree from a not fully merged index.");
		return GIT_EUNMERGED;
	}

	if (index->tree != NULL && index->tree->entry_count >= 0) {
		git_oid_cpy(oid, &index->tree->oid);
		return 0;
	}

	/* The tree cache didn't help us; we'll have to write
	 * out a tree. If the index is ignore_case, we must
	 * make it case-sensitive for the duration of the tree-write
	 * operation. */

	if (index->ignore_case) {
		old_ignore_case = true;
		git_index__set_ignore_case(index, false);
	}

	ret = write_tree(oid, repo, index, "", 0);

	if (old_ignore_case)
		git_index__set_ignore_case(index, true);

	index->tree = NULL;

	if (ret < 0)
		return ret;

	git_pool_clear(&index->tree_pool);

	if ((ret = git_tree_lookup(&tree, repo, oid)) < 0)
		return ret;

	/* Read the tree cache into the index */
	ret = git_tree_cache_read_tree(&index->tree, tree, &index->tree_pool);
	git_tree_free(tree);

	return ret;
}

int git_treebuilder_create(
	git_treebuilder **builder_p,
	git_repository *repo,
	const git_tree *source)
{
	git_treebuilder *bld;
	size_t i;

	assert(builder_p && repo);

	bld = git__calloc(1, sizeof(git_treebuilder));
	GITERR_CHECK_ALLOC(bld);

	bld->repo = repo;

	if (git_strmap_alloc(&bld->map) < 0) {
		git__free(bld);
		return -1;
	}

	if (source != NULL) {
		git_tree_entry *entry_src;

		git_vector_foreach(&source->entries, i, entry_src) {
			if (append_entry(
				bld, entry_src->filename,
				&entry_src->oid,
				entry_src->attr) < 0)
				goto on_error;
		}
	}

	*builder_p = bld;
	return 0;

on_error:
	git_treebuilder_free(bld);
	return -1;
}

int git_treebuilder_insert(
	const git_tree_entry **entry_out,
	git_treebuilder *bld,
	const char *filename,
	const git_oid *id,
	git_filemode_t filemode)
{
	git_tree_entry *entry;
	int error;
	git_strmap_iter pos;

	assert(bld && id && filename);

	if (!valid_filemode(filemode))
		return tree_error("Failed to insert entry. Invalid filemode for file", filename);

	if (!valid_entry_name(bld->repo, filename))
		return tree_error("Failed to insert entry. Invalid name for a tree entry", filename);

	pos = git_strmap_lookup_index(bld->map, filename);
	if (git_strmap_valid_index(bld->map, pos)) {
		entry = git_strmap_value_at(bld->map, pos);
	} else {
		entry = alloc_entry(filename);
		GITERR_CHECK_ALLOC(entry);

		git_strmap_insert(bld->map, entry->filename, entry, error);

		if (error < 0) {
			git_tree_entry_free(entry);
			giterr_set(GITERR_TREE, "failed to insert %s", filename);
			return -1;
		}
	}

	git_oid_cpy(&entry->oid, id);
	entry->attr = filemode;

	if (entry_out)
		*entry_out = entry;

	return 0;
}

static git_tree_entry *treebuilder_get(git_treebuilder *bld, const char *filename)
{
	git_tree_entry *entry = NULL;
	git_strmap_iter pos;

	assert(bld && filename);

	pos = git_strmap_lookup_index(bld->map, filename);
	if (git_strmap_valid_index(bld->map, pos))
		entry = git_strmap_value_at(bld->map, pos);

	return entry;
}

const git_tree_entry *git_treebuilder_get(git_treebuilder *bld, const char *filename)
{
	return treebuilder_get(bld, filename);
}

int git_treebuilder_remove(git_treebuilder *bld, const char *filename)
{
	git_tree_entry *entry = treebuilder_get(bld, filename);

	if (entry == NULL)
		return tree_error("Failed to remove entry. File isn't in the tree", filename);

	git_strmap_delete(bld->map, filename);
	git_tree_entry_free(entry);

	return 0;
}

int git_treebuilder_write(git_oid *oid, git_treebuilder *bld)
{
	int error = 0;
	size_t i, entrycount;
	git_buf tree = GIT_BUF_INIT;
	git_odb *odb;
	git_tree_entry *entry;
	git_vector entries;

	assert(bld);

	entrycount = git_strmap_num_entries(bld->map);
	if (git_vector_init(&entries, entrycount, entry_sort_cmp) < 0)
		return -1;

	git_strmap_foreach_value(bld->map, entry, {
		if (git_vector_insert(&entries, entry) < 0)
			return -1;
	});

	git_vector_sort(&entries);

	/* Grow the buffer beforehand to an estimated size */
	error = git_buf_grow(&tree, entrycount * 72);

	for (i = 0; i < entries.length && !error; ++i) {
		git_tree_entry *entry = git_vector_get(&entries, i);

		git_buf_printf(&tree, "%o ", entry->attr);
		git_buf_put(&tree, entry->filename, entry->filename_len + 1);
		git_buf_put(&tree, (char *)entry->oid.id, GIT_OID_RAWSZ);

		if (git_buf_oom(&tree))
			error = -1;
	}

	git_vector_free(&entries);

	if (!error &&
		!(error = git_repository_odb__weakptr(&odb, bld->repo)))
		error = git_odb_write(oid, odb, tree.ptr, tree.size, GIT_OBJ_TREE);

	git_buf_free(&tree);
	return error;
}

void git_treebuilder_filter(
	git_treebuilder *bld,
	git_treebuilder_filter_cb filter,
	void *payload)
{
	const char *filename;
	git_tree_entry *entry;

	assert(bld && filter);

	git_strmap_foreach(bld->map, filename, entry, {
			if (filter(entry, payload)) {
				git_strmap_delete(bld->map, filename);
				git_tree_entry_free(entry);
			}
	});
}

void git_treebuilder_clear(git_treebuilder *bld)
{
	git_tree_entry *e;

	assert(bld);

	git_strmap_foreach_value(bld->map, e, git_tree_entry_free(e));
	git_strmap_clear(bld->map);
}

void git_treebuilder_free(git_treebuilder *bld)
{
	if (bld == NULL)
		return;

	git_treebuilder_clear(bld);
	git_strmap_free(bld->map);
	git__free(bld);
}

static size_t subpath_len(const char *path)
{
	const char *slash_pos = strchr(path, '/');
	if (slash_pos == NULL)
		return strlen(path);

	return slash_pos - path;
}

int git_tree_entry_bypath(
	git_tree_entry **entry_out,
	const git_tree *root,
	const char *path)
{
	int error = 0;
	git_tree *subtree;
	const git_tree_entry *entry;
	size_t filename_len;

	/* Find how long is the current path component (i.e.
	 * the filename between two slashes */
	filename_len = subpath_len(path);

	if (filename_len == 0) {
		giterr_set(GITERR_TREE, "Invalid tree path given");
		return GIT_ENOTFOUND;
	}

	entry = entry_fromname(root, path, filename_len);

	if (entry == NULL) {
		giterr_set(GITERR_TREE,
			"The path '%s' does not exist in the given tree", path);
		return GIT_ENOTFOUND;
	}

	switch (path[filename_len]) {
	case '/':
		/* If there are more components in the path...
		 * then this entry *must* be a tree */
		if (!git_tree_entry__is_tree(entry)) {
			giterr_set(GITERR_TREE,
				"The path '%s' does not exist in the given tree", path);
			return GIT_ENOTFOUND;
		}

		/* If there's only a slash left in the path, we 
		 * return the current entry; otherwise, we keep
		 * walking down the path */
		if (path[filename_len + 1] != '\0')
			break;

	case '\0':
		/* If there are no more components in the path, return
		 * this entry */
		return git_tree_entry_dup(entry_out, entry);
	}

	if (git_tree_lookup(&subtree, root->object.repo, &entry->oid) < 0)
		return -1;

	error = git_tree_entry_bypath(
		entry_out,
		subtree,
		path + filename_len + 1
	);

	git_tree_free(subtree);
	return error;
}

static int tree_walk(
	const git_tree *tree,
	git_treewalk_cb callback,
	git_buf *path,
	void *payload,
	bool preorder)
{
	int error = 0;
	size_t i;
	const git_tree_entry *entry;

	git_vector_foreach(&tree->entries, i, entry) {
		if (preorder) {
			error = callback(path->ptr, entry, payload);
			if (error < 0) { /* negative value stops iteration */
				giterr_set_after_callback_function(error, "git_tree_walk");
				break;
			}
			if (error > 0) { /* positive value skips this entry */
				error = 0;
				continue;
			}
		}

		if (git_tree_entry__is_tree(entry)) {
			git_tree *subtree;
			size_t path_len = git_buf_len(path);

			error = git_tree_lookup(&subtree, tree->object.repo, &entry->oid);
			if (error < 0)
				break;

			/* append the next entry to the path */
			git_buf_puts(path, entry->filename);
			git_buf_putc(path, '/');

			if (git_buf_oom(path))
				error = -1;
			else
				error = tree_walk(subtree, callback, path, payload, preorder);

			git_tree_free(subtree);
			if (error != 0)
				break;

			git_buf_truncate(path, path_len);
		}

		if (!preorder) {
			error = callback(path->ptr, entry, payload);
			if (error < 0) { /* negative value stops iteration */
				giterr_set_after_callback_function(error, "git_tree_walk");
				break;
			}
			error = 0;
		}
	}

	return error;
}

int git_tree_walk(
	const git_tree *tree,
	git_treewalk_mode mode,
	git_treewalk_cb callback,
	void *payload)
{
	int error = 0;
	git_buf root_path = GIT_BUF_INIT;

	if (mode != GIT_TREEWALK_POST && mode != GIT_TREEWALK_PRE) {
		giterr_set(GITERR_INVALID, "Invalid walking mode for tree walk");
		return -1;
	}

	error = tree_walk(
		tree, callback, &root_path, payload, (mode == GIT_TREEWALK_PRE));

	git_buf_free(&root_path);

	return error;
}

