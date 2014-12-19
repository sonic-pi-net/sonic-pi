/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_index_h__
#define INCLUDE_index_h__

#include "fileops.h"
#include "filebuf.h"
#include "vector.h"
#include "tree-cache.h"
#include "git2/odb.h"
#include "git2/index.h"

#define GIT_INDEX_FILE "index"
#define GIT_INDEX_FILE_MODE 0666

struct git_index {
	git_refcount rc;

	char *index_file_path;
	git_futils_filestamp stamp;

	git_vector entries;

	git_mutex  lock;    /* lock held while entries is being changed */
	git_vector deleted; /* deleted entries if readers > 0 */
	git_atomic readers; /* number of active iterators */

	unsigned int on_disk:1;
	unsigned int ignore_case:1;
	unsigned int distrust_filemode:1;
	unsigned int no_symlinks:1;

	git_tree_cache *tree;
	git_pool tree_pool;

	git_vector names;
	git_vector reuc;

	git_vector_cmp entries_cmp_path;
	git_vector_cmp entries_search;
	git_vector_cmp entries_search_path;
	git_vector_cmp reuc_search;
};

struct git_index_conflict_iterator {
	git_index *index;
	size_t cur;
};

extern void git_index_entry__init_from_stat(
	git_index_entry *entry, struct stat *st, bool trust_mode);

/* Index entry comparison functions for array sorting */
extern int git_index_entry_cmp(const void *a, const void *b);
extern int git_index_entry_icmp(const void *a, const void *b);

/* Index entry search functions for search using a search spec */
extern int git_index_entry_srch(const void *a, const void *b);
extern int git_index_entry_isrch(const void *a, const void *b);

/* Search index for `path`, returning GIT_ENOTFOUND if it does not exist
 * (but not setting an error message).
 *
 * `at_pos` is set to the position where it is or would be inserted.
 * Pass `path_len` as strlen of path or 0 to call strlen internally.
 */
extern int git_index__find_pos(
	size_t *at_pos, git_index *index, const char *path, size_t path_len, int stage);

extern void git_index__set_ignore_case(git_index *index, bool ignore_case);

extern unsigned int git_index__create_mode(unsigned int mode);

GIT_INLINE(const git_futils_filestamp *) git_index__filestamp(git_index *index)
{
   return &index->stamp;
}

extern int git_index__changed_relative_to(git_index *index, const git_futils_filestamp *fs);

/* Copy the current entries vector *and* increment the index refcount.
 * Call `git_index__release_snapshot` when done.
 */
extern int git_index_snapshot_new(git_vector *snap, git_index *index);
extern void git_index_snapshot_release(git_vector *snap, git_index *index);

/* Allow searching in a snapshot; entries must already be sorted! */
extern int git_index_snapshot_find(
	size_t *at_pos, git_vector *snap, git_vector_cmp entry_srch,
	const char *path, size_t path_len, int stage);


#endif
