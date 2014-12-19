/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_iterator_h__
#define INCLUDE_iterator_h__

#include "common.h"
#include "git2/index.h"
#include "vector.h"
#include "buffer.h"

typedef struct git_iterator git_iterator;

typedef enum {
	GIT_ITERATOR_TYPE_EMPTY = 0,
	GIT_ITERATOR_TYPE_TREE = 1,
	GIT_ITERATOR_TYPE_INDEX = 2,
	GIT_ITERATOR_TYPE_WORKDIR = 3,
	GIT_ITERATOR_TYPE_FS = 4,
} git_iterator_type_t;

typedef enum {
	/** ignore case for entry sort order */
	GIT_ITERATOR_IGNORE_CASE = (1u << 0),
	/** force case sensitivity for entry sort order */
	GIT_ITERATOR_DONT_IGNORE_CASE = (1u << 1),
	/** return tree items in addition to blob items */
	GIT_ITERATOR_INCLUDE_TREES    = (1u << 2),
	/** don't flatten trees, requiring advance_into (implies INCLUDE_TREES) */
	GIT_ITERATOR_DONT_AUTOEXPAND  = (1u << 3),
	/** convert precomposed unicode to decomposed unicode */
	GIT_ITERATOR_PRECOMPOSE_UNICODE = (1u << 4),
} git_iterator_flag_t;

typedef struct {
	int (*current)(const git_index_entry **, git_iterator *);
	int (*advance)(const git_index_entry **, git_iterator *);
	int (*advance_into)(const git_index_entry **, git_iterator *);
	int (*seek)(git_iterator *, const char *prefix);
	int (*reset)(git_iterator *, const char *start, const char *end);
	int (*at_end)(git_iterator *);
	void (*free)(git_iterator *);
} git_iterator_callbacks;

struct git_iterator {
	git_iterator_type_t type;
	git_iterator_callbacks *cb;
	git_repository *repo;
	char *start;
	char *end;
	int (*prefixcomp)(const char *str, const char *prefix);
	size_t stat_calls;
	unsigned int flags;
};

extern int git_iterator_for_nothing(
	git_iterator **out,
	git_iterator_flag_t flags,
	const char *start,
	const char *end);

/* tree iterators will match the ignore_case value from the index of the
 * repository, unless you override with a non-zero flag value
 */
extern int git_iterator_for_tree(
	git_iterator **out,
	git_tree *tree,
	git_iterator_flag_t flags,
	const char *start,
	const char *end);

/* index iterators will take the ignore_case value from the index; the
 * ignore_case flags are not used
 */
extern int git_iterator_for_index(
	git_iterator **out,
	git_index *index,
	git_iterator_flag_t flags,
	const char *start,
	const char *end);

extern int git_iterator_for_workdir_ext(
	git_iterator **out,
	git_repository *repo,
	const char *repo_workdir,
	git_index *index,
	git_tree *tree,
	git_iterator_flag_t flags,
	const char *start,
	const char *end);

/* workdir iterators will match the ignore_case value from the index of the
 * repository, unless you override with a non-zero flag value
 */
GIT_INLINE(int) git_iterator_for_workdir(
	git_iterator **out,
	git_repository *repo,
	git_index *index,
	git_tree *tree,
	git_iterator_flag_t flags,
	const char *start,
	const char *end)
{
	return git_iterator_for_workdir_ext(out, repo, NULL, index, tree, flags, start, end);
}

/* for filesystem iterators, you have to explicitly pass in the ignore_case
 * behavior that you desire
 */
extern int git_iterator_for_filesystem(
	git_iterator **out,
	const char *root,
	git_iterator_flag_t flags,
	const char *start,
	const char *end);

extern void git_iterator_free(git_iterator *iter);

/* Return a git_index_entry structure for the current value the iterator
 * is looking at or NULL if the iterator is at the end.
 *
 * The entry may noy be fully populated.  Tree iterators will only have a
 * value mode, OID, and path.  Workdir iterators will not have an OID (but
 * you can use `git_iterator_current_oid()` to calculate it on demand).
 *
 * You do not need to free the entry.  It is still "owned" by the iterator.
 * Once you call `git_iterator_advance()` then the old entry is no longer
 * guaranteed to be valid - it may be freed or just overwritten in place.
 */
GIT_INLINE(int) git_iterator_current(
	const git_index_entry **entry, git_iterator *iter)
{
	return iter->cb->current(entry, iter);
}

/**
 * Advance to the next item for the iterator.
 *
 * If GIT_ITERATOR_INCLUDE_TREES is set, this may be a tree item.  If
 * GIT_ITERATOR_DONT_AUTOEXPAND is set, calling this again when on a tree
 * item will skip over all the items under that tree.
 */
GIT_INLINE(int) git_iterator_advance(
	const git_index_entry **entry, git_iterator *iter)
{
	return iter->cb->advance(entry, iter);
}

/**
 * Iterate into a tree item (when GIT_ITERATOR_DONT_AUTOEXPAND is set).
 *
 * git_iterator_advance() steps through all items being iterated over
 * (either with or without trees, depending on GIT_ITERATOR_INCLUDE_TREES),
 * but if GIT_ITERATOR_DONT_AUTOEXPAND is set, it will skip to the next
 * sibling of a tree instead of going to the first child of the tree.  In
 * that case, use this function to advance to the first child of the tree.
 *
 * If the current item is not a tree, this is a no-op.
 *
 * For filesystem and working directory iterators, a tree (i.e. directory)
 * can be empty.  In that case, this function returns GIT_ENOTFOUND and
 * does not advance.  That can't happen for tree and index iterators.
 */
GIT_INLINE(int) git_iterator_advance_into(
	const git_index_entry **entry, git_iterator *iter)
{
	return iter->cb->advance_into(entry, iter);
}

/**
 * Advance into a tree or skip over it if it is empty.
 *
 * Because `git_iterator_advance_into` may return GIT_ENOTFOUND if the
 * directory is empty (only with filesystem and working directory
 * iterators) and a common response is to just call `git_iterator_advance`
 * when that happens, this bundles the two into a single simple call.
 */
GIT_INLINE(int) git_iterator_advance_into_or_over(
	const git_index_entry **entry, git_iterator *iter)
{
	int error = iter->cb->advance_into(entry, iter);
	if (error == GIT_ENOTFOUND) {
		giterr_clear();
		error = iter->cb->advance(entry, iter);
	}
	return error;
}

/* Seek is currently unimplemented */
GIT_INLINE(int) git_iterator_seek(
	git_iterator *iter, const char *prefix)
{
	return iter->cb->seek(iter, prefix);
}

/**
 * Go back to the start of the iteration.
 *
 * This resets the iterator to the start of the iteration.  It also allows
 * you to reset the `start` and `end` pathname boundaries of the iteration
 * when doing so.
 */
GIT_INLINE(int) git_iterator_reset(
	git_iterator *iter, const char *start, const char *end)
{
	return iter->cb->reset(iter, start, end);
}

/**
 * Check if the iterator is at the end
 *
 * @return 0 if not at end, >0 if at end
 */
GIT_INLINE(int) git_iterator_at_end(git_iterator *iter)
{
	return iter->cb->at_end(iter);
}

GIT_INLINE(git_iterator_type_t) git_iterator_type(git_iterator *iter)
{
	return iter->type;
}

GIT_INLINE(git_repository *) git_iterator_owner(git_iterator *iter)
{
	return iter->repo;
}

GIT_INLINE(git_iterator_flag_t) git_iterator_flags(git_iterator *iter)
{
	return iter->flags;
}

GIT_INLINE(bool) git_iterator_ignore_case(git_iterator *iter)
{
	return ((iter->flags & GIT_ITERATOR_IGNORE_CASE) != 0);
}

extern int git_iterator_set_ignore_case(git_iterator *iter, bool ignore_case);

extern int git_iterator_current_tree_entry(
	const git_tree_entry **entry_out, git_iterator *iter);

extern int git_iterator_current_parent_tree(
	const git_tree **tree_out, git_iterator *iter, const char *parent_path);

extern bool git_iterator_current_is_ignored(git_iterator *iter);

extern bool git_iterator_current_tree_is_ignored(git_iterator *iter);

extern int git_iterator_cmp(
	git_iterator *iter, const char *path_prefix);

/**
 * Get full path of the current item from a workdir iterator.  This will
 * return NULL for a non-workdir iterator.  The git_buf is still owned by
 * the iterator; this is exposed just for efficiency.
 */
extern int git_iterator_current_workdir_path(
	git_buf **path, git_iterator *iter);

/* Return index pointer if index iterator, else NULL */
extern git_index *git_iterator_get_index(git_iterator *iter);

typedef enum {
	GIT_ITERATOR_STATUS_NORMAL = 0,
	GIT_ITERATOR_STATUS_IGNORED = 1,
	GIT_ITERATOR_STATUS_EMPTY = 2
} git_iterator_status_t;

/* Advance over a directory and check if it contains no files or just
 * ignored files.
 *
 * In a tree or the index, all directories will contain files, but in the
 * working directory it is possible to have an empty directory tree or a
 * tree that only contains ignored files.  Many Git operations treat these
 * cases specially.  This advances over a directory (presumably an
 * untracked directory) but checks during the scan if there are any files
 * and any non-ignored files.
 */
extern int git_iterator_advance_over_with_status(
	const git_index_entry **entry, git_iterator_status_t *status, git_iterator *iter);

#endif
