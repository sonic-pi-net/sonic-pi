/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "iterator.h"
#include "tree.h"
#include "index.h"
#include "ignore.h"
#include "buffer.h"
#include "submodule.h"
#include <ctype.h>

#define ITERATOR_SET_CB(P,NAME_LC) do { \
	(P)->cb.current = NAME_LC ## _iterator__current; \
	(P)->cb.advance = NAME_LC ## _iterator__advance; \
	(P)->cb.advance_into = NAME_LC ## _iterator__advance_into; \
	(P)->cb.seek    = NAME_LC ## _iterator__seek; \
	(P)->cb.reset   = NAME_LC ## _iterator__reset; \
	(P)->cb.at_end  = NAME_LC ## _iterator__at_end; \
	(P)->cb.free    = NAME_LC ## _iterator__free; \
	} while (0)

#define ITERATOR_CASE_FLAGS \
	(GIT_ITERATOR_IGNORE_CASE | GIT_ITERATOR_DONT_IGNORE_CASE)

#define ITERATOR_BASE_INIT(P,NAME_LC,NAME_UC,REPO) do { \
	(P)->base.type    = GIT_ITERATOR_TYPE_ ## NAME_UC; \
	(P)->base.cb      = &(P)->cb; \
	ITERATOR_SET_CB(P,NAME_LC); \
	(P)->base.repo    = (REPO); \
	(P)->base.start   = start ? git__strdup(start) : NULL; \
	(P)->base.end     = end ? git__strdup(end) : NULL; \
	if ((start && !(P)->base.start) || (end && !(P)->base.end)) { \
		git__free(P); return -1; } \
	(P)->base.prefixcomp = git__prefixcmp; \
	(P)->base.flags = flags & ~ITERATOR_CASE_FLAGS; \
	if ((P)->base.flags & GIT_ITERATOR_DONT_AUTOEXPAND) \
		(P)->base.flags |= GIT_ITERATOR_INCLUDE_TREES; \
	} while (0)

#define iterator__flag(I,F) ((((git_iterator *)(I))->flags & GIT_ITERATOR_ ## F) != 0)
#define iterator__ignore_case(I)     iterator__flag(I,IGNORE_CASE)
#define iterator__include_trees(I)   iterator__flag(I,INCLUDE_TREES)
#define iterator__dont_autoexpand(I) iterator__flag(I,DONT_AUTOEXPAND)
#define iterator__do_autoexpand(I)   !iterator__flag(I,DONT_AUTOEXPAND)

#define GIT_ITERATOR_FIRST_ACCESS (1 << 15)
#define iterator__has_been_accessed(I) iterator__flag(I,FIRST_ACCESS)

#define iterator__end(I) ((git_iterator *)(I))->end
#define iterator__past_end(I,PATH) \
	(iterator__end(I) && ((git_iterator *)(I))->prefixcomp((PATH),iterator__end(I)) > 0)


static int iterator__reset_range(
	git_iterator *iter, const char *start, const char *end)
{
	if (start) {
		if (iter->start)
			git__free(iter->start);
		iter->start = git__strdup(start);
		GITERR_CHECK_ALLOC(iter->start);
	}

	if (end) {
		if (iter->end)
			git__free(iter->end);
		iter->end = git__strdup(end);
		GITERR_CHECK_ALLOC(iter->end);
	}

	iter->flags &= ~GIT_ITERATOR_FIRST_ACCESS;

	return 0;
}

static int iterator__update_ignore_case(
	git_iterator *iter,
	git_iterator_flag_t flags)
{
	int error = 0, ignore_case = -1;

	if ((flags & GIT_ITERATOR_IGNORE_CASE) != 0)
		ignore_case = true;
	else if ((flags & GIT_ITERATOR_DONT_IGNORE_CASE) != 0)
		ignore_case = false;
	else {
		git_index *index;

		if (!(error = git_repository_index__weakptr(&index, iter->repo)))
			ignore_case = (index->ignore_case != false);
	}

	if (ignore_case > 0)
		iter->flags = (iter->flags | GIT_ITERATOR_IGNORE_CASE);
	else if (ignore_case == 0)
		iter->flags = (iter->flags & ~GIT_ITERATOR_IGNORE_CASE);

	iter->prefixcomp = iterator__ignore_case(iter) ?
		git__prefixcmp_icase : git__prefixcmp;

	return error;
}

GIT_INLINE(void) iterator__clear_entry(const git_index_entry **entry)
{
	if (entry) *entry = NULL;
}


static int empty_iterator__noop(const git_index_entry **e, git_iterator *i)
{
	GIT_UNUSED(i);
	iterator__clear_entry(e);
	return GIT_ITEROVER;
}

static int empty_iterator__seek(git_iterator *i, const char *p)
{
	GIT_UNUSED(i); GIT_UNUSED(p);
	return -1;
}

static int empty_iterator__reset(git_iterator *i, const char *s, const char *e)
{
	GIT_UNUSED(i); GIT_UNUSED(s); GIT_UNUSED(e);
	return 0;
}

static int empty_iterator__at_end(git_iterator *i)
{
	GIT_UNUSED(i);
	return 1;
}

static void empty_iterator__free(git_iterator *i)
{
	GIT_UNUSED(i);
}

typedef struct {
	git_iterator base;
	git_iterator_callbacks cb;
} empty_iterator;

int git_iterator_for_nothing(
	git_iterator **iter,
	git_iterator_flag_t flags,
	const char *start,
	const char *end)
{
	empty_iterator *i = git__calloc(1, sizeof(empty_iterator));
	GITERR_CHECK_ALLOC(i);

#define empty_iterator__current empty_iterator__noop
#define empty_iterator__advance empty_iterator__noop
#define empty_iterator__advance_into empty_iterator__noop

	ITERATOR_BASE_INIT(i, empty, EMPTY, NULL);

	if ((flags & GIT_ITERATOR_IGNORE_CASE) != 0)
		i->base.flags |= GIT_ITERATOR_IGNORE_CASE;

	*iter = (git_iterator *)i;
	return 0;
}


typedef struct tree_iterator_entry tree_iterator_entry;
struct tree_iterator_entry {
	tree_iterator_entry *parent;
	const git_tree_entry *te;
	git_tree *tree;
};

typedef struct tree_iterator_frame tree_iterator_frame;
struct tree_iterator_frame {
	tree_iterator_frame *up, *down;

	size_t n_entries; /* items in this frame */
	size_t current;   /* start of currently active range in frame */
	size_t next;      /* start of next range in frame */

	const char *start;
	size_t startlen;

	tree_iterator_entry *entries[GIT_FLEX_ARRAY];
};

typedef struct {
	git_iterator base;
	git_iterator_callbacks cb;
	tree_iterator_frame *head, *root;
	git_pool pool;
	git_index_entry entry;
	git_buf path;
	int path_ambiguities;
	bool path_has_filename;
	bool entry_is_current;
	int (*strncomp)(const char *a, const char *b, size_t sz);
} tree_iterator;

static char *tree_iterator__current_filename(
	tree_iterator *ti, const git_tree_entry *te)
{
	if (!ti->path_has_filename) {
		if (git_buf_joinpath(&ti->path, ti->path.ptr, te->filename) < 0)
			return NULL;

		if (git_tree_entry__is_tree(te) && git_buf_putc(&ti->path, '/') < 0)
			return NULL;

		ti->path_has_filename = true;
	}

	return ti->path.ptr;
}

static void tree_iterator__rewrite_filename(tree_iterator *ti)
{
	tree_iterator_entry *scan = ti->head->entries[ti->head->current];
	ssize_t strpos = ti->path.size;
	const git_tree_entry *te;

	if (strpos && ti->path.ptr[strpos - 1] == '/')
		strpos--;

	for (; scan && (te = scan->te); scan = scan->parent) {
		strpos -= te->filename_len;
		memcpy(&ti->path.ptr[strpos], te->filename, te->filename_len);
		strpos -= 1; /* separator */
	}
}

static int tree_iterator__te_cmp(
	const git_tree_entry *a,
	const git_tree_entry *b,
	int (*compare)(const char *, const char *, size_t))
{
	return git_path_cmp(
		a->filename, a->filename_len, a->attr == GIT_FILEMODE_TREE,
		b->filename, b->filename_len, b->attr == GIT_FILEMODE_TREE,
		compare);
}

static int tree_iterator__ci_cmp(const void *a, const void *b, void *p)
{
	const tree_iterator_entry *ae = a, *be = b;
	int cmp = tree_iterator__te_cmp(ae->te, be->te, git__strncasecmp);

	if (!cmp) {
		/* stabilize sort order among equivalent names */
		if (!ae->parent->te || !be->parent->te)
			cmp = tree_iterator__te_cmp(ae->te, be->te, git__strncmp);
		else
			cmp = tree_iterator__ci_cmp(ae->parent, be->parent, p);
	}

	return cmp;
}

static int tree_iterator__search_cmp(const void *key, const void *val, void *p)
{
	const tree_iterator_frame *tf = key;
	const git_tree_entry *te = ((tree_iterator_entry *)val)->te;

	return git_path_cmp(
		tf->start, tf->startlen, false,
		te->filename, te->filename_len, te->attr == GIT_FILEMODE_TREE,
		((tree_iterator *)p)->strncomp);
}

static bool tree_iterator__move_to_next(
	tree_iterator *ti, tree_iterator_frame *tf)
{
	if (tf->next > tf->current + 1)
		ti->path_ambiguities--;

	if (!tf->up) { /* at root */
		tf->current = tf->next;
		return false;
	}

	for (; tf->current < tf->next; tf->current++) {
		git_tree_free(tf->entries[tf->current]->tree);
		tf->entries[tf->current]->tree = NULL;
	}

	return (tf->current < tf->n_entries);
}

static int tree_iterator__set_next(tree_iterator *ti, tree_iterator_frame *tf)
{
	int error = 0;
	const git_tree_entry *te, *last = NULL;

	tf->next = tf->current;

	for (; tf->next < tf->n_entries; tf->next++, last = te) {
		te = tf->entries[tf->next]->te;

		if (last && tree_iterator__te_cmp(last, te, ti->strncomp))
			break;

		/* try to load trees for items in [current,next) range */
		if (!error && git_tree_entry__is_tree(te))
			error = git_tree_lookup(
				&tf->entries[tf->next]->tree, ti->base.repo, &te->oid);
	}

	if (tf->next > tf->current + 1)
		ti->path_ambiguities++;

	/* if a tree lookup failed, advance over this span and return failure */
	if (error < 0) {
		tree_iterator__move_to_next(ti, tf);
		return error;
	}

	if (last && !tree_iterator__current_filename(ti, last))
		return -1; /* must have been allocation failure */

	return 0;
}

GIT_INLINE(bool) tree_iterator__at_tree(tree_iterator *ti)
{
	return (ti->head->current < ti->head->n_entries &&
			ti->head->entries[ti->head->current]->tree != NULL);
}

static int tree_iterator__push_frame(tree_iterator *ti)
{
	int error = 0;
	tree_iterator_frame *head = ti->head, *tf = NULL;
	size_t i, n_entries = 0;

	if (head->current >= head->n_entries || !head->entries[head->current]->tree)
		return GIT_ITEROVER;

	for (i = head->current; i < head->next; ++i)
		n_entries += git_tree_entrycount(head->entries[i]->tree);

	tf = git__calloc(sizeof(tree_iterator_frame) +
		n_entries * sizeof(tree_iterator_entry *), 1);
	GITERR_CHECK_ALLOC(tf);

	tf->n_entries = n_entries;

	tf->up     = head;
	head->down = tf;
	ti->head   = tf;

	for (i = head->current, n_entries = 0; i < head->next; ++i) {
		git_tree *tree = head->entries[i]->tree;
		size_t j, max_j = git_tree_entrycount(tree);

		for (j = 0; j < max_j; ++j) {
			tree_iterator_entry *entry = git_pool_malloc(&ti->pool, 1);
			GITERR_CHECK_ALLOC(entry);

			entry->parent = head->entries[i];
			entry->te     = git_tree_entry_byindex(tree, j);
			entry->tree   = NULL;

			tf->entries[n_entries++] = entry;
		}
	}

	/* if ignore_case, sort entries case insensitively */
	if (iterator__ignore_case(ti))
		git__tsort_r(
			(void **)tf->entries, tf->n_entries, tree_iterator__ci_cmp, tf);

	/* pick tf->current based on "start" (or start at zero) */
	if (head->startlen > 0) {
		git__bsearch_r((void **)tf->entries, tf->n_entries, head,
			tree_iterator__search_cmp, ti, &tf->current);

		while (tf->current &&
			   !tree_iterator__search_cmp(head, tf->entries[tf->current-1], ti))
			tf->current--;

		if ((tf->start = strchr(head->start, '/')) != NULL) {
			tf->start++;
			tf->startlen = strlen(tf->start);
		}
	}

	ti->path_has_filename = ti->entry_is_current = false;

	if ((error = tree_iterator__set_next(ti, tf)) < 0)
		return error;

	/* autoexpand as needed */
	if (!iterator__include_trees(ti) && tree_iterator__at_tree(ti))
		return tree_iterator__push_frame(ti);

	return 0;
}

static bool tree_iterator__pop_frame(tree_iterator *ti, bool final)
{
	tree_iterator_frame *tf = ti->head;

	if (!tf->up)
		return false;

	ti->head = tf->up;
	ti->head->down = NULL;

	tree_iterator__move_to_next(ti, tf);

	if (!final) { /* if final, don't bother to clean up */
		git_pool_free_array(&ti->pool, tf->n_entries, (void **)tf->entries);
		git_buf_rtruncate_at_char(&ti->path, '/');
	}

	git__free(tf);

	return true;
}

static void tree_iterator__pop_all(tree_iterator *ti, bool to_end, bool final)
{
	while (tree_iterator__pop_frame(ti, final)) /* pop to root */;

	if (!final) {
		ti->head->current = to_end ? ti->head->n_entries : 0;
		ti->path_ambiguities = 0;
		git_buf_clear(&ti->path);
	}
}

static int tree_iterator__update_entry(tree_iterator *ti)
{
	tree_iterator_frame *tf;
    const git_tree_entry *te;

	if (ti->entry_is_current)
        return 0;

	tf = ti->head;
    te = tf->entries[tf->current]->te;

	ti->entry.mode = te->attr;
	git_oid_cpy(&ti->entry.id, &te->oid);

	ti->entry.path = tree_iterator__current_filename(ti, te);
	GITERR_CHECK_ALLOC(ti->entry.path);

	if (ti->path_ambiguities > 0)
		tree_iterator__rewrite_filename(ti);

	if (iterator__past_end(ti, ti->entry.path)) {
		tree_iterator__pop_all(ti, true, false);
		return GIT_ITEROVER;
	}

	ti->entry_is_current = true;

	return 0;
}

static int tree_iterator__current(
	const git_index_entry **entry, git_iterator *self)
{
	int error;
	tree_iterator *ti = (tree_iterator *)self;
	tree_iterator_frame *tf = ti->head;

	iterator__clear_entry(entry);

	if (tf->current >= tf->n_entries)
		return GIT_ITEROVER;

	if ((error = tree_iterator__update_entry(ti)) < 0)
		return error;

	if (entry)
		*entry = &ti->entry;

	ti->base.flags |= GIT_ITERATOR_FIRST_ACCESS;

	return 0;
}

static int tree_iterator__advance_into(
	const git_index_entry **entry, git_iterator *self)
{
	int error = 0;
	tree_iterator *ti = (tree_iterator *)self;

	iterator__clear_entry(entry);

	if (tree_iterator__at_tree(ti))
		error = tree_iterator__push_frame(ti);

	if (!error && entry)
		error = tree_iterator__current(entry, self);

	return error;
}

static int tree_iterator__advance(
	const git_index_entry **entry, git_iterator *self)
{
	int error;
	tree_iterator *ti = (tree_iterator *)self;
	tree_iterator_frame *tf = ti->head;

	iterator__clear_entry(entry);

	if (tf->current >= tf->n_entries)
		return GIT_ITEROVER;

	if (!iterator__has_been_accessed(ti))
		return tree_iterator__current(entry, self);

	if (iterator__do_autoexpand(ti) && iterator__include_trees(ti) &&
		tree_iterator__at_tree(ti))
		return tree_iterator__advance_into(entry, self);

	if (ti->path_has_filename) {
		git_buf_rtruncate_at_char(&ti->path, '/');
		ti->path_has_filename = ti->entry_is_current = false;
	}

	/* scan forward and up, advancing in frame or popping frame when done */
	while (!tree_iterator__move_to_next(ti, tf) &&
		   tree_iterator__pop_frame(ti, false))
		tf = ti->head;

	/* find next and load trees */
	if ((error = tree_iterator__set_next(ti, tf)) < 0)
		return error;

	/* deal with include_trees / auto_expand as needed */
	if (!iterator__include_trees(ti) && tree_iterator__at_tree(ti))
		return tree_iterator__advance_into(entry, self);

	return tree_iterator__current(entry, self);
}

static int tree_iterator__seek(git_iterator *self, const char *prefix)
{
	GIT_UNUSED(self); GIT_UNUSED(prefix);
	return -1;
}

static int tree_iterator__reset(
	git_iterator *self, const char *start, const char *end)
{
	tree_iterator *ti = (tree_iterator *)self;

	tree_iterator__pop_all(ti, false, false);

	if (iterator__reset_range(self, start, end) < 0)
		return -1;

	return tree_iterator__push_frame(ti); /* re-expand root tree */
}

static int tree_iterator__at_end(git_iterator *self)
{
	tree_iterator *ti = (tree_iterator *)self;
	return (ti->head->current >= ti->head->n_entries);
}

static void tree_iterator__free(git_iterator *self)
{
	tree_iterator *ti = (tree_iterator *)self;

	tree_iterator__pop_all(ti, true, false);

	git_tree_free(ti->head->entries[0]->tree);
	git__free(ti->head);
	git_pool_clear(&ti->pool);
	git_buf_free(&ti->path);
}

static int tree_iterator__create_root_frame(tree_iterator *ti, git_tree *tree)
{
	size_t sz = sizeof(tree_iterator_frame) + sizeof(tree_iterator_entry);
	tree_iterator_frame *root = git__calloc(sz, sizeof(char));
	GITERR_CHECK_ALLOC(root);

	root->n_entries  = 1;
	root->next       = 1;
	root->start      = ti->base.start;
	root->startlen   = root->start ? strlen(root->start) : 0;
	root->entries[0] = git_pool_mallocz(&ti->pool, 1);
	GITERR_CHECK_ALLOC(root->entries[0]);
	root->entries[0]->tree = tree;

	ti->head = ti->root = root;

	return 0;
}

int git_iterator_for_tree(
	git_iterator **iter,
	git_tree *tree,
	git_iterator_flag_t flags,
	const char *start,
	const char *end)
{
	int error;
	tree_iterator *ti;

	if (tree == NULL)
		return git_iterator_for_nothing(iter, flags, start, end);

	if ((error = git_object_dup((git_object **)&tree, (git_object *)tree)) < 0)
		return error;

	ti = git__calloc(1, sizeof(tree_iterator));
	GITERR_CHECK_ALLOC(ti);

	ITERATOR_BASE_INIT(ti, tree, TREE, git_tree_owner(tree));

	if ((error = iterator__update_ignore_case((git_iterator *)ti, flags)) < 0)
		goto fail;
	ti->strncomp = iterator__ignore_case(ti) ? git__strncasecmp : git__strncmp;

	if ((error = git_pool_init(&ti->pool, sizeof(tree_iterator_entry),0)) < 0 ||
		(error = tree_iterator__create_root_frame(ti, tree)) < 0 ||
		(error = tree_iterator__push_frame(ti)) < 0) /* expand root now */
		goto fail;

	*iter = (git_iterator *)ti;
	return 0;

fail:
	git_iterator_free((git_iterator *)ti);
	return error;
}


typedef struct {
	git_iterator base;
	git_iterator_callbacks cb;
	git_index *index;
	git_vector entries;
	git_vector_cmp entry_srch;
	size_t current;
	/* when not in autoexpand mode, use these to represent "tree" state */
	git_buf partial;
	size_t partial_pos;
	char restore_terminator;
	git_index_entry tree_entry;
} index_iterator;

static const git_index_entry *index_iterator__index_entry(index_iterator *ii)
{
	const git_index_entry *ie = git_vector_get(&ii->entries, ii->current);

	if (ie != NULL && iterator__past_end(ii, ie->path)) {
		ii->current = git_vector_length(&ii->entries);
		ie = NULL;
	}

	return ie;
}

static const git_index_entry *index_iterator__skip_conflicts(index_iterator *ii)
{
	const git_index_entry *ie;

	while ((ie = index_iterator__index_entry(ii)) != NULL &&
		   git_index_entry_stage(ie) != 0)
		ii->current++;

	return ie;
}

static void index_iterator__next_prefix_tree(index_iterator *ii)
{
	const char *slash;

	if (!iterator__include_trees(ii))
		return;

	slash = strchr(&ii->partial.ptr[ii->partial_pos], '/');

	if (slash != NULL) {
		ii->partial_pos = (slash - ii->partial.ptr) + 1;
		ii->restore_terminator = ii->partial.ptr[ii->partial_pos];
		ii->partial.ptr[ii->partial_pos] = '\0';
	} else {
		ii->partial_pos = ii->partial.size;
	}

	if (index_iterator__index_entry(ii) == NULL)
		ii->partial_pos = ii->partial.size;
}

static int index_iterator__first_prefix_tree(index_iterator *ii)
{
	const git_index_entry *ie = index_iterator__skip_conflicts(ii);
	const char *scan, *prior, *slash;

	if (!ie || !iterator__include_trees(ii))
		return 0;

	/* find longest common prefix with prior index entry */
	for (scan = slash = ie->path, prior = ii->partial.ptr;
		 *scan && *scan == *prior; ++scan, ++prior)
		if (*scan == '/')
			slash = scan;

	if (git_buf_sets(&ii->partial, ie->path) < 0)
		return -1;

	ii->partial_pos = (slash - ie->path) + 1;
	index_iterator__next_prefix_tree(ii);

	return 0;
}

#define index_iterator__at_tree(I) \
	(iterator__include_trees(I) && (I)->partial_pos < (I)->partial.size)

static int index_iterator__current(
	const git_index_entry **entry, git_iterator *self)
{
	index_iterator *ii = (index_iterator *)self;
	const git_index_entry *ie = git_vector_get(&ii->entries, ii->current);

	if (ie != NULL && index_iterator__at_tree(ii)) {
		ii->tree_entry.path = ii->partial.ptr;
		ie = &ii->tree_entry;
	}

	if (entry)
		*entry = ie;

	ii->base.flags |= GIT_ITERATOR_FIRST_ACCESS;

	return (ie != NULL) ? 0 : GIT_ITEROVER;
}

static int index_iterator__at_end(git_iterator *self)
{
	index_iterator *ii = (index_iterator *)self;
	return (ii->current >= git_vector_length(&ii->entries));
}

static int index_iterator__advance(
	const git_index_entry **entry, git_iterator *self)
{
	index_iterator *ii = (index_iterator *)self;
	size_t entrycount = git_vector_length(&ii->entries);
	const git_index_entry *ie;

	if (!iterator__has_been_accessed(ii))
		return index_iterator__current(entry, self);

	if (index_iterator__at_tree(ii)) {
		if (iterator__do_autoexpand(ii)) {
			ii->partial.ptr[ii->partial_pos] = ii->restore_terminator;
			index_iterator__next_prefix_tree(ii);
		} else {
			/* advance to sibling tree (i.e. find entry with new prefix) */
			while (ii->current < entrycount) {
				ii->current++;

				if (!(ie = git_vector_get(&ii->entries, ii->current)) ||
					ii->base.prefixcomp(ie->path, ii->partial.ptr) != 0)
					break;
			}

			if (index_iterator__first_prefix_tree(ii) < 0)
				return -1;
		}
	} else {
		if (ii->current < entrycount)
			ii->current++;

		if (index_iterator__first_prefix_tree(ii) < 0)
			return -1;
	}

	return index_iterator__current(entry, self);
}

static int index_iterator__advance_into(
	const git_index_entry **entry, git_iterator *self)
{
	index_iterator *ii = (index_iterator *)self;
	const git_index_entry *ie = git_vector_get(&ii->entries, ii->current);

	if (ie != NULL && index_iterator__at_tree(ii)) {
		if (ii->restore_terminator)
			ii->partial.ptr[ii->partial_pos] = ii->restore_terminator;
		index_iterator__next_prefix_tree(ii);
	}

	return index_iterator__current(entry, self);
}

static int index_iterator__seek(git_iterator *self, const char *prefix)
{
	GIT_UNUSED(self); GIT_UNUSED(prefix);
	return -1;
}

static int index_iterator__reset(
	git_iterator *self, const char *start, const char *end)
{
	index_iterator *ii = (index_iterator *)self;
	const git_index_entry *ie;

	if (iterator__reset_range(self, start, end) < 0)
		return -1;

	ii->current = 0;

	if (ii->base.start)
		git_index_snapshot_find(
			&ii->current, &ii->entries, ii->entry_srch, ii->base.start, 0, 0);

	if ((ie = index_iterator__skip_conflicts(ii)) == NULL)
		return 0;

	if (git_buf_sets(&ii->partial, ie->path) < 0)
		return -1;

	ii->partial_pos = 0;

	if (ii->base.start) {
		size_t startlen = strlen(ii->base.start);

		ii->partial_pos = (startlen > ii->partial.size) ?
			ii->partial.size : startlen;
	}

	index_iterator__next_prefix_tree(ii);

	return 0;
}

static void index_iterator__free(git_iterator *self)
{
	index_iterator *ii = (index_iterator *)self;
	git_index_snapshot_release(&ii->entries, ii->index);
	ii->index = NULL;
	git_buf_free(&ii->partial);
}

int git_iterator_for_index(
	git_iterator **iter,
	git_index  *index,
	git_iterator_flag_t flags,
	const char *start,
	const char *end)
{
	int error = 0;
	index_iterator *ii = git__calloc(1, sizeof(index_iterator));
	GITERR_CHECK_ALLOC(ii);

	if ((error = git_index_snapshot_new(&ii->entries, index)) < 0) {
		git__free(ii);
		return error;
	}
	ii->index = index;

	ITERATOR_BASE_INIT(ii, index, INDEX, git_index_owner(index));

	if ((error = iterator__update_ignore_case((git_iterator *)ii, flags)) < 0) {
		git_iterator_free((git_iterator *)ii);
		return error;
	}

	ii->entry_srch = iterator__ignore_case(ii) ?
		git_index_entry_isrch : git_index_entry_srch;

	git_vector_set_cmp(&ii->entries, iterator__ignore_case(ii) ?
		git_index_entry_icmp : git_index_entry_cmp);
	git_vector_sort(&ii->entries);

	git_buf_init(&ii->partial, 0);
	ii->tree_entry.mode = GIT_FILEMODE_TREE;

	index_iterator__reset((git_iterator *)ii, NULL, NULL);

	*iter = (git_iterator *)ii;
	return 0;
}


typedef struct fs_iterator_frame fs_iterator_frame;
struct fs_iterator_frame {
	fs_iterator_frame *next;
	git_vector entries;
	size_t index;
	int is_ignored;
};

typedef struct fs_iterator fs_iterator;
struct fs_iterator {
	git_iterator base;
	git_iterator_callbacks cb;
	fs_iterator_frame *stack;
	git_index_entry entry;
	git_buf path;
	size_t root_len;
	uint32_t dirload_flags;
	int depth;

	int (*enter_dir_cb)(fs_iterator *self);
	int (*leave_dir_cb)(fs_iterator *self);
	int (*update_entry_cb)(fs_iterator *self);
};

#define FS_MAX_DEPTH 100

static fs_iterator_frame *fs_iterator__alloc_frame(fs_iterator *fi)
{
	fs_iterator_frame *ff = git__calloc(1, sizeof(fs_iterator_frame));
	git_vector_cmp entry_compare = CASESELECT(
		iterator__ignore_case(fi),
		git_path_with_stat_cmp_icase, git_path_with_stat_cmp);

	if (ff && git_vector_init(&ff->entries, 0, entry_compare) < 0) {
		git__free(ff);
		ff = NULL;
	}

	return ff;
}

static void fs_iterator__free_frame(fs_iterator_frame *ff)
{
	git_vector_free_deep(&ff->entries);
	git__free(ff);
}

static void fs_iterator__pop_frame(
	fs_iterator *fi, fs_iterator_frame *ff, bool pop_last)
{
	if (fi && fi->stack == ff) {
		if (!ff->next && !pop_last) {
			memset(&fi->entry, 0, sizeof(fi->entry));
			return;
		}

		if (fi->leave_dir_cb)
			(void)fi->leave_dir_cb(fi);

		fi->stack = ff->next;
		fi->depth--;
	}

	fs_iterator__free_frame(ff);
}

static int fs_iterator__update_entry(fs_iterator *fi);
static int fs_iterator__advance_over(
	const git_index_entry **entry, git_iterator *self);

static int fs_iterator__entry_cmp(const void *i, const void *item)
{
	const fs_iterator *fi = (const fs_iterator *)i;
	const git_path_with_stat *ps = item;
	return fi->base.prefixcomp(fi->base.start, ps->path);
}

static void fs_iterator__seek_frame_start(
	fs_iterator *fi, fs_iterator_frame *ff)
{
	if (!ff)
		return;

	if (fi->base.start)
		git_vector_bsearch2(
			&ff->index, &ff->entries, fs_iterator__entry_cmp, fi);
	else
		ff->index = 0;
}

static int fs_iterator__expand_dir(fs_iterator *fi)
{
	int error;
	fs_iterator_frame *ff;

	if (fi->depth > FS_MAX_DEPTH) {
		giterr_set(GITERR_REPOSITORY,
			"Directory nesting is too deep (%d)", fi->depth);
		return -1;
	}

	ff = fs_iterator__alloc_frame(fi);
	GITERR_CHECK_ALLOC(ff);

	error = git_path_dirload_with_stat(
		fi->path.ptr, fi->root_len, fi->dirload_flags,
		fi->base.start, fi->base.end, &ff->entries);

	if (error < 0) {
		git_error_state last_error = { 0 };
		giterr_capture(&last_error, error);

		/* these callbacks may clear the error message */
		fs_iterator__free_frame(ff);
		fs_iterator__advance_over(NULL, (git_iterator *)fi);
		/* next time return value we skipped to */
		fi->base.flags &= ~GIT_ITERATOR_FIRST_ACCESS;

		return giterr_restore(&last_error);
	}

	if (ff->entries.length == 0) {
		fs_iterator__free_frame(ff);
		return GIT_ENOTFOUND;
	}
	fi->base.stat_calls += ff->entries.length;

	fs_iterator__seek_frame_start(fi, ff);

	ff->next  = fi->stack;
	fi->stack = ff;
	fi->depth++;

	if (fi->enter_dir_cb && (error = fi->enter_dir_cb(fi)) < 0)
		return error;

	return fs_iterator__update_entry(fi);
}

static int fs_iterator__current(
	const git_index_entry **entry, git_iterator *self)
{
	fs_iterator *fi = (fs_iterator *)self;
	const git_index_entry *fe = (fi->entry.path == NULL) ? NULL : &fi->entry;

	if (entry)
		*entry = fe;

	fi->base.flags |= GIT_ITERATOR_FIRST_ACCESS;

	return (fe != NULL) ? 0 : GIT_ITEROVER;
}

static int fs_iterator__at_end(git_iterator *self)
{
	return (((fs_iterator *)self)->entry.path == NULL);
}

static int fs_iterator__advance_into(
	const git_index_entry **entry, git_iterator *iter)
{
	int error = 0;
	fs_iterator *fi = (fs_iterator *)iter;

	iterator__clear_entry(entry);

	/* Allow you to explicitly advance into a commit/submodule (as well as a
	 * tree) to avoid cases where an entry is mislabeled as a submodule in
	 * the working directory.  The fs iterator will never have COMMMIT
	 * entries on it's own, but a wrapper might add them.
	 */
	if (fi->entry.path != NULL &&
		(fi->entry.mode == GIT_FILEMODE_TREE ||
		 fi->entry.mode == GIT_FILEMODE_COMMIT))
		/* returns GIT_ENOTFOUND if the directory is empty */
		error = fs_iterator__expand_dir(fi);

	if (!error && entry)
		error = fs_iterator__current(entry, iter);

	if (!error && !fi->entry.path)
		error = GIT_ITEROVER;

	return error;
}

static int fs_iterator__advance_over(
	const git_index_entry **entry, git_iterator *self)
{
	int error = 0;
	fs_iterator *fi = (fs_iterator *)self;
	fs_iterator_frame *ff;
	git_path_with_stat *next;

	if (entry != NULL)
		*entry = NULL;

	while (fi->entry.path != NULL) {
		ff   = fi->stack;
		next = git_vector_get(&ff->entries, ++ff->index);

		if (next != NULL)
			break;

		fs_iterator__pop_frame(fi, ff, false);
	}

	error = fs_iterator__update_entry(fi);

	if (!error && entry != NULL)
		error = fs_iterator__current(entry, self);

	return error;
}

static int fs_iterator__advance(
	const git_index_entry **entry, git_iterator *self)
{
	fs_iterator *fi = (fs_iterator *)self;

	if (!iterator__has_been_accessed(fi))
		return fs_iterator__current(entry, self);

	/* given include_trees & autoexpand, we might have to go into a tree */
	if (iterator__do_autoexpand(fi) &&
		fi->entry.path != NULL &&
		fi->entry.mode == GIT_FILEMODE_TREE)
	{
		int error = fs_iterator__advance_into(entry, self);
		if (error != GIT_ENOTFOUND)
			return error;
		/* continue silently past empty directories if autoexpanding */
		giterr_clear();
	}

	return fs_iterator__advance_over(entry, self);
}

static int fs_iterator__seek(git_iterator *self, const char *prefix)
{
	GIT_UNUSED(self);
	GIT_UNUSED(prefix);
	/* pop stack until matching prefix */
	/* find prefix item in current frame */
	/* push subdirectories as deep as possible while matching */
	return 0;
}

static int fs_iterator__reset(
	git_iterator *self, const char *start, const char *end)
{
	int error;
	fs_iterator *fi = (fs_iterator *)self;

	while (fi->stack != NULL && fi->stack->next != NULL)
		fs_iterator__pop_frame(fi, fi->stack, false);
	fi->depth = 0;

	if ((error = iterator__reset_range(self, start, end)) < 0)
		return error;

	fs_iterator__seek_frame_start(fi, fi->stack);

	error = fs_iterator__update_entry(fi);
	if (error == GIT_ITEROVER)
		error = 0;

	return error;
}

static void fs_iterator__free(git_iterator *self)
{
	fs_iterator *fi = (fs_iterator *)self;

	while (fi->stack != NULL)
		fs_iterator__pop_frame(fi, fi->stack, true);

	git_buf_free(&fi->path);
}

static int fs_iterator__update_entry(fs_iterator *fi)
{
	git_path_with_stat *ps;

	memset(&fi->entry, 0, sizeof(fi->entry));

	if (!fi->stack)
		return GIT_ITEROVER;

	ps = git_vector_get(&fi->stack->entries, fi->stack->index);
	if (!ps)
		return GIT_ITEROVER;

	git_buf_truncate(&fi->path, fi->root_len);
	if (git_buf_put(&fi->path, ps->path, ps->path_len) < 0)
		return -1;

	if (iterator__past_end(fi, fi->path.ptr + fi->root_len))
		return GIT_ITEROVER;

	fi->entry.path = ps->path;
	git_index_entry__init_from_stat(&fi->entry, &ps->st, true);

	/* need different mode here to keep directories during iteration */
	fi->entry.mode = git_futils_canonical_mode(ps->st.st_mode);

	/* allow wrapper to check/update the entry (can force skip) */
	if (fi->update_entry_cb &&
		fi->update_entry_cb(fi) == GIT_ENOTFOUND)
		return fs_iterator__advance_over(NULL, (git_iterator *)fi);

	/* if this is a tree and trees aren't included, then skip */
	if (fi->entry.mode == GIT_FILEMODE_TREE && !iterator__include_trees(fi)) {
		int error = fs_iterator__advance_into(NULL, (git_iterator *)fi);
		if (error != GIT_ENOTFOUND)
			return error;
		giterr_clear();
		return fs_iterator__advance_over(NULL, (git_iterator *)fi);
	}

	return 0;
}

static int fs_iterator__initialize(
	git_iterator **out, fs_iterator *fi, const char *root)
{
	int error;

	if (git_buf_sets(&fi->path, root) < 0 || git_path_to_dir(&fi->path) < 0) {
		git__free(fi);
		return -1;
	}
	fi->root_len = fi->path.size;

	fi->dirload_flags =
		(iterator__ignore_case(fi) ? GIT_PATH_DIR_IGNORE_CASE : 0) |
		(iterator__flag(fi, PRECOMPOSE_UNICODE) ?
			GIT_PATH_DIR_PRECOMPOSE_UNICODE : 0);

	if ((error = fs_iterator__expand_dir(fi)) < 0) {
		if (error == GIT_ENOTFOUND || error == GIT_ITEROVER) {
			giterr_clear();
			error = 0;
		} else {
			git_iterator_free((git_iterator *)fi);
			fi = NULL;
		}
	}

	*out = (git_iterator *)fi;
	return error;
}

int git_iterator_for_filesystem(
	git_iterator **out,
	const char *root,
	git_iterator_flag_t flags,
	const char *start,
	const char *end)
{
	fs_iterator *fi = git__calloc(1, sizeof(fs_iterator));
	GITERR_CHECK_ALLOC(fi);

	ITERATOR_BASE_INIT(fi, fs, FS, NULL);

	if ((flags & GIT_ITERATOR_IGNORE_CASE) != 0)
		fi->base.flags |= GIT_ITERATOR_IGNORE_CASE;

	return fs_iterator__initialize(out, fi, root);
}


typedef struct {
	fs_iterator fi;
	git_ignores ignores;
	int is_ignored;

	/*
	 * We may have a tree or the index+snapshot to compare against
	 * when checking for submodules.
	 */
	git_tree *tree;
	git_index *index;
	git_vector index_snapshot;
	git_vector_cmp entry_srch;

} workdir_iterator;

GIT_INLINE(bool) workdir_path_is_dotgit(const git_buf *path)
{
	size_t len;

	if (!path || (len = path->size) < 4)
		return false;

	if (path->ptr[len - 1] == '/')
		len--;

	if (tolower(path->ptr[len - 1]) != 't' ||
		tolower(path->ptr[len - 2]) != 'i' ||
		tolower(path->ptr[len - 3]) != 'g' ||
		tolower(path->ptr[len - 4]) != '.')
		return false;

	return (len == 4 || path->ptr[len - 5] == '/');
}

/**
 * Figure out if an entry is a submodule.
 *
 * We consider it a submodule if the path is listed as a submodule in
 * either the tree or the index.
 */
static int is_submodule(workdir_iterator *wi, git_path_with_stat *ie)
{
	int error, is_submodule = 0;

	if (wi->tree) {
		git_tree_entry *e;

		/* remove the trailing slash for finding */
		ie->path[ie->path_len-1] = '\0';
		error = git_tree_entry_bypath(&e, wi->tree, ie->path);
		ie->path[ie->path_len-1] = '/';
		if (error < 0 && error != GIT_ENOTFOUND)
			return 0;
		if (!error) {
			is_submodule = e->attr == GIT_FILEMODE_COMMIT;
			git_tree_entry_free(e);
		}
	}

	if (!is_submodule && wi->index) {
		git_index_entry *e;
		size_t pos;

		error = git_index_snapshot_find(&pos, &wi->index_snapshot, wi->entry_srch, ie->path, ie->path_len-1, 0);
		if (error < 0 && error != GIT_ENOTFOUND)
			return 0;

		if (!error) {
			e = git_vector_get(&wi->index_snapshot, pos);

			is_submodule = e->mode == GIT_FILEMODE_COMMIT;
		}
	}

	return is_submodule;
}

static int workdir_iterator__enter_dir(fs_iterator *fi)
{
	workdir_iterator *wi = (workdir_iterator *)fi;
	fs_iterator_frame *ff = fi->stack;
	size_t pos;
	git_path_with_stat *entry;
	bool found_submodules = false;

	/* check if this directory is ignored */
	if (git_ignore__lookup(
			&ff->is_ignored, &wi->ignores, fi->path.ptr + fi->root_len) < 0) {
		giterr_clear();
		ff->is_ignored = GIT_IGNORE_NOTFOUND;
	}

	/* if this is not the top level directory... */
	if (ff->next != NULL) {
		ssize_t slash_pos = git_buf_rfind_next(&fi->path, '/');

		/* inherit ignored from parent if no rule specified */
		if (ff->is_ignored <= GIT_IGNORE_NOTFOUND)
			ff->is_ignored = ff->next->is_ignored;

		/* push new ignores for files in this directory */
		(void)git_ignore__push_dir(&wi->ignores, &fi->path.ptr[slash_pos + 1]);
	}

	/* convert submodules to GITLINK and remove trailing slashes */
	git_vector_foreach(&ff->entries, pos, entry) {
		if (!S_ISDIR(entry->st.st_mode) || !strcmp(GIT_DIR, entry->path))
			continue;

		if (is_submodule(wi, entry)) {
			entry->st.st_mode = GIT_FILEMODE_COMMIT;
			entry->path_len--;
			entry->path[entry->path_len] = '\0';
			found_submodules = true;
		}
	}

	/* if we renamed submodules, re-sort and re-seek to start */
	if (found_submodules) {
		git_vector_set_sorted(&ff->entries, 0);
		git_vector_sort(&ff->entries);
		fs_iterator__seek_frame_start(fi, ff);
	}

	return 0;
}

static int workdir_iterator__leave_dir(fs_iterator *fi)
{
	workdir_iterator *wi = (workdir_iterator *)fi;
	git_ignore__pop_dir(&wi->ignores);
	return 0;
}

static int workdir_iterator__update_entry(fs_iterator *fi)
{
	workdir_iterator *wi = (workdir_iterator *)fi;

	/* skip over .git entries */
	if (workdir_path_is_dotgit(&fi->path))
		return GIT_ENOTFOUND;

	/* reset is_ignored since we haven't checked yet */
	wi->is_ignored = GIT_IGNORE_UNCHECKED;

	return 0;
}

static void workdir_iterator__free(git_iterator *self)
{
	workdir_iterator *wi = (workdir_iterator *)self;
	if (wi->index)
		git_index_snapshot_release(&wi->index_snapshot, wi->index);
	git_tree_free(wi->tree);
	fs_iterator__free(self);
	git_ignore__free(&wi->ignores);
}

int git_iterator_for_workdir_ext(
	git_iterator **out,
	git_repository *repo,
	const char *repo_workdir,
	git_index *index,
	git_tree *tree,
	git_iterator_flag_t flags,
	const char *start,
	const char *end)
{
	int error, precompose = 0;
	workdir_iterator *wi;

	if (!repo_workdir) {
		if (git_repository__ensure_not_bare(repo, "scan working directory") < 0)
			return GIT_EBAREREPO;
		repo_workdir = git_repository_workdir(repo);
	}

	/* initialize as an fs iterator then do overrides */
	wi = git__calloc(1, sizeof(workdir_iterator));
	GITERR_CHECK_ALLOC(wi);
	ITERATOR_BASE_INIT((&wi->fi), fs, FS, repo);

	wi->fi.base.type = GIT_ITERATOR_TYPE_WORKDIR;
	wi->fi.cb.free = workdir_iterator__free;
	wi->fi.enter_dir_cb = workdir_iterator__enter_dir;
	wi->fi.leave_dir_cb = workdir_iterator__leave_dir;
	wi->fi.update_entry_cb = workdir_iterator__update_entry;

	if ((error = iterator__update_ignore_case((git_iterator *)wi, flags)) < 0 ||
		(error = git_ignore__for_path(repo, ".gitignore", &wi->ignores)) < 0)
	{
		git_iterator_free((git_iterator *)wi);
		return error;
	}

	if (tree && (error = git_object_dup((git_object **)&wi->tree, (git_object *)tree)) < 0)
		return error;

	wi->index = index;
	if (index && (error = git_index_snapshot_new(&wi->index_snapshot, index)) < 0) {
		git_iterator_free((git_iterator *)wi);
		return error;
	}
	wi->entry_srch = iterator__ignore_case(wi) ?
		git_index_entry_isrch : git_index_entry_srch;


	/* try to look up precompose and set flag if appropriate */
	if (git_repository__cvar(&precompose, repo, GIT_CVAR_PRECOMPOSE) < 0)
		giterr_clear();
	else if (precompose)
		wi->fi.base.flags |= GIT_ITERATOR_PRECOMPOSE_UNICODE;

	return fs_iterator__initialize(out, &wi->fi, repo_workdir);
}


void git_iterator_free(git_iterator *iter)
{
	if (iter == NULL)
		return;

	iter->cb->free(iter);

	git__free(iter->start);
	git__free(iter->end);

	memset(iter, 0, sizeof(*iter));

	git__free(iter);
}

int git_iterator_set_ignore_case(git_iterator *iter, bool ignore_case)
{
	bool desire_ignore_case  = (ignore_case != 0);

	if (iterator__ignore_case(iter) == desire_ignore_case)
		return 0;

	if (iter->type == GIT_ITERATOR_TYPE_EMPTY) {
		if (desire_ignore_case)
			iter->flags |= GIT_ITERATOR_IGNORE_CASE;
		else
			iter->flags &= ~GIT_ITERATOR_IGNORE_CASE;
	} else {
		giterr_set(GITERR_INVALID,
			"Cannot currently set ignore case on non-empty iterators");
		return -1;
	}

	return 0;
}

git_index *git_iterator_get_index(git_iterator *iter)
{
	if (iter->type == GIT_ITERATOR_TYPE_INDEX)
		return ((index_iterator *)iter)->index;
	return NULL;
}

int git_iterator_current_tree_entry(
	const git_tree_entry **tree_entry, git_iterator *iter)
{
	if (iter->type != GIT_ITERATOR_TYPE_TREE)
		*tree_entry = NULL;
	else {
		tree_iterator_frame *tf = ((tree_iterator *)iter)->head;
		*tree_entry = (tf->current < tf->n_entries) ?
			tf->entries[tf->current]->te : NULL;
	}

	return 0;
}

int git_iterator_current_parent_tree(
	const git_tree **tree_ptr,
	git_iterator *iter,
	const char *parent_path)
{
	tree_iterator *ti = (tree_iterator *)iter;
	tree_iterator_frame *tf;
	const char *scan = parent_path;
	const git_tree_entry *te;

	*tree_ptr = NULL;

	if (iter->type != GIT_ITERATOR_TYPE_TREE)
		return 0;

	for (tf = ti->root; *scan; ) {
		if (!(tf = tf->down) ||
			tf->current >= tf->n_entries ||
			!(te = tf->entries[tf->current]->te) ||
			ti->strncomp(scan, te->filename, te->filename_len) != 0)
			return 0;

		scan += te->filename_len;
		if (*scan == '/')
			scan++;
	}

	*tree_ptr = tf->entries[tf->current]->tree;
	return 0;
}

static void workdir_iterator_update_is_ignored(workdir_iterator *wi)
{
	if (git_ignore__lookup(
			&wi->is_ignored, &wi->ignores, wi->fi.entry.path) < 0) {
		giterr_clear();
		wi->is_ignored = GIT_IGNORE_NOTFOUND;
	}

	/* use ignore from containing frame stack */
	if (wi->is_ignored <= GIT_IGNORE_NOTFOUND)
		wi->is_ignored = wi->fi.stack->is_ignored;
}

bool git_iterator_current_is_ignored(git_iterator *iter)
{
	workdir_iterator *wi = (workdir_iterator *)iter;

	if (iter->type != GIT_ITERATOR_TYPE_WORKDIR)
		return false;

	if (wi->is_ignored != GIT_IGNORE_UNCHECKED)
		return (bool)(wi->is_ignored == GIT_IGNORE_TRUE);

	workdir_iterator_update_is_ignored(wi);

	return (bool)(wi->is_ignored == GIT_IGNORE_TRUE);
}

bool git_iterator_current_tree_is_ignored(git_iterator *iter)
{
	workdir_iterator *wi = (workdir_iterator *)iter;

	if (iter->type != GIT_ITERATOR_TYPE_WORKDIR)
		return false;

	return (bool)(wi->fi.stack->is_ignored == GIT_IGNORE_TRUE);
}

int git_iterator_cmp(git_iterator *iter, const char *path_prefix)
{
	const git_index_entry *entry;

	/* a "done" iterator is after every prefix */
	if (git_iterator_current(&entry, iter) < 0 || entry == NULL)
		return 1;

	/* a NULL prefix is after any valid iterator */
	if (!path_prefix)
		return -1;

	return iter->prefixcomp(entry->path, path_prefix);
}

int git_iterator_current_workdir_path(git_buf **path, git_iterator *iter)
{
	workdir_iterator *wi = (workdir_iterator *)iter;

	if (iter->type != GIT_ITERATOR_TYPE_WORKDIR || !wi->fi.entry.path)
		*path = NULL;
	else
		*path = &wi->fi.path;

	return 0;
}

int git_iterator_advance_over_with_status(
	const git_index_entry **entryptr,
	git_iterator_status_t *status,
	git_iterator *iter)
{
	int error = 0;
	workdir_iterator *wi = (workdir_iterator *)iter;
	char *base = NULL;
	const git_index_entry *entry;

	*status = GIT_ITERATOR_STATUS_NORMAL;

	if (iter->type != GIT_ITERATOR_TYPE_WORKDIR)
		return git_iterator_advance(entryptr, iter);
	if ((error = git_iterator_current(&entry, iter)) < 0)
		return error;

	if (!S_ISDIR(entry->mode)) {
		workdir_iterator_update_is_ignored(wi);
		if (wi->is_ignored == GIT_IGNORE_TRUE)
			*status = GIT_ITERATOR_STATUS_IGNORED;
		return git_iterator_advance(entryptr, iter);
	}

	*status = GIT_ITERATOR_STATUS_EMPTY;

	base = git__strdup(entry->path);
	GITERR_CHECK_ALLOC(base);

	/* scan inside directory looking for a non-ignored item */
	while (entry && !iter->prefixcomp(entry->path, base)) {
		workdir_iterator_update_is_ignored(wi);

		/* if we found an explicitly ignored item, then update from
		 * EMPTY to IGNORED
		 */
		if (wi->is_ignored == GIT_IGNORE_TRUE)
			*status = GIT_ITERATOR_STATUS_IGNORED;
		else if (S_ISDIR(entry->mode)) {
			error = git_iterator_advance_into(&entry, iter);

			if (!error)
				continue;
			else if (error == GIT_ENOTFOUND) {
				error = 0;
				wi->is_ignored = GIT_IGNORE_TRUE; /* mark empty dirs ignored */
			} else
				break; /* real error, stop here */
		} else {
			/* we found a non-ignored item, treat parent as untracked */
			*status = GIT_ITERATOR_STATUS_NORMAL;
			break;
		}

		if ((error = git_iterator_advance(&entry, iter)) < 0)
			break;
	}

	/* wrap up scan back to base directory */
	while (entry && !iter->prefixcomp(entry->path, base))
		if ((error = git_iterator_advance(&entry, iter)) < 0)
			break;

	*entryptr = entry;
	git__free(base);

	return error;
}

