/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <assert.h>

#include "checkout.h"

#include "git2/repository.h"
#include "git2/refs.h"
#include "git2/tree.h"
#include "git2/blob.h"
#include "git2/config.h"
#include "git2/diff.h"
#include "git2/submodule.h"
#include "git2/sys/index.h"

#include "refs.h"
#include "repository.h"
#include "index.h"
#include "filter.h"
#include "blob.h"
#include "diff.h"
#include "pathspec.h"
#include "buf_text.h"
#include "merge_file.h"
#include "path.h"

/* See docs/checkout-internals.md for more information */

enum {
	CHECKOUT_ACTION__NONE = 0,
	CHECKOUT_ACTION__REMOVE = 1,
	CHECKOUT_ACTION__UPDATE_BLOB = 2,
	CHECKOUT_ACTION__UPDATE_SUBMODULE = 4,
	CHECKOUT_ACTION__CONFLICT = 8,
	CHECKOUT_ACTION__UPDATE_CONFLICT = 16,
	CHECKOUT_ACTION__MAX = 16,
	CHECKOUT_ACTION__DEFER_REMOVE = 32,
	CHECKOUT_ACTION__REMOVE_AND_UPDATE =
		(CHECKOUT_ACTION__UPDATE_BLOB | CHECKOUT_ACTION__REMOVE),
};

typedef struct {
	git_repository *repo;
	git_diff *diff;
	git_checkout_options opts;
	bool opts_free_baseline;
	char *pfx;
	git_index *index;
	git_pool pool;
	git_vector removes;
	git_vector conflicts;
	git_buf path;
	size_t workdir_len;
	git_buf tmp;
	unsigned int strategy;
	int can_symlink;
	bool reload_submodules;
	size_t total_steps;
	size_t completed_steps;
} checkout_data;

typedef struct {
	const git_index_entry *ancestor;
	const git_index_entry *ours;
	const git_index_entry *theirs;

	int name_collision:1,
		directoryfile:1,
		one_to_two:1,
		binary:1,
		submodule:1;
} checkout_conflictdata;

static int checkout_notify(
	checkout_data *data,
	git_checkout_notify_t why,
	const git_diff_delta *delta,
	const git_index_entry *wditem)
{
	git_diff_file wdfile;
	const git_diff_file *baseline = NULL, *target = NULL, *workdir = NULL;
	const char *path = NULL;

	if (!data->opts.notify_cb ||
		(why & data->opts.notify_flags) == 0)
		return 0;

	if (wditem) {
		memset(&wdfile, 0, sizeof(wdfile));

		git_oid_cpy(&wdfile.id, &wditem->id);
		wdfile.path = wditem->path;
		wdfile.size = wditem->file_size;
		wdfile.flags = GIT_DIFF_FLAG_VALID_ID;
		wdfile.mode = wditem->mode;

		workdir = &wdfile;

		path = wditem->path;
	}

	if (delta) {
		switch (delta->status) {
		case GIT_DELTA_UNMODIFIED:
		case GIT_DELTA_MODIFIED:
		case GIT_DELTA_TYPECHANGE:
		default:
			baseline = &delta->old_file;
			target = &delta->new_file;
			break;
		case GIT_DELTA_ADDED:
		case GIT_DELTA_IGNORED:
		case GIT_DELTA_UNTRACKED:
			target = &delta->new_file;
			break;
		case GIT_DELTA_DELETED:
			baseline = &delta->old_file;
			break;
		}

		path = delta->old_file.path;
	}

	{
		int error = data->opts.notify_cb(
			why, path, baseline, target, workdir, data->opts.notify_payload);

		return giterr_set_after_callback_function(
			error, "git_checkout notification");
	}
}

static bool checkout_is_workdir_modified(
	checkout_data *data,
	const git_diff_file *baseitem,
	const git_index_entry *wditem)
{
	git_oid oid;
	const git_index_entry *ie;

	/* handle "modified" submodule */
	if (wditem->mode == GIT_FILEMODE_COMMIT) {
		git_submodule *sm;
		unsigned int sm_status = 0;
		const git_oid *sm_oid = NULL;
		bool rval = false;

		if (git_submodule_lookup(&sm, data->repo, wditem->path) < 0) {
			giterr_clear();
			return true;
		}

		if (git_submodule_status(&sm_status, sm) < 0 ||
			GIT_SUBMODULE_STATUS_IS_WD_DIRTY(sm_status))
			rval = true;
		else if ((sm_oid = git_submodule_wd_id(sm)) == NULL)
			rval = false;
		else
			rval = (git_oid__cmp(&baseitem->id, sm_oid) != 0);

		git_submodule_free(sm);
		return rval;
	}

	/* Look at the cache to decide if the workdir is modified.  If not,
	 * we can simply compare the oid in the cache to the baseitem instead
	 * of hashing the file.
	 */
	if ((ie = git_index_get_bypath(data->index, wditem->path, 0)) != NULL) {
		if (wditem->mtime.seconds == ie->mtime.seconds &&
			wditem->mtime.nanoseconds == ie->mtime.nanoseconds &&
			wditem->file_size == ie->file_size)
			return (git_oid__cmp(&baseitem->id, &ie->id) != 0);
	}

	/* depending on where base is coming from, we may or may not know
	 * the actual size of the data, so we can't rely on this shortcut.
	 */
	if (baseitem->size && wditem->file_size != baseitem->size)
		return true;

	if (git_diff__oid_for_entry(&oid, data->diff, wditem, NULL) < 0)
		return false;

	return (git_oid__cmp(&baseitem->id, &oid) != 0);
}

#define CHECKOUT_ACTION_IF(FLAG,YES,NO) \
	((data->strategy & GIT_CHECKOUT_##FLAG) ? CHECKOUT_ACTION__##YES : CHECKOUT_ACTION__##NO)

static int checkout_action_common(
	int *action,
	checkout_data *data,
	const git_diff_delta *delta,
	const git_index_entry *wd)
{
	git_checkout_notify_t notify = GIT_CHECKOUT_NOTIFY_NONE;

	if ((data->strategy & GIT_CHECKOUT_UPDATE_ONLY) != 0)
		*action = (*action & ~CHECKOUT_ACTION__REMOVE);

	if ((*action & CHECKOUT_ACTION__UPDATE_BLOB) != 0) {
		if (S_ISGITLINK(delta->new_file.mode))
			*action = (*action & ~CHECKOUT_ACTION__UPDATE_BLOB) |
				CHECKOUT_ACTION__UPDATE_SUBMODULE;

		/* to "update" a symlink, we must remove the old one first */
		if (delta->new_file.mode == GIT_FILEMODE_LINK && wd != NULL)
			*action |= CHECKOUT_ACTION__REMOVE;

		notify = GIT_CHECKOUT_NOTIFY_UPDATED;
	}

	if ((*action & CHECKOUT_ACTION__CONFLICT) != 0)
		notify = GIT_CHECKOUT_NOTIFY_CONFLICT;

	return checkout_notify(data, notify, delta, wd);
}

static int checkout_action_no_wd(
	int *action,
	checkout_data *data,
	const git_diff_delta *delta)
{
	int error = 0;

	*action = CHECKOUT_ACTION__NONE;

	switch (delta->status) {
	case GIT_DELTA_UNMODIFIED: /* case 12 */
		error = checkout_notify(data, GIT_CHECKOUT_NOTIFY_DIRTY, delta, NULL);
		if (error)
			return error;
		*action = CHECKOUT_ACTION_IF(SAFE_CREATE, UPDATE_BLOB, NONE);
		break;
	case GIT_DELTA_ADDED:    /* case 2 or 28 (and 5 but not really) */
		*action = CHECKOUT_ACTION_IF(SAFE, UPDATE_BLOB, NONE);
		break;
	case GIT_DELTA_MODIFIED: /* case 13 (and 35 but not really) */
		*action = CHECKOUT_ACTION_IF(SAFE_CREATE, UPDATE_BLOB, CONFLICT);
		break;
	case GIT_DELTA_TYPECHANGE: /* case 21 (B->T) and 28 (T->B)*/
		if (delta->new_file.mode == GIT_FILEMODE_TREE)
			*action = CHECKOUT_ACTION_IF(SAFE, UPDATE_BLOB, NONE);
		break;
	case GIT_DELTA_DELETED: /* case 8 or 25 */
		*action = CHECKOUT_ACTION_IF(SAFE, REMOVE, NONE);
		break;
	default: /* impossible */
		break;
	}

	return checkout_action_common(action, data, delta, NULL);
}

static bool wd_item_is_removable(git_iterator *iter, const git_index_entry *wd)
{
	git_buf *full = NULL;

	if (wd->mode != GIT_FILEMODE_TREE)
		return true;
	if (git_iterator_current_workdir_path(&full, iter) < 0)
		return true;
	return !full || !git_path_contains(full, DOT_GIT);
}

static int checkout_queue_remove(checkout_data *data, const char *path)
{
	char *copy = git_pool_strdup(&data->pool, path);
	GITERR_CHECK_ALLOC(copy);
	return git_vector_insert(&data->removes, copy);
}

/* note that this advances the iterator over the wd item */
static int checkout_action_wd_only(
	checkout_data *data,
	git_iterator *workdir,
	const git_index_entry **wditem,
	git_vector *pathspec)
{
	int error = 0;
	bool remove = false;
	git_checkout_notify_t notify = GIT_CHECKOUT_NOTIFY_NONE;
	const git_index_entry *wd = *wditem;

	if (!git_pathspec__match(
			pathspec, wd->path,
			(data->strategy & GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH) != 0,
			git_iterator_ignore_case(workdir), NULL, NULL))
		return git_iterator_advance(wditem, workdir);

	/* check if item is tracked in the index but not in the checkout diff */
	if (data->index != NULL) {
		size_t pos;

		error = git_index__find_pos(
			&pos, data->index, wd->path, 0, GIT_INDEX_STAGE_ANY);

		if (wd->mode != GIT_FILEMODE_TREE) {
			if (!error) { /* found by git_index__find_pos call */
				notify = GIT_CHECKOUT_NOTIFY_DIRTY;
				remove = ((data->strategy & GIT_CHECKOUT_FORCE) != 0);
			} else if (error != GIT_ENOTFOUND)
				return error;
			else
				error = 0; /* git_index__find_pos does not set error msg */
		} else {
			/* for tree entries, we have to see if there are any index
			 * entries that are contained inside that tree
			 */
			const git_index_entry *e = git_index_get_byindex(data->index, pos);

			if (e != NULL && data->diff->pfxcomp(e->path, wd->path) == 0) {
				notify = GIT_CHECKOUT_NOTIFY_DIRTY;
				remove = ((data->strategy & GIT_CHECKOUT_FORCE) != 0);
			}
		}
	}

	if (notify != GIT_CHECKOUT_NOTIFY_NONE) {
		/* if we found something in the index, notify and advance */
		if ((error = checkout_notify(data, notify, NULL, wd)) != 0)
			return error;

		if (remove && wd_item_is_removable(workdir, wd))
			error = checkout_queue_remove(data, wd->path);

		if (!error)
			error = git_iterator_advance(wditem, workdir);
	} else {
		/* untracked or ignored - can't know which until we advance through */
		bool over = false, removable = wd_item_is_removable(workdir, wd);
		git_iterator_status_t untracked_state;

		/* copy the entry for issuing notification callback later */
		git_index_entry saved_wd = *wd;
		git_buf_sets(&data->tmp, wd->path);
		saved_wd.path = data->tmp.ptr;

		error = git_iterator_advance_over_with_status(
			wditem, &untracked_state, workdir);
		if (error == GIT_ITEROVER)
			over = true;
		else if (error < 0)
			return error;

		if (untracked_state == GIT_ITERATOR_STATUS_IGNORED) {
			notify = GIT_CHECKOUT_NOTIFY_IGNORED;
			remove = ((data->strategy & GIT_CHECKOUT_REMOVE_IGNORED) != 0);
		} else {
			notify = GIT_CHECKOUT_NOTIFY_UNTRACKED;
			remove = ((data->strategy & GIT_CHECKOUT_REMOVE_UNTRACKED) != 0);
		}

		if ((error = checkout_notify(data, notify, NULL, &saved_wd)) != 0)
			return error;

		if (remove && removable)
			error = checkout_queue_remove(data, saved_wd.path);

		if (!error && over) /* restore ITEROVER if needed */
			error = GIT_ITEROVER;
	}

	return error;
}

static bool submodule_is_config_only(
	checkout_data *data,
	const char *path)
{
	git_submodule *sm = NULL;
	unsigned int sm_loc = 0;
	bool rval = false;

	if (git_submodule_lookup(&sm, data->repo, path) < 0)
		return true;

	if (git_submodule_location(&sm_loc, sm) < 0 ||
		sm_loc == GIT_SUBMODULE_STATUS_IN_CONFIG)
		rval = true;

	git_submodule_free(sm);

	return rval;
}

static int checkout_action_with_wd(
	int *action,
	checkout_data *data,
	const git_diff_delta *delta,
	git_iterator *workdir,
	const git_index_entry *wd)
{
	*action = CHECKOUT_ACTION__NONE;

	switch (delta->status) {
	case GIT_DELTA_UNMODIFIED: /* case 14/15 or 33 */
		if (checkout_is_workdir_modified(data, &delta->old_file, wd)) {
			GITERR_CHECK_ERROR(
				checkout_notify(data, GIT_CHECKOUT_NOTIFY_DIRTY, delta, wd) );
			*action = CHECKOUT_ACTION_IF(FORCE, UPDATE_BLOB, NONE);
		}
		break;
	case GIT_DELTA_ADDED: /* case 3, 4 or 6 */
		if (git_iterator_current_is_ignored(workdir))
			*action = CHECKOUT_ACTION_IF(DONT_OVERWRITE_IGNORED, CONFLICT, UPDATE_BLOB);
		else
			*action = CHECKOUT_ACTION_IF(FORCE, UPDATE_BLOB, CONFLICT);
		break;
	case GIT_DELTA_DELETED: /* case 9 or 10 (or 26 but not really) */
		if (checkout_is_workdir_modified(data, &delta->old_file, wd))
			*action = CHECKOUT_ACTION_IF(FORCE, REMOVE, CONFLICT);
		else
			*action = CHECKOUT_ACTION_IF(SAFE, REMOVE, NONE);
		break;
	case GIT_DELTA_MODIFIED: /* case 16, 17, 18 (or 36 but not really) */
		if (checkout_is_workdir_modified(data, &delta->old_file, wd))
			*action = CHECKOUT_ACTION_IF(FORCE, UPDATE_BLOB, CONFLICT);
		else
			*action = CHECKOUT_ACTION_IF(SAFE, UPDATE_BLOB, NONE);
		break;
	case GIT_DELTA_TYPECHANGE: /* case 22, 23, 29, 30 */
		if (delta->old_file.mode == GIT_FILEMODE_TREE) {
			if (wd->mode == GIT_FILEMODE_TREE)
				/* either deleting items in old tree will delete the wd dir,
				 * or we'll get a conflict when we attempt blob update...
				 */
				*action = CHECKOUT_ACTION_IF(SAFE, UPDATE_BLOB, NONE);
			else if (wd->mode == GIT_FILEMODE_COMMIT) {
				/* workdir is possibly a "phantom" submodule - treat as a
				 * tree if the only submodule info came from the config
				 */
				if (submodule_is_config_only(data, wd->path))
					*action = CHECKOUT_ACTION_IF(SAFE, UPDATE_BLOB, NONE);
				else
					*action = CHECKOUT_ACTION_IF(FORCE, REMOVE_AND_UPDATE, CONFLICT);
			} else
				*action = CHECKOUT_ACTION_IF(FORCE, REMOVE, CONFLICT);
		}
		else if (checkout_is_workdir_modified(data, &delta->old_file, wd))
			*action = CHECKOUT_ACTION_IF(FORCE, REMOVE_AND_UPDATE, CONFLICT);
		else
			*action = CHECKOUT_ACTION_IF(SAFE, REMOVE_AND_UPDATE, NONE);

		/* don't update if the typechange is to a tree */
		if (delta->new_file.mode == GIT_FILEMODE_TREE)
			*action = (*action & ~CHECKOUT_ACTION__UPDATE_BLOB);
		break;
	default: /* impossible */
		break;
	}

	return checkout_action_common(action, data, delta, wd);
}

static int checkout_action_with_wd_blocker(
	int *action,
	checkout_data *data,
	const git_diff_delta *delta,
	const git_index_entry *wd)
{
	*action = CHECKOUT_ACTION__NONE;

	switch (delta->status) {
	case GIT_DELTA_UNMODIFIED:
		/* should show delta as dirty / deleted */
		GITERR_CHECK_ERROR(
			checkout_notify(data, GIT_CHECKOUT_NOTIFY_DIRTY, delta, wd) );
		*action = CHECKOUT_ACTION_IF(FORCE, REMOVE_AND_UPDATE, NONE);
		break;
	case GIT_DELTA_ADDED:
	case GIT_DELTA_MODIFIED:
		*action = CHECKOUT_ACTION_IF(FORCE, REMOVE_AND_UPDATE, CONFLICT);
		break;
	case GIT_DELTA_DELETED:
		*action = CHECKOUT_ACTION_IF(FORCE, REMOVE, CONFLICT);
		break;
	case GIT_DELTA_TYPECHANGE:
		/* not 100% certain about this... */
		*action = CHECKOUT_ACTION_IF(FORCE, REMOVE_AND_UPDATE, CONFLICT);
		break;
	default: /* impossible */
		break;
	}

	return checkout_action_common(action, data, delta, wd);
}

static int checkout_action_with_wd_dir(
	int *action,
	checkout_data *data,
	const git_diff_delta *delta,
	git_iterator *workdir,
	const git_index_entry *wd)
{
	*action = CHECKOUT_ACTION__NONE;

	switch (delta->status) {
	case GIT_DELTA_UNMODIFIED: /* case 19 or 24 (or 34 but not really) */
		GITERR_CHECK_ERROR(
			checkout_notify(data, GIT_CHECKOUT_NOTIFY_DIRTY, delta, NULL));
		GITERR_CHECK_ERROR(
			checkout_notify(data, GIT_CHECKOUT_NOTIFY_UNTRACKED, NULL, wd));
		break;
	case GIT_DELTA_ADDED:/* case 4 (and 7 for dir) */
	case GIT_DELTA_MODIFIED: /* case 20 (or 37 but not really) */
		if (delta->old_file.mode == GIT_FILEMODE_COMMIT)
			/* expected submodule (and maybe found one) */;
		else if (delta->new_file.mode != GIT_FILEMODE_TREE)
			*action = git_iterator_current_is_ignored(workdir) ?
				CHECKOUT_ACTION_IF(DONT_OVERWRITE_IGNORED, CONFLICT, REMOVE_AND_UPDATE) :
				CHECKOUT_ACTION_IF(FORCE, REMOVE_AND_UPDATE, CONFLICT);
		break;
	case GIT_DELTA_DELETED: /* case 11 (and 27 for dir) */
		if (delta->old_file.mode != GIT_FILEMODE_TREE)
			GITERR_CHECK_ERROR(
				checkout_notify(data, GIT_CHECKOUT_NOTIFY_UNTRACKED, NULL, wd));
		break;
	case GIT_DELTA_TYPECHANGE: /* case 24 or 31 */
		if (delta->old_file.mode == GIT_FILEMODE_TREE) {
			/* For typechange from dir, remove dir and add blob, but it is
			 * not safe to remove dir if it contains modified files.
			 * However, safely removing child files will remove the parent
			 * directory if is it left empty, so we can defer removing the
			 * dir and it will succeed if no children are left.
			 */
			*action = CHECKOUT_ACTION_IF(SAFE, UPDATE_BLOB, NONE);
			if (*action != CHECKOUT_ACTION__NONE)
				*action |= CHECKOUT_ACTION__DEFER_REMOVE;
		}
		else if (delta->new_file.mode != GIT_FILEMODE_TREE)
			/* For typechange to dir, dir is already created so no action */
			*action = CHECKOUT_ACTION_IF(FORCE, REMOVE_AND_UPDATE, CONFLICT);
		break;
	default: /* impossible */
		break;
	}

	return checkout_action_common(action, data, delta, wd);
}

static int checkout_action(
	int *action,
	checkout_data *data,
	git_diff_delta *delta,
	git_iterator *workdir,
	const git_index_entry **wditem,
	git_vector *pathspec)
{
	int cmp = -1, error;
	int (*strcomp)(const char *, const char *) = data->diff->strcomp;
	int (*pfxcomp)(const char *str, const char *pfx) = data->diff->pfxcomp;
	int (*advance)(const git_index_entry **, git_iterator *) = NULL;

	/* move workdir iterator to follow along with deltas */

	while (1) {
		const git_index_entry *wd = *wditem;

		if (!wd)
			return checkout_action_no_wd(action, data, delta);

		cmp = strcomp(wd->path, delta->old_file.path);

		/* 1. wd before delta ("a/a" before "a/b")
		 * 2. wd prefixes delta & should expand ("a/" before "a/b")
		 * 3. wd prefixes delta & cannot expand ("a/b" before "a/b/c")
		 * 4. wd equals delta ("a/b" and "a/b")
		 * 5. wd after delta & delta prefixes wd ("a/b/c" after "a/b/" or "a/b")
		 * 6. wd after delta ("a/c" after "a/b")
		 */

		if (cmp < 0) {
			cmp = pfxcomp(delta->old_file.path, wd->path);

			if (cmp == 0) {
				if (wd->mode == GIT_FILEMODE_TREE) {
					/* case 2 - entry prefixed by workdir tree */
					error = git_iterator_advance_into_or_over(wditem, workdir);
					if (error < 0 && error != GIT_ITEROVER)
						goto done;
					continue;
				}

				/* case 3 maybe - wd contains non-dir where dir expected */
				if (delta->old_file.path[strlen(wd->path)] == '/') {
					error = checkout_action_with_wd_blocker(
						action, data, delta, wd);
					advance = git_iterator_advance;
					goto done;
				}
			}

			/* case 1 - handle wd item (if it matches pathspec) */
			error = checkout_action_wd_only(data, workdir, wditem, pathspec);
			if (error && error != GIT_ITEROVER)
				goto done;
			continue;
		}

		if (cmp == 0) {
			/* case 4 */
			error = checkout_action_with_wd(action, data, delta, workdir, wd);
			advance = git_iterator_advance;
			goto done;
		}

		cmp = pfxcomp(wd->path, delta->old_file.path);

		if (cmp == 0) { /* case 5 */
			if (wd->path[strlen(delta->old_file.path)] != '/')
				return checkout_action_no_wd(action, data, delta);

			if (delta->status == GIT_DELTA_TYPECHANGE) {
				if (delta->old_file.mode == GIT_FILEMODE_TREE) {
					error = checkout_action_with_wd(action, data, delta, workdir, wd);
					advance = git_iterator_advance_into;
					goto done;
				}

				if (delta->new_file.mode == GIT_FILEMODE_TREE ||
					delta->new_file.mode == GIT_FILEMODE_COMMIT ||
					delta->old_file.mode == GIT_FILEMODE_COMMIT)
				{
					error = checkout_action_with_wd(action, data, delta, workdir, wd);
					advance = git_iterator_advance;
					goto done;
				}
			}

			return checkout_action_with_wd_dir(action, data, delta, workdir, wd);
		}

		/* case 6 - wd is after delta */
		return checkout_action_no_wd(action, data, delta);
	}

done:
	if (!error && advance != NULL &&
		(error = advance(wditem, workdir)) < 0) {
		*wditem = NULL;
		if (error == GIT_ITEROVER)
			error = 0;
	}

	return error;
}

static int checkout_remaining_wd_items(
	checkout_data *data,
	git_iterator *workdir,
	const git_index_entry *wd,
	git_vector *spec)
{
	int error = 0;

	while (wd && !error)
		error = checkout_action_wd_only(data, workdir, &wd, spec);

	if (error == GIT_ITEROVER)
		error = 0;

	return error;
}

GIT_INLINE(int) checkout_idxentry_cmp(
	const git_index_entry *a,
	const git_index_entry *b)
{
	if (!a && !b)
		return 0;
	else if (!a && b)
		return -1;
	else if(a && !b)
		return 1;
	else
		return strcmp(a->path, b->path);
}

static int checkout_conflictdata_cmp(const void *a, const void *b)
{
	const checkout_conflictdata *ca = a;
	const checkout_conflictdata *cb = b;
	int diff;

	if ((diff = checkout_idxentry_cmp(ca->ancestor, cb->ancestor)) == 0 &&
		(diff = checkout_idxentry_cmp(ca->ours, cb->theirs)) == 0)
		diff = checkout_idxentry_cmp(ca->theirs, cb->theirs);

	return diff;
}

int checkout_conflictdata_empty(
	const git_vector *conflicts, size_t idx, void *payload)
{
	checkout_conflictdata *conflict;

	GIT_UNUSED(payload);

	if ((conflict = git_vector_get(conflicts, idx)) == NULL)
		return -1;

	if (conflict->ancestor || conflict->ours || conflict->theirs)
		return 0;

	git__free(conflict);
	return 1;
}

GIT_INLINE(bool) conflict_pathspec_match(
	checkout_data *data,
	git_iterator *workdir,
	git_vector *pathspec,
	const git_index_entry *ancestor,
	const git_index_entry *ours,
	const git_index_entry *theirs)
{
	/* if the pathspec matches ours *or* theirs, proceed */
	if (ours && git_pathspec__match(pathspec, ours->path,
		(data->strategy & GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH) != 0,
		git_iterator_ignore_case(workdir), NULL, NULL))
		return true;

	if (theirs && git_pathspec__match(pathspec, theirs->path,
		(data->strategy & GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH) != 0,
		git_iterator_ignore_case(workdir), NULL, NULL))
		return true;

	if (ancestor && git_pathspec__match(pathspec, ancestor->path,
		(data->strategy & GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH) != 0,
		git_iterator_ignore_case(workdir), NULL, NULL))
		return true;

	return false;
}

GIT_INLINE(int) checkout_conflict_detect_submodule(checkout_conflictdata *conflict)
{
	conflict->submodule = ((conflict->ancestor && S_ISGITLINK(conflict->ancestor->mode)) ||
		(conflict->ours && S_ISGITLINK(conflict->ours->mode)) ||
		(conflict->theirs && S_ISGITLINK(conflict->theirs->mode)));
	return 0;
}

GIT_INLINE(int) checkout_conflict_detect_binary(git_repository *repo, checkout_conflictdata *conflict)
{
	git_blob *ancestor_blob = NULL, *our_blob = NULL, *their_blob = NULL;
	int error = 0;

	if (conflict->submodule)
		return 0;

	if (conflict->ancestor) {
		if ((error = git_blob_lookup(&ancestor_blob, repo, &conflict->ancestor->id)) < 0)
			goto done;

		conflict->binary = git_blob_is_binary(ancestor_blob);
	}

	if (!conflict->binary && conflict->ours) {
		if ((error = git_blob_lookup(&our_blob, repo, &conflict->ours->id)) < 0)
			goto done;

		conflict->binary = git_blob_is_binary(our_blob);
	}

	if (!conflict->binary && conflict->theirs) {
		if ((error = git_blob_lookup(&their_blob, repo, &conflict->theirs->id)) < 0)
			goto done;

		conflict->binary = git_blob_is_binary(their_blob);
	}

done:
	git_blob_free(ancestor_blob);
	git_blob_free(our_blob);
	git_blob_free(their_blob);

	return error;
}

static int checkout_conflicts_load(checkout_data *data, git_iterator *workdir, git_vector *pathspec)
{
	git_index_conflict_iterator *iterator = NULL;
	const git_index_entry *ancestor, *ours, *theirs;
	checkout_conflictdata *conflict;
	int error = 0;

	if ((error = git_index_conflict_iterator_new(&iterator, data->index)) < 0)
		goto done;

	data->conflicts._cmp = checkout_conflictdata_cmp;

	/* Collect the conflicts */
	while ((error = git_index_conflict_next(&ancestor, &ours, &theirs, iterator)) == 0) {
		if (!conflict_pathspec_match(data, workdir, pathspec, ancestor, ours, theirs))
			continue;

		conflict = git__calloc(1, sizeof(checkout_conflictdata));
		GITERR_CHECK_ALLOC(conflict);

		conflict->ancestor = ancestor;
		conflict->ours = ours;
		conflict->theirs = theirs;

		if ((error = checkout_conflict_detect_submodule(conflict)) < 0 ||
		    (error = checkout_conflict_detect_binary(data->repo, conflict)) < 0)
		{
			git__free(conflict);
			goto done;
		}

		git_vector_insert(&data->conflicts, conflict);
	}

	if (error == GIT_ITEROVER)
		error = 0;

done:
	git_index_conflict_iterator_free(iterator);

	return error;
}

GIT_INLINE(int) checkout_conflicts_cmp_entry(
	const char *path,
	const git_index_entry *entry)
{
	return strcmp((const char *)path, entry->path);
}

static int checkout_conflicts_cmp_ancestor(const void *p, const void *c)
{
	const char *path = p;
	const checkout_conflictdata *conflict = c;

	if (!conflict->ancestor)
		return 1;

	return checkout_conflicts_cmp_entry(path, conflict->ancestor);
}

static checkout_conflictdata *checkout_conflicts_search_ancestor(
	checkout_data *data,
	const char *path)
{
	size_t pos;

	if (git_vector_bsearch2(&pos, &data->conflicts, checkout_conflicts_cmp_ancestor, path) < 0)
		return NULL;

	return git_vector_get(&data->conflicts, pos);
}

static checkout_conflictdata *checkout_conflicts_search_branch(
	checkout_data *data,
	const char *path)
{
	checkout_conflictdata *conflict;
	size_t i;

	git_vector_foreach(&data->conflicts, i, conflict) {
		int cmp = -1;

		if (conflict->ancestor)
			break;

		if (conflict->ours)
			cmp = checkout_conflicts_cmp_entry(path, conflict->ours);
		else if (conflict->theirs)
			cmp = checkout_conflicts_cmp_entry(path, conflict->theirs);

		if (cmp == 0)
			return conflict;
	}

	return NULL;
}

static int checkout_conflicts_load_byname_entry(
	checkout_conflictdata **ancestor_out,
	checkout_conflictdata **ours_out,
	checkout_conflictdata **theirs_out,
	checkout_data *data,
	const git_index_name_entry *name_entry)
{
	checkout_conflictdata *ancestor, *ours = NULL, *theirs = NULL;
	int error = 0;

	*ancestor_out = NULL;
	*ours_out = NULL;
	*theirs_out = NULL;

	if (!name_entry->ancestor) {
		giterr_set(GITERR_INDEX, "A NAME entry exists without an ancestor");
		error = -1;
		goto done;
	}

	if (!name_entry->ours && !name_entry->theirs) {
		giterr_set(GITERR_INDEX, "A NAME entry exists without an ours or theirs");
		error = -1;
		goto done;
	}

	if ((ancestor = checkout_conflicts_search_ancestor(data,
		name_entry->ancestor)) == NULL) {
		giterr_set(GITERR_INDEX,
			"A NAME entry referenced ancestor entry '%s' which does not exist in the main index",
			name_entry->ancestor);
		error = -1;
		goto done;
	}

	if (name_entry->ours) {
		if (strcmp(name_entry->ancestor, name_entry->ours) == 0)
			ours = ancestor;
		else if ((ours = checkout_conflicts_search_branch(data, name_entry->ours)) == NULL ||
			ours->ours == NULL) {
			giterr_set(GITERR_INDEX,
				"A NAME entry referenced our entry '%s' which does not exist in the main index",
				name_entry->ours);
			error = -1;
			goto done;
		}
	}

	if (name_entry->theirs) {
		if (strcmp(name_entry->ancestor, name_entry->theirs) == 0)
			theirs = ancestor;
		else if (name_entry->ours && strcmp(name_entry->ours, name_entry->theirs) == 0)
			theirs = ours;
		else if ((theirs = checkout_conflicts_search_branch(data, name_entry->theirs)) == NULL ||
			theirs->theirs == NULL) {
			giterr_set(GITERR_INDEX,
				"A NAME entry referenced their entry '%s' which does not exist in the main index",
				name_entry->theirs);
			error = -1;
			goto done;
		}
	}

	*ancestor_out = ancestor;
	*ours_out = ours;
	*theirs_out = theirs;

done:
	return error;
}

static int checkout_conflicts_coalesce_renames(
	checkout_data *data)
{
	const git_index_name_entry *name_entry;
	checkout_conflictdata *ancestor_conflict, *our_conflict, *their_conflict;
	size_t i, names;
	int error = 0;

	/* Juggle entries based on renames */
	names = git_index_name_entrycount(data->index);

	for (i = 0; i < names; i++) {
		name_entry = git_index_name_get_byindex(data->index, i);

		if ((error = checkout_conflicts_load_byname_entry(
			&ancestor_conflict, &our_conflict, &their_conflict,
			data, name_entry)) < 0)
			goto done;

		if (our_conflict && our_conflict != ancestor_conflict) {
			ancestor_conflict->ours = our_conflict->ours;
			our_conflict->ours = NULL;

			if (our_conflict->theirs)
				our_conflict->name_collision = 1;

			if (our_conflict->name_collision)
				ancestor_conflict->name_collision = 1;
		}

		if (their_conflict && their_conflict != ancestor_conflict) {
			ancestor_conflict->theirs = their_conflict->theirs;
			their_conflict->theirs = NULL;

			if (their_conflict->ours)
				their_conflict->name_collision = 1;

			if (their_conflict->name_collision)
				ancestor_conflict->name_collision = 1;
		}

		if (our_conflict && our_conflict != ancestor_conflict &&
			their_conflict && their_conflict != ancestor_conflict)
			ancestor_conflict->one_to_two = 1;
	}

	git_vector_remove_matching(
		&data->conflicts, checkout_conflictdata_empty, NULL);

done:
	return error;
}

static int checkout_conflicts_mark_directoryfile(
	checkout_data *data)
{
	checkout_conflictdata *conflict;
	const git_index_entry *entry;
	size_t i, j, len;
	const char *path;
	int prefixed, error = 0;

	len = git_index_entrycount(data->index);

	/* Find d/f conflicts */
	git_vector_foreach(&data->conflicts, i, conflict) {
		if ((conflict->ours && conflict->theirs) ||
			(!conflict->ours && !conflict->theirs))
			continue;

		path = conflict->ours ?
			conflict->ours->path : conflict->theirs->path;

		if ((error = git_index_find(&j, data->index, path)) < 0) {
			if (error == GIT_ENOTFOUND)
				giterr_set(GITERR_INDEX,
					"Index inconsistency, could not find entry for expected conflict '%s'", path);

			goto done;
		}

		for (; j < len; j++) {
			if ((entry = git_index_get_byindex(data->index, j)) == NULL) {
				giterr_set(GITERR_INDEX,
					"Index inconsistency, truncated index while loading expected conflict '%s'", path);
				error = -1;
				goto done;
			}

			prefixed = git_path_equal_or_prefixed(path, entry->path);

			if (prefixed == GIT_PATH_EQUAL)
				continue;

			if (prefixed == GIT_PATH_PREFIX)
				conflict->directoryfile = 1;

			break;
		}
	}

done:
	return error;
}

static int checkout_get_conflicts(
	checkout_data *data,
	git_iterator *workdir,
	git_vector *pathspec)
{
	int error = 0;

	if (data->strategy & GIT_CHECKOUT_SKIP_UNMERGED)
		return 0;

	if ((error = checkout_conflicts_load(data, workdir, pathspec)) < 0 ||
		(error = checkout_conflicts_coalesce_renames(data)) < 0 ||
		(error = checkout_conflicts_mark_directoryfile(data)) < 0)
		goto done;

done:
	return error;
}

static int checkout_get_actions(
	uint32_t **actions_ptr,
	size_t **counts_ptr,
	checkout_data *data,
	git_iterator *workdir)
{
	int error = 0, act;
	const git_index_entry *wditem;
	git_vector pathspec = GIT_VECTOR_INIT, *deltas;
	git_pool pathpool = GIT_POOL_INIT_STRINGPOOL;
	git_diff_delta *delta;
	size_t i, *counts = NULL;
	uint32_t *actions = NULL;

	if (data->opts.paths.count > 0 &&
		git_pathspec__vinit(&pathspec, &data->opts.paths, &pathpool) < 0)
		return -1;

	if ((error = git_iterator_current(&wditem, workdir)) < 0 &&
		error != GIT_ITEROVER)
		goto fail;

	deltas = &data->diff->deltas;

	*counts_ptr = counts = git__calloc(CHECKOUT_ACTION__MAX+1, sizeof(size_t));
	*actions_ptr = actions = git__calloc(
		deltas->length ? deltas->length : 1, sizeof(uint32_t));
	if (!counts || !actions) {
		error = -1;
		goto fail;
	}

	git_vector_foreach(deltas, i, delta) {
		error = checkout_action(&act, data, delta, workdir, &wditem, &pathspec);
		if (error != 0)
			goto fail;

		actions[i] = act;

		if (act & CHECKOUT_ACTION__REMOVE)
			counts[CHECKOUT_ACTION__REMOVE]++;
		if (act & CHECKOUT_ACTION__UPDATE_BLOB)
			counts[CHECKOUT_ACTION__UPDATE_BLOB]++;
		if (act & CHECKOUT_ACTION__UPDATE_SUBMODULE)
			counts[CHECKOUT_ACTION__UPDATE_SUBMODULE]++;
		if (act & CHECKOUT_ACTION__CONFLICT)
			counts[CHECKOUT_ACTION__CONFLICT]++;
	}

	error = checkout_remaining_wd_items(data, workdir, wditem, &pathspec);
	if (error)
		goto fail;

	counts[CHECKOUT_ACTION__REMOVE] += data->removes.length;

	if (counts[CHECKOUT_ACTION__CONFLICT] > 0 &&
		(data->strategy & GIT_CHECKOUT_ALLOW_CONFLICTS) == 0)
	{
		giterr_set(GITERR_CHECKOUT, "%d %s checkout",
			(int)counts[CHECKOUT_ACTION__CONFLICT],
			counts[CHECKOUT_ACTION__CONFLICT] == 1 ?
			"conflict prevents" : "conflicts prevent");
		error = GIT_EMERGECONFLICT;
		goto fail;
	}


	if ((error = checkout_get_conflicts(data, workdir, &pathspec)) < 0)
		goto fail;

	counts[CHECKOUT_ACTION__UPDATE_CONFLICT] = git_vector_length(&data->conflicts);

	git_pathspec__vfree(&pathspec);
	git_pool_clear(&pathpool);

	return 0;

fail:
	*counts_ptr = NULL;
	git__free(counts);
	*actions_ptr = NULL;
	git__free(actions);

	git_pathspec__vfree(&pathspec);
	git_pool_clear(&pathpool);

	return error;
}

static int buffer_to_file(
	struct stat *st,
	git_buf *buf,
	const char *path,
	mode_t dir_mode,
	int file_open_flags,
	mode_t file_mode)
{
	int error;

	if ((error = git_futils_mkpath2file(path, dir_mode)) < 0)
		return error;

	if ((error = git_futils_writebuffer(
			buf, path, file_open_flags, file_mode)) < 0)
		return error;

	if (st != NULL && (error = p_stat(path, st)) < 0)
		giterr_set(GITERR_OS, "Error statting '%s'", path);

	else if (GIT_PERMS_IS_EXEC(file_mode) &&
			(error = p_chmod(path, file_mode)) < 0)
		giterr_set(GITERR_OS, "Failed to set permissions on '%s'", path);

	return error;
}

static int blob_content_to_file(
	struct stat *st,
	git_blob *blob,
	const char *path,
	const char * hint_path,
	mode_t entry_filemode,
	git_checkout_options *opts)
{
	int error = 0;
	mode_t file_mode = opts->file_mode ? opts->file_mode : entry_filemode;
	git_buf out = GIT_BUF_INIT;
	git_filter_list *fl = NULL;

	if (hint_path == NULL)
		hint_path = path;

	if (!opts->disable_filters)
		error = git_filter_list_load(
			&fl, git_blob_owner(blob), blob, hint_path,
			GIT_FILTER_TO_WORKTREE, GIT_FILTER_OPT_DEFAULT);

	if (!error)
		error = git_filter_list_apply_to_blob(&out, fl, blob);

	git_filter_list_free(fl);

	if (!error) {
		error = buffer_to_file(
			st, &out, path, opts->dir_mode, opts->file_open_flags, file_mode);

		st->st_mode = entry_filemode;

		git_buf_free(&out);
	}

	return error;
}

static int blob_content_to_link(
	struct stat *st,
	git_blob *blob,
	const char *path,
	mode_t dir_mode,
	int can_symlink)
{
	git_buf linktarget = GIT_BUF_INIT;
	int error;

	if ((error = git_futils_mkpath2file(path, dir_mode)) < 0)
		return error;

	if ((error = git_blob__getbuf(&linktarget, blob)) < 0)
		return error;

	if (can_symlink) {
		if ((error = p_symlink(git_buf_cstr(&linktarget), path)) < 0)
			giterr_set(GITERR_OS, "Could not create symlink %s\n", path);
	} else {
		error = git_futils_fake_symlink(git_buf_cstr(&linktarget), path);
	}

	if (!error) {
		if ((error = p_lstat(path, st)) < 0)
			giterr_set(GITERR_CHECKOUT, "Could not stat symlink %s", path);

		st->st_mode = GIT_FILEMODE_LINK;
	}

	git_buf_free(&linktarget);

	return error;
}

static int checkout_update_index(
	checkout_data *data,
	const git_diff_file *file,
	struct stat *st)
{
	git_index_entry entry;

	if (!data->index)
		return 0;

	memset(&entry, 0, sizeof(entry));
	entry.path = (char *)file->path; /* cast to prevent warning */
	git_index_entry__init_from_stat(&entry, st, true);
	git_oid_cpy(&entry.id, &file->id);

	return git_index_add(data->index, &entry);
}

static int checkout_submodule_update_index(
	checkout_data *data,
	const git_diff_file *file)
{
	struct stat st;

	/* update the index unless prevented */
	if ((data->strategy & GIT_CHECKOUT_DONT_UPDATE_INDEX) != 0)
		return 0;

	git_buf_truncate(&data->path, data->workdir_len);
	if (git_buf_puts(&data->path, file->path) < 0)
		return -1;

	if (p_stat(git_buf_cstr(&data->path), &st) < 0) {
		giterr_set(
			GITERR_CHECKOUT, "Could not stat submodule %s\n", file->path);
		return GIT_ENOTFOUND;
	}

	st.st_mode = GIT_FILEMODE_COMMIT;

	return checkout_update_index(data, file, &st);
}

static int checkout_submodule(
	checkout_data *data,
	const git_diff_file *file)
{
	int error = 0;

	/* Until submodules are supported, UPDATE_ONLY means do nothing here */
	if ((data->strategy & GIT_CHECKOUT_UPDATE_ONLY) != 0)
		return 0;

	if ((error = git_futils_mkdir(
			file->path, data->opts.target_directory,
			data->opts.dir_mode, GIT_MKDIR_PATH)) < 0)
		return error;

	if ((error = git_submodule_lookup(NULL, data->repo, file->path)) < 0) {
		/* I've observed repos with submodules in the tree that do not
		 * have a .gitmodules - core Git just makes an empty directory
		 */
		if (error == GIT_ENOTFOUND) {
			giterr_clear();
			return checkout_submodule_update_index(data, file);
		}

		return error;
	}

	/* TODO: Support checkout_strategy options.  Two circumstances:
	 * 1 - submodule already checked out, but we need to move the HEAD
	 *     to the new OID, or
	 * 2 - submodule not checked out and we should recursively check it out
	 *
	 * Checkout will not execute a pull on the submodule, but a clone
	 * command should probably be able to.  Do we need a submodule callback?
	 */

	return checkout_submodule_update_index(data, file);
}

static void report_progress(
	checkout_data *data,
	const char *path)
{
	if (data->opts.progress_cb)
		data->opts.progress_cb(
			path, data->completed_steps, data->total_steps,
			data->opts.progress_payload);
}

static int checkout_safe_for_update_only(const char *path, mode_t expected_mode)
{
	struct stat st;

	if (p_lstat(path, &st) < 0) {
		/* if doesn't exist, then no error and no update */
		if (errno == ENOENT || errno == ENOTDIR)
			return 0;

		/* otherwise, stat error and no update */
		giterr_set(GITERR_OS, "Failed to stat file '%s'", path);
		return -1;
	}

	/* only safe for update if this is the same type of file */
	if ((st.st_mode & ~0777) == (expected_mode & ~0777))
		return 1;

	return 0;
}

static int checkout_write_content(
	checkout_data *data,
	const git_oid *oid,
	const char *full_path,
	const char *hint_path,
	unsigned int mode,
	struct stat *st)
{
	int error = 0;
	git_blob *blob;

	if ((error = git_blob_lookup(&blob, data->repo, oid)) < 0)
		return error;

	if (S_ISLNK(mode))
		error = blob_content_to_link(
			st, blob, full_path, data->opts.dir_mode, data->can_symlink);
	else
		error = blob_content_to_file(
			st, blob, full_path, hint_path, mode, &data->opts);

	git_blob_free(blob);

	/* if we try to create the blob and an existing directory blocks it from
	 * being written, then there must have been a typechange conflict in a
	 * parent directory - suppress the error and try to continue.
	 */
	if ((data->strategy & GIT_CHECKOUT_ALLOW_CONFLICTS) != 0 &&
		(error == GIT_ENOTFOUND || error == GIT_EEXISTS))
	{
		giterr_clear();
		error = 0;
	}

	return error;
}

static int checkout_blob(
	checkout_data *data,
	const git_diff_file *file)
{
	int error = 0;
	struct stat st;

	git_buf_truncate(&data->path, data->workdir_len);
	if (git_buf_puts(&data->path, file->path) < 0)
		return -1;

	if ((data->strategy & GIT_CHECKOUT_UPDATE_ONLY) != 0) {
		int rval = checkout_safe_for_update_only(
			git_buf_cstr(&data->path), file->mode);
		if (rval <= 0)
			return rval;
	}

	error = checkout_write_content(
		data, &file->id, git_buf_cstr(&data->path), NULL, file->mode, &st);

	/* update the index unless prevented */
	if (!error && (data->strategy & GIT_CHECKOUT_DONT_UPDATE_INDEX) == 0)
		error = checkout_update_index(data, file, &st);

	/* update the submodule data if this was a new .gitmodules file */
	if (!error && strcmp(file->path, ".gitmodules") == 0)
		data->reload_submodules = true;

	return error;
}

static int checkout_remove_the_old(
	unsigned int *actions,
	checkout_data *data)
{
	int error = 0;
	git_diff_delta *delta;
	const char *str;
	size_t i;
	const char *workdir = git_buf_cstr(&data->path);
	uint32_t flg = GIT_RMDIR_EMPTY_PARENTS |
		GIT_RMDIR_REMOVE_FILES | GIT_RMDIR_REMOVE_BLOCKERS;

	if (data->opts.checkout_strategy & GIT_CHECKOUT_SKIP_LOCKED_DIRECTORIES)
		flg |= GIT_RMDIR_SKIP_NONEMPTY;

	git_buf_truncate(&data->path, data->workdir_len);

	git_vector_foreach(&data->diff->deltas, i, delta) {
		if (actions[i] & CHECKOUT_ACTION__REMOVE) {
			error = git_futils_rmdir_r(delta->old_file.path, workdir, flg);
			if (error < 0)
				return error;

			data->completed_steps++;
			report_progress(data, delta->old_file.path);

			if ((actions[i] & CHECKOUT_ACTION__UPDATE_BLOB) == 0 &&
				(data->strategy & GIT_CHECKOUT_DONT_UPDATE_INDEX) == 0 &&
				data->index != NULL)
			{
				(void)git_index_remove(data->index, delta->old_file.path, 0);
			}
		}
	}

	git_vector_foreach(&data->removes, i, str) {
		error = git_futils_rmdir_r(str, workdir, flg);
		if (error < 0)
			return error;

		data->completed_steps++;
		report_progress(data, str);

		if ((data->strategy & GIT_CHECKOUT_DONT_UPDATE_INDEX) == 0 &&
			data->index != NULL)
		{
			if (str[strlen(str) - 1] == '/')
				(void)git_index_remove_directory(data->index, str, 0);
			else
				(void)git_index_remove(data->index, str, 0);
		}
	}

	return 0;
}

static int checkout_deferred_remove(git_repository *repo, const char *path)
{
#if 0
	int error = git_futils_rmdir_r(
		path, data->opts.target_directory, GIT_RMDIR_EMPTY_PARENTS);

	if (error == GIT_ENOTFOUND) {
		error = 0;
		giterr_clear();
	}

	return error;
#else
	GIT_UNUSED(repo);
	GIT_UNUSED(path);
	assert(false);
	return 0;
#endif
}

static int checkout_create_the_new(
	unsigned int *actions,
	checkout_data *data)
{
	int error = 0;
	git_diff_delta *delta;
	size_t i;

	git_vector_foreach(&data->diff->deltas, i, delta) {
		if (actions[i] & CHECKOUT_ACTION__DEFER_REMOVE) {
			/* this had a blocker directory that should only be removed iff
			 * all of the contents of the directory were safely removed
			 */
			if ((error = checkout_deferred_remove(
					data->repo, delta->old_file.path)) < 0)
				return error;
		}

		if (actions[i] & CHECKOUT_ACTION__UPDATE_BLOB) {
			error = checkout_blob(data, &delta->new_file);
			if (error < 0)
				return error;

			data->completed_steps++;
			report_progress(data, delta->new_file.path);
		}
	}

	return 0;
}

static int checkout_create_submodules(
	unsigned int *actions,
	checkout_data *data)
{
	int error = 0;
	git_diff_delta *delta;
	size_t i;

	/* initial reload of submodules if .gitmodules was changed */
	if (data->reload_submodules &&
		(error = git_submodule_reload_all(data->repo, 1)) < 0)
		return error;

	git_vector_foreach(&data->diff->deltas, i, delta) {
		if (actions[i] & CHECKOUT_ACTION__DEFER_REMOVE) {
			/* this has a blocker directory that should only be removed iff
			 * all of the contents of the directory were safely removed
			 */
			if ((error = checkout_deferred_remove(
					data->repo, delta->old_file.path)) < 0)
				return error;
		}

		if (actions[i] & CHECKOUT_ACTION__UPDATE_SUBMODULE) {
			int error = checkout_submodule(data, &delta->new_file);
			if (error < 0)
				return error;

			data->completed_steps++;
			report_progress(data, delta->new_file.path);
		}
	}

	/* final reload once submodules have been updated */
	return git_submodule_reload_all(data->repo, 1);
}

static int checkout_lookup_head_tree(git_tree **out, git_repository *repo)
{
	int error = 0;
	git_reference *ref = NULL;
	git_object *head;

	if (!(error = git_repository_head(&ref, repo)) &&
		!(error = git_reference_peel(&head, ref, GIT_OBJ_TREE)))
		*out = (git_tree *)head;

	git_reference_free(ref);

	return error;
}


static int conflict_entry_name(
	git_buf *out,
	const char *side_name,
	const char *filename)
{
	if (git_buf_puts(out, side_name) < 0 ||
		git_buf_putc(out, ':') < 0 ||
		git_buf_puts(out, filename) < 0)
		return -1;

	return 0;
}

static int checkout_path_suffixed(git_buf *path, const char *suffix)
{
	size_t path_len;
	int i = 0, error = 0;

	if ((error = git_buf_putc(path, '~')) < 0 || (error = git_buf_puts(path, suffix)) < 0)
		return -1;

	path_len = git_buf_len(path);

	while (git_path_exists(git_buf_cstr(path)) && i < INT_MAX) {
		git_buf_truncate(path, path_len);

		if ((error = git_buf_putc(path, '_')) < 0 ||
			(error = git_buf_printf(path, "%d", i)) < 0)
			return error;

		i++;
	}

	if (i == INT_MAX) {
		git_buf_truncate(path, path_len);

		giterr_set(GITERR_CHECKOUT, "Could not write '%s': working directory file exists", path);
		return GIT_EEXISTS;
	}

	return 0;
}

static int checkout_write_entry(
	checkout_data *data,
	checkout_conflictdata *conflict,
	const git_index_entry *side)
{
	const char *hint_path = NULL, *suffix;
	struct stat st;
	int error;

	assert (side == conflict->ours || side == conflict->theirs);

	git_buf_truncate(&data->path, data->workdir_len);
	if (git_buf_puts(&data->path, side->path) < 0)
		return -1;

	if ((conflict->name_collision || conflict->directoryfile) &&
		(data->strategy & GIT_CHECKOUT_USE_OURS) == 0 &&
		(data->strategy & GIT_CHECKOUT_USE_THEIRS) == 0) {

		if (side == conflict->ours)
			suffix = data->opts.our_label ? data->opts.our_label :
				"ours";
		else
			suffix = data->opts.their_label ? data->opts.their_label :
				"theirs";

		if (checkout_path_suffixed(&data->path, suffix) < 0)
			return -1;

		hint_path = side->path;
	}

	if ((data->strategy & GIT_CHECKOUT_UPDATE_ONLY) != 0 &&
		(error = checkout_safe_for_update_only(git_buf_cstr(&data->path), side->mode)) <= 0)
		return error;

	return checkout_write_content(data,
		&side->id, git_buf_cstr(&data->path), hint_path, side->mode, &st);
}

static int checkout_write_entries(
	checkout_data *data,
	checkout_conflictdata *conflict)
{
	int error = 0;

	if ((error = checkout_write_entry(data, conflict, conflict->ours)) >= 0)
		error = checkout_write_entry(data, conflict, conflict->theirs);

	return error;
}

static int checkout_merge_path(
	git_buf *out,
	checkout_data *data,
	checkout_conflictdata *conflict,
	git_merge_file_result *result)
{
	const char *our_label_raw, *their_label_raw, *suffix;
	int error = 0;

	if ((error = git_buf_joinpath(out, git_repository_workdir(data->repo), result->path)) < 0)
		return error;

	/* Most conflicts simply use the filename in the index */
	if (!conflict->name_collision)
		return 0;

	/* Rename 2->1 conflicts need the branch name appended */
	our_label_raw = data->opts.our_label ? data->opts.our_label : "ours";
	their_label_raw = data->opts.their_label ? data->opts.their_label : "theirs";
	suffix = strcmp(result->path, conflict->ours->path) == 0 ? our_label_raw : their_label_raw;

	if ((error = checkout_path_suffixed(out, suffix)) < 0)
		return error;

	return 0;
}

static int checkout_write_merge(
	checkout_data *data,
	checkout_conflictdata *conflict)
{
	git_buf our_label = GIT_BUF_INIT, their_label = GIT_BUF_INIT,
		path_suffixed = GIT_BUF_INIT, path_workdir = GIT_BUF_INIT;
	git_merge_file_options opts = GIT_MERGE_FILE_OPTIONS_INIT;
	git_merge_file_result result = {0};
	git_filebuf output = GIT_FILEBUF_INIT;
	int error = 0;

	if (data->opts.checkout_strategy & GIT_CHECKOUT_CONFLICT_STYLE_DIFF3)
		opts.flags |= GIT_MERGE_FILE_STYLE_DIFF3;

	opts.ancestor_label = data->opts.ancestor_label ?
		data->opts.ancestor_label : "ancestor";
	opts.our_label = data->opts.our_label ?
		data->opts.our_label : "ours";
	opts.their_label = data->opts.their_label ?
		data->opts.their_label : "theirs";

	/* If all the paths are identical, decorate the diff3 file with the branch
	 * names.  Otherwise, append branch_name:path.
	 */
	if (conflict->ours && conflict->theirs &&
		strcmp(conflict->ours->path, conflict->theirs->path) != 0) {

		if ((error = conflict_entry_name(
			&our_label, opts.our_label, conflict->ours->path)) < 0 ||
			(error = conflict_entry_name(
			&their_label, opts.their_label, conflict->theirs->path)) < 0)
			goto done;

		opts.our_label = git_buf_cstr(&our_label);
		opts.their_label = git_buf_cstr(&their_label);
	}

	if ((error = git_merge_file_from_index(&result, data->repo,
		conflict->ancestor, conflict->ours, conflict->theirs, &opts)) < 0)
		goto done;

	if (result.path == NULL || result.mode == 0) {
		giterr_set(GITERR_CHECKOUT, "Could not merge contents of file");
		error = GIT_EMERGECONFLICT;
		goto done;
	}

	if ((error = checkout_merge_path(&path_workdir, data, conflict, &result)) < 0)
		goto done;

	if ((data->strategy & GIT_CHECKOUT_UPDATE_ONLY) != 0 &&
		(error = checkout_safe_for_update_only(git_buf_cstr(&path_workdir), result.mode)) <= 0)
		goto done;

	if ((error = git_futils_mkpath2file(path_workdir.ptr, 0755)) < 0 ||
		(error = git_filebuf_open(&output, path_workdir.ptr, GIT_FILEBUF_DO_NOT_BUFFER, result.mode)) < 0 ||
		(error = git_filebuf_write(&output, result.ptr, result.len)) < 0 ||
		(error = git_filebuf_commit(&output)) < 0)
		goto done;

done:
	git_buf_free(&our_label);
	git_buf_free(&their_label);

	git_merge_file_result_free(&result);
	git_buf_free(&path_workdir);
	git_buf_free(&path_suffixed);

	return error;
}

static int checkout_create_conflicts(checkout_data *data)
{
	checkout_conflictdata *conflict;
	size_t i;
	int error = 0;

	git_vector_foreach(&data->conflicts, i, conflict) {

		/* Both deleted: nothing to do */
		if (conflict->ours == NULL && conflict->theirs == NULL)
			error = 0;

		else if ((data->strategy & GIT_CHECKOUT_USE_OURS) &&
			conflict->ours)
			error = checkout_write_entry(data, conflict, conflict->ours);
		else if ((data->strategy & GIT_CHECKOUT_USE_THEIRS) &&
			conflict->theirs)
			error = checkout_write_entry(data, conflict, conflict->theirs);

		/* Ignore the other side of name collisions. */
		else if ((data->strategy & GIT_CHECKOUT_USE_OURS) &&
			!conflict->ours && conflict->name_collision)
			error = 0;
		else if ((data->strategy & GIT_CHECKOUT_USE_THEIRS) &&
			!conflict->theirs && conflict->name_collision)
			error = 0;

		/* For modify/delete, name collisions and d/f conflicts, write
		 * the file (potentially with the name mangled.
		 */
		else if (conflict->ours != NULL && conflict->theirs == NULL)
			error = checkout_write_entry(data, conflict, conflict->ours);
		else if (conflict->ours == NULL && conflict->theirs != NULL)
			error = checkout_write_entry(data, conflict, conflict->theirs);

		/* Add/add conflicts and rename 1->2 conflicts, write the
		 * ours/theirs sides (potentially name mangled).
		 */
		else if (conflict->one_to_two)
			error = checkout_write_entries(data, conflict);

		/* If all sides are links, write the ours side */
		else if (S_ISLNK(conflict->ours->mode) &&
			S_ISLNK(conflict->theirs->mode))
			error = checkout_write_entry(data, conflict, conflict->ours);
		/* Link/file conflicts, write the file side */
		else if (S_ISLNK(conflict->ours->mode))
			error = checkout_write_entry(data, conflict, conflict->theirs);
		else if (S_ISLNK(conflict->theirs->mode))
			error = checkout_write_entry(data, conflict, conflict->ours);

		/* If any side is a gitlink, do nothing. */
		else if (conflict->submodule)
			error = 0;

		/* If any side is binary, write the ours side */
		else if (conflict->binary)
			error = checkout_write_entry(data, conflict, conflict->ours);

		else if (!error)
			error = checkout_write_merge(data, conflict);

		if (error)
			break;

		data->completed_steps++;
		report_progress(data,
			conflict->ours ? conflict->ours->path :
			(conflict->theirs ? conflict->theirs->path : conflict->ancestor->path));
	}

	return error;
}


static void checkout_data_clear(checkout_data *data)
{
	if (data->opts_free_baseline) {
		git_tree_free(data->opts.baseline);
		data->opts.baseline = NULL;
	}

	git_vector_free(&data->removes);
	git_pool_clear(&data->pool);

	git_vector_free_deep(&data->conflicts);

	git__free(data->pfx);
	data->pfx = NULL;

	git_buf_free(&data->path);
	git_buf_free(&data->tmp);

	git_index_free(data->index);
	data->index = NULL;
}

static int checkout_data_init(
	checkout_data *data,
	git_iterator *target,
	const git_checkout_options *proposed)
{
	int error = 0;
	git_repository *repo = git_iterator_owner(target);

	memset(data, 0, sizeof(*data));

	if (!repo) {
		giterr_set(GITERR_CHECKOUT, "Cannot checkout nothing");
		return -1;
	}

	if ((!proposed || !proposed->target_directory) &&
		(error = git_repository__ensure_not_bare(repo, "checkout")) < 0)
		return error;

	data->repo = repo;

	GITERR_CHECK_VERSION(
		proposed, GIT_CHECKOUT_OPTIONS_VERSION, "git_checkout_options");

	if (!proposed)
		GIT_INIT_STRUCTURE(&data->opts, GIT_CHECKOUT_OPTIONS_VERSION);
	else
		memmove(&data->opts, proposed, sizeof(git_checkout_options));

	if (!data->opts.target_directory)
		data->opts.target_directory = git_repository_workdir(repo);
	else if (!git_path_isdir(data->opts.target_directory) &&
			 (error = git_futils_mkdir(data->opts.target_directory, NULL,
					GIT_DIR_MODE, GIT_MKDIR_VERIFY_DIR)) < 0)
		goto cleanup;

	/* refresh config and index content unless NO_REFRESH is given */
	if ((data->opts.checkout_strategy & GIT_CHECKOUT_NO_REFRESH) == 0) {
		git_config *cfg;

		if ((error = git_repository_config__weakptr(&cfg, repo)) < 0 ||
			(error = git_config_refresh(cfg)) < 0)
			goto cleanup;

		/* if we are checking out the index, don't reload,
		 * otherwise get index and force reload
		 */
		if ((data->index = git_iterator_get_index(target)) != NULL) {
			GIT_REFCOUNT_INC(data->index);
		} else {
			/* otherwise, grab and reload the index */
			if ((error = git_repository_index(&data->index, data->repo)) < 0 ||
				(error = git_index_read(data->index, true)) < 0)
				goto cleanup;

			/* cannot checkout if unresolved conflicts exist */
			if ((data->opts.checkout_strategy & GIT_CHECKOUT_FORCE) == 0 &&
				git_index_has_conflicts(data->index)) {
				error = GIT_EMERGECONFLICT;
				giterr_set(GITERR_CHECKOUT,
					"unresolved conflicts exist in the index");
				goto cleanup;
			}

			/* clean conflict data when doing a tree or commit checkout */
			git_index_name_clear(data->index);
			git_index_reuc_clear(data->index);
		}
	}

	/* if you are forcing, definitely allow safe updates */
	if ((data->opts.checkout_strategy & GIT_CHECKOUT_FORCE) != 0)
		data->opts.checkout_strategy |= GIT_CHECKOUT_SAFE_CREATE;
	if ((data->opts.checkout_strategy & GIT_CHECKOUT_SAFE_CREATE) != 0)
		data->opts.checkout_strategy |= GIT_CHECKOUT_SAFE;

	data->strategy = data->opts.checkout_strategy;

	/* opts->disable_filters is false by default */

	if (!data->opts.dir_mode)
		data->opts.dir_mode = GIT_DIR_MODE;

	if (!data->opts.file_open_flags)
		data->opts.file_open_flags = O_CREAT | O_TRUNC | O_WRONLY;

	data->pfx = git_pathspec_prefix(&data->opts.paths);

	if ((error = git_repository__cvar(
			 &data->can_symlink, repo, GIT_CVAR_SYMLINKS)) < 0)
		goto cleanup;

	if (!data->opts.baseline) {
		data->opts_free_baseline = true;

		error = checkout_lookup_head_tree(&data->opts.baseline, repo);

		if (error == GIT_EUNBORNBRANCH) {
			error = 0;
			giterr_clear();
		}

		if (error < 0)
			goto cleanup;
	}

	if ((data->opts.checkout_strategy &
		(GIT_CHECKOUT_CONFLICT_STYLE_MERGE | GIT_CHECKOUT_CONFLICT_STYLE_DIFF3)) == 0) {
		const char *conflict_style;
		git_config *cfg = NULL;

		if ((error = git_repository_config__weakptr(&cfg, repo)) < 0 ||
			(error = git_config_get_string(&conflict_style, cfg, "merge.conflictstyle")) < 0 ||
			error == GIT_ENOTFOUND)
			;
		else if (error)
			goto cleanup;
		else if (strcmp(conflict_style, "merge") == 0)
			data->opts.checkout_strategy |= GIT_CHECKOUT_CONFLICT_STYLE_MERGE;
		else if (strcmp(conflict_style, "diff3") == 0)
			data->opts.checkout_strategy |= GIT_CHECKOUT_CONFLICT_STYLE_DIFF3;
		else {
			giterr_set(GITERR_CHECKOUT, "unknown style '%s' given for 'merge.conflictstyle'",
				conflict_style);
			error = -1;
			goto cleanup;
		}
	}

	if ((error = git_vector_init(&data->removes, 0, git__strcmp_cb)) < 0 ||
		(error = git_vector_init(&data->conflicts, 0, NULL)) < 0 ||
		(error = git_pool_init(&data->pool, 1, 0)) < 0 ||
		(error = git_buf_puts(&data->path, data->opts.target_directory)) < 0 ||
		(error = git_path_to_dir(&data->path)) < 0)
		goto cleanup;

	data->workdir_len = git_buf_len(&data->path);

cleanup:
	if (error < 0)
		checkout_data_clear(data);

	return error;
}

int git_checkout_iterator(
	git_iterator *target,
	const git_checkout_options *opts)
{
	int error = 0;
	git_iterator *baseline = NULL, *workdir = NULL;
	checkout_data data = {0};
	git_diff_options diff_opts = GIT_DIFF_OPTIONS_INIT;
	uint32_t *actions = NULL;
	size_t *counts = NULL;
	git_iterator_flag_t iterflags = 0;

	/* initialize structures and options */
	error = checkout_data_init(&data, target, opts);
	if (error < 0)
		return error;

	diff_opts.flags =
		GIT_DIFF_INCLUDE_UNMODIFIED |
		GIT_DIFF_INCLUDE_UNTRACKED |
		GIT_DIFF_RECURSE_UNTRACKED_DIRS | /* needed to match baseline */
		GIT_DIFF_INCLUDE_IGNORED |
		GIT_DIFF_INCLUDE_TYPECHANGE |
		GIT_DIFF_INCLUDE_TYPECHANGE_TREES |
		GIT_DIFF_SKIP_BINARY_CHECK;
	if (data.opts.checkout_strategy & GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH)
		diff_opts.flags |= GIT_DIFF_DISABLE_PATHSPEC_MATCH;
	if (data.opts.paths.count > 0)
		diff_opts.pathspec = data.opts.paths;

	/* set up iterators */

	iterflags = git_iterator_ignore_case(target) ?
		GIT_ITERATOR_IGNORE_CASE : GIT_ITERATOR_DONT_IGNORE_CASE;

	if ((error = git_iterator_reset(target, data.pfx, data.pfx)) < 0 ||
		(error = git_iterator_for_workdir_ext(
			&workdir, data.repo, data.opts.target_directory,
			iterflags | GIT_ITERATOR_DONT_AUTOEXPAND,
			data.pfx, data.pfx)) < 0 ||
		(error = git_iterator_for_tree(
			&baseline, data.opts.baseline,
			iterflags, data.pfx, data.pfx)) < 0)
		goto cleanup;

	/* Should not have case insensitivity mismatch */
	assert(git_iterator_ignore_case(workdir) == git_iterator_ignore_case(baseline));

	/* Generate baseline-to-target diff which will include an entry for
	 * every possible update that might need to be made.
	 */
	if ((error = git_diff__from_iterators(
			&data.diff, data.repo, baseline, target, &diff_opts)) < 0)
		goto cleanup;

	/* Loop through diff (and working directory iterator) building a list of
	 * actions to be taken, plus look for conflicts and send notifications,
	 * then loop through conflicts.
	 */
	if ((error = checkout_get_actions(&actions, &counts, &data, workdir)) != 0)
		goto cleanup;

	data.total_steps = counts[CHECKOUT_ACTION__REMOVE] +
		counts[CHECKOUT_ACTION__UPDATE_BLOB] +
		counts[CHECKOUT_ACTION__UPDATE_SUBMODULE] +
		counts[CHECKOUT_ACTION__UPDATE_CONFLICT];

	report_progress(&data, NULL); /* establish 0 baseline */

	/* To deal with some order dependencies, perform remaining checkout
	 * in three passes: removes, then update blobs, then update submodules.
	 */
	if (counts[CHECKOUT_ACTION__REMOVE] > 0 &&
		(error = checkout_remove_the_old(actions, &data)) < 0)
		goto cleanup;

	if (counts[CHECKOUT_ACTION__UPDATE_BLOB] > 0 &&
		(error = checkout_create_the_new(actions, &data)) < 0)
		goto cleanup;

	if (counts[CHECKOUT_ACTION__UPDATE_SUBMODULE] > 0 &&
		(error = checkout_create_submodules(actions, &data)) < 0)
		goto cleanup;

	if (counts[CHECKOUT_ACTION__UPDATE_CONFLICT] > 0 &&
		(error = checkout_create_conflicts(&data)) < 0)
		goto cleanup;

	assert(data.completed_steps == data.total_steps);

cleanup:
	if (!error && data.index != NULL &&
		(data.strategy & GIT_CHECKOUT_DONT_UPDATE_INDEX) == 0)
		error = git_index_write(data.index);

	git_diff_free(data.diff);
	git_iterator_free(workdir);
	git_iterator_free(baseline);
	git__free(actions);
	git__free(counts);
	checkout_data_clear(&data);

	return error;
}

int git_checkout_index(
	git_repository *repo,
	git_index *index,
	const git_checkout_options *opts)
{
	int error;
	git_iterator *index_i;

	if (!index && !repo) {
		giterr_set(GITERR_CHECKOUT,
			"Must provide either repository or index to checkout");
		return -1;
	}
	if (index && repo && git_index_owner(index) != repo) {
		giterr_set(GITERR_CHECKOUT,
			"Index to checkout does not match repository");
		return -1;
	}

	if (!repo)
		repo = git_index_owner(index);

	if (!index && (error = git_repository_index__weakptr(&index, repo)) < 0)
		return error;
	GIT_REFCOUNT_INC(index);

	if (!(error = git_iterator_for_index(&index_i, index, 0, NULL, NULL)))
		error = git_checkout_iterator(index_i, opts);

	git_iterator_free(index_i);
	git_index_free(index);

	return error;
}

int git_checkout_tree(
	git_repository *repo,
	const git_object *treeish,
	const git_checkout_options *opts)
{
	int error;
	git_tree *tree = NULL;
	git_iterator *tree_i = NULL;

	if (!treeish && !repo) {
		giterr_set(GITERR_CHECKOUT,
			"Must provide either repository or tree to checkout");
		return -1;
	}
	if (treeish && repo && git_object_owner(treeish) != repo) {
		giterr_set(GITERR_CHECKOUT,
			"Object to checkout does not match repository");
		return -1;
	}

	if (!repo)
		repo = git_object_owner(treeish);

	if (treeish) {
		if (git_object_peel((git_object **)&tree, treeish, GIT_OBJ_TREE) < 0) {
			giterr_set(
				GITERR_CHECKOUT, "Provided object cannot be peeled to a tree");
			return -1;
		}
	}
	else {
		if ((error = checkout_lookup_head_tree(&tree, repo)) < 0) {
			if (error != GIT_EUNBORNBRANCH)
				giterr_set(
					GITERR_CHECKOUT,
					"HEAD could not be peeled to a tree and no treeish given");
			return error;
		}
	}

	if (!(error = git_iterator_for_tree(&tree_i, tree, 0, NULL, NULL)))
		error = git_checkout_iterator(tree_i, opts);

	git_iterator_free(tree_i);
	git_tree_free(tree);

	return error;
}

int git_checkout_head(
	git_repository *repo,
	const git_checkout_options *opts)
{
	assert(repo);
	return git_checkout_tree(repo, NULL, opts);
}

int git_checkout_init_options(git_checkout_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_checkout_options, GIT_CHECKOUT_OPTIONS_INIT);
	return 0;
}
