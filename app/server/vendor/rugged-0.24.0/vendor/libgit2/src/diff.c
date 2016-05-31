/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"
#include "diff.h"
#include "fileops.h"
#include "config.h"
#include "attr_file.h"
#include "filter.h"
#include "pathspec.h"
#include "index.h"
#include "odb.h"
#include "submodule.h"

#define DIFF_FLAG_IS_SET(DIFF,FLAG) (((DIFF)->opts.flags & (FLAG)) != 0)
#define DIFF_FLAG_ISNT_SET(DIFF,FLAG) (((DIFF)->opts.flags & (FLAG)) == 0)
#define DIFF_FLAG_SET(DIFF,FLAG,VAL) (DIFF)->opts.flags = \
	(VAL) ? ((DIFF)->opts.flags | (FLAG)) : ((DIFF)->opts.flags & ~(VAL))

static git_diff_delta *diff_delta__alloc(
	git_diff *diff,
	git_delta_t status,
	const char *path)
{
	git_diff_delta *delta = git__calloc(1, sizeof(git_diff_delta));
	if (!delta)
		return NULL;

	delta->old_file.path = git_pool_strdup(&diff->pool, path);
	if (delta->old_file.path == NULL) {
		git__free(delta);
		return NULL;
	}

	delta->new_file.path = delta->old_file.path;

	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_REVERSE)) {
		switch (status) {
		case GIT_DELTA_ADDED:   status = GIT_DELTA_DELETED; break;
		case GIT_DELTA_DELETED: status = GIT_DELTA_ADDED; break;
		default: break; /* leave other status values alone */
		}
	}
	delta->status = status;

	return delta;
}

static int diff_insert_delta(
	git_diff *diff, git_diff_delta *delta, const char *matched_pathspec)
{
	int error = 0;

	if (diff->opts.notify_cb) {
		error = diff->opts.notify_cb(
			diff, delta, matched_pathspec, diff->opts.payload);

		if (error) {
			git__free(delta);

			if (error > 0)	/* positive value means to skip this delta */
				return 0;
			else			/* negative value means to cancel diff */
				return giterr_set_after_callback_function(error, "git_diff");
		}
	}

	if ((error = git_vector_insert(&diff->deltas, delta)) < 0)
		git__free(delta);

	return error;
}

static bool diff_pathspec_match(
	const char **matched_pathspec,
	git_diff *diff,
	const git_index_entry *entry)
{
	bool disable_pathspec_match =
		DIFF_FLAG_IS_SET(diff, GIT_DIFF_DISABLE_PATHSPEC_MATCH);

	/* If we're disabling fnmatch, then the iterator has already applied
	 * the filters to the files for us and we don't have to do anything.
	 * However, this only applies to *files* - the iterator will include
	 * directories that we need to recurse into when not autoexpanding,
	 * so we still need to apply the pathspec match to directories.
	 */
	if ((S_ISLNK(entry->mode) || S_ISREG(entry->mode)) &&
		disable_pathspec_match) {
		*matched_pathspec = entry->path;
		return true;
	}

	return git_pathspec__match(
		&diff->pathspec, entry->path, disable_pathspec_match,
		DIFF_FLAG_IS_SET(diff, GIT_DIFF_IGNORE_CASE),
		matched_pathspec, NULL);
}

static int diff_delta__from_one(
	git_diff *diff,
	git_delta_t status,
	const git_index_entry *oitem,
	const git_index_entry *nitem)
{
	const git_index_entry *entry = nitem;
	bool has_old = false;
	git_diff_delta *delta;
	const char *matched_pathspec;

	assert((oitem != NULL) ^ (nitem != NULL));

	if (oitem) {
		entry = oitem;
		has_old = true;
	}

	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_REVERSE))
		has_old = !has_old;

	if ((entry->flags & GIT_IDXENTRY_VALID) != 0)
		return 0;

	if (status == GIT_DELTA_IGNORED &&
		DIFF_FLAG_ISNT_SET(diff, GIT_DIFF_INCLUDE_IGNORED))
		return 0;

	if (status == GIT_DELTA_UNTRACKED &&
		DIFF_FLAG_ISNT_SET(diff, GIT_DIFF_INCLUDE_UNTRACKED))
		return 0;

	if (status == GIT_DELTA_UNREADABLE &&
		DIFF_FLAG_ISNT_SET(diff, GIT_DIFF_INCLUDE_UNREADABLE))
		return 0;

	if (!diff_pathspec_match(&matched_pathspec, diff, entry))
		return 0;

	delta = diff_delta__alloc(diff, status, entry->path);
	GITERR_CHECK_ALLOC(delta);

	/* This fn is just for single-sided diffs */
	assert(status != GIT_DELTA_MODIFIED);
	delta->nfiles = 1;

	if (has_old) {
		delta->old_file.mode = entry->mode;
		delta->old_file.size = entry->file_size;
		delta->old_file.flags |= GIT_DIFF_FLAG_EXISTS;
		git_oid_cpy(&delta->old_file.id, &entry->id);
	} else /* ADDED, IGNORED, UNTRACKED */ {
		delta->new_file.mode = entry->mode;
		delta->new_file.size = entry->file_size;
		delta->new_file.flags |= GIT_DIFF_FLAG_EXISTS;
		git_oid_cpy(&delta->new_file.id, &entry->id);
	}

	delta->old_file.flags |= GIT_DIFF_FLAG_VALID_ID;

	if (has_old || !git_oid_iszero(&delta->new_file.id))
		delta->new_file.flags |= GIT_DIFF_FLAG_VALID_ID;

	return diff_insert_delta(diff, delta, matched_pathspec);
}

static int diff_delta__from_two(
	git_diff *diff,
	git_delta_t status,
	const git_index_entry *old_entry,
	uint32_t old_mode,
	const git_index_entry *new_entry,
	uint32_t new_mode,
	const git_oid *new_id,
	const char *matched_pathspec)
{
	const git_oid *old_id = &old_entry->id;
	git_diff_delta *delta;
	const char *canonical_path = old_entry->path;

	if (status == GIT_DELTA_UNMODIFIED &&
		DIFF_FLAG_ISNT_SET(diff, GIT_DIFF_INCLUDE_UNMODIFIED))
		return 0;

	if (!new_id)
		new_id = &new_entry->id;

	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_REVERSE)) {
		uint32_t temp_mode = old_mode;
		const git_index_entry *temp_entry = old_entry;
		const git_oid *temp_id = old_id;

		old_entry = new_entry;
		new_entry = temp_entry;
		old_mode = new_mode;
		new_mode = temp_mode;
		old_id = new_id;
		new_id = temp_id;
	}

	delta = diff_delta__alloc(diff, status, canonical_path);
	GITERR_CHECK_ALLOC(delta);
	delta->nfiles = 2;

	if (!git_index_entry_is_conflict(old_entry)) {
		delta->old_file.size = old_entry->file_size;
		delta->old_file.mode = old_mode;
		git_oid_cpy(&delta->old_file.id, old_id);
		delta->old_file.flags |= GIT_DIFF_FLAG_VALID_ID |
			GIT_DIFF_FLAG_EXISTS;
	}

	if (!git_index_entry_is_conflict(new_entry)) {
		git_oid_cpy(&delta->new_file.id, new_id);
		delta->new_file.size = new_entry->file_size;
		delta->new_file.mode = new_mode;
		delta->old_file.flags |= GIT_DIFF_FLAG_EXISTS;
		delta->new_file.flags |= GIT_DIFF_FLAG_EXISTS;

		if (!git_oid_iszero(&new_entry->id))
			delta->new_file.flags |= GIT_DIFF_FLAG_VALID_ID;
	}

	return diff_insert_delta(diff, delta, matched_pathspec);
}

static git_diff_delta *diff_delta__last_for_item(
	git_diff *diff,
	const git_index_entry *item)
{
	git_diff_delta *delta = git_vector_last(&diff->deltas);
	if (!delta)
		return NULL;

	switch (delta->status) {
	case GIT_DELTA_UNMODIFIED:
	case GIT_DELTA_DELETED:
		if (git_oid__cmp(&delta->old_file.id, &item->id) == 0)
			return delta;
		break;
	case GIT_DELTA_ADDED:
		if (git_oid__cmp(&delta->new_file.id, &item->id) == 0)
			return delta;
		break;
	case GIT_DELTA_UNREADABLE:
	case GIT_DELTA_UNTRACKED:
		if (diff->strcomp(delta->new_file.path, item->path) == 0 &&
			git_oid__cmp(&delta->new_file.id, &item->id) == 0)
			return delta;
		break;
	case GIT_DELTA_MODIFIED:
		if (git_oid__cmp(&delta->old_file.id, &item->id) == 0 ||
			git_oid__cmp(&delta->new_file.id, &item->id) == 0)
			return delta;
		break;
	default:
		break;
	}

	return NULL;
}

static char *diff_strdup_prefix(git_pool *pool, const char *prefix)
{
	size_t len = strlen(prefix);

	/* append '/' at end if needed */
	if (len > 0 && prefix[len - 1] != '/')
		return git_pool_strcat(pool, prefix, "/");
	else
		return git_pool_strndup(pool, prefix, len + 1);
}

GIT_INLINE(const char *) diff_delta__path(const git_diff_delta *delta)
{
	const char *str = delta->old_file.path;

	if (!str ||
		delta->status == GIT_DELTA_ADDED ||
		delta->status == GIT_DELTA_RENAMED ||
		delta->status == GIT_DELTA_COPIED)
		str = delta->new_file.path;

	return str;
}

const char *git_diff_delta__path(const git_diff_delta *delta)
{
	return diff_delta__path(delta);
}

int git_diff_delta__cmp(const void *a, const void *b)
{
	const git_diff_delta *da = a, *db = b;
	int val = strcmp(diff_delta__path(da), diff_delta__path(db));
	return val ? val : ((int)da->status - (int)db->status);
}

int git_diff_delta__casecmp(const void *a, const void *b)
{
	const git_diff_delta *da = a, *db = b;
	int val = strcasecmp(diff_delta__path(da), diff_delta__path(db));
	return val ? val : ((int)da->status - (int)db->status);
}

GIT_INLINE(const char *) diff_delta__i2w_path(const git_diff_delta *delta)
{
	return delta->old_file.path ?
		delta->old_file.path : delta->new_file.path;
}

int git_diff_delta__i2w_cmp(const void *a, const void *b)
{
	const git_diff_delta *da = a, *db = b;
	int val = strcmp(diff_delta__i2w_path(da), diff_delta__i2w_path(db));
	return val ? val : ((int)da->status - (int)db->status);
}

int git_diff_delta__i2w_casecmp(const void *a, const void *b)
{
	const git_diff_delta *da = a, *db = b;
	int val = strcasecmp(diff_delta__i2w_path(da), diff_delta__i2w_path(db));
	return val ? val : ((int)da->status - (int)db->status);
}

bool git_diff_delta__should_skip(
	const git_diff_options *opts, const git_diff_delta *delta)
{
	uint32_t flags = opts ? opts->flags : 0;

	if (delta->status == GIT_DELTA_UNMODIFIED &&
		(flags & GIT_DIFF_INCLUDE_UNMODIFIED) == 0)
		return true;

	if (delta->status == GIT_DELTA_IGNORED &&
		(flags & GIT_DIFF_INCLUDE_IGNORED) == 0)
		return true;

	if (delta->status == GIT_DELTA_UNTRACKED &&
		(flags & GIT_DIFF_INCLUDE_UNTRACKED) == 0)
		return true;

	if (delta->status == GIT_DELTA_UNREADABLE &&
		(flags & GIT_DIFF_INCLUDE_UNREADABLE) == 0)
		return true;

	return false;
}


static const char *diff_mnemonic_prefix(
	git_iterator_type_t type, bool left_side)
{
	const char *pfx = "";

	switch (type) {
	case GIT_ITERATOR_TYPE_EMPTY:   pfx = "c"; break;
	case GIT_ITERATOR_TYPE_TREE:    pfx = "c"; break;
	case GIT_ITERATOR_TYPE_INDEX:   pfx = "i"; break;
	case GIT_ITERATOR_TYPE_WORKDIR: pfx = "w"; break;
	case GIT_ITERATOR_TYPE_FS:      pfx = left_side ? "1" : "2"; break;
	default: break;
	}

	/* note: without a deeper look at pathspecs, there is no easy way
	 * to get the (o)bject / (w)ork tree mnemonics working...
	 */

	return pfx;
}

static int diff_entry_cmp(const void *a, const void *b)
{
	const git_index_entry *entry_a = a;
	const git_index_entry *entry_b = b;

	return strcmp(entry_a->path, entry_b->path);
}

static int diff_entry_icmp(const void *a, const void *b)
{
	const git_index_entry *entry_a = a;
	const git_index_entry *entry_b = b;

	return strcasecmp(entry_a->path, entry_b->path);
}

static void diff_set_ignore_case(git_diff *diff, bool ignore_case)
{
	if (!ignore_case) {
		diff->opts.flags &= ~GIT_DIFF_IGNORE_CASE;

		diff->strcomp    = git__strcmp;
		diff->strncomp   = git__strncmp;
		diff->pfxcomp    = git__prefixcmp;
		diff->entrycomp  = diff_entry_cmp;

		git_vector_set_cmp(&diff->deltas, git_diff_delta__cmp);
	} else {
		diff->opts.flags |= GIT_DIFF_IGNORE_CASE;

		diff->strcomp    = git__strcasecmp;
		diff->strncomp   = git__strncasecmp;
		diff->pfxcomp    = git__prefixcmp_icase;
		diff->entrycomp  = diff_entry_icmp;

		git_vector_set_cmp(&diff->deltas, git_diff_delta__casecmp);
	}

	git_vector_sort(&diff->deltas);
}

static git_diff *diff_list_alloc(
	git_repository *repo,
	git_iterator *old_iter,
	git_iterator *new_iter)
{
	git_diff_options dflt = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = git__calloc(1, sizeof(git_diff));
	if (!diff)
		return NULL;

	assert(repo && old_iter && new_iter);

	GIT_REFCOUNT_INC(diff);
	diff->repo = repo;
	diff->old_src = old_iter->type;
	diff->new_src = new_iter->type;
	memcpy(&diff->opts, &dflt, sizeof(diff->opts));

	git_pool_init(&diff->pool, 1);

	if (git_vector_init(&diff->deltas, 0, git_diff_delta__cmp) < 0) {
		git_diff_free(diff);
		return NULL;
	}

	/* Use case-insensitive compare if either iterator has
	 * the ignore_case bit set */
	diff_set_ignore_case(
		diff,
		git_iterator_ignore_case(old_iter) ||
		git_iterator_ignore_case(new_iter));

	return diff;
}

static int diff_list_apply_options(
	git_diff *diff,
	const git_diff_options *opts)
{
	git_config *cfg = NULL;
	git_repository *repo = diff->repo;
	git_pool *pool = &diff->pool;
	int val;

	if (opts) {
		/* copy user options (except case sensitivity info from iterators) */
		bool icase = DIFF_FLAG_IS_SET(diff, GIT_DIFF_IGNORE_CASE);
		memcpy(&diff->opts, opts, sizeof(diff->opts));
		DIFF_FLAG_SET(diff, GIT_DIFF_IGNORE_CASE, icase);

		/* initialize pathspec from options */
		if (git_pathspec__vinit(&diff->pathspec, &opts->pathspec, pool) < 0)
			return -1;
	}

	/* flag INCLUDE_TYPECHANGE_TREES implies INCLUDE_TYPECHANGE */
	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_INCLUDE_TYPECHANGE_TREES))
		diff->opts.flags |= GIT_DIFF_INCLUDE_TYPECHANGE;

	/* flag INCLUDE_UNTRACKED_CONTENT implies INCLUDE_UNTRACKED */
	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_SHOW_UNTRACKED_CONTENT))
		diff->opts.flags |= GIT_DIFF_INCLUDE_UNTRACKED;

	/* load config values that affect diff behavior */
	if ((val = git_repository_config_snapshot(&cfg, repo)) < 0)
		return val;

	if (!git_config__cvar(&val, cfg, GIT_CVAR_SYMLINKS) && val)
		diff->diffcaps = diff->diffcaps | GIT_DIFFCAPS_HAS_SYMLINKS;

	if (!git_config__cvar(&val, cfg, GIT_CVAR_IGNORESTAT) && val)
		diff->diffcaps = diff->diffcaps | GIT_DIFFCAPS_IGNORE_STAT;

	if ((diff->opts.flags & GIT_DIFF_IGNORE_FILEMODE) == 0 &&
		!git_config__cvar(&val, cfg, GIT_CVAR_FILEMODE) && val)
		diff->diffcaps = diff->diffcaps | GIT_DIFFCAPS_TRUST_MODE_BITS;

	if (!git_config__cvar(&val, cfg, GIT_CVAR_TRUSTCTIME) && val)
		diff->diffcaps = diff->diffcaps | GIT_DIFFCAPS_TRUST_CTIME;

	/* Don't set GIT_DIFFCAPS_USE_DEV - compile time option in core git */

	/* If not given explicit `opts`, check `diff.xyz` configs */
	if (!opts) {
		int context = git_config__get_int_force(cfg, "diff.context", 3);
		diff->opts.context_lines = context >= 0 ? (uint32_t)context : 3;

		/* add other defaults here */
	}

	/* Reverse src info if diff is reversed */
	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_REVERSE)) {
		git_iterator_type_t tmp_src = diff->old_src;
		diff->old_src = diff->new_src;
		diff->new_src = tmp_src;
	}

	/* Unset UPDATE_INDEX unless diffing workdir and index */
	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_UPDATE_INDEX) &&
		(!(diff->old_src == GIT_ITERATOR_TYPE_WORKDIR ||
		   diff->new_src == GIT_ITERATOR_TYPE_WORKDIR) ||
		 !(diff->old_src == GIT_ITERATOR_TYPE_INDEX ||
		   diff->new_src == GIT_ITERATOR_TYPE_INDEX)))
		diff->opts.flags &= ~GIT_DIFF_UPDATE_INDEX;

	/* if ignore_submodules not explicitly set, check diff config */
	if (diff->opts.ignore_submodules <= 0) {
		 git_config_entry *entry;
		git_config__lookup_entry(&entry, cfg, "diff.ignoresubmodules", true);

		if (entry && git_submodule_parse_ignore(
				&diff->opts.ignore_submodules, entry->value) < 0)
			giterr_clear();
		git_config_entry_free(entry);
	}

	/* if either prefix is not set, figure out appropriate value */
	if (!diff->opts.old_prefix || !diff->opts.new_prefix) {
		const char *use_old = DIFF_OLD_PREFIX_DEFAULT;
		const char *use_new = DIFF_NEW_PREFIX_DEFAULT;

		if (git_config__get_bool_force(cfg, "diff.noprefix", 0))
			use_old = use_new = "";
		else if (git_config__get_bool_force(cfg, "diff.mnemonicprefix", 0)) {
			use_old = diff_mnemonic_prefix(diff->old_src, true);
			use_new = diff_mnemonic_prefix(diff->new_src, false);
		}

		if (!diff->opts.old_prefix)
			diff->opts.old_prefix = use_old;
		if (!diff->opts.new_prefix)
			diff->opts.new_prefix = use_new;
	}

	/* strdup prefix from pool so we're not dependent on external data */
	diff->opts.old_prefix = diff_strdup_prefix(pool, diff->opts.old_prefix);
	diff->opts.new_prefix = diff_strdup_prefix(pool, diff->opts.new_prefix);

	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_REVERSE)) {
		const char *tmp_prefix = diff->opts.old_prefix;
		diff->opts.old_prefix  = diff->opts.new_prefix;
		diff->opts.new_prefix  = tmp_prefix;
	}

	git_config_free(cfg);

	/* check strdup results for error */
	return (!diff->opts.old_prefix || !diff->opts.new_prefix) ? -1 : 0;
}

static void diff_list_free(git_diff *diff)
{
	git_vector_free_deep(&diff->deltas);

	git_pathspec__vfree(&diff->pathspec);
	git_pool_clear(&diff->pool);

	git__memzero(diff, sizeof(*diff));
	git__free(diff);
}

void git_diff_free(git_diff *diff)
{
	if (!diff)
		return;

	GIT_REFCOUNT_DEC(diff, diff_list_free);
}

void git_diff_addref(git_diff *diff)
{
	GIT_REFCOUNT_INC(diff);
}

int git_diff__oid_for_file(
	git_oid *out,
	git_diff *diff,
	const char *path,
	uint16_t mode,
	git_off_t size)
{
	git_index_entry entry;

	memset(&entry, 0, sizeof(entry));
	entry.mode = mode;
	entry.file_size = size;
	entry.path = (char *)path;

	return git_diff__oid_for_entry(out, diff, &entry, mode, NULL);
}

int git_diff__oid_for_entry(
	git_oid *out,
	git_diff *diff,
	const git_index_entry *src,
	uint16_t mode,
	const git_oid *update_match)
{
	int error = 0;
	git_buf full_path = GIT_BUF_INIT;
	git_index_entry entry = *src;
	git_filter_list *fl = NULL;

	memset(out, 0, sizeof(*out));

	if (git_buf_joinpath(
		&full_path, git_repository_workdir(diff->repo), entry.path) < 0)
		return -1;

	if (!mode) {
		struct stat st;

		diff->perf.stat_calls++;

		if (p_stat(full_path.ptr, &st) < 0) {
			error = git_path_set_error(errno, entry.path, "stat");
			git_buf_free(&full_path);
			return error;
		}

		git_index_entry__init_from_stat(
			&entry, &st, (diff->diffcaps & GIT_DIFFCAPS_TRUST_MODE_BITS) != 0);
	}

	/* calculate OID for file if possible */
	if (S_ISGITLINK(mode)) {
		git_submodule *sm;

		if (!git_submodule_lookup(&sm, diff->repo, entry.path)) {
			const git_oid *sm_oid = git_submodule_wd_id(sm);
			if (sm_oid)
				git_oid_cpy(out, sm_oid);
			git_submodule_free(sm);
		} else {
			/* if submodule lookup failed probably just in an intermediate
			 * state where some init hasn't happened, so ignore the error
			 */
			giterr_clear();
		}
	} else if (S_ISLNK(mode)) {
		error = git_odb__hashlink(out, full_path.ptr);
		diff->perf.oid_calculations++;
	} else if (!git__is_sizet(entry.file_size)) {
		giterr_set(GITERR_OS, "File size overflow (for 32-bits) on '%s'",
			entry.path);
		error = -1;
	} else if (!(error = git_filter_list_load(
		&fl, diff->repo, NULL, entry.path,
		GIT_FILTER_TO_ODB, GIT_FILTER_ALLOW_UNSAFE)))
	{
		int fd = git_futils_open_ro(full_path.ptr);
		if (fd < 0)
			error = fd;
		else {
			error = git_odb__hashfd_filtered(
				out, fd, (size_t)entry.file_size, GIT_OBJ_BLOB, fl);
			p_close(fd);
			diff->perf.oid_calculations++;
		}

		git_filter_list_free(fl);
	}

	/* update index for entry if requested */
	if (!error && update_match && git_oid_equal(out, update_match)) {
		git_index *idx;
		git_index_entry updated_entry;

		memcpy(&updated_entry, &entry, sizeof(git_index_entry));
		updated_entry.mode = mode;
		git_oid_cpy(&updated_entry.id, out);

		if (!(error = git_repository_index__weakptr(&idx, diff->repo))) {
			error = git_index_add(idx, &updated_entry);
			diff->index_updated = true;
		}
 	}

	git_buf_free(&full_path);
	return error;
}

typedef struct {
	git_repository *repo;
	git_iterator *old_iter;
	git_iterator *new_iter;
	const git_index_entry *oitem;
	const git_index_entry *nitem;
} diff_in_progress;

#define MODE_BITS_MASK 0000777

static int maybe_modified_submodule(
	git_delta_t *status,
	git_oid *found_oid,
	git_diff *diff,
	diff_in_progress *info)
{
	int error = 0;
	git_submodule *sub;
	unsigned int sm_status = 0;
	git_submodule_ignore_t ign = diff->opts.ignore_submodules;

	*status = GIT_DELTA_UNMODIFIED;

	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_IGNORE_SUBMODULES) ||
		ign == GIT_SUBMODULE_IGNORE_ALL)
		return 0;

	if ((error = git_submodule_lookup(
			&sub, diff->repo, info->nitem->path)) < 0) {

		/* GIT_EEXISTS means dir with .git in it was found - ignore it */
		if (error == GIT_EEXISTS) {
			giterr_clear();
			error = 0;
		}
		return error;
	}

	if (ign <= 0 && git_submodule_ignore(sub) == GIT_SUBMODULE_IGNORE_ALL)
		/* ignore it */;
	else if ((error = git_submodule__status(
			&sm_status, NULL, NULL, found_oid, sub, ign)) < 0)
		/* return error below */;

	/* check IS_WD_UNMODIFIED because this case is only used
	 * when the new side of the diff is the working directory
	 */
	else if (!GIT_SUBMODULE_STATUS_IS_WD_UNMODIFIED(sm_status))
		*status = GIT_DELTA_MODIFIED;

	/* now that we have a HEAD OID, check if HEAD moved */
	else if ((sm_status & GIT_SUBMODULE_STATUS_IN_WD) != 0 &&
		!git_oid_equal(&info->oitem->id, found_oid))
		*status = GIT_DELTA_MODIFIED;

	git_submodule_free(sub);
	return error;
}

static int maybe_modified(
	git_diff *diff,
	diff_in_progress *info)
{
	git_oid noid;
	git_delta_t status = GIT_DELTA_MODIFIED;
	const git_index_entry *oitem = info->oitem;
	const git_index_entry *nitem = info->nitem;
	unsigned int omode = oitem->mode;
	unsigned int nmode = nitem->mode;
	bool new_is_workdir = (info->new_iter->type == GIT_ITERATOR_TYPE_WORKDIR);
	bool modified_uncertain = false;
	const char *matched_pathspec;
	int error = 0;

	if (!diff_pathspec_match(&matched_pathspec, diff, oitem))
		return 0;

	memset(&noid, 0, sizeof(noid));

	/* on platforms with no symlinks, preserve mode of existing symlinks */
	if (S_ISLNK(omode) && S_ISREG(nmode) && new_is_workdir &&
		!(diff->diffcaps & GIT_DIFFCAPS_HAS_SYMLINKS))
		nmode = omode;

	/* on platforms with no execmode, just preserve old mode */
	if (!(diff->diffcaps & GIT_DIFFCAPS_TRUST_MODE_BITS) &&
		(nmode & MODE_BITS_MASK) != (omode & MODE_BITS_MASK) &&
		new_is_workdir)
		nmode = (nmode & ~MODE_BITS_MASK) | (omode & MODE_BITS_MASK);

	/* if one side is a conflict, mark the whole delta as conflicted */
	if (git_index_entry_is_conflict(oitem) ||
			git_index_entry_is_conflict(nitem)) {
		status = GIT_DELTA_CONFLICTED;

	/* support "assume unchanged" (poorly, b/c we still stat everything) */
	} else if ((oitem->flags & GIT_IDXENTRY_VALID) != 0) {
		status = GIT_DELTA_UNMODIFIED;

	/* support "skip worktree" index bit */
	} else if ((oitem->flags_extended & GIT_IDXENTRY_SKIP_WORKTREE) != 0) {
		status = GIT_DELTA_UNMODIFIED;

	/* if basic type of file changed, then split into delete and add */
	} else if (GIT_MODE_TYPE(omode) != GIT_MODE_TYPE(nmode)) {
		if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_INCLUDE_TYPECHANGE)) {
			status = GIT_DELTA_TYPECHANGE;
		}

		else if (nmode == GIT_FILEMODE_UNREADABLE) {
			if (!(error = diff_delta__from_one(diff, GIT_DELTA_DELETED, oitem, NULL)))
				error = diff_delta__from_one(diff, GIT_DELTA_UNREADABLE, NULL, nitem);
			return error;
		}

		else {
			if (!(error = diff_delta__from_one(diff, GIT_DELTA_DELETED, oitem, NULL)))
				error = diff_delta__from_one(diff, GIT_DELTA_ADDED, NULL, nitem);
			return error;
		}

	/* if oids and modes match (and are valid), then file is unmodified */
	} else if (git_oid_equal(&oitem->id, &nitem->id) &&
			 omode == nmode &&
			 !git_oid_iszero(&oitem->id)) {
		status = GIT_DELTA_UNMODIFIED;

	/* if we have an unknown OID and a workdir iterator, then check some
	 * circumstances that can accelerate things or need special handling
	 */
	} else if (git_oid_iszero(&nitem->id) && new_is_workdir) {
		bool use_ctime = ((diff->diffcaps & GIT_DIFFCAPS_TRUST_CTIME) != 0);
		git_index *index;
		git_iterator_index(&index, info->new_iter);

		status = GIT_DELTA_UNMODIFIED;

		if (S_ISGITLINK(nmode)) {
			if ((error = maybe_modified_submodule(&status, &noid, diff, info)) < 0)
				return error;
		}

		/* if the stat data looks different, then mark modified - this just
		 * means that the OID will be recalculated below to confirm change
		 */
		else if (omode != nmode || oitem->file_size != nitem->file_size) {
			status = GIT_DELTA_MODIFIED;
			modified_uncertain =
				(oitem->file_size <= 0 && nitem->file_size > 0);
		}
		else if (!git_index_time_eq(&oitem->mtime, &nitem->mtime) ||
			(use_ctime && !git_index_time_eq(&oitem->ctime, &nitem->ctime)) ||
			oitem->ino != nitem->ino ||
			oitem->uid != nitem->uid ||
			oitem->gid != nitem->gid ||
			git_index_entry_newer_than_index(nitem, index))
		{
			status = GIT_DELTA_MODIFIED;
			modified_uncertain = true;
		}

	/* if mode is GITLINK and submodules are ignored, then skip */
	} else if (S_ISGITLINK(nmode) &&
			 DIFF_FLAG_IS_SET(diff, GIT_DIFF_IGNORE_SUBMODULES)) {
		status = GIT_DELTA_UNMODIFIED;
	}

	/* if we got here and decided that the files are modified, but we
	 * haven't calculated the OID of the new item, then calculate it now
	 */
	if (modified_uncertain && git_oid_iszero(&nitem->id)) {
		const git_oid *update_check =
			DIFF_FLAG_IS_SET(diff, GIT_DIFF_UPDATE_INDEX) && omode == nmode ?
			&oitem->id : NULL;

		if ((error = git_diff__oid_for_entry(
				&noid, diff, nitem, nmode, update_check)) < 0)
			return error;

		/* if oid matches, then mark unmodified (except submodules, where
		 * the filesystem content may be modified even if the oid still
		 * matches between the index and the workdir HEAD)
		 */
		if (omode == nmode && !S_ISGITLINK(omode) &&
			git_oid_equal(&oitem->id, &noid))
			status = GIT_DELTA_UNMODIFIED;
	}

	/* If we want case changes, then break this into a delete of the old
	 * and an add of the new so that consumers can act accordingly (eg,
	 * checkout will update the case on disk.)
	 */
	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_IGNORE_CASE) &&
		DIFF_FLAG_IS_SET(diff, GIT_DIFF_INCLUDE_CASECHANGE) &&
		strcmp(oitem->path, nitem->path) != 0) {

		if (!(error = diff_delta__from_one(diff, GIT_DELTA_DELETED, oitem, NULL)))
			error = diff_delta__from_one(diff, GIT_DELTA_ADDED, NULL, nitem);

		return error;
	}

	return diff_delta__from_two(
		diff, status, oitem, omode, nitem, nmode,
		git_oid_iszero(&noid) ? NULL : &noid, matched_pathspec);
}

static bool entry_is_prefixed(
	git_diff *diff,
	const git_index_entry *item,
	const git_index_entry *prefix_item)
{
	size_t pathlen;

	if (!item || diff->pfxcomp(item->path, prefix_item->path) != 0)
		return false;

	pathlen = strlen(prefix_item->path);

	return (prefix_item->path[pathlen - 1] == '/' ||
			item->path[pathlen] == '\0' ||
			item->path[pathlen] == '/');
}

static int iterator_current(
	const git_index_entry **entry,
	git_iterator *iterator)
{
	int error;

	if ((error = git_iterator_current(entry, iterator)) == GIT_ITEROVER) {
		*entry = NULL;
		error = 0;
	}

	return error;
}

static int iterator_advance(
	const git_index_entry **entry,
	git_iterator *iterator)
{
	const git_index_entry *prev_entry = *entry;
	int cmp, error;

	/* if we're looking for conflicts, we only want to report
	 * one conflict for each file, instead of all three sides.
	 * so if this entry is a conflict for this file, and the
	 * previous one was a conflict for the same file, skip it.
	 */
	while ((error = git_iterator_advance(entry, iterator)) == 0) {
		if (!(iterator->flags & GIT_ITERATOR_INCLUDE_CONFLICTS) ||
			!git_index_entry_is_conflict(prev_entry) ||
			!git_index_entry_is_conflict(*entry))
			break;

		cmp = (iterator->flags & GIT_ITERATOR_IGNORE_CASE) ?
			strcasecmp(prev_entry->path, (*entry)->path) :
			strcmp(prev_entry->path, (*entry)->path);

		if (cmp)
			break;
	}

	if (error == GIT_ITEROVER) {
		*entry = NULL;
		error = 0;
	}

	return error;
}

static int iterator_advance_into(
	const git_index_entry **entry,
	git_iterator *iterator)
{
	int error;

	if ((error = git_iterator_advance_into(entry, iterator)) == GIT_ITEROVER) {
		*entry = NULL;
		error = 0;
	}

	return error;
}

static int iterator_advance_over_with_status(
	const git_index_entry **entry,
	git_iterator_status_t *status,
	git_iterator *iterator)
{
	int error;

	if ((error = git_iterator_advance_over_with_status(
			entry, status, iterator)) == GIT_ITEROVER) {
		*entry = NULL;
		error = 0;
	}

	return error;
}

static int handle_unmatched_new_item(
	git_diff *diff, diff_in_progress *info)
{
	int error = 0;
	const git_index_entry *nitem = info->nitem;
	git_delta_t delta_type = GIT_DELTA_UNTRACKED;
	bool contains_oitem;

	/* check if this is a prefix of the other side */
	contains_oitem = entry_is_prefixed(diff, info->oitem, nitem);

	/* update delta_type if this item is conflicted */
	if (git_index_entry_is_conflict(nitem))
		delta_type = GIT_DELTA_CONFLICTED;

	/* update delta_type if this item is ignored */
	else if (git_iterator_current_is_ignored(info->new_iter))
		delta_type = GIT_DELTA_IGNORED;

	if (nitem->mode == GIT_FILEMODE_TREE) {
		bool recurse_into_dir = contains_oitem;

		/* check if user requests recursion into this type of dir */
		recurse_into_dir = contains_oitem ||
			(delta_type == GIT_DELTA_UNTRACKED &&
			 DIFF_FLAG_IS_SET(diff, GIT_DIFF_RECURSE_UNTRACKED_DIRS)) ||
			(delta_type == GIT_DELTA_IGNORED &&
			 DIFF_FLAG_IS_SET(diff, GIT_DIFF_RECURSE_IGNORED_DIRS));

		/* do not advance into directories that contain a .git file */
		if (recurse_into_dir && !contains_oitem) {
			git_buf *full = NULL;
			if (git_iterator_current_workdir_path(&full, info->new_iter) < 0)
				return -1;
			if (full && git_path_contains(full, DOT_GIT)) {
				/* TODO: warning if not a valid git repository */
				recurse_into_dir = false;
			}
		}

		/* still have to look into untracked directories to match core git -
		 * with no untracked files, directory is treated as ignored
		 */
		if (!recurse_into_dir &&
			delta_type == GIT_DELTA_UNTRACKED &&
			DIFF_FLAG_ISNT_SET(diff, GIT_DIFF_ENABLE_FAST_UNTRACKED_DIRS))
		{
			git_diff_delta *last;
			git_iterator_status_t untracked_state;

			/* attempt to insert record for this directory */
			if ((error = diff_delta__from_one(diff, delta_type, NULL, nitem)) != 0)
				return error;

			/* if delta wasn't created (because of rules), just skip ahead */
			last = diff_delta__last_for_item(diff, nitem);
			if (!last)
				return iterator_advance(&info->nitem, info->new_iter);

			/* iterate into dir looking for an actual untracked file */
			if ((error = iterator_advance_over_with_status(
					&info->nitem, &untracked_state, info->new_iter)) < 0)
				return error;

			/* if we found nothing that matched our pathlist filter, exclude */
			if (untracked_state == GIT_ITERATOR_STATUS_FILTERED) {
				git_vector_pop(&diff->deltas);
				git__free(last);
			}

			/* if we found nothing or just ignored items, update the record */
			if (untracked_state == GIT_ITERATOR_STATUS_IGNORED ||
				untracked_state == GIT_ITERATOR_STATUS_EMPTY) {
				last->status = GIT_DELTA_IGNORED;

				/* remove the record if we don't want ignored records */
				if (DIFF_FLAG_ISNT_SET(diff, GIT_DIFF_INCLUDE_IGNORED)) {
					git_vector_pop(&diff->deltas);
					git__free(last);
				}
			}

			return 0;
		}

		/* try to advance into directory if necessary */
		if (recurse_into_dir) {
			error = iterator_advance_into(&info->nitem, info->new_iter);

			/* if real error or no error, proceed with iteration */
			if (error != GIT_ENOTFOUND)
				return error;
			giterr_clear();

			/* if directory is empty, can't advance into it, so either skip
			 * it or ignore it
			 */
			if (contains_oitem)
				return iterator_advance(&info->nitem, info->new_iter);
			delta_type = GIT_DELTA_IGNORED;
		}
	}

	else if (delta_type == GIT_DELTA_IGNORED &&
		DIFF_FLAG_ISNT_SET(diff, GIT_DIFF_RECURSE_IGNORED_DIRS) &&
		git_iterator_current_tree_is_ignored(info->new_iter))
		/* item contained in ignored directory, so skip over it */
		return iterator_advance(&info->nitem, info->new_iter);

	else if (info->new_iter->type != GIT_ITERATOR_TYPE_WORKDIR) {
		if (delta_type != GIT_DELTA_CONFLICTED)
			delta_type = GIT_DELTA_ADDED;
	}

	else if (nitem->mode == GIT_FILEMODE_COMMIT) {
		/* ignore things that are not actual submodules */
		if (git_submodule_lookup(NULL, info->repo, nitem->path) != 0) {
			giterr_clear();
			delta_type = GIT_DELTA_IGNORED;

			/* if this contains a tracked item, treat as normal TREE */
			if (contains_oitem) {
				error = iterator_advance_into(&info->nitem, info->new_iter);
				if (error != GIT_ENOTFOUND)
					return error;

				giterr_clear();
				return iterator_advance(&info->nitem, info->new_iter);
			}
		}
	}

	else if (nitem->mode == GIT_FILEMODE_UNREADABLE) {
		if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_INCLUDE_UNREADABLE_AS_UNTRACKED))
			delta_type = GIT_DELTA_UNTRACKED;
		else
			delta_type = GIT_DELTA_UNREADABLE;
	}

	/* Actually create the record for this item if necessary */
	if ((error = diff_delta__from_one(diff, delta_type, NULL, nitem)) != 0)
		return error;

	/* If user requested TYPECHANGE records, then check for that instead of
	 * just generating an ADDED/UNTRACKED record
	 */
	if (delta_type != GIT_DELTA_IGNORED &&
		DIFF_FLAG_IS_SET(diff, GIT_DIFF_INCLUDE_TYPECHANGE_TREES) &&
		contains_oitem)
	{
		/* this entry was prefixed with a tree - make TYPECHANGE */
		git_diff_delta *last = diff_delta__last_for_item(diff, nitem);
		if (last) {
			last->status = GIT_DELTA_TYPECHANGE;
			last->old_file.mode = GIT_FILEMODE_TREE;
		}
	}

	return iterator_advance(&info->nitem, info->new_iter);
}

static int handle_unmatched_old_item(
	git_diff *diff, diff_in_progress *info)
{
	git_delta_t delta_type = GIT_DELTA_DELETED;
	int error;

	/* update delta_type if this item is conflicted */
	if (git_index_entry_is_conflict(info->oitem))
		delta_type = GIT_DELTA_CONFLICTED;

	if ((error = diff_delta__from_one(diff, delta_type, info->oitem, NULL)) < 0)
		return error;

	/* if we are generating TYPECHANGE records then check for that
	 * instead of just generating a DELETE record
	 */
	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_INCLUDE_TYPECHANGE_TREES) &&
		entry_is_prefixed(diff, info->nitem, info->oitem))
	{
		/* this entry has become a tree! convert to TYPECHANGE */
		git_diff_delta *last = diff_delta__last_for_item(diff, info->oitem);
		if (last) {
			last->status = GIT_DELTA_TYPECHANGE;
			last->new_file.mode = GIT_FILEMODE_TREE;
		}

		/* If new_iter is a workdir iterator, then this situation
		 * will certainly be followed by a series of untracked items.
		 * Unless RECURSE_UNTRACKED_DIRS is set, skip over them...
		 */
		if (S_ISDIR(info->nitem->mode) &&
			DIFF_FLAG_ISNT_SET(diff, GIT_DIFF_RECURSE_UNTRACKED_DIRS))
			return iterator_advance(&info->nitem, info->new_iter);
	}

	return iterator_advance(&info->oitem, info->old_iter);
}

static int handle_matched_item(
	git_diff *diff, diff_in_progress *info)
{
	int error = 0;

	if ((error = maybe_modified(diff, info)) < 0)
		return error;

	if (!(error = iterator_advance(&info->oitem, info->old_iter)))
		error = iterator_advance(&info->nitem, info->new_iter);

	return error;
}

int git_diff__from_iterators(
	git_diff **diff_ptr,
	git_repository *repo,
	git_iterator *old_iter,
	git_iterator *new_iter,
	const git_diff_options *opts)
{
	int error = 0;
	diff_in_progress info;
	git_diff *diff;

	*diff_ptr = NULL;

	diff = diff_list_alloc(repo, old_iter, new_iter);
	GITERR_CHECK_ALLOC(diff);

	info.repo = repo;
	info.old_iter = old_iter;
	info.new_iter = new_iter;

	/* make iterators have matching icase behavior */
	if (DIFF_FLAG_IS_SET(diff, GIT_DIFF_IGNORE_CASE)) {
		if ((error = git_iterator_set_ignore_case(old_iter, true)) < 0 ||
			(error = git_iterator_set_ignore_case(new_iter, true)) < 0)
			goto cleanup;
	}

	/* finish initialization */
	if ((error = diff_list_apply_options(diff, opts)) < 0)
		goto cleanup;

	if ((error = iterator_current(&info.oitem, old_iter)) < 0 ||
		(error = iterator_current(&info.nitem, new_iter)) < 0)
		goto cleanup;

	/* run iterators building diffs */
	while (!error && (info.oitem || info.nitem)) {
		int cmp;

		/* report progress */
		if (opts && opts->progress_cb) {
			if ((error = opts->progress_cb(diff,
					info.oitem ? info.oitem->path : NULL,
					info.nitem ? info.nitem->path : NULL,
					opts->payload)))
				break;
		}

		cmp = info.oitem ?
			(info.nitem ? diff->entrycomp(info.oitem, info.nitem) : -1) : 1;

		/* create DELETED records for old items not matched in new */
		if (cmp < 0)
			error = handle_unmatched_old_item(diff, &info);

		/* create ADDED, TRACKED, or IGNORED records for new items not
		 * matched in old (and/or descend into directories as needed)
		 */
		else if (cmp > 0)
			error = handle_unmatched_new_item(diff, &info);

		/* otherwise item paths match, so create MODIFIED record
		 * (or ADDED and DELETED pair if type changed)
		 */
		else
			error = handle_matched_item(diff, &info);
	}

	diff->perf.stat_calls += old_iter->stat_calls + new_iter->stat_calls;

cleanup:
	if (!error)
		*diff_ptr = diff;
	else
		git_diff_free(diff);

	return error;
}

#define DIFF_FROM_ITERATORS(MAKE_FIRST, FLAGS_FIRST, MAKE_SECOND, FLAGS_SECOND) do { \
	git_iterator *a = NULL, *b = NULL; \
	char *pfx = (opts && !(opts->flags & GIT_DIFF_DISABLE_PATHSPEC_MATCH)) ? \
		git_pathspec_prefix(&opts->pathspec) : NULL; \
	git_iterator_options a_opts = GIT_ITERATOR_OPTIONS_INIT, \
		b_opts = GIT_ITERATOR_OPTIONS_INIT; \
	a_opts.flags = FLAGS_FIRST; \
	a_opts.start = pfx; \
	a_opts.end = pfx; \
	b_opts.flags = FLAGS_SECOND; \
	b_opts.start = pfx; \
	b_opts.end = pfx; \
	GITERR_CHECK_VERSION(opts, GIT_DIFF_OPTIONS_VERSION, "git_diff_options"); \
	if (opts && (opts->flags & GIT_DIFF_DISABLE_PATHSPEC_MATCH)) { \
		a_opts.pathlist.strings = opts->pathspec.strings; \
		a_opts.pathlist.count = opts->pathspec.count; \
		b_opts.pathlist.strings = opts->pathspec.strings; \
		b_opts.pathlist.count = opts->pathspec.count; \
	} \
	if (!error && !(error = MAKE_FIRST) && !(error = MAKE_SECOND)) \
		error = git_diff__from_iterators(diff, repo, a, b, opts); \
	git__free(pfx); git_iterator_free(a); git_iterator_free(b); \
} while (0)

int git_diff_tree_to_tree(
	git_diff **diff,
	git_repository *repo,
	git_tree *old_tree,
	git_tree *new_tree,
	const git_diff_options *opts)
{
	git_iterator_flag_t iflag = GIT_ITERATOR_DONT_IGNORE_CASE;
	int error = 0;

	assert(diff && repo);

	/* for tree to tree diff, be case sensitive even if the index is
	 * currently case insensitive, unless the user explicitly asked
	 * for case insensitivity
	 */
	if (opts && (opts->flags & GIT_DIFF_IGNORE_CASE) != 0)
		iflag = GIT_ITERATOR_IGNORE_CASE;

	DIFF_FROM_ITERATORS(
		git_iterator_for_tree(&a, old_tree, &a_opts), iflag,
		git_iterator_for_tree(&b, new_tree, &b_opts), iflag
	);

	return error;
}

static int diff_load_index(git_index **index, git_repository *repo)
{
	int error = git_repository_index__weakptr(index, repo);

	/* reload the repository index when user did not pass one in */
	if (!error && git_index_read(*index, false) < 0)
		giterr_clear();

	return error;
}

int git_diff_tree_to_index(
	git_diff **diff,
	git_repository *repo,
	git_tree *old_tree,
	git_index *index,
	const git_diff_options *opts)
{
	git_iterator_flag_t iflag = GIT_ITERATOR_DONT_IGNORE_CASE |
		GIT_ITERATOR_INCLUDE_CONFLICTS;
	bool index_ignore_case = false;
	int error = 0;

	assert(diff && repo);

	if (!index && (error = diff_load_index(&index, repo)) < 0)
		return error;

	index_ignore_case = index->ignore_case;

	DIFF_FROM_ITERATORS(
		git_iterator_for_tree(&a, old_tree, &a_opts), iflag,
		git_iterator_for_index(&b, repo, index, &b_opts), iflag
	);

	/* if index is in case-insensitive order, re-sort deltas to match */
	if (!error && index_ignore_case)
		diff_set_ignore_case(*diff, true);

	return error;
}

int git_diff_index_to_workdir(
	git_diff **diff,
	git_repository *repo,
	git_index *index,
	const git_diff_options *opts)
{
	int error = 0;

	assert(diff && repo);

	if (!index && (error = diff_load_index(&index, repo)) < 0)
		return error;

	DIFF_FROM_ITERATORS(
		git_iterator_for_index(&a, repo, index, &a_opts),
		GIT_ITERATOR_INCLUDE_CONFLICTS,

		git_iterator_for_workdir(&b, repo, index, NULL, &b_opts),
		GIT_ITERATOR_DONT_AUTOEXPAND
	);

	if (!error && DIFF_FLAG_IS_SET(*diff, GIT_DIFF_UPDATE_INDEX) && (*diff)->index_updated)
		error = git_index_write(index);

	return error;
}

int git_diff_tree_to_workdir(
	git_diff **diff,
	git_repository *repo,
	git_tree *old_tree,
	const git_diff_options *opts)
{
	int error = 0;
	git_index *index;

	assert(diff && repo);

	if ((error = git_repository_index__weakptr(&index, repo)))
		return error;

	DIFF_FROM_ITERATORS(
		git_iterator_for_tree(&a, old_tree, &a_opts), 0,
		git_iterator_for_workdir(&b, repo, index, old_tree, &b_opts), GIT_ITERATOR_DONT_AUTOEXPAND
	);

	return error;
}

int git_diff_tree_to_workdir_with_index(
	git_diff **diff,
	git_repository *repo,
	git_tree *old_tree,
	const git_diff_options *opts)
{
	int error = 0;
	git_diff *d1 = NULL, *d2 = NULL;
	git_index *index = NULL;

	assert(diff && repo);

	if ((error = diff_load_index(&index, repo)) < 0)
		return error;

	if (!(error = git_diff_tree_to_index(&d1, repo, old_tree, index, opts)) &&
		!(error = git_diff_index_to_workdir(&d2, repo, index, opts)))
		error = git_diff_merge(d1, d2);

	git_diff_free(d2);

	if (error) {
		git_diff_free(d1);
		d1 = NULL;
	}

	*diff = d1;
	return error;
}

int git_diff_index_to_index(
	git_diff **diff,
	git_repository *repo,
	git_index *old_index,
	git_index *new_index,
	const git_diff_options *opts)
{
	int error = 0;

	assert(diff && old_index && new_index);

	DIFF_FROM_ITERATORS(
		git_iterator_for_index(&a, repo, old_index, &a_opts), GIT_ITERATOR_DONT_IGNORE_CASE,
		git_iterator_for_index(&b, repo, new_index, &b_opts), GIT_ITERATOR_DONT_IGNORE_CASE
	);

	/* if index is in case-insensitive order, re-sort deltas to match */
	if (!error && (old_index->ignore_case || new_index->ignore_case))
		diff_set_ignore_case(*diff, true);

	return error;
}

size_t git_diff_num_deltas(const git_diff *diff)
{
	assert(diff);
	return diff->deltas.length;
}

size_t git_diff_num_deltas_of_type(const git_diff *diff, git_delta_t type)
{
	size_t i, count = 0;
	const git_diff_delta *delta;

	assert(diff);

	git_vector_foreach(&diff->deltas, i, delta) {
		count += (delta->status == type);
	}

	return count;
}

const git_diff_delta *git_diff_get_delta(const git_diff *diff, size_t idx)
{
	assert(diff);
	return git_vector_get(&diff->deltas, idx);
}

int git_diff_is_sorted_icase(const git_diff *diff)
{
	return (diff->opts.flags & GIT_DIFF_IGNORE_CASE) != 0;
}

int git_diff_get_perfdata(git_diff_perfdata *out, const git_diff *diff)
{
	assert(out);
	GITERR_CHECK_VERSION(out, GIT_DIFF_PERFDATA_VERSION, "git_diff_perfdata");
	out->stat_calls = diff->perf.stat_calls;
	out->oid_calculations = diff->perf.oid_calculations;
	return 0;
}

int git_diff__paired_foreach(
	git_diff *head2idx,
	git_diff *idx2wd,
	int (*cb)(git_diff_delta *h2i, git_diff_delta *i2w, void *payload),
	void *payload)
{
	int cmp, error = 0;
	git_diff_delta *h2i, *i2w;
	size_t i, j, i_max, j_max;
	int (*strcomp)(const char *, const char *) = git__strcmp;
	bool h2i_icase, i2w_icase, icase_mismatch;

	i_max = head2idx ? head2idx->deltas.length : 0;
	j_max = idx2wd ? idx2wd->deltas.length : 0;
	if (!i_max && !j_max)
		return 0;

	/* At some point, tree-to-index diffs will probably never ignore case,
	 * even if that isn't true now.  Index-to-workdir diffs may or may not
	 * ignore case, but the index filename for the idx2wd diff should
	 * still be using the canonical case-preserving name.
	 *
	 * Therefore the main thing we need to do here is make sure the diffs
	 * are traversed in a compatible order.  To do this, we temporarily
	 * resort a mismatched diff to get the order correct.
	 *
	 * In order to traverse renames in the index->workdir, we need to
	 * ensure that we compare the index name on both sides, so we
	 * always sort by the old name in the i2w list.
	 */
	h2i_icase = head2idx != NULL &&
		(head2idx->opts.flags & GIT_DIFF_IGNORE_CASE) != 0;

	i2w_icase = idx2wd != NULL &&
		(idx2wd->opts.flags & GIT_DIFF_IGNORE_CASE) != 0;

	icase_mismatch =
		(head2idx != NULL && idx2wd != NULL && h2i_icase != i2w_icase);

	if (icase_mismatch && h2i_icase) {
		git_vector_set_cmp(&head2idx->deltas, git_diff_delta__cmp);
		git_vector_sort(&head2idx->deltas);
	}

	if (i2w_icase && !icase_mismatch) {
		strcomp = git__strcasecmp;

		git_vector_set_cmp(&idx2wd->deltas, git_diff_delta__i2w_casecmp);
		git_vector_sort(&idx2wd->deltas);
	} else if (idx2wd != NULL) {
		git_vector_set_cmp(&idx2wd->deltas, git_diff_delta__i2w_cmp);
		git_vector_sort(&idx2wd->deltas);
	}

	for (i = 0, j = 0; i < i_max || j < j_max; ) {
		h2i = head2idx ? GIT_VECTOR_GET(&head2idx->deltas, i) : NULL;
		i2w = idx2wd ? GIT_VECTOR_GET(&idx2wd->deltas, j) : NULL;

		cmp = !i2w ? -1 : !h2i ? 1 :
			strcomp(h2i->new_file.path, i2w->old_file.path);

		if (cmp < 0) {
			i++; i2w = NULL;
		} else if (cmp > 0) {
			j++; h2i = NULL;
		} else {
			i++; j++;
		}

		if ((error = cb(h2i, i2w, payload)) != 0) {
			giterr_set_after_callback(error);
			break;
		}
	}

	/* restore case-insensitive delta sort */
	if (icase_mismatch && h2i_icase) {
		git_vector_set_cmp(&head2idx->deltas, git_diff_delta__casecmp);
		git_vector_sort(&head2idx->deltas);
	}

	/* restore idx2wd sort by new path */
	if (idx2wd != NULL) {
		git_vector_set_cmp(&idx2wd->deltas,
			i2w_icase ? git_diff_delta__casecmp : git_diff_delta__cmp);
		git_vector_sort(&idx2wd->deltas);
	}

	return error;
}

int git_diff__commit(
	git_diff **diff,
	git_repository *repo,
	const git_commit *commit,
	const git_diff_options *opts)
{
	git_commit *parent = NULL;
	git_diff *commit_diff = NULL;
	git_tree *old_tree = NULL, *new_tree = NULL;
	size_t parents;
	int error = 0;

	if ((parents = git_commit_parentcount(commit)) > 1) {
		char commit_oidstr[GIT_OID_HEXSZ + 1];

		error = -1;
		giterr_set(GITERR_INVALID, "Commit %s is a merge commit",
			git_oid_tostr(commit_oidstr, GIT_OID_HEXSZ + 1, git_commit_id(commit)));
		goto on_error;
	}

	if (parents > 0)
		if ((error = git_commit_parent(&parent, commit, 0)) < 0 ||
			(error = git_commit_tree(&old_tree, parent)) < 0)
				goto on_error;

	if ((error = git_commit_tree(&new_tree, commit)) < 0 ||
		(error = git_diff_tree_to_tree(&commit_diff, repo, old_tree, new_tree, opts)) < 0)
			goto on_error;

	*diff = commit_diff;

on_error:
	git_tree_free(new_tree);
	git_tree_free(old_tree);
	git_commit_free(parent);

	return error;
}

int git_diff_format_email__append_header_tobuf(
	git_buf *out,
	const git_oid *id,
	const git_signature *author,
	const char *summary,
	const char *body,
	size_t patch_no,
	size_t total_patches,
	bool exclude_patchno_marker)
{
	char idstr[GIT_OID_HEXSZ + 1];
	char date_str[GIT_DATE_RFC2822_SZ];
	int error = 0;

	git_oid_fmt(idstr, id);
	idstr[GIT_OID_HEXSZ] = '\0';

	if ((error = git__date_rfc2822_fmt(date_str, sizeof(date_str), &author->when)) < 0)
		return error;

	error = git_buf_printf(out,
				"From %s Mon Sep 17 00:00:00 2001\n" \
				"From: %s <%s>\n" \
				"Date: %s\n" \
				"Subject: ",
				idstr,
				author->name, author->email,
				date_str);

	if (error < 0)
		return error;

	if (!exclude_patchno_marker) {
		if (total_patches == 1) {
			error = git_buf_puts(out, "[PATCH] ");
		} else {
			error = git_buf_printf(out, "[PATCH %"PRIuZ"/%"PRIuZ"] ", patch_no, total_patches);
		}

		if (error < 0)
			return error;
	}

	error = git_buf_printf(out, "%s\n\n", summary);

	if (body) {
		git_buf_puts(out, body);

		if (out->ptr[out->size - 1] != '\n')
			git_buf_putc(out, '\n');
	}

	return error;
}

int git_diff_format_email__append_patches_tobuf(
	git_buf *out,
	git_diff *diff)
{
	size_t i, deltas;
	int error = 0;

	deltas = git_diff_num_deltas(diff);

	for (i = 0; i < deltas; ++i) {
		git_patch *patch = NULL;

		if ((error = git_patch_from_diff(&patch, diff, i)) >= 0)
			error = git_patch_to_buf(out, patch);

		git_patch_free(patch);

		if (error < 0)
			break;
	}

	return error;
}

int git_diff_format_email(
	git_buf *out,
	git_diff *diff,
	const git_diff_format_email_options *opts)
{
	git_diff_stats *stats = NULL;
	char *summary = NULL, *loc = NULL;
	bool ignore_marker;
	unsigned int format_flags = 0;
	size_t allocsize;
	int error;

	assert(out && diff && opts);
	assert(opts->summary && opts->id && opts->author);

	GITERR_CHECK_VERSION(opts, GIT_DIFF_FORMAT_EMAIL_OPTIONS_VERSION, "git_format_email_options");

	if ((ignore_marker = opts->flags & GIT_DIFF_FORMAT_EMAIL_EXCLUDE_SUBJECT_PATCH_MARKER) == false) {
		if (opts->patch_no > opts->total_patches) {
			giterr_set(GITERR_INVALID, "patch %"PRIuZ" out of range. max %"PRIuZ, opts->patch_no, opts->total_patches);
			return -1;
		}

		if (opts->patch_no == 0) {
			giterr_set(GITERR_INVALID, "invalid patch no %"PRIuZ". should be >0", opts->patch_no);
			return -1;
		}
	}

	/* the summary we receive may not be clean.
	 * it could potentially contain new line characters
	 * or not be set, sanitize, */
	if ((loc = strpbrk(opts->summary, "\r\n")) != NULL) {
		size_t offset = 0;

		if ((offset = (loc - opts->summary)) == 0) {
			giterr_set(GITERR_INVALID, "summary is empty");
			error = -1;
			goto on_error;
		}

		GITERR_CHECK_ALLOC_ADD(&allocsize, offset, 1);
		summary = git__calloc(allocsize, sizeof(char));
		GITERR_CHECK_ALLOC(summary);

		strncpy(summary, opts->summary, offset);
	}

	error = git_diff_format_email__append_header_tobuf(out,
				opts->id, opts->author, summary == NULL ? opts->summary : summary,
				opts->body, opts->patch_no, opts->total_patches, ignore_marker);

	if (error < 0)
		goto on_error;

	format_flags = GIT_DIFF_STATS_FULL | GIT_DIFF_STATS_INCLUDE_SUMMARY;

	if ((error = git_buf_puts(out, "---\n")) < 0 ||
		(error = git_diff_get_stats(&stats, diff)) < 0 ||
		(error = git_diff_stats_to_buf(out, stats, format_flags, 0)) < 0 ||
		(error = git_buf_putc(out, '\n')) < 0 ||
		(error = git_diff_format_email__append_patches_tobuf(out, diff)) < 0)
			goto on_error;

	error = git_buf_puts(out, "--\nlibgit2 " LIBGIT2_VERSION "\n\n");

on_error:
	git__free(summary);
	git_diff_stats_free(stats);

	return error;
}

int git_diff_commit_as_email(
	git_buf *out,
	git_repository *repo,
	git_commit *commit,
	size_t patch_no,
	size_t total_patches,
	git_diff_format_email_flags_t flags,
	const git_diff_options *diff_opts)
{
	git_diff *diff = NULL;
	git_diff_format_email_options opts = GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT;
	int error;

	assert (out && repo && commit);

	opts.flags = flags;
	opts.patch_no = patch_no;
	opts.total_patches = total_patches;
	opts.id = git_commit_id(commit);
	opts.summary = git_commit_summary(commit);
	opts.body = git_commit_body(commit);
	opts.author = git_commit_author(commit);

	if ((error = git_diff__commit(&diff, repo, commit, diff_opts)) < 0)
		return error;

	error = git_diff_format_email(out, diff, &opts);

	git_diff_free(diff);
	return error;
}

int git_diff_init_options(git_diff_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_diff_options, GIT_DIFF_OPTIONS_INIT);
	return 0;
}

int git_diff_find_init_options(
	git_diff_find_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_diff_find_options, GIT_DIFF_FIND_OPTIONS_INIT);
	return 0;
}

int git_diff_format_email_init_options(
	git_diff_format_email_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_diff_format_email_options,
		GIT_DIFF_FORMAT_EMAIL_OPTIONS_INIT);
	return 0;
}
