/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "repository.h"
#include "commit.h"
#include "message.h"
#include "tree.h"
#include "reflog.h"
#include "git2/diff.h"
#include "git2/stash.h"
#include "git2/status.h"
#include "git2/checkout.h"
#include "git2/index.h"
#include "git2/transaction.h"
#include "git2/merge.h"
#include "index.h"
#include "signature.h"
#include "iterator.h"
#include "merge.h"
#include "diff.h"

static int create_error(int error, const char *msg)
{
	giterr_set(GITERR_STASH, "Cannot stash changes - %s", msg);
	return error;
}

static int retrieve_head(git_reference **out, git_repository *repo)
{
	int error = git_repository_head(out, repo);

	if (error == GIT_EUNBORNBRANCH)
		return create_error(error, "You do not have the initial commit yet.");

	return error;
}

static int append_abbreviated_oid(git_buf *out, const git_oid *b_commit)
{
	char *formatted_oid;

	formatted_oid = git_oid_allocfmt(b_commit);
	GITERR_CHECK_ALLOC(formatted_oid);

	git_buf_put(out, formatted_oid, 7);
	git__free(formatted_oid);

	return git_buf_oom(out) ? -1 : 0;
}

static int append_commit_description(git_buf *out, git_commit* commit)
{
	const char *summary = git_commit_summary(commit);
	GITERR_CHECK_ALLOC(summary);

	if (append_abbreviated_oid(out, git_commit_id(commit)) < 0)
		return -1;

	git_buf_putc(out, ' ');
	git_buf_puts(out, summary);
	git_buf_putc(out, '\n');

	return git_buf_oom(out) ? -1 : 0;
}

static int retrieve_base_commit_and_message(
	git_commit **b_commit,
	git_buf *stash_message,
	git_repository *repo)
{
	git_reference *head = NULL;
	int error;

	if ((error = retrieve_head(&head, repo)) < 0)
		return error;

	if (strcmp("HEAD", git_reference_name(head)) == 0)
		error = git_buf_puts(stash_message, "(no branch): ");
	else
		error = git_buf_printf(
			stash_message,
			"%s: ",
			git_reference_name(head) + strlen(GIT_REFS_HEADS_DIR));
	if (error < 0)
		goto cleanup;

	if ((error = git_commit_lookup(
			 b_commit, repo, git_reference_target(head))) < 0)
		goto cleanup;

	if ((error = append_commit_description(stash_message, *b_commit)) < 0)
		goto cleanup;

cleanup:
	git_reference_free(head);
	return error;
}

static int build_tree_from_index(git_tree **out, git_index *index)
{
	int error;
	git_oid i_tree_oid;

	if ((error = git_index_write_tree(&i_tree_oid, index)) < 0)
		return error;

	return git_tree_lookup(out, git_index_owner(index), &i_tree_oid);
}

static int commit_index(
	git_commit **i_commit,
	git_index *index,
	const git_signature *stasher,
	const char *message,
	const git_commit *parent)
{
	git_tree *i_tree = NULL;
	git_oid i_commit_oid;
	git_buf msg = GIT_BUF_INIT;
	int error;

	if ((error = build_tree_from_index(&i_tree, index)) < 0)
		goto cleanup;

	if ((error = git_buf_printf(&msg, "index on %s\n", message)) < 0)
		goto cleanup;

	if ((error = git_commit_create(
		&i_commit_oid,
		git_index_owner(index),
		NULL,
		stasher,
		stasher,
		NULL,
		git_buf_cstr(&msg),
		i_tree,
		1,
		&parent)) < 0)
		goto cleanup;

	error = git_commit_lookup(i_commit, git_index_owner(index), &i_commit_oid);

cleanup:
	git_tree_free(i_tree);
	git_buf_free(&msg);
	return error;
}

struct stash_update_rules {
	bool include_changed;
	bool include_untracked;
	bool include_ignored;
};

static int stash_update_index_from_diff(
	git_index *index,
	const git_diff *diff,
	struct stash_update_rules *data)
{
	int error = 0;
	size_t d, max_d = git_diff_num_deltas(diff);

	for (d = 0; !error && d < max_d; ++d) {
		const char *add_path = NULL;
		const git_diff_delta *delta = git_diff_get_delta(diff, d);

		switch (delta->status) {
		case GIT_DELTA_IGNORED:
			if (data->include_ignored)
				add_path = delta->new_file.path;
			break;

		case GIT_DELTA_UNTRACKED:
			if (data->include_untracked &&
				delta->new_file.mode != GIT_FILEMODE_TREE)
				add_path = delta->new_file.path;
			break;

		case GIT_DELTA_ADDED:
		case GIT_DELTA_MODIFIED:
			if (data->include_changed)
				add_path = delta->new_file.path;
			break;

		case GIT_DELTA_DELETED:
			if (data->include_changed &&
				!git_index_find(NULL, index, delta->old_file.path))
				error = git_index_remove(index, delta->old_file.path, 0);
			break;

		default:
			/* Unimplemented */
			giterr_set(
				GITERR_INVALID,
				"Cannot update index. Unimplemented status (%d)",
				delta->status);
			return -1;
		}

		if (add_path != NULL)
			error = git_index_add_bypath(index, add_path);
	}

	return error;
}

static int build_untracked_tree(
	git_tree **tree_out,
	git_index *index,
	git_commit *i_commit,
	uint32_t flags)
{
	git_tree *i_tree = NULL;
	git_diff *diff = NULL;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	struct stash_update_rules data = {0};
	int error;

	git_index_clear(index);

	if (flags & GIT_STASH_INCLUDE_UNTRACKED) {
		opts.flags |= GIT_DIFF_INCLUDE_UNTRACKED |
			GIT_DIFF_RECURSE_UNTRACKED_DIRS;
		data.include_untracked = true;
	}

	if (flags & GIT_STASH_INCLUDE_IGNORED) {
		opts.flags |= GIT_DIFF_INCLUDE_IGNORED |
			GIT_DIFF_RECURSE_IGNORED_DIRS;
		data.include_ignored = true;
	}

	if ((error = git_commit_tree(&i_tree, i_commit)) < 0)
		goto cleanup;

	if ((error = git_diff_tree_to_workdir(
			&diff, git_index_owner(index), i_tree, &opts)) < 0)
		goto cleanup;

	if ((error = stash_update_index_from_diff(index, diff, &data)) < 0)
		goto cleanup;

	error = build_tree_from_index(tree_out, index);

cleanup:
	git_diff_free(diff);
	git_tree_free(i_tree);
	return error;
}

static int commit_untracked(
	git_commit **u_commit,
	git_index *index,
	const git_signature *stasher,
	const char *message,
	git_commit *i_commit,
	uint32_t flags)
{
	git_tree *u_tree = NULL;
	git_oid u_commit_oid;
	git_buf msg = GIT_BUF_INIT;
	int error;

	if ((error = build_untracked_tree(&u_tree, index, i_commit, flags)) < 0)
		goto cleanup;

	if ((error = git_buf_printf(&msg, "untracked files on %s\n", message)) < 0)
		goto cleanup;

	if ((error = git_commit_create(
		&u_commit_oid,
		git_index_owner(index),
		NULL,
		stasher,
		stasher,
		NULL,
		git_buf_cstr(&msg),
		u_tree,
		0,
		NULL)) < 0)
		goto cleanup;

	error = git_commit_lookup(u_commit, git_index_owner(index), &u_commit_oid);

cleanup:
	git_tree_free(u_tree);
	git_buf_free(&msg);
	return error;
}

static git_diff_delta *stash_delta_merge(
	const git_diff_delta *a,
	const git_diff_delta *b,
	git_pool *pool)
{
	/* Special case for stash: if a file is deleted in the index, but exists
	 * in the working tree, we need to stash the workdir copy for the workdir.
	 */
	if (a->status == GIT_DELTA_DELETED && b->status == GIT_DELTA_UNTRACKED) {
		git_diff_delta *dup = git_diff__delta_dup(b, pool);

		if (dup)
			dup->status = GIT_DELTA_MODIFIED;
		return dup;
	}

	return git_diff__merge_like_cgit(a, b, pool);
}

static int build_workdir_tree(
	git_tree **tree_out,
	git_index *index,
	git_commit *b_commit)
{
	git_repository *repo = git_index_owner(index);
	git_tree *b_tree = NULL;
	git_diff *diff = NULL, *idx_to_wd = NULL;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	struct stash_update_rules data = {0};
	int error;

	opts.flags = GIT_DIFF_IGNORE_SUBMODULES | GIT_DIFF_INCLUDE_UNTRACKED;

	if ((error = git_commit_tree(&b_tree, b_commit)) < 0)
		goto cleanup;

	if ((error = git_diff_tree_to_index(&diff, repo, b_tree, index, &opts)) < 0 ||
		(error = git_diff_index_to_workdir(&idx_to_wd, repo, index, &opts)) < 0 ||
		(error = git_diff__merge(diff, idx_to_wd, stash_delta_merge)) < 0)
		goto cleanup;

	data.include_changed = true;

	if ((error = stash_update_index_from_diff(index, diff, &data)) < 0)
		goto cleanup;

	error = build_tree_from_index(tree_out, index);

cleanup:
	git_diff_free(idx_to_wd);
	git_diff_free(diff);
	git_tree_free(b_tree);

	return error;
}

static int commit_worktree(
	git_oid *w_commit_oid,
	git_index *index,
	const git_signature *stasher,
	const char *message,
	git_commit *i_commit,
	git_commit *b_commit,
	git_commit *u_commit)
{
	int error = 0;
	git_tree *w_tree = NULL, *i_tree = NULL;
	const git_commit *parents[] = {	NULL, NULL,	NULL };

	parents[0] = b_commit;
	parents[1] = i_commit;
	parents[2] = u_commit;

	if ((error = git_commit_tree(&i_tree, i_commit)) < 0)
		goto cleanup;

	if ((error = git_index_read_tree(index, i_tree)) < 0)
		goto cleanup;

	if ((error = build_workdir_tree(&w_tree, index, b_commit)) < 0)
		goto cleanup;

	error = git_commit_create(
		w_commit_oid,
		git_index_owner(index),
		NULL,
		stasher,
		stasher,
		NULL,
		message,
		w_tree,
		u_commit ? 3 : 2,
		parents);

cleanup:
	git_tree_free(i_tree);
	git_tree_free(w_tree);
	return error;
}

static int prepare_worktree_commit_message(
	git_buf* msg,
	const char *user_message)
{
	git_buf buf = GIT_BUF_INIT;
	int error;

	if ((error = git_buf_set(&buf, git_buf_cstr(msg), git_buf_len(msg))) < 0)
		return error;

	git_buf_clear(msg);

	if (!user_message)
		git_buf_printf(msg, "WIP on %s", git_buf_cstr(&buf));
	else {
		const char *colon;

		if ((colon = strchr(git_buf_cstr(&buf), ':')) == NULL)
			goto cleanup;

		git_buf_puts(msg, "On ");
		git_buf_put(msg, git_buf_cstr(&buf), colon - buf.ptr);
		git_buf_printf(msg, ": %s\n", user_message);
	}

	error = (git_buf_oom(msg) || git_buf_oom(&buf)) ? -1 : 0;

cleanup:
	git_buf_free(&buf);

	return error;
}

static int update_reflog(
	git_oid *w_commit_oid,
	git_repository *repo,
	const char *message)
{
	git_reference *stash;
	int error;

	if ((error = git_reference_ensure_log(repo, GIT_REFS_STASH_FILE)) < 0)
		return error;

	error = git_reference_create(&stash, repo, GIT_REFS_STASH_FILE, w_commit_oid, 1, message);

	git_reference_free(stash);

	return error;
}

static int is_dirty_cb(const char *path, unsigned int status, void *payload)
{
	GIT_UNUSED(path);
	GIT_UNUSED(status);
	GIT_UNUSED(payload);

	return GIT_PASSTHROUGH;
}

static int ensure_there_are_changes_to_stash(
	git_repository *repo,
	bool include_untracked_files,
	bool include_ignored_files)
{
	int error;
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;

	opts.show  = GIT_STATUS_SHOW_INDEX_AND_WORKDIR;
	opts.flags = GIT_STATUS_OPT_EXCLUDE_SUBMODULES;

	if (include_untracked_files)
		opts.flags |= GIT_STATUS_OPT_INCLUDE_UNTRACKED |
			GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;

	if (include_ignored_files)
		opts.flags |= GIT_STATUS_OPT_INCLUDE_IGNORED |
			GIT_STATUS_OPT_RECURSE_IGNORED_DIRS;

	error = git_status_foreach_ext(repo, &opts, is_dirty_cb, NULL);

	if (error == GIT_PASSTHROUGH)
		return 0;

	if (!error)
		return create_error(GIT_ENOTFOUND, "There is nothing to stash.");

	return error;
}

static int reset_index_and_workdir(
	git_repository *repo,
	git_commit *commit,
	bool remove_untracked,
	bool remove_ignored)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	if (remove_untracked)
		opts.checkout_strategy |= GIT_CHECKOUT_REMOVE_UNTRACKED;

	if (remove_ignored)
		opts.checkout_strategy |= GIT_CHECKOUT_REMOVE_IGNORED;

	return git_checkout_tree(repo, (git_object *)commit, &opts);
}

int git_stash_save(
	git_oid *out,
	git_repository *repo,
	const git_signature *stasher,
	const char *message,
	uint32_t flags)
{
	git_index *index = NULL;
	git_commit *b_commit = NULL, *i_commit = NULL, *u_commit = NULL;
	git_buf msg = GIT_BUF_INIT;
	int error;

	assert(out && repo && stasher);

	if ((error = git_repository__ensure_not_bare(repo, "stash save")) < 0)
		return error;

	if ((error = retrieve_base_commit_and_message(&b_commit, &msg, repo)) < 0)
		goto cleanup;

	if ((error = ensure_there_are_changes_to_stash(
		repo,
		(flags & GIT_STASH_INCLUDE_UNTRACKED) != 0,
		(flags & GIT_STASH_INCLUDE_IGNORED) != 0)) < 0)
		goto cleanup;

	if ((error = git_repository_index(&index, repo)) < 0)
		goto cleanup;

	if ((error = commit_index(
			&i_commit, index, stasher, git_buf_cstr(&msg), b_commit)) < 0)
		goto cleanup;

	if ((flags & (GIT_STASH_INCLUDE_UNTRACKED | GIT_STASH_INCLUDE_IGNORED)) &&
		(error = commit_untracked(
			&u_commit, index, stasher, git_buf_cstr(&msg),
			i_commit, flags)) < 0)
		goto cleanup;

	if ((error = prepare_worktree_commit_message(&msg, message)) < 0)
		goto cleanup;

	if ((error = commit_worktree(
			out, index, stasher, git_buf_cstr(&msg),
			i_commit, b_commit, u_commit)) < 0)
		goto cleanup;

	git_buf_rtrim(&msg);

	if ((error = update_reflog(out, repo, git_buf_cstr(&msg))) < 0)
		goto cleanup;

	if ((error = reset_index_and_workdir(
		repo,
		((flags & GIT_STASH_KEEP_INDEX) != 0) ? i_commit : b_commit,
		(flags & GIT_STASH_INCLUDE_UNTRACKED) != 0,
		(flags & GIT_STASH_INCLUDE_IGNORED) != 0)) < 0)
		goto cleanup;

cleanup:

	git_buf_free(&msg);
	git_commit_free(i_commit);
	git_commit_free(b_commit);
	git_commit_free(u_commit);
	git_index_free(index);

	return error;
}

static int retrieve_stash_commit(
	git_commit **commit,
	git_repository *repo,
	size_t index)
{
	git_reference *stash = NULL;
	git_reflog *reflog = NULL;
	int error;
	size_t max;
	const git_reflog_entry *entry;

	if ((error = git_reference_lookup(&stash, repo, GIT_REFS_STASH_FILE)) < 0)
		goto cleanup;

	if ((error = git_reflog_read(&reflog, repo, GIT_REFS_STASH_FILE)) < 0)
		goto cleanup;

	max = git_reflog_entrycount(reflog);
	if (!max || index > max - 1) {
		error = GIT_ENOTFOUND;
		giterr_set(GITERR_STASH, "No stashed state at position %" PRIuZ, index);
		goto cleanup;
	}

	entry = git_reflog_entry_byindex(reflog, index);
	if ((error = git_commit_lookup(commit, repo, git_reflog_entry_id_new(entry))) < 0)
		goto cleanup;

cleanup:
	git_reference_free(stash);
	git_reflog_free(reflog);
	return error;
}

static int retrieve_stash_trees(
	git_tree **out_stash_tree,
	git_tree **out_base_tree,
	git_tree **out_index_tree,
	git_tree **out_index_parent_tree,
	git_tree **out_untracked_tree,
	git_commit *stash_commit)
{
	git_tree *stash_tree = NULL;
	git_commit *base_commit = NULL;
	git_tree *base_tree = NULL;
	git_commit *index_commit = NULL;
	git_tree *index_tree = NULL;
	git_commit *index_parent_commit = NULL;
	git_tree *index_parent_tree = NULL;
	git_commit *untracked_commit = NULL;
	git_tree *untracked_tree = NULL;
	int error;

	if ((error = git_commit_tree(&stash_tree, stash_commit)) < 0)
		goto cleanup;

	if ((error = git_commit_parent(&base_commit, stash_commit, 0)) < 0)
		goto cleanup;
	if ((error = git_commit_tree(&base_tree, base_commit)) < 0)
		goto cleanup;

	if ((error = git_commit_parent(&index_commit, stash_commit, 1)) < 0)
		goto cleanup;
	if ((error = git_commit_tree(&index_tree, index_commit)) < 0)
		goto cleanup;

	if ((error = git_commit_parent(&index_parent_commit, index_commit, 0)) < 0)
		goto cleanup;
	if ((error = git_commit_tree(&index_parent_tree, index_parent_commit)) < 0)
		goto cleanup;

	if (git_commit_parentcount(stash_commit) == 3) {
		if ((error = git_commit_parent(&untracked_commit, stash_commit, 2)) < 0)
			goto cleanup;
		if ((error = git_commit_tree(&untracked_tree, untracked_commit)) < 0)
			goto cleanup;
	}

	*out_stash_tree = stash_tree;
	*out_base_tree = base_tree;
	*out_index_tree = index_tree;
	*out_index_parent_tree = index_parent_tree;
	*out_untracked_tree = untracked_tree;

cleanup:
	git_commit_free(untracked_commit);
	git_commit_free(index_parent_commit);
	git_commit_free(index_commit);
	git_commit_free(base_commit);
	if (error < 0) {
		git_tree_free(stash_tree);
		git_tree_free(base_tree);
		git_tree_free(index_tree);
		git_tree_free(index_parent_tree);
		git_tree_free(untracked_tree);
	}
	return error;
}

static int merge_indexes(
	git_index **out,
	git_repository *repo,
	git_tree *ancestor_tree,
	git_index *ours_index,
	git_index *theirs_index)
{
	git_iterator *ancestor = NULL, *ours = NULL, *theirs = NULL;
	git_iterator_options iter_opts = GIT_ITERATOR_OPTIONS_INIT;
	int error;

	iter_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

	if ((error = git_iterator_for_tree(&ancestor, ancestor_tree, &iter_opts)) < 0 ||
		(error = git_iterator_for_index(&ours, repo, ours_index, &iter_opts)) < 0 ||
		(error = git_iterator_for_index(&theirs, repo, theirs_index, &iter_opts)) < 0)
		goto done;

	error = git_merge__iterators(out, repo, ancestor, ours, theirs, NULL);

done:
	git_iterator_free(ancestor);
	git_iterator_free(ours);
	git_iterator_free(theirs);
	return error;
}

static int merge_index_and_tree(
	git_index **out,
	git_repository *repo,
	git_tree *ancestor_tree,
	git_index *ours_index,
	git_tree *theirs_tree)
{
	git_iterator *ancestor = NULL, *ours = NULL, *theirs = NULL;
	git_iterator_options iter_opts = GIT_ITERATOR_OPTIONS_INIT;
	int error;

	iter_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

	if ((error = git_iterator_for_tree(&ancestor, ancestor_tree, &iter_opts)) < 0 ||
		(error = git_iterator_for_index(&ours, repo, ours_index, &iter_opts)) < 0 ||
		(error = git_iterator_for_tree(&theirs, theirs_tree, &iter_opts)) < 0)
		goto done;

	error = git_merge__iterators(out, repo, ancestor, ours, theirs, NULL);

done:
	git_iterator_free(ancestor);
	git_iterator_free(ours);
	git_iterator_free(theirs);
	return error;
}

static void normalize_apply_options(
	git_stash_apply_options *opts,
	const git_stash_apply_options *given_apply_opts)
{
	if (given_apply_opts != NULL) {
		memcpy(opts, given_apply_opts, sizeof(git_stash_apply_options));
	} else {
		git_stash_apply_options default_apply_opts = GIT_STASH_APPLY_OPTIONS_INIT;
		memcpy(opts, &default_apply_opts, sizeof(git_stash_apply_options));
	}

	if ((opts->checkout_options.checkout_strategy & (GIT_CHECKOUT_SAFE | GIT_CHECKOUT_FORCE)) == 0)
		opts->checkout_options.checkout_strategy = GIT_CHECKOUT_SAFE;

	if (!opts->checkout_options.our_label)
		opts->checkout_options.our_label = "Updated upstream";

	if (!opts->checkout_options.their_label)
		opts->checkout_options.their_label = "Stashed changes";
}

int git_stash_apply_init_options(git_stash_apply_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_stash_apply_options, GIT_STASH_APPLY_OPTIONS_INIT);
	return 0;
}

#define NOTIFY_PROGRESS(opts, progress_type)				\
	do {								\
		if ((opts).progress_cb &&				\
		    (error = (opts).progress_cb((progress_type), (opts).progress_payload))) { \
			error = (error < 0) ? error : -1;		\
			goto cleanup;					\
		}							\
	} while(false);

static int ensure_clean_index(git_repository *repo, git_index *index)
{
	git_tree *head_tree = NULL;
	git_diff *index_diff = NULL;
	int error = 0;

	if ((error = git_repository_head_tree(&head_tree, repo)) < 0 ||
		(error = git_diff_tree_to_index(
			&index_diff, repo, head_tree, index, NULL)) < 0)
		goto done;

	if (git_diff_num_deltas(index_diff) > 0) {
		giterr_set(GITERR_STASH, "%" PRIuZ " uncommitted changes exist in the index",
			git_diff_num_deltas(index_diff));
		error = GIT_EUNCOMMITTED;
	}

done:
	git_diff_free(index_diff);
	git_tree_free(head_tree);
	return error;
}

static int stage_new_file(const git_index_entry **entries, void *data)
{
	git_index *index = data;

	if(entries[0] == NULL)
		return git_index_add(index, entries[1]);
	else
		return git_index_add(index, entries[0]);
}

static int stage_new_files(
	git_index **out,
	git_tree *parent_tree,
	git_tree *tree)
{
	git_iterator *iterators[2] = { NULL, NULL };
	git_iterator_options iterator_options = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index = NULL;
	int error;

	if ((error = git_index_new(&index)) < 0 ||
		(error = git_iterator_for_tree(
			&iterators[0], parent_tree, &iterator_options)) < 0 ||
		(error = git_iterator_for_tree(
			&iterators[1], tree, &iterator_options)) < 0)
		goto done;

	error = git_iterator_walk(iterators, 2, stage_new_file, index);

done:
	if (error < 0)
		git_index_free(index);
	else
		*out = index;

	git_iterator_free(iterators[0]);
	git_iterator_free(iterators[1]);

	return error;
}

int git_stash_apply(
	git_repository *repo,
	size_t index,
	const git_stash_apply_options *given_opts)
{
	git_stash_apply_options opts;
	unsigned int checkout_strategy;
	git_commit *stash_commit = NULL;
	git_tree *stash_tree = NULL;
	git_tree *stash_parent_tree = NULL;
	git_tree *index_tree = NULL;
	git_tree *index_parent_tree = NULL;
	git_tree *untracked_tree = NULL;
	git_index *stash_adds = NULL;
	git_index *repo_index = NULL;
	git_index *unstashed_index = NULL;
	git_index *modified_index = NULL;
	git_index *untracked_index = NULL;
	int error;

	GITERR_CHECK_VERSION(given_opts, GIT_STASH_APPLY_OPTIONS_VERSION, "git_stash_apply_options");

	normalize_apply_options(&opts, given_opts);
	checkout_strategy = opts.checkout_options.checkout_strategy;

	NOTIFY_PROGRESS(opts, GIT_STASH_APPLY_PROGRESS_LOADING_STASH);

	/* Retrieve commit corresponding to the given stash */
	if ((error = retrieve_stash_commit(&stash_commit, repo, index)) < 0)
		goto cleanup;

	/* Retrieve all trees in the stash */
	if ((error = retrieve_stash_trees(
			&stash_tree, &stash_parent_tree, &index_tree,
			&index_parent_tree, &untracked_tree, stash_commit)) < 0)
		goto cleanup;

	/* Load repo index */
	if ((error = git_repository_index(&repo_index, repo)) < 0)
		goto cleanup;

	NOTIFY_PROGRESS(opts, GIT_STASH_APPLY_PROGRESS_ANALYZE_INDEX);

	if ((error = ensure_clean_index(repo, repo_index)) < 0)
		goto cleanup;

	/* Restore index if required */
	if ((opts.flags & GIT_STASH_APPLY_REINSTATE_INDEX) &&
		git_oid_cmp(git_tree_id(stash_parent_tree), git_tree_id(index_tree))) {

		if ((error = merge_index_and_tree(
				&unstashed_index, repo, index_parent_tree, repo_index, index_tree)) < 0)
			goto cleanup;

		if (git_index_has_conflicts(unstashed_index)) {
			error = GIT_ECONFLICT;
			goto cleanup;
		}

	/* Otherwise, stage any new files in the stash tree.  (Note: their
	 * previously unstaged contents are staged, not the previously staged.)
	 */
	} else if ((opts.flags & GIT_STASH_APPLY_REINSTATE_INDEX) == 0) {
		if ((error = stage_new_files(
				&stash_adds, stash_parent_tree, stash_tree)) < 0 ||
			(error = merge_indexes(
				&unstashed_index, repo, stash_parent_tree, repo_index, stash_adds)) < 0)
			goto cleanup;
	}

	NOTIFY_PROGRESS(opts, GIT_STASH_APPLY_PROGRESS_ANALYZE_MODIFIED);

	/* Restore modified files in workdir */
	if ((error = merge_index_and_tree(
			&modified_index, repo, stash_parent_tree, repo_index, stash_tree)) < 0)
		goto cleanup;

	/* If applicable, restore untracked / ignored files in workdir */
	if (untracked_tree) {
		NOTIFY_PROGRESS(opts, GIT_STASH_APPLY_PROGRESS_ANALYZE_UNTRACKED);

		if ((error = merge_index_and_tree(&untracked_index, repo, NULL, repo_index, untracked_tree)) < 0)
			goto cleanup;
	}

	if (untracked_index) {
		opts.checkout_options.checkout_strategy |= GIT_CHECKOUT_DONT_UPDATE_INDEX;

		NOTIFY_PROGRESS(opts, GIT_STASH_APPLY_PROGRESS_CHECKOUT_UNTRACKED);

		if ((error = git_checkout_index(repo, untracked_index, &opts.checkout_options)) < 0)
			goto cleanup;

		opts.checkout_options.checkout_strategy = checkout_strategy;
	}


	/* If there are conflicts in the modified index, then we need to actually
	 * check that out as the repo's index.  Otherwise, we don't update the
	 * index.
	 */

	if (!git_index_has_conflicts(modified_index))
		opts.checkout_options.checkout_strategy |= GIT_CHECKOUT_DONT_UPDATE_INDEX;

	/* Check out the modified index using the existing repo index as baseline,
	 * so that existing modifications in the index can be rewritten even when
	 * checking out safely.
	 */
	opts.checkout_options.baseline_index = repo_index;

	NOTIFY_PROGRESS(opts, GIT_STASH_APPLY_PROGRESS_CHECKOUT_MODIFIED);

	if ((error = git_checkout_index(repo, modified_index, &opts.checkout_options)) < 0)
		goto cleanup;

	if (unstashed_index && !git_index_has_conflicts(modified_index)) {
		if ((error = git_index_read_index(repo_index, unstashed_index)) < 0)
			goto cleanup;
	}

	NOTIFY_PROGRESS(opts, GIT_STASH_APPLY_PROGRESS_DONE);

	error = git_index_write(repo_index);

cleanup:
	git_index_free(untracked_index);
	git_index_free(modified_index);
	git_index_free(unstashed_index);
	git_index_free(stash_adds);
	git_index_free(repo_index);
	git_tree_free(untracked_tree);
	git_tree_free(index_parent_tree);
	git_tree_free(index_tree);
	git_tree_free(stash_parent_tree);
	git_tree_free(stash_tree);
	git_commit_free(stash_commit);
	return error;
}

int git_stash_foreach(
	git_repository *repo,
	git_stash_cb callback,
	void *payload)
{
	git_reference *stash;
	git_reflog *reflog = NULL;
	int error;
	size_t i, max;
	const git_reflog_entry *entry;

	error = git_reference_lookup(&stash, repo, GIT_REFS_STASH_FILE);
	if (error == GIT_ENOTFOUND) {
		giterr_clear();
		return 0;
	}
	if (error < 0)
		goto cleanup;

	if ((error = git_reflog_read(&reflog, repo, GIT_REFS_STASH_FILE)) < 0)
		goto cleanup;

	max = git_reflog_entrycount(reflog);
	for (i = 0; i < max; i++) {
		entry = git_reflog_entry_byindex(reflog, i);

		error = callback(i,
			git_reflog_entry_message(entry),
			git_reflog_entry_id_new(entry),
			payload);

		if (error) {
			giterr_set_after_callback(error);
			break;
		}
	}

cleanup:
	git_reference_free(stash);
	git_reflog_free(reflog);
	return error;
}

int git_stash_drop(
	git_repository *repo,
	size_t index)
{
	git_transaction *tx;
	git_reference *stash = NULL;
	git_reflog *reflog = NULL;
	size_t max;
	int error;

	if ((error = git_transaction_new(&tx, repo)) < 0)
		return error;

	if ((error = git_transaction_lock_ref(tx, GIT_REFS_STASH_FILE)) < 0)
		goto cleanup;

	if ((error = git_reference_lookup(&stash, repo, GIT_REFS_STASH_FILE)) < 0)
		goto cleanup;

	if ((error = git_reflog_read(&reflog, repo, GIT_REFS_STASH_FILE)) < 0)
		goto cleanup;

	max = git_reflog_entrycount(reflog);

	if (!max || index > max - 1) {
		error = GIT_ENOTFOUND;
		giterr_set(GITERR_STASH, "No stashed state at position %" PRIuZ, index);
		goto cleanup;
	}

	if ((error = git_reflog_drop(reflog, index, true)) < 0)
		goto cleanup;

	if ((error = git_transaction_set_reflog(tx, GIT_REFS_STASH_FILE, reflog)) < 0)
		goto cleanup;

	if (max == 1) {
		if ((error = git_transaction_remove(tx, GIT_REFS_STASH_FILE)) < 0)
			goto cleanup;
	} else if (index == 0) {
		const git_reflog_entry *entry;

		entry = git_reflog_entry_byindex(reflog, 0);
		if ((error = git_transaction_set_target(tx, GIT_REFS_STASH_FILE, &entry->oid_cur, NULL, NULL)) < 0)
			goto cleanup;
	}

	error = git_transaction_commit(tx);

cleanup:
	git_reference_free(stash);
	git_transaction_free(tx);
	git_reflog_free(reflog);
	return error;
}

int git_stash_pop(
	git_repository *repo,
	size_t index,
	const git_stash_apply_options *options)
{
	int error;

	if ((error = git_stash_apply(repo, index, options)) < 0)
		return error;

	return git_stash_drop(repo, index);
}
