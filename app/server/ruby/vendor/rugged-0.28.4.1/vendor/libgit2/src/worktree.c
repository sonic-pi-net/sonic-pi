/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "worktree.h"

#include "git2/branch.h"
#include "git2/commit.h"
#include "git2/worktree.h"

#include "repository.h"

static bool is_worktree_dir(const char *dir)
{
	git_buf buf = GIT_BUF_INIT;
	int error;

	if (git_buf_sets(&buf, dir) < 0)
		return -1;

	error = git_path_contains_file(&buf, "commondir")
		&& git_path_contains_file(&buf, "gitdir")
		&& git_path_contains_file(&buf, "HEAD");

	git_buf_dispose(&buf);
	return error;
}

int git_worktree_list(git_strarray *wts, git_repository *repo)
{
	git_vector worktrees = GIT_VECTOR_INIT;
	git_buf path = GIT_BUF_INIT;
	char *worktree;
	size_t i, len;
	int error;

	assert(wts && repo);

	wts->count = 0;
	wts->strings = NULL;

	if ((error = git_buf_printf(&path, "%s/worktrees/", repo->commondir)) < 0)
		goto exit;
	if (!git_path_exists(path.ptr) || git_path_is_empty_dir(path.ptr))
		goto exit;
	if ((error = git_path_dirload(&worktrees, path.ptr, path.size, 0x0)) < 0)
		goto exit;

	len = path.size;

	git_vector_foreach(&worktrees, i, worktree) {
		git_buf_truncate(&path, len);
		git_buf_puts(&path, worktree);

		if (!is_worktree_dir(path.ptr)) {
			git_vector_remove(&worktrees, i);
			git__free(worktree);
		}
	}

	wts->strings = (char **)git_vector_detach(&wts->count, NULL, &worktrees);

exit:
	git_buf_dispose(&path);

	return error;
}

char *git_worktree__read_link(const char *base, const char *file)
{
	git_buf path = GIT_BUF_INIT, buf = GIT_BUF_INIT;

	assert(base && file);

	if (git_buf_joinpath(&path, base, file) < 0)
		goto err;
	if (git_futils_readbuffer(&buf, path.ptr) < 0)
		goto err;
	git_buf_dispose(&path);

	git_buf_rtrim(&buf);

	if (!git_path_is_relative(buf.ptr))
		return git_buf_detach(&buf);

	if (git_buf_sets(&path, base) < 0)
		goto err;
	if (git_path_apply_relative(&path, buf.ptr) < 0)
		goto err;
	git_buf_dispose(&buf);

	return git_buf_detach(&path);

err:
	git_buf_dispose(&buf);
	git_buf_dispose(&path);

	return NULL;
}

static int write_wtfile(const char *base, const char *file, const git_buf *buf)
{
	git_buf path = GIT_BUF_INIT;
	int err;

	assert(base && file && buf);

	if ((err = git_buf_joinpath(&path, base, file)) < 0)
		goto out;

	if ((err = git_futils_writebuffer(buf, path.ptr, O_CREAT|O_EXCL|O_WRONLY, 0644)) < 0)
		goto out;

out:
	git_buf_dispose(&path);

	return err;
}

static int open_worktree_dir(git_worktree **out, const char *parent, const char *dir, const char *name)
{
	git_buf gitdir = GIT_BUF_INIT;
	git_worktree *wt = NULL;
	int error = 0;

	if (!is_worktree_dir(dir)) {
		error = -1;
		goto out;
	}

	if ((wt = git__calloc(1, sizeof(*wt))) == NULL) {
		error = -1;
		goto out;
	}

	if ((wt->name = git__strdup(name)) == NULL
	    || (wt->commondir_path = git_worktree__read_link(dir, "commondir")) == NULL
	    || (wt->gitlink_path = git_worktree__read_link(dir, "gitdir")) == NULL
	    || (parent && (wt->parent_path = git__strdup(parent)) == NULL)
	    || (wt->worktree_path = git_path_dirname(wt->gitlink_path)) == NULL) {
		error = -1;
		goto out;
	}

	if ((error = git_path_prettify_dir(&gitdir, dir, NULL)) < 0)
		goto out;
	wt->gitdir_path = git_buf_detach(&gitdir);

	wt->locked = !!git_worktree_is_locked(NULL, wt);

	*out = wt;

out:
	if (error)
		git_worktree_free(wt);
	git_buf_dispose(&gitdir);

	return error;
}

int git_worktree_lookup(git_worktree **out, git_repository *repo, const char *name)
{
	git_buf path = GIT_BUF_INIT;
	git_worktree *wt = NULL;
	int error;

	assert(repo && name);

	*out = NULL;

	if ((error = git_buf_printf(&path, "%s/worktrees/%s", repo->commondir, name)) < 0)
		goto out;

	if ((error = (open_worktree_dir(out, git_repository_workdir(repo), path.ptr, name))) < 0)
		goto out;

out:
	git_buf_dispose(&path);

	if (error)
		git_worktree_free(wt);

	return error;
}

int git_worktree_open_from_repository(git_worktree **out, git_repository *repo)
{
	git_buf parent = GIT_BUF_INIT;
	const char *gitdir, *commondir;
	char *name = NULL;
	int error = 0;

	if (!git_repository_is_worktree(repo)) {
		git_error_set(GIT_ERROR_WORKTREE, "cannot open worktree of a non-worktree repo");
		error = -1;
		goto out;
	}

	gitdir = git_repository_path(repo);
	commondir = git_repository_commondir(repo);

	if ((error = git_path_prettify_dir(&parent, "..", commondir)) < 0)
		goto out;

	/* The name is defined by the last component in '.git/worktree/%s' */
	name = git_path_basename(gitdir);

	if ((error = open_worktree_dir(out, parent.ptr, gitdir, name)) < 0)
		goto out;

out:
	git__free(name);
	git_buf_dispose(&parent);

	return error;
}

void git_worktree_free(git_worktree *wt)
{
	if (!wt)
		return;

	git__free(wt->commondir_path);
	git__free(wt->worktree_path);
	git__free(wt->gitlink_path);
	git__free(wt->gitdir_path);
	git__free(wt->parent_path);
	git__free(wt->name);
	git__free(wt);
}

int git_worktree_validate(const git_worktree *wt)
{
	assert(wt);

	if (!is_worktree_dir(wt->gitdir_path)) {
		git_error_set(GIT_ERROR_WORKTREE,
			"worktree gitdir ('%s') is not valid",
			wt->gitlink_path);
		return GIT_ERROR;
	}

	if (wt->parent_path && !git_path_exists(wt->parent_path)) {
		git_error_set(GIT_ERROR_WORKTREE,
			"worktree parent directory ('%s') does not exist ",
			wt->parent_path);
		return GIT_ERROR;
	}

	if (!git_path_exists(wt->commondir_path)) {
		git_error_set(GIT_ERROR_WORKTREE,
			"worktree common directory ('%s') does not exist ",
			wt->commondir_path);
		return GIT_ERROR;
	}

	return 0;
}

int git_worktree_add_options_init(git_worktree_add_options *opts,
	unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(opts, version,
		git_worktree_add_options, GIT_WORKTREE_ADD_OPTIONS_INIT);
	return 0;
}

int git_worktree_add_init_options(git_worktree_add_options *opts,
	unsigned int version)
{
	return git_worktree_add_options_init(opts, version);
}

int git_worktree_add(git_worktree **out, git_repository *repo,
	const char *name, const char *worktree,
	const git_worktree_add_options *opts)
{
	git_buf gitdir = GIT_BUF_INIT, wddir = GIT_BUF_INIT, buf = GIT_BUF_INIT;
	git_reference *ref = NULL, *head = NULL;
	git_commit *commit = NULL;
	git_repository *wt = NULL;
	git_checkout_options coopts = GIT_CHECKOUT_OPTIONS_INIT;
	git_worktree_add_options wtopts = GIT_WORKTREE_ADD_OPTIONS_INIT;
	int err;

	GIT_ERROR_CHECK_VERSION(
		opts, GIT_WORKTREE_ADD_OPTIONS_VERSION, "git_worktree_add_options");

	if (opts)
		memcpy(&wtopts, opts, sizeof(wtopts));

	assert(out && repo && name && worktree);

	*out = NULL;

	if (wtopts.ref) {
		if (!git_reference_is_branch(wtopts.ref)) {
			git_error_set(GIT_ERROR_WORKTREE, "reference is not a branch");
			err = -1;
			goto out;
		}

		if (git_branch_is_checked_out(wtopts.ref)) {
			git_error_set(GIT_ERROR_WORKTREE, "reference is already checked out");
			err = -1;
			goto out;
		}
	}

	/* Create gitdir directory ".git/worktrees/<name>" */
	if ((err = git_buf_joinpath(&gitdir, repo->commondir, "worktrees")) < 0)
		goto out;
	if (!git_path_exists(gitdir.ptr))
		if ((err = git_futils_mkdir(gitdir.ptr, 0755, GIT_MKDIR_EXCL)) < 0)
			goto out;
	if ((err = git_buf_joinpath(&gitdir, gitdir.ptr, name)) < 0)
		goto out;
	if ((err = git_futils_mkdir(gitdir.ptr, 0755, GIT_MKDIR_EXCL)) < 0)
		goto out;
	if ((err = git_path_prettify_dir(&gitdir, gitdir.ptr, NULL)) < 0)
		goto out;

	/* Create worktree work dir */
	if ((err = git_futils_mkdir(worktree, 0755, GIT_MKDIR_EXCL)) < 0)
		goto out;
	if ((err = git_path_prettify_dir(&wddir, worktree, NULL)) < 0)
		goto out;

	if (wtopts.lock) {
		int fd;

		if ((err = git_buf_joinpath(&buf, gitdir.ptr, "locked")) < 0)
			goto out;

		if ((fd = p_creat(buf.ptr, 0644)) < 0) {
			err = fd;
			goto out;
		}

		p_close(fd);
		git_buf_clear(&buf);
	}

	/* Create worktree .git file */
	if ((err = git_buf_printf(&buf, "gitdir: %s\n", gitdir.ptr)) < 0)
		goto out;
	if ((err = write_wtfile(wddir.ptr, ".git", &buf)) < 0)
		goto out;

	/* Create gitdir files */
	if ((err = git_path_prettify_dir(&buf, repo->commondir, NULL) < 0)
	    || (err = git_buf_putc(&buf, '\n')) < 0
	    || (err = write_wtfile(gitdir.ptr, "commondir", &buf)) < 0)
		goto out;
	if ((err = git_buf_joinpath(&buf, wddir.ptr, ".git")) < 0
	    || (err = git_buf_putc(&buf, '\n')) < 0
	    || (err = write_wtfile(gitdir.ptr, "gitdir", &buf)) < 0)
		goto out;

	/* Set up worktree reference */
	if (wtopts.ref) {
		if ((err = git_reference_dup(&ref, wtopts.ref)) < 0)
			goto out;
	} else {
		if ((err = git_repository_head(&head, repo)) < 0)
			goto out;
		if ((err = git_commit_lookup(&commit, repo, &head->target.oid)) < 0)
			goto out;
		if ((err = git_branch_create(&ref, repo, name, commit, false)) < 0)
			goto out;
	}

	/* Set worktree's HEAD */
	if ((err = git_repository_create_head(gitdir.ptr, git_reference_name(ref))) < 0)
		goto out;
	if ((err = git_repository_open(&wt, wddir.ptr)) < 0)
		goto out;

	/* Checkout worktree's HEAD */
	coopts.checkout_strategy = GIT_CHECKOUT_FORCE;
	if ((err = git_checkout_head(wt, &coopts)) < 0)
		goto out;

	/* Load result */
	if ((err = git_worktree_lookup(out, repo, name)) < 0)
		goto out;

out:
	git_buf_dispose(&gitdir);
	git_buf_dispose(&wddir);
	git_buf_dispose(&buf);
	git_reference_free(ref);
	git_reference_free(head);
	git_commit_free(commit);
	git_repository_free(wt);

	return err;
}

int git_worktree_lock(git_worktree *wt, const char *reason)
{
	git_buf buf = GIT_BUF_INIT, path = GIT_BUF_INIT;
	int err;

	assert(wt);

	if ((err = git_worktree_is_locked(NULL, wt)) < 0)
		goto out;

	if ((err = git_buf_joinpath(&path, wt->gitdir_path, "locked")) < 0)
		goto out;

	if (reason)
		git_buf_attach_notowned(&buf, reason, strlen(reason));

	if ((err = git_futils_writebuffer(&buf, path.ptr, O_CREAT|O_EXCL|O_WRONLY, 0644)) < 0)
		goto out;

	wt->locked = 1;

out:
	git_buf_dispose(&path);

	return err;
}

int git_worktree_unlock(git_worktree *wt)
{
	git_buf path = GIT_BUF_INIT;

	assert(wt);

	if (!git_worktree_is_locked(NULL, wt))
		return 1;

	if (git_buf_joinpath(&path, wt->gitdir_path, "locked") < 0)
		return -1;

	if (p_unlink(path.ptr) != 0) {
		git_buf_dispose(&path);
		return -1;
	}

	wt->locked = 0;

	git_buf_dispose(&path);

	return 0;
}

int git_worktree_is_locked(git_buf *reason, const git_worktree *wt)
{
	git_buf path = GIT_BUF_INIT;
	int ret;

	assert(wt);

	if (reason)
		git_buf_clear(reason);

	if ((ret = git_buf_joinpath(&path, wt->gitdir_path, "locked")) < 0)
		goto out;
	if ((ret = git_path_exists(path.ptr)) && reason)
		git_futils_readbuffer(reason, path.ptr);

out:
	git_buf_dispose(&path);

	return ret;
}

const char *git_worktree_name(const git_worktree *wt)
{
	assert(wt);
	return wt->name;
}

const char *git_worktree_path(const git_worktree *wt)
{
	assert(wt);
	return wt->worktree_path;
}

int git_worktree_prune_options_init(
	git_worktree_prune_options *opts,
	unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(opts, version,
		git_worktree_prune_options, GIT_WORKTREE_PRUNE_OPTIONS_INIT);
	return 0;
}

int git_worktree_pruneinit_options(git_worktree_prune_options *opts,
	unsigned int version)
{
	return git_worktree_prune_options_init(opts, version);
}

int git_worktree_is_prunable(git_worktree *wt,
	git_worktree_prune_options *opts)
{
	git_buf reason = GIT_BUF_INIT;
	git_worktree_prune_options popts = GIT_WORKTREE_PRUNE_OPTIONS_INIT;

	GIT_ERROR_CHECK_VERSION(
		opts, GIT_WORKTREE_PRUNE_OPTIONS_VERSION,
		"git_worktree_prune_options");

	if (opts)
		memcpy(&popts, opts, sizeof(popts));

	if ((popts.flags & GIT_WORKTREE_PRUNE_LOCKED) == 0 &&
		git_worktree_is_locked(&reason, wt))
	{
		if (!reason.size)
			git_buf_attach_notowned(&reason, "no reason given", 15);
		git_error_set(GIT_ERROR_WORKTREE, "not pruning locked working tree: '%s'", reason.ptr);
		git_buf_dispose(&reason);

		return 0;
	}

	if ((popts.flags & GIT_WORKTREE_PRUNE_VALID) == 0 &&
		git_worktree_validate(wt) == 0)
	{
		git_error_set(GIT_ERROR_WORKTREE, "not pruning valid working tree");
		return 0;
	}

	return 1;
}

int git_worktree_prune(git_worktree *wt,
	git_worktree_prune_options *opts)
{
	git_worktree_prune_options popts = GIT_WORKTREE_PRUNE_OPTIONS_INIT;
	git_buf path = GIT_BUF_INIT;
	char *wtpath;
	int err;

	GIT_ERROR_CHECK_VERSION(
		opts, GIT_WORKTREE_PRUNE_OPTIONS_VERSION,
		"git_worktree_prune_options");

	if (opts)
		memcpy(&popts, opts, sizeof(popts));

	if (!git_worktree_is_prunable(wt, &popts)) {
		err = -1;
		goto out;
	}

	/* Delete gitdir in parent repository */
	if ((err = git_buf_printf(&path, "%s/worktrees/%s", wt->commondir_path, wt->name)) < 0)
		goto out;
	if (!git_path_exists(path.ptr))
	{
		git_error_set(GIT_ERROR_WORKTREE, "worktree gitdir '%s' does not exist", path.ptr);
		err = -1;
		goto out;
	}
	if ((err = git_futils_rmdir_r(path.ptr, NULL, GIT_RMDIR_REMOVE_FILES)) < 0)
		goto out;

	/* Skip deletion of the actual working tree if it does
	 * not exist or deletion was not requested */
	if ((popts.flags & GIT_WORKTREE_PRUNE_WORKING_TREE) == 0 ||
		!git_path_exists(wt->gitlink_path))
	{
		goto out;
	}

	if ((wtpath = git_path_dirname(wt->gitlink_path)) == NULL)
		goto out;
	git_buf_attach(&path, wtpath, 0);
	if (!git_path_exists(path.ptr))
	{
		git_error_set(GIT_ERROR_WORKTREE, "working tree '%s' does not exist", path.ptr);
		err = -1;
		goto out;
	}
	if ((err = git_futils_rmdir_r(path.ptr, NULL, GIT_RMDIR_REMOVE_FILES)) < 0)
		goto out;

out:
	git_buf_dispose(&path);

	return err;
}
