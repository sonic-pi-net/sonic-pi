/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <assert.h>

#include "git2/clone.h"
#include "git2/remote.h"
#include "git2/revparse.h"
#include "git2/branch.h"
#include "git2/config.h"
#include "git2/checkout.h"
#include "git2/commit.h"
#include "git2/tree.h"

#include "common.h"
#include "remote.h"
#include "fileops.h"
#include "refs.h"
#include "path.h"
#include "repository.h"

static int create_branch(
	git_reference **branch,
	git_repository *repo,
	const git_oid *target,
	const char *name,
	const git_signature *signature,
	const char *log_message)
{
	git_commit *head_obj = NULL;
	git_reference *branch_ref = NULL;
	int error;

	/* Find the target commit */
	if ((error = git_commit_lookup(&head_obj, repo, target)) < 0)
		return error;

	/* Create the new branch */
	error = git_branch_create(&branch_ref, repo, name, head_obj, 0, signature, log_message);

	git_commit_free(head_obj);

	if (!error)
		*branch = branch_ref;
	else
		git_reference_free(branch_ref);

	return error;
}

static int setup_tracking_config(
	git_repository *repo,
	const char *branch_name,
	const char *remote_name,
	const char *merge_target)
{
	git_config *cfg;
	git_buf remote_key = GIT_BUF_INIT, merge_key = GIT_BUF_INIT;
	int error = -1;

	if (git_repository_config__weakptr(&cfg, repo) < 0)
		return -1;

	if (git_buf_printf(&remote_key, "branch.%s.remote", branch_name) < 0)
		goto cleanup;

	if (git_buf_printf(&merge_key, "branch.%s.merge", branch_name) < 0)
		goto cleanup;

	if (git_config_set_string(cfg, git_buf_cstr(&remote_key), remote_name) < 0)
		goto cleanup;

	if (git_config_set_string(cfg, git_buf_cstr(&merge_key), merge_target) < 0)
		goto cleanup;

	error = 0;

cleanup:
	git_buf_free(&remote_key);
	git_buf_free(&merge_key);
	return error;
}

static int create_tracking_branch(
	git_reference **branch,
	git_repository *repo,
	const git_oid *target,
	const char *branch_name,
	const git_signature *signature,
	const char *log_message)
{
	int error;

	if ((error = create_branch(branch, repo, target, branch_name, signature, log_message)) < 0)
		return error;

	return setup_tracking_config(
		repo,
		branch_name,
		GIT_REMOTE_ORIGIN,
		git_reference_name(*branch));
}

struct head_info {
	git_repository *repo;
	git_oid remote_head_oid;
	git_buf branchname;
	const git_refspec *refspec;
	bool found;
};

static int reference_matches_remote_head(
	const char *reference_name,
	void *payload)
{
	struct head_info *head_info = (struct head_info *)payload;
	git_oid oid;
	int error;

	/* TODO: Should we guard against references
	 * which name doesn't start with refs/heads/ ?
	 */

	error = git_reference_name_to_id(&oid, head_info->repo, reference_name);
	if (error == GIT_ENOTFOUND) {
		/* If the reference doesn't exists, it obviously cannot match the
		 * expected oid. */
		giterr_clear();
		return 0;
	}

	if (!error && !git_oid__cmp(&head_info->remote_head_oid, &oid)) {
		/* Determine the local reference name from the remote tracking one */
		error = git_refspec_rtransform(
			&head_info->branchname, head_info->refspec, reference_name);

		if (!error &&
			git_buf_len(&head_info->branchname) > 0 &&
			!(error = git_buf_sets(
				&head_info->branchname,
				git_buf_cstr(&head_info->branchname) +
				strlen(GIT_REFS_HEADS_DIR))))
		{
			head_info->found = true;
			error = GIT_ITEROVER;
		}
	}

	return error;
}

static int update_head_to_new_branch(
	git_repository *repo,
	const git_oid *target,
	const char *name,
	const git_signature *signature,
	const char *reflog_message)
{
	git_reference *tracking_branch = NULL;
	int error = create_tracking_branch(&tracking_branch, repo, target, name,
			signature, reflog_message);

	if (!error)
		error = git_repository_set_head(
			repo, git_reference_name(tracking_branch),
			signature, reflog_message);

	git_reference_free(tracking_branch);

	/* if it already existed, then the user's refspec created it for us, ignore it' */
	if (error == GIT_EEXISTS)
		error = 0;

	return error;
}

static int update_head_to_remote(
		git_repository *repo,
		git_remote *remote,
		const git_signature *signature,
		const char *reflog_message)
{
	int error = 0;
	size_t refs_len;
	git_refspec dummy_spec;
	const git_remote_head *remote_head, **refs;
	struct head_info head_info;
	git_buf remote_master_name = GIT_BUF_INIT;

	if ((error = git_remote_ls(&refs, &refs_len, remote)) < 0)
		return error;

	/* Did we just clone an empty repository? */
	if (refs_len == 0)
		return setup_tracking_config(
			repo, "master", GIT_REMOTE_ORIGIN, GIT_REFS_HEADS_MASTER_FILE);

	/* Get the remote's HEAD. This is always the first ref in the list. */
	remote_head = refs[0];
	assert(remote_head);

	memset(&head_info, 0, sizeof(head_info));
	git_oid_cpy(&head_info.remote_head_oid, &remote_head->oid);
	head_info.repo = repo;
	head_info.refspec =
		git_remote__matching_refspec(remote, GIT_REFS_HEADS_MASTER_FILE);

	if (head_info.refspec == NULL) {
		memset(&dummy_spec, 0, sizeof(git_refspec));
		head_info.refspec = &dummy_spec;
	}

	/* Determine the remote tracking reference name from the local master */
	if ((error = git_refspec_transform(
		&remote_master_name,
		head_info.refspec,
		GIT_REFS_HEADS_MASTER_FILE)) < 0)
		return error;

	/* Check to see if the remote HEAD points to the remote master */
	error = reference_matches_remote_head(
		git_buf_cstr(&remote_master_name), &head_info);
	if (error < 0 && error != GIT_ITEROVER)
		goto cleanup;

	if (head_info.found) {
		error = update_head_to_new_branch(
			repo,
			&head_info.remote_head_oid,
			git_buf_cstr(&head_info.branchname),
			signature, reflog_message);
		goto cleanup;
	}

	/* Not master. Check all the other refs. */
	error = git_reference_foreach_name(
		repo, reference_matches_remote_head, &head_info);
	if (error < 0 && error != GIT_ITEROVER)
		goto cleanup;

	if (head_info.found) {
		error = update_head_to_new_branch(
			repo,
			&head_info.remote_head_oid,
			git_buf_cstr(&head_info.branchname),
			signature, reflog_message);
	} else {
		error = git_repository_set_head_detached(
			repo, &head_info.remote_head_oid, signature, reflog_message);
	}

cleanup:
	git_buf_free(&remote_master_name);
	git_buf_free(&head_info.branchname);
	return error;
}

static int update_head_to_branch(
		git_repository *repo,
		const char *remote_name,
		const char *branch,
		const git_signature *signature,
		const char *reflog_message)
{
	int retcode;
	git_buf remote_branch_name = GIT_BUF_INIT;
	git_reference* remote_ref = NULL;

	assert(remote_name && branch);

	if ((retcode = git_buf_printf(&remote_branch_name, GIT_REFS_REMOTES_DIR "%s/%s",
		remote_name, branch)) < 0 )
		goto cleanup;

	if ((retcode = git_reference_lookup(&remote_ref, repo, git_buf_cstr(&remote_branch_name))) < 0)
		goto cleanup;

	retcode = update_head_to_new_branch(repo, git_reference_target(remote_ref), branch,
			signature, reflog_message);

cleanup:
	git_reference_free(remote_ref);
	git_buf_free(&remote_branch_name);
	return retcode;
}

/*
 * submodules?
 */

static int create_and_configure_origin(
		git_remote **out,
		git_repository *repo,
		const char *url,
		const git_clone_options *options)
{
	int error;
	git_remote *origin = NULL;
	const char *name;

	name = options->remote_name ? options->remote_name : "origin";
	if ((error = git_remote_create(&origin, repo, name, url)) < 0)
		goto on_error;

	if (options->ignore_cert_errors)
		git_remote_check_cert(origin, 0);

	if ((error = git_remote_set_callbacks(origin, &options->remote_callbacks)) < 0)
		goto on_error;

	if ((error = git_remote_save(origin)) < 0)
		goto on_error;

	*out = origin;
	return 0;

on_error:
	git_remote_free(origin);
	return error;
}

static bool should_checkout(
	git_repository *repo,
	bool is_bare,
	const git_checkout_options *opts)
{
	if (is_bare)
		return false;

	if (!opts)
		return false;

	if (opts->checkout_strategy == GIT_CHECKOUT_NONE)
		return false;

	return !git_repository_head_unborn(repo);
}

int git_clone_into(git_repository *repo, git_remote *_remote, const git_checkout_options *co_opts, const char *branch, const git_signature *signature)
{
	int error = 0, old_fetchhead;
	git_buf reflog_message = GIT_BUF_INIT;
	git_remote *remote;
	const git_remote_callbacks *callbacks;

	assert(repo && _remote);

	if (!git_repository_is_empty(repo)) {
		giterr_set(GITERR_INVALID, "the repository is not empty");
		return -1;
	}

	if ((error = git_remote_dup(&remote, _remote)) < 0)
		return error;

	callbacks = git_remote_get_callbacks(_remote);
	if (!giterr__check_version(callbacks, 1, "git_remote_callbacks") &&
	    (error = git_remote_set_callbacks(remote, git_remote_get_callbacks(_remote))) < 0)
		goto cleanup;

	if ((error = git_remote_add_fetch(remote, "refs/tags/*:refs/tags/*")) < 0)
		goto cleanup;

	old_fetchhead = git_remote_update_fetchhead(remote);
	git_remote_set_update_fetchhead(remote, 0);
	git_buf_printf(&reflog_message, "clone: from %s", git_remote_url(remote));

	if ((error = git_remote_fetch(remote, signature, git_buf_cstr(&reflog_message))) != 0)
		goto cleanup;

	if (branch)
		error = update_head_to_branch(repo, git_remote_name(remote), branch,
				signature, git_buf_cstr(&reflog_message));
	/* Point HEAD to the same ref as the remote's head */
	else
		error = update_head_to_remote(repo, remote, signature, git_buf_cstr(&reflog_message));

	if (!error && should_checkout(repo, git_repository_is_bare(repo), co_opts))
		error = git_checkout_head(repo, co_opts);

cleanup:
	git_remote_set_update_fetchhead(remote, old_fetchhead);
	git_remote_free(remote);
	git_buf_free(&reflog_message);

	return error;
}

int git_clone(
	git_repository **out,
	const char *url,
	const char *local_path,
	const git_clone_options *_options)
{
	int error = 0;
	git_repository *repo = NULL;
	git_remote *origin;
	git_clone_options options = GIT_CLONE_OPTIONS_INIT;
	uint32_t rmdir_flags = GIT_RMDIR_REMOVE_FILES;

	assert(out && url && local_path);

	if (_options)
		memcpy(&options, _options, sizeof(git_clone_options));

	GITERR_CHECK_VERSION(&options, GIT_CLONE_OPTIONS_VERSION, "git_clone_options");

	/* Only clone to a new directory or an empty directory */
	if (git_path_exists(local_path) && !git_path_is_empty_dir(local_path)) {
		giterr_set(GITERR_INVALID,
			"'%s' exists and is not an empty directory", local_path);
		return GIT_EEXISTS;
	}

	/* Only remove the root directory on failure if we create it */
	if (git_path_exists(local_path))
		rmdir_flags |= GIT_RMDIR_SKIP_ROOT;

	if ((error = git_repository_init(&repo, local_path, options.bare)) < 0)
		return error;

	if (!(error = create_and_configure_origin(&origin, repo, url, &options))) {
		error = git_clone_into(
			repo, origin, &options.checkout_opts, options.checkout_branch, options.signature);

		git_remote_free(origin);
	}

	if (error != 0) {
		git_error_state last_error = {0};
		giterr_capture(&last_error, error);

		git_repository_free(repo);
		repo = NULL;

		(void)git_futils_rmdir_r(local_path, NULL, rmdir_flags);

		giterr_restore(&last_error);
	}

	*out = repo;
	return error;
}

int git_clone_init_options(git_clone_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_clone_options, GIT_CLONE_OPTIONS_INIT);
	return 0;
}
