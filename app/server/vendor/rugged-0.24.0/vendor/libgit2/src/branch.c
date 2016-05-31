/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "commit.h"
#include "tag.h"
#include "config.h"
#include "refspec.h"
#include "refs.h"
#include "remote.h"
#include "annotated_commit.h"

#include "git2/branch.h"

static int retrieve_branch_reference(
	git_reference **branch_reference_out,
	git_repository *repo,
	const char *branch_name,
	int is_remote)
{
	git_reference *branch = NULL;
	int error = 0;
	char *prefix;
	git_buf ref_name = GIT_BUF_INIT;

	prefix = is_remote ? GIT_REFS_REMOTES_DIR : GIT_REFS_HEADS_DIR;

	if ((error = git_buf_joinpath(&ref_name, prefix, branch_name)) < 0)
		/* OOM */;
	else if ((error = git_reference_lookup(&branch, repo, ref_name.ptr)) < 0)
		giterr_set(
			GITERR_REFERENCE, "Cannot locate %s branch '%s'",
			is_remote ? "remote-tracking" : "local", branch_name);

	*branch_reference_out = branch; /* will be NULL on error */

	git_buf_free(&ref_name);
	return error;
}

static int not_a_local_branch(const char *reference_name)
{
	giterr_set(
		GITERR_INVALID,
		"Reference '%s' is not a local branch.", reference_name);
	return -1;
}

static int create_branch(
	git_reference **ref_out,
	git_repository *repository,
	const char *branch_name,
	const git_commit *commit,
	const char *from,
	int force)
{
	int is_head = 0;
	git_reference *branch = NULL;
	git_buf canonical_branch_name = GIT_BUF_INIT,
			  log_message = GIT_BUF_INIT;
	int error = -1;

	assert(branch_name && commit && ref_out);
	assert(git_object_owner((const git_object *)commit) == repository);

	if (force && git_branch_lookup(&branch, repository, branch_name, GIT_BRANCH_LOCAL) == 0) {
		error = git_branch_is_head(branch);
		git_reference_free(branch);
		branch = NULL;

		if (error < 0)
			goto cleanup;

		is_head = error;
	}

	if (is_head && force) {
		giterr_set(GITERR_REFERENCE, "Cannot force update branch '%s' as it is "
			"the current HEAD of the repository.", branch_name);
		error = -1;
		goto cleanup;
	}

	if (git_buf_joinpath(&canonical_branch_name, GIT_REFS_HEADS_DIR, branch_name) < 0)
		goto cleanup;

	if (git_buf_printf(&log_message, "branch: Created from %s", from) < 0)
		goto cleanup;

	error = git_reference_create(&branch, repository,
		git_buf_cstr(&canonical_branch_name), git_commit_id(commit), force,
		git_buf_cstr(&log_message));

	if (!error)
		*ref_out = branch;

cleanup:
	git_buf_free(&canonical_branch_name);
	git_buf_free(&log_message);
	return error;
}

int git_branch_create(
	git_reference **ref_out,
	git_repository *repository,
	const char *branch_name,
	const git_commit *commit,
	int force)
{
	return create_branch(ref_out, repository, branch_name, commit, git_oid_tostr_s(git_commit_id(commit)), force);
}

int git_branch_create_from_annotated(
	git_reference **ref_out,
	git_repository *repository,
	const char *branch_name,
	const git_annotated_commit *commit,
	int force)
{
	return create_branch(ref_out, repository, branch_name, commit->commit, commit->ref_name, force);
}

int git_branch_delete(git_reference *branch)
{
	int is_head;
	git_buf config_section = GIT_BUF_INIT;
	int error = -1;

	assert(branch);

	if (!git_reference_is_branch(branch) && !git_reference_is_remote(branch)) {
		giterr_set(GITERR_INVALID, "Reference '%s' is not a valid branch.",
			git_reference_name(branch));
		return GIT_ENOTFOUND;
	}

	if ((is_head = git_branch_is_head(branch)) < 0)
		return is_head;

	if (is_head) {
		giterr_set(GITERR_REFERENCE, "Cannot delete branch '%s' as it is "
			"the current HEAD of the repository.", git_reference_name(branch));
		return -1;
	}

	if (git_buf_join(&config_section, '.', "branch",
			git_reference_name(branch) + strlen(GIT_REFS_HEADS_DIR)) < 0)
		goto on_error;

	if (git_config_rename_section(
		git_reference_owner(branch), git_buf_cstr(&config_section), NULL) < 0)
		goto on_error;

	error = git_reference_delete(branch);

on_error:
	git_buf_free(&config_section);
	return error;
}

typedef struct {
	git_reference_iterator *iter;
	unsigned int flags;
} branch_iter;

int git_branch_next(git_reference **out, git_branch_t *out_type, git_branch_iterator *_iter)
{
	branch_iter *iter = (branch_iter *) _iter;
	git_reference *ref;
	int error;

	while ((error = git_reference_next(&ref, iter->iter)) == 0) {
		if ((iter->flags & GIT_BRANCH_LOCAL) &&
		    !git__prefixcmp(ref->name, GIT_REFS_HEADS_DIR)) {
			*out = ref;
			*out_type = GIT_BRANCH_LOCAL;

			return 0;
		} else  if ((iter->flags & GIT_BRANCH_REMOTE) &&
			    !git__prefixcmp(ref->name, GIT_REFS_REMOTES_DIR)) {
			*out = ref;
			*out_type = GIT_BRANCH_REMOTE;

			return 0;
		} else {
			git_reference_free(ref);
		}
	}

	return error;
}

int git_branch_iterator_new(
	git_branch_iterator **out,
	git_repository *repo,
	git_branch_t list_flags)
{
	branch_iter *iter;

	iter = git__calloc(1, sizeof(branch_iter));
	GITERR_CHECK_ALLOC(iter);

	iter->flags = list_flags;

	if (git_reference_iterator_new(&iter->iter, repo) < 0) {
		git__free(iter);
		return -1;
	}

	*out = (git_branch_iterator *) iter;

	return 0;
}

void git_branch_iterator_free(git_branch_iterator *_iter)
{
	branch_iter *iter = (branch_iter *) _iter;

	if (iter == NULL)
		return;

	git_reference_iterator_free(iter->iter);
	git__free(iter);
}

int git_branch_move(
	git_reference **out,
	git_reference *branch,
	const char *new_branch_name,
	int force)
{
	git_buf new_reference_name = GIT_BUF_INIT,
	        old_config_section = GIT_BUF_INIT,
	        new_config_section = GIT_BUF_INIT,
	        log_message = GIT_BUF_INIT;
	int error;

	assert(branch && new_branch_name);

	if (!git_reference_is_branch(branch))
		return not_a_local_branch(git_reference_name(branch));

	if ((error = git_buf_joinpath(&new_reference_name, GIT_REFS_HEADS_DIR, new_branch_name)) < 0)
		goto done;

	if ((error = git_buf_printf(&log_message, "branch: renamed %s to %s",
				    git_reference_name(branch), git_buf_cstr(&new_reference_name))) < 0)
			goto done;

	/* first update ref then config so failure won't trash config */

	error = git_reference_rename(
		out, branch, git_buf_cstr(&new_reference_name), force,
		git_buf_cstr(&log_message));
	if (error < 0)
		goto done;

	git_buf_join(&old_config_section, '.', "branch",
		git_reference_name(branch) + strlen(GIT_REFS_HEADS_DIR));
	git_buf_join(&new_config_section, '.', "branch", new_branch_name);

	error = git_config_rename_section(
		git_reference_owner(branch),
		git_buf_cstr(&old_config_section),
		git_buf_cstr(&new_config_section));

done:
	git_buf_free(&new_reference_name);
	git_buf_free(&old_config_section);
	git_buf_free(&new_config_section);
	git_buf_free(&log_message);

	return error;
}

int git_branch_lookup(
	git_reference **ref_out,
	git_repository *repo,
	const char *branch_name,
	git_branch_t branch_type)
{
	assert(ref_out && repo && branch_name);

	return retrieve_branch_reference(ref_out, repo, branch_name, branch_type == GIT_BRANCH_REMOTE);
}

int git_branch_name(
	const char **out,
	const git_reference *ref)
{
	const char *branch_name;

	assert(out && ref);

	branch_name = ref->name;

	if (git_reference_is_branch(ref)) {
		branch_name += strlen(GIT_REFS_HEADS_DIR);
	} else if (git_reference_is_remote(ref)) {
		branch_name += strlen(GIT_REFS_REMOTES_DIR);
	} else {
		giterr_set(GITERR_INVALID,
				"Reference '%s' is neither a local nor a remote branch.", ref->name);
		return -1;
	}
	*out = branch_name;
	return 0;
}

static int retrieve_upstream_configuration(
	git_buf *out,
	const git_config *config,
	const char *canonical_branch_name,
	const char *format)
{
	git_buf buf = GIT_BUF_INIT;
	int error;

	if (git_buf_printf(&buf, format,
		canonical_branch_name + strlen(GIT_REFS_HEADS_DIR)) < 0)
			return -1;

	error = git_config_get_string_buf(out, config, git_buf_cstr(&buf));
	git_buf_free(&buf);
	return error;
}

int git_branch_upstream_name(
	git_buf *out,
	git_repository *repo,
	const char *refname)
{
	git_buf remote_name = GIT_BUF_INIT;
	git_buf merge_name = GIT_BUF_INIT;
	git_buf buf = GIT_BUF_INIT;
	int error = -1;
	git_remote *remote = NULL;
	const git_refspec *refspec;
	git_config *config;

	assert(out && refname);

	git_buf_sanitize(out);

	if (!git_reference__is_branch(refname))
		return not_a_local_branch(refname);

	if ((error = git_repository_config_snapshot(&config, repo)) < 0)
		return error;

	if ((error = retrieve_upstream_configuration(
		&remote_name, config, refname, "branch.%s.remote")) < 0)
			goto cleanup;

	if ((error = retrieve_upstream_configuration(
		&merge_name, config, refname, "branch.%s.merge")) < 0)
			goto cleanup;

	if (git_buf_len(&remote_name) == 0 || git_buf_len(&merge_name) == 0) {
		giterr_set(GITERR_REFERENCE,
			"branch '%s' does not have an upstream", refname);
		error = GIT_ENOTFOUND;
		goto cleanup;
	}

	if (strcmp(".", git_buf_cstr(&remote_name)) != 0) {
		if ((error = git_remote_lookup(&remote, repo, git_buf_cstr(&remote_name))) < 0)
			goto cleanup;

		refspec = git_remote__matching_refspec(remote, git_buf_cstr(&merge_name));
		if (!refspec) {
			error = GIT_ENOTFOUND;
			goto cleanup;
		}

		if (git_refspec_transform(&buf, refspec, git_buf_cstr(&merge_name)) < 0)
			goto cleanup;
	} else
		if (git_buf_set(&buf, git_buf_cstr(&merge_name), git_buf_len(&merge_name)) < 0)
			goto cleanup;

	error = git_buf_set(out, git_buf_cstr(&buf), git_buf_len(&buf));

cleanup:
	git_config_free(config);
	git_remote_free(remote);
	git_buf_free(&remote_name);
	git_buf_free(&merge_name);
	git_buf_free(&buf);
	return error;
}

int git_branch_upstream_remote(git_buf *buf, git_repository *repo, const char *refname)
{
	int error;
	git_config *cfg;

	if (!git_reference__is_branch(refname))
		return not_a_local_branch(refname);

	if ((error = git_repository_config__weakptr(&cfg, repo)) < 0)
		return error;

	git_buf_sanitize(buf);

	if ((error = retrieve_upstream_configuration(buf, cfg, refname, "branch.%s.remote")) < 0)
		return error;

	if (git_buf_len(buf) == 0) {
		giterr_set(GITERR_REFERENCE, "branch '%s' does not have an upstream remote", refname);
		error = GIT_ENOTFOUND;
		git_buf_clear(buf);
	}

	return error;
}

int git_branch_remote_name(git_buf *buf, git_repository *repo, const char *refname)
{
	git_strarray remote_list = {0};
	size_t i;
	git_remote *remote;
	const git_refspec *fetchspec;
	int error = 0;
	char *remote_name = NULL;

	assert(buf && repo && refname);

	git_buf_sanitize(buf);

	/* Verify that this is a remote branch */
	if (!git_reference__is_remote(refname)) {
		giterr_set(GITERR_INVALID, "Reference '%s' is not a remote branch.",
			refname);
		error = GIT_ERROR;
		goto cleanup;
	}

	/* Get the remotes */
	if ((error = git_remote_list(&remote_list, repo)) < 0)
		goto cleanup;

	/* Find matching remotes */
	for (i = 0; i < remote_list.count; i++) {
		if ((error = git_remote_lookup(&remote, repo, remote_list.strings[i])) < 0)
			continue;

		fetchspec = git_remote__matching_dst_refspec(remote, refname);
		if (fetchspec) {
			/* If we have not already set out yet, then set
			 * it to the matching remote name. Otherwise
			 * multiple remotes match this reference, and it
			 * is ambiguous. */
			if (!remote_name) {
				remote_name = remote_list.strings[i];
			} else {
				git_remote_free(remote);

				giterr_set(GITERR_REFERENCE,
					"Reference '%s' is ambiguous", refname);
				error = GIT_EAMBIGUOUS;
				goto cleanup;
			}
		}

		git_remote_free(remote);
	}

	if (remote_name) {
		git_buf_clear(buf);
		error = git_buf_puts(buf, remote_name);
	} else {
		giterr_set(GITERR_REFERENCE,
			"Could not determine remote for '%s'", refname);
		error = GIT_ENOTFOUND;
	}

cleanup:
	if (error < 0)
		git_buf_free(buf);

	git_strarray_free(&remote_list);
	return error;
}

int git_branch_upstream(
	git_reference **tracking_out,
	const git_reference *branch)
{
	int error;
	git_buf tracking_name = GIT_BUF_INIT;

	if ((error = git_branch_upstream_name(&tracking_name,
		git_reference_owner(branch), git_reference_name(branch))) < 0)
			return error;

	error = git_reference_lookup(
		tracking_out,
		git_reference_owner(branch),
		git_buf_cstr(&tracking_name));

	git_buf_free(&tracking_name);
	return error;
}

static int unset_upstream(git_config *config, const char *shortname)
{
	git_buf buf = GIT_BUF_INIT;

	if (git_buf_printf(&buf, "branch.%s.remote", shortname) < 0)
		return -1;

	if (git_config_delete_entry(config, git_buf_cstr(&buf)) < 0)
		goto on_error;

	git_buf_clear(&buf);
	if (git_buf_printf(&buf, "branch.%s.merge", shortname) < 0)
		goto on_error;

	if (git_config_delete_entry(config, git_buf_cstr(&buf)) < 0)
		goto on_error;

	git_buf_free(&buf);
	return 0;

on_error:
	git_buf_free(&buf);
	return -1;
}

int git_branch_set_upstream(git_reference *branch, const char *upstream_name)
{
	git_buf key = GIT_BUF_INIT, value = GIT_BUF_INIT;
	git_reference *upstream;
	git_repository *repo;
	git_remote *remote = NULL;
	git_config *config;
	const char *name, *shortname;
	int local, error;
	const git_refspec *fetchspec;

	name = git_reference_name(branch);
	if (!git_reference__is_branch(name))
		return not_a_local_branch(name);

	if (git_repository_config__weakptr(&config, git_reference_owner(branch)) < 0)
		return -1;

	shortname = name + strlen(GIT_REFS_HEADS_DIR);

	if (upstream_name == NULL)
		return unset_upstream(config, shortname);

	repo = git_reference_owner(branch);

	/* First we need to figure out whether it's a branch or remote-tracking */
	if (git_branch_lookup(&upstream, repo, upstream_name, GIT_BRANCH_LOCAL) == 0)
		local = 1;
	else if (git_branch_lookup(&upstream, repo, upstream_name, GIT_BRANCH_REMOTE) == 0)
		local = 0;
	else {
		giterr_set(GITERR_REFERENCE,
			"Cannot set upstream for branch '%s'", shortname);
		return GIT_ENOTFOUND;
	}

	/*
	 * If it's local, the remote is "." and the branch name is
	 * simply the refname. Otherwise we need to figure out what
	 * the remote-tracking branch's name on the remote is and use
	 * that.
	 */
	if (local)
		error = git_buf_puts(&value, ".");
	else
		error = git_branch_remote_name(&value, repo, git_reference_name(upstream));

	if (error < 0)
		goto on_error;

	if (git_buf_printf(&key, "branch.%s.remote", shortname) < 0)
		goto on_error;

	if (git_config_set_string(config, git_buf_cstr(&key), git_buf_cstr(&value)) < 0)
		goto on_error;

	if (local) {
		git_buf_clear(&value);
		if (git_buf_puts(&value, git_reference_name(upstream)) < 0)
			goto on_error;
	} else {
		/* Get the remoe-tracking branch's refname in its repo */
		if (git_remote_lookup(&remote, repo, git_buf_cstr(&value)) < 0)
			goto on_error;

		fetchspec = git_remote__matching_dst_refspec(remote, git_reference_name(upstream));
		git_buf_clear(&value);
		if (!fetchspec || git_refspec_rtransform(&value, fetchspec, git_reference_name(upstream)) < 0)
			goto on_error;

		git_remote_free(remote);
		remote = NULL;
	}

	git_buf_clear(&key);
	if (git_buf_printf(&key, "branch.%s.merge", shortname) < 0)
		goto on_error;

	if (git_config_set_string(config, git_buf_cstr(&key), git_buf_cstr(&value)) < 0)
		goto on_error;

	git_reference_free(upstream);
	git_buf_free(&key);
	git_buf_free(&value);

	return 0;

on_error:
	git_reference_free(upstream);
	git_buf_free(&key);
	git_buf_free(&value);
	git_remote_free(remote);

	return -1;
}

int git_branch_is_head(
		const git_reference *branch)
{
	git_reference *head;
	bool is_same = false;
	int error;

	assert(branch);

	if (!git_reference_is_branch(branch))
		return false;

	error = git_repository_head(&head, git_reference_owner(branch));

	if (error == GIT_EUNBORNBRANCH || error == GIT_ENOTFOUND)
		return false;

	if (error < 0)
		return -1;

	is_same = strcmp(
		git_reference_name(branch),
		git_reference_name(head)) == 0;

	git_reference_free(head);

	return is_same;
}
