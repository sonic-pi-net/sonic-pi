/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "branch.h"

#include "commit.h"
#include "tag.h"
#include "config.h"
#include "refspec.h"
#include "refs.h"
#include "remote.h"
#include "annotated_commit.h"
#include "worktree.h"

#include "git2/branch.h"

static int retrieve_branch_reference(
	git_reference **branch_reference_out,
	git_repository *repo,
	const char *branch_name,
	bool is_remote)
{
	git_reference *branch = NULL;
	int error = 0;
	char *prefix;
	git_buf ref_name = GIT_BUF_INIT;

	prefix = is_remote ? GIT_REFS_REMOTES_DIR : GIT_REFS_HEADS_DIR;

	if ((error = git_buf_joinpath(&ref_name, prefix, branch_name)) < 0)
		/* OOM */;
	else if ((error = git_reference_lookup(&branch, repo, ref_name.ptr)) < 0)
		git_error_set(
			GIT_ERROR_REFERENCE, "cannot locate %s branch '%s'",
			is_remote ? "remote-tracking" : "local", branch_name);

	*branch_reference_out = branch; /* will be NULL on error */

	git_buf_dispose(&ref_name);
	return error;
}

static int not_a_local_branch(const char *reference_name)
{
	git_error_set(
		GIT_ERROR_INVALID,
		"reference '%s' is not a local branch.", reference_name);
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
	int is_unmovable_head = 0;
	git_reference *branch = NULL;
	git_buf canonical_branch_name = GIT_BUF_INIT,
			  log_message = GIT_BUF_INIT;
	int error = -1;
	int bare = git_repository_is_bare(repository);

	GIT_ASSERT_ARG(branch_name);
	GIT_ASSERT_ARG(commit);
	GIT_ASSERT_ARG(ref_out);
	GIT_ASSERT_ARG(git_commit_owner(commit) == repository);

	if (!git__strcmp(branch_name, "HEAD")) {
		git_error_set(GIT_ERROR_REFERENCE, "'HEAD' is not a valid branch name");
		error = -1;
		goto cleanup;
	}

	if (force && !bare && git_branch_lookup(&branch, repository, branch_name, GIT_BRANCH_LOCAL) == 0) {
		error = git_branch_is_head(branch);
		git_reference_free(branch);
		branch = NULL;

		if (error < 0)
			goto cleanup;

		is_unmovable_head = error;
	}

	if (is_unmovable_head && force) {
		git_error_set(GIT_ERROR_REFERENCE, "cannot force update branch '%s' as it is "
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
	git_buf_dispose(&canonical_branch_name);
	git_buf_dispose(&log_message);
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
	return create_branch(ref_out,
		repository, branch_name, commit->commit, commit->description, force);
}

static int branch_is_checked_out(git_repository *worktree, void *payload)
{
	git_reference *branch = (git_reference *) payload;
	git_reference *head = NULL;
	int error;

	if (git_repository_is_bare(worktree))
		return 0;

	if ((error = git_reference_lookup(&head, worktree, GIT_HEAD_FILE)) < 0) {
		if (error == GIT_ENOTFOUND)
			error = 0;
		goto out;
	}

	if (git_reference_type(head) != GIT_REFERENCE_SYMBOLIC)
		goto out;

	error = !git__strcmp(head->target.symbolic, branch->name);

out:
	git_reference_free(head);
	return error;
}

int git_branch_is_checked_out(const git_reference *branch)
{
	GIT_ASSERT_ARG(branch);

	if (!git_reference_is_branch(branch))
		return 0;
	return git_repository_foreach_worktree(git_reference_owner(branch),
					       branch_is_checked_out, (void *)branch) == 1;
}

int git_branch_delete(git_reference *branch)
{
	int is_head;
	git_buf config_section = GIT_BUF_INIT;
	int error = -1;

	GIT_ASSERT_ARG(branch);

	if (!git_reference_is_branch(branch) && !git_reference_is_remote(branch)) {
		git_error_set(GIT_ERROR_INVALID, "reference '%s' is not a valid branch.",
			git_reference_name(branch));
		return GIT_ENOTFOUND;
	}

	if ((is_head = git_branch_is_head(branch)) < 0)
		return is_head;

	if (is_head) {
		git_error_set(GIT_ERROR_REFERENCE, "cannot delete branch '%s' as it is "
			"the current HEAD of the repository.", git_reference_name(branch));
		return -1;
	}

	if (git_reference_is_branch(branch) && git_branch_is_checked_out(branch)) {
		git_error_set(GIT_ERROR_REFERENCE, "Cannot delete branch '%s' as it is "
			"the current HEAD of a linked repository.", git_reference_name(branch));
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
	git_buf_dispose(&config_section);
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
	GIT_ERROR_CHECK_ALLOC(iter);

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

	GIT_ASSERT_ARG(branch);
	GIT_ASSERT_ARG(new_branch_name);

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
	git_buf_dispose(&new_reference_name);
	git_buf_dispose(&old_config_section);
	git_buf_dispose(&new_config_section);
	git_buf_dispose(&log_message);

	return error;
}

int git_branch_lookup(
	git_reference **ref_out,
	git_repository *repo,
	const char *branch_name,
	git_branch_t branch_type)
{
	int error = -1;

	GIT_ASSERT_ARG(ref_out);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(branch_name);

	switch (branch_type) {
	case GIT_BRANCH_LOCAL:
	case GIT_BRANCH_REMOTE:
		error = retrieve_branch_reference(ref_out, repo, branch_name, branch_type == GIT_BRANCH_REMOTE);
		break;
	case GIT_BRANCH_ALL:
		error = retrieve_branch_reference(ref_out, repo, branch_name, false);
		if (error == GIT_ENOTFOUND)
			error = retrieve_branch_reference(ref_out, repo, branch_name, true);
		break;
	default:
		GIT_ASSERT(false);
	}
	return error;
}

int git_branch_name(
	const char **out,
	const git_reference *ref)
{
	const char *branch_name;

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(ref);

	branch_name = ref->name;

	if (git_reference_is_branch(ref)) {
		branch_name += strlen(GIT_REFS_HEADS_DIR);
	} else if (git_reference_is_remote(ref)) {
		branch_name += strlen(GIT_REFS_REMOTES_DIR);
	} else {
		git_error_set(GIT_ERROR_INVALID,
				"reference '%s' is neither a local nor a remote branch.", ref->name);
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
	git_buf_dispose(&buf);
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

	GIT_ASSERT_ARG(out);
	GIT_ASSERT_ARG(refname);

	if ((error = git_buf_sanitize(out)) < 0)
		return error;

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
		git_error_set(GIT_ERROR_REFERENCE,
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
	git_buf_dispose(&remote_name);
	git_buf_dispose(&merge_name);
	git_buf_dispose(&buf);
	return error;
}

static int git_branch_upstream_with_format(git_buf *buf, git_repository *repo, const char *refname, const char *format, const char *format_name)
{
	int error;
	git_config *cfg;

	if (!git_reference__is_branch(refname))
		return not_a_local_branch(refname);

	if ((error = git_repository_config__weakptr(&cfg, repo)) < 0)
		return error;

	if ((error = git_buf_sanitize(buf)) < 0 ||
	    (error = retrieve_upstream_configuration(buf, cfg, refname, format)) < 0)
		return error;

	if (git_buf_len(buf) == 0) {
		git_error_set(GIT_ERROR_REFERENCE, "branch '%s' does not have an upstream %s", refname, format_name);
		error = GIT_ENOTFOUND;
		git_buf_clear(buf);
	}

	return error;
}

int git_branch_upstream_remote(git_buf *buf, git_repository *repo, const char *refname)
{
	return git_branch_upstream_with_format(buf, repo, refname, "branch.%s.remote", "remote");
}

int git_branch_upstream_merge(git_buf *buf, git_repository *repo, const char *refname)
{
	return git_branch_upstream_with_format(buf, repo, refname, "branch.%s.merge", "merge");
}

int git_branch_remote_name(git_buf *buf, git_repository *repo, const char *refname)
{
	git_strarray remote_list = {0};
	size_t i;
	git_remote *remote;
	const git_refspec *fetchspec;
	int error = 0;
	char *remote_name = NULL;

	GIT_ASSERT_ARG(buf);
	GIT_ASSERT_ARG(repo);
	GIT_ASSERT_ARG(refname);

	if ((error = git_buf_sanitize(buf)) < 0)
		return error;

	/* Verify that this is a remote branch */
	if (!git_reference__is_remote(refname)) {
		git_error_set(GIT_ERROR_INVALID, "reference '%s' is not a remote branch.",
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

				git_error_set(GIT_ERROR_REFERENCE,
					"reference '%s' is ambiguous", refname);
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
		git_error_set(GIT_ERROR_REFERENCE,
			"could not determine remote for '%s'", refname);
		error = GIT_ENOTFOUND;
	}

cleanup:
	if (error < 0)
		git_buf_dispose(buf);

	git_strarray_dispose(&remote_list);
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

	git_buf_dispose(&tracking_name);
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

	git_buf_dispose(&buf);
	return 0;

on_error:
	git_buf_dispose(&buf);
	return -1;
}

int git_branch_set_upstream(git_reference *branch, const char *branch_name)
{
	git_buf key = GIT_BUF_INIT, remote_name = GIT_BUF_INIT, merge_refspec = GIT_BUF_INIT;
	git_reference *upstream;
	git_repository *repo;
	git_remote *remote = NULL;
	git_config *config;
	const char *refname, *shortname;
	int local, error;
	const git_refspec *fetchspec;

	refname = git_reference_name(branch);
	if (!git_reference__is_branch(refname))
		return not_a_local_branch(refname);

	if (git_repository_config__weakptr(&config, git_reference_owner(branch)) < 0)
		return -1;

	shortname = refname + strlen(GIT_REFS_HEADS_DIR);

	/* We're unsetting, delegate and bail-out */
	if (branch_name == NULL)
		return unset_upstream(config, shortname);

	repo = git_reference_owner(branch);

	/* First we need to resolve name to a branch */
	if (git_branch_lookup(&upstream, repo, branch_name, GIT_BRANCH_LOCAL) == 0)
		local = 1;
	else if (git_branch_lookup(&upstream, repo, branch_name, GIT_BRANCH_REMOTE) == 0)
		local = 0;
	else {
		git_error_set(GIT_ERROR_REFERENCE,
			"cannot set upstream for branch '%s'", shortname);
		return GIT_ENOTFOUND;
	}

	/*
	 * If it's a local-tracking branch, its remote is "." (as "the local
	 * repository"), and the branch name is simply the refname.
	 * Otherwise we need to figure out what the remote-tracking branch's
	 * name on the remote is and use that.
	 */
	if (local)
		error = git_buf_puts(&remote_name, ".");
	else
		error = git_branch_remote_name(&remote_name, repo, git_reference_name(upstream));

	if (error < 0)
		goto on_error;

	/* Update the upsteam branch config with the new name */
	if (git_buf_printf(&key, "branch.%s.remote", shortname) < 0)
		goto on_error;

	if (git_config_set_string(config, git_buf_cstr(&key), git_buf_cstr(&remote_name)) < 0)
		goto on_error;

	if (local) {
		/* A local branch uses the upstream refname directly */
		if (git_buf_puts(&merge_refspec, git_reference_name(upstream)) < 0)
			goto on_error;
	} else {
		/* We transform the upstream branch name according to the remote's refspecs */
		if (git_remote_lookup(&remote, repo, git_buf_cstr(&remote_name)) < 0)
			goto on_error;

		fetchspec = git_remote__matching_dst_refspec(remote, git_reference_name(upstream));
		if (!fetchspec || git_refspec_rtransform(&merge_refspec, fetchspec, git_reference_name(upstream)) < 0)
			goto on_error;

		git_remote_free(remote);
		remote = NULL;
	}

	/* Update the merge branch config with the refspec */
	git_buf_clear(&key);
	if (git_buf_printf(&key, "branch.%s.merge", shortname) < 0)
		goto on_error;

	if (git_config_set_string(config, git_buf_cstr(&key), git_buf_cstr(&merge_refspec)) < 0)
		goto on_error;

	git_reference_free(upstream);
	git_buf_dispose(&key);
	git_buf_dispose(&remote_name);
	git_buf_dispose(&merge_refspec);

	return 0;

on_error:
	git_reference_free(upstream);
	git_buf_dispose(&key);
	git_buf_dispose(&remote_name);
	git_buf_dispose(&merge_refspec);
	git_remote_free(remote);

	return -1;
}

int git_branch_is_head(
		const git_reference *branch)
{
	git_reference *head;
	bool is_same = false;
	int error;

	GIT_ASSERT_ARG(branch);

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

int git_branch_name_is_valid(int *valid, const char *name)
{
	git_buf ref_name = GIT_BUF_INIT;
	int error = 0;

	GIT_ASSERT(valid);

	*valid = 0;

	/*
	 * Discourage branch name starting with dash,
	 * https://github.com/git/git/commit/6348624010888b
	 * and discourage HEAD as branch name,
	 * https://github.com/git/git/commit/a625b092cc5994
	 */
	if (!name || name[0] == '-' || !git__strcmp(name, "HEAD"))
		goto done;

	if ((error = git_buf_puts(&ref_name, GIT_REFS_HEADS_DIR)) < 0 ||
	    (error = git_buf_puts(&ref_name, name)) < 0)
		goto done;

	error = git_reference_name_is_valid(valid, ref_name.ptr);

done:
	git_buf_dispose(&ref_name);
	return error;
}
