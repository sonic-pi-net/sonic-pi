/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "annotated_commit.h"

#include "git2/commit.h"
#include "git2/refs.h"
#include "git2/repository.h"
#include "git2/annotated_commit.h"

static int annotated_commit_init(
	git_annotated_commit **out,
	git_repository *repo,
	const git_oid *id,
	const char *ref_name,
	const char *remote_url)
{
	git_annotated_commit *annotated_commit;
	int error = 0;

	assert(out && id);

	*out = NULL;

	annotated_commit = git__calloc(1, sizeof(git_annotated_commit));
	GITERR_CHECK_ALLOC(annotated_commit);

	if (ref_name) {
		annotated_commit->ref_name = git__strdup(ref_name);
		GITERR_CHECK_ALLOC(annotated_commit->ref_name);
	}

	if (remote_url) {
		annotated_commit->remote_url = git__strdup(remote_url);
		GITERR_CHECK_ALLOC(annotated_commit->remote_url);
	}

	git_oid_fmt(annotated_commit->id_str, id);
	annotated_commit->id_str[GIT_OID_HEXSZ] = '\0';

	if ((error = git_commit_lookup(&annotated_commit->commit, repo, id)) < 0) {
		git_annotated_commit_free(annotated_commit);
		return error;
	}

	*out = annotated_commit;
	return error;
}

int git_annotated_commit_from_ref(
	git_annotated_commit **out,
	git_repository *repo,
	const git_reference *ref)
{
	git_reference *resolved;
	int error = 0;

	assert(out && repo && ref);

	*out = NULL;

	if ((error = git_reference_resolve(&resolved, ref)) < 0)
		return error;
	
	error = annotated_commit_init(out, repo, git_reference_target(resolved),
		git_reference_name(ref), NULL);

	git_reference_free(resolved);
	return error;
}

int git_annotated_commit_lookup(
	git_annotated_commit **out,
	git_repository *repo,
	const git_oid *id)
{
	assert(out && repo && id);

	return annotated_commit_init(out, repo, id, NULL, NULL);
}

int git_annotated_commit_from_fetchhead(
	git_annotated_commit **out,
	git_repository *repo,
	const char *branch_name,
	const char *remote_url,
	const git_oid *id)
{
	assert(repo && id && branch_name && remote_url);

	return annotated_commit_init(out, repo, id, branch_name, remote_url);
}

const git_oid *git_annotated_commit_id(
	const git_annotated_commit *annotated_commit)
{
	assert(annotated_commit);
	return git_commit_id(annotated_commit->commit);
}

void git_annotated_commit_free(git_annotated_commit *annotated_commit)
{
	if (annotated_commit == NULL)
		return;

	if (annotated_commit->commit != NULL)
		git_commit_free(annotated_commit->commit);

	if (annotated_commit->ref_name != NULL)
		git__free(annotated_commit->ref_name);

	if (annotated_commit->remote_url != NULL)
		git__free(annotated_commit->remote_url);

	git__free(annotated_commit);
}
