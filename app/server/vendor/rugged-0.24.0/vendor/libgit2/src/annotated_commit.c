/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "annotated_commit.h"
#include "refs.h"
#include "cache.h"

#include "git2/commit.h"
#include "git2/refs.h"
#include "git2/repository.h"
#include "git2/annotated_commit.h"
#include "git2/revparse.h"
#include "git2/tree.h"
#include "git2/index.h"

static int annotated_commit_init(
	git_annotated_commit **out,
	git_repository *repo,
	const git_oid *id,
	const char *ref_name,
	const char *remote_url)
{
	git_annotated_commit *annotated_commit;
	git_commit *commit = NULL;
	int error = 0;

	assert(out && id);

	*out = NULL;

	if ((error = git_commit_lookup(&commit, repo, id)) < 0 ||
		(error = git_annotated_commit_from_commit(&annotated_commit,
			commit)) < 0)
		goto done;

	if (ref_name) {
		annotated_commit->ref_name = git__strdup(ref_name);
		GITERR_CHECK_ALLOC(annotated_commit->ref_name);
	}

	if (remote_url) {
		annotated_commit->remote_url = git__strdup(remote_url);
		GITERR_CHECK_ALLOC(annotated_commit->remote_url);
	}

	*out = annotated_commit;

done:
	git_commit_free(commit);
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

int git_annotated_commit_from_head(
	git_annotated_commit **out,
	git_repository *repo)
{
	git_reference *head;
	int error;

	assert(out && repo);

	*out = NULL;

    if ((error = git_reference_lookup(&head, repo, GIT_HEAD_FILE)) < 0)
		return -1;

	error = git_annotated_commit_from_ref(out, repo, head);

	git_reference_free(head);
	return error;
}

int git_annotated_commit_from_commit(
	git_annotated_commit **out,
	git_commit *commit)
{
	git_annotated_commit *annotated_commit;

	assert(out && commit);

	*out = NULL;

	annotated_commit = git__calloc(1, sizeof(git_annotated_commit));
	GITERR_CHECK_ALLOC(annotated_commit);

	annotated_commit->type = GIT_ANNOTATED_COMMIT_REAL;

	git_cached_obj_incref(commit);
	annotated_commit->commit = commit;

	git_oid_fmt(annotated_commit->id_str, git_commit_id(commit));
	annotated_commit->id_str[GIT_OID_HEXSZ] = '\0';

	*out = annotated_commit;
	return 0;
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

int git_annotated_commit_from_revspec(
	git_annotated_commit **out,
	git_repository *repo,
	const char *revspec)
{
	git_object *obj, *commit;
	int error;

	assert(out && repo && revspec);

	if ((error = git_revparse_single(&obj, repo, revspec)) < 0)
		return error;

	if ((error = git_object_peel(&commit, obj, GIT_OBJ_COMMIT))) {
		git_object_free(obj);
		return error;
	}

	error = annotated_commit_init(out, repo, git_object_id(commit), revspec, NULL);

	git_object_free(obj);
	git_object_free(commit);

	return error;
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

	switch (annotated_commit->type) {
		case GIT_ANNOTATED_COMMIT_REAL:
			git_commit_free(annotated_commit->commit);
			git_tree_free(annotated_commit->tree);
			git__free(annotated_commit->ref_name);
			git__free(annotated_commit->remote_url);
			break;
		case GIT_ANNOTATED_COMMIT_VIRTUAL:
			git_index_free(annotated_commit->index);
			git_array_clear(annotated_commit->parents);
			break;
		default:
			abort();
	}

	git__free(annotated_commit);
}
