/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "buffer.h"
#include "repository.h"
#include "posix.h"
#include "filebuf.h"
#include "merge.h"
#include "array.h"
#include "config.h"
#include "annotated_commit.h"
#include "index.h"

#include <git2/types.h>
#include <git2/annotated_commit.h>
#include <git2/rebase.h>
#include <git2/commit.h>
#include <git2/reset.h>
#include <git2/revwalk.h>
#include <git2/notes.h>

#define REBASE_APPLY_DIR    "rebase-apply"
#define REBASE_MERGE_DIR    "rebase-merge"

#define HEAD_NAME_FILE      "head-name"
#define ORIG_HEAD_FILE      "orig-head"
#define HEAD_FILE           "head"
#define ONTO_FILE           "onto"
#define ONTO_NAME_FILE      "onto_name"
#define QUIET_FILE          "quiet"

#define MSGNUM_FILE         "msgnum"
#define END_FILE            "end"
#define CMT_FILE_FMT        "cmt.%" PRIuZ
#define CURRENT_FILE        "current"
#define REWRITTEN_FILE      "rewritten"

#define ORIG_DETACHED_HEAD  "detached HEAD"

#define NOTES_DEFAULT_REF   NULL

#define REBASE_DIR_MODE     0777
#define REBASE_FILE_MODE    0666

typedef enum {
	GIT_REBASE_TYPE_NONE = 0,
	GIT_REBASE_TYPE_APPLY = 1,
	GIT_REBASE_TYPE_MERGE = 2,
	GIT_REBASE_TYPE_INTERACTIVE = 3,
} git_rebase_type_t;

struct git_rebase {
	git_repository *repo;

	git_rebase_options options;

	git_rebase_type_t type;
	char *state_path;

	int head_detached : 1,
		inmemory : 1,
		quiet : 1,
		started : 1;

	git_array_t(git_rebase_operation) operations;
	size_t current;

	/* Used by in-memory rebase */
	git_index *index;
	git_commit *last_commit;

	/* Used by regular (not in-memory) merge-style rebase */
	git_oid orig_head_id;
	char *orig_head_name;

	git_oid onto_id;
	char *onto_name;
};

#define GIT_REBASE_STATE_INIT {0}

static int rebase_state_type(
	git_rebase_type_t *type_out,
	char **path_out,
	git_repository *repo)
{
	git_buf path = GIT_BUF_INIT;
	git_rebase_type_t type = GIT_REBASE_TYPE_NONE;

	if (git_buf_joinpath(&path, repo->path_repository, REBASE_APPLY_DIR) < 0)
		return -1;

	if (git_path_isdir(git_buf_cstr(&path))) {
		type = GIT_REBASE_TYPE_APPLY;
		goto done;
	}

	git_buf_clear(&path);
	if (git_buf_joinpath(&path, repo->path_repository, REBASE_MERGE_DIR) < 0)
		return -1;

	if (git_path_isdir(git_buf_cstr(&path))) {
		type = GIT_REBASE_TYPE_MERGE;
		goto done;
	}

done:
	*type_out = type;

	if (type != GIT_REBASE_TYPE_NONE && path_out)
		*path_out = git_buf_detach(&path);

	git_buf_free(&path);

	return 0;
}

GIT_INLINE(int) rebase_readfile(
	git_buf *out,
	git_buf *state_path,
	const char *filename)
{
	size_t state_path_len = state_path->size;
	int error;

	git_buf_clear(out);

	if ((error = git_buf_joinpath(state_path, state_path->ptr, filename)) < 0 ||
		(error = git_futils_readbuffer(out, state_path->ptr)) < 0)
		goto done;

	git_buf_rtrim(out);

done:
	git_buf_truncate(state_path, state_path_len);
	return error;
}

GIT_INLINE(int) rebase_readint(
	size_t *out, git_buf *asc_out, git_buf *state_path, const char *filename)
{
	int32_t num;
	const char *eol;
	int error = 0;

	if ((error = rebase_readfile(asc_out, state_path, filename)) < 0)
		return error;

	if (git__strtol32(&num, asc_out->ptr, &eol, 10) < 0 || num < 0 || *eol) {
		giterr_set(GITERR_REBASE, "The file '%s' contains an invalid numeric value", filename);
		return -1;
	}

	*out = (size_t) num;

	return 0;
}

GIT_INLINE(int) rebase_readoid(
	git_oid *out, git_buf *str_out, git_buf *state_path, const char *filename)
{
	int error;

	if ((error = rebase_readfile(str_out, state_path, filename)) < 0)
		return error;

	if (str_out->size != GIT_OID_HEXSZ || git_oid_fromstr(out, str_out->ptr) < 0) {
		giterr_set(GITERR_REBASE, "The file '%s' contains an invalid object ID", filename);
		return -1;
	}

	return 0;
}

static git_rebase_operation *rebase_operation_alloc(
	git_rebase *rebase,
	git_rebase_operation_t type,
	git_oid *id,
	const char *exec)
{
	git_rebase_operation *operation;

	assert((type == GIT_REBASE_OPERATION_EXEC) == !id);
	assert((type == GIT_REBASE_OPERATION_EXEC) == !!exec);

	if ((operation = git_array_alloc(rebase->operations)) == NULL)
		return NULL;

	operation->type = type;
	git_oid_cpy((git_oid *)&operation->id, id);
	operation->exec = exec;

	return operation;
}

static int rebase_open_merge(git_rebase *rebase)
{
	git_buf state_path = GIT_BUF_INIT, buf = GIT_BUF_INIT, cmt = GIT_BUF_INIT;
	git_oid id;
	git_rebase_operation *operation;
	size_t i, msgnum = 0, end;
	int error;

	if ((error = git_buf_puts(&state_path, rebase->state_path)) < 0)
		goto done;

	/* Read 'msgnum' if it exists (otherwise, let msgnum = 0) */
	if ((error = rebase_readint(&msgnum, &buf, &state_path, MSGNUM_FILE)) < 0 &&
		error != GIT_ENOTFOUND)
		goto done;

	if (msgnum) {
		rebase->started = 1;
		rebase->current = msgnum - 1;
	}

	/* Read 'end' */
	if ((error = rebase_readint(&end, &buf, &state_path, END_FILE)) < 0)
		goto done;

	/* Read 'current' if it exists */
	if ((error = rebase_readoid(&id, &buf, &state_path, CURRENT_FILE)) < 0 &&
		error != GIT_ENOTFOUND)
		goto done;

	/* Read cmt.* */
	git_array_init_to_size(rebase->operations, end);
	GITERR_CHECK_ARRAY(rebase->operations);

	for (i = 0; i < end; i++) {
		git_buf_clear(&cmt);

		if ((error = git_buf_printf(&cmt, "cmt.%" PRIuZ, (i+1))) < 0 ||
			(error = rebase_readoid(&id, &buf, &state_path, cmt.ptr)) < 0)
			goto done;

		operation = rebase_operation_alloc(rebase, GIT_REBASE_OPERATION_PICK, &id, NULL);
		GITERR_CHECK_ALLOC(operation);
	}

	/* Read 'onto_name' */
	if ((error = rebase_readfile(&buf, &state_path, ONTO_NAME_FILE)) < 0)
		goto done;

	rebase->onto_name = git_buf_detach(&buf);

done:
	git_buf_free(&cmt);
	git_buf_free(&state_path);
	git_buf_free(&buf);

	return error;
}

static int rebase_alloc(git_rebase **out, const git_rebase_options *rebase_opts)
{
	git_rebase *rebase = git__calloc(1, sizeof(git_rebase));
	GITERR_CHECK_ALLOC(rebase);

	*out = NULL;

	if (rebase_opts)
		memcpy(&rebase->options, rebase_opts, sizeof(git_rebase_options));
	else
		git_rebase_init_options(&rebase->options, GIT_REBASE_OPTIONS_VERSION);

	if (rebase_opts && rebase_opts->rewrite_notes_ref) {
		rebase->options.rewrite_notes_ref = git__strdup(rebase_opts->rewrite_notes_ref);
		GITERR_CHECK_ALLOC(rebase->options.rewrite_notes_ref);
	}

	if ((rebase->options.checkout_options.checkout_strategy & (GIT_CHECKOUT_SAFE | GIT_CHECKOUT_FORCE)) == 0)
		rebase->options.checkout_options.checkout_strategy = GIT_CHECKOUT_SAFE;

	*out = rebase;

	return 0;
}

static int rebase_check_versions(const git_rebase_options *given_opts)
{
	GITERR_CHECK_VERSION(given_opts, GIT_REBASE_OPTIONS_VERSION, "git_rebase_options");

	if (given_opts)
		GITERR_CHECK_VERSION(&given_opts->checkout_options, GIT_CHECKOUT_OPTIONS_VERSION, "git_checkout_options");

	return 0;
}

int git_rebase_open(
	git_rebase **out,
	git_repository *repo,
	const git_rebase_options *given_opts)
{
	git_rebase *rebase;
	git_buf path = GIT_BUF_INIT, orig_head_name = GIT_BUF_INIT,
		orig_head_id = GIT_BUF_INIT, onto_id = GIT_BUF_INIT;
	int state_path_len, error;

	assert(repo);

	if ((error = rebase_check_versions(given_opts)) < 0)
		return error;

	if (rebase_alloc(&rebase, given_opts) < 0)
		return -1;

	rebase->repo = repo;

	if ((error = rebase_state_type(&rebase->type, &rebase->state_path, repo)) < 0)
		goto done;

	if (rebase->type == GIT_REBASE_TYPE_NONE) {
		giterr_set(GITERR_REBASE, "There is no rebase in progress");
		error = GIT_ENOTFOUND;
		goto done;
	}

	if ((error = git_buf_puts(&path, rebase->state_path)) < 0)
		goto done;

	state_path_len = git_buf_len(&path);

	if ((error = git_buf_joinpath(&path, path.ptr, HEAD_NAME_FILE)) < 0 ||
		(error = git_futils_readbuffer(&orig_head_name, path.ptr)) < 0)
		goto done;

	git_buf_rtrim(&orig_head_name);

	if (strcmp(ORIG_DETACHED_HEAD, orig_head_name.ptr) == 0)
		rebase->head_detached = 1;

	git_buf_truncate(&path, state_path_len);

	if ((error = git_buf_joinpath(&path, path.ptr, ORIG_HEAD_FILE)) < 0)
		goto done;

	if (!git_path_isfile(path.ptr)) {
		/* Previous versions of git.git used 'head' here; support that. */
		git_buf_truncate(&path, state_path_len);

		if ((error = git_buf_joinpath(&path, path.ptr, HEAD_FILE)) < 0)
			goto done;
	}

	if ((error = git_futils_readbuffer(&orig_head_id, path.ptr)) < 0)
		goto done;

	git_buf_rtrim(&orig_head_id);

	if ((error = git_oid_fromstr(&rebase->orig_head_id, orig_head_id.ptr)) < 0)
		goto done;

	git_buf_truncate(&path, state_path_len);

	if ((error = git_buf_joinpath(&path, path.ptr, ONTO_FILE)) < 0 ||
		(error = git_futils_readbuffer(&onto_id, path.ptr)) < 0)
		goto done;

	git_buf_rtrim(&onto_id);

	if ((error = git_oid_fromstr(&rebase->onto_id, onto_id.ptr)) < 0)
		goto done;

	if (!rebase->head_detached)
		rebase->orig_head_name = git_buf_detach(&orig_head_name);

	switch (rebase->type) {
	case GIT_REBASE_TYPE_INTERACTIVE:
		giterr_set(GITERR_REBASE, "Interactive rebase is not supported");
		error = -1;
		break;
	case GIT_REBASE_TYPE_MERGE:
		error = rebase_open_merge(rebase);
		break;
	case GIT_REBASE_TYPE_APPLY:
		giterr_set(GITERR_REBASE, "Patch application rebase is not supported");
		error = -1;
		break;
	default:
		abort();
	}

done:
	if (error == 0)
		*out = rebase;
	else
		git_rebase_free(rebase);

	git_buf_free(&path);
	git_buf_free(&orig_head_name);
	git_buf_free(&orig_head_id);
	git_buf_free(&onto_id);
	return error;
}

static int rebase_cleanup(git_rebase *rebase)
{
	if (!rebase || rebase->inmemory)
		return 0;

	return git_path_isdir(rebase->state_path) ?
		git_futils_rmdir_r(rebase->state_path, NULL, GIT_RMDIR_REMOVE_FILES) :
		0;
}

static int rebase_setupfile(git_rebase *rebase, const char *filename, int flags, const char *fmt, ...)
{
	git_buf path = GIT_BUF_INIT,
		contents = GIT_BUF_INIT;
	va_list ap;
	int error;

	va_start(ap, fmt);
	git_buf_vprintf(&contents, fmt, ap);
	va_end(ap);

	if ((error = git_buf_joinpath(&path, rebase->state_path, filename)) == 0)
		error = git_futils_writebuffer(&contents, path.ptr, flags, REBASE_FILE_MODE);

	git_buf_free(&path);
	git_buf_free(&contents);

	return error;
}

static const char *rebase_onto_name(const git_annotated_commit *onto)
{
	if (onto->ref_name && git__strncmp(onto->ref_name, "refs/heads/", 11) == 0)
		return onto->ref_name + 11;
	else if (onto->ref_name)
		return onto->ref_name;
	else
		return onto->id_str;
}

static int rebase_setupfiles_merge(git_rebase *rebase)
{
	git_buf commit_filename = GIT_BUF_INIT;
	char id_str[GIT_OID_HEXSZ];
	git_rebase_operation *operation;
	size_t i;
	int error = 0;

	if ((error = rebase_setupfile(rebase, END_FILE, -1, "%" PRIuZ "\n", git_array_size(rebase->operations))) < 0 ||
		(error = rebase_setupfile(rebase, ONTO_NAME_FILE, -1, "%s\n", rebase->onto_name)) < 0)
		goto done;

	for (i = 0; i < git_array_size(rebase->operations); i++) {
		operation = git_array_get(rebase->operations, i);

		git_buf_clear(&commit_filename);
		git_buf_printf(&commit_filename, CMT_FILE_FMT, i+1);

		git_oid_fmt(id_str, &operation->id);

		if ((error = rebase_setupfile(rebase, commit_filename.ptr, -1,
				"%.*s\n", GIT_OID_HEXSZ, id_str)) < 0)
			goto done;
	}

done:
	git_buf_free(&commit_filename);
	return error;
}

static int rebase_setupfiles(git_rebase *rebase)
{
	char onto[GIT_OID_HEXSZ], orig_head[GIT_OID_HEXSZ];
	const char *orig_head_name;

	git_oid_fmt(onto, &rebase->onto_id);
	git_oid_fmt(orig_head, &rebase->orig_head_id);

	if (p_mkdir(rebase->state_path, REBASE_DIR_MODE) < 0) {
		giterr_set(GITERR_OS, "Failed to create rebase directory '%s'", rebase->state_path);
		return -1;
	}

	orig_head_name = rebase->head_detached ? ORIG_DETACHED_HEAD :
		rebase->orig_head_name;

	if (git_repository__set_orig_head(rebase->repo, &rebase->orig_head_id) < 0 ||
		rebase_setupfile(rebase, HEAD_NAME_FILE, -1, "%s\n", orig_head_name) < 0 ||
		rebase_setupfile(rebase, ONTO_FILE, -1, "%.*s\n", GIT_OID_HEXSZ, onto) < 0 ||
		rebase_setupfile(rebase, ORIG_HEAD_FILE, -1, "%.*s\n", GIT_OID_HEXSZ, orig_head) < 0 ||
		rebase_setupfile(rebase, QUIET_FILE, -1, rebase->quiet ? "t\n" : "\n") < 0)
		return -1;

	return rebase_setupfiles_merge(rebase);
}

int git_rebase_init_options(git_rebase_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_rebase_options, GIT_REBASE_OPTIONS_INIT);
	return 0;
}

static int rebase_ensure_not_in_progress(git_repository *repo)
{
	int error;
	git_rebase_type_t type;

	if ((error = rebase_state_type(&type, NULL, repo)) < 0)
		return error;

	if (type != GIT_REBASE_TYPE_NONE) {
		giterr_set(GITERR_REBASE, "There is an existing rebase in progress");
		return -1;
	}

	return 0;
}

static int rebase_ensure_not_dirty(
	git_repository *repo,
	bool check_index,
	bool check_workdir,
	int fail_with)
{
	git_tree *head = NULL;
	git_index *index = NULL;
	git_diff *diff = NULL;
	int error = 0;

	if (check_index) {
		if ((error = git_repository_head_tree(&head, repo)) < 0 ||
			(error = git_repository_index(&index, repo)) < 0 ||
			(error = git_diff_tree_to_index(&diff, repo, head, index, NULL)) < 0)
			goto done;

		if (git_diff_num_deltas(diff) > 0) {
			giterr_set(GITERR_REBASE, "Uncommitted changes exist in index");
			error = fail_with;
			goto done;
		}

		git_diff_free(diff);
		diff = NULL;
	}

	if (check_workdir) {
		if ((error = git_diff_index_to_workdir(&diff, repo, index, NULL)) < 0)
			goto done;

		if (git_diff_num_deltas(diff) > 0) {
			giterr_set(GITERR_REBASE, "Unstaged changes exist in workdir");
			error = fail_with;
			goto done;
		}
	}

done:
	git_diff_free(diff);
	git_index_free(index);
	git_tree_free(head);

	return error;
}

static int rebase_init_operations(
	git_rebase *rebase,
	git_repository *repo,
	const git_annotated_commit *branch,
	const git_annotated_commit *upstream,
	const git_annotated_commit *onto)
{
	git_revwalk *revwalk = NULL;
	git_commit *commit;
	git_oid id;
	bool merge;
	git_rebase_operation *operation;
	int error;

	if (!upstream)
		upstream = onto;

	if ((error = git_revwalk_new(&revwalk, rebase->repo)) < 0 ||
		(error = git_revwalk_push(revwalk, git_annotated_commit_id(branch))) < 0 ||
		(error = git_revwalk_hide(revwalk, git_annotated_commit_id(upstream))) < 0)
		goto done;

	git_revwalk_sorting(revwalk, GIT_SORT_REVERSE);

	while ((error = git_revwalk_next(&id, revwalk)) == 0) {
		if ((error = git_commit_lookup(&commit, repo, &id)) < 0)
			goto done;

		merge = (git_commit_parentcount(commit) > 1);
		git_commit_free(commit);

		if (merge)
			continue;

		operation = rebase_operation_alloc(rebase, GIT_REBASE_OPERATION_PICK, &id, NULL);
		GITERR_CHECK_ALLOC(operation);
	}

	error = 0;

done:
	git_revwalk_free(revwalk);
	return error;
}

static int rebase_init_merge(
	git_rebase *rebase,
	git_repository *repo,
	const git_annotated_commit *branch,
	const git_annotated_commit *upstream,
	const git_annotated_commit *onto)
{
	git_reference *head_ref = NULL;
	git_commit *onto_commit = NULL;
	git_buf reflog = GIT_BUF_INIT;
	git_buf state_path = GIT_BUF_INIT;
	int error;

	GIT_UNUSED(upstream);

	if ((error = git_buf_joinpath(&state_path, repo->path_repository, REBASE_MERGE_DIR)) < 0)
		goto done;

	rebase->state_path = git_buf_detach(&state_path);
	GITERR_CHECK_ALLOC(rebase->state_path);

	if (branch->ref_name && strcmp(branch->ref_name, "HEAD")) {
		rebase->orig_head_name = git__strdup(branch->ref_name);
		GITERR_CHECK_ALLOC(rebase->orig_head_name);
	} else {
		rebase->head_detached = 1;
	}

	rebase->onto_name = git__strdup(rebase_onto_name(onto));
	GITERR_CHECK_ALLOC(rebase->onto_name);

	rebase->quiet = rebase->options.quiet;

	git_oid_cpy(&rebase->orig_head_id, git_annotated_commit_id(branch));
	git_oid_cpy(&rebase->onto_id, git_annotated_commit_id(onto));

	if ((error = rebase_setupfiles(rebase)) < 0 ||
		(error = git_buf_printf(&reflog,
			"rebase: checkout %s", rebase_onto_name(onto))) < 0 ||
		(error = git_commit_lookup(
			&onto_commit, repo, git_annotated_commit_id(onto))) < 0 ||
		(error = git_checkout_tree(repo,
			(git_object *)onto_commit, &rebase->options.checkout_options)) < 0 ||
		(error = git_reference_create(&head_ref, repo, GIT_HEAD_FILE,
			git_annotated_commit_id(onto), 1, reflog.ptr)) < 0)
		goto done;

done:
	git_reference_free(head_ref);
	git_commit_free(onto_commit);
	git_buf_free(&reflog);
	git_buf_free(&state_path);

	return error;
}

static int rebase_init_inmemory(
	git_rebase *rebase,
	git_repository *repo,
	const git_annotated_commit *branch,
	const git_annotated_commit *upstream,
	const git_annotated_commit *onto)
{
	GIT_UNUSED(branch);
	GIT_UNUSED(upstream);

	return git_commit_lookup(
		&rebase->last_commit, repo, git_annotated_commit_id(onto));
}

int git_rebase_init(
	git_rebase **out,
	git_repository *repo,
	const git_annotated_commit *branch,
	const git_annotated_commit *upstream,
	const git_annotated_commit *onto,
	const git_rebase_options *given_opts)
{
	git_rebase *rebase = NULL;
	git_annotated_commit *head_branch = NULL;
	git_reference *head_ref = NULL;
	bool inmemory = (given_opts && given_opts->inmemory);
	int error;

	assert(repo && (upstream || onto));

	*out = NULL;

	if (!onto)
		onto = upstream;

	if ((error = rebase_check_versions(given_opts)) < 0)
		goto done;

	if (!inmemory) {
		if ((error = git_repository__ensure_not_bare(repo, "rebase")) < 0 ||
			(error = rebase_ensure_not_in_progress(repo)) < 0 ||
			(error = rebase_ensure_not_dirty(repo, true, true, GIT_ERROR)) < 0)
			goto done;
	}

	if (!branch) {
		if ((error = git_repository_head(&head_ref, repo)) < 0 ||
			(error = git_annotated_commit_from_ref(&head_branch, repo, head_ref)) < 0)
			goto done;

		branch = head_branch;
	}

	if (rebase_alloc(&rebase, given_opts) < 0)
		return -1;

	rebase->repo = repo;
	rebase->inmemory = inmemory;
	rebase->type = GIT_REBASE_TYPE_MERGE;

	if ((error = rebase_init_operations(rebase, repo, branch, upstream, onto)) < 0)
		goto done;

	if (inmemory)
		error = rebase_init_inmemory(rebase, repo, branch, upstream, onto);
	else
		error = rebase_init_merge(rebase, repo, branch ,upstream, onto);

	if (error == 0)
		*out = rebase;

done:
	git_reference_free(head_ref);
	git_annotated_commit_free(head_branch);

	if (error < 0) {
		rebase_cleanup(rebase);
		git_rebase_free(rebase);
	}

	return error;
}

static void normalize_checkout_options_for_apply(
	git_checkout_options *checkout_opts,
	git_rebase *rebase,
	git_commit *current_commit)
{
	memcpy(checkout_opts, &rebase->options.checkout_options, sizeof(git_checkout_options));

	if (!checkout_opts->ancestor_label)
		checkout_opts->ancestor_label = "ancestor";

	if (rebase->type == GIT_REBASE_TYPE_MERGE) {
		if (!checkout_opts->our_label)
			checkout_opts->our_label = rebase->onto_name;

		if (!checkout_opts->their_label)
			checkout_opts->their_label = git_commit_summary(current_commit);
	} else {
		abort();
	}
}

GIT_INLINE(int) rebase_movenext(git_rebase *rebase)
{
	size_t next = rebase->started ? rebase->current + 1 : 0;

	if (next == git_array_size(rebase->operations))
		return GIT_ITEROVER;

	rebase->started = 1;
	rebase->current = next;

	return 0;
}

static int rebase_next_merge(
	git_rebase_operation **out,
	git_rebase *rebase)
{
	git_buf path = GIT_BUF_INIT;
	git_commit *current_commit = NULL, *parent_commit = NULL;
	git_tree *current_tree = NULL, *head_tree = NULL, *parent_tree = NULL;
	git_index *index = NULL;
	git_indexwriter indexwriter = GIT_INDEXWRITER_INIT;
	git_rebase_operation *operation;
	git_checkout_options checkout_opts;
	char current_idstr[GIT_OID_HEXSZ];
	unsigned int parent_count;
	int error;

	*out = NULL;

	operation = git_array_get(rebase->operations, rebase->current);

	if ((error = git_commit_lookup(&current_commit, rebase->repo, &operation->id)) < 0 ||
		(error = git_commit_tree(&current_tree, current_commit)) < 0 ||
		(error = git_repository_head_tree(&head_tree, rebase->repo)) < 0)
		goto done;

	if ((parent_count = git_commit_parentcount(current_commit)) > 1) {
		giterr_set(GITERR_REBASE, "Cannot rebase a merge commit");
		error = -1;
		goto done;
	} else if (parent_count) {
		if ((error = git_commit_parent(&parent_commit, current_commit, 0)) < 0 ||
			(error = git_commit_tree(&parent_tree, parent_commit)) < 0)
			goto done;
	}

	git_oid_fmt(current_idstr, &operation->id);

	normalize_checkout_options_for_apply(&checkout_opts, rebase, current_commit);

	if ((error = git_indexwriter_init_for_operation(&indexwriter, rebase->repo, &checkout_opts.checkout_strategy)) < 0 ||
		(error = rebase_setupfile(rebase, MSGNUM_FILE, -1, "%" PRIuZ "\n", rebase->current+1)) < 0 ||
		(error = rebase_setupfile(rebase, CURRENT_FILE, -1, "%.*s\n", GIT_OID_HEXSZ, current_idstr)) < 0 ||
		(error = git_merge_trees(&index, rebase->repo, parent_tree, head_tree, current_tree, &rebase->options.merge_options)) < 0 ||
		(error = git_merge__check_result(rebase->repo, index)) < 0 ||
		(error = git_checkout_index(rebase->repo, index, &checkout_opts)) < 0 ||
		(error = git_indexwriter_commit(&indexwriter)) < 0)
		goto done;

	*out = operation;

done:
	git_indexwriter_cleanup(&indexwriter);
	git_index_free(index);
	git_tree_free(current_tree);
	git_tree_free(head_tree);
	git_tree_free(parent_tree);
	git_commit_free(parent_commit);
	git_commit_free(current_commit);
	git_buf_free(&path);

	return error;
}

static int rebase_next_inmemory(
	git_rebase_operation **out,
	git_rebase *rebase)
{
	git_commit *current_commit = NULL, *parent_commit = NULL;
	git_tree *current_tree = NULL, *head_tree = NULL, *parent_tree = NULL;
	git_rebase_operation *operation;
	git_index *index = NULL;
	unsigned int parent_count;
	int error;

	*out = NULL;

	operation = git_array_get(rebase->operations, rebase->current);

	if ((error = git_commit_lookup(&current_commit, rebase->repo, &operation->id)) < 0 ||
		(error = git_commit_tree(&current_tree, current_commit)) < 0)
		goto done;

	if ((parent_count = git_commit_parentcount(current_commit)) > 1) {
		giterr_set(GITERR_REBASE, "Cannot rebase a merge commit");
		error = -1;
		goto done;
	} else if (parent_count) {
		if ((error = git_commit_parent(&parent_commit, current_commit, 0)) < 0 ||
			(error = git_commit_tree(&parent_tree, parent_commit)) < 0)
			goto done;
	}

	if ((error = git_commit_tree(&head_tree, rebase->last_commit)) < 0 ||
		(error = git_merge_trees(&index, rebase->repo, parent_tree, head_tree, current_tree, &rebase->options.merge_options)) < 0)
		goto done;

	if (!rebase->index) {
		rebase->index = index;
		index = NULL;
	} else {
		if ((error = git_index_read_index(rebase->index, index)) < 0)
			goto done;
	}

	*out = operation;

done:
	git_commit_free(current_commit);
	git_commit_free(parent_commit);
	git_tree_free(current_tree);
	git_tree_free(head_tree);
	git_tree_free(parent_tree);
	git_index_free(index);

	return error;
}

int git_rebase_next(
	git_rebase_operation **out,
	git_rebase *rebase)
{
	int error;

	assert(out && rebase);

	if ((error = rebase_movenext(rebase)) < 0)
		return error;

	if (rebase->inmemory)
		error = rebase_next_inmemory(out, rebase);
	else if (rebase->type == GIT_REBASE_TYPE_MERGE)
		error = rebase_next_merge(out, rebase);
	else
		abort();

	return error;
}

int git_rebase_inmemory_index(
	git_index **out,
	git_rebase *rebase)
{
	assert(out && rebase && rebase->index);

	GIT_REFCOUNT_INC(rebase->index);
	*out = rebase->index;

	return 0;
}

static int rebase_commit__create(
	git_commit **out,
	git_rebase *rebase,
	git_index *index,
	git_commit *parent_commit,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message)
{
	git_rebase_operation *operation;
	git_commit *current_commit = NULL, *commit = NULL;
	git_tree *parent_tree = NULL, *tree = NULL;
	git_oid tree_id, commit_id;
	int error;

	operation = git_array_get(rebase->operations, rebase->current);

	if (git_index_has_conflicts(index)) {
		giterr_set(GITERR_REBASE, "conflicts have not been resolved");
		error = GIT_EUNMERGED;
		goto done;
	}

	if ((error = git_commit_lookup(&current_commit, rebase->repo, &operation->id)) < 0 ||
		(error = git_commit_tree(&parent_tree, parent_commit)) < 0 ||
		(error = git_index_write_tree_to(&tree_id, index, rebase->repo)) < 0 ||
		(error = git_tree_lookup(&tree, rebase->repo, &tree_id)) < 0)
		goto done;

	if (git_oid_equal(&tree_id, git_tree_id(parent_tree))) {
		giterr_set(GITERR_REBASE, "this patch has already been applied");
		error = GIT_EAPPLIED;
		goto done;
	}

	if (!author)
		author = git_commit_author(current_commit);

	if (!message) {
		message_encoding = git_commit_message_encoding(current_commit);
		message = git_commit_message(current_commit);
	}

	if ((error = git_commit_create(&commit_id, rebase->repo, NULL, author,
		committer, message_encoding, message, tree, 1,
		(const git_commit **)&parent_commit)) < 0 ||
		(error = git_commit_lookup(&commit, rebase->repo, &commit_id)) < 0)
		goto done;

	*out = commit;

done:
	if (error < 0)
		git_commit_free(commit);

	git_commit_free(current_commit);
	git_tree_free(parent_tree);
	git_tree_free(tree);

	return error;
}

static int rebase_commit_merge(
	git_oid *commit_id,
	git_rebase *rebase,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message)
{
	git_rebase_operation *operation;
	git_reference *head = NULL;
	git_commit *head_commit = NULL, *commit = NULL;
	git_index *index = NULL;
	char old_idstr[GIT_OID_HEXSZ], new_idstr[GIT_OID_HEXSZ];
	int error;

	operation = git_array_get(rebase->operations, rebase->current);
	assert(operation);

	if ((error = rebase_ensure_not_dirty(rebase->repo, false, true, GIT_EUNMERGED)) < 0 ||
		(error = git_repository_head(&head, rebase->repo)) < 0 ||
		(error = git_reference_peel((git_object **)&head_commit, head, GIT_OBJ_COMMIT)) < 0 ||
		(error = git_repository_index(&index, rebase->repo)) < 0 ||
		(error = rebase_commit__create(&commit, rebase, index, head_commit,
			author, committer, message_encoding, message)) < 0 ||
		(error = git_reference__update_for_commit(
			rebase->repo, NULL, "HEAD", git_commit_id(commit), "rebase")) < 0)
		goto done;

	git_oid_fmt(old_idstr, &operation->id);
	git_oid_fmt(new_idstr, git_commit_id(commit));

	if ((error = rebase_setupfile(rebase, REWRITTEN_FILE, O_CREAT|O_WRONLY|O_APPEND,
		"%.*s %.*s\n", GIT_OID_HEXSZ, old_idstr, GIT_OID_HEXSZ, new_idstr)) < 0)
		goto done;

	git_oid_cpy(commit_id, git_commit_id(commit));

done:
	git_index_free(index);
	git_reference_free(head);
	git_commit_free(head_commit);
	git_commit_free(commit);
	return error;
}

static int rebase_commit_inmemory(
	git_oid *commit_id,
	git_rebase *rebase,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message)
{
	git_commit *commit = NULL;
	int error = 0;

	assert(rebase->index);
	assert(rebase->last_commit);
	assert(rebase->current < rebase->operations.size);

	if ((error = rebase_commit__create(&commit, rebase, rebase->index,
		rebase->last_commit, author, committer, message_encoding, message)) < 0)
		goto done;

	git_commit_free(rebase->last_commit);
	rebase->last_commit = commit;

	git_oid_cpy(commit_id, git_commit_id(commit));

done:
	if (error < 0)
		git_commit_free(commit);

	return error;
}

int git_rebase_commit(
	git_oid *id,
	git_rebase *rebase,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message)
{
	int error;

	assert(rebase && committer);

	if (rebase->inmemory)
		error = rebase_commit_inmemory(
			id, rebase, author, committer, message_encoding, message);
	else if (rebase->type == GIT_REBASE_TYPE_MERGE)
		error = rebase_commit_merge(
			id, rebase, author, committer, message_encoding, message);
	else
		abort();

	return error;
}

int git_rebase_abort(git_rebase *rebase)
{
	git_reference *orig_head_ref = NULL;
	git_commit *orig_head_commit = NULL;
	int error;

	assert(rebase);

	if (rebase->inmemory)
		return 0;

	error = rebase->head_detached ?
		git_reference_create(&orig_head_ref, rebase->repo, GIT_HEAD_FILE,
			 &rebase->orig_head_id, 1, "rebase: aborting") :
		git_reference_symbolic_create(
			&orig_head_ref, rebase->repo, GIT_HEAD_FILE, rebase->orig_head_name, 1,
			"rebase: aborting");

	if (error < 0)
		goto done;

	if ((error = git_commit_lookup(
			&orig_head_commit, rebase->repo, &rebase->orig_head_id)) < 0 ||
		(error = git_reset(rebase->repo, (git_object *)orig_head_commit,
			GIT_RESET_HARD, &rebase->options.checkout_options)) < 0)
		goto done;

	error = rebase_cleanup(rebase);

done:
	git_commit_free(orig_head_commit);
	git_reference_free(orig_head_ref);

	return error;
}

static int notes_ref_lookup(git_buf *out, git_rebase *rebase)
{
	git_config *config = NULL;
	int do_rewrite, error;

	if (rebase->options.rewrite_notes_ref) {
		git_buf_attach_notowned(out,
			rebase->options.rewrite_notes_ref,
			strlen(rebase->options.rewrite_notes_ref));
		return 0;
	}

	if ((error = git_repository_config(&config, rebase->repo)) < 0 ||
		(error = git_config_get_bool(&do_rewrite, config, "notes.rewrite.rebase")) < 0) {

		if (error != GIT_ENOTFOUND)
			goto done;

		giterr_clear();
		do_rewrite = 1;
	}

	error = do_rewrite ?
		git_config_get_string_buf(out, config, "notes.rewriteref") :
		GIT_ENOTFOUND;

done:
	git_config_free(config);
	return error;
}

static int rebase_copy_note(
	git_rebase *rebase,
	const char *notes_ref,
	git_oid *from,
	git_oid *to,
	const git_signature *committer)
{
	git_note *note = NULL;
	git_oid note_id;
	git_signature *who = NULL;
	int error;

	if ((error = git_note_read(&note, rebase->repo, notes_ref, from)) < 0) {
		if (error == GIT_ENOTFOUND) {
			giterr_clear();
			error = 0;
		}

		goto done;
	}

	if (!committer) {
		if((error = git_signature_default(&who, rebase->repo)) < 0) {
			if (error != GIT_ENOTFOUND ||
				(error = git_signature_now(&who, "unknown", "unknown")) < 0)
				goto done;

			giterr_clear();
		}

		committer = who;
	}

	error = git_note_create(&note_id, rebase->repo, notes_ref,
		git_note_author(note), committer, to, git_note_message(note), 0);

done:
	git_note_free(note);
	git_signature_free(who);

	return error;
}

static int rebase_copy_notes(
	git_rebase *rebase,
	const git_signature *committer)
{
	git_buf path = GIT_BUF_INIT, rewritten = GIT_BUF_INIT, notes_ref = GIT_BUF_INIT;
	char *pair_list, *fromstr, *tostr, *end;
	git_oid from, to;
	unsigned int linenum = 1;
	int error = 0;

	if ((error = notes_ref_lookup(&notes_ref, rebase)) < 0) {
		if (error == GIT_ENOTFOUND) {
			giterr_clear();
			error = 0;
		}

		goto done;
	}

	if ((error = git_buf_joinpath(&path, rebase->state_path, REWRITTEN_FILE)) < 0 ||
		(error = git_futils_readbuffer(&rewritten, path.ptr)) < 0)
		goto done;

	pair_list = rewritten.ptr;

	while (*pair_list) {
		fromstr = pair_list;

		if ((end = strchr(fromstr, '\n')) == NULL)
			goto on_error;

		pair_list = end+1;
		*end = '\0';

		if ((end = strchr(fromstr, ' ')) == NULL)
			goto on_error;

		tostr = end+1;
		*end = '\0';

		if (strlen(fromstr) != GIT_OID_HEXSZ ||
			strlen(tostr) != GIT_OID_HEXSZ ||
			git_oid_fromstr(&from, fromstr) < 0 ||
			git_oid_fromstr(&to, tostr) < 0)
			goto on_error;

		if ((error = rebase_copy_note(rebase, notes_ref.ptr, &from, &to, committer)) < 0)
			goto done;

		linenum++;
	}

	goto done;

on_error:
	giterr_set(GITERR_REBASE, "Invalid rewritten file at line %d", linenum);
	error = -1;

done:
	git_buf_free(&rewritten);
	git_buf_free(&path);
	git_buf_free(&notes_ref);

	return error;
}

static int return_to_orig_head(git_rebase *rebase)
{
	git_reference *terminal_ref = NULL, *branch_ref = NULL, *head_ref = NULL;
	git_commit *terminal_commit = NULL;
	git_buf branch_msg = GIT_BUF_INIT, head_msg = GIT_BUF_INIT;
	char onto[GIT_OID_HEXSZ];
	int error = 0;

	git_oid_fmt(onto, &rebase->onto_id);

	if ((error = git_buf_printf(&branch_msg,
			"rebase finished: %s onto %.*s",
			rebase->orig_head_name, GIT_OID_HEXSZ, onto)) == 0 &&
		(error = git_buf_printf(&head_msg,
			"rebase finished: returning to %s",
			rebase->orig_head_name)) == 0 &&
		(error = git_repository_head(&terminal_ref, rebase->repo)) == 0 &&
		(error = git_reference_peel((git_object **)&terminal_commit,
			terminal_ref, GIT_OBJ_COMMIT)) == 0 &&
		(error = git_reference_create_matching(&branch_ref,
			rebase->repo, rebase->orig_head_name,
			git_commit_id(terminal_commit), 1,
			&rebase->orig_head_id, branch_msg.ptr)) == 0)
		error = git_reference_symbolic_create(&head_ref,
			rebase->repo, GIT_HEAD_FILE, rebase->orig_head_name, 1,
			head_msg.ptr);

	git_buf_free(&head_msg);
	git_buf_free(&branch_msg);
	git_commit_free(terminal_commit);
	git_reference_free(head_ref);
	git_reference_free(branch_ref);
	git_reference_free(terminal_ref);

	return error;
}

int git_rebase_finish(
	git_rebase *rebase,
	const git_signature *signature)
{
	int error = 0;

	assert(rebase);

	if (rebase->inmemory)
		return 0;

	if (!rebase->head_detached)
		error = return_to_orig_head(rebase);

	if (error == 0 && (error = rebase_copy_notes(rebase, signature)) == 0)
		error = rebase_cleanup(rebase);

	return error;
}

size_t git_rebase_operation_entrycount(git_rebase *rebase)
{
	assert(rebase);

	return git_array_size(rebase->operations);
}

size_t git_rebase_operation_current(git_rebase *rebase)
{
	assert(rebase);

	return rebase->started ? rebase->current : GIT_REBASE_NO_OPERATION;
}

git_rebase_operation *git_rebase_operation_byindex(git_rebase *rebase, size_t idx)
{
	assert(rebase);

	return git_array_get(rebase->operations, idx);
}

void git_rebase_free(git_rebase *rebase)
{
	if (rebase == NULL)
		return;

	git_index_free(rebase->index);
	git_commit_free(rebase->last_commit);
	git__free(rebase->onto_name);
	git__free(rebase->orig_head_name);
	git__free(rebase->state_path);
	git_array_clear(rebase->operations);
	git__free((char *)rebase->options.rewrite_notes_ref);
	git__free(rebase);
}
