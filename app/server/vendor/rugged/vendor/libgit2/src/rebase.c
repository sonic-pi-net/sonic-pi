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

	git_rebase_type_t type;
	char *state_path;

	int head_detached : 1,
		quiet : 1,
		started : 1;

	char *orig_head_name;
	git_oid orig_head_id;

	git_oid onto_id;
	char *onto_name;

	git_array_t(git_rebase_operation) operations;
	size_t current;
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

static int rebase_open_merge(git_rebase *rebase)
{
	git_buf state_path = GIT_BUF_INIT, buf = GIT_BUF_INIT, cmt = GIT_BUF_INIT;
	git_oid current_id = {{0}};
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
	if ((error = rebase_readoid(&current_id, &buf, &state_path, CURRENT_FILE)) < 0 &&
		error != GIT_ENOTFOUND)
		goto done;

	/* Read cmt.* */
	git_array_init_to_size(rebase->operations, end);
	GITERR_CHECK_ARRAY(rebase->operations);

	for (i = 0; i < end; i++) {
		operation = git_array_alloc(rebase->operations);
		GITERR_CHECK_ALLOC(operation);

		git_buf_clear(&cmt);

		if ((error = git_buf_printf(&cmt, "cmt.%" PRIuZ, (i+1))) < 0 ||
			(error = rebase_readoid((git_oid *)&operation->id, &buf, &state_path, cmt.ptr)) < 0)
			goto done;
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

int git_rebase_open(git_rebase **out, git_repository *repo)
{
	git_rebase *rebase;
	git_buf path = GIT_BUF_INIT, orig_head_name = GIT_BUF_INIT,
		orig_head_id = GIT_BUF_INIT, onto_id = GIT_BUF_INIT;
	int state_path_len, error;

	assert(repo);

	rebase = git__calloc(1, sizeof(git_rebase));
	GITERR_CHECK_ALLOC(rebase);

	rebase->repo = repo;

	if ((error = rebase_state_type(&rebase->type, &rebase->state_path, repo)) < 0)
		goto done;

	if (rebase->type == GIT_REBASE_TYPE_NONE) {
		giterr_set(GITERR_REBASE, "There is no rebase in progress");
		return GIT_ENOTFOUND;
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

	if ((error = rebase_setupfile(rebase, END_FILE, -1, "%d\n", git_array_size(rebase->operations))) < 0 ||
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

	git_oid_fmt(onto, &rebase->onto_id);
	git_oid_fmt(orig_head, &rebase->orig_head_id);

	if (p_mkdir(rebase->state_path, REBASE_DIR_MODE) < 0) {
		giterr_set(GITERR_OS, "Failed to create rebase directory '%s'", rebase->state_path);
		return -1;
	}

	if (git_repository__set_orig_head(rebase->repo, &rebase->orig_head_id) < 0 ||
		rebase_setupfile(rebase, HEAD_NAME_FILE, -1, "%s\n", rebase->orig_head_name) < 0 ||
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

static int rebase_normalize_opts(
	git_repository *repo,
	git_rebase_options *opts,
	const git_rebase_options *given_opts)
{
	git_rebase_options default_opts = GIT_REBASE_OPTIONS_INIT;
	git_config *config;

	if (given_opts)
		memcpy(opts, given_opts, sizeof(git_rebase_options));
	else
		memcpy(opts, &default_opts, sizeof(git_rebase_options));

	if (git_repository_config(&config, repo) < 0)
		return -1;

	if (given_opts && given_opts->rewrite_notes_ref) {
		opts->rewrite_notes_ref = git__strdup(given_opts->rewrite_notes_ref);
		GITERR_CHECK_ALLOC(opts->rewrite_notes_ref);
	} else if (git_config__get_bool_force(config, "notes.rewrite.rebase", 1)) {
		const char *rewrite_ref = git_config__get_string_force(
			config, "notes.rewriteref", NOTES_DEFAULT_REF);

		if (rewrite_ref) {
			opts->rewrite_notes_ref = git__strdup(rewrite_ref);
			GITERR_CHECK_ALLOC(opts->rewrite_notes_ref);
		}
	}

	git_config_free(config);

	return 0;
}

static void rebase_opts_free(git_rebase_options *opts)
{
	if (!opts)
		return;

	git__free((char *)opts->rewrite_notes_ref);
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

static int rebase_ensure_not_dirty(git_repository *repo)
{
	git_tree *head = NULL;
	git_index *index = NULL;
	git_diff *diff = NULL;
	int error;

	if ((error = git_repository_head_tree(&head, repo)) < 0 ||
		(error = git_repository_index(&index, repo)) < 0 ||
		(error = git_diff_tree_to_index(&diff, repo, head, index, NULL)) < 0)
		goto done;

	if (git_diff_num_deltas(diff) > 0) {
		giterr_set(GITERR_REBASE, "Uncommitted changes exist in index");
		error = -1;
		goto done;
	}

	git_diff_free(diff);
	diff = NULL;

	if ((error = git_diff_index_to_workdir(&diff, repo, index, NULL)) < 0)
		goto done;

	if (git_diff_num_deltas(diff) > 0) {
		giterr_set(GITERR_REBASE, "Unstaged changes exist in workdir");
		error = -1;
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

	git_revwalk_sorting(revwalk, GIT_SORT_REVERSE | GIT_SORT_TIME);

	while ((error = git_revwalk_next(&id, revwalk)) == 0) {
		if ((error = git_commit_lookup(&commit, repo, &id)) < 0)
			goto done;

		merge = (git_commit_parentcount(commit) > 1);
		git_commit_free(commit);

		if (merge)
			continue;

		operation = git_array_alloc(rebase->operations);
		operation->type = GIT_REBASE_OPERATION_PICK;
		git_oid_cpy((git_oid *)&operation->id, &id);
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
	if (rebase_init_operations(rebase, repo, branch, upstream, onto) < 0)
		return -1;

	rebase->onto_name = git__strdup(rebase_onto_name(onto));
	GITERR_CHECK_ALLOC(rebase->onto_name);

	return 0;
}

static int rebase_init(
	git_rebase *rebase,
	git_repository *repo,
	const git_annotated_commit *branch,
	const git_annotated_commit *upstream,
	const git_annotated_commit *onto,
	const git_rebase_options *opts)
{
	git_buf state_path = GIT_BUF_INIT;
	int error;

	git_buf_joinpath(&state_path, repo->path_repository, REBASE_MERGE_DIR);

	rebase->repo = repo;
	rebase->type = GIT_REBASE_TYPE_MERGE;
	rebase->state_path = git_buf_detach(&state_path);
	rebase->orig_head_name = git__strdup(branch->ref_name ? branch->ref_name : ORIG_DETACHED_HEAD);
	rebase->quiet = opts->quiet;

	git_oid_cpy(&rebase->orig_head_id, git_annotated_commit_id(branch));
	git_oid_cpy(&rebase->onto_id, git_annotated_commit_id(onto));

	if (!rebase->orig_head_name || !rebase->state_path)
		return -1;

	error = rebase_init_merge(rebase, repo, branch, upstream, onto);

	git_buf_free(&state_path);

	return error;
}

int git_rebase_init(
	git_rebase **out,
	git_repository *repo,
	const git_annotated_commit *branch,
	const git_annotated_commit *upstream,
	const git_annotated_commit *onto,
	const git_signature *signature,
	const git_rebase_options *given_opts)
{
	git_rebase *rebase = NULL;
	git_rebase_options opts;
	git_reference *head_ref = NULL;
	git_buf reflog = GIT_BUF_INIT;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
	int error;

	assert(repo && branch && (upstream || onto));

	*out = NULL;

	GITERR_CHECK_VERSION(given_opts, GIT_MERGE_OPTIONS_VERSION, "git_merge_options");

	if (!onto)
		onto = upstream;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	if ((error = rebase_normalize_opts(repo, &opts, given_opts)) < 0 ||
		(error = git_repository__ensure_not_bare(repo, "rebase")) < 0 ||
		(error = rebase_ensure_not_in_progress(repo)) < 0 ||
		(error = rebase_ensure_not_dirty(repo)) < 0)
		return error;

	rebase = git__calloc(1, sizeof(git_rebase));
	GITERR_CHECK_ALLOC(rebase);

	if ((error = rebase_init(rebase, repo, branch, upstream, onto, &opts)) < 0 ||
		(error = rebase_setupfiles(rebase)) < 0 ||
		(error = git_buf_printf(&reflog,
			"rebase: checkout %s", rebase_onto_name(onto))) < 0 ||
		(error = git_reference_create(&head_ref, repo, GIT_HEAD_FILE,
			git_annotated_commit_id(onto), 1, signature, reflog.ptr)) < 0 ||
		(error = git_checkout_head(repo, &checkout_opts)) < 0)
		goto done;

	*out = rebase;

done:
	if (error < 0) {
		rebase_cleanup(rebase);
		git_rebase_free(rebase);
	}

	git_reference_free(head_ref);
	git_buf_free(&reflog);
	rebase_opts_free(&opts);

	return error;
}

static void normalize_checkout_opts(
	git_rebase *rebase,
	git_commit *current_commit,
	git_checkout_options *checkout_opts,
	const git_checkout_options *given_checkout_opts)
{
	if (given_checkout_opts != NULL)
		memcpy(checkout_opts, given_checkout_opts, sizeof(git_checkout_options));
	else {
		git_checkout_options default_checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;
		default_checkout_opts.checkout_strategy =  GIT_CHECKOUT_SAFE;

		memcpy(checkout_opts, &default_checkout_opts, sizeof(git_checkout_options));
	}

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
	git_rebase *rebase,
	git_checkout_options *given_checkout_opts)
{
	git_buf path = GIT_BUF_INIT;
	git_checkout_options checkout_opts = {0};
	git_commit *current_commit = NULL, *parent_commit = NULL;
	git_tree *current_tree = NULL, *head_tree = NULL, *parent_tree = NULL;
	git_index *index = NULL;
	git_rebase_operation *operation;
	char current_idstr[GIT_OID_HEXSZ];
	unsigned int parent_count;
	int error;

	*out = NULL;

	if ((error = rebase_movenext(rebase)) < 0)
		goto done;

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

	if ((error = rebase_setupfile(rebase, MSGNUM_FILE, -1, "%d\n", rebase->current+1)) < 0 ||
		(error = rebase_setupfile(rebase, CURRENT_FILE, -1, "%.*s\n", GIT_OID_HEXSZ, current_idstr)) < 0)
		goto done;

	normalize_checkout_opts(rebase, current_commit, &checkout_opts, given_checkout_opts);

	if ((error = git_merge_trees(&index, rebase->repo, parent_tree, head_tree, current_tree, NULL)) < 0 ||
		(error = git_merge__check_result(rebase->repo, index)) < 0 ||
		(error = git_checkout_index(rebase->repo, index, &checkout_opts)) < 0)
		goto done;

	*out = operation;

done:
	git_index_free(index);
	git_tree_free(current_tree);
	git_tree_free(head_tree);
	git_tree_free(parent_tree);
	git_commit_free(parent_commit);
	git_commit_free(current_commit);
	git_buf_free(&path);

	return error;
}

int git_rebase_next(
	git_rebase_operation **out,
	git_rebase *rebase,
	git_checkout_options *checkout_opts)
{
	int error;

	assert(out && rebase);

	switch (rebase->type) {
	case GIT_REBASE_TYPE_MERGE:
		error = rebase_next_merge(out, rebase, checkout_opts);
		break;
	default:
		abort();
	}

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
	git_index *index = NULL;
	git_reference *head = NULL;
	git_commit *current_commit = NULL, *head_commit = NULL, *commit = NULL;
	git_rebase_operation *operation;
	git_tree *head_tree = NULL, *tree = NULL;
	git_diff *diff = NULL;
	git_oid tree_id;
	git_buf reflog_msg = GIT_BUF_INIT;
	char old_idstr[GIT_OID_HEXSZ], new_idstr[GIT_OID_HEXSZ];
	int error;

	operation = git_array_get(rebase->operations, rebase->current);
	assert(operation);

	if ((error = git_repository_index(&index, rebase->repo)) < 0)
		goto done;

	if (git_index_has_conflicts(index)) {
		giterr_set(GITERR_REBASE, "Conflicts have not been resolved");
		error = GIT_EMERGECONFLICT;
		goto done;
	}

	if ((error = git_commit_lookup(&current_commit, rebase->repo, &operation->id)) < 0 ||
		(error = git_repository_head(&head, rebase->repo)) < 0 ||
		(error = git_reference_peel((git_object **)&head_commit, head, GIT_OBJ_COMMIT)) < 0 ||
		(error = git_commit_tree(&head_tree, head_commit)) < 0 ||
		(error = git_diff_tree_to_index(&diff, rebase->repo, head_tree, index, NULL)) < 0)
		goto done;

	if (git_diff_num_deltas(diff) == 0) {
		giterr_set(GITERR_REBASE, "This patch has already been applied");
		error = GIT_EAPPLIED;
		goto done;
	}

	if ((error = git_index_write_tree(&tree_id, index)) < 0 ||
		(error = git_tree_lookup(&tree, rebase->repo, &tree_id)) < 0)
		goto done;

	if (!author)
		author = git_commit_author(current_commit);

	if (!message) {
		message_encoding = git_commit_message_encoding(current_commit);
		message = git_commit_message(current_commit);
	}

	if ((error = git_commit_create(commit_id, rebase->repo, NULL, author,
			committer, message_encoding, message, tree, 1,
			(const git_commit **)&head_commit)) < 0 ||
		(error = git_commit_lookup(&commit, rebase->repo, commit_id)) < 0 ||
		(error = git_reference__update_for_commit(
			rebase->repo, NULL, "HEAD", commit_id, committer, "rebase")) < 0)
		goto done;

	git_oid_fmt(old_idstr, git_commit_id(current_commit));
	git_oid_fmt(new_idstr, commit_id);

	error = rebase_setupfile(rebase, REWRITTEN_FILE, O_CREAT|O_WRONLY|O_APPEND,
		"%.*s %.*s\n", GIT_OID_HEXSZ, old_idstr, GIT_OID_HEXSZ, new_idstr);

done:
	git_buf_free(&reflog_msg);
	git_commit_free(commit);
	git_diff_free(diff);
	git_tree_free(tree);
	git_tree_free(head_tree);
	git_commit_free(head_commit);
	git_commit_free(current_commit);
	git_reference_free(head);
	git_index_free(index);

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

	switch (rebase->type) {
	case GIT_REBASE_TYPE_MERGE:
		error = rebase_commit_merge(
			id, rebase, author, committer, message_encoding, message);
		break;
	default:
		abort();
	}

	return error;
}

int git_rebase_abort(git_rebase *rebase, const git_signature *signature)
{
	git_reference *orig_head_ref = NULL;
	git_commit *orig_head_commit = NULL;
	int error;

	assert(rebase && signature);

	error = rebase->head_detached ?
		git_reference_create(&orig_head_ref, rebase->repo, GIT_HEAD_FILE,
			 &rebase->orig_head_id, 1, signature, "rebase: aborting") :
		git_reference_symbolic_create(
			&orig_head_ref, rebase->repo, GIT_HEAD_FILE, rebase->orig_head_name, 1,
			signature, "rebase: aborting");

	if (error < 0)
		goto done;

	if ((error = git_commit_lookup(
			&orig_head_commit, rebase->repo, &rebase->orig_head_id)) < 0 ||
		(error = git_reset(rebase->repo, (git_object *)orig_head_commit,
			GIT_RESET_HARD, NULL, signature, NULL)) < 0)
		goto done;

	error = rebase_cleanup(rebase);

done:
	git_commit_free(orig_head_commit);
	git_reference_free(orig_head_ref);

	return error;
}

static int rebase_copy_note(
	git_rebase *rebase,
	git_oid *from,
	git_oid *to,
	const git_signature *committer,
	const git_rebase_options *opts)
{
	git_note *note = NULL;
	git_oid note_id;
	git_signature *who = NULL;
	int error;

	if ((error = git_note_read(&note, rebase->repo, opts->rewrite_notes_ref, from)) < 0) {
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

	error = git_note_create(&note_id, rebase->repo, opts->rewrite_notes_ref,
		git_note_author(note), committer, to, git_note_message(note), 0);

done:
	git_note_free(note);
	git_signature_free(who);

	return error;
}

static int rebase_copy_notes(
	git_rebase *rebase,
	const git_signature *committer,
	const git_rebase_options *opts)
{
	git_buf path = GIT_BUF_INIT, rewritten = GIT_BUF_INIT;
	char *pair_list, *fromstr, *tostr, *end;
	git_oid from, to;
	unsigned int linenum = 1;
	int error = 0;

	if (!opts->rewrite_notes_ref)
		goto done;

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

		if ((error = rebase_copy_note(rebase, &from, &to, committer, opts)) < 0)
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

	return error;
}

int git_rebase_finish(
	git_rebase *rebase,
	const git_signature *signature,
	const git_rebase_options *given_opts)
{
	git_rebase_options opts;
	git_reference *terminal_ref = NULL, *branch_ref = NULL, *head_ref = NULL;
	git_commit *terminal_commit = NULL;
	git_buf branch_msg = GIT_BUF_INIT, head_msg = GIT_BUF_INIT;
	char onto[GIT_OID_HEXSZ];
	int error;

	assert(rebase);

	if ((error = rebase_normalize_opts(rebase->repo, &opts, given_opts)) < 0)
		goto done;

	git_oid_fmt(onto, &rebase->onto_id);

	if ((error = git_buf_printf(&branch_msg, "rebase finished: %s onto %.*s",
			rebase->orig_head_name, GIT_OID_HEXSZ, onto)) < 0 ||
		(error = git_buf_printf(&head_msg, "rebase finished: returning to %s",
			rebase->orig_head_name)) < 0 ||
		(error = git_repository_head(&terminal_ref, rebase->repo)) < 0 ||
		(error = git_reference_peel((git_object **)&terminal_commit,
			terminal_ref, GIT_OBJ_COMMIT)) < 0 ||
		(error = git_reference_create_matching(&branch_ref,
			rebase->repo, rebase->orig_head_name, git_commit_id(terminal_commit), 1,
			&rebase->orig_head_id, signature, branch_msg.ptr)) < 0 ||
		(error = git_reference_symbolic_create(&head_ref,
			rebase->repo, GIT_HEAD_FILE, rebase->orig_head_name, 1,
			signature, head_msg.ptr)) < 0 ||
		(error = rebase_copy_notes(rebase, signature, &opts)) < 0)
		goto done;

	error = rebase_cleanup(rebase);

done:
	git_buf_free(&head_msg);
	git_buf_free(&branch_msg);
	git_commit_free(terminal_commit);
	git_reference_free(head_ref);
	git_reference_free(branch_ref);
	git_reference_free(terminal_ref);
	rebase_opts_free(&opts);

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

	return rebase->current;
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

	git__free(rebase->onto_name);
	git__free(rebase->orig_head_name);
	git__free(rebase->state_path);
	git_array_clear(rebase->operations);
	git__free(rebase);
}
