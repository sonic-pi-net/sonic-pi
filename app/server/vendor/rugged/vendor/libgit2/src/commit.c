/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2/common.h"
#include "git2/object.h"
#include "git2/repository.h"
#include "git2/signature.h"
#include "git2/sys/commit.h"

#include "common.h"
#include "odb.h"
#include "commit.h"
#include "signature.h"
#include "message.h"

void git_commit__free(void *_commit)
{
	git_commit *commit = _commit;

	git_array_clear(commit->parent_ids);

	git_signature_free(commit->author);
	git_signature_free(commit->committer);

	git__free(commit->raw_header);
	git__free(commit->raw_message);
	git__free(commit->message_encoding);
	git__free(commit->summary);

	git__free(commit);
}

static int update_ref_for_commit(git_repository *repo, git_reference *ref, const char *update_ref, const git_oid *id, const git_signature *committer)
{
	git_reference *ref2 = NULL;
	int error;
	git_commit *c;
	const char *shortmsg;
	git_buf reflog_msg = GIT_BUF_INIT;

	if ((error = git_commit_lookup(&c, repo, id)) < 0) {
		return error;
	}

	shortmsg = git_commit_summary(c);
	git_buf_printf(&reflog_msg, "commit%s: %s",
		       git_commit_parentcount(c) == 0 ? " (initial)" : "",
		       shortmsg);
	git_commit_free(c);

	if (ref) {
		error = git_reference_set_target(&ref2, ref, id, committer, git_buf_cstr(&reflog_msg));
		git_reference_free(ref2);
	} else {
		error = git_reference__update_terminal(repo, update_ref, id, committer, git_buf_cstr(&reflog_msg));
	}

	git_buf_free(&reflog_msg);
	return error;
}

int git_commit_create_from_callback(
	git_oid *id,
	git_repository *repo,
	const char *update_ref,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_oid *tree,
	git_commit_parent_callback parent_cb,
	void *parent_payload)
{
	git_reference *ref = NULL;
	int error = 0, matched_parent = 0;
	const git_oid *current_id = NULL;
	git_buf commit = GIT_BUF_INIT;
	size_t i = 0;
	git_odb *odb;
	const git_oid *parent;

	assert(id && repo && tree && parent_cb);

	if (update_ref) {
		error = git_reference_lookup_resolved(&ref, repo, update_ref, 10);
		if (error < 0 && error != GIT_ENOTFOUND)
			return error;
	}
	giterr_clear();

	if (ref)
		current_id = git_reference_target(ref);

	git_oid__writebuf(&commit, "tree ", tree);

	while ((parent = parent_cb(i, parent_payload)) != NULL) {
		git_oid__writebuf(&commit, "parent ", parent);
		if (i == 0 && current_id && git_oid_equal(current_id, parent))
			matched_parent = 1;
		i++;
	}

	if (ref && !matched_parent) {
		git_reference_free(ref);
		git_buf_free(&commit);
		giterr_set(GITERR_OBJECT, "failed to create commit: current tip is not the first parent");
		return GIT_EMODIFIED;
	}

	git_signature__writebuf(&commit, "author ", author);
	git_signature__writebuf(&commit, "committer ", committer);

	if (message_encoding != NULL)
		git_buf_printf(&commit, "encoding %s\n", message_encoding);

	git_buf_putc(&commit, '\n');

	if (git_buf_puts(&commit, message) < 0)
		goto on_error;

	if (git_repository_odb__weakptr(&odb, repo) < 0)
		goto on_error;

	if (git_odb_write(id, odb, commit.ptr, commit.size, GIT_OBJ_COMMIT) < 0)
		goto on_error;

	git_buf_free(&commit);

	if (update_ref != NULL) {
		error = update_ref_for_commit(repo, ref, update_ref, id, committer);
		git_reference_free(ref);
		return error;
	}

	return 0;

on_error:
	git_buf_free(&commit);
	giterr_set(GITERR_OBJECT, "Failed to create commit.");
	return -1;
}

typedef struct {
	size_t total;
	va_list args;
} commit_parent_varargs;

static const git_oid *commit_parent_from_varargs(size_t curr, void *payload)
{
	commit_parent_varargs *data = payload;
	const git_commit *commit;
	if (curr >= data->total)
		return NULL;
	commit = va_arg(data->args, const git_commit *);
	return commit ? git_commit_id(commit) : NULL;
}

int git_commit_create_v(
	git_oid *id,
	git_repository *repo,
	const char *update_ref,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_tree *tree,
	size_t parent_count,
	...)
{
	int error = 0;
	commit_parent_varargs data;

	assert(tree && git_tree_owner(tree) == repo);

	data.total = parent_count;
	va_start(data.args, parent_count);

	error = git_commit_create_from_callback(
		id, repo, update_ref, author, committer,
		message_encoding, message, git_tree_id(tree),
		commit_parent_from_varargs, &data);

	va_end(data.args);
	return error;
}

typedef struct {
	size_t total;
	const git_oid **parents;
} commit_parent_oids;

static const git_oid *commit_parent_from_ids(size_t curr, void *payload)
{
	commit_parent_oids *data = payload;
	return (curr < data->total) ? data->parents[curr] : NULL;
}

int git_commit_create_from_ids(
	git_oid *id,
	git_repository *repo,
	const char *update_ref,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_oid *tree,
	size_t parent_count,
	const git_oid *parents[])
{
	commit_parent_oids data = { parent_count, parents };

	return git_commit_create_from_callback(
		id, repo, update_ref, author, committer,
		message_encoding, message, tree,
		commit_parent_from_ids, &data);
}

typedef struct {
	size_t total;
	const git_commit **parents;
	git_repository *repo;
} commit_parent_data;

static const git_oid *commit_parent_from_array(size_t curr, void *payload)
{
	commit_parent_data *data = payload;
	const git_commit *commit;
	if (curr >= data->total)
		return NULL;
	commit = data->parents[curr];
	if (git_commit_owner(commit) != data->repo)
		return NULL;
	return git_commit_id(commit);
}

int git_commit_create(
	git_oid *id,
	git_repository *repo,
	const char *update_ref,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_tree *tree,
	size_t parent_count,
	const git_commit *parents[])
{
	commit_parent_data data = { parent_count, parents, repo };

	assert(tree && git_tree_owner(tree) == repo);

	return git_commit_create_from_callback(
		id, repo, update_ref, author, committer,
		message_encoding, message, git_tree_id(tree),
		commit_parent_from_array, &data);
}

static const git_oid *commit_parent_for_amend(size_t curr, void *payload)
{
	const git_commit *commit_to_amend = payload;
	if (curr >= git_array_size(commit_to_amend->parent_ids))
		return NULL;
	return git_array_get(commit_to_amend->parent_ids, curr);
}

int git_commit_amend(
	git_oid *id,
	const git_commit *commit_to_amend,
	const char *update_ref,
	const git_signature *author,
	const git_signature *committer,
	const char *message_encoding,
	const char *message,
	const git_tree *tree)
{
	git_repository *repo;
	git_oid tree_id;
	git_reference *ref;
	int error;

	assert(id && commit_to_amend);

	repo = git_commit_owner(commit_to_amend);

	if (!author)
		author = git_commit_author(commit_to_amend);
	if (!committer)
		committer = git_commit_committer(commit_to_amend);
	if (!message_encoding)
		message_encoding = git_commit_message_encoding(commit_to_amend);
	if (!message)
		message = git_commit_message(commit_to_amend);

	if (!tree) {
		git_tree *old_tree;
		GITERR_CHECK_ERROR( git_commit_tree(&old_tree, commit_to_amend) );
		git_oid_cpy(&tree_id, git_tree_id(old_tree));
		git_tree_free(old_tree);
	} else {
		assert(git_tree_owner(tree) == repo);
		git_oid_cpy(&tree_id, git_tree_id(tree));
	}

	if (update_ref) {
		if ((error = git_reference_lookup_resolved(&ref, repo, update_ref, 5)) < 0)
			return error;

		if (git_oid_cmp(git_commit_id(commit_to_amend), git_reference_target(ref))) {
			git_reference_free(ref);
			giterr_set(GITERR_REFERENCE, "commit to amend is not the tip of the given branch");
			return -1;
		}
	}

	error = git_commit_create_from_callback(
		id, repo, NULL, author, committer, message_encoding, message,
		&tree_id, commit_parent_for_amend, (void *)commit_to_amend);

	if (!error && update_ref) {
		error = update_ref_for_commit(repo, ref, NULL, id, committer);
		git_reference_free(ref);
	}

	return error;
}

int git_commit__parse(void *_commit, git_odb_object *odb_obj)
{
	git_commit *commit = _commit;
	const char *buffer_start = git_odb_object_data(odb_obj), *buffer;
	const char *buffer_end = buffer_start + git_odb_object_size(odb_obj);
	git_oid parent_id;
	size_t header_len;

	buffer = buffer_start;

	/* Allocate for one, which will allow not to realloc 90% of the time  */
	git_array_init_to_size(commit->parent_ids, 1);
	GITERR_CHECK_ARRAY(commit->parent_ids);

	/* The tree is always the first field */
	if (git_oid__parse(&commit->tree_id, &buffer, buffer_end, "tree ") < 0)
		goto bad_buffer;

	/*
	 * TODO: commit grafts!
	 */

	while (git_oid__parse(&parent_id, &buffer, buffer_end, "parent ") == 0) {
		git_oid *new_id = git_array_alloc(commit->parent_ids);
		GITERR_CHECK_ALLOC(new_id);

		git_oid_cpy(new_id, &parent_id);
	}

	commit->author = git__malloc(sizeof(git_signature));
	GITERR_CHECK_ALLOC(commit->author);

	if (git_signature__parse(commit->author, &buffer, buffer_end, "author ", '\n') < 0)
		return -1;

	/* Always parse the committer; we need the commit time */
	commit->committer = git__malloc(sizeof(git_signature));
	GITERR_CHECK_ALLOC(commit->committer);

	if (git_signature__parse(commit->committer, &buffer, buffer_end, "committer ", '\n') < 0)
		return -1;

	/* Parse add'l header entries */
	while (buffer < buffer_end) {
		const char *eoln = buffer;
		if (buffer[-1] == '\n' && buffer[0] == '\n')
			break;

		while (eoln < buffer_end && *eoln != '\n')
			++eoln;

		if (git__prefixcmp(buffer, "encoding ") == 0) {
			buffer += strlen("encoding ");

			commit->message_encoding = git__strndup(buffer, eoln - buffer);
			GITERR_CHECK_ALLOC(commit->message_encoding);
		}

		if (eoln < buffer_end && *eoln == '\n')
			++eoln;
		buffer = eoln;
	}

	header_len = buffer - buffer_start;
	commit->raw_header = git__strndup(buffer_start, header_len);
	GITERR_CHECK_ALLOC(commit->raw_header);

	/* point "buffer" to data after header, +1 for the final LF */
	buffer = buffer_start + header_len + 1;

	/* extract commit message */
	if (buffer <= buffer_end) {
		commit->raw_message = git__strndup(buffer, buffer_end - buffer);
		GITERR_CHECK_ALLOC(commit->raw_message);
	}

	return 0;

bad_buffer:
	giterr_set(GITERR_OBJECT, "Failed to parse bad commit object");
	return -1;
}

#define GIT_COMMIT_GETTER(_rvalue, _name, _return) \
	_rvalue git_commit_##_name(const git_commit *commit) \
	{\
		assert(commit); \
		return _return; \
	}

GIT_COMMIT_GETTER(const git_signature *, author, commit->author)
GIT_COMMIT_GETTER(const git_signature *, committer, commit->committer)
GIT_COMMIT_GETTER(const char *, message_raw, commit->raw_message)
GIT_COMMIT_GETTER(const char *, message_encoding, commit->message_encoding)
GIT_COMMIT_GETTER(const char *, raw_header, commit->raw_header)
GIT_COMMIT_GETTER(git_time_t, time, commit->committer->when.time)
GIT_COMMIT_GETTER(int, time_offset, commit->committer->when.offset)
GIT_COMMIT_GETTER(unsigned int, parentcount, (unsigned int)git_array_size(commit->parent_ids))
GIT_COMMIT_GETTER(const git_oid *, tree_id, &commit->tree_id);

const char *git_commit_message(const git_commit *commit)
{
	const char *message;

	assert(commit);

	message = commit->raw_message;

	/* trim leading newlines from raw message */
	while (*message && *message == '\n')
		++message;

	return message;
}

const char *git_commit_summary(git_commit *commit)
{
	git_buf summary = GIT_BUF_INIT;
	const char *msg, *space;

	assert(commit);

	if (!commit->summary) {
		for (msg = git_commit_message(commit), space = NULL; *msg; ++msg) {
			if (msg[0] == '\n' && (!msg[1] || msg[1] == '\n'))
				break;
			else if (msg[0] == '\n')
				git_buf_putc(&summary, ' ');
			else if (git__isspace(msg[0]))
				space = space ? space : msg;
			else if (space) {
				git_buf_put(&summary, space, (msg - space) + 1);
				space = NULL;
			} else
				git_buf_putc(&summary, *msg);
		}

		commit->summary = git_buf_detach(&summary);
		if (!commit->summary)
			commit->summary = git__strdup("");
	}

	return commit->summary;
}

int git_commit_tree(git_tree **tree_out, const git_commit *commit)
{
	assert(commit);
	return git_tree_lookup(tree_out, commit->object.repo, &commit->tree_id);
}

const git_oid *git_commit_parent_id(
	const git_commit *commit, unsigned int n)
{
	assert(commit);

	return git_array_get(commit->parent_ids, n);
}

int git_commit_parent(
	git_commit **parent, const git_commit *commit, unsigned int n)
{
	const git_oid *parent_id;
	assert(commit);

	parent_id = git_commit_parent_id(commit, n);
	if (parent_id == NULL) {
		giterr_set(GITERR_INVALID, "Parent %u does not exist", n);
		return GIT_ENOTFOUND;
	}

	return git_commit_lookup(parent, commit->object.repo, parent_id);
}

int git_commit_nth_gen_ancestor(
	git_commit **ancestor,
	const git_commit *commit,
	unsigned int n)
{
	git_commit *current, *parent = NULL;
	int error;

	assert(ancestor && commit);

	if (git_object_dup((git_object **) &current, (git_object *) commit) < 0)
		return -1;

	if (n == 0) {
		*ancestor = current;
		return 0;
	}

	while (n--) {
		error = git_commit_parent(&parent, current, 0);

		git_commit_free(current);

		if (error < 0)
			return error;

		current = parent;
	}

	*ancestor = parent;
	return 0;
}
