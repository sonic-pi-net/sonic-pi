/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "git2.h"

#include "common.h"
#include "pack.h"
#include "pack-objects.h"
#include "remote.h"
#include "vector.h"
#include "push.h"
#include "tree.h"

static int push_spec_rref_cmp(const void *a, const void *b)
{
	const push_spec *push_spec_a = a, *push_spec_b = b;

	return strcmp(push_spec_a->rref, push_spec_b->rref);
}

static int push_status_ref_cmp(const void *a, const void *b)
{
	const push_status *push_status_a = a, *push_status_b = b;

	return strcmp(push_status_a->ref, push_status_b->ref);
}

int git_push_new(git_push **out, git_remote *remote)
{
	git_push *p;

	*out = NULL;

	p = git__calloc(1, sizeof(*p));
	GITERR_CHECK_ALLOC(p);

	p->repo = remote->repo;
	p->remote = remote;
	p->report_status = 1;
	p->pb_parallelism = 1;

	if (git_vector_init(&p->specs, 0, push_spec_rref_cmp) < 0) {
		git__free(p);
		return -1;
	}

	if (git_vector_init(&p->status, 0, push_status_ref_cmp) < 0) {
		git_vector_free(&p->specs);
		git__free(p);
		return -1;
	}

	*out = p;
	return 0;
}

int git_push_set_options(git_push *push, const git_push_options *opts)
{
	if (!push || !opts)
		return -1;

	GITERR_CHECK_VERSION(opts, GIT_PUSH_OPTIONS_VERSION, "git_push_options");

	push->pb_parallelism = opts->pb_parallelism;

	return 0;
}

int git_push_set_callbacks(
	git_push *push,
	git_packbuilder_progress pack_progress_cb,
	void *pack_progress_cb_payload,
	git_push_transfer_progress transfer_progress_cb,
	void *transfer_progress_cb_payload)
{
	if (!push)
		return -1;

	push->pack_progress_cb = pack_progress_cb;
	push->pack_progress_cb_payload = pack_progress_cb_payload;

	push->transfer_progress_cb = transfer_progress_cb;
	push->transfer_progress_cb_payload = transfer_progress_cb_payload;

	return 0;
}

static void free_refspec(push_spec *spec)
{
	if (spec == NULL)
		return;

	if (spec->lref)
		git__free(spec->lref);

	if (spec->rref)
		git__free(spec->rref);

	git__free(spec);
}

static int check_rref(char *ref)
{
	if (git__prefixcmp(ref, "refs/")) {
		giterr_set(GITERR_INVALID, "Not a valid reference '%s'", ref);
		return -1;
	}

	return 0;
}

static int check_lref(git_push *push, char *ref)
{
	/* lref must be resolvable to an existing object */
	git_object *obj;
	int error = git_revparse_single(&obj, push->repo, ref);
	git_object_free(obj);

	if (!error)
		return 0;

	if (error == GIT_ENOTFOUND)
		giterr_set(GITERR_REFERENCE,
			"src refspec '%s' does not match any existing object", ref);
	else
		giterr_set(GITERR_INVALID, "Not a valid reference '%s'", ref);
	return -1;
}

static int parse_refspec(git_push *push, push_spec **spec, const char *str)
{
	push_spec *s;
	char *delim;

	*spec = NULL;

	s = git__calloc(1, sizeof(*s));
	GITERR_CHECK_ALLOC(s);

	if (str[0] == '+') {
		s->force = true;
		str++;
	}

	delim = strchr(str, ':');
	if (delim == NULL) {
		s->lref = git__strdup(str);
		if (!s->lref || check_lref(push, s->lref) < 0)
			goto on_error;
	} else {
		if (delim - str) {
			s->lref = git__strndup(str, delim - str);
			if (!s->lref || check_lref(push, s->lref) < 0)
				goto on_error;
		}

		if (strlen(delim + 1)) {
			s->rref = git__strdup(delim + 1);
			if (!s->rref || check_rref(s->rref) < 0)
				goto on_error;
		}
	}

	if (!s->lref && !s->rref)
		goto on_error;

	/* If rref is ommitted, use the same ref name as lref */
	if (!s->rref) {
		s->rref = git__strdup(s->lref);
		if (!s->rref || check_rref(s->rref) < 0)
			goto on_error;
	}

	*spec = s;
	return 0;

on_error:
	free_refspec(s);
	return -1;
}

int git_push_add_refspec(git_push *push, const char *refspec)
{
	push_spec *spec;

	if (parse_refspec(push, &spec, refspec) < 0 ||
	    git_vector_insert(&push->specs, spec) < 0)
		return -1;

	return 0;
}

int git_push_update_tips(
		git_push *push,
		const git_signature *signature,
		const char *reflog_message)
{
	git_buf remote_ref_name = GIT_BUF_INIT;
	size_t i, j;
	git_refspec *fetch_spec;
	push_spec *push_spec = NULL;
	git_reference *remote_ref;
	push_status *status;
	int error = 0;

	git_vector_foreach(&push->status, i, status) {
		int fire_callback = 1;

		/* Find the corresponding remote ref */
		fetch_spec = git_remote__matching_refspec(push->remote, status->ref);
		if (!fetch_spec)
			continue;

		if ((error = git_refspec_transform(&remote_ref_name, fetch_spec, status->ref)) < 0)
			goto on_error;

		/* Find matching  push ref spec */
		git_vector_foreach(&push->specs, j, push_spec) {
			if (!strcmp(push_spec->rref, status->ref))
				break;
		}

		/* Could not find the corresponding push ref spec for this push update */
		if (j == push->specs.length)
			continue;

		/* If this ref update was successful (ok, not ng), it will have an empty message */
		if (status->msg == NULL) {
			/* Update the remote ref */
			if (git_oid_iszero(&push_spec->loid)) {
				error = git_reference_lookup(&remote_ref, push->remote->repo, git_buf_cstr(&remote_ref_name));

				if (error >= 0) {
					error = git_reference_delete(remote_ref);
					git_reference_free(remote_ref);
				}
			} else {
				error = git_reference_create(NULL, push->remote->repo,
							git_buf_cstr(&remote_ref_name), &push_spec->loid, 1, signature,
							reflog_message ? reflog_message : "update by push");
			}
		}

		if (error < 0) {
			if (error != GIT_ENOTFOUND)
				goto on_error;

			giterr_clear();
			fire_callback = 0;
		}

		if (fire_callback && push->remote->callbacks.update_tips) {
			error = push->remote->callbacks.update_tips(git_buf_cstr(&remote_ref_name),
						&push_spec->roid, &push_spec->loid, push->remote->callbacks.payload);

			if (error < 0)
				goto on_error;
		}
	}

	error = 0;

on_error:
	git_buf_free(&remote_ref_name);
	return error;
}

/**
 * Insert all tags until we find a non-tag object, which is returned
 * in `out`.
 */
static int enqueue_tag(git_object **out, git_push *push, git_oid *id)
{
	git_object *obj = NULL, *target = NULL;
	int error;

	if ((error = git_object_lookup(&obj, push->repo, id, GIT_OBJ_TAG)) < 0)
		return error;

	while (git_object_type(obj) == GIT_OBJ_TAG) {
		if ((error = git_packbuilder_insert(push->pb, git_object_id(obj), NULL)) < 0)
			break;

		if ((error = git_tag_target(&target, (git_tag *) obj)) < 0)
			break;

		git_object_free(obj);
		obj = target;
	}

	if (error < 0)
		git_object_free(obj);
	else
		*out = obj;

	return error;
}

static int revwalk(git_vector *commits, git_push *push)
{
	git_remote_head *head;
	push_spec *spec;
	git_revwalk *rw;
	git_oid oid;
	unsigned int i;
	int error = -1;

	if (git_revwalk_new(&rw, push->repo) < 0)
		return -1;

	git_revwalk_sorting(rw, GIT_SORT_TIME);

	git_vector_foreach(&push->specs, i, spec) {
		git_otype type;
		size_t size;

		if (git_oid_iszero(&spec->loid))
			/*
			 * Delete reference on remote side;
			 * nothing to do here.
			 */
			continue;

		if (git_oid_equal(&spec->loid, &spec->roid))
			continue; /* up-to-date */

		if (git_odb_read_header(&size, &type, push->repo->_odb, &spec->loid) < 0)
			goto on_error;

		if (type == GIT_OBJ_TAG) {
			git_object *target;

			if ((error = enqueue_tag(&target, push, &spec->loid)) < 0)
				goto on_error;

			if (git_object_type(target) == GIT_OBJ_COMMIT) {
				if (git_revwalk_push(rw, git_object_id(target)) < 0) {
					git_object_free(target);
					goto on_error;
				}
			} else {
				if (git_packbuilder_insert(
					push->pb, git_object_id(target), NULL) < 0) {
					git_object_free(target);
					goto on_error;
				}
			}
			git_object_free(target);
		} else if (git_revwalk_push(rw, &spec->loid) < 0)
			goto on_error;

		if (!spec->force) {
			git_oid base;

			if (git_oid_iszero(&spec->roid))
				continue;

			if (!git_odb_exists(push->repo->_odb, &spec->roid)) {
				giterr_set(GITERR_REFERENCE, "Cannot push missing reference");
				error = GIT_ENONFASTFORWARD;
				goto on_error;
			}

			error = git_merge_base(&base, push->repo,
					       &spec->loid, &spec->roid);

			if (error == GIT_ENOTFOUND ||
				(!error && !git_oid_equal(&base, &spec->roid))) {
				giterr_set(GITERR_REFERENCE,
					"Cannot push non-fastforwardable reference");
				error = GIT_ENONFASTFORWARD;
				goto on_error;
			}

			if (error < 0)
				goto on_error;
		}
	}

	git_vector_foreach(&push->remote->refs, i, head) {
		if (git_oid_iszero(&head->oid))
			continue;

		/* TODO */
		git_revwalk_hide(rw, &head->oid);
	}

	while ((error = git_revwalk_next(&oid, rw)) == 0) {
		git_oid *o = git__malloc(GIT_OID_RAWSZ);
		if (!o) {
			error = -1;
			goto on_error;
		}
		git_oid_cpy(o, &oid);
		if ((error = git_vector_insert(commits, o)) < 0)
			goto on_error;
	}

on_error:
	git_revwalk_free(rw);
	return error == GIT_ITEROVER ? 0 : error;
}

static int enqueue_object(
	const git_tree_entry *entry,
	git_packbuilder *pb)
{
	switch (git_tree_entry_type(entry)) {
		case GIT_OBJ_COMMIT:
			return 0;
		case GIT_OBJ_TREE:
			return git_packbuilder_insert_tree(pb, &entry->oid);
		default:
			return git_packbuilder_insert(pb, &entry->oid, entry->filename);
	}
}

static int queue_differences(
	git_tree *base,
	git_tree *delta,
	git_packbuilder *pb)
{
	git_tree *b_child = NULL, *d_child = NULL;
	size_t b_length = git_tree_entrycount(base);
	size_t d_length = git_tree_entrycount(delta);
	size_t i = 0, j = 0;
	int error;

	while (i < b_length && j < d_length) {
		const git_tree_entry *b_entry = git_tree_entry_byindex(base, i);
		const git_tree_entry *d_entry = git_tree_entry_byindex(delta, j);
		int cmp = 0;

		if (!git_oid__cmp(&b_entry->oid, &d_entry->oid))
			goto loop;

		cmp = strcmp(b_entry->filename, d_entry->filename);

		/* If the entries are both trees and they have the same name but are
		 * different, then we'll recurse after adding the right-hand entry */
		if (!cmp &&
			git_tree_entry__is_tree(b_entry) &&
			git_tree_entry__is_tree(d_entry)) {
			/* Add the right-hand entry */
			if ((error = git_packbuilder_insert(pb, &d_entry->oid,
				d_entry->filename)) < 0)
				goto on_error;

			/* Acquire the subtrees and recurse */
			if ((error = git_tree_lookup(&b_child,
					git_tree_owner(base), &b_entry->oid)) < 0 ||
				(error = git_tree_lookup(&d_child,
					git_tree_owner(delta), &d_entry->oid)) < 0 ||
				(error = queue_differences(b_child, d_child, pb)) < 0)
				goto on_error;

			git_tree_free(b_child); b_child = NULL;
			git_tree_free(d_child); d_child = NULL;
		}
		/* If the object is new or different in the right-hand tree,
		 * then enumerate it */
		else if (cmp >= 0 &&
			(error = enqueue_object(d_entry, pb)) < 0)
			goto on_error;

	loop:
		if (cmp <= 0) i++;
		if (cmp >= 0) j++;
	}

	/* Drain the right-hand tree of entries */
	for (; j < d_length; j++)
		if ((error = enqueue_object(git_tree_entry_byindex(delta, j), pb)) < 0)
			goto on_error;

	error = 0;

on_error:
	if (b_child)
		git_tree_free(b_child);

	if (d_child)
		git_tree_free(d_child);

	return error;
}

static int queue_objects(git_push *push)
{
	git_vector commits = GIT_VECTOR_INIT;
	git_oid *oid;
	size_t i;
	unsigned j;
	int error;

	if ((error = revwalk(&commits, push)) < 0)
		goto on_error;

	git_vector_foreach(&commits, i, oid) {
		git_commit *parent = NULL, *commit;
		git_tree *tree = NULL, *ptree = NULL;
		size_t parentcount;

		if ((error = git_commit_lookup(&commit,	push->repo, oid)) < 0)
			goto on_error;

		/* Insert the commit */
		if ((error = git_packbuilder_insert(push->pb, oid, NULL)) < 0)
			goto loop_error;

		parentcount = git_commit_parentcount(commit);

		if (!parentcount) {
			if ((error = git_packbuilder_insert_tree(push->pb,
				git_commit_tree_id(commit))) < 0)
				goto loop_error;
		} else {
			if ((error = git_tree_lookup(&tree, push->repo,
					git_commit_tree_id(commit))) < 0 ||
				(error = git_packbuilder_insert(push->pb,
					git_commit_tree_id(commit), NULL)) < 0)
				goto loop_error;

			/* For each parent, add the items which are different */
			for (j = 0; j < parentcount; j++) {
				if ((error = git_commit_parent(&parent, commit, j)) < 0 ||
					(error = git_commit_tree(&ptree, parent)) < 0 ||
					(error = queue_differences(ptree, tree, push->pb)) < 0)
					goto loop_error;

				git_tree_free(ptree); ptree = NULL;
				git_commit_free(parent); parent = NULL;
			}
		}

		error = 0;

	loop_error:
		if (tree)
			git_tree_free(tree);

		if (ptree)
			git_tree_free(ptree);

		if (parent)
			git_commit_free(parent);

		git_commit_free(commit);

		if (error < 0)
			goto on_error;
	}

	error = 0;

on_error:
	git_vector_free_deep(&commits);
	return error;
}

static int calculate_work(git_push *push)
{
	git_remote_head *head;
	push_spec *spec;
	unsigned int i, j;

	/* Update local and remote oids*/

	git_vector_foreach(&push->specs, i, spec) {
		if (spec->lref) {
			/* This is a create or update.  Local ref must exist. */
			if (git_reference_name_to_id(
					&spec->loid, push->repo, spec->lref) < 0) {
				giterr_set(GITERR_REFERENCE, "No such reference '%s'", spec->lref);
				return -1;
			}
		}

		if (spec->rref) {
			/* Remote ref may or may not (e.g. during create) already exist. */
			git_vector_foreach(&push->remote->refs, j, head) {
				if (!strcmp(spec->rref, head->name)) {
					git_oid_cpy(&spec->roid, &head->oid);
					break;
				}
			}
		}
	}

	return 0;
}

static int do_push(git_push *push)
{
	int error = 0;
	git_transport *transport = push->remote->transport;

	if (!transport->push) {
		giterr_set(GITERR_NET, "Remote transport doesn't support push");
		error = -1;
		goto on_error;
	}

	/*
	 * A pack-file MUST be sent if either create or update command
	 * is used, even if the server already has all the necessary
	 * objects.  In this case the client MUST send an empty pack-file.
	 */

	if ((error = git_packbuilder_new(&push->pb, push->repo)) < 0)
		goto on_error;

	git_packbuilder_set_threads(push->pb, push->pb_parallelism);

	if (push->pack_progress_cb)
		if ((error = git_packbuilder_set_callbacks(push->pb, push->pack_progress_cb, push->pack_progress_cb_payload)) < 0)
			goto on_error;

	if ((error = calculate_work(push)) < 0 ||
		(error = queue_objects(push)) < 0 ||
		(error = transport->push(transport, push)) < 0)
		goto on_error;

on_error:
	git_packbuilder_free(push->pb);
	return error;
}

static int filter_refs(git_remote *remote)
{
	const git_remote_head **heads;
	size_t heads_len, i;

	git_vector_clear(&remote->refs);

	if (git_remote_ls(&heads, &heads_len, remote) < 0)
		return -1;

	for (i = 0; i < heads_len; i++) {
		if (git_vector_insert(&remote->refs, (void *)heads[i]) < 0)
			return -1;
	}

	return 0;
}

int git_push_finish(git_push *push)
{
	int error;

	if (!git_remote_connected(push->remote) &&
		(error = git_remote_connect(push->remote, GIT_DIRECTION_PUSH)) < 0)
		return error;

	if ((error = filter_refs(push->remote)) < 0 ||
		(error = do_push(push)) < 0)
		return error;

	return 0;
}

int git_push_unpack_ok(const git_push *push)
{
	return push->unpack_ok;
}

int git_push_status_foreach(git_push *push,
		int (*cb)(const char *ref, const char *msg, void *data),
		void *data)
{
	push_status *status;
	unsigned int i;

	git_vector_foreach(&push->status, i, status) {
		int error = cb(status->ref, status->msg, data);
		if (error)
			return giterr_set_after_callback(error);
	}

	return 0;
}

void git_push_status_free(push_status *status)
{
	if (status == NULL)
		return;

	git__free(status->msg);
	git__free(status->ref);
	git__free(status);
}

void git_push_free(git_push *push)
{
	push_spec *spec;
	push_status *status;
	unsigned int i;

	if (push == NULL)
		return;

	git_vector_foreach(&push->specs, i, spec) {
		free_refspec(spec);
	}
	git_vector_free(&push->specs);

	git_vector_foreach(&push->status, i, status) {
		git_push_status_free(status);
	}
	git_vector_free(&push->status);

	git__free(push);
}

int git_push_init_options(git_push_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_push_options, GIT_PUSH_OPTIONS_INIT);
	return 0;
}
