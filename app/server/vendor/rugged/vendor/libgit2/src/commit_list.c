/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "commit_list.h"
#include "common.h"
#include "revwalk.h"
#include "pool.h"
#include "odb.h"

int git_commit_list_time_cmp(const void *a, const void *b)
{
	const git_commit_list_node *commit_a = a;
	const git_commit_list_node *commit_b = b;

	return (commit_a->time < commit_b->time);
}

git_commit_list *git_commit_list_insert(git_commit_list_node *item, git_commit_list **list_p)
{
	git_commit_list *new_list = git__malloc(sizeof(git_commit_list));
	if (new_list != NULL) {
		new_list->item = item;
		new_list->next = *list_p;
	}
	*list_p = new_list;
	return new_list;
}

git_commit_list *git_commit_list_insert_by_date(git_commit_list_node *item, git_commit_list **list_p)
{
	git_commit_list **pp = list_p;
	git_commit_list *p;

	while ((p = *pp) != NULL) {
		if (git_commit_list_time_cmp(p->item, item) > 0)
			break;

		pp = &p->next;
	}

	return git_commit_list_insert(item, pp);
}

git_commit_list_node *git_commit_list_alloc_node(git_revwalk *walk)
{
	return (git_commit_list_node *)git_pool_malloc(&walk->commit_pool, COMMIT_ALLOC);
}

static int commit_error(git_commit_list_node *commit, const char *msg)
{
	char commit_oid[GIT_OID_HEXSZ + 1];
	git_oid_fmt(commit_oid, &commit->oid);
	commit_oid[GIT_OID_HEXSZ] = '\0';

	giterr_set(GITERR_ODB, "Failed to parse commit %s - %s", commit_oid, msg);

	return -1;
}

static git_commit_list_node **alloc_parents(
	git_revwalk *walk, git_commit_list_node *commit, size_t n_parents)
{
	if (n_parents <= PARENTS_PER_COMMIT)
		return (git_commit_list_node **)((char *)commit + sizeof(git_commit_list_node));

	return (git_commit_list_node **)git_pool_malloc(
		&walk->commit_pool, (uint32_t)(n_parents * sizeof(git_commit_list_node *)));
}


void git_commit_list_free(git_commit_list **list_p)
{
	git_commit_list *list = *list_p;

	if (list == NULL)
		return;

	while (list) {
		git_commit_list *temp = list;
		list = temp->next;
		git__free(temp);
	}

	*list_p = NULL;
}

git_commit_list_node *git_commit_list_pop(git_commit_list **stack)
{
	git_commit_list *top = *stack;
	git_commit_list_node *item = top ? top->item : NULL;

	if (top) {
		*stack = top->next;
		git__free(top);
	}
	return item;
}

static int commit_quick_parse(
	git_revwalk *walk,
	git_commit_list_node *commit,
	const uint8_t *buffer,
	size_t buffer_len)
{
	const size_t parent_len = strlen("parent ") + GIT_OID_HEXSZ + 1;
	const uint8_t *buffer_end = buffer + buffer_len;
	const uint8_t *parents_start, *committer_start;
	int i, parents = 0;
	int commit_time;

	buffer += strlen("tree ") + GIT_OID_HEXSZ + 1;

	parents_start = buffer;
	while (buffer + parent_len < buffer_end && memcmp(buffer, "parent ", strlen("parent ")) == 0) {
		parents++;
		buffer += parent_len;
	}

	commit->parents = alloc_parents(walk, commit, parents);
	GITERR_CHECK_ALLOC(commit->parents);

	buffer = parents_start;
	for (i = 0; i < parents; ++i) {
		git_oid oid;

		if (git_oid_fromstr(&oid, (const char *)buffer + strlen("parent ")) < 0)
			return -1;

		commit->parents[i] = git_revwalk__commit_lookup(walk, &oid);
		if (commit->parents[i] == NULL)
			return -1;

		buffer += parent_len;
	}

	commit->out_degree = (unsigned short)parents;

	if ((committer_start = buffer = memchr(buffer, '\n', buffer_end - buffer)) == NULL)
		return commit_error(commit, "object is corrupted");

	buffer++;

	if ((buffer = memchr(buffer, '\n', buffer_end - buffer)) == NULL)
		return commit_error(commit, "object is corrupted");

	/* Skip trailing spaces */
	while (buffer > committer_start && git__isspace(*buffer))
		buffer--;

	/* Seek for the begining of the pack of digits */
	while (buffer > committer_start && git__isdigit(*buffer))
		buffer--;

	/* Skip potential timezone offset */
	if ((buffer > committer_start) && (*buffer == '+' || *buffer == '-')) {
		buffer--;

		while (buffer > committer_start && git__isspace(*buffer))
			buffer--;

		while (buffer > committer_start && git__isdigit(*buffer))
			buffer--;
	}

	if ((buffer == committer_start) || (git__strtol32(&commit_time, (char *)(buffer + 1), NULL, 10) < 0))
		return commit_error(commit, "cannot parse commit time");

	commit->time = (time_t)commit_time;
	commit->parsed = 1;
	return 0;
}

int git_commit_list_parse(git_revwalk *walk, git_commit_list_node *commit)
{
	git_odb_object *obj;
	int error;

	if (commit->parsed)
		return 0;

	if ((error = git_odb_read(&obj, walk->odb, &commit->oid)) < 0)
		return error;

	if (obj->cached.type != GIT_OBJ_COMMIT) {
		giterr_set(GITERR_INVALID, "Object is no commit object");
		error = -1;
	} else
		error = commit_quick_parse(
			walk, commit,
			(const uint8_t *)git_odb_object_data(obj),
			git_odb_object_size(obj));

	git_odb_object_free(obj);
	return error;
}

