/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "revwalk.h"
#include "merge.h"
#include "git2/graph.h"

static int interesting(git_pqueue *list, git_commit_list *roots)
{
	unsigned int i;

	for (i = 0; i < git_pqueue_size(list); i++) {
		git_commit_list_node *commit = git_pqueue_get(list, i);
		if ((commit->flags & STALE) == 0)
			return 1;
	}

	while(roots) {
		if ((roots->item->flags & STALE) == 0)
			return 1;
		roots = roots->next;
	}

	return 0;
}

static int mark_parents(git_revwalk *walk, git_commit_list_node *one,
	git_commit_list_node *two)
{
	unsigned int i;
	git_commit_list *roots = NULL;
	git_pqueue list;

	/* if the commit is repeated, we have a our merge base already */
	if (one == two) {
		one->flags |= PARENT1 | PARENT2 | RESULT;
		return 0;
	}

	if (git_pqueue_init(&list, 0, 2, git_commit_list_time_cmp) < 0)
		return -1;

	if (git_commit_list_parse(walk, one) < 0)
		goto on_error;
	one->flags |= PARENT1;
	if (git_pqueue_insert(&list, one) < 0)
		goto on_error;

	if (git_commit_list_parse(walk, two) < 0)
		goto on_error;
	two->flags |= PARENT2;
	if (git_pqueue_insert(&list, two) < 0)
		goto on_error;

	/* as long as there are non-STALE commits */
	while (interesting(&list, roots)) {
		git_commit_list_node *commit = git_pqueue_pop(&list);
		int flags;

		if (commit == NULL)
			break;

		flags = commit->flags & (PARENT1 | PARENT2 | STALE);
		if (flags == (PARENT1 | PARENT2)) {
			if (!(commit->flags & RESULT))
				commit->flags |= RESULT;
			/* we mark the parents of a merge stale */
			flags |= STALE;
		}

		for (i = 0; i < commit->out_degree; i++) {
			git_commit_list_node *p = commit->parents[i];
			if ((p->flags & flags) == flags)
				continue;

			if (git_commit_list_parse(walk, p) < 0)
				goto on_error;

			p->flags |= flags;
			if (git_pqueue_insert(&list, p) < 0)
				goto on_error;
		}

		/* Keep track of root commits, to make sure the path gets marked */
		if (commit->out_degree == 0) {
			if (git_commit_list_insert(commit, &roots) == NULL)
				goto on_error;
		}
	}

	git_commit_list_free(&roots);
	git_pqueue_free(&list);
	return 0;

on_error:
	git_commit_list_free(&roots);
	git_pqueue_free(&list);
	return -1;
}


static int ahead_behind(git_commit_list_node *one, git_commit_list_node *two,
	size_t *ahead, size_t *behind)
{
	git_commit_list_node *commit;
	git_pqueue pq;
	int error = 0, i;
	*ahead = 0;
	*behind = 0;

	if (git_pqueue_init(&pq, 0, 2, git_commit_list_time_cmp) < 0)
		return -1;

	if ((error = git_pqueue_insert(&pq, one)) < 0 ||
		(error = git_pqueue_insert(&pq, two)) < 0)
		goto done;

	while ((commit = git_pqueue_pop(&pq)) != NULL) {
		if (commit->flags & RESULT ||
			(commit->flags & (PARENT1 | PARENT2)) == (PARENT1 | PARENT2))
			continue;
		else if (commit->flags & PARENT1)
			(*ahead)++;
		else if (commit->flags & PARENT2)
			(*behind)++;

		for (i = 0; i < commit->out_degree; i++) {
			git_commit_list_node *p = commit->parents[i];
			if ((error = git_pqueue_insert(&pq, p)) < 0)
				goto done;
		}
		commit->flags |= RESULT;
	}

done:
	git_pqueue_free(&pq);
	return error;
}

int git_graph_ahead_behind(size_t *ahead, size_t *behind, git_repository *repo,
	const git_oid *local, const git_oid *upstream)
{
	git_revwalk *walk;
	git_commit_list_node *commit_u, *commit_l;

	if (git_revwalk_new(&walk, repo) < 0)
		return -1;

	commit_u = git_revwalk__commit_lookup(walk, upstream);
	if (commit_u == NULL)
		goto on_error;

	commit_l = git_revwalk__commit_lookup(walk, local);
	if (commit_l == NULL)
		goto on_error;

	if (mark_parents(walk, commit_l, commit_u) < 0)
		goto on_error;
	if (ahead_behind(commit_l, commit_u, ahead, behind) < 0)
		goto on_error;

	git_revwalk_free(walk);

	return 0;

on_error:
	git_revwalk_free(walk);
	return -1;
}

int git_graph_descendant_of(git_repository *repo, const git_oid *commit, const git_oid *ancestor)
{
	git_oid merge_base;
	int error;

	if (git_oid_equal(commit, ancestor))
		return 0;

	error = git_merge_base(&merge_base, repo, commit, ancestor);
	/* No merge-base found, it's not a descendant */
	if (error == GIT_ENOTFOUND)
		return 0;

	if (error < 0)
		return error;

	return git_oid_equal(&merge_base, ancestor);
}
