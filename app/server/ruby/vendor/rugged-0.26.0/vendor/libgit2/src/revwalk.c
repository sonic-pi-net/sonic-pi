/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "revwalk.h"

#include "commit.h"
#include "odb.h"
#include "pool.h"

#include "git2/revparse.h"
#include "merge.h"
#include "vector.h"

git_commit_list_node *git_revwalk__commit_lookup(
	git_revwalk *walk, const git_oid *oid)
{
	git_commit_list_node *commit;
	khiter_t pos;
	int ret;

	/* lookup and reserve space if not already present */
	pos = git_oidmap_lookup_index(walk->commits, oid);
	if (git_oidmap_valid_index(walk->commits, pos))
		return git_oidmap_value_at(walk->commits, pos);

	commit = git_commit_list_alloc_node(walk);
	if (commit == NULL)
		return NULL;

	git_oid_cpy(&commit->oid, oid);

	pos = git_oidmap_put(walk->commits, &commit->oid, &ret);
	assert(ret != 0);
	git_oidmap_set_value_at(walk->commits, pos, commit);

	return commit;
}

static int push_commit(git_revwalk *walk, const git_oid *oid, int uninteresting, int from_glob)
{
	git_oid commit_id;
	int error;
	git_object *obj, *oobj;
	git_commit_list_node *commit;
	git_commit_list *list;

	if ((error = git_object_lookup(&oobj, walk->repo, oid, GIT_OBJ_ANY)) < 0)
		return error;

	error = git_object_peel(&obj, oobj, GIT_OBJ_COMMIT);
	git_object_free(oobj);

	if (error == GIT_ENOTFOUND || error == GIT_EINVALIDSPEC || error == GIT_EPEEL) {
		/* If this comes from e.g. push_glob("tags"), ignore this */
		if (from_glob)
			return 0;

		giterr_set(GITERR_INVALID, "object is not a committish");
		return -1;
	}
	if (error < 0)
		return error;

	git_oid_cpy(&commit_id, git_object_id(obj));
	git_object_free(obj);

	commit = git_revwalk__commit_lookup(walk, &commit_id);
	if (commit == NULL)
		return -1; /* error already reported by failed lookup */

	/* A previous hide already told us we don't want this commit  */
	if (commit->uninteresting)
		return 0;

	if (uninteresting)
		walk->did_hide = 1;
	else
		walk->did_push = 1;

	commit->uninteresting = uninteresting;
	list = walk->user_input;
	if (git_commit_list_insert(commit, &list) == NULL) {
		giterr_set_oom();
		return -1;
	}

	walk->user_input = list;

	return 0;
}

int git_revwalk_push(git_revwalk *walk, const git_oid *oid)
{
	assert(walk && oid);
	return push_commit(walk, oid, 0, false);
}


int git_revwalk_hide(git_revwalk *walk, const git_oid *oid)
{
	assert(walk && oid);
	return push_commit(walk, oid, 1, false);
}

static int push_ref(git_revwalk *walk, const char *refname, int hide, int from_glob)
{
	git_oid oid;

	if (git_reference_name_to_id(&oid, walk->repo, refname) < 0)
		return -1;

	return push_commit(walk, &oid, hide, from_glob);
}

static int push_glob(git_revwalk *walk, const char *glob, int hide)
{
	int error = 0;
	git_buf buf = GIT_BUF_INIT;
	git_reference *ref;
	git_reference_iterator *iter;
	size_t wildcard;

	assert(walk && glob);

	/* refs/ is implied if not given in the glob */
	if (git__prefixcmp(glob, GIT_REFS_DIR) != 0)
		git_buf_joinpath(&buf, GIT_REFS_DIR, glob);
	else
		git_buf_puts(&buf, glob);
	GITERR_CHECK_ALLOC_BUF(&buf);

	/* If no '?', '*' or '[' exist, we append '/ *' to the glob */
	wildcard = strcspn(glob, "?*[");
	if (!glob[wildcard])
		git_buf_put(&buf, "/*", 2);

	if ((error = git_reference_iterator_glob_new(&iter, walk->repo, buf.ptr)) < 0)
		goto out;

	while ((error = git_reference_next(&ref, iter)) == 0) {
		error = push_ref(walk, git_reference_name(ref), hide, true);
		git_reference_free(ref);
		if (error < 0)
			break;
	}
	git_reference_iterator_free(iter);

	if (error == GIT_ITEROVER)
		error = 0;
out:
	git_buf_free(&buf);
	return error;
}

int git_revwalk_push_glob(git_revwalk *walk, const char *glob)
{
	assert(walk && glob);
	return push_glob(walk, glob, 0);
}

int git_revwalk_hide_glob(git_revwalk *walk, const char *glob)
{
	assert(walk && glob);
	return push_glob(walk, glob, 1);
}

int git_revwalk_push_head(git_revwalk *walk)
{
	assert(walk);
	return push_ref(walk, GIT_HEAD_FILE, 0, false);
}

int git_revwalk_hide_head(git_revwalk *walk)
{
	assert(walk);
	return push_ref(walk, GIT_HEAD_FILE, 1, false);
}

int git_revwalk_push_ref(git_revwalk *walk, const char *refname)
{
	assert(walk && refname);
	return push_ref(walk, refname, 0, false);
}

int git_revwalk_push_range(git_revwalk *walk, const char *range)
{
	git_revspec revspec;
	int error = 0;

	if ((error = git_revparse(&revspec, walk->repo, range)))
		return error;

	if (revspec.flags & GIT_REVPARSE_MERGE_BASE) {
		/* TODO: support "<commit>...<commit>" */
		giterr_set(GITERR_INVALID, "symmetric differences not implemented in revwalk");
		return GIT_EINVALIDSPEC;
	}

	if ((error = push_commit(walk, git_object_id(revspec.from), 1, false)))
		goto out;

	error = push_commit(walk, git_object_id(revspec.to), 0, false);

out:
	git_object_free(revspec.from);
	git_object_free(revspec.to);
	return error;
}

int git_revwalk_hide_ref(git_revwalk *walk, const char *refname)
{
	assert(walk && refname);
	return push_ref(walk, refname, 1, false);
}

static int revwalk_enqueue_timesort(git_revwalk *walk, git_commit_list_node *commit)
{
	return git_pqueue_insert(&walk->iterator_time, commit);
}

static int revwalk_enqueue_unsorted(git_revwalk *walk, git_commit_list_node *commit)
{
	return git_commit_list_insert(commit, &walk->iterator_rand) ? 0 : -1;
}

static int revwalk_next_timesort(git_commit_list_node **object_out, git_revwalk *walk)
{
	git_commit_list_node *next;

	while ((next = git_pqueue_pop(&walk->iterator_time)) != NULL) {
		/* Some commits might become uninteresting after being added to the list */
		if (!next->uninteresting) {
			*object_out = next;
			return 0;
		}
	}

	giterr_clear();
	return GIT_ITEROVER;
}

static int revwalk_next_unsorted(git_commit_list_node **object_out, git_revwalk *walk)
{
	git_commit_list_node *next;

	while ((next = git_commit_list_pop(&walk->iterator_rand)) != NULL) {
		/* Some commits might become uninteresting after being added to the list */
		if (!next->uninteresting) {
			*object_out = next;
			return 0;
		}
	}

	giterr_clear();
	return GIT_ITEROVER;
}

static int revwalk_next_toposort(git_commit_list_node **object_out, git_revwalk *walk)
{
	git_commit_list_node *next;

	while ((next = git_commit_list_pop(&walk->iterator_topo)) != NULL) {
		/* Some commits might become uninteresting after being added to the list */
		if (!next->uninteresting) {
			*object_out = next;
			return 0;
		}
	}

	giterr_clear();
	return GIT_ITEROVER;
}

static int revwalk_next_reverse(git_commit_list_node **object_out, git_revwalk *walk)
{
	*object_out = git_commit_list_pop(&walk->iterator_reverse);
	return *object_out ? 0 : GIT_ITEROVER;
}

static void mark_parents_uninteresting(git_commit_list_node *commit)
{
	unsigned short i;
	git_commit_list *parents = NULL;

	for (i = 0; i < commit->out_degree; i++)
		git_commit_list_insert(commit->parents[i], &parents);


	while (parents) {
		commit = git_commit_list_pop(&parents);

		while (commit) {
			if (commit->uninteresting)
				break;

			commit->uninteresting = 1;
			/*
			 * If we've reached this commit some other way
			 * already, we need to mark its parents uninteresting
			 * as well.
			 */
			if (!commit->parents)
				break;

			for (i = 0; i < commit->out_degree; i++)
				git_commit_list_insert(commit->parents[i], &parents);
			commit = commit->parents[0];
		}
	}
}

static int add_parents_to_list(git_revwalk *walk, git_commit_list_node *commit, git_commit_list **list)
{
	unsigned short i;
	int error;

	if (commit->added)
		return 0;

	commit->added = 1;

	/*
	 * Go full on in the uninteresting case as we want to include
	 * as many of these as we can.
	 *
	 * Usually we haven't parsed the parent of a parent, but if we
	 * have it we reached it via other means so we want to mark
	 * its parents recursively too.
	 */
	if (commit->uninteresting) {
		for (i = 0; i < commit->out_degree; i++) {
			git_commit_list_node *p = commit->parents[i];
			p->uninteresting = 1;

			/* git does it gently here, but we don't like missing objects */
			if ((error = git_commit_list_parse(walk, p)) < 0)
				return error;

			if (p->parents)
				mark_parents_uninteresting(p);

			p->seen = 1;
			git_commit_list_insert_by_date(p, list);
		}

		return 0;
	}

	/*
	 * Now on to what we do if the commit is indeed
	 * interesting. Here we do want things like first-parent take
	 * effect as this is what we'll be showing.
	 */
	for (i = 0; i < commit->out_degree; i++) {
		git_commit_list_node *p = commit->parents[i];

		if ((error = git_commit_list_parse(walk, p)) < 0)
			return error;

		if (walk->hide_cb && walk->hide_cb(&p->oid, walk->hide_cb_payload))
			continue;

		if (!p->seen) {
			p->seen = 1;
			git_commit_list_insert_by_date(p, list);
		}

		if (walk->first_parent)
			break;
	}
	return 0;
}

static int everybody_uninteresting(git_commit_list *orig)
{
	git_commit_list *list = orig;

	while (list) {
		git_commit_list_node *commit = list->item;
		list = list->next;
		if (!commit->uninteresting)
			return 0;
	}

	return 1;
}

/* How many unintersting commits we want to look at after we run out of interesting ones */
#define SLOP 5

static int still_interesting(git_commit_list *list, int64_t time, int slop)
{
	/* The empty list is pretty boring */
	if (!list)
		return 0;

	/*
	 * If the destination list has commits with an earlier date
	 * than our source we want to continue looking.
	 */
	if (time <= list->item->time)
		return SLOP;

	/* If we find interesting commits, we reset the slop count */
	if (!everybody_uninteresting(list))
		return SLOP;

	/* Everything's uninteresting, reduce the count */
	return slop - 1;
}

static int limit_list(git_commit_list **out, git_revwalk *walk, git_commit_list *commits)
{
	int error, slop = SLOP;
	int64_t time = ~0ll;
	git_commit_list *list = commits;
	git_commit_list *newlist = NULL;
	git_commit_list **p = &newlist;

	while (list) {
		git_commit_list_node *commit = git_commit_list_pop(&list);

		if ((error = add_parents_to_list(walk, commit, &list)) < 0)
			return error;

		if (commit->uninteresting) {
			mark_parents_uninteresting(commit);

			slop = still_interesting(list, time, slop);
			if (slop)
				continue;

			break;
		}

		if (!commit->uninteresting && walk->hide_cb && walk->hide_cb(&commit->oid, walk->hide_cb_payload))
				continue;

		time = commit->time;
		p = &git_commit_list_insert(commit, p)->next;
	}

	git_commit_list_free(&list);
	*out = newlist;
	return 0;
}

static int sort_in_topological_order(git_commit_list **out, git_revwalk *walk, git_commit_list *list)
{
	git_commit_list *ll = NULL, *newlist, **pptr;
	git_commit_list_node *next;
	git_pqueue queue;
	git_vector_cmp queue_cmp = NULL;
	unsigned short i;
	int error;

	if (walk->sorting & GIT_SORT_TIME)
		queue_cmp = git_commit_list_time_cmp;

	if ((error = git_pqueue_init(&queue, 0, 8, queue_cmp)))
		return error;

	/*
	 * Start by resetting the in-degree to 1 for the commits in
	 * our list. We want to go through this list again, so we
	 * store it in the commit list as we extract it from the lower
	 * machinery.
	 */
	for (ll = list; ll; ll = ll->next) {
		ll->item->in_degree = 1;
	}

	/*
	 * Count up how many children each commit has. We limit
	 * ourselves to those commits in the original list (in-degree
	 * of 1) avoiding setting it for any parent that was hidden.
	 */
	for(ll = list; ll; ll = ll->next) {
		for (i = 0; i < ll->item->out_degree; ++i) {
			git_commit_list_node *parent = ll->item->parents[i];
			if (parent->in_degree)
				parent->in_degree++;
		}
	}

	/*
	 * Now we find the tips i.e. those not reachable from any other node
	 * i.e. those which still have an in-degree of 1.
	 */
	for(ll = list; ll; ll = ll->next) {
		if (ll->item->in_degree == 1) {
			if ((error = git_pqueue_insert(&queue, ll->item)))
				goto cleanup;
		}
	}

	/*
	 * We need to output the tips in the order that they came out of the
	 * traversal, so if we're not doing time-sorting, we need to reverse the
	 * pqueue in order to get them to come out as we inserted them.
	 */
	if ((walk->sorting & GIT_SORT_TIME) == 0)
		git_pqueue_reverse(&queue);


	pptr = &newlist;
	newlist = NULL;
	while ((next = git_pqueue_pop(&queue)) != NULL) {
		for (i = 0; i < next->out_degree; ++i) {
			git_commit_list_node *parent = next->parents[i];
			if (parent->in_degree == 0)
				continue;

			if (--parent->in_degree == 1) {
				if ((error = git_pqueue_insert(&queue, parent)))
					goto cleanup;
			}
		}

		/* All the children of 'item' have been emitted (since we got to it via the priority queue) */
		next->in_degree = 0;

		pptr = &git_commit_list_insert(next, pptr)->next;
	}

	*out = newlist;
	error = 0;

cleanup:
	git_pqueue_free(&queue);
	return error;
}

static int prepare_walk(git_revwalk *walk)
{
	int error;
	git_commit_list *list, *commits = NULL;
	git_commit_list_node *next;

	/* If there were no pushes, we know that the walk is already over */
	if (!walk->did_push) {
		giterr_clear();
		return GIT_ITEROVER;
	}

	for (list = walk->user_input; list; list = list->next) {
		git_commit_list_node *commit = list->item;
		if ((error = git_commit_list_parse(walk, commit)) < 0)
			return error;

		if (commit->uninteresting)
			mark_parents_uninteresting(commit);

		if (!commit->seen) {
			commit->seen = 1;
			git_commit_list_insert(commit, &commits);
		}
	}

	if ((error = limit_list(&commits, walk, commits)) < 0)
		return error;

	if (walk->sorting & GIT_SORT_TOPOLOGICAL) {
		error = sort_in_topological_order(&walk->iterator_topo, walk, commits);
		git_commit_list_free(&commits);

		if (error < 0)
			return error;

		walk->get_next = &revwalk_next_toposort;
	} else if (walk->sorting & GIT_SORT_TIME) {
		for (list = commits; list && !error; list = list->next)
			error = walk->enqueue(walk, list->item);

		git_commit_list_free(&commits);

		if (error < 0)
			return error;
	} else {
		walk->iterator_rand = commits;
		walk->get_next = revwalk_next_unsorted;
	}

	if (walk->sorting & GIT_SORT_REVERSE) {

		while ((error = walk->get_next(&next, walk)) == 0)
			if (git_commit_list_insert(next, &walk->iterator_reverse) == NULL)
				return -1;

		if (error != GIT_ITEROVER)
			return error;

		walk->get_next = &revwalk_next_reverse;
	}

	walk->walking = 1;
	return 0;
}


int git_revwalk_new(git_revwalk **revwalk_out, git_repository *repo)
{
	git_revwalk *walk = git__calloc(1, sizeof(git_revwalk));
	GITERR_CHECK_ALLOC(walk);

	walk->commits = git_oidmap_alloc();
	GITERR_CHECK_ALLOC(walk->commits);

	if (git_pqueue_init(&walk->iterator_time, 0, 8, git_commit_list_time_cmp) < 0)
		return -1;

	git_pool_init(&walk->commit_pool, COMMIT_ALLOC);
	walk->get_next = &revwalk_next_unsorted;
	walk->enqueue = &revwalk_enqueue_unsorted;

	walk->repo = repo;

	if (git_repository_odb(&walk->odb, repo) < 0) {
		git_revwalk_free(walk);
		return -1;
	}

	*revwalk_out = walk;
	return 0;
}

void git_revwalk_free(git_revwalk *walk)
{
	if (walk == NULL)
		return;

	git_revwalk_reset(walk);
	git_odb_free(walk->odb);

	git_oidmap_free(walk->commits);
	git_pool_clear(&walk->commit_pool);
	git_pqueue_free(&walk->iterator_time);
	git__free(walk);
}

git_repository *git_revwalk_repository(git_revwalk *walk)
{
	assert(walk);
	return walk->repo;
}

void git_revwalk_sorting(git_revwalk *walk, unsigned int sort_mode)
{
	assert(walk);

	if (walk->walking)
		git_revwalk_reset(walk);

	walk->sorting = sort_mode;

	if (walk->sorting & GIT_SORT_TIME) {
		walk->get_next = &revwalk_next_timesort;
		walk->enqueue = &revwalk_enqueue_timesort;
	} else {
		walk->get_next = &revwalk_next_unsorted;
		walk->enqueue = &revwalk_enqueue_unsorted;
	}
}

void git_revwalk_simplify_first_parent(git_revwalk *walk)
{
	walk->first_parent = 1;
}

int git_revwalk_next(git_oid *oid, git_revwalk *walk)
{
	int error;
	git_commit_list_node *next;

	assert(walk && oid);

	if (!walk->walking) {
		if ((error = prepare_walk(walk)) < 0)
			return error;
	}

	error = walk->get_next(&next, walk);

	if (error == GIT_ITEROVER) {
		git_revwalk_reset(walk);
		giterr_clear();
		return GIT_ITEROVER;
	}

	if (!error)
		git_oid_cpy(oid, &next->oid);

	return error;
}

void git_revwalk_reset(git_revwalk *walk)
{
	git_commit_list_node *commit;

	assert(walk);

	git_oidmap_foreach_value(walk->commits, commit, {
		commit->seen = 0;
		commit->in_degree = 0;
		commit->topo_delay = 0;
		commit->uninteresting = 0;
		commit->added = 0;
		commit->flags = 0;
		});

	git_pqueue_clear(&walk->iterator_time);
	git_commit_list_free(&walk->iterator_topo);
	git_commit_list_free(&walk->iterator_rand);
	git_commit_list_free(&walk->iterator_reverse);
	git_commit_list_free(&walk->user_input);
	walk->first_parent = 0;
	walk->walking = 0;
	walk->did_push = walk->did_hide = 0;
}

int git_revwalk_add_hide_cb(
	git_revwalk *walk,
	git_revwalk_hide_cb hide_cb,
	void *payload)
{
	assert(walk);

	if (walk->walking)
		git_revwalk_reset(walk);

	if (walk->hide_cb) {
		/* There is already a callback added */
		giterr_set(GITERR_INVALID, "there is already a callback added to hide commits in revwalk");
		return -1;
	}

	walk->hide_cb = hide_cb;
	walk->hide_cb_payload = payload;

	return 0;
}

