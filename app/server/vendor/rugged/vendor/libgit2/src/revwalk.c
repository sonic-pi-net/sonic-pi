/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "commit.h"
#include "odb.h"
#include "pool.h"

#include "revwalk.h"
#include "git2/revparse.h"
#include "merge.h"

git_commit_list_node *git_revwalk__commit_lookup(
	git_revwalk *walk, const git_oid *oid)
{
	git_commit_list_node *commit;
	khiter_t pos;
	int ret;

	/* lookup and reserve space if not already present */
	pos = kh_get(oid, walk->commits, oid);
	if (pos != kh_end(walk->commits))
		return kh_value(walk->commits, pos);

	commit = git_commit_list_alloc_node(walk);
	if (commit == NULL)
		return NULL;

	git_oid_cpy(&commit->oid, oid);

	pos = kh_put(oid, walk->commits, &commit->oid, &ret);
	assert(ret != 0);
	kh_value(walk->commits, pos) = commit;

	return commit;
}

static int mark_uninteresting(git_revwalk *walk, git_commit_list_node *commit)
{
	int error;
	unsigned short i;
	git_array_t(git_commit_list_node *) pending = GIT_ARRAY_INIT;
	git_commit_list_node **tmp;

	assert(commit);

	git_array_alloc(pending);
	GITERR_CHECK_ARRAY(pending);

	do {
		commit->uninteresting = 1;

		if ((error = git_commit_list_parse(walk, commit)) < 0)
			return error;

		for (i = 0; i < commit->out_degree; ++i)
			if (!commit->parents[i]->uninteresting) {
				git_commit_list_node **node = git_array_alloc(pending);
				GITERR_CHECK_ALLOC(node);
				*node = commit->parents[i];
			}

		tmp = git_array_pop(pending);
		commit = tmp ? *tmp : NULL;

	} while (git_array_size(pending) > 0);

	git_array_clear(pending);

	return 0;
}

static int process_commit(git_revwalk *walk, git_commit_list_node *commit, int hide)
{
	int error;

	if (!hide && walk->hide_cb)
		hide = walk->hide_cb(&commit->oid, walk->hide_cb_payload);

	if (hide && mark_uninteresting(walk, commit) < 0)
		return -1;

	if (commit->seen)
		return 0;

	commit->seen = 1;

	if ((error = git_commit_list_parse(walk, commit)) < 0)
		return error;

	if (!hide)
		return walk->enqueue(walk, commit);

	return 0;
}

static int process_commit_parents(git_revwalk *walk, git_commit_list_node *commit)
{
	unsigned short i, max;
	int error = 0;

	max = commit->out_degree;
	if (walk->first_parent && commit->out_degree)
		max = 1;

	for (i = 0; i < max && !error; ++i)
		error = process_commit(walk, commit->parents[i], commit->uninteresting);

	return error;
}

static int push_commit(git_revwalk *walk, const git_oid *oid, int uninteresting, int from_glob)
{
	git_oid commit_id;
	int error;
	git_object *obj, *oobj;
	git_commit_list_node *commit;

	if ((error = git_object_lookup(&oobj, walk->repo, oid, GIT_OBJ_ANY)) < 0)
		return error;

	error = git_object_peel(&obj, oobj, GIT_OBJ_COMMIT);
	git_object_free(oobj);

	if (error == GIT_ENOTFOUND) {
		/* If this comes from e.g. push_glob("tags"), ignore this */
		if (from_glob)
			return 0;

		giterr_set(GITERR_INVALID, "Object is not a committish");
		return -1;
	}
	if (error < 0)
		return error;

	git_oid_cpy(&commit_id, git_object_id(obj));
	git_object_free(obj);

	commit = git_revwalk__commit_lookup(walk, &commit_id);
	if (commit == NULL)
		return -1; /* error already reported by failed lookup */

	commit->uninteresting = uninteresting;
	if (walk->one == NULL && !uninteresting) {
		walk->one = commit;
	} else {
		if (git_vector_insert(&walk->twos, commit) < 0)
			return -1;
	}

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
	if (git_buf_oom(&buf))
		return -1;

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
		giterr_set(GITERR_INVALID, "Symmetric differences not implemented in revwalk");
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
	int error;
	git_commit_list_node *next;

	while ((next = git_pqueue_pop(&walk->iterator_time)) != NULL)
		if (!next->uninteresting) {
			if ((error = process_commit_parents(walk, next)) < 0)
				return error;

			*object_out = next;
			return 0;
		}

	giterr_clear();
	return GIT_ITEROVER;
}

static int revwalk_next_unsorted(git_commit_list_node **object_out, git_revwalk *walk)
{
	int error;
	git_commit_list_node *next;

	while ((next = git_commit_list_pop(&walk->iterator_rand)) != NULL)
		if (!next->uninteresting) {
			if ((error = process_commit_parents(walk, next)) < 0)
				return error;

			*object_out = next;
			return 0;
		}

	giterr_clear();
	return GIT_ITEROVER;
}

static int revwalk_next_toposort(git_commit_list_node **object_out, git_revwalk *walk)
{
	git_commit_list_node *next;
	unsigned short i, max;

	for (;;) {
		next = git_commit_list_pop(&walk->iterator_topo);
		if (next == NULL) {
			giterr_clear();
			return GIT_ITEROVER;
		}

		if (next->in_degree > 0) {
			next->topo_delay = 1;
			continue;
		}


		max = next->out_degree;
		if (walk->first_parent && next->out_degree)
			max = 1;

		for (i = 0; i < max; ++i) {
			git_commit_list_node *parent = next->parents[i];

			if (--parent->in_degree == 0 && parent->topo_delay) {
				parent->topo_delay = 0;
				if (git_commit_list_insert(parent, &walk->iterator_topo) == NULL)
					return -1;
			}
		}

		*object_out = next;
		return 0;
	}
}

static int revwalk_next_reverse(git_commit_list_node **object_out, git_revwalk *walk)
{
	*object_out = git_commit_list_pop(&walk->iterator_reverse);
	return *object_out ? 0 : GIT_ITEROVER;
}


static int prepare_walk(git_revwalk *walk)
{
	int error;
	unsigned int i;
	git_commit_list_node *next, *two;

	/*
	 * If walk->one is NULL, there were no positive references,
	 * so we know that the walk is already over.
	 */
	if (walk->one == NULL) {
		giterr_clear();
		return GIT_ITEROVER;
	}

	if (process_commit(walk, walk->one, walk->one->uninteresting) < 0)
		return -1;

	git_vector_foreach(&walk->twos, i, two) {
		if (process_commit(walk, two, two->uninteresting) < 0)
			return -1;
	}

	if (walk->sorting & GIT_SORT_TOPOLOGICAL) {
		unsigned short i;

		while ((error = walk->get_next(&next, walk)) == 0) {
			for (i = 0; i < next->out_degree; ++i) {
				git_commit_list_node *parent = next->parents[i];
				parent->in_degree++;
			}

			if (git_commit_list_insert(next, &walk->iterator_topo) == NULL)
				return -1;
		}

		if (error != GIT_ITEROVER)
			return error;

		walk->get_next = &revwalk_next_toposort;
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
	git_revwalk *walk;

	walk = git__malloc(sizeof(git_revwalk));
	GITERR_CHECK_ALLOC(walk);

	memset(walk, 0x0, sizeof(git_revwalk));

	walk->commits = git_oidmap_alloc();
	GITERR_CHECK_ALLOC(walk->commits);

	if (git_pqueue_init(
			&walk->iterator_time, 0, 8, git_commit_list_time_cmp) < 0 ||
		git_vector_init(&walk->twos, 4, NULL) < 0 ||
		git_pool_init(&walk->commit_pool, 1,
			git_pool__suggest_items_per_page(COMMIT_ALLOC) * COMMIT_ALLOC) < 0)
		return -1;

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
	git_vector_free(&walk->twos);
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

	kh_foreach_value(walk->commits, commit, {
		commit->seen = 0;
		commit->in_degree = 0;
		commit->topo_delay = 0;
		commit->uninteresting = 0;
		});

	git_pqueue_clear(&walk->iterator_time);
	git_commit_list_free(&walk->iterator_topo);
	git_commit_list_free(&walk->iterator_rand);
	git_commit_list_free(&walk->iterator_reverse);
	walk->walking = 0;

	walk->one = NULL;
	git_vector_clear(&walk->twos);
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
		giterr_set(GITERR_INVALID, "There is already a callback added to hide commits in revision walker.");
		return -1;
	}

	walk->hide_cb = hide_cb;
	walk->hide_cb_payload = payload;

	return 0;
}

