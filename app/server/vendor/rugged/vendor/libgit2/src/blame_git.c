/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "blame_git.h"
#include "commit.h"
#include "blob.h"
#include "xdiff/xinclude.h"

/*
 * Origin is refcounted and usually we keep the blob contents to be
 * reused.
 */
static git_blame__origin *origin_incref(git_blame__origin *o)
{
	if (o)
		o->refcnt++;
	return o;
}

static void origin_decref(git_blame__origin *o)
{
	if (o && --o->refcnt <= 0) {
		if (o->previous)
			origin_decref(o->previous);
		git_blob_free(o->blob);
		git_commit_free(o->commit);
		git__free(o);
	}
}

/* Given a commit and a path in it, create a new origin structure. */
static int make_origin(git_blame__origin **out, git_commit *commit, const char *path)
{
	int error = 0;
	git_blame__origin *o;

	o = git__calloc(1, sizeof(*o) + strlen(path) + 1);
	GITERR_CHECK_ALLOC(o);
	o->commit = commit;
	o->refcnt = 1;
	strcpy(o->path, path);

	if (!(error = git_object_lookup_bypath((git_object**)&o->blob, (git_object*)commit,
			path, GIT_OBJ_BLOB))) {
		*out = o;
	} else {
		origin_decref(o);
	}
	return error;
}

/* Locate an existing origin or create a new one. */
int git_blame__get_origin(
		git_blame__origin **out,
		git_blame *blame,
		git_commit *commit,
		const char *path)
{
	git_blame__entry *e;

	for (e = blame->ent; e; e = e->next) {
		if (e->suspect->commit == commit && !strcmp(e->suspect->path, path)) {
			*out = origin_incref(e->suspect);
		}
	}
	return make_origin(out, commit, path);
}

typedef struct blame_chunk_cb_data {
	git_blame *blame;
	git_blame__origin *target;
	git_blame__origin *parent;
	long tlno;
	long plno;
}blame_chunk_cb_data;

static bool same_suspect(git_blame__origin *a, git_blame__origin *b)
{
	if (a == b)
		return true;
	if (git_oid_cmp(git_commit_id(a->commit), git_commit_id(b->commit)))
		return false;
	return 0 == strcmp(a->path, b->path);
}

/* find the line number of the last line the target is suspected for */
static int find_last_in_target(git_blame *blame, git_blame__origin *target)
{
	git_blame__entry *e;
	int last_in_target = -1;

	for (e=blame->ent; e; e=e->next) {
		if (e->guilty || !same_suspect(e->suspect, target))
			continue;
		if (last_in_target < e->s_lno + e->num_lines)
			last_in_target = e->s_lno + e->num_lines;
	}
	return last_in_target;
}

/*
 * It is known that lines between tlno to same came from parent, and e
 * has an overlap with that range.  it also is known that parent's
 * line plno corresponds to e's line tlno.
 *
 *                <---- e ----->
 *                   <------>         (entirely within)
 *                   <------------>   (extends past)
 *             <------------>         (starts before)
 *             <------------------>   (entirely encloses)
 *
 * Split e into potentially three parts; before this chunk, the chunk
 * to be blamed for the parent, and after that portion.
 */
static void split_overlap(git_blame__entry *split, git_blame__entry *e,
		int tlno, int plno, int same, git_blame__origin *parent)
{
	int chunk_end_lno;

	if (e->s_lno < tlno) {
		/* there is a pre-chunk part not blamed on the parent */
		split[0].suspect = origin_incref(e->suspect);
		split[0].lno = e->lno;
		split[0].s_lno = e->s_lno;
		split[0].num_lines = tlno - e->s_lno;
		split[1].lno = e->lno + tlno - e->s_lno;
		split[1].s_lno = plno;
	} else {
		split[1].lno = e->lno;
		split[1].s_lno = plno + (e->s_lno - tlno);
	}

	if (same < e->s_lno + e->num_lines) {
		/* there is a post-chunk part not blamed on parent */
		split[2].suspect = origin_incref(e->suspect);
		split[2].lno = e->lno + (same - e->s_lno);
		split[2].s_lno = e->s_lno + (same - e->s_lno);
		split[2].num_lines = e->s_lno + e->num_lines - same;
		chunk_end_lno = split[2].lno;
	} else {
		chunk_end_lno = e->lno + e->num_lines;
	}
	split[1].num_lines = chunk_end_lno - split[1].lno;

	/*
	 * if it turns out there is nothing to blame the parent for, forget about
	 * the splitting. !split[1].suspect signals this.
	 */
	if (split[1].num_lines < 1)
		return;
	split[1].suspect = origin_incref(parent);
}

/*
 * Link in a new blame entry to the scoreboard. Entries that cover the same
 * line range have been removed from the scoreboard previously.
 */
static void add_blame_entry(git_blame *blame, git_blame__entry *e)
{
	git_blame__entry *ent, *prev = NULL;

	origin_incref(e->suspect);

	for (ent = blame->ent; ent && ent->lno < e->lno; ent = ent->next)
		prev = ent;

	/* prev, if not NULL, is the last one that is below e */
	e->prev = prev;
	if (prev) {
		e->next = prev->next;
		prev->next = e;
	} else {
		e->next = blame->ent;
		blame->ent = e;
	}
	if (e->next)
		e->next->prev = e;
}

/*
 * src typically is on-stack; we want to copy the information in it to
 * a malloced blame_entry that is already on the linked list of the scoreboard.
 * The origin of dst loses a refcnt while the origin of src gains one.
 */
static void dup_entry(git_blame__entry *dst, git_blame__entry *src)
{
	git_blame__entry *p, *n;

	p = dst->prev;
	n = dst->next;
	origin_incref(src->suspect);
	origin_decref(dst->suspect);
	memcpy(dst, src, sizeof(*src));
	dst->prev = p;
	dst->next = n;
	dst->score = 0;
}

/*
 * split_overlap() divided an existing blame e into up to three parts in split.
 * Adjust the linked list of blames in the scoreboard to reflect the split.
 */
static void split_blame(git_blame *blame, git_blame__entry *split, git_blame__entry *e)
{
	git_blame__entry *new_entry;

	if (split[0].suspect && split[2].suspect) {
		/* The first part (reuse storage for the existing entry e */
		dup_entry(e, &split[0]);

		/* The last part -- me */
		new_entry = git__malloc(sizeof(*new_entry));
		memcpy(new_entry, &(split[2]), sizeof(git_blame__entry));
		add_blame_entry(blame, new_entry);

		/* ... and the middle part -- parent */
		new_entry = git__malloc(sizeof(*new_entry));
		memcpy(new_entry, &(split[1]), sizeof(git_blame__entry));
		add_blame_entry(blame, new_entry);
	} else if (!split[0].suspect && !split[2].suspect) {
		/*
		 * The parent covers the entire area; reuse storage for e and replace it
		 * with the parent
		 */
		dup_entry(e, &split[1]);
	} else if (split[0].suspect) {
		/* me and then parent */
		dup_entry(e, &split[0]);
		new_entry = git__malloc(sizeof(*new_entry));
		memcpy(new_entry, &(split[1]), sizeof(git_blame__entry));
		add_blame_entry(blame, new_entry);
	} else {
		/* parent and then me */
		dup_entry(e, &split[1]);
		new_entry = git__malloc(sizeof(*new_entry));
		memcpy(new_entry, &(split[2]), sizeof(git_blame__entry));
		add_blame_entry(blame, new_entry);
	}
}

/* 
 * After splitting the blame, the origins used by the on-stack blame_entry
 * should lose one refcnt each.
 */
static void decref_split(git_blame__entry *split)
{
	int i;
	for (i=0; i<3; i++)
		origin_decref(split[i].suspect);
}

/*
 * Helper for blame_chunk(). blame_entry e is known to overlap with the patch
 * hunk; split it and pass blame to the parent.
 */
static void blame_overlap(
		git_blame *blame,
		git_blame__entry *e,
		int tlno,
		int plno,
		int same,
		git_blame__origin *parent)
{
	git_blame__entry split[3] = {{0}};

	split_overlap(split, e, tlno, plno, same, parent);
	if (split[1].suspect)
		split_blame(blame, split, e);
	decref_split(split);
}

/*
 * Process one hunk from the patch between the current suspect for blame_entry
 * e and its parent. Find and split the overlap, and pass blame to the
 * overlapping part to the parent.
 */
static void blame_chunk(
		git_blame *blame,
		int tlno,
		int plno,
		int same,
		git_blame__origin *target,
		git_blame__origin *parent)
{
	git_blame__entry *e;

	for (e = blame->ent; e; e = e->next) {
		if (e->guilty || !same_suspect(e->suspect, target))
			continue;
		if (same <= e->s_lno)
			continue;
		if (tlno < e->s_lno + e->num_lines) {
			blame_overlap(blame, e, tlno, plno, same, parent);
		}
	}
}

static int my_emit(
		xdfenv_t *xe,
		xdchange_t *xscr,
		xdemitcb_t *ecb,
		xdemitconf_t const *xecfg)
{
	xdchange_t *xch = xscr;
	GIT_UNUSED(xe);
	GIT_UNUSED(xecfg);
	while (xch) {
		blame_chunk_cb_data *d = ecb->priv;
		blame_chunk(d->blame, d->tlno, d->plno, xch->i2, d->target, d->parent);
		d->plno = xch->i1 + xch->chg1;
		d->tlno = xch->i2 + xch->chg2;
		xch = xch->next;
	}
	return 0;
}

static void trim_common_tail(mmfile_t *a, mmfile_t *b, long ctx)
{
	const int blk = 1024;
	long trimmed = 0, recovered = 0;
	char *ap = a->ptr + a->size;
	char *bp = b->ptr + b->size;
	long smaller = (long)((a->size < b->size) ? a->size : b->size);

	if (ctx)
		return;

	while (blk + trimmed <= smaller && !memcmp(ap - blk, bp - blk, blk)) {
		trimmed += blk;
		ap -= blk;
		bp -= blk;
	}

	while (recovered < trimmed)
		if (ap[recovered++] == '\n')
			break;
	a->size -= trimmed - recovered;
	b->size -= trimmed - recovered;
}

static int diff_hunks(mmfile_t file_a, mmfile_t file_b, void *cb_data)
{
	xpparam_t xpp = {0};
	xdemitconf_t xecfg = {0};
	xdemitcb_t ecb = {0};

	xecfg.emit_func = (void(*)(void))my_emit;
	ecb.priv = cb_data;

	trim_common_tail(&file_a, &file_b, 0);
	return xdl_diff(&file_a, &file_b, &xpp, &xecfg, &ecb);
}

static void fill_origin_blob(git_blame__origin *o, mmfile_t *file)
{
	memset(file, 0, sizeof(*file));
	if (o->blob) {
		file->ptr = (char*)git_blob_rawcontent(o->blob);
		file->size = (size_t)git_blob_rawsize(o->blob);
	}
}

static int pass_blame_to_parent(
		git_blame *blame,
		git_blame__origin *target,
		git_blame__origin *parent)
{
	int last_in_target;
	mmfile_t file_p, file_o;
	blame_chunk_cb_data d = { blame, target, parent, 0, 0 };

	last_in_target = find_last_in_target(blame, target);
	if (last_in_target < 0)
		return 1; /* nothing remains for this target */

	fill_origin_blob(parent, &file_p);
	fill_origin_blob(target, &file_o);

	diff_hunks(file_p, file_o, &d);
	/* The reset (i.e. anything after tlno) are the same as the parent */
	blame_chunk(blame, d.tlno, d.plno, last_in_target, target, parent);

	return 0;
}

static int paths_on_dup(void **old, void *new)
{
	GIT_UNUSED(old);
	git__free(new);
	return -1;
}

static git_blame__origin* find_origin(
		git_blame *blame,
		git_commit *parent,
		git_blame__origin *origin)
{
	git_blame__origin *porigin = NULL;
	git_diff *difflist = NULL;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_tree *otree=NULL, *ptree=NULL;

	/* Get the trees from this commit and its parent */
	if (0 != git_commit_tree(&otree, origin->commit) ||
	    0 != git_commit_tree(&ptree, parent))
		goto cleanup;

	/* Configure the diff */
	diffopts.context_lines = 0;
	diffopts.flags = GIT_DIFF_SKIP_BINARY_CHECK;

	/* Check to see if files we're interested have changed */
	diffopts.pathspec.count = blame->paths.length;
	diffopts.pathspec.strings = (char**)blame->paths.contents;
	if (0 != git_diff_tree_to_tree(&difflist, blame->repository, ptree, otree, &diffopts))
			goto cleanup;

	if (!git_diff_num_deltas(difflist)) {
		/* No changes; copy data */
		git_blame__get_origin(&porigin, blame, parent, origin->path);
	} else {
		git_diff_find_options findopts = GIT_DIFF_FIND_OPTIONS_INIT;
		int i;

		/* Generate a full diff between the two trees */
		git_diff_free(difflist);
		diffopts.pathspec.count = 0;
		if (0 != git_diff_tree_to_tree(&difflist, blame->repository, ptree, otree, &diffopts))
			goto cleanup;

		/* Let diff find renames */
		findopts.flags = GIT_DIFF_FIND_RENAMES;
		if (0 != git_diff_find_similar(difflist, &findopts))
			goto cleanup;

		/* Find one that matches */
		for (i=0; i<(int)git_diff_num_deltas(difflist); i++) {
			const git_diff_delta *delta = git_diff_get_delta(difflist, i);

			if (!git_vector_bsearch(NULL, &blame->paths, delta->new_file.path))
			{
				git_vector_insert_sorted(&blame->paths, (void*)git__strdup(delta->old_file.path),
						paths_on_dup);
				make_origin(&porigin, parent, delta->old_file.path);
			}
		}
	}

cleanup:
	git_diff_free(difflist);
	git_tree_free(otree);
	git_tree_free(ptree);
	return porigin;
}

/*
 * The blobs of origin and porigin exactly match, so everything origin is
 * suspected for can be blamed on the parent.
 */
static void pass_whole_blame(git_blame *blame,
		git_blame__origin *origin, git_blame__origin *porigin)
{
	git_blame__entry *e;

	if (!porigin->blob)
		git_object_lookup((git_object**)&porigin->blob, blame->repository,
				git_blob_id(origin->blob), GIT_OBJ_BLOB);
	for (e=blame->ent; e; e=e->next) {
		if (!same_suspect(e->suspect, origin))
			continue;
		origin_incref(porigin);
		origin_decref(e->suspect);
		e->suspect = porigin;
	}
}

static void pass_blame(git_blame *blame, git_blame__origin *origin, uint32_t opt)
{
	git_commit *commit = origin->commit;
	int i, num_parents;
	git_blame__origin *sg_buf[16];
	git_blame__origin *porigin, **sg_origin = sg_buf;

	num_parents = git_commit_parentcount(commit);
	if (!git_oid_cmp(git_commit_id(commit), &blame->options.oldest_commit))
		/* Stop at oldest specified commit */
		num_parents = 0;
	else if (opt & GIT_BLAME_FIRST_PARENT && num_parents > 1)
		/* Limit search to the first parent */
		num_parents = 1;

	if (!num_parents) {
		git_oid_cpy(&blame->options.oldest_commit, git_commit_id(commit));
		goto finish;
	}
	else if (num_parents < (int)ARRAY_SIZE(sg_buf))
		memset(sg_buf, 0, sizeof(sg_buf));
	else
		sg_origin = git__calloc(num_parents, sizeof(*sg_origin));

	for (i=0; i<num_parents; i++) {
		git_commit *p;
		int j, same;

		if (sg_origin[i])
			continue;

		git_commit_parent(&p, origin->commit, i);
		porigin = find_origin(blame, p, origin);

		if (!porigin)
			continue;
		if (porigin->blob && origin->blob &&
		    !git_oid_cmp(git_blob_id(porigin->blob), git_blob_id(origin->blob))) {
			pass_whole_blame(blame, origin, porigin);
			origin_decref(porigin);
			goto finish;
		}
		for (j = same = 0; j<i; j++)
			if (sg_origin[j] &&
				 !git_oid_cmp(git_blob_id(sg_origin[j]->blob), git_blob_id(porigin->blob))) {
				same = 1;
				break;
			}
		if (!same)
			sg_origin[i] = porigin;
		else
			origin_decref(porigin);
	}

	/* Standard blame */
	for (i=0; i<num_parents; i++) {
		git_blame__origin *porigin = sg_origin[i];
		if (!porigin)
			continue;
		if (!origin->previous) {
			origin_incref(porigin);
			origin->previous = porigin;
		}
		if (pass_blame_to_parent(blame, origin, porigin))
			goto finish;
	}

	/* TODO: optionally find moves in parents' files */

	/* TODO: optionally find copies in parents' files */

finish:
	for (i=0; i<num_parents; i++)
		if (sg_origin[i])
			origin_decref(sg_origin[i]);
	if (sg_origin != sg_buf)
		git__free(sg_origin);
	return;
}

/*
 * If two blame entries that are next to each other came from
 * contiguous lines in the same origin (i.e. <commit, path> pair),
 * merge them together.
 */
static void coalesce(git_blame *blame)
{
	git_blame__entry *ent, *next;

	for (ent=blame->ent; ent && (next = ent->next); ent = next) {
		if (same_suspect(ent->suspect, next->suspect) &&
		    ent->guilty == next->guilty &&
		    ent->s_lno + ent->num_lines == next->s_lno)
		{
			ent->num_lines += next->num_lines;
			ent->next = next->next;
			if (ent->next)
				ent->next->prev = ent;
			origin_decref(next->suspect);
			git__free(next);
			ent->score = 0;
			next = ent; /* again */
		}
	}
}

void git_blame__like_git(git_blame *blame, uint32_t opt)
{
	while (true) {
		git_blame__entry *ent;
		git_blame__origin *suspect = NULL;

		/* Find a suspect to break down */
		for (ent = blame->ent; !suspect && ent; ent = ent->next)
			if (!ent->guilty)
				suspect = ent->suspect;
		if (!suspect)
			return; /* all done */

		/* We'll use this suspect later in the loop, so hold on to it for now. */
		origin_incref(suspect);
		pass_blame(blame, suspect, opt);

		/* Take responsibility for the remaining entries */
		for (ent = blame->ent; ent; ent = ent->next) {
			if (same_suspect(ent->suspect, suspect)) {
				ent->guilty = true;
				ent->is_boundary = !git_oid_cmp(
						git_commit_id(suspect->commit),
						&blame->options.oldest_commit);
			}
		}
		origin_decref(suspect);
	}

	coalesce(blame);
}

void git_blame__free_entry(git_blame__entry *ent)
{
	if (!ent) return;
	origin_decref(ent->suspect);
	git__free(ent);
}
