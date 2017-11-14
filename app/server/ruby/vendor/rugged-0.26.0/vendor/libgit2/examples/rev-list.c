/*
 * libgit2 "rev-list" example - shows how to transform a rev-spec into a list
 * of commit ids
 *
 * Written by the libgit2 contributors
 *
 * To the extent possible under law, the author(s) have dedicated all copyright
 * and related and neighboring rights to this software to the public domain
 * worldwide. This software is distributed without any warranty.
 *
 * You should have received a copy of the CC0 Public Domain Dedication along
 * with this software. If not, see
 * <http://creativecommons.org/publicdomain/zero/1.0/>.
 */

#include "common.h"

static int revwalk_parseopts(git_repository *repo, git_revwalk *walk, int nopts, char **opts);

int main (int argc, char **argv)
{
	git_repository *repo;
	git_revwalk *walk;
	git_oid oid;
	char buf[GIT_OID_HEXSZ+1];

	git_libgit2_init();

	check_lg2(git_repository_open_ext(&repo, ".", 0, NULL), "opening repository", NULL);
	check_lg2(git_revwalk_new(&walk, repo), "allocating revwalk", NULL);
	check_lg2(revwalk_parseopts(repo, walk, argc-1, argv+1), "parsing options", NULL);

	while (!git_revwalk_next(&oid, walk)) {
		git_oid_fmt(buf, &oid);
		buf[GIT_OID_HEXSZ] = '\0';
		printf("%s\n", buf);
	}

	git_libgit2_shutdown();
	return 0;
}

static int push_commit(git_revwalk *walk, const git_oid *oid, int hide)
{
	if (hide)
		return git_revwalk_hide(walk, oid);
	else
		return git_revwalk_push(walk, oid);
}

static int push_spec(git_repository *repo, git_revwalk *walk, const char *spec, int hide)
{
	int error;
	git_object *obj;

	if ((error = git_revparse_single(&obj, repo, spec)) < 0)
		return error;

	error = push_commit(walk, git_object_id(obj), hide);
	git_object_free(obj);
	return error;
}

static int push_range(git_repository *repo, git_revwalk *walk, const char *range, int hide)
{
	git_revspec revspec;
	int error = 0;

	if ((error = git_revparse(&revspec, repo, range)))
		return error;

	if (revspec.flags & GIT_REVPARSE_MERGE_BASE) {
		/* TODO: support "<commit>...<commit>" */
		return GIT_EINVALIDSPEC;
	}

	if ((error = push_commit(walk, git_object_id(revspec.from), !hide)))
		goto out;

	error = push_commit(walk, git_object_id(revspec.to), hide);

out:
	git_object_free(revspec.from);
	git_object_free(revspec.to);
	return error;
}

static int revwalk_parseopts(git_repository *repo, git_revwalk *walk, int nopts, char **opts)
{
	int hide, i, error;
	unsigned int sorting = GIT_SORT_NONE;

	hide = 0;
	for (i = 0; i < nopts; i++) {
		if (!strcmp(opts[i], "--topo-order")) {
			sorting = GIT_SORT_TOPOLOGICAL | (sorting & GIT_SORT_REVERSE);
			git_revwalk_sorting(walk, sorting);
		} else if (!strcmp(opts[i], "--date-order")) {
			sorting = GIT_SORT_TIME | (sorting & GIT_SORT_REVERSE);
			git_revwalk_sorting(walk, sorting);
		} else if (!strcmp(opts[i], "--reverse")) {
			sorting = (sorting & ~GIT_SORT_REVERSE)
			    | ((sorting & GIT_SORT_REVERSE) ? 0 : GIT_SORT_REVERSE);
			git_revwalk_sorting(walk, sorting);
		} else if (!strcmp(opts[i], "--not")) {
			hide = !hide;
		} else if (opts[i][0] == '^') {
			if ((error = push_spec(repo, walk, opts[i] + 1, !hide)))
				return error;
		} else if (strstr(opts[i], "..")) {
			if ((error = push_range(repo, walk, opts[i], hide)))
				return error;
		} else {
			if ((error = push_spec(repo, walk, opts[i], hide)))
				return error;
		}
	}

	return 0;
}

