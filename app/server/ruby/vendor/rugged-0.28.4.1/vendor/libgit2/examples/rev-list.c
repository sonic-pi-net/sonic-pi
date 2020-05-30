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

#include <assert.h>

static int revwalk_parse_options(git_sort_t *sort, struct args_info *args);
static int revwalk_parse_revs(git_repository *repo, git_revwalk *walk, struct args_info *args);

int lg2_rev_list(git_repository *repo, int argc, char **argv)
{
	struct args_info args = ARGS_INFO_INIT;
	git_revwalk *walk;
	git_oid oid;
	git_sort_t sort;
	char buf[GIT_OID_HEXSZ+1];

	check_lg2(revwalk_parse_options(&sort, &args), "parsing options", NULL);

	check_lg2(git_revwalk_new(&walk, repo), "allocating revwalk", NULL);
	git_revwalk_sorting(walk, sort);
	check_lg2(revwalk_parse_revs(repo, walk, &args), "parsing revs", NULL);

	while (!git_revwalk_next(&oid, walk)) {
		git_oid_fmt(buf, &oid);
		buf[GIT_OID_HEXSZ] = '\0';
		printf("%s\n", buf);
	}

	git_revwalk_free(walk);
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

static void print_usage(void)
{
	fprintf(stderr, "rev-list [--git-dir=dir] [--topo-order|--date-order] [--reverse] <revspec>\n");
	exit(-1);
}

static int revwalk_parse_options(git_sort_t *sort, struct args_info *args)
{
	assert(sort && args);
	*sort = GIT_SORT_NONE;

	if (args->argc < 1)
		print_usage();

	for (args->pos = 1; args->pos < args->argc; ++args->pos) {
		const char *curr = args->argv[args->pos];

		if (!strcmp(curr, "--topo-order")) {
			*sort |= GIT_SORT_TOPOLOGICAL;
		} else if (!strcmp(curr, "--date-order")) {
			*sort |= GIT_SORT_TIME;
		} else if (!strcmp(curr, "--reverse")) {
			*sort |= (*sort & ~GIT_SORT_REVERSE) ^ GIT_SORT_REVERSE;
		} else {
			break;
		}
	}
	return 0;
}

static int revwalk_parse_revs(git_repository *repo, git_revwalk *walk, struct args_info *args)
{
	int hide, error;
	git_oid oid;

	hide = 0;
	for (; args->pos < args->argc; ++args->pos) {
		const char *curr = args->argv[args->pos];

		if (!strcmp(curr, "--not")) {
			hide = !hide;
		} else if (curr[0] == '^') {
			if ((error = push_spec(repo, walk, curr + 1, !hide)))
				return error;
		} else if (strstr(curr, "..")) {
			if ((error = push_range(repo, walk, curr, hide)))
				return error;
		} else {
			if (push_spec(repo, walk, curr, hide) == 0)
				continue;

			if ((error = git_oid_fromstr(&oid, curr)))
				return error;
			if ((error = push_commit(walk, &oid, hide)))
				return error;
		}
	}

	return 0;
}

