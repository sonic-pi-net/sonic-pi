/*
 * libgit2 "rev-parse" example - shows how to parse revspecs
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

/** Forward declarations for helpers. */
struct parse_state {
	const char *repodir;
	const char *spec;
	int not;
};
static void parse_opts(struct parse_state *ps, int argc, char *argv[]);
static int parse_revision(git_repository *repo, struct parse_state *ps);

int lg2_rev_parse(git_repository *repo, int argc, char *argv[])
{
	struct parse_state ps = {0};

	parse_opts(&ps, argc, argv);

	check_lg2(parse_revision(repo, &ps), "Parsing", NULL);

	return 0;
}

static void usage(const char *message, const char *arg)
{
	if (message && arg)
		fprintf(stderr, "%s: %s\n", message, arg);
	else if (message)
		fprintf(stderr, "%s\n", message);
	fprintf(stderr, "usage: rev-parse [ --option ] <args>...\n");
	exit(1);
}

static void parse_opts(struct parse_state *ps, int argc, char *argv[])
{
	struct args_info args = ARGS_INFO_INIT;

	for (args.pos=1; args.pos < argc; ++args.pos) {
		const char *a = argv[args.pos];

		if (a[0] != '-') {
			if (ps->spec)
				usage("Too many specs", a);
			ps->spec = a;
		} else if (!strcmp(a, "--not"))
			ps->not = !ps->not;
		else if (!match_str_arg(&ps->repodir, &args, "--git-dir"))
			usage("Cannot handle argument", a);
	}
}

static int parse_revision(git_repository *repo, struct parse_state *ps)
{
	git_revspec rs;
	char str[GIT_OID_HEXSZ + 1];

	check_lg2(git_revparse(&rs, repo, ps->spec), "Could not parse", ps->spec);

	if ((rs.flags & GIT_REVPARSE_SINGLE) != 0) {
		git_oid_tostr(str, sizeof(str), git_object_id(rs.from));
		printf("%s\n", str);
		git_object_free(rs.from);
	}
	else if ((rs.flags & GIT_REVPARSE_RANGE) != 0) {
		git_oid_tostr(str, sizeof(str), git_object_id(rs.to));
		printf("%s\n", str);
		git_object_free(rs.to);

		if ((rs.flags & GIT_REVPARSE_MERGE_BASE) != 0) {
			git_oid base;
			check_lg2(git_merge_base(&base, repo,
						git_object_id(rs.from), git_object_id(rs.to)),
					"Could not find merge base", ps->spec);

			git_oid_tostr(str, sizeof(str), &base);
			printf("%s\n", str);
		}

		git_oid_tostr(str, sizeof(str), git_object_id(rs.from));
		printf("^%s\n", str);
		git_object_free(rs.from);
	}
	else {
		fatal("Invalid results from git_revparse", ps->spec);
	}

	return 0;
}

