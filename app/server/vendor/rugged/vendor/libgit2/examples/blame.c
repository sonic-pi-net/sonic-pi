/*
 * libgit2 "blame" example - shows how to use the blame API
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

#ifdef _MSC_VER
#define snprintf sprintf_s
#define strcasecmp strcmpi
#endif

/**
 * This example demonstrates how to invoke the libgit2 blame API to roughly
 * simulate the output of `git blame` and a few of its command line arguments.
 */

struct opts {
	char *path;
	char *commitspec;
	int C;
	int M;
	int start_line;
	int end_line;
	int F;
};
static void parse_opts(struct opts *o, int argc, char *argv[]);

int main(int argc, char *argv[])
{
	int i, line, break_on_null_hunk;
	char spec[1024] = {0};
	struct opts o = {0};
	const char *rawdata;
	git_repository *repo = NULL;
	git_revspec revspec = {0};
	git_blame_options blameopts = GIT_BLAME_OPTIONS_INIT;
	git_blame *blame = NULL;
	git_blob *blob;
	git_object *obj;

	git_threads_init();

	parse_opts(&o, argc, argv);
	if (o.M) blameopts.flags |= GIT_BLAME_TRACK_COPIES_SAME_COMMIT_MOVES;
	if (o.C) blameopts.flags |= GIT_BLAME_TRACK_COPIES_SAME_COMMIT_COPIES;
	if (o.F) blameopts.flags |= GIT_BLAME_FIRST_PARENT;

	/** Open the repository. */
	check_lg2(git_repository_open_ext(&repo, ".", 0, NULL), "Couldn't open repository", NULL);

	/**
	 * The commit range comes in "commitish" form. Use the rev-parse API to
	 * nail down the end points.
	 */
	if (o.commitspec) {
		check_lg2(git_revparse(&revspec, repo, o.commitspec), "Couldn't parse commit spec", NULL);
		if (revspec.flags & GIT_REVPARSE_SINGLE) {
			git_oid_cpy(&blameopts.newest_commit, git_object_id(revspec.from));
			git_object_free(revspec.from);
		} else {
			git_oid_cpy(&blameopts.oldest_commit, git_object_id(revspec.from));
			git_oid_cpy(&blameopts.newest_commit, git_object_id(revspec.to));
			git_object_free(revspec.from);
			git_object_free(revspec.to);
		}
	}

	/** Run the blame. */
	check_lg2(git_blame_file(&blame, repo, o.path, &blameopts), "Blame error", NULL);

	/**
	 * Get the raw data inside the blob for output. We use the
	 * `commitish:path/to/file.txt` format to find it.
	 */
	if (git_oid_iszero(&blameopts.newest_commit))
		strcpy(spec, "HEAD");
	else
		git_oid_tostr(spec, sizeof(spec), &blameopts.newest_commit);
	strcat(spec, ":");
	strcat(spec, o.path);

	check_lg2(git_revparse_single(&obj, repo, spec), "Object lookup error", NULL);
	check_lg2(git_blob_lookup(&blob, repo, git_object_id(obj)), "Blob lookup error", NULL);
	git_object_free(obj);

	rawdata = git_blob_rawcontent(blob);

	/** Produce the output. */
	line = 1;
	i = 0;
	break_on_null_hunk = 0;
	while (i < git_blob_rawsize(blob)) {
		const char *eol = strchr(rawdata+i, '\n');
		char oid[10] = {0};
		const git_blame_hunk *hunk = git_blame_get_hunk_byline(blame, line);

		if (break_on_null_hunk && !hunk) break;

		if (hunk) {
			char sig[128] = {0};
			break_on_null_hunk = 1;
			

			git_oid_tostr(oid, 10, &hunk->final_commit_id);
			snprintf(sig, 30, "%s <%s>", hunk->final_signature->name, hunk->final_signature->email);

			printf("%s ( %-30s %3d) %.*s\n",
					oid,
					sig,
					line,
					(int)(eol-rawdata-i),
					rawdata+i);
		}

		i = (int)(eol - rawdata + 1);
		line++;
	}

	/** Cleanup. */
	git_blob_free(blob);
	git_blame_free(blame);
	git_repository_free(repo);

	git_threads_shutdown();

	return 0;
}

/** Tell the user how to make this thing work. */
static void usage(const char *msg, const char *arg)
{
	if (msg && arg)
		fprintf(stderr, "%s: %s\n", msg, arg);
	else if (msg)
		fprintf(stderr, "%s\n", msg);
	fprintf(stderr, "usage: blame [options] [<commit range>] <path>\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "   <commit range>      example: `HEAD~10..HEAD`, or `1234abcd`\n");
	fprintf(stderr, "   -L <n,m>            process only line range n-m, counting from 1\n");
	fprintf(stderr, "   -M                  find line moves within and across files\n");
	fprintf(stderr, "   -C                  find line copies within and across files\n");
	fprintf(stderr, "   -F                  follow only the first parent commits\n");
	fprintf(stderr, "\n");
	exit(1);
}

/** Parse the arguments. */
static void parse_opts(struct opts *o, int argc, char *argv[])
{
	int i;
	char *bare_args[3] = {0};

	if (argc < 2) usage(NULL, NULL);

	for (i=1; i<argc; i++) {
		char *a = argv[i];

		if (a[0] != '-') {
			int i=0;
			while (bare_args[i] && i < 3) ++i;
			if (i >= 3)
				usage("Invalid argument set", NULL);
			bare_args[i] = a;
		}
		else if (!strcmp(a, "--"))
			continue;
		else if (!strcasecmp(a, "-M"))
			o->M = 1;
		else if (!strcasecmp(a, "-C"))
			o->C = 1;
		else if (!strcasecmp(a, "-F"))
			o->F = 1;
		else if (!strcasecmp(a, "-L")) {
			i++; a = argv[i];
			if (i >= argc) fatal("Not enough arguments to -L", NULL);
			check_lg2(sscanf(a, "%d,%d", &o->start_line, &o->end_line)-2, "-L format error", NULL);
		}
		else {
			/* commit range */
			if (o->commitspec) fatal("Only one commit spec allowed", NULL);
			o->commitspec = a;
		}
	}

	/* Handle the bare arguments */
	if (!bare_args[0]) usage("Please specify a path", NULL);
	o->path = bare_args[0];
	if (bare_args[1]) {
		/* <commitspec> <path> */
		o->path = bare_args[1];
		o->commitspec = bare_args[0];
	}
	if (bare_args[2]) {
		/* <oldcommit> <newcommit> <path> */
		char spec[128] = {0};
		o->path = bare_args[2];
		sprintf(spec, "%s..%s", bare_args[0], bare_args[1]);
		o->commitspec = spec;
	}
}
