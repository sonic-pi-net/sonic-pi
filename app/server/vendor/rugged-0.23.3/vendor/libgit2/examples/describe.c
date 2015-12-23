/*
 * libgit2 "describe" example - shows how to describe commits
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

/**
 * The following example partially reimplements the `git describe` command
 * and some of its options.
 *
 * These commands should work:

 * - Describe HEAD with default options (`describe`)
 * - Describe specified revision (`describe master~2`)
 * - Describe specified revisions (`describe master~2 HEAD~3`)
 * - Describe HEAD with dirty state suffix (`describe --dirty=*`)
 * - Describe consider all refs (`describe --all master`)
 * - Describe consider lightweight tags (`describe --tags temp-tag`)
 * - Describe show non-default abbreviated size (`describe --abbrev=10`)
 * - Describe always output the long format if matches a tag (`describe --long v1.0`)
 * - Describe consider only tags of specified pattern (`describe --match v*-release`)
 * - Describe show the fallback result (`describe --always`)
 * - Describe follow only the first parent commit (`describe --first-parent`)
 *
 * The command line parsing logic is simplified and doesn't handle
 * all of the use cases.
 */

/** describe_options represents the parsed command line options */
typedef struct {
	const char **commits;
	size_t commit_count;
	git_describe_options describe_options;
	git_describe_format_options format_options;
} describe_options;

typedef struct args_info args_info;

static void *xrealloc(void *oldp, size_t newsz)
{
	void *p = realloc(oldp, newsz);
	if (p == NULL) {
		fprintf(stderr, "Cannot allocate memory, exiting.\n");
		exit(1);
	}
	return p;
}

static void opts_add_commit(describe_options *opts, const char *commit)
{
	size_t sz;

	assert(opts != NULL);

	sz = ++opts->commit_count * sizeof(opts->commits[0]);
	opts->commits = xrealloc(opts->commits, sz);
	opts->commits[opts->commit_count - 1] = commit;
}

static void do_describe_single(git_repository *repo, describe_options *opts, const char *rev)
{
	git_object *commit;
	git_describe_result *describe_result;
	git_buf buf = { 0 };
	
	if (rev) {
		check_lg2(git_revparse_single(&commit, repo, rev),
			"Failed to lookup rev", rev);

		check_lg2(git_describe_commit(&describe_result, commit, &opts->describe_options),
			"Failed to describe rev", rev);
	}
	else
		check_lg2(git_describe_workdir(&describe_result, repo, &opts->describe_options),
			"Failed to describe workdir", NULL);

	check_lg2(git_describe_format(&buf, describe_result, &opts->format_options),
			"Failed to format describe rev", rev);

	printf("%s\n", buf.ptr);
}

static void do_describe(git_repository *repo, describe_options *opts)
{
	if (opts->commit_count == 0)
		do_describe_single(repo, opts, NULL);
	else
	{
		size_t i;
		for (i = 0; i < opts->commit_count; i++)
			do_describe_single(repo, opts, opts->commits[i]);
	}
}

static void print_usage(void)
{
	fprintf(stderr, "usage: see `git help describe`\n");
	exit(1);
}

/** Parse command line arguments */
static void parse_options(describe_options *opts, int argc, char **argv)
{
	args_info args = ARGS_INFO_INIT;

	for (args.pos = 1; args.pos < argc; ++args.pos) {
		const char *curr = argv[args.pos];

		if (curr[0] != '-') {
			opts_add_commit(opts, curr);
		} else if (!strcmp(curr, "--all")) {
			opts->describe_options.describe_strategy = GIT_DESCRIBE_ALL;
		} else if (!strcmp(curr, "--tags")) {
			opts->describe_options.describe_strategy = GIT_DESCRIBE_TAGS;
		} else if (!strcmp(curr, "--exact-match")) {
			opts->describe_options.max_candidates_tags = 0;
		} else if (!strcmp(curr, "--long")) {
			opts->format_options.always_use_long_format = 1;
		} else if (!strcmp(curr, "--always")) {
			opts->describe_options.show_commit_oid_as_fallback = 1;
		} else if (!strcmp(curr, "--first-parent")) {
			opts->describe_options.only_follow_first_parent = 1;
		} else if (optional_str_arg(&opts->format_options.dirty_suffix, &args, "--dirty", "-dirty")) {
		} else if (match_int_arg((int *)&opts->format_options.abbreviated_size, &args, "--abbrev", 0)) {
		} else if (match_int_arg((int *)&opts->describe_options.max_candidates_tags, &args, "--candidates", 0)) {
		} else if (match_str_arg(&opts->describe_options.pattern, &args, "--match")) {
		} else {
			print_usage();
		}
	}

	if (opts->commit_count > 0) {
		if (opts->format_options.dirty_suffix)
			fatal("--dirty is incompatible with commit-ishes", NULL);
	}
	else {
		if (!opts->format_options.dirty_suffix || !opts->format_options.dirty_suffix[0]) {
			opts_add_commit(opts, "HEAD");
		}
	}
}

/** Initialize describe_options struct */
static void describe_options_init(describe_options *opts)
{
	memset(opts, 0, sizeof(*opts));

	opts->commits = NULL;
	opts->commit_count = 0;
	git_describe_init_options(&opts->describe_options, GIT_DESCRIBE_OPTIONS_VERSION);
	git_describe_init_format_options(&opts->format_options, GIT_DESCRIBE_FORMAT_OPTIONS_VERSION);
}

int main(int argc, char **argv)
{
	git_repository *repo;
	describe_options opts;

	git_libgit2_init();

	check_lg2(git_repository_open_ext(&repo, ".", 0, NULL),
			"Could not open repository", NULL);

	describe_options_init(&opts);
	parse_options(&opts, argc, argv);

	do_describe(repo, &opts);

	git_repository_free(repo);
	git_libgit2_shutdown();

	return 0;
}
