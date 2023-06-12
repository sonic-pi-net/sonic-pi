/*
 * libgit2 "ls-files" example - shows how to view all files currently in the index
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

/**
 * This example demonstrates the libgit2 index APIs to roughly
 * simulate the output of `git ls-files`.
 * `git ls-files` has many options and this currently does not show them.
 *
 * `git ls-files` base command shows all paths in the index at that time.
 * This includes staged and committed files, but unstaged files will not display.
 *
 * This currently supports the default behavior and the `--error-unmatch` option.
 */

struct ls_options {
	int error_unmatch;
	char *files[1024];
	size_t file_count;
};

static void usage(const char *message, const char *arg)
{
	if (message && arg)
		fprintf(stderr, "%s: %s\n", message, arg);
	else if (message)
		fprintf(stderr, "%s\n", message);
	fprintf(stderr, "usage: ls-files [--error-unmatch] [--] [<file>...]\n");
	exit(1);
}

static int parse_options(struct ls_options *opts, int argc, char *argv[])
{
	int parsing_files = 0;
	int i;

	memset(opts, 0, sizeof(struct ls_options));

	if (argc < 2)
		return 0;

	for (i = 1; i < argc; ++i) {
		char *a = argv[i];

		/* if it doesn't start with a '-' or is after the '--' then it is a file */
		if (a[0] != '-' || parsing_files) {
			parsing_files = 1;

			/* watch for overflows (just in case) */
			if (opts->file_count == 1024) {
				fprintf(stderr, "ls-files can only support 1024 files at this time.\n");
				return -1;
			}

			opts->files[opts->file_count++] = a;
		} else if (!strcmp(a, "--")) {
			parsing_files = 1;
		} else if (!strcmp(a, "--error-unmatch")) {
			opts->error_unmatch = 1;
		} else {
			usage("Unsupported argument", a);
			return -1;
		}
	}

	return 0;
}

static int print_paths(struct ls_options *opts, git_index *index)
{
	size_t i;
	const git_index_entry *entry;

	/* if there are no files explicitly listed by the user print all entries in the index */
	if (opts->file_count == 0) {
		size_t entry_count = git_index_entrycount(index);

		for (i = 0; i < entry_count; i++) {
			entry = git_index_get_byindex(index, i);
			puts(entry->path);
		}
		return 0;
	}

	/* loop through the files found in the args and print them if they exist */
	for (i = 0; i < opts->file_count; ++i) {
		const char *path = opts->files[i];

		if ((entry = git_index_get_bypath(index, path, GIT_INDEX_STAGE_NORMAL)) != NULL) {
			puts(path);
		} else if (opts->error_unmatch) {
			fprintf(stderr, "error: pathspec '%s' did not match any file(s) known to git.\n", path);
			fprintf(stderr, "Did you forget to 'git add'?\n");
			return -1;
		}
	}

	return 0;
}

int lg2_ls_files(git_repository *repo, int argc, char *argv[])
{
	git_index *index = NULL;
	struct ls_options opts;
	int error;

	if ((error = parse_options(&opts, argc, argv)) < 0)
		return error;

	if ((error = git_repository_index(&index, repo)) < 0)
		goto cleanup;

	error = print_paths(&opts, index);

cleanup:
	git_index_free(index);

	return error;
}
