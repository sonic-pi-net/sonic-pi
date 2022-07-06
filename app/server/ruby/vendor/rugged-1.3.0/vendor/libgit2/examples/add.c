/*
 * libgit2 "add" example - shows how to modify the index
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
 * The following example demonstrates how to add files with libgit2.
 *
 * It will use the repository in the current working directory, and act
 * on files passed as its parameters.
 *
 * Recognized options are:
 *   -v/--verbose: show the file's status after acting on it.
 *   -n/--dry-run: do not actually change the index.
 *   -u/--update: update the index instead of adding to it.
 */

enum index_mode {
	INDEX_NONE,
	INDEX_ADD,
};

struct index_options {
	int dry_run;
	int verbose;
	git_repository *repo;
	enum index_mode mode;
	int add_update;
};

/* Forward declarations for helpers */
static void parse_opts(const char **repo_path, struct index_options *opts, struct args_info *args);
int print_matched_cb(const char *path, const char *matched_pathspec, void *payload);

int lg2_add(git_repository *repo, int argc, char **argv)
{
	git_index_matched_path_cb matched_cb = NULL;
	git_index *index;
	git_strarray array = {0};
	struct index_options options = {0};
	struct args_info args = ARGS_INFO_INIT;

	options.mode = INDEX_ADD;

	/* Parse the options & arguments. */
	parse_opts(NULL, &options, &args);
	strarray_from_args(&array, &args);

	/* Grab the repository's index. */
	check_lg2(git_repository_index(&index, repo), "Could not open repository index", NULL);

	/* Setup a callback if the requested options need it */
	if (options.verbose || options.dry_run) {
		matched_cb = &print_matched_cb;
	}

	options.repo = repo;

	/* Perform the requested action with the index and files */
	if (options.add_update) {
		git_index_update_all(index, &array, matched_cb, &options);
	} else {
		git_index_add_all(index, &array, 0, matched_cb, &options);
	}

	/* Cleanup memory */
	git_index_write(index);
	git_index_free(index);

	return 0;
}

/*
 * This callback is called for each file under consideration by
 * git_index_(update|add)_all above.
 * It makes uses of the callback's ability to abort the action.
 */
int print_matched_cb(const char *path, const char *matched_pathspec, void *payload)
{
	struct index_options *opts = (struct index_options *)(payload);
	int ret;
	unsigned status;
	(void)matched_pathspec;

	/* Get the file status */
	if (git_status_file(&status, opts->repo, path) < 0)
		return -1;

	if ((status & GIT_STATUS_WT_MODIFIED) || (status & GIT_STATUS_WT_NEW)) {
		printf("add '%s'\n", path);
		ret = 0;
	} else {
		ret = 1;
	}

	if (opts->dry_run)
		ret = 1;

	return ret;
}

void init_array(git_strarray *array, int argc, char **argv)
{
	unsigned int i;

	array->count = argc;
	array->strings = calloc(array->count, sizeof(char *));
	assert(array->strings != NULL);

	for (i = 0; i < array->count; i++) {
		array->strings[i] = argv[i];
	}

	return;
}

void print_usage(void)
{
	fprintf(stderr, "usage: add [options] [--] file-spec [file-spec] [...]\n\n");
	fprintf(stderr, "\t-n, --dry-run    dry run\n");
	fprintf(stderr, "\t-v, --verbose    be verbose\n");
	fprintf(stderr, "\t-u, --update     update tracked files\n");
	exit(1);
}

static void parse_opts(const char **repo_path, struct index_options *opts, struct args_info *args)
{
	if (args->argc <= 1)
		print_usage();

	for (args->pos = 1; args->pos < args->argc; ++args->pos) {
		const char *curr = args->argv[args->pos];

		if (curr[0] != '-') {
			if (!strcmp("add", curr)) {
				opts->mode = INDEX_ADD;
				continue;
			} else if (opts->mode == INDEX_NONE) {
				fprintf(stderr, "missing command: %s", curr);
				print_usage();
				break;
			} else {
				/* We might be looking at a filename */
				break;
			}
		} else if (match_bool_arg(&opts->verbose, args, "--verbose") ||
				   match_bool_arg(&opts->dry_run, args, "--dry-run") ||
				   match_str_arg(repo_path, args, "--git-dir") ||
				   (opts->mode == INDEX_ADD && match_bool_arg(&opts->add_update, args, "--update"))) {
			continue;
		} else if (match_bool_arg(NULL, args, "--help")) {
			print_usage();
			break;
		} else if (match_arg_separator(args)) {
			break;
		} else {
			fprintf(stderr, "Unsupported option %s.\n", curr);
			print_usage();
		}
	}
}
