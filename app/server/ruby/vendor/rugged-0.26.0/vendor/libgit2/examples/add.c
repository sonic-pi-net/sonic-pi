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
#include <assert.h>

enum print_options {
	SKIP = 1,
	VERBOSE = 2,
	UPDATE = 4,
};

struct print_payload {
	enum print_options options;
	git_repository *repo;
};

/* Forward declarations for helpers */
static void parse_opts(int *options, int *count, int argc, char *argv[]);
void init_array(git_strarray *array, int argc, char **argv);
int print_matched_cb(const char *path, const char *matched_pathspec, void *payload);

int main (int argc, char** argv)
{
	git_index_matched_path_cb matched_cb = NULL;
	git_repository *repo = NULL;
	git_index *index;
	git_strarray array = {0};
	int options = 0, count = 0;
	struct print_payload payload = {0};

	git_libgit2_init();

	parse_opts(&options, &count, argc, argv);

	init_array(&array, argc-count, argv+count);

	check_lg2(git_repository_open(&repo, "."), "No git repository", NULL);
	check_lg2(git_repository_index(&index, repo), "Could not open repository index", NULL);

	if (options&VERBOSE || options&SKIP) {
		matched_cb = &print_matched_cb;
	}

	payload.options = options;
	payload.repo = repo;

	if (options&UPDATE) {
		git_index_update_all(index, &array, matched_cb, &payload);
	} else {
		git_index_add_all(index, &array, 0, matched_cb, &payload);
	}

	git_index_write(index);
	git_index_free(index);
	git_repository_free(repo);

	git_libgit2_shutdown();

	return 0;
}

int print_matched_cb(const char *path, const char *matched_pathspec, void *payload)
{
	struct print_payload p = *(struct print_payload*)(payload);
	int ret;
	unsigned status;
	(void)matched_pathspec;

	if (git_status_file(&status, p.repo, path)) {
		return -1;
	}

	if (status & GIT_STATUS_WT_MODIFIED || status & GIT_STATUS_WT_NEW) {
		printf("add '%s'\n", path);
		ret = 0;
	} else {
		ret = 1;
	}

	if(p.options & SKIP) {
		ret = 1;
	}

	return ret;
}

void init_array(git_strarray *array, int argc, char **argv)
{
	unsigned int i;

	array->count = argc;
	array->strings = malloc(sizeof(char*) * array->count);
	assert(array->strings!=NULL);

	for(i=0; i<array->count; i++) {
		array->strings[i]=argv[i];
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

static void parse_opts(int *options, int *count, int argc, char *argv[])
{
	int i;

	for (i = 1; i < argc; ++i) {
		if (argv[i][0] != '-') {
			break;
		}
		else if(!strcmp(argv[i], "--verbose") || !strcmp(argv[i], "-v")) {
			*options |= VERBOSE;
		}
		else if(!strcmp(argv[i], "--dry-run") || !strcmp(argv[i], "-n")) {
			*options |= SKIP;
		}
		else if(!strcmp(argv[i], "--update") || !strcmp(argv[i], "-u")) {
			*options |= UPDATE;
		}
		else if(!strcmp(argv[i], "-h")) {
			print_usage();
			break;
		}
		else if(!strcmp(argv[i], "--")) {
			i++;
			break;
		}
		else {
			fprintf(stderr, "Unsupported option %s.\n", argv[i]);
			print_usage();
		}
	}

	if (argc<=i)
		print_usage();

	*count = i;
}
