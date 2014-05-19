#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "common.h"

// This part is not strictly libgit2-dependent, but you can use this
// as a starting point for a git-like tool

struct {
	char *name;
	git_cb fn;
} commands[] = {
	{"ls-remote", ls_remote},
	{"fetch", fetch},
	{"clone", do_clone},
	{"index-pack", index_pack},
	{ NULL, NULL}
};

static int run_command(git_cb fn, int argc, char **argv)
{
	int error;
	git_repository *repo;

// Before running the actual command, create an instance of the local
// repository and pass it to the function.

	error = git_repository_open(&repo, ".git");
	if (error < 0)
		repo = NULL;

	// Run the command. If something goes wrong, print the error message to stderr
	error = fn(repo, argc, argv);
	if (error < 0) {
		if (giterr_last() == NULL)
			fprintf(stderr, "Error without message");
		else
			fprintf(stderr, "Bad news:\n %s\n", giterr_last()->message);
	}

	if(repo)
		git_repository_free(repo);

	return !!error;
}

int main(int argc, char **argv)
{
	int i;

	if (argc < 2) {
		fprintf(stderr, "usage: %s <cmd> [repo]\n", argv[0]);
		exit(EXIT_FAILURE);
	}

	git_threads_init();

	for (i = 0; commands[i].name != NULL; ++i) {
		if (!strcmp(argv[1], commands[i].name))
			return run_command(commands[i].fn, --argc, ++argv);
	}

	fprintf(stderr, "Command not found: %s\n", argv[1]);
	return 1;
}
