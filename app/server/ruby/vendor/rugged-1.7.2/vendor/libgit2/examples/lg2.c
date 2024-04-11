#include "common.h"

/* This part is not strictly libgit2-dependent, but you can use this
 * as a starting point for a git-like tool */

typedef int (*git_command_fn)(git_repository *, int , char **);

struct {
	char *name;
	git_command_fn fn;
	char requires_repo;
} commands[] = {
	{ "add",          lg2_add,          1 },
	{ "blame",        lg2_blame,        1 },
	{ "cat-file",     lg2_cat_file,     1 },
	{ "checkout",     lg2_checkout,     1 },
	{ "clone",        lg2_clone,        0 },
	{ "commit",       lg2_commit,       1 },
	{ "config",       lg2_config,       1 },
	{ "describe",     lg2_describe,     1 },
	{ "diff",         lg2_diff,         1 },
	{ "fetch",        lg2_fetch,        1 },
	{ "for-each-ref", lg2_for_each_ref, 1 },
	{ "general",      lg2_general,      0 },
	{ "index-pack",   lg2_index_pack,   1 },
	{ "init",         lg2_init,         0 },
	{ "log",          lg2_log,          1 },
	{ "ls-files",     lg2_ls_files,     1 },
	{ "ls-remote",    lg2_ls_remote,    1 },
	{ "merge",        lg2_merge,        1 },
	{ "push",         lg2_push,        1  },
	{ "remote",       lg2_remote,       1 },
	{ "rev-list",     lg2_rev_list,     1 },
	{ "rev-parse",    lg2_rev_parse,    1 },
	{ "show-index",   lg2_show_index,   0 },
	{ "stash",        lg2_stash,        1 },
	{ "status",       lg2_status,       1 },
	{ "tag",          lg2_tag,          1 },
};

static int run_command(git_command_fn fn, git_repository *repo, struct args_info args)
{
	int error;

	/* Run the command. If something goes wrong, print the error message to stderr */
	error = fn(repo, args.argc - args.pos, &args.argv[args.pos]);
	if (error < 0) {
		if (git_error_last() == NULL)
			fprintf(stderr, "Error without message");
		else
			fprintf(stderr, "Bad news:\n %s\n", git_error_last()->message);
	}

	return !!error;
}

static int usage(const char *prog)
{
	size_t i;

	fprintf(stderr, "usage: %s <cmd>...\n\nAvailable commands:\n\n", prog);
	for (i = 0; i < ARRAY_SIZE(commands); i++)
		fprintf(stderr, "\t%s\n", commands[i].name);

	exit(EXIT_FAILURE);
}

int main(int argc, char **argv)
{
	struct args_info args = ARGS_INFO_INIT;
	git_repository *repo = NULL;
	const char *git_dir = NULL;
	int return_code = 1;
	size_t i;

	if (argc < 2)
		usage(argv[0]);

	git_libgit2_init();

	for (args.pos = 1; args.pos < args.argc; ++args.pos) {
		char *a = args.argv[args.pos];

		if (a[0] != '-') {
			/* non-arg */
			break;
		} else if (optional_str_arg(&git_dir, &args, "--git-dir", ".git")) {
			continue;
		} else if (match_arg_separator(&args)) {
			break;
		}
	}

	if (args.pos == args.argc)
		usage(argv[0]);

	if (!git_dir)
		git_dir = ".";

	for (i = 0; i < ARRAY_SIZE(commands); ++i) {
		if (strcmp(args.argv[args.pos], commands[i].name))
			continue;

		/*
		 * Before running the actual command, create an instance
		 * of the local repository and pass it to the function.
		 * */
		if (commands[i].requires_repo) {
			check_lg2(git_repository_open_ext(&repo, git_dir, 0, NULL),
				  "Unable to open repository '%s'", git_dir);
		}

		return_code = run_command(commands[i].fn, repo, args);
		goto shutdown;
	}

	fprintf(stderr, "Command not found: %s\n", argv[1]);

shutdown:
	git_repository_free(repo);
	git_libgit2_shutdown();

	return return_code;
}
