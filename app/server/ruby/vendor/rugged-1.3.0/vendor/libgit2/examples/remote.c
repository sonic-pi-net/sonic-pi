/*
 * libgit2 "remote" example - shows how to modify remotes for a repo
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
 * This is a sample program that is similar to "git remote".  See the
 * documentation for that (try "git help remote") to understand what this
 * program is emulating.
 *
 * This demonstrates using the libgit2 APIs to modify remotes of a repository.
 */

enum subcmd {
	subcmd_add,
	subcmd_remove,
	subcmd_rename,
	subcmd_seturl,
	subcmd_show,
};

struct remote_opts {
	enum subcmd cmd;

	/* for command-specific args */
	int argc;
	char **argv;
};

static int cmd_add(git_repository *repo, struct remote_opts *o);
static int cmd_remove(git_repository *repo, struct remote_opts *o);
static int cmd_rename(git_repository *repo, struct remote_opts *o);
static int cmd_seturl(git_repository *repo, struct remote_opts *o);
static int cmd_show(git_repository *repo, struct remote_opts *o);

static void parse_subcmd(
	struct remote_opts *opt, int argc, char **argv);
static void usage(const char *msg, const char *arg);

int lg2_remote(git_repository *repo, int argc, char *argv[])
{
	int retval = 0;
	struct remote_opts opt = {0};

	parse_subcmd(&opt, argc, argv);

	switch (opt.cmd)
	{
	case subcmd_add:
		retval = cmd_add(repo, &opt);
		break;
	case subcmd_remove:
		retval = cmd_remove(repo, &opt);
		break;
	case subcmd_rename:
		retval = cmd_rename(repo, &opt);
		break;
	case subcmd_seturl:
		retval = cmd_seturl(repo, &opt);
		break;
	case subcmd_show:
		retval = cmd_show(repo, &opt);
		break;
	}

	return retval;
}

static int cmd_add(git_repository *repo, struct remote_opts *o)
{
	char *name, *url;
	git_remote *remote = {0};

	if (o->argc != 2)
		usage("you need to specify a name and URL", NULL);

	name = o->argv[0];
	url = o->argv[1];

	check_lg2(git_remote_create(&remote, repo, name, url),
			"could not create remote", NULL);

	return 0;
}

static int cmd_remove(git_repository *repo, struct remote_opts *o)
{
	char *name;

	if (o->argc != 1)
		usage("you need to specify a name", NULL);

	name = o->argv[0];

	check_lg2(git_remote_delete(repo, name),
			"could not delete remote", name);

	return 0;
}

static int cmd_rename(git_repository *repo, struct remote_opts *o)
{
	int i, retval;
	char *old, *new;
	git_strarray problems = {0};

	if (o->argc != 2)
		usage("you need to specify old and new remote name", NULL);

	old = o->argv[0];
	new = o->argv[1];

	retval = git_remote_rename(&problems, repo, old, new);
	if (!retval)
		return 0;

	for (i = 0; i < (int) problems.count; i++) {
		puts(problems.strings[0]);
	}

	git_strarray_dispose(&problems);

	return retval;
}

static int cmd_seturl(git_repository *repo, struct remote_opts *o)
{
	int i, retval, push = 0;
	char *name = NULL, *url = NULL;

	for (i = 0; i < o->argc; i++) {
		char *arg = o->argv[i];

		if (!strcmp(arg, "--push")) {
			push = 1;
		} else if (arg[0] != '-' && name == NULL) {
			name = arg;
		} else if (arg[0] != '-' && url == NULL) {
			url = arg;
		} else {
			usage("invalid argument to set-url", arg);
		}
	}

	if (name == NULL || url == NULL)
		usage("you need to specify remote and the new URL", NULL);

	if (push)
		retval = git_remote_set_pushurl(repo, name, url);
	else
		retval = git_remote_set_url(repo, name, url);

	check_lg2(retval, "could not set URL", url);

	return 0;
}

static int cmd_show(git_repository *repo, struct remote_opts *o)
{
	int i;
	const char *arg, *name, *fetch, *push;
	int verbose = 0;
	git_strarray remotes = {0};
	git_remote *remote = {0};

	for (i = 0; i < o->argc; i++) {
		arg = o->argv[i];

		if (!strcmp(arg, "-v") || !strcmp(arg, "--verbose")) {
			verbose = 1;
		}
	}

	check_lg2(git_remote_list(&remotes, repo),
		"could not retrieve remotes", NULL);

	for (i = 0; i < (int) remotes.count; i++) {
		name = remotes.strings[i];
		if (!verbose) {
			puts(name);
			continue;
		}

		check_lg2(git_remote_lookup(&remote, repo, name),
			"could not look up remote", name);

		fetch = git_remote_url(remote);
		if (fetch)
			printf("%s\t%s (fetch)\n", name, fetch);
		push = git_remote_pushurl(remote);
		/* use fetch URL if no distinct push URL has been set */
		push = push ? push : fetch;
		if (push)
			printf("%s\t%s (push)\n", name, push);

		git_remote_free(remote);
	}

	git_strarray_dispose(&remotes);

	return 0;
}

static void parse_subcmd(
	struct remote_opts *opt, int argc, char **argv)
{
	char *arg = argv[1];
	enum subcmd cmd = 0;

	if (argc < 2)
		usage("no command specified", NULL);

	if (!strcmp(arg, "add")) {
		cmd = subcmd_add;
	} else if (!strcmp(arg, "remove")) {
		cmd = subcmd_remove;
	} else if (!strcmp(arg, "rename")) {
		cmd = subcmd_rename;
	} else if (!strcmp(arg, "set-url")) {
		cmd = subcmd_seturl;
	} else if (!strcmp(arg, "show")) {
		cmd = subcmd_show;
	} else {
		usage("command is not valid", arg);
	}
	opt->cmd = cmd;

	opt->argc = argc - 2; /* executable and subcommand are removed */
	opt->argv = argv + 2;
}

static void usage(const char *msg, const char *arg)
{
	fputs("usage: remote add <name> <url>\n", stderr);
	fputs("       remote remove <name>\n", stderr);
	fputs("       remote rename <old> <new>\n", stderr);
	fputs("       remote set-url [--push] <name> <newurl>\n", stderr);
	fputs("       remote show [-v|--verbose]\n", stderr);

	if (msg && !arg)
		fprintf(stderr, "\n%s\n", msg);
	else if (msg && arg)
		fprintf(stderr, "\n%s: %s\n", msg, arg);
	exit(1);
}
