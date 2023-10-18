/*
 * libgit2 "stash" example - shows how to use the stash API
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

#include <stdarg.h>

#include "common.h"

enum subcmd {
	SUBCMD_APPLY,
	SUBCMD_LIST,
	SUBCMD_POP,
	SUBCMD_PUSH
};

struct opts {
	enum subcmd cmd;
	int argc;
	char **argv;
};

static void usage(const char *fmt, ...)
{
	va_list ap;

	fputs("usage: git stash list\n", stderr);
	fputs("   or: git stash ( pop | apply )\n", stderr);
	fputs("   or: git stash [push]\n", stderr);
	fputs("\n", stderr);

	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);

	exit(1);
}

static void parse_subcommand(struct opts *opts, int argc, char *argv[])
{
	char *arg = (argc < 2) ? "push" : argv[1];
	enum subcmd cmd;

	if (!strcmp(arg, "apply")) {
		cmd = SUBCMD_APPLY;
	} else if (!strcmp(arg, "list")) {
		cmd = SUBCMD_LIST;
	} else if (!strcmp(arg, "pop")) {
		cmd = SUBCMD_POP;
	} else if (!strcmp(arg, "push")) {
		cmd = SUBCMD_PUSH;
	} else {
		usage("invalid command %s", arg);
		return;
	}

	opts->cmd = cmd;
	opts->argc = (argc < 2) ? argc - 1 : argc - 2;
	opts->argv = argv;
}

static int cmd_apply(git_repository *repo, struct opts *opts)
{
	if (opts->argc)
		usage("apply does not accept any parameters");

	check_lg2(git_stash_apply(repo, 0, NULL),
		  "Unable to apply stash", NULL);

	return 0;
}

static int list_stash_cb(size_t index, const char *message,
			 const git_oid *stash_id, void *payload)
{
	UNUSED(stash_id);
	UNUSED(payload);
	printf("stash@{%"PRIuZ"}: %s\n", index, message);
	return 0;
}

static int cmd_list(git_repository *repo, struct opts *opts)
{
	if (opts->argc)
		usage("list does not accept any parameters");

	check_lg2(git_stash_foreach(repo, list_stash_cb, NULL),
		  "Unable to list stashes", NULL);

	return 0;
}

static int cmd_push(git_repository *repo, struct opts *opts)
{
	git_signature *signature;
	git_commit *stash;
	git_oid stashid;

	if (opts->argc)
		usage("push does not accept any parameters");

	check_lg2(git_signature_default(&signature, repo),
		  "Unable to get signature", NULL);
	check_lg2(git_stash_save(&stashid, repo, signature, NULL, GIT_STASH_DEFAULT),
		  "Unable to save stash", NULL);
	check_lg2(git_commit_lookup(&stash, repo, &stashid),
		  "Unable to lookup stash commit", NULL);

	printf("Saved working directory %s\n", git_commit_summary(stash));

	git_signature_free(signature);
	git_commit_free(stash);

	return 0;
}

static int cmd_pop(git_repository *repo, struct opts *opts)
{
	if (opts->argc)
		usage("pop does not accept any parameters");

	check_lg2(git_stash_pop(repo, 0, NULL),
		  "Unable to pop stash", NULL);

	printf("Dropped refs/stash@{0}\n");

	return 0;
}

int lg2_stash(git_repository *repo, int argc, char *argv[])
{
	struct opts opts = { 0 };

	parse_subcommand(&opts, argc, argv);

	switch (opts.cmd) {
		case SUBCMD_APPLY:
			return cmd_apply(repo, &opts);
		case SUBCMD_LIST:
			return cmd_list(repo, &opts);
		case SUBCMD_PUSH:
			return cmd_push(repo, &opts);
		case SUBCMD_POP:
			return cmd_pop(repo, &opts);
	}

	return -1;
}
