/*
 * libgit2 "tag" example - shows how to list, create and delete tags
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
 * The following example partially reimplements the `git tag` command
 * and some of its options.
 *
 * These commands should work:

 * - Tag name listing (`tag`)
 * - Filtered tag listing with messages (`tag -n3 -l "v0.1*"`)
 * - Lightweight tag creation (`tag test v0.18.0`)
 * - Tag creation (`tag -a -m "Test message" test v0.18.0`)
 * - Tag deletion (`tag -d test`)
 *
 * The command line parsing logic is simplified and doesn't handle
 * all of the use cases.
 */

/** tag_options represents the parsed command line options */
typedef struct {
	const char *message;
	const char *pattern;
	const char *tag_name;
	const char *target;
	int num_lines;
	int force;
} tag_options;

/** tag_state represents the current program state for dragging around */
typedef struct {
	git_repository *repo;
	tag_options *opts;
} tag_state;

/** An action to execute based on the command line arguments */
typedef void (*tag_action)(tag_state *state);
typedef struct args_info args_info;

static void check(int result, const char *message)
{
	if (result) fatal(message, NULL);
}

/** Tag listing: Print individual message lines */
static void print_list_lines(const char *message, const tag_state *state)
{
	const char *msg = message;
	int num = state->opts->num_lines - 1;

	if (!msg) return;

	/** first line - headline */
	while(*msg && *msg != '\n') printf("%c", *msg++);

	/** skip over new lines */
	while(*msg && *msg == '\n') msg++;

	printf("\n");

	/** print just headline? */
	if (num == 0) return;
	if (*msg && msg[1]) printf("\n");

	/** print individual commit/tag lines */
	while (*msg && num-- >= 2) {
		printf("    ");

		while (*msg && *msg != '\n') printf("%c", *msg++);

		/** handle consecutive new lines */
		if (*msg && *msg == '\n' && msg[1] == '\n') {
			num--;
			printf("\n");
		}
		while(*msg && *msg == '\n') msg++;

		printf("\n");
	}
}

/** Tag listing: Print an actual tag object */
static void print_tag(git_tag *tag, const tag_state *state)
{
	printf("%-16s", git_tag_name(tag));

	if (state->opts->num_lines) {
		const char *msg = git_tag_message(tag);
		print_list_lines(msg, state);
	} else {
		printf("\n");
	}
}

/** Tag listing: Print a commit (target of a lightweight tag) */
static void print_commit(git_commit *commit, const char *name,
		const tag_state *state)
{
	printf("%-16s", name);

	if (state->opts->num_lines) {
		const char *msg = git_commit_message(commit);
		print_list_lines(msg, state);
	} else {
		printf("\n");
	}
}

/** Tag listing: Fallback, should not happen */
static void print_name(const char *name)
{
	printf("%s\n", name);
}

/** Tag listing: Lookup tags based on ref name and dispatch to print */
static int each_tag(const char *name, tag_state *state)
{
	git_repository *repo = state->repo;
	git_object *obj;

	check_lg2(git_revparse_single(&obj, repo, name),
			"Failed to lookup rev", name);

	switch (git_object_type(obj)) {
		case GIT_OBJ_TAG:
			print_tag((git_tag *) obj, state);
			break;
		case GIT_OBJ_COMMIT:
			print_commit((git_commit *) obj, name, state);
			break;
		default:
			print_name(name);
	}

	git_object_free(obj);
	return 0;
}

static void action_list_tags(tag_state *state)
{
	const char *pattern = state->opts->pattern;
	git_strarray tag_names = {0};
	size_t i;

	check_lg2(git_tag_list_match(&tag_names, pattern ? pattern : "*", state->repo),
			"Unable to get list of tags", NULL);

	for(i = 0; i < tag_names.count; i++) {
		each_tag(tag_names.strings[i], state);
	}

	git_strarray_free(&tag_names);
}

static void action_delete_tag(tag_state *state)
{
	tag_options *opts = state->opts;
	git_object *obj;
	git_buf abbrev_oid = {0};

	check(!opts->tag_name, "Name required");

	check_lg2(git_revparse_single(&obj, state->repo, opts->tag_name),
			"Failed to lookup rev", opts->tag_name);

	check_lg2(git_object_short_id(&abbrev_oid, obj),
			"Unable to get abbreviated OID", opts->tag_name);

	check_lg2(git_tag_delete(state->repo, opts->tag_name),
			"Unable to delete tag", opts->tag_name);

	printf("Deleted tag '%s' (was %s)\n", opts->tag_name, abbrev_oid.ptr);

	git_buf_free(&abbrev_oid);
	git_object_free(obj);
}

static void action_create_lighweight_tag(tag_state *state)
{
	git_repository *repo = state->repo;
	tag_options *opts = state->opts;
	git_oid oid;
	git_object *target;

	check(!opts->tag_name, "Name required");

	if (!opts->target) opts->target = "HEAD";

	check(!opts->target, "Target required");

	check_lg2(git_revparse_single(&target, repo, opts->target),
			"Unable to resolve spec", opts->target);

	check_lg2(git_tag_create_lightweight(&oid, repo, opts->tag_name,
				target, opts->force), "Unable to create tag", NULL);

	git_object_free(target);
}

static void action_create_tag(tag_state *state)
{
	git_repository *repo = state->repo;
	tag_options *opts = state->opts;
	git_signature *tagger;
	git_oid oid;
	git_object *target;

	check(!opts->tag_name, "Name required");
	check(!opts->message, "Message required");

	if (!opts->target) opts->target = "HEAD";

	check_lg2(git_revparse_single(&target, repo, opts->target),
			"Unable to resolve spec", opts->target);

	check_lg2(git_signature_default(&tagger, repo),
			"Unable to create signature", NULL);

	check_lg2(git_tag_create(&oid, repo, opts->tag_name,
				target, tagger, opts->message, opts->force), "Unable to create tag", NULL);

	git_object_free(target);
	git_signature_free(tagger);
}

static void print_usage(void)
{
	fprintf(stderr, "usage: see `git help tag`\n");
	exit(1);
}

/** Parse command line arguments and choose action to run when done */
static void parse_options(tag_action *action, tag_options *opts, int argc, char **argv)
{
	args_info args = ARGS_INFO_INIT;
	*action = &action_list_tags;

	for (args.pos = 1; args.pos < argc; ++args.pos) {
		const char *curr = argv[args.pos];

		if (curr[0] != '-') {
			if (!opts->tag_name)
				opts->tag_name = curr;
			else if (!opts->target)
				opts->target = curr;
			else
				print_usage();

			if (*action != &action_create_tag)
				*action = &action_create_lighweight_tag;
		} else if (!strcmp(curr, "-n")) {
			opts->num_lines = 1;
			*action = &action_list_tags;
		} else if (!strcmp(curr, "-a")) {
			*action = &action_create_tag;
		} else if (!strcmp(curr, "-f")) {
			opts->force = 1;
		} else if (match_int_arg(&opts->num_lines, &args, "-n", 0)) {
			*action = &action_list_tags;
		} else if (match_str_arg(&opts->pattern, &args, "-l")) {
			*action = &action_list_tags;
		} else if (match_str_arg(&opts->tag_name, &args, "-d")) {
			*action = &action_delete_tag;
		} else if (match_str_arg(&opts->message, &args, "-m")) {
			*action = &action_create_tag;
		}
	}
}

/** Initialize tag_options struct */
static void tag_options_init(tag_options *opts)
{
	memset(opts, 0, sizeof(*opts));

	opts->message   = NULL;
	opts->pattern   = NULL;
	opts->tag_name  = NULL;
	opts->target    = NULL;
	opts->num_lines = 0;
	opts->force     = 0;
}

int main(int argc, char **argv)
{
	git_repository *repo;
	tag_options opts;
	tag_action action;
	tag_state state;

	git_libgit2_init();

	check_lg2(git_repository_open_ext(&repo, ".", 0, NULL),
			"Could not open repository", NULL);

	tag_options_init(&opts);
	parse_options(&action, &opts, argc, argv);

	state.repo = repo;
	state.opts = &opts;
	action(&state);

	git_repository_free(repo);
	git_libgit2_shutdown();

	return 0;
}
