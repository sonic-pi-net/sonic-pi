/*
 * libgit2 "log" example - shows how to walk history and get commit info
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
 * This example demonstrates the libgit2 rev walker APIs to roughly
 * simulate the output of `git log` and a few of command line arguments.
 * `git log` has many many options and this only shows a few of them.
 *
 * This does not have:
 *
 * - Robust error handling
 * - Colorized or paginated output formatting
 * - Most of the `git log` options
 *
 * This does have:
 *
 * - Examples of translating command line arguments to equivalent libgit2
 *   revwalker configuration calls
 * - Simplified options to apply pathspec limits and to show basic diffs
 */

/** log_state represents walker being configured while handling options */
struct log_state {
	git_repository *repo;
	const char *repodir;
	git_revwalk *walker;
	int hide;
	int sorting;
	int revisions;
};

/** utility functions that are called to configure the walker */
static void set_sorting(struct log_state *s, unsigned int sort_mode);
static void push_rev(struct log_state *s, git_object *obj, int hide);
static int add_revision(struct log_state *s, const char *revstr);

/** log_options holds other command line options that affect log output */
struct log_options {
	int show_diff;
	int show_log_size;
	int skip, limit;
	int min_parents, max_parents;
	git_time_t before;
	git_time_t after;
	const char *author;
	const char *committer;
	const char *grep;
};

/** utility functions that parse options and help with log output */
static int parse_options(
	struct log_state *s, struct log_options *opt, int argc, char **argv);
static void print_time(const git_time *intime, const char *prefix);
static void print_commit(git_commit *commit, struct log_options *opts);
static int match_with_parent(git_commit *commit, int i, git_diff_options *);

/** utility functions for filtering */
static int signature_matches(const git_signature *sig, const char *filter);
static int log_message_matches(const git_commit *commit, const char *filter);

int lg2_log(git_repository *repo, int argc, char *argv[])
{
	int i, count = 0, printed = 0, parents, last_arg;
	struct log_state s;
	struct log_options opt;
	git_diff_options diffopts = GIT_DIFF_OPTIONS_INIT;
	git_oid oid;
	git_commit *commit = NULL;
	git_pathspec *ps = NULL;

	/** Parse arguments and set up revwalker. */
	last_arg = parse_options(&s, &opt, argc, argv);
	s.repo = repo;

	diffopts.pathspec.strings = &argv[last_arg];
	diffopts.pathspec.count	  = argc - last_arg;
	if (diffopts.pathspec.count > 0)
		check_lg2(git_pathspec_new(&ps, &diffopts.pathspec),
			"Building pathspec", NULL);

	if (!s.revisions)
		add_revision(&s, NULL);

	/** Use the revwalker to traverse the history. */

	printed = count = 0;

	for (; !git_revwalk_next(&oid, s.walker); git_commit_free(commit)) {
		check_lg2(git_commit_lookup(&commit, s.repo, &oid),
			"Failed to look up commit", NULL);

		parents = (int)git_commit_parentcount(commit);
		if (parents < opt.min_parents)
			continue;
		if (opt.max_parents > 0 && parents > opt.max_parents)
			continue;

		if (diffopts.pathspec.count > 0) {
			int unmatched = parents;

			if (parents == 0) {
				git_tree *tree;
				check_lg2(git_commit_tree(&tree, commit), "Get tree", NULL);
				if (git_pathspec_match_tree(
						NULL, tree, GIT_PATHSPEC_NO_MATCH_ERROR, ps) != 0)
					unmatched = 1;
				git_tree_free(tree);
			} else if (parents == 1) {
				unmatched = match_with_parent(commit, 0, &diffopts) ? 0 : 1;
			} else {
				for (i = 0; i < parents; ++i) {
					if (match_with_parent(commit, i, &diffopts))
						unmatched--;
				}
			}

			if (unmatched > 0)
				continue;
		}

		if (!signature_matches(git_commit_author(commit), opt.author))
			continue;

		if (!signature_matches(git_commit_committer(commit), opt.committer))
			continue;

		if (!log_message_matches(commit, opt.grep))
			continue;

		if (count++ < opt.skip)
			continue;
		if (opt.limit != -1 && printed++ >= opt.limit) {
			git_commit_free(commit);
			break;
		}

		print_commit(commit, &opt);

		if (opt.show_diff) {
			git_tree *a = NULL, *b = NULL;
			git_diff *diff = NULL;

			if (parents > 1)
				continue;
			check_lg2(git_commit_tree(&b, commit), "Get tree", NULL);
			if (parents == 1) {
				git_commit *parent;
				check_lg2(git_commit_parent(&parent, commit, 0), "Get parent", NULL);
				check_lg2(git_commit_tree(&a, parent), "Tree for parent", NULL);
				git_commit_free(parent);
			}

			check_lg2(git_diff_tree_to_tree(
				&diff, git_commit_owner(commit), a, b, &diffopts),
				"Diff commit with parent", NULL);
			check_lg2(
                git_diff_print(diff, GIT_DIFF_FORMAT_PATCH, diff_output, NULL),
				"Displaying diff", NULL);

			git_diff_free(diff);
			git_tree_free(a);
			git_tree_free(b);
		}
	}

	git_pathspec_free(ps);
	git_revwalk_free(s.walker);

	return 0;
}

/** Determine if the given git_signature does not contain the filter text. */
static int signature_matches(const git_signature *sig, const char *filter) {
	if (filter == NULL)
		return 1;

	if (sig != NULL &&
		(strstr(sig->name, filter) != NULL ||
		strstr(sig->email, filter) != NULL))
		return 1;

	return 0;
}

static int log_message_matches(const git_commit *commit, const char *filter) {
	const char *message = NULL;

	if (filter == NULL)
		return 1;

	if ((message = git_commit_message(commit)) != NULL &&
		strstr(message, filter) != NULL)
		return 1;

	return 0;
}

/** Push object (for hide or show) onto revwalker. */
static void push_rev(struct log_state *s, git_object *obj, int hide)
{
	hide = s->hide ^ hide;

	/** Create revwalker on demand if it doesn't already exist. */
	if (!s->walker) {
		check_lg2(git_revwalk_new(&s->walker, s->repo),
			"Could not create revision walker", NULL);
		git_revwalk_sorting(s->walker, s->sorting);
	}

	if (!obj)
		check_lg2(git_revwalk_push_head(s->walker),
			"Could not find repository HEAD", NULL);
	else if (hide)
		check_lg2(git_revwalk_hide(s->walker, git_object_id(obj)),
			"Reference does not refer to a commit", NULL);
	else
		check_lg2(git_revwalk_push(s->walker, git_object_id(obj)),
			"Reference does not refer to a commit", NULL);

	git_object_free(obj);
}

/** Parse revision string and add revs to walker. */
static int add_revision(struct log_state *s, const char *revstr)
{
	git_revspec revs;
	int hide = 0;

	if (!revstr) {
		push_rev(s, NULL, hide);
		return 0;
	}

	if (*revstr == '^') {
		revs.flags = GIT_REVPARSE_SINGLE;
		hide = !hide;

		if (git_revparse_single(&revs.from, s->repo, revstr + 1) < 0)
			return -1;
	} else if (git_revparse(&revs, s->repo, revstr) < 0)
		return -1;

	if ((revs.flags & GIT_REVPARSE_SINGLE) != 0)
		push_rev(s, revs.from, hide);
	else {
		push_rev(s, revs.to, hide);

		if ((revs.flags & GIT_REVPARSE_MERGE_BASE) != 0) {
			git_oid base;
			check_lg2(git_merge_base(&base, s->repo,
				git_object_id(revs.from), git_object_id(revs.to)),
				"Could not find merge base", revstr);
			check_lg2(
				git_object_lookup(&revs.to, s->repo, &base, GIT_OBJECT_COMMIT),
				"Could not find merge base commit", NULL);

			push_rev(s, revs.to, hide);
		}

		push_rev(s, revs.from, !hide);
	}

	return 0;
}

/** Update revwalker with sorting mode. */
static void set_sorting(struct log_state *s, unsigned int sort_mode)
{
	/** Open repo on demand if it isn't already open. */
	if (!s->repo) {
		if (!s->repodir) s->repodir = ".";
		check_lg2(git_repository_open_ext(&s->repo, s->repodir, 0, NULL),
			"Could not open repository", s->repodir);
	}

	/** Create revwalker on demand if it doesn't already exist. */
	if (!s->walker)
		check_lg2(git_revwalk_new(&s->walker, s->repo),
			"Could not create revision walker", NULL);

	if (sort_mode == GIT_SORT_REVERSE)
		s->sorting = s->sorting ^ GIT_SORT_REVERSE;
	else
		s->sorting = sort_mode | (s->sorting & GIT_SORT_REVERSE);

	git_revwalk_sorting(s->walker, s->sorting);
}

/** Helper to format a git_time value like Git. */
static void print_time(const git_time *intime, const char *prefix)
{
	char sign, out[32];
	struct tm *intm;
	int offset, hours, minutes;
	time_t t;

	offset = intime->offset;
	if (offset < 0) {
		sign = '-';
		offset = -offset;
	} else {
		sign = '+';
	}

	hours   = offset / 60;
	minutes = offset % 60;

	t = (time_t)intime->time + (intime->offset * 60);

	intm = gmtime(&t);
	strftime(out, sizeof(out), "%a %b %e %T %Y", intm);

	printf("%s%s %c%02d%02d\n", prefix, out, sign, hours, minutes);
}

/** Helper to print a commit object. */
static void print_commit(git_commit *commit, struct log_options *opts)
{
	char buf[GIT_OID_HEXSZ + 1];
	int i, count;
	const git_signature *sig;
	const char *scan, *eol;

	git_oid_tostr(buf, sizeof(buf), git_commit_id(commit));
	printf("commit %s\n", buf);

	if (opts->show_log_size) {
		printf("log size %d\n", (int)strlen(git_commit_message(commit)));
	}

	if ((count = (int)git_commit_parentcount(commit)) > 1) {
		printf("Merge:");
		for (i = 0; i < count; ++i) {
			git_oid_tostr(buf, 8, git_commit_parent_id(commit, i));
			printf(" %s", buf);
		}
		printf("\n");
	}

	if ((sig = git_commit_author(commit)) != NULL) {
		printf("Author: %s <%s>\n", sig->name, sig->email);
		print_time(&sig->when, "Date:   ");
	}
	printf("\n");

	for (scan = git_commit_message(commit); scan && *scan; ) {
		for (eol = scan; *eol && *eol != '\n'; ++eol) /* find eol */;

		printf("    %.*s\n", (int)(eol - scan), scan);
		scan = *eol ? eol + 1 : NULL;
	}
	printf("\n");
}

/** Helper to find how many files in a commit changed from its nth parent. */
static int match_with_parent(git_commit *commit, int i, git_diff_options *opts)
{
	git_commit *parent;
	git_tree *a, *b;
	git_diff *diff;
	int ndeltas;

	check_lg2(
		git_commit_parent(&parent, commit, (size_t)i), "Get parent", NULL);
	check_lg2(git_commit_tree(&a, parent), "Tree for parent", NULL);
	check_lg2(git_commit_tree(&b, commit), "Tree for commit", NULL);
	check_lg2(
		git_diff_tree_to_tree(&diff, git_commit_owner(commit), a, b, opts),
		"Checking diff between parent and commit", NULL);

	ndeltas = (int)git_diff_num_deltas(diff);

	git_diff_free(diff);
	git_tree_free(a);
	git_tree_free(b);
	git_commit_free(parent);

	return ndeltas > 0;
}

/** Print a usage message for the program. */
static void usage(const char *message, const char *arg)
{
	if (message && arg)
		fprintf(stderr, "%s: %s\n", message, arg);
	else if (message)
		fprintf(stderr, "%s\n", message);
	fprintf(stderr, "usage: log [<options>]\n");
	exit(1);
}

/** Parse some log command line options. */
static int parse_options(
	struct log_state *s, struct log_options *opt, int argc, char **argv)
{
	struct args_info args = ARGS_INFO_INIT;

	memset(s, 0, sizeof(*s));
	s->sorting = GIT_SORT_TIME;

	memset(opt, 0, sizeof(*opt));
	opt->max_parents = -1;
	opt->limit = -1;

	for (args.pos = 1; args.pos < argc; ++args.pos) {
		const char *a = argv[args.pos];

		if (a[0] != '-') {
			if (!add_revision(s, a))
				s->revisions++;
			else
				/** Try failed revision parse as filename. */
				break;
		} else if (!match_arg_separator(&args)) {
			break;
		}
		else if (!strcmp(a, "--date-order"))
			set_sorting(s, GIT_SORT_TIME);
		else if (!strcmp(a, "--topo-order"))
			set_sorting(s, GIT_SORT_TOPOLOGICAL);
		else if (!strcmp(a, "--reverse"))
			set_sorting(s, GIT_SORT_REVERSE);
		else if (match_str_arg(&opt->author, &args, "--author"))
			/** Found valid --author */;
		else if (match_str_arg(&opt->committer, &args, "--committer"))
			/** Found valid --committer */;
		else if (match_str_arg(&opt->grep, &args, "--grep"))
			/** Found valid --grep */;
		else if (match_str_arg(&s->repodir, &args, "--git-dir"))
			/** Found git-dir. */;
		else if (match_int_arg(&opt->skip, &args, "--skip", 0))
			/** Found valid --skip. */;
		else if (match_int_arg(&opt->limit, &args, "--max-count", 0))
			/** Found valid --max-count. */;
		else if (a[1] >= '0' && a[1] <= '9')
			is_integer(&opt->limit, a + 1, 0);
		else if (match_int_arg(&opt->limit, &args, "-n", 0))
			/** Found valid -n. */;
		else if (!strcmp(a, "--merges"))
			opt->min_parents = 2;
		else if (!strcmp(a, "--no-merges"))
			opt->max_parents = 1;
		else if (!strcmp(a, "--no-min-parents"))
			opt->min_parents = 0;
		else if (!strcmp(a, "--no-max-parents"))
			opt->max_parents = -1;
		else if (match_int_arg(&opt->max_parents, &args, "--max-parents=", 1))
			/** Found valid --max-parents. */;
		else if (match_int_arg(&opt->min_parents, &args, "--min-parents=", 0))
			/** Found valid --min_parents. */;
		else if (!strcmp(a, "-p") || !strcmp(a, "-u") || !strcmp(a, "--patch"))
			opt->show_diff = 1;
		else if (!strcmp(a, "--log-size"))
			opt->show_log_size = 1;
		else
			usage("Unsupported argument", a);
	}

	return args.pos;
}

