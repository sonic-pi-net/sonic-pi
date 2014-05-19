/*
 * libgit2 "status" example - shows how to use the status APIs
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
#ifdef _WIN32
#define sleep(a) Sleep(a * 1000)
#else
#include <unistd.h>
#endif

/**
 * This example demonstrates the use of the libgit2 status APIs,
 * particularly the `git_status_list` object, to roughly simulate the
 * output of running `git status`.  It serves as a simple example of
 * using those APIs to get basic status information.
 *
 * This does not have:
 *
 * - Robust error handling
 * - Colorized or paginated output formatting
 *
 * This does have:
 *
 * - Examples of translating command line arguments to the status
 *   options settings to mimic `git status` results.
 * - A sample status formatter that matches the default "long" format
 *   from `git status`
 * - A sample status formatter that matches the "short" format
 */

enum {
	FORMAT_DEFAULT   = 0,
	FORMAT_LONG      = 1,
	FORMAT_SHORT     = 2,
	FORMAT_PORCELAIN = 3,
};

#define MAX_PATHSPEC 8

struct opts {
	git_status_options statusopt;
	char *repodir;
	char *pathspec[MAX_PATHSPEC];
	int npaths;
	int format;
	int zterm;
	int showbranch;
	int showsubmod;
	int repeat;
};

static void parse_opts(struct opts *o, int argc, char *argv[]);
static void show_branch(git_repository *repo, int format);
static void print_long(git_status_list *status);
static void print_short(git_repository *repo, git_status_list *status);
static int print_submod(git_submodule *sm, const char *name, void *payload);

int main(int argc, char *argv[])
{
	git_repository *repo = NULL;
	git_status_list *status;
	struct opts o = { GIT_STATUS_OPTIONS_INIT, "." };

	git_threads_init();

	o.statusopt.show  = GIT_STATUS_SHOW_INDEX_AND_WORKDIR;
	o.statusopt.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_RENAMES_HEAD_TO_INDEX |
		GIT_STATUS_OPT_SORT_CASE_SENSITIVELY;

	parse_opts(&o, argc, argv);

	/**
	 * Try to open the repository at the given path (or at the current
	 * directory if none was given).
	 */
	check_lg2(git_repository_open_ext(&repo, o.repodir, 0, NULL),
		  "Could not open repository", o.repodir);

	if (git_repository_is_bare(repo))
		fatal("Cannot report status on bare repository",
			git_repository_path(repo));

show_status:
	if (o.repeat)
		printf("\033[H\033[2J");

	/**
	 * Run status on the repository
	 *
	 * We use `git_status_list_new()` to generate a list of status
	 * information which lets us iterate over it at our
	 * convenience and extract the data we want to show out of
	 * each entry.
	 *
	 * You can use `git_status_foreach()` or
	 * `git_status_foreach_ext()` if you'd prefer to execute a
	 * callback for each entry. The latter gives you more control
	 * about what results are presented.
	 */
	check_lg2(git_status_list_new(&status, repo, &o.statusopt),
		"Could not get status", NULL);

	if (o.showbranch)
		show_branch(repo, o.format);

	if (o.showsubmod) {
		int submod_count = 0;
		check_lg2(git_submodule_foreach(repo, print_submod, &submod_count),
			"Cannot iterate submodules", o.repodir);
	}

	if (o.format == FORMAT_LONG)
		print_long(status);
	else
		print_short(repo, status);

	git_status_list_free(status);

	if (o.repeat) {
		sleep(o.repeat);
		goto show_status;
	}

	git_repository_free(repo);
	git_threads_shutdown();

	return 0;
}

/**
 * If the user asked for the branch, let's show the short name of the
 * branch.
 */
static void show_branch(git_repository *repo, int format)
{
	int error = 0;
	const char *branch = NULL;
	git_reference *head = NULL;

	error = git_repository_head(&head, repo);

	if (error == GIT_EUNBORNBRANCH || error == GIT_ENOTFOUND)
		branch = NULL;
	else if (!error) {
		branch = git_reference_shorthand(head);
	} else
		check_lg2(error, "failed to get current branch", NULL);

	if (format == FORMAT_LONG)
		printf("# On branch %s\n",
			branch ? branch : "Not currently on any branch.");
	else
		printf("## %s\n", branch ? branch : "HEAD (no branch)");

	git_reference_free(head);
}

/**
 * This function print out an output similar to git's status command
 * in long form, including the command-line hints.
 */
static void print_long(git_status_list *status)
{
	size_t i, maxi = git_status_list_entrycount(status);
	const git_status_entry *s;
	int header = 0, changes_in_index = 0;
	int changed_in_workdir = 0, rm_in_workdir = 0;
	const char *old_path, *new_path;

	/** Print index changes. */

	for (i = 0; i < maxi; ++i) {
		char *istatus = NULL;

		s = git_status_byindex(status, i);

		if (s->status == GIT_STATUS_CURRENT)
			continue;

		if (s->status & GIT_STATUS_WT_DELETED)
			rm_in_workdir = 1;

		if (s->status & GIT_STATUS_INDEX_NEW)
			istatus = "new file: ";
		if (s->status & GIT_STATUS_INDEX_MODIFIED)
			istatus = "modified: ";
		if (s->status & GIT_STATUS_INDEX_DELETED)
			istatus = "deleted:  ";
		if (s->status & GIT_STATUS_INDEX_RENAMED)
			istatus = "renamed:  ";
		if (s->status & GIT_STATUS_INDEX_TYPECHANGE)
			istatus = "typechange:";

		if (istatus == NULL)
			continue;

		if (!header) {
			printf("# Changes to be committed:\n");
			printf("#   (use \"git reset HEAD <file>...\" to unstage)\n");
			printf("#\n");
			header = 1;
		}

		old_path = s->head_to_index->old_file.path;
		new_path = s->head_to_index->new_file.path;

		if (old_path && new_path && strcmp(old_path, new_path))
			printf("#\t%s  %s -> %s\n", istatus, old_path, new_path);
		else
			printf("#\t%s  %s\n", istatus, old_path ? old_path : new_path);
	}

	if (header) {
		changes_in_index = 1;
		printf("#\n");
	}
	header = 0;

	/** Print workdir changes to tracked files. */

	for (i = 0; i < maxi; ++i) {
		char *wstatus = NULL;

		s = git_status_byindex(status, i);

		/**
		 * With `GIT_STATUS_OPT_INCLUDE_UNMODIFIED` (not used in this example)
		 * `index_to_workdir` may not be `NULL` even if there are
		 * no differences, in which case it will be a `GIT_DELTA_UNMODIFIED`.
		 */
		if (s->status == GIT_STATUS_CURRENT || s->index_to_workdir == NULL)
			continue;

		/** Print out the output since we know the file has some changes */
		if (s->status & GIT_STATUS_WT_MODIFIED)
			wstatus = "modified: ";
		if (s->status & GIT_STATUS_WT_DELETED)
			wstatus = "deleted:  ";
		if (s->status & GIT_STATUS_WT_RENAMED)
			wstatus = "renamed:  ";
		if (s->status & GIT_STATUS_WT_TYPECHANGE)
			wstatus = "typechange:";

		if (wstatus == NULL)
			continue;

		if (!header) {
			printf("# Changes not staged for commit:\n");
			printf("#   (use \"git add%s <file>...\" to update what will be committed)\n", rm_in_workdir ? "/rm" : "");
			printf("#   (use \"git checkout -- <file>...\" to discard changes in working directory)\n");
			printf("#\n");
			header = 1;
		}

		old_path = s->index_to_workdir->old_file.path;
		new_path = s->index_to_workdir->new_file.path;

		if (old_path && new_path && strcmp(old_path, new_path))
			printf("#\t%s  %s -> %s\n", wstatus, old_path, new_path);
		else
			printf("#\t%s  %s\n", wstatus, old_path ? old_path : new_path);
	}

	if (header) {
		changed_in_workdir = 1;
		printf("#\n");
	}

	/** Print untracked files. */

	header = 0;

	for (i = 0; i < maxi; ++i) {
		s = git_status_byindex(status, i);

		if (s->status == GIT_STATUS_WT_NEW) {

			if (!header) {
				printf("# Untracked files:\n");
				printf("#   (use \"git add <file>...\" to include in what will be committed)\n");
				printf("#\n");
				header = 1;
			}

			printf("#\t%s\n", s->index_to_workdir->old_file.path);
		}
	}

	header = 0;

	/** Print ignored files. */

	for (i = 0; i < maxi; ++i) {
		s = git_status_byindex(status, i);

		if (s->status == GIT_STATUS_IGNORED) {

			if (!header) {
				printf("# Ignored files:\n");
				printf("#   (use \"git add -f <file>...\" to include in what will be committed)\n");
				printf("#\n");
				header = 1;
			}

			printf("#\t%s\n", s->index_to_workdir->old_file.path);
		}
	}

	if (!changes_in_index && changed_in_workdir)
		printf("no changes added to commit (use \"git add\" and/or \"git commit -a\")\n");
}

/**
 * This version of the output prefixes each path with two status
 * columns and shows submodule status information.
 */
static void print_short(git_repository *repo, git_status_list *status)
{
	size_t i, maxi = git_status_list_entrycount(status);
	const git_status_entry *s;
	char istatus, wstatus;
	const char *extra, *a, *b, *c;

	for (i = 0; i < maxi; ++i) {
		s = git_status_byindex(status, i);

		if (s->status == GIT_STATUS_CURRENT)
			continue;

		a = b = c = NULL;
		istatus = wstatus = ' ';
		extra = "";

		if (s->status & GIT_STATUS_INDEX_NEW)
			istatus = 'A';
		if (s->status & GIT_STATUS_INDEX_MODIFIED)
			istatus = 'M';
		if (s->status & GIT_STATUS_INDEX_DELETED)
			istatus = 'D';
		if (s->status & GIT_STATUS_INDEX_RENAMED)
			istatus = 'R';
		if (s->status & GIT_STATUS_INDEX_TYPECHANGE)
			istatus = 'T';

		if (s->status & GIT_STATUS_WT_NEW) {
			if (istatus == ' ')
				istatus = '?';
			wstatus = '?';
		}
		if (s->status & GIT_STATUS_WT_MODIFIED)
			wstatus = 'M';
		if (s->status & GIT_STATUS_WT_DELETED)
			wstatus = 'D';
		if (s->status & GIT_STATUS_WT_RENAMED)
			wstatus = 'R';
		if (s->status & GIT_STATUS_WT_TYPECHANGE)
			wstatus = 'T';

		if (s->status & GIT_STATUS_IGNORED) {
			istatus = '!';
			wstatus = '!';
		}

		if (istatus == '?' && wstatus == '?')
			continue;

		/**
		 * A commit in a tree is how submodules are stored, so
		 * let's go take a look at its status.
		 */
		if (s->index_to_workdir &&
			s->index_to_workdir->new_file.mode == GIT_FILEMODE_COMMIT)
		{
			git_submodule *sm = NULL;
			unsigned int smstatus = 0;

			if (!git_submodule_lookup(
					&sm, repo, s->index_to_workdir->new_file.path)) {

				if (!git_submodule_status(&smstatus, sm)) {
					if (smstatus & GIT_SUBMODULE_STATUS_WD_MODIFIED)
						extra = " (new commits)";
					else if (smstatus & GIT_SUBMODULE_STATUS_WD_INDEX_MODIFIED)
						extra = " (modified content)";
					else if (smstatus & GIT_SUBMODULE_STATUS_WD_WD_MODIFIED)
						extra = " (modified content)";
					else if (smstatus & GIT_SUBMODULE_STATUS_WD_UNTRACKED)
						extra = " (untracked content)";
				}
			}

			git_submodule_free(sm);
		}

		/**
		 * Now that we have all the information, format the output.
		 */

		if (s->head_to_index) {
			a = s->head_to_index->old_file.path;
			b = s->head_to_index->new_file.path;
		}
		if (s->index_to_workdir) {
			if (!a)
				a = s->index_to_workdir->old_file.path;
			if (!b)
				b = s->index_to_workdir->old_file.path;
			c = s->index_to_workdir->new_file.path;
		}

		if (istatus == 'R') {
			if (wstatus == 'R')
				printf("%c%c %s %s %s%s\n", istatus, wstatus, a, b, c, extra);
			else
				printf("%c%c %s %s%s\n", istatus, wstatus, a, b, extra);
		} else {
			if (wstatus == 'R')
				printf("%c%c %s %s%s\n", istatus, wstatus, a, c, extra);
			else
				printf("%c%c %s%s\n", istatus, wstatus, a, extra);
		}
	}

	for (i = 0; i < maxi; ++i) {
		s = git_status_byindex(status, i);

		if (s->status == GIT_STATUS_WT_NEW)
			printf("?? %s\n", s->index_to_workdir->old_file.path);
	}
}

static int print_submod(git_submodule *sm, const char *name, void *payload)
{
	int *count = payload;
	(void)name;

	if (*count == 0)
		printf("# Submodules\n");
	(*count)++;

	printf("# - submodule '%s' at %s\n",
		git_submodule_name(sm), git_submodule_path(sm));

	return 0;
}

/**
 * Parse options that git's status command supports.
 */
static void parse_opts(struct opts *o, int argc, char *argv[])
{
	struct args_info args = ARGS_INFO_INIT;

	for (args.pos = 1; args.pos < argc; ++args.pos) {
		char *a = argv[args.pos];

		if (a[0] != '-') {
			if (o->npaths < MAX_PATHSPEC)
				o->pathspec[o->npaths++] = a;
			else
				fatal("Example only supports a limited pathspec", NULL);
		}
		else if (!strcmp(a, "-s") || !strcmp(a, "--short"))
			o->format = FORMAT_SHORT;
		else if (!strcmp(a, "--long"))
			o->format = FORMAT_LONG;
		else if (!strcmp(a, "--porcelain"))
			o->format = FORMAT_PORCELAIN;
		else if (!strcmp(a, "-b") || !strcmp(a, "--branch"))
			o->showbranch = 1;
		else if (!strcmp(a, "-z")) {
			o->zterm = 1;
			if (o->format == FORMAT_DEFAULT)
				o->format = FORMAT_PORCELAIN;
		}
		else if (!strcmp(a, "--ignored"))
			o->statusopt.flags |= GIT_STATUS_OPT_INCLUDE_IGNORED;
		else if (!strcmp(a, "-uno") ||
				 !strcmp(a, "--untracked-files=no"))
			o->statusopt.flags &= ~GIT_STATUS_OPT_INCLUDE_UNTRACKED;
		else if (!strcmp(a, "-unormal") ||
				 !strcmp(a, "--untracked-files=normal"))
			o->statusopt.flags |= GIT_STATUS_OPT_INCLUDE_UNTRACKED;
		else if (!strcmp(a, "-uall") ||
				 !strcmp(a, "--untracked-files=all"))
			o->statusopt.flags |= GIT_STATUS_OPT_INCLUDE_UNTRACKED |
				GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;
		else if (!strcmp(a, "--ignore-submodules=all"))
			o->statusopt.flags |= GIT_STATUS_OPT_EXCLUDE_SUBMODULES;
		else if (!strncmp(a, "--git-dir=", strlen("--git-dir=")))
			o->repodir = a + strlen("--git-dir=");
		else if (!strcmp(a, "--repeat"))
			o->repeat = 10;
		else if (match_int_arg(&o->repeat, &args, "--repeat", 0))
			/* okay */;
		else if (!strcmp(a, "--list-submodules"))
			o->showsubmod = 1;
		else
			check_lg2(-1, "Unsupported option", a);
	}

	if (o->format == FORMAT_DEFAULT)
		o->format = FORMAT_LONG;
	if (o->format == FORMAT_LONG)
		o->showbranch = 1;
	if (o->npaths > 0) {
		o->statusopt.pathspec.strings = o->pathspec;
		o->statusopt.pathspec.count   = o->npaths;
	}
}
