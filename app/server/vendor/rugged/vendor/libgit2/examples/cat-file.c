/*
 * libgit2 "cat-file" example - shows how to print data from the ODB
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

static void print_signature(const char *header, const git_signature *sig)
{
	char sign;
	int offset, hours, minutes;

	if (!sig)
		return;

	offset = sig->when.offset;
	if (offset < 0) {
		sign = '-';
		offset = -offset;
	} else {
		sign = '+';
	}

	hours   = offset / 60;
	minutes = offset % 60;

	printf("%s %s <%s> %ld %c%02d%02d\n",
		   header, sig->name, sig->email, (long)sig->when.time,
		   sign, hours, minutes);
}

/** Printing out a blob is simple, get the contents and print */
static void show_blob(const git_blob *blob)
{
	/* ? Does this need crlf filtering? */
	fwrite(git_blob_rawcontent(blob), (size_t)git_blob_rawsize(blob), 1, stdout);
}

/** Show each entry with its type, id and attributes */
static void show_tree(const git_tree *tree)
{
	size_t i, max_i = (int)git_tree_entrycount(tree);
	char oidstr[GIT_OID_HEXSZ + 1];
	const git_tree_entry *te;

	for (i = 0; i < max_i; ++i) {
		te = git_tree_entry_byindex(tree, i);

		git_oid_tostr(oidstr, sizeof(oidstr), git_tree_entry_id(te));

		printf("%06o %s %s\t%s\n",
			git_tree_entry_filemode(te),
			git_object_type2string(git_tree_entry_type(te)),
			oidstr, git_tree_entry_name(te));
	}
}

/**
 * Commits and tags have a few interesting fields in their header.
 */
static void show_commit(const git_commit *commit)
{
	unsigned int i, max_i;
	char oidstr[GIT_OID_HEXSZ + 1];

	git_oid_tostr(oidstr, sizeof(oidstr), git_commit_tree_id(commit));
	printf("tree %s\n", oidstr);

	max_i = (unsigned int)git_commit_parentcount(commit);
	for (i = 0; i < max_i; ++i) {
		git_oid_tostr(oidstr, sizeof(oidstr), git_commit_parent_id(commit, i));
		printf("parent %s\n", oidstr);
	}

	print_signature("author", git_commit_author(commit));
	print_signature("committer", git_commit_committer(commit));

	if (git_commit_message(commit))
		printf("\n%s\n", git_commit_message(commit));
}

static void show_tag(const git_tag *tag)
{
	char oidstr[GIT_OID_HEXSZ + 1];

	git_oid_tostr(oidstr, sizeof(oidstr), git_tag_target_id(tag));;
	printf("object %s\n", oidstr);
	printf("type %s\n", git_object_type2string(git_tag_target_type(tag)));
	printf("tag %s\n", git_tag_name(tag));
	print_signature("tagger", git_tag_tagger(tag));

	if (git_tag_message(tag))
		printf("\n%s\n", git_tag_message(tag));
}

enum {
	SHOW_TYPE = 1,
	SHOW_SIZE = 2,
	SHOW_NONE = 3,
	SHOW_PRETTY = 4
};

/* Forward declarations for option-parsing helper */
struct opts {
	const char *dir;
	const char *rev;
	int action;
	int verbose;
};
static void parse_opts(struct opts *o, int argc, char *argv[]);


/** Entry point for this command */
int main(int argc, char *argv[])
{
	git_repository *repo;
	struct opts o = { ".", NULL, 0, 0 };
	git_object *obj = NULL;
	char oidstr[GIT_OID_HEXSZ + 1];

	git_threads_init();

	parse_opts(&o, argc, argv);

	check_lg2(git_repository_open_ext(&repo, o.dir, 0, NULL),
			"Could not open repository", NULL);
	check_lg2(git_revparse_single(&obj, repo, o.rev),
			"Could not resolve", o.rev);

	if (o.verbose) {
		char oidstr[GIT_OID_HEXSZ + 1];
		git_oid_tostr(oidstr, sizeof(oidstr), git_object_id(obj));

		printf("%s %s\n--\n",
			git_object_type2string(git_object_type(obj)), oidstr);
	}

	switch (o.action) {
	case SHOW_TYPE:
		printf("%s\n", git_object_type2string(git_object_type(obj)));
		break;
	case SHOW_SIZE: {
		git_odb *odb;
		git_odb_object *odbobj;

		check_lg2(git_repository_odb(&odb, repo), "Could not open ODB", NULL);
		check_lg2(git_odb_read(&odbobj, odb, git_object_id(obj)),
			"Could not find obj", NULL);

		printf("%ld\n", (long)git_odb_object_size(odbobj));

		git_odb_object_free(odbobj);
		git_odb_free(odb);
		}
		break;
	case SHOW_NONE:
		/* just want return result */
		break;
	case SHOW_PRETTY:

		switch (git_object_type(obj)) {
		case GIT_OBJ_BLOB:
			show_blob((const git_blob *)obj);
			break;
		case GIT_OBJ_COMMIT:
			show_commit((const git_commit *)obj);
			break;
		case GIT_OBJ_TREE:
			show_tree((const git_tree *)obj);
			break;
		case GIT_OBJ_TAG:
			show_tag((const git_tag *)obj);
			break;
		default:
			printf("unknown %s\n", oidstr);
			break;
		}
		break;
	}

	git_object_free(obj);
	git_repository_free(repo);

	git_threads_shutdown();

	return 0;
}

/** Print out usage information */
static void usage(const char *message, const char *arg)
{
	if (message && arg)
		fprintf(stderr, "%s: %s\n", message, arg);
	else if (message)
		fprintf(stderr, "%s\n", message);
	fprintf(stderr,
			"usage: cat-file (-t | -s | -e | -p) [-v] [-q] "
			"[-h|--help] [--git-dir=<dir>] <object>\n");
	exit(1);
}

/** Parse the command-line options taken from git */
static void parse_opts(struct opts *o, int argc, char *argv[])
{
	struct args_info args = ARGS_INFO_INIT;

	for (args.pos = 1; args.pos < argc; ++args.pos) {
		char *a = argv[args.pos];

		if (a[0] != '-') {
			if (o->rev != NULL)
				usage("Only one rev should be provided", NULL);
			else
				o->rev = a;
		}
		else if (!strcmp(a, "-t"))
			o->action = SHOW_TYPE;
		else if (!strcmp(a, "-s"))
			o->action = SHOW_SIZE;
		else if (!strcmp(a, "-e"))
			o->action = SHOW_NONE;
		else if (!strcmp(a, "-p"))
			o->action = SHOW_PRETTY;
		else if (!strcmp(a, "-q"))
			o->verbose = 0;
		else if (!strcmp(a, "-v"))
			o->verbose = 1;
		else if (!strcmp(a, "--help") || !strcmp(a, "-h"))
			usage(NULL, NULL);
		else if (!match_str_arg(&o->dir, &args, "--git-dir"))
			usage("Unknown option", a);
	}

	if (!o->action || !o->rev)
		usage(NULL, NULL);

}
