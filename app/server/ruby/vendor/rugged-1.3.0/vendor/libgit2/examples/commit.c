/*
 * libgit2 "commit" example - shows how to create a git commit
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
 * This example demonstrates the libgit2 commit APIs to roughly
 * simulate `git commit` with the commit message argument.
 *
 * This does not have:
 *
 * - Robust error handling
 * - Most of the `git commit` options
 *
 * This does have:
 *
 * - Example of performing a git commit with a comment
 * 
 */
int lg2_commit(git_repository *repo, int argc, char **argv)
{
	const char *opt = argv[1];
	const char *comment = argv[2];
	int error;

	git_oid commit_oid,tree_oid;
	git_tree *tree;
	git_index *index;	
	git_object *parent = NULL;
	git_reference *ref = NULL;
	git_signature *signature;	

	/* Validate args */
	if (argc < 3 || strcmp(opt, "-m") != 0) {
		printf ("USAGE: %s -m <comment>\n", argv[0]);
		return -1;
	}

	error = git_revparse_ext(&parent, &ref, repo, "HEAD");
	if (error == GIT_ENOTFOUND) {
		printf("HEAD not found. Creating first commit\n");
		error = 0;
	} else if (error != 0) {
		const git_error *err = git_error_last();
		if (err) printf("ERROR %d: %s\n", err->klass, err->message);
		else printf("ERROR %d: no detailed info\n", error);
	}

	check_lg2(git_repository_index(&index, repo), "Could not open repository index", NULL);
	check_lg2(git_index_write_tree(&tree_oid, index), "Could not write tree", NULL);;
	check_lg2(git_index_write(index), "Could not write index", NULL);;

	check_lg2(git_tree_lookup(&tree, repo, &tree_oid), "Error looking up tree", NULL);
	
	check_lg2(git_signature_default(&signature, repo), "Error creating signature", NULL);
	
	check_lg2(git_commit_create_v(
		&commit_oid,
		repo,
		"HEAD",
		signature,
		signature,
		NULL,
		comment,
		tree,
		parent ? 1 : 0, parent), "Error creating commit", NULL);

	git_index_free(index);
	git_signature_free(signature);
	git_tree_free(tree);	

	return error;
}
