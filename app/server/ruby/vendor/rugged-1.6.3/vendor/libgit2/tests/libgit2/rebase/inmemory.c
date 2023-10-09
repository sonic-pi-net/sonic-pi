#include "clar_libgit2.h"
#include "git2/rebase.h"
#include "posix.h"

#include <fcntl.h>

static git_repository *repo;
static git_signature *signature;

/* Fixture setup and teardown */
void test_rebase_inmemory__initialize(void)
{
	repo = cl_git_sandbox_init("rebase");

	cl_git_pass(git_signature_new(&signature,
		"Rebaser", "rebaser@rebaser.rb", 1405694510, 0));
}

void test_rebase_inmemory__cleanup(void)
{
	git_signature_free(signature);
	cl_git_sandbox_cleanup();
}

void test_rebase_inmemory__not_in_rebase_state(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_options opts = GIT_REBASE_OPTIONS_INIT;

	opts.inmemory = true;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/beef"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, &opts));

	cl_assert_equal_i(GIT_REPOSITORY_STATE_NONE, git_repository_state(repo));

	git_rebase_free(rebase);

	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);

	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
}

void test_rebase_inmemory__can_resolve_conflicts(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_status_list *status_list;
	git_oid pick_id, commit_id, expected_commit_id;
	git_index *rebase_index, *repo_index;
	git_index_entry resolution = {{0}};
	git_rebase_options opts = GIT_REBASE_OPTIONS_INIT;

	opts.inmemory = true;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/asparagus"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, &opts));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));

	git_oid__fromstr(&pick_id, "33f915f9e4dbd9f4b24430e48731a59b45b15500", GIT_OID_SHA1);

	cl_assert_equal_i(GIT_REBASE_OPERATION_PICK, rebase_operation->type);
	cl_assert_equal_oid(&pick_id, &rebase_operation->id);

	/* ensure that we did not do anything stupid to the workdir or repo index */
	cl_git_pass(git_repository_index(&repo_index, repo));
	cl_assert(!git_index_has_conflicts(repo_index));

	cl_git_pass(git_status_list_new(&status_list, repo, NULL));
	cl_assert_equal_i(0, git_status_list_entrycount(status_list));

	/* but that the index returned from rebase does have conflicts */
	cl_git_pass(git_rebase_inmemory_index(&rebase_index, rebase));
	cl_assert(git_index_has_conflicts(rebase_index));

	cl_git_fail_with(GIT_EUNMERGED, git_rebase_commit(&commit_id, rebase, NULL, signature, NULL, NULL));

	/* ensure that we can work with the in-memory index to resolve the conflict */
	resolution.path = "asparagus.txt";
	resolution.mode = GIT_FILEMODE_BLOB;
	git_oid__fromstr(&resolution.id, "414dfc71ead79c07acd4ea47fecf91f289afc4b9", GIT_OID_SHA1);
	cl_git_pass(git_index_conflict_remove(rebase_index, "asparagus.txt"));
	cl_git_pass(git_index_add(rebase_index, &resolution));

	/* and finally create a commit for the resolved rebase operation */
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature, NULL, NULL));

	cl_git_pass(git_oid__fromstr(&expected_commit_id, "db7af47222181e548810da2ab5fec0e9357c5637", GIT_OID_SHA1));
	cl_assert_equal_oid(&commit_id, &expected_commit_id);

	git_status_list_free(status_list);
	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_index_free(repo_index);
	git_index_free(rebase_index);
	git_rebase_free(rebase);
}

void test_rebase_inmemory__no_common_ancestor(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_oid commit_id, expected_final_id;
	git_rebase_options opts = GIT_REBASE_OPTIONS_INIT;

	opts.inmemory = true;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/barley"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, &opts));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_finish(rebase, signature));

	git_oid__fromstr(&expected_final_id, "71e7ee8d4fe7d8bf0d107355197e0a953dfdb7f3", GIT_OID_SHA1);
	cl_assert_equal_oid(&expected_final_id, &commit_id);

	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}

void test_rebase_inmemory__with_directories(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *upstream_ref;
	git_annotated_commit *branch_head, *upstream_head;
	git_rebase_operation *rebase_operation;
	git_oid commit_id, tree_id;
	git_commit *commit;
	git_rebase_options opts = GIT_REBASE_OPTIONS_INIT;

	opts.inmemory = true;

	git_oid__fromstr(&tree_id, "a4d6d9c3d57308fd8e320cf2525bae8f1adafa57", GIT_OID_SHA1);

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/deep_gravy"));
	cl_git_pass(git_reference_lookup(&upstream_ref, repo, "refs/heads/veal"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&upstream_head, repo, upstream_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, upstream_head, NULL, &opts));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_pass(git_rebase_next(&rebase_operation, rebase));
	cl_git_pass(git_rebase_commit(&commit_id, rebase, NULL, signature,
		NULL, NULL));

	cl_git_fail_with(GIT_ITEROVER, git_rebase_next(&rebase_operation, rebase));

	cl_git_pass(git_commit_lookup(&commit, repo, &commit_id));
	cl_assert_equal_oid(&tree_id, git_commit_tree_id(commit));

	git_commit_free(commit);
	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(upstream_head);
	git_reference_free(branch_ref);
	git_reference_free(upstream_ref);
	git_rebase_free(rebase);
}
