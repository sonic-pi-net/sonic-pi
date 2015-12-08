#include "clar_libgit2.h"
#include "git2/rebase.h"
#include "merge.h"
#include "posix.h"
#include "annotated_commit.h"

#include <fcntl.h>

static git_repository *repo;

// Fixture setup and teardown
void test_rebase_abort__initialize(void)
{
	repo = cl_git_sandbox_init("rebase");
}

void test_rebase_abort__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void test_abort(git_annotated_commit *branch, git_annotated_commit *onto)
{
	git_rebase *rebase;
	git_reference *head_ref, *branch_ref = NULL;
	git_status_list *statuslist;
	git_reflog *reflog;
	const git_reflog_entry *reflog_entry;

	cl_git_pass(git_rebase_open(&rebase, repo, NULL));
	cl_git_pass(git_rebase_abort(rebase));

	cl_assert_equal_i(GIT_REPOSITORY_STATE_NONE, git_repository_state(repo));

	/* Make sure the refs are updated appropriately */
	cl_git_pass(git_reference_lookup(&head_ref, repo, "HEAD"));

	if (branch->ref_name == NULL)
		cl_assert_equal_oid(git_annotated_commit_id(branch), git_reference_target(head_ref));
	else {
		cl_assert_equal_s("refs/heads/beef", git_reference_symbolic_target(head_ref));
		cl_git_pass(git_reference_lookup(&branch_ref, repo, git_reference_symbolic_target(head_ref)));
		cl_assert_equal_oid(git_annotated_commit_id(branch), git_reference_target(branch_ref));
	}

	git_status_list_new(&statuslist, repo, NULL);
	cl_assert_equal_i(0, git_status_list_entrycount(statuslist));
	git_status_list_free(statuslist);

	/* Make sure the reflogs are updated appropriately */
	cl_git_pass(git_reflog_read(&reflog, repo, "HEAD"));

	cl_assert(reflog_entry = git_reflog_entry_byindex(reflog, 0));
	cl_assert_equal_oid(git_annotated_commit_id(onto), git_reflog_entry_id_old(reflog_entry));
	cl_assert_equal_oid(git_annotated_commit_id(branch), git_reflog_entry_id_new(reflog_entry));
	cl_assert_equal_s("rebase: aborting", git_reflog_entry_message(reflog_entry));

	git_reflog_free(reflog);
	git_reference_free(head_ref);
	git_reference_free(branch_ref);
	git_rebase_free(rebase);
}

void test_rebase_abort__merge(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *onto_ref;
	git_annotated_commit *branch_head, *onto_head;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/beef"));
	cl_git_pass(git_reference_lookup(&onto_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&onto_head, repo, onto_ref));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, NULL, onto_head, NULL));
	cl_assert_equal_i(GIT_REPOSITORY_STATE_REBASE_MERGE, git_repository_state(repo));

	test_abort(branch_head, onto_head);

	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(onto_head);

	git_reference_free(branch_ref);
	git_reference_free(onto_ref);
	git_rebase_free(rebase);
}

void test_rebase_abort__detached_head(void)
{
	git_rebase *rebase;
	git_oid branch_id;
	git_reference *onto_ref;
	git_signature *signature;
	git_annotated_commit *branch_head, *onto_head;

	git_oid_fromstr(&branch_id, "b146bd7608eac53d9bf9e1a6963543588b555c64");
	cl_git_pass(git_reference_lookup(&onto_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_lookup(&branch_head, repo, &branch_id));
	cl_git_pass(git_annotated_commit_from_ref(&onto_head, repo, onto_ref));

	cl_git_pass(git_signature_new(&signature, "Rebaser", "rebaser@example.com", 1404157834, -400));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, NULL, onto_head, NULL));
	cl_assert_equal_i(GIT_REPOSITORY_STATE_REBASE_MERGE, git_repository_state(repo));

	test_abort(branch_head, onto_head);

	git_signature_free(signature);

	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(onto_head);

	git_reference_free(onto_ref);
	git_rebase_free(rebase);
}

void test_rebase_abort__old_style_head_file(void)
{
	git_rebase *rebase;
	git_reference *branch_ref, *onto_ref;
	git_signature *signature;
	git_annotated_commit *branch_head, *onto_head;

	cl_git_pass(git_reference_lookup(&branch_ref, repo, "refs/heads/beef"));
	cl_git_pass(git_reference_lookup(&onto_ref, repo, "refs/heads/master"));

	cl_git_pass(git_annotated_commit_from_ref(&branch_head, repo, branch_ref));
	cl_git_pass(git_annotated_commit_from_ref(&onto_head, repo, onto_ref));

	cl_git_pass(git_signature_new(&signature, "Rebaser", "rebaser@example.com", 1404157834, -400));

	cl_git_pass(git_rebase_init(&rebase, repo, branch_head, NULL, onto_head, NULL));
	cl_assert_equal_i(GIT_REPOSITORY_STATE_REBASE_MERGE, git_repository_state(repo));

	p_rename("rebase-merge/.git/rebase-merge/orig-head",
		"rebase-merge/.git/rebase-merge/head");

	test_abort(branch_head, onto_head);

	git_signature_free(signature);

	git_annotated_commit_free(branch_head);
	git_annotated_commit_free(onto_head);

	git_reference_free(branch_ref);
	git_reference_free(onto_ref);
	git_rebase_free(rebase);
}
