#include "clar_libgit2.h"

static const char *committer_name = "Vicent Marti";
static const char *committer_email = "vicent@github.com";
static const char *commit_message = "This commit has been created in memory\n\
   This is a commit created in memory and it will be written back to disk\n";
static const char *tree_oid = "1810dff58d8a660512d4832e740f692884338ccd";
static const char *root_commit_message = "This is a root commit\n\
   This is a root commit and should be the only one in this branch\n";
static const char *root_reflog_message = "commit (initial): This is a root commit \
   This is a root commit and should be the only one in this branch";
static char *head_old;
static git_reference *head, *branch;
static git_commit *commit;

// Fixture setup
static git_repository *g_repo;
void test_commit_write__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
}

void test_commit_write__cleanup(void)
{
	git_reference_free(head);
	head = NULL;

	git_reference_free(branch);
	branch = NULL;

	git_commit_free(commit);
	commit = NULL;

	git__free(head_old);
	head_old = NULL;

	cl_git_sandbox_cleanup();
}


// write a new commit object from memory to disk
void test_commit_write__from_memory(void)
{
   git_oid tree_id, parent_id, commit_id;
   git_signature *author, *committer;
   const git_signature *author1, *committer1;
   git_commit *parent;
   git_tree *tree;
   const char *commit_id_str = "8496071c1b46c854b31185ea97743be6a8774479";

   git_oid_fromstr(&tree_id, tree_oid);
   cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

   git_oid_fromstr(&parent_id, commit_id_str);
   cl_git_pass(git_commit_lookup(&parent, g_repo, &parent_id));

   /* create signatures */
   cl_git_pass(git_signature_new(&committer, committer_name, committer_email, 123456789, 60));
   cl_git_pass(git_signature_new(&author, committer_name, committer_email, 987654321, 90));

   cl_git_pass(git_commit_create_v(
      &commit_id, /* out id */
      g_repo,
      NULL, /* do not update the HEAD */
      author,
      committer,
      NULL,
      commit_message,
      tree,
      1, parent));

   git_object_free((git_object *)parent);
   git_object_free((git_object *)tree);

   git_signature_free(committer);
   git_signature_free(author);

   cl_git_pass(git_commit_lookup(&commit, g_repo, &commit_id));

   /* Check attributes were set correctly */
   author1 = git_commit_author(commit);
   cl_assert(author1 != NULL);
   cl_assert_equal_s(committer_name, author1->name);
   cl_assert_equal_s(committer_email, author1->email);
   cl_assert(author1->when.time == 987654321);
   cl_assert(author1->when.offset == 90);

   committer1 = git_commit_committer(commit);
   cl_assert(committer1 != NULL);
   cl_assert_equal_s(committer_name, committer1->name);
   cl_assert_equal_s(committer_email, committer1->email);
   cl_assert(committer1->when.time == 123456789);
   cl_assert(committer1->when.offset == 60);

   cl_assert_equal_s(commit_message, git_commit_message(commit));
}

// create a root commit
void test_commit_write__root(void)
{
	git_oid tree_id, commit_id;
	const git_oid *branch_oid;
	git_signature *author, *committer;
	const char *branch_name = "refs/heads/root-commit-branch";
	git_tree *tree;
	git_reflog *log;
	const git_reflog_entry *entry;

	git_oid_fromstr(&tree_id, tree_oid);
	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

	/* create signatures */
	cl_git_pass(git_signature_new(&committer, committer_name, committer_email, 123456789, 60));
	cl_git_pass(git_signature_new(&author, committer_name, committer_email, 987654321, 90));

	/* First we need to update HEAD so it points to our non-existant branch */
	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_assert(git_reference_type(head) == GIT_REF_SYMBOLIC);
	head_old = git__strdup(git_reference_symbolic_target(head));
	cl_assert(head_old != NULL);
	git_reference_free(head);

	cl_git_pass(git_reference_symbolic_create(&head, g_repo, "HEAD", branch_name, 1, NULL, NULL));

	cl_git_pass(git_commit_create_v(
		&commit_id, /* out id */
		g_repo,
		"HEAD",
		author,
		committer,
		NULL,
		root_commit_message,
		tree,
		0));

	git_object_free((git_object *)tree);
	git_signature_free(author);

	/*
	 * The fact that creating a commit works has already been
	 * tested. Here we just make sure it's our commit and that it was
	 * written as a root commit.
	 */
	cl_git_pass(git_commit_lookup(&commit, g_repo, &commit_id));
	cl_assert(git_commit_parentcount(commit) == 0);
	cl_git_pass(git_reference_lookup(&branch, g_repo, branch_name));
	branch_oid = git_reference_target(branch);
	cl_git_pass(git_oid_cmp(branch_oid, &commit_id));
	cl_assert_equal_s(root_commit_message, git_commit_message(commit));

	cl_git_pass(git_reflog_read(&log, g_repo, branch_name));
	cl_assert_equal_i(1, git_reflog_entrycount(log));
	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s(committer->email, git_reflog_entry_committer(entry)->email);
	cl_assert_equal_s(committer->name, git_reflog_entry_committer(entry)->name);
	cl_assert_equal_s(root_reflog_message, git_reflog_entry_message(entry));

	git_signature_free(committer);
	git_reflog_free(log);
}
