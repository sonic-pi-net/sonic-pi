#include "clar_libgit2.h"
#include "git2/sys/commit.h"

static const char *committer_name = "Vicent Marti";
static const char *committer_email = "vicent@github.com";
static const char *commit_message = "This commit has been created in memory\n\
   This is a commit created in memory and it will be written back to disk\n";
static const char *tree_id_str = "1810dff58d8a660512d4832e740f692884338ccd";
static const char *parent_id_str = "8496071c1b46c854b31185ea97743be6a8774479";
static const char *root_commit_message = "This is a root commit\n\
   This is a root commit and should be the only one in this branch\n";
static const char *root_reflog_message = "commit (initial): This is a root commit \
This is a root commit and should be the only one in this branch";
static char *head_old;
static git_reference *head, *branch;
static git_commit *commit;

/* Fixture setup */
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

	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_OBJECT_CREATION, 1));
}


/* write a new commit object from memory to disk */
void test_commit_write__from_memory(void)
{
   git_oid tree_id, parent_id, commit_id;
   git_signature *author, *committer;
   const git_signature *author1, *committer1;
   git_commit *parent;
   git_tree *tree;

   git_oid__fromstr(&tree_id, tree_id_str, GIT_OID_SHA1);
   cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

   git_oid__fromstr(&parent_id, parent_id_str, GIT_OID_SHA1);
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

void test_commit_write__into_buf(void)
{
	git_oid tree_id;
	git_signature *author, *committer;
	git_tree *tree;
	git_commit *parent;
	git_oid parent_id;
	git_buf commit = GIT_BUF_INIT;

	git_oid__fromstr(&tree_id, tree_id_str, GIT_OID_SHA1);
	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

	/* create signatures */
	cl_git_pass(git_signature_new(&committer, committer_name, committer_email, 123456789, 60));
	cl_git_pass(git_signature_new(&author, committer_name, committer_email, 987654321, 90));

	git_oid__fromstr(&parent_id, parent_id_str, GIT_OID_SHA1);
	cl_git_pass(git_commit_lookup(&parent, g_repo, &parent_id));

	cl_git_pass(git_commit_create_buffer(&commit, g_repo, author, committer,
					     NULL, root_commit_message, tree, 1, (const git_commit **) &parent));

	cl_assert_equal_s(commit.ptr,
			  "tree 1810dff58d8a660512d4832e740f692884338ccd\n\
parent 8496071c1b46c854b31185ea97743be6a8774479\n\
author Vicent Marti <vicent@github.com> 987654321 +0130\n\
committer Vicent Marti <vicent@github.com> 123456789 +0100\n\
\n\
This is a root commit\n\
   This is a root commit and should be the only one in this branch\n\
");

	git_buf_dispose(&commit);
	git_tree_free(tree);
	git_commit_free(parent);
	git_signature_free(author);
	git_signature_free(committer);
}

/* create a root commit */
void test_commit_write__root(void)
{
	git_oid tree_id, commit_id;
	const git_oid *branch_oid;
	git_signature *author, *committer;
	const char *branch_name = "refs/heads/root-commit-branch";
	git_tree *tree;
	git_reflog *log;
	const git_reflog_entry *entry;

	git_oid__fromstr(&tree_id, tree_id_str, GIT_OID_SHA1);
	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

	/* create signatures */
	cl_git_pass(git_signature_new(&committer, committer_name, committer_email, 123456789, 60));
	cl_git_pass(git_signature_new(&author, committer_name, committer_email, 987654321, 90));

	/* First we need to update HEAD so it points to our non-existent branch */
	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_assert(git_reference_type(head) == GIT_REFERENCE_SYMBOLIC);
	head_old = git__strdup(git_reference_symbolic_target(head));
	cl_assert(head_old != NULL);
	git_reference_free(head);

	cl_git_pass(git_reference_symbolic_create(&head, g_repo, "HEAD", branch_name, 1, NULL));

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
	cl_assert_equal_oid(branch_oid, &commit_id);
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

static int create_commit_from_ids(
	git_oid *result,
	const git_oid *tree_id,
	const git_oid *parent_id)
{
	git_signature *author, *committer;
	const git_oid *parent_ids[1];
	int ret;

	cl_git_pass(git_signature_new(
		&committer, committer_name, committer_email, 123456789, 60));
	cl_git_pass(git_signature_new(
		&author, committer_name, committer_email, 987654321, 90));

	parent_ids[0] = parent_id;

	ret = git_commit_create_from_ids(
		result,
		g_repo,
		NULL,
		author,
		committer,
		NULL,
		root_commit_message,
		tree_id,
		1,
		parent_ids);

	git_signature_free(committer);
	git_signature_free(author);

	return ret;
}

void test_commit_write__can_write_invalid_objects(void)
{
	git_oid expected_id, tree_id, parent_id, commit_id;

	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_OBJECT_CREATION, 0));

	/* this is a valid tree and parent */
	git_oid__fromstr(&tree_id, tree_id_str, GIT_OID_SHA1);
	git_oid__fromstr(&parent_id, parent_id_str, GIT_OID_SHA1);

	git_oid__fromstr(&expected_id, "c8571bbec3a72c4bcad31648902e5a453f1adece", GIT_OID_SHA1);
	cl_git_pass(create_commit_from_ids(&commit_id, &tree_id, &parent_id));
	cl_assert_equal_oid(&expected_id, &commit_id);

	/* this is a wholly invented tree id */
	git_oid__fromstr(&tree_id, "1234567890123456789012345678901234567890", GIT_OID_SHA1);
	git_oid__fromstr(&parent_id, parent_id_str, GIT_OID_SHA1);

	git_oid__fromstr(&expected_id, "996008340b8e68d69bf3c28d7c57fb7ec3c8e202", GIT_OID_SHA1);
	cl_git_pass(create_commit_from_ids(&commit_id, &tree_id, &parent_id));
	cl_assert_equal_oid(&expected_id, &commit_id);

	/* this is a wholly invented parent id */
	git_oid__fromstr(&tree_id, tree_id_str, GIT_OID_SHA1);
	git_oid__fromstr(&parent_id, "1234567890123456789012345678901234567890", GIT_OID_SHA1);

	git_oid__fromstr(&expected_id, "d78f660cab89d9791ca6714b57978bf2a7e709fd", GIT_OID_SHA1);
	cl_git_pass(create_commit_from_ids(&commit_id, &tree_id, &parent_id));
	cl_assert_equal_oid(&expected_id, &commit_id);

	/* these are legitimate objects, but of the wrong type */
	git_oid__fromstr(&tree_id, parent_id_str, GIT_OID_SHA1);
	git_oid__fromstr(&parent_id, tree_id_str, GIT_OID_SHA1);

	git_oid__fromstr(&expected_id, "5d80c07414e3f18792949699dfcacadf7748f361", GIT_OID_SHA1);
	cl_git_pass(create_commit_from_ids(&commit_id, &tree_id, &parent_id));
	cl_assert_equal_oid(&expected_id, &commit_id);
}

void test_commit_write__can_validate_objects(void)
{
	git_oid tree_id, parent_id, commit_id;

	/* this is a valid tree and parent */
	git_oid__fromstr(&tree_id, tree_id_str, GIT_OID_SHA1);
	git_oid__fromstr(&parent_id, parent_id_str, GIT_OID_SHA1);
	cl_git_pass(create_commit_from_ids(&commit_id, &tree_id, &parent_id));

	/* this is a wholly invented tree id */
	git_oid__fromstr(&tree_id, "1234567890123456789012345678901234567890", GIT_OID_SHA1);
	git_oid__fromstr(&parent_id, parent_id_str, GIT_OID_SHA1);
	cl_git_fail(create_commit_from_ids(&commit_id, &tree_id, &parent_id));

	/* this is a wholly invented parent id */
	git_oid__fromstr(&tree_id, tree_id_str, GIT_OID_SHA1);
	git_oid__fromstr(&parent_id, "1234567890123456789012345678901234567890", GIT_OID_SHA1);
	cl_git_fail(create_commit_from_ids(&commit_id, &tree_id, &parent_id));

	/* these are legitimate objects, but of the wrong type */
	git_oid__fromstr(&tree_id, parent_id_str, GIT_OID_SHA1);
	git_oid__fromstr(&parent_id, tree_id_str, GIT_OID_SHA1);
	cl_git_fail(create_commit_from_ids(&commit_id, &tree_id, &parent_id));
}

void test_commit_write__attach_signature_checks_objects(void)
{
	const char *sig = "magic word: pretty please";
	const char *badtree =  "tree 6b79e22d69bf46e289df0345a14ca059dfc9bdf6\n\
parent 34734e478d6cf50c27c9d69026d93974d052c454\n\
author Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
\n\
a simple commit which does not work\n";

	const char *badparent =  "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904\n\
parent 34734e478d6cf50c27c9d69026d93974d052c454\n\
author Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
\n\
a simple commit which does not work\n";

	git_oid id;

	cl_git_fail_with(-1, git_commit_create_with_signature(&id, g_repo, badtree, sig, "magicsig"));
	cl_git_fail_with(-1, git_commit_create_with_signature(&id, g_repo, badparent, sig, "magicsig"));

}

void test_commit_write__attach_singleline_signature(void)
{
	const char *sig = "magic word: pretty please";

	const char *data =  "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904\n\
parent 8496071c1b46c854b31185ea97743be6a8774479\n\
author Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
\n\
a simple commit which works\n";

	const char *complete =  "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904\n\
parent 8496071c1b46c854b31185ea97743be6a8774479\n\
author Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
magicsig magic word: pretty please\n\
\n\
a simple commit which works\n";

	git_oid id;
	git_odb *odb;
	git_odb_object *obj;

	cl_git_pass(git_commit_create_with_signature(&id, g_repo, data, sig, "magicsig"));

	cl_git_pass(git_repository_odb(&odb, g_repo));
	cl_git_pass(git_odb_read(&obj, odb, &id));
	cl_assert_equal_s(complete, git_odb_object_data(obj));

	git_odb_object_free(obj);
	git_odb_free(odb);
}

void test_commit_write__attach_multiline_signature(void)
{
		const char *gpgsig = "-----BEGIN PGP SIGNATURE-----\n\
Version: GnuPG v1.4.12 (Darwin)\n\
\n\
iQIcBAABAgAGBQJQ+FMIAAoJEH+LfPdZDSs1e3EQAJMjhqjWF+WkGLHju7pTw2al\n\
o6IoMAhv0Z/LHlWhzBd9e7JeCnanRt12bAU7yvYp9+Z+z+dbwqLwDoFp8LVuigl8\n\
JGLcnwiUW3rSvhjdCp9irdb4+bhKUnKUzSdsR2CK4/hC0N2i/HOvMYX+BRsvqweq\n\
AsAkA6dAWh+gAfedrBUkCTGhlNYoetjdakWqlGL1TiKAefEZrtA1TpPkGn92vbLq\n\
SphFRUY9hVn1ZBWrT3hEpvAIcZag3rTOiRVT1X1flj8B2vGCEr3RrcwOIZikpdaW\n\
who/X3xh/DGbI2RbuxmmJpxxP/8dsVchRJJzBwG+yhwU/iN3MlV2c5D69tls/Dok\n\
6VbyU4lm/ae0y3yR83D9dUlkycOnmmlBAHKIZ9qUts9X7mWJf0+yy2QxJVpjaTGG\n\
cmnQKKPeNIhGJk2ENnnnzjEve7L7YJQF6itbx5VCOcsGh3Ocb3YR7DMdWjt7f8pu\n\
c6j+q1rP7EpE2afUN/geSlp5i3x8aXZPDj67jImbVCE/Q1X9voCtyzGJH7MXR0N9\n\
ZpRF8yzveRfMH8bwAJjSOGAFF5XkcR/RNY95o+J+QcgBLdX48h+ZdNmUf6jqlu3J\n\
7KmTXXQcOVpN6dD3CmRFsbjq+x6RHwa8u1iGn+oIkX908r97ckfB/kHKH7ZdXIJc\n\
cpxtDQQMGYFpXK/71stq\n\
=ozeK\n\
-----END PGP SIGNATURE-----";

	const char *data =  "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904\n\
parent 8496071c1b46c854b31185ea97743be6a8774479\n\
author Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
\n\
a simple commit which works\n";

const char *complete = "tree 4b825dc642cb6eb9a060e54bf8d69288fbee4904\n\
parent 8496071c1b46c854b31185ea97743be6a8774479\n\
author Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
committer Ben Burkert <ben@benburkert.com> 1358451456 -0800\n\
gpgsig -----BEGIN PGP SIGNATURE-----\n\
 Version: GnuPG v1.4.12 (Darwin)\n\
 \n\
 iQIcBAABAgAGBQJQ+FMIAAoJEH+LfPdZDSs1e3EQAJMjhqjWF+WkGLHju7pTw2al\n\
 o6IoMAhv0Z/LHlWhzBd9e7JeCnanRt12bAU7yvYp9+Z+z+dbwqLwDoFp8LVuigl8\n\
 JGLcnwiUW3rSvhjdCp9irdb4+bhKUnKUzSdsR2CK4/hC0N2i/HOvMYX+BRsvqweq\n\
 AsAkA6dAWh+gAfedrBUkCTGhlNYoetjdakWqlGL1TiKAefEZrtA1TpPkGn92vbLq\n\
 SphFRUY9hVn1ZBWrT3hEpvAIcZag3rTOiRVT1X1flj8B2vGCEr3RrcwOIZikpdaW\n\
 who/X3xh/DGbI2RbuxmmJpxxP/8dsVchRJJzBwG+yhwU/iN3MlV2c5D69tls/Dok\n\
 6VbyU4lm/ae0y3yR83D9dUlkycOnmmlBAHKIZ9qUts9X7mWJf0+yy2QxJVpjaTGG\n\
 cmnQKKPeNIhGJk2ENnnnzjEve7L7YJQF6itbx5VCOcsGh3Ocb3YR7DMdWjt7f8pu\n\
 c6j+q1rP7EpE2afUN/geSlp5i3x8aXZPDj67jImbVCE/Q1X9voCtyzGJH7MXR0N9\n\
 ZpRF8yzveRfMH8bwAJjSOGAFF5XkcR/RNY95o+J+QcgBLdX48h+ZdNmUf6jqlu3J\n\
 7KmTXXQcOVpN6dD3CmRFsbjq+x6RHwa8u1iGn+oIkX908r97ckfB/kHKH7ZdXIJc\n\
 cpxtDQQMGYFpXK/71stq\n\
 =ozeK\n\
 -----END PGP SIGNATURE-----\n\
\n\
a simple commit which works\n";

	git_oid one, two;
	git_odb *odb;
	git_odb_object *obj;

	cl_git_pass(git_commit_create_with_signature(&one, g_repo, data, gpgsig, "gpgsig"));
	cl_git_pass(git_commit_create_with_signature(&two, g_repo, data, gpgsig, NULL));

	cl_assert(!git_oid_cmp(&one, &two));
	cl_git_pass(git_repository_odb(&odb, g_repo));
	cl_git_pass(git_odb_read(&obj, odb, &one));
	cl_assert_equal_s(complete, git_odb_object_data(obj));

	git_odb_object_free(obj);
	git_odb_free(odb);
}
