#include "clar_libgit2.h"
#include "posix.h"

static git_repository *repo;

void test_object_commit_commitstagedfile__initialize(void)
{
	cl_fixture("treebuilder");
	cl_git_pass(git_repository_init(&repo, "treebuilder/", 0));
	cl_assert(repo != NULL);
}

void test_object_commit_commitstagedfile__cleanup(void)
{
	git_repository_free(repo);
	repo = NULL;

	cl_fixture_cleanup("treebuilder");
}

void test_object_commit_commitstagedfile__generate_predictable_object_ids(void)
{
	git_index *index;
	const git_index_entry *entry;
	git_oid expected_blob_oid, tree_oid, expected_tree_oid, commit_oid, expected_commit_oid;
	git_signature *signature;
	git_tree *tree;
	git_buf buffer;

	/*
	 * The test below replicates the following git scenario
	 *
	 * $ echo "test" > test.txt
	 * $ git hash-object test.txt
	 * 9daeafb9864cf43055ae93beb0afd6c7d144bfa4
	 *
	 * $ git add .
	 * $ git commit -m "Initial commit"
	 *
	 * $ git log
	 * commit 1fe3126578fc4eca68c193e4a3a0a14a0704624d
	 * Author: nulltoken <emeric.fermas@gmail.com>
	 * Date:   Wed Dec 14 08:29:03 2011 +0100
	 *
	 *     Initial commit
	 *
	 * $ git show 1fe3 --format=raw
	 * commit 1fe3126578fc4eca68c193e4a3a0a14a0704624d
	 * tree 2b297e643c551e76cfa1f93810c50811382f9117
	 * author nulltoken <emeric.fermas@gmail.com> 1323847743 +0100
	 * committer nulltoken <emeric.fermas@gmail.com> 1323847743 +0100
	 * 
	 *     Initial commit
	 * 
	 * diff --git a/test.txt b/test.txt
	 * new file mode 100644
	 * index 0000000..9daeafb
	 * --- /dev/null
	 * +++ b/test.txt
	 * @@ -0,0 +1 @@
	 * +test
	 *
	 * $ git ls-tree 2b297
	 * 100644 blob 9daeafb9864cf43055ae93beb0afd6c7d144bfa4    test.txt
	 */

	cl_git_pass(git_oid_fromstr(&expected_commit_oid, "1fe3126578fc4eca68c193e4a3a0a14a0704624d"));
	cl_git_pass(git_oid_fromstr(&expected_tree_oid, "2b297e643c551e76cfa1f93810c50811382f9117"));
	cl_git_pass(git_oid_fromstr(&expected_blob_oid, "9daeafb9864cf43055ae93beb0afd6c7d144bfa4"));

	/*
	 * Add a new file to the index
	 */
	cl_git_mkfile("treebuilder/test.txt", "test\n");
	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, "test.txt"));

	entry = git_index_get_byindex(index, 0);

	cl_assert(git_oid_cmp(&expected_blob_oid, &entry->id) == 0);

	/*
	 * Information about index entry should match test file
	 */
	{
		struct stat st;
		cl_must_pass(p_lstat("treebuilder/test.txt", &st));
		cl_assert(entry->file_size == st.st_size);
#ifndef _WIN32
		/*
		 * Windows doesn't populate these fields, and the signage is
		 * wrong in the Windows version of the struct, so lets avoid
		 * the "comparing signed and unsigned" compilation warning in
		 * that case.
		 */
		cl_assert(entry->uid == st.st_uid);
		cl_assert(entry->gid == st.st_gid);
#endif
	}

	/*
	 * Build the tree from the index
	 */
	cl_git_pass(git_index_write_tree(&tree_oid, index));

	cl_assert(git_oid_cmp(&expected_tree_oid, &tree_oid) == 0);

	/*
	 * Commit the staged file
	 */
	cl_git_pass(git_signature_new(&signature, "nulltoken", "emeric.fermas@gmail.com", 1323847743, 60));
	cl_git_pass(git_tree_lookup(&tree, repo, &tree_oid));

	memset(&buffer, 0, sizeof(git_buf));
	cl_git_pass(git_message_prettify(&buffer, "Initial commit", 0, '#'));

	cl_git_pass(git_commit_create_v(
		&commit_oid,
		repo,
		"HEAD",
		signature,
		signature,
		NULL,
		buffer.ptr,
		tree,
		0));

	cl_assert(git_oid_cmp(&expected_commit_oid, &commit_oid) == 0);

	git_buf_free(&buffer);
	git_signature_free(signature);
	git_tree_free(tree);
	git_index_free(index);
}

static void assert_commit_tree_has_n_entries(git_commit *c, int count)
{
	git_tree *tree;
	cl_git_pass(git_commit_tree(&tree, c));
	cl_assert_equal_i(count, git_tree_entrycount(tree));
	git_tree_free(tree);
}

static void assert_commit_is_head_(git_commit *c, const char *file, int line)
{
	git_commit *head;
	cl_git_pass(git_revparse_single((git_object **)&head, repo, "HEAD"));
	clar__assert(git_oid_equal(git_commit_id(c), git_commit_id(head)), file, line, "Commit is not the HEAD", NULL, 1);
	git_commit_free(head);
}
#define assert_commit_is_head(C) assert_commit_is_head_((C),__FILE__,__LINE__)

void test_object_commit_commitstagedfile__amend_commit(void)
{
	git_index *index;
	git_oid old_oid, new_oid, tree_oid;
	git_commit *old_commit, *new_commit;
	git_tree *tree;

	/* make a commit */

	cl_git_mkfile("treebuilder/myfile", "This is a file\n");
	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, "myfile"));
	cl_repo_commit_from_index(&old_oid, repo, NULL, 0, "first commit");

	cl_git_pass(git_commit_lookup(&old_commit, repo, &old_oid));

	cl_assert_equal_i(0, git_commit_parentcount(old_commit));
	assert_commit_tree_has_n_entries(old_commit, 1);
	assert_commit_is_head(old_commit);

	/* let's amend the message of the HEAD commit */

	cl_git_pass(git_commit_amend(
		&new_oid, old_commit, "HEAD", NULL, NULL, NULL, "Initial commit", NULL));

	/* fail because the commit isn't the tip of the branch anymore */
	cl_git_fail(git_commit_amend(
		&new_oid, old_commit, "HEAD", NULL, NULL, NULL, "Initial commit", NULL));

	cl_git_pass(git_commit_lookup(&new_commit, repo, &new_oid));

	cl_assert_equal_i(0, git_commit_parentcount(new_commit));
	assert_commit_tree_has_n_entries(new_commit, 1);
	assert_commit_is_head(new_commit);

	git_commit_free(old_commit);

	old_commit = new_commit;

	/* let's amend the tree of that last commit */

	cl_git_mkfile("treebuilder/anotherfile", "This is another file\n");
	cl_git_pass(git_index_add_bypath(index, "anotherfile"));
	cl_git_pass(git_index_write_tree(&tree_oid, index));
	cl_git_pass(git_tree_lookup(&tree, repo, &tree_oid));
	cl_assert_equal_i(2, git_tree_entrycount(tree));

	/* fail to amend on a ref which does not exist */
	cl_git_fail_with(GIT_ENOTFOUND, git_commit_amend(
		&new_oid, old_commit, "refs/heads/nope", NULL, NULL, NULL, "Initial commit", tree));

	cl_git_pass(git_commit_amend(
		&new_oid, old_commit, "HEAD", NULL, NULL, NULL, "Initial commit", tree));
	git_tree_free(tree);

	cl_git_pass(git_commit_lookup(&new_commit, repo, &new_oid));

	cl_assert_equal_i(0, git_commit_parentcount(new_commit));
	assert_commit_tree_has_n_entries(new_commit, 2);
	assert_commit_is_head(new_commit);

	/* cleanup */

	git_commit_free(old_commit);
	git_commit_free(new_commit);
	git_index_free(index);
}
