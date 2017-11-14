#include "clar_libgit2.h"
#include "odb.h"
#include "filebuf.h"

#define TEST_REPO_PATH "redundant.git"

git_repository *g_repo;
git_odb *g_odb;

void test_odb_emptyobjects__initialize(void)
{
	g_repo = cl_git_sandbox_init(TEST_REPO_PATH);
	cl_git_pass(git_repository_odb(&g_odb, g_repo));
}

void test_odb_emptyobjects__cleanup(void)
{
	git_odb_free(g_odb);
	cl_git_sandbox_cleanup();
}

void test_odb_emptyobjects__blob_notfound(void)
{
	git_oid id, written_id;
	git_blob *blob;

	cl_git_pass(git_oid_fromstr(&id, "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"));
	cl_git_fail_with(GIT_ENOTFOUND, git_blob_lookup(&blob, g_repo, &id));

	cl_git_pass(git_odb_write(&written_id, g_odb, "", 0, GIT_OBJ_BLOB));
	cl_assert(git_path_exists(TEST_REPO_PATH "/objects/e6/9de29bb2d1d6434b8b29ae775ad8c2e48c5391"));
}

void test_odb_emptyobjects__read_tree(void)
{
	git_oid id;
	git_tree *tree;

	cl_git_pass(git_oid_fromstr(&id, "4b825dc642cb6eb9a060e54bf8d69288fbee4904"));
	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));
	cl_assert_equal_i(GIT_OBJ_TREE, git_object_type((git_object *) tree));
	cl_assert_equal_i(0, git_tree_entrycount(tree));
	cl_assert_equal_p(NULL, git_tree_entry_byname(tree, "foo"));
	git_tree_free(tree);
}

void test_odb_emptyobjects__read_tree_odb(void)
{
	git_oid id;
	git_odb_object *tree_odb;

	cl_git_pass(git_oid_fromstr(&id, "4b825dc642cb6eb9a060e54bf8d69288fbee4904"));
	cl_git_pass(git_odb_read(&tree_odb, g_odb, &id));
	cl_assert(git_odb_object_data(tree_odb));
	cl_assert_equal_s("", git_odb_object_data(tree_odb));
	cl_assert_equal_i(0, git_odb_object_size(tree_odb));
	git_odb_object_free(tree_odb);
}
