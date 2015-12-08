#include "clar_libgit2.h"
#include "odb.h"
#include "filebuf.h"

git_repository *g_repo;

void test_odb_emptyobjects__initialize(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture("testrepo.git")));
}
void test_odb_emptyobjects__cleanup(void)
{
	git_repository_free(g_repo);
}

void test_odb_emptyobjects__read(void)
{
	git_oid id;
	git_blob *blob;

	cl_git_pass(git_oid_fromstr(&id, "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"));
	cl_git_pass(git_blob_lookup(&blob, g_repo, &id));
	cl_assert_equal_i(GIT_OBJ_BLOB, git_object_type((git_object *) blob));
	cl_assert(git_blob_rawcontent(blob));
	cl_assert_equal_s("", git_blob_rawcontent(blob));
	cl_assert_equal_i(0, git_blob_rawsize(blob));
	git_blob_free(blob);
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
	git_odb *odb;
	git_odb_object *tree_odb;

	cl_git_pass(git_oid_fromstr(&id, "4b825dc642cb6eb9a060e54bf8d69288fbee4904"));
	cl_git_pass(git_repository_odb(&odb, g_repo));
	cl_git_pass(git_odb_read(&tree_odb, odb, &id));
	cl_assert(git_odb_object_data(tree_odb));
	cl_assert_equal_s("", git_odb_object_data(tree_odb));
	cl_assert_equal_i(0, git_odb_object_size(tree_odb));
	git_odb_object_free(tree_odb);
	git_odb_free(odb);
}
