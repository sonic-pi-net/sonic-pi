#include "clar_libgit2.h"

#include "tree.h"

static const char *tree_oid = "1810dff58d8a660512d4832e740f692884338ccd";

static git_repository *g_repo;

// Fixture setup and teardown
void test_object_tree_read__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
}

void test_object_tree_read__cleanup(void)
{
   cl_git_sandbox_cleanup();
}



void test_object_tree_read__loaded(void)
{
   // acces randomly the entries on a loaded tree
	git_oid id;
	git_tree *tree;

	git_oid_fromstr(&id, tree_oid);

	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));

	cl_assert(git_tree_entry_byname(tree, "README") != NULL);
	cl_assert(git_tree_entry_byname(tree, "NOTEXISTS") == NULL);
	cl_assert(git_tree_entry_byname(tree, "") == NULL);
	cl_assert(git_tree_entry_byindex(tree, 0) != NULL);
	cl_assert(git_tree_entry_byindex(tree, 2) != NULL);
	cl_assert(git_tree_entry_byindex(tree, 3) == NULL);
	cl_assert(git_tree_entry_byindex(tree, (unsigned int)-1) == NULL);

	git_tree_free(tree);
}

void test_object_tree_read__two(void)
{
   // read a tree from the repository
	git_oid id;
	git_tree *tree;
	const git_tree_entry *entry;
	git_object *obj;

	git_oid_fromstr(&id, tree_oid);

	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));

	cl_assert(git_tree_entrycount(tree) == 3);

	/* GH-86: git_object_lookup() should also check the type if the object comes from the cache */
	cl_assert(git_object_lookup(&obj, g_repo, &id, GIT_OBJ_TREE) == 0);
	cl_assert(obj != NULL);
	git_object_free(obj);
	obj = NULL;
	cl_git_fail(git_object_lookup(&obj, g_repo, &id, GIT_OBJ_BLOB));
	cl_assert(obj == NULL);

	entry = git_tree_entry_byname(tree, "README");
	cl_assert(entry != NULL);

	cl_assert_equal_s(git_tree_entry_name(entry), "README");

	cl_git_pass(git_tree_entry_to_object(&obj, g_repo, entry));
	cl_assert(obj != NULL);

	git_object_free(obj);
	git_tree_free(tree);
}
