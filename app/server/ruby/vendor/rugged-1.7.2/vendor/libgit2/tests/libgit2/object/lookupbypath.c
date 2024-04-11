#include "clar_libgit2.h"

#include "repository.h"

static git_repository *g_repo;
static git_tree *g_root_tree;
static git_commit *g_head_commit;
static git_object *g_expectedobject,
						*g_actualobject;

void test_object_lookupbypath__initialize(void)
{
	git_reference *head;
	git_tree_entry *tree_entry;

	cl_git_pass(git_repository_open(&g_repo, cl_fixture("attr/.gitted")));

	cl_git_pass(git_repository_head(&head, g_repo));
	cl_git_pass(git_reference_peel((git_object**)&g_head_commit, head, GIT_OBJECT_COMMIT));
	cl_git_pass(git_commit_tree(&g_root_tree, g_head_commit));
	cl_git_pass(git_tree_entry_bypath(&tree_entry, g_root_tree, "subdir/subdir_test2.txt"));
	cl_git_pass(git_object_lookup(&g_expectedobject, g_repo, git_tree_entry_id(tree_entry),
				GIT_OBJECT_ANY));

	git_tree_entry_free(tree_entry);
	git_reference_free(head);

	g_actualobject = NULL;
}
void test_object_lookupbypath__cleanup(void)
{
	git_object_free(g_actualobject);
	git_object_free(g_expectedobject);
	git_tree_free(g_root_tree);
	git_commit_free(g_head_commit);
	g_expectedobject = NULL;
	git_repository_free(g_repo);
	g_repo = NULL;
}

void test_object_lookupbypath__errors(void)
{
	cl_assert_equal_i(GIT_EINVALIDSPEC,
			git_object_lookup_bypath(&g_actualobject, (git_object*)g_root_tree,
				"subdir/subdir_test2.txt", GIT_OBJECT_TREE)); /* It's not a tree */
	cl_assert_equal_i(GIT_ENOTFOUND,
			git_object_lookup_bypath(&g_actualobject, (git_object*)g_root_tree,
				"file/doesnt/exist", GIT_OBJECT_ANY));
}

void test_object_lookupbypath__from_root_tree(void)
{
	cl_git_pass(git_object_lookup_bypath(&g_actualobject, (git_object*)g_root_tree,
				"subdir/subdir_test2.txt", GIT_OBJECT_BLOB));
	cl_assert_equal_oid(git_object_id(g_expectedobject),
		git_object_id(g_actualobject));
}

void test_object_lookupbypath__from_head_commit(void)
{
	cl_git_pass(git_object_lookup_bypath(&g_actualobject, (git_object*)g_head_commit,
				"subdir/subdir_test2.txt", GIT_OBJECT_BLOB));
	cl_assert_equal_oid(git_object_id(g_expectedobject),
				git_object_id(g_actualobject));
}

void test_object_lookupbypath__from_subdir_tree(void)
{
	git_tree_entry *entry = NULL;
	git_tree *tree = NULL;

	cl_git_pass(git_tree_entry_bypath(&entry, g_root_tree, "subdir"));
	cl_git_pass(git_tree_lookup(&tree, g_repo, git_tree_entry_id(entry)));

	cl_git_pass(git_object_lookup_bypath(&g_actualobject, (git_object*)tree,
				"subdir_test2.txt", GIT_OBJECT_BLOB));
	cl_assert_equal_oid(git_object_id(g_expectedobject),
				git_object_id(g_actualobject));

	git_tree_entry_free(entry);
	git_tree_free(tree);
}

