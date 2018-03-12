#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/index.h"

static git_repository *g_repo;
static git_odb *g_odb;
static git_index *g_index;
static git_oid g_empty_id;

void test_index_collision__initialize(void)
{
	g_repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_pass(git_repository_odb(&g_odb, g_repo));
	cl_git_pass(git_repository_index(&g_index, g_repo));

	cl_git_pass(git_odb_write(&g_empty_id, g_odb, "", 0, GIT_OBJ_BLOB));
}

void test_index_collision__cleanup(void)
{
	git_index_free(g_index);
	git_odb_free(g_odb);
	cl_git_sandbox_cleanup();
}

void test_index_collision__add(void)
{
	git_index_entry entry;
	git_oid tree_id;
	git_tree *tree;

	memset(&entry, 0, sizeof(entry));
	entry.ctime.seconds = 12346789;
	entry.mtime.seconds = 12346789;
	entry.mode  = 0100644;
	entry.file_size = 0;
	git_oid_cpy(&entry.id, &g_empty_id);

	entry.path = "a/b";
	cl_git_pass(git_index_add(g_index, &entry));

	/* create a tree/blob collision */
	entry.path = "a/b/c";
	cl_git_fail(git_index_add(g_index, &entry));

	cl_git_pass(git_index_write_tree(&tree_id, g_index));
	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

	git_tree_free(tree);
}

void test_index_collision__add_with_highstage_1(void)
{
	git_index_entry entry;

	memset(&entry, 0, sizeof(entry));
	entry.ctime.seconds = 12346789;
	entry.mtime.seconds = 12346789;
	entry.mode  = 0100644;
	entry.file_size = 0;
	git_oid_cpy(&entry.id, &g_empty_id);

	entry.path = "a/b";
	GIT_IDXENTRY_STAGE_SET(&entry, 2);
	cl_git_pass(git_index_add(g_index, &entry));

	/* create a blob beneath the previous tree entry */
	entry.path = "a/b/c";
	entry.flags = 0;
	cl_git_pass(git_index_add(g_index, &entry));

	/* create another tree entry above the blob */
	entry.path = "a/b";
	GIT_IDXENTRY_STAGE_SET(&entry, 1);
	cl_git_pass(git_index_add(g_index, &entry));
}

void test_index_collision__add_with_highstage_2(void)
{
	git_index_entry entry;

	memset(&entry, 0, sizeof(entry));
	entry.ctime.seconds = 12346789;
	entry.mtime.seconds = 12346789;
	entry.mode  = 0100644;
	entry.file_size = 0;
	git_oid_cpy(&entry.id, &g_empty_id);

	entry.path = "a/b/c";
	GIT_IDXENTRY_STAGE_SET(&entry, 1);
	cl_git_pass(git_index_add(g_index, &entry));

	/* create a blob beneath the previous tree entry */
	entry.path = "a/b/c";
	GIT_IDXENTRY_STAGE_SET(&entry, 2);
	cl_git_pass(git_index_add(g_index, &entry));

	/* create another tree entry above the blob */
	entry.path = "a/b";
	GIT_IDXENTRY_STAGE_SET(&entry, 3);
	cl_git_pass(git_index_add(g_index, &entry));
}
