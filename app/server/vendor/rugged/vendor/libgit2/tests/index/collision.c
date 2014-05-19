#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/index.h"

git_repository *repo = NULL;

void test_index_collision__cleanup(void)
{
	cl_git_sandbox_cleanup();
	repo = NULL;
}

void test_index_collision__add(void)
{
	git_index *index;
	git_index_entry entry;
	git_oid tree_id;
	git_tree *tree;

	repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_pass(git_repository_index(&index, repo));

	memset(&entry, 0, sizeof(entry));
	entry.ctime.seconds = 12346789;
	entry.mtime.seconds = 12346789;
	entry.mode  = 0100644;
	entry.file_size = 0;
	git_oid_fromstr(&entry.id, "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391");

	entry.path = "a/b";
	cl_git_pass(git_index_add(index, &entry));

	/* create a tree/blob collision */
	entry.path = "a/b/c";
	cl_git_fail(git_index_add(index, &entry));

	cl_git_pass(git_index_write_tree(&tree_id, index));
	cl_git_pass(git_tree_lookup(&tree, repo, &tree_id));

	git_tree_free(tree);
	git_index_free(index);
}

void test_index_collision__add_with_highstage_1(void)
{
	git_index *index;
	git_index_entry entry;

	repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_pass(git_repository_index(&index, repo));

	memset(&entry, 0, sizeof(entry));
	entry.ctime.seconds = 12346789;
	entry.mtime.seconds = 12346789;
	entry.mode  = 0100644;
	entry.file_size = 0;
	git_oid_fromstr(&entry.id, "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391");

	entry.path = "a/b";
	entry.flags = (2 << GIT_IDXENTRY_STAGESHIFT);
	cl_git_pass(git_index_add(index, &entry));

	/* create a blob beneath the previous tree entry */
	entry.path = "a/b/c";
	entry.flags = 0;
	cl_git_pass(git_index_add(index, &entry));

	/* create another tree entry above the blob */
	entry.path = "a/b";
	entry.flags = (1 << GIT_IDXENTRY_STAGESHIFT);
	cl_git_pass(git_index_add(index, &entry));

	git_index_free(index);
}

void test_index_collision__add_with_highstage_2(void)
{
	git_index *index;
	git_index_entry entry;

	repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_pass(git_repository_index(&index, repo));

	memset(&entry, 0, sizeof(entry));
	entry.ctime.seconds = 12346789;
	entry.mtime.seconds = 12346789;
	entry.mode  = 0100644;
	entry.file_size = 0;
	git_oid_fromstr(&entry.id, "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391");

	entry.path = "a/b/c";
	entry.flags = (1 << GIT_IDXENTRY_STAGESHIFT);
	cl_git_pass(git_index_add(index, &entry));

	/* create a blob beneath the previous tree entry */
	entry.path = "a/b/c";
	entry.flags = (2 << GIT_IDXENTRY_STAGESHIFT);
	cl_git_pass(git_index_add(index, &entry));

	/* create another tree entry above the blob */
	entry.path = "a/b";
	entry.flags = (3 << GIT_IDXENTRY_STAGESHIFT);
	cl_git_pass(git_index_add(index, &entry));

	git_index_free(index);
}
