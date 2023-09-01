#include "clar_libgit2.h"

#include "tree.h"

static const char *tree_oid = "1810dff58d8a660512d4832e740f692884338ccd";

static git_repository *g_repo;

/* Fixture setup and teardown */
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
	/* access randomly the entries on a loaded tree */
	git_oid id;
	git_tree *tree;

	git_oid__fromstr(&id, tree_oid, GIT_OID_SHA1);

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
	/* read a tree from the repository */
	git_oid id;
	git_tree *tree;
	const git_tree_entry *entry;
	git_object *obj;

	git_oid__fromstr(&id, tree_oid, GIT_OID_SHA1);

	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));

	cl_assert(git_tree_entrycount(tree) == 3);

	/* GH-86: git_object_lookup() should also check the type if the object comes from the cache */
	cl_assert(git_object_lookup(&obj, g_repo, &id, GIT_OBJECT_TREE) == 0);
	cl_assert(obj != NULL);
	git_object_free(obj);
	obj = NULL;
	cl_git_fail(git_object_lookup(&obj, g_repo, &id, GIT_OBJECT_BLOB));
	cl_assert(obj == NULL);

	entry = git_tree_entry_byname(tree, "README");
	cl_assert(entry != NULL);

	cl_assert_equal_s(git_tree_entry_name(entry), "README");

	cl_git_pass(git_tree_entry_to_object(&obj, g_repo, entry));
	cl_assert(obj != NULL);

	git_object_free(obj);
	git_tree_free(tree);
}

#define BIGFILE "bigfile"

#ifdef GIT_ARCH_64
#define BIGFILE_SIZE (off_t)4294967296
#else
# define BIGFILE_SIZE SIZE_MAX
#endif

void test_object_tree_read__largefile(void)
{
	const git_tree_entry *entry;
	git_index_entry ie;
	git_commit *commit;
	git_object *object;
	git_index *index;
	git_tree *tree;
	git_oid oid;
	char *buf;

	if (!cl_is_env_set("GITTEST_INVASIVE_FS_SIZE"))
		cl_skip();

	cl_assert(buf = git__calloc(1, BIGFILE_SIZE));

	memset(&ie, 0, sizeof(ie));
	ie.mode = GIT_FILEMODE_BLOB;
	ie.path = BIGFILE;

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_add_from_buffer(index, &ie, buf, BIGFILE_SIZE));
	cl_repo_commit_from_index(&oid, g_repo, NULL, 0, BIGFILE);

	cl_git_pass(git_commit_lookup(&commit, g_repo, &oid));
	cl_git_pass(git_commit_tree(&tree, commit));
	cl_assert(entry = git_tree_entry_byname(tree, BIGFILE));
	cl_git_pass(git_tree_entry_to_object(&object, g_repo, entry));

	git_object_free(object);
	git_tree_free(tree);
	git_index_free(index);
	git_commit_free(commit);
	git__free(buf);
}
