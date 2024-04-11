#include "clar_libgit2.h"
#include "git2.h"
#include "index.h"
#include "tree-cache.h"

static git_repository *g_repo;

void test_index_cache__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");
}

void test_index_cache__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

void test_index_cache__write_extension_at_root(void)
{
	git_index *index;
	git_tree *tree;
	git_oid id;
	const char *tree_id_str = "45dd856fdd4d89b884c340ba0e047752d9b085d6";
	const char *index_file = "index-tree";

	cl_git_pass(git_index__open(&index, index_file, GIT_OID_SHA1));
	cl_assert(index->tree == NULL);
	cl_git_pass(git_oid__fromstr(&id, tree_id_str, GIT_OID_SHA1));
	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));
	cl_git_pass(git_index_read_tree(index, tree));
	git_tree_free(tree);

	cl_assert(index->tree);
	cl_git_pass(git_index_write(index));
	git_index_free(index);

	cl_git_pass(git_index__open(&index, index_file, GIT_OID_SHA1));
	cl_assert(index->tree);

	cl_assert_equal_i(git_index_entrycount(index), index->tree->entry_count);
	cl_assert_equal_i(0, index->tree->children_count);

	cl_assert(git_oid_equal(&id, &index->tree->oid));

	cl_git_pass(p_unlink(index_file));
	git_index_free(index);
}

void test_index_cache__write_extension_invalidated_root(void)
{
	git_index *index;
	git_tree *tree;
	git_oid id;
	const char *tree_id_str = "45dd856fdd4d89b884c340ba0e047752d9b085d6";
	const char *index_file = "index-tree-invalidated";
	git_index_entry entry;

	cl_git_pass(git_index__open(&index, index_file, GIT_OID_SHA1));
	cl_assert(index->tree == NULL);
	cl_git_pass(git_oid__fromstr(&id, tree_id_str, GIT_OID_SHA1));
	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));
	cl_git_pass(git_index_read_tree(index, tree));
	git_tree_free(tree);

	cl_assert(index->tree);

	memset(&entry, 0x0, sizeof(git_index_entry));
	git_oid_cpy(&entry.id, &git_index_get_byindex(index, 0)->id);
	entry.mode = GIT_FILEMODE_BLOB;
	entry.path = "some-new-file.txt";

	cl_git_pass(git_index_add(index, &entry));

	cl_assert_equal_i(-1, index->tree->entry_count);

	cl_git_pass(git_index_write(index));
	git_index_free(index);

	cl_git_pass(git_index__open(&index, index_file, GIT_OID_SHA1));
	cl_assert(index->tree);

	cl_assert_equal_i(-1, index->tree->entry_count);
	cl_assert_equal_i(0, index->tree->children_count);

	cl_assert(git_oid_cmp(&id, &index->tree->oid));

	cl_git_pass(p_unlink(index_file));
	git_index_free(index);
}

void test_index_cache__read_tree_no_children(void)
{
	git_index *index;
	git_index_entry entry;
	git_tree *tree;
	git_oid id;

	cl_git_pass(git_index__new(&index, GIT_OID_SHA1));
	cl_assert(index->tree == NULL);
	cl_git_pass(git_oid__fromstr(&id, "45dd856fdd4d89b884c340ba0e047752d9b085d6", GIT_OID_SHA1));
	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));
	cl_git_pass(git_index_read_tree(index, tree));
	git_tree_free(tree);

	cl_assert(index->tree);
	cl_assert(git_oid_equal(&id, &index->tree->oid));
	cl_assert_equal_i(0, index->tree->children_count);
	cl_assert_equal_i(git_index_entrycount(index), index->tree->entry_count);

	memset(&entry, 0x0, sizeof(git_index_entry));
	entry.path = "new.txt";
	entry.mode = GIT_FILEMODE_BLOB;
	git_oid__fromstr(&entry.id, "d4bcc68acd4410bf836a39f20afb2c2ece09584e", GIT_OID_SHA1);

	cl_git_pass(git_index_add(index, &entry));
	cl_assert_equal_i(-1, index->tree->entry_count);

	git_index_free(index);
}

void test_index_cache__two_levels(void)
{
	git_tree *tree;
	git_oid tree_id;
	git_index *index;
	git_index_entry entry;
	const git_tree_cache *tree_cache;

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_clear(index));

	memset(&entry, 0x0, sizeof(entry));
	entry.mode = GIT_FILEMODE_BLOB;
	cl_git_pass(git_oid__fromstr(&entry.id, "a8233120f6ad708f843d861ce2b7228ec4e3dec6", GIT_OID_SHA1));
	entry.path = "top-level.txt";
	cl_git_pass(git_index_add(index, &entry));

	entry.path = "subdir/file.txt";
	cl_git_pass(git_index_add(index, &entry));

	/* the read-tree fills the tree cache */
	cl_git_pass(git_index_write_tree(&tree_id, index));
	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));
	cl_git_pass(git_index_read_tree(index, tree));
	git_tree_free(tree);
	cl_git_pass(git_index_write(index));

	/* we now must have cache entries for "" and "subdir" */
	cl_assert(index->tree);
	cl_assert(git_tree_cache_get(index->tree, "subdir"));

	cl_git_pass(git_index_read(index, true));
	/* we must still have cache entries for "" and "subdir", since we wrote it out */
	cl_assert(index->tree);
	cl_assert(git_tree_cache_get(index->tree, "subdir"));

	entry.path = "top-level.txt";
	cl_git_pass(git_oid__fromstr(&entry.id, "3697d64be941a53d4ae8f6a271e4e3fa56b022cc", GIT_OID_SHA1));
	cl_git_pass(git_index_add(index, &entry));

	/* writ out the index after we invalidate the root */
	cl_git_pass(git_index_write(index));
	cl_git_pass(git_index_read(index, true));

	/* the cache for the subtree must still be valid, even if the root isn't */
	cl_assert(index->tree);
	cl_assert_equal_i(-1, index->tree->entry_count);
	cl_assert_equal_i(1, index->tree->children_count);
	tree_cache = git_tree_cache_get(index->tree, "subdir");
	cl_assert(tree_cache);
	cl_assert_equal_i(1, tree_cache->entry_count);

	git_index_free(index);
}

void test_index_cache__read_tree_children(void)
{
	git_index *index;
	git_index_entry entry;
	git_tree *tree;
	const git_tree_cache *cache;
	git_oid tree_id;

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_clear(index));
	cl_assert(index->tree == NULL);


	/* add a bunch of entries at different levels */
	memset(&entry, 0x0, sizeof(git_index_entry));
	entry.path = "top-level";
	entry.mode = GIT_FILEMODE_BLOB;
	git_oid__fromstr(&entry.id, "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf", GIT_OID_SHA1);
	cl_git_pass(git_index_add(index, &entry));


	entry.path = "subdir/some-file";
	cl_git_pass(git_index_add(index, &entry));

	entry.path = "subdir/even-deeper/some-file";
	cl_git_pass(git_index_add(index, &entry));

	entry.path = "subdir2/some-file";
	cl_git_pass(git_index_add(index, &entry));

	cl_git_pass(git_index_write_tree(&tree_id, index));
	cl_git_pass(git_index_clear(index));
	cl_assert(index->tree == NULL);

	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));
	cl_git_pass(git_index_read_tree(index, tree));
	git_tree_free(tree);

	cl_assert(index->tree);
	cl_assert_equal_i(2, index->tree->children_count);

	/* override with a slightly different id, also dummy */
	entry.path = "subdir/some-file";
	git_oid__fromstr(&entry.id, "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf", GIT_OID_SHA1);
	cl_git_pass(git_index_add(index, &entry));

	cl_assert_equal_i(-1, index->tree->entry_count);

	cache = git_tree_cache_get(index->tree, "subdir");
	cl_assert(cache);
	cl_assert_equal_i(-1, cache->entry_count);

	cache = git_tree_cache_get(index->tree, "subdir/even-deeper");
	cl_assert(cache);
	cl_assert_equal_i(1, cache->entry_count);

	cache = git_tree_cache_get(index->tree, "subdir2");
	cl_assert(cache);
	cl_assert_equal_i(1, cache->entry_count);

	git_index_free(index);
}
