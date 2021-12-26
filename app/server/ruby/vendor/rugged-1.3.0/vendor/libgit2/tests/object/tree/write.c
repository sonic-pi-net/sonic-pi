#include "clar_libgit2.h"

#include "tree.h"

static const char *blob_oid = "fa49b077972391ad58037050f2a75f74e3671e92";
static const char *first_tree  = "181037049a54a1eb5fab404658a3a250b44335d7";
static const char *second_tree = "f60079018b664e4e79329a7ef9559c8d9e0378d1";
static const char *third_tree = "eb86d8b81d6adbd5290a935d6c9976882de98488";

static git_repository *g_repo;

/* Fixture setup and teardown */
void test_object_tree_write__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
}

void test_object_tree_write__cleanup(void)
{
   cl_git_sandbox_cleanup();

	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_OBJECT_CREATION, 1));
}

void test_object_tree_write__from_memory(void)
{
	/* write a tree from a memory */
	git_treebuilder *builder;
	git_tree *tree;
	git_oid id, bid, rid, id2;

	git_oid_fromstr(&id, first_tree);
	git_oid_fromstr(&id2, second_tree);
	git_oid_fromstr(&bid, blob_oid);

	/* create a second tree from first tree using `git_treebuilder_insert`
	 * on REPOSITORY_FOLDER.
	 */
	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));
	cl_git_pass(git_treebuilder_new(&builder, g_repo, tree));

	cl_git_fail(git_treebuilder_insert(NULL, builder, "",
		&bid, GIT_FILEMODE_BLOB));
	cl_git_fail(git_treebuilder_insert(NULL, builder, "/",
		&bid, GIT_FILEMODE_BLOB));
	cl_git_fail(git_treebuilder_insert(NULL, builder, ".git",
		&bid, GIT_FILEMODE_BLOB));
	cl_git_fail(git_treebuilder_insert(NULL, builder, "..",
		&bid, GIT_FILEMODE_BLOB));
	cl_git_fail(git_treebuilder_insert(NULL, builder, ".",
		&bid, GIT_FILEMODE_BLOB));
	cl_git_fail(git_treebuilder_insert(NULL, builder, "folder/new.txt",
		&bid, GIT_FILEMODE_BLOB));

	cl_git_pass(git_treebuilder_insert(
		NULL, builder, "new.txt", &bid, GIT_FILEMODE_BLOB));

	cl_git_pass(git_treebuilder_write(&rid, builder));

	cl_assert(git_oid_cmp(&rid, &id2) == 0);

	git_treebuilder_free(builder);
	git_tree_free(tree);
}

void test_object_tree_write__subtree(void)
{
	/* write a hierarchical tree from a memory */
	git_treebuilder *builder;
	git_tree *tree;
	git_oid id, bid, subtree_id, id2, id3;
	git_oid id_hiearar;

	git_oid_fromstr(&id, first_tree);
	git_oid_fromstr(&id2, second_tree);
	git_oid_fromstr(&id3, third_tree);
	git_oid_fromstr(&bid, blob_oid);

	/* create subtree */
	cl_git_pass(git_treebuilder_new(&builder, g_repo, NULL));
	cl_git_pass(git_treebuilder_insert(
		NULL, builder, "new.txt", &bid, GIT_FILEMODE_BLOB)); /* -V536 */
	cl_git_pass(git_treebuilder_write(&subtree_id, builder));
	git_treebuilder_free(builder);

	/* create parent tree */
	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));
	cl_git_pass(git_treebuilder_new(&builder, g_repo, tree));
	cl_git_pass(git_treebuilder_insert(
		NULL, builder, "new", &subtree_id, GIT_FILEMODE_TREE)); /* -V536 */
	cl_git_pass(git_treebuilder_write(&id_hiearar, builder));
	git_treebuilder_free(builder);
	git_tree_free(tree);

	cl_assert(git_oid_cmp(&id_hiearar, &id3) == 0);

	/* check data is correct */
	cl_git_pass(git_tree_lookup(&tree, g_repo, &id_hiearar));
	cl_assert(2 == git_tree_entrycount(tree));
	git_tree_free(tree);
}

/*
 * And the Lord said: Is this tree properly sorted?
 */
void test_object_tree_write__sorted_subtrees(void)
{
	git_treebuilder *builder;
	git_tree *tree;
	unsigned int i;
	int position_c = -1, position_cake = -1, position_config = -1;

	struct {
		unsigned int attr;
		const char *filename;
	} entries[] = {
		{ GIT_FILEMODE_BLOB, ".gitattributes" },
	  	{ GIT_FILEMODE_BLOB, ".gitignore" },
	  	{ GIT_FILEMODE_BLOB, ".htaccess" },
	  	{ GIT_FILEMODE_BLOB, "Capfile" },
	  	{ GIT_FILEMODE_BLOB, "Makefile"},
	  	{ GIT_FILEMODE_BLOB, "README"},
	  	{ GIT_FILEMODE_TREE, "app"},
	  	{ GIT_FILEMODE_TREE, "cake"},
	  	{ GIT_FILEMODE_TREE, "config"},
	  	{ GIT_FILEMODE_BLOB, "c"},
	  	{ GIT_FILEMODE_BLOB, "git_test.txt"},
	  	{ GIT_FILEMODE_BLOB, "htaccess.htaccess"},
	  	{ GIT_FILEMODE_BLOB, "index.php"},
	  	{ GIT_FILEMODE_TREE, "plugins"},
	  	{ GIT_FILEMODE_TREE, "schemas"},
	  	{ GIT_FILEMODE_TREE, "ssl-certs"},
	  	{ GIT_FILEMODE_TREE, "vendors"}
	};

	git_oid bid, tid, tree_oid;

	cl_git_pass(git_oid_fromstr(&bid, blob_oid));
	cl_git_pass(git_oid_fromstr(&tid, first_tree));

	cl_git_pass(git_treebuilder_new(&builder, g_repo, NULL));

	for (i = 0; i < ARRAY_SIZE(entries); ++i) {
		git_oid *id = entries[i].attr == GIT_FILEMODE_TREE ?  &tid : &bid;

		cl_git_pass(git_treebuilder_insert(NULL,
			builder, entries[i].filename, id, entries[i].attr));
	}

	cl_git_pass(git_treebuilder_write(&tree_oid, builder));

	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_oid));
	for (i = 0; i < git_tree_entrycount(tree); i++) {
		const git_tree_entry *entry = git_tree_entry_byindex(tree, i);

		if (strcmp(entry->filename, "c") == 0)
			position_c = i;

		if (strcmp(entry->filename, "cake") == 0)
			position_cake = i;

		if (strcmp(entry->filename, "config") == 0)
			position_config = i;
	}

	git_tree_free(tree);

	cl_assert(position_c != -1);
	cl_assert(position_cake != -1);
	cl_assert(position_config != -1);

	cl_assert(position_c < position_cake);
	cl_assert(position_cake < position_config);

	git_treebuilder_free(builder);
}

static struct {
	unsigned int attr;
	const char *filename;
} _entries[] = {
	{ GIT_FILEMODE_BLOB, "aardvark" },
	{ GIT_FILEMODE_BLOB, ".first" },
	{ GIT_FILEMODE_BLOB, "apple" },
	{ GIT_FILEMODE_BLOB, "last"},
	{ GIT_FILEMODE_BLOB, "apple_after"},
	{ GIT_FILEMODE_BLOB, "after_aardvark"},
	{ 0, NULL },
};

void test_object_tree_write__removing_and_re_adding_in_treebuilder(void)
{
	git_treebuilder *builder;
	int i, aardvark_i, apple_i, apple_after_i, apple_extra_i, last_i;
	git_oid entry_oid, tree_oid;
	git_tree *tree;

	cl_git_pass(git_oid_fromstr(&entry_oid, blob_oid));

	cl_git_pass(git_treebuilder_new(&builder, g_repo, NULL));

	cl_assert_equal_i(0, (int)git_treebuilder_entrycount(builder));

	for (i = 0; _entries[i].filename; ++i)
		cl_git_pass(git_treebuilder_insert(NULL,
			builder, _entries[i].filename, &entry_oid, _entries[i].attr));

	cl_assert_equal_i(6, (int)git_treebuilder_entrycount(builder));

	cl_git_pass(git_treebuilder_remove(builder, "apple"));
	cl_assert_equal_i(5, (int)git_treebuilder_entrycount(builder));

	cl_git_pass(git_treebuilder_remove(builder, "apple_after"));
	cl_assert_equal_i(4, (int)git_treebuilder_entrycount(builder));

	cl_git_pass(git_treebuilder_insert(
		NULL, builder, "before_last", &entry_oid, GIT_FILEMODE_BLOB));
	cl_assert_equal_i(5, (int)git_treebuilder_entrycount(builder));

	/* reinsert apple_after */
	cl_git_pass(git_treebuilder_insert(
		NULL, builder, "apple_after", &entry_oid, GIT_FILEMODE_BLOB));
	cl_assert_equal_i(6, (int)git_treebuilder_entrycount(builder));

	cl_git_pass(git_treebuilder_remove(builder, "last"));
	cl_assert_equal_i(5, (int)git_treebuilder_entrycount(builder));

	/* reinsert last */
	cl_git_pass(git_treebuilder_insert(
		NULL, builder, "last", &entry_oid, GIT_FILEMODE_BLOB));
	cl_assert_equal_i(6, (int)git_treebuilder_entrycount(builder));

	cl_git_pass(git_treebuilder_insert(
		NULL, builder, "apple_extra", &entry_oid, GIT_FILEMODE_BLOB));
	cl_assert_equal_i(7, (int)git_treebuilder_entrycount(builder));

	cl_git_pass(git_treebuilder_write(&tree_oid, builder));

	git_treebuilder_free(builder);

	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_oid));

	cl_assert_equal_i(7, (int)git_tree_entrycount(tree));

	cl_assert(git_tree_entry_byname(tree, ".first") != NULL);
	cl_assert(git_tree_entry_byname(tree, "apple") == NULL);
	cl_assert(git_tree_entry_byname(tree, "apple_after") != NULL);
	cl_assert(git_tree_entry_byname(tree, "apple_extra") != NULL);
	cl_assert(git_tree_entry_byname(tree, "last") != NULL);

	aardvark_i = apple_i = apple_after_i = apple_extra_i = last_i = -1;

	for (i = 0; i < 7; ++i) {
		const git_tree_entry *entry = git_tree_entry_byindex(tree, i);

		if (!strcmp(entry->filename, "aardvark"))
			aardvark_i = i;
		else if (!strcmp(entry->filename, "apple"))
			apple_i = i;
		else if (!strcmp(entry->filename, "apple_after"))
			apple_after_i = i;
		else if (!strcmp(entry->filename, "apple_extra"))
			apple_extra_i = i;
		else if (!strcmp(entry->filename, "last"))
			last_i = i;
	}

	cl_assert_equal_i(-1, apple_i);
	cl_assert_equal_i(6, last_i);
	cl_assert(aardvark_i < apple_after_i);
	cl_assert(apple_after_i < apple_extra_i);

	git_tree_free(tree);
}

static int treebuilder_filter_prefixed(
	const git_tree_entry *entry, void *payload)
{
	return !git__prefixcmp(git_tree_entry_name(entry), payload);
}

void test_object_tree_write__filtering(void)
{
	git_treebuilder *builder;
	int i;
	git_oid entry_oid, tree_oid;
	git_tree *tree;

	git_oid_fromstr(&entry_oid, blob_oid);

	cl_git_pass(git_treebuilder_new(&builder, g_repo, NULL));

	for (i = 0; _entries[i].filename; ++i)
		cl_git_pass(git_treebuilder_insert(NULL,
			builder, _entries[i].filename, &entry_oid, _entries[i].attr));

	cl_assert_equal_i(6, (int)git_treebuilder_entrycount(builder));

	cl_assert(git_treebuilder_get(builder, "apple") != NULL);
	cl_assert(git_treebuilder_get(builder, "aardvark") != NULL);
	cl_assert(git_treebuilder_get(builder, "last") != NULL);

	git_treebuilder_filter(builder, treebuilder_filter_prefixed, "apple");

	cl_assert_equal_i(4, (int)git_treebuilder_entrycount(builder));

	cl_assert(git_treebuilder_get(builder, "apple") == NULL);
	cl_assert(git_treebuilder_get(builder, "aardvark") != NULL);
	cl_assert(git_treebuilder_get(builder, "last") != NULL);

	git_treebuilder_filter(builder, treebuilder_filter_prefixed, "a");

	cl_assert_equal_i(2, (int)git_treebuilder_entrycount(builder));

	cl_assert(git_treebuilder_get(builder, "aardvark") == NULL);
	cl_assert(git_treebuilder_get(builder, "last") != NULL);

	cl_git_pass(git_treebuilder_write(&tree_oid, builder));

	git_treebuilder_free(builder);

	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_oid));

	cl_assert_equal_i(2, (int)git_tree_entrycount(tree));

	git_tree_free(tree);
}

void test_object_tree_write__cruel_paths(void)
{
	static const char *the_paths[] = {
		"C:\\",
		" : * ? \" \n < > |",
		"a\\b",
		"\\\\b\a",
		":\\",
		"COM1",
		"foo.aux",
		REP1024("1234"), /* 4096 char string */
		REP1024("12345678"), /* 8192 char string */
		"\xC5\xAA\x6E\xC4\xAD\x63\xC5\x8D\x64\x65\xCC\xBD", /* Ūnĭcōde̽ */
		NULL
	};
	git_treebuilder *builder;
	git_tree *tree;
	git_oid id, bid, subid;
	const char **scan;
	int count = 0, i, j;
	git_tree_entry *te;

	git_oid_fromstr(&bid, blob_oid);

	/* create tree */
	cl_git_pass(git_treebuilder_new(&builder, g_repo, NULL));
	for (scan = the_paths; *scan; ++scan) {
		cl_git_pass(git_treebuilder_insert(
			NULL, builder, *scan, &bid, GIT_FILEMODE_BLOB));
		count++;
	}
	cl_git_pass(git_treebuilder_write(&id, builder));
	git_treebuilder_free(builder);

	/* check data is correct */
	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));

	cl_assert_equal_i(count, git_tree_entrycount(tree));

	for (scan = the_paths; *scan; ++scan) {
		const git_tree_entry *cte = git_tree_entry_byname(tree, *scan);
		cl_assert(cte != NULL);
		cl_assert_equal_s(*scan, git_tree_entry_name(cte));
	}
	for (scan = the_paths; *scan; ++scan) {
		cl_git_pass(git_tree_entry_bypath(&te, tree, *scan));
		cl_assert_equal_s(*scan, git_tree_entry_name(te));
		git_tree_entry_free(te);
	}

	git_tree_free(tree);

	/* let's try longer paths */
	cl_git_pass(git_treebuilder_new(&builder, g_repo, NULL));
	for (scan = the_paths; *scan; ++scan) {
		cl_git_pass(git_treebuilder_insert(
			NULL, builder, *scan, &id, GIT_FILEMODE_TREE));
	}
	cl_git_pass(git_treebuilder_write(&subid, builder));
	git_treebuilder_free(builder);

	/* check data is correct */
	cl_git_pass(git_tree_lookup(&tree, g_repo, &subid));

	cl_assert_equal_i(count, git_tree_entrycount(tree));

	for (i = 0; i < count; ++i) {
		for (j = 0; j < count; ++j) {
			git_buf b = GIT_BUF_INIT;
			cl_git_pass(git_buf_joinpath(&b, the_paths[i], the_paths[j]));
			cl_git_pass(git_tree_entry_bypath(&te, tree, b.ptr));
			cl_assert_equal_s(the_paths[j], git_tree_entry_name(te));
			git_tree_entry_free(te);
			git_buf_dispose(&b);
		}
	}

	git_tree_free(tree);
}

void test_object_tree_write__protect_filesystems(void)
{
	git_treebuilder *builder;
	git_oid bid;

	cl_git_pass(git_oid_fromstr(&bid, "fa49b077972391ad58037050f2a75f74e3671e92"));

	/* Ensure that (by default) we can write objects with funny names on
	 * platforms that are not affected.
	 */
	cl_git_pass(git_treebuilder_new(&builder, g_repo, NULL));

	cl_git_fail(git_treebuilder_insert(NULL, builder, ".git.", &bid, GIT_FILEMODE_BLOB));
	cl_git_fail(git_treebuilder_insert(NULL, builder, "git~1", &bid, GIT_FILEMODE_BLOB));

#ifndef __APPLE__
	cl_git_pass(git_treebuilder_insert(NULL, builder, ".git\xef\xbb\xbf", &bid, GIT_FILEMODE_BLOB));
	cl_git_pass(git_treebuilder_insert(NULL, builder, ".git\xe2\x80\xad", &bid, GIT_FILEMODE_BLOB));
#endif

	git_treebuilder_free(builder);

	/* Now turn on core.protectHFS and core.protectNTFS and validate that these
	 * paths are rejected.
	 */

	cl_repo_set_bool(g_repo, "core.protectHFS", true);
	cl_repo_set_bool(g_repo, "core.protectNTFS", true);

	cl_git_pass(git_treebuilder_new(&builder, g_repo, NULL));

	cl_git_fail(git_treebuilder_insert(NULL, builder, ".git.", &bid, GIT_FILEMODE_BLOB));
	cl_git_fail(git_treebuilder_insert(NULL, builder, "git~1", &bid, GIT_FILEMODE_BLOB));

	cl_git_fail(git_treebuilder_insert(NULL, builder, ".git\xef\xbb\xbf", &bid, GIT_FILEMODE_BLOB));
	cl_git_fail(git_treebuilder_insert(NULL, builder, ".git\xe2\x80\xad", &bid, GIT_FILEMODE_BLOB));
	cl_git_fail(git_treebuilder_insert(NULL, builder, ".git::$INDEX_ALLOCATION/dummy-file", &bid, GIT_FILEMODE_BLOB));

	git_treebuilder_free(builder);
}

static void test_invalid_objects(bool should_allow_invalid)
{
	git_treebuilder *builder;
	git_oid valid_blob_id, invalid_blob_id, valid_tree_id, invalid_tree_id;

#define assert_allowed(expr) \
	clar__assert(!(expr) == should_allow_invalid, \
		__FILE__, __func__, __LINE__, \
		(should_allow_invalid ? \
		 "Expected function call to succeed: " #expr : \
		 "Expected function call to fail: " #expr), \
		NULL, 1)

	cl_git_pass(git_oid_fromstr(&valid_blob_id, blob_oid));
	cl_git_pass(git_oid_fromstr(&invalid_blob_id,
		"1234567890123456789012345678901234567890"));
	cl_git_pass(git_oid_fromstr(&valid_tree_id, first_tree));
	cl_git_pass(git_oid_fromstr(&invalid_tree_id,
		"0000000000111111111122222222223333333333"));

	cl_git_pass(git_treebuilder_new(&builder, g_repo, NULL));

	/* test valid blobs and trees (these should always pass) */
	cl_git_pass(git_treebuilder_insert(NULL, builder, "file.txt", &valid_blob_id, GIT_FILEMODE_BLOB));
	cl_git_pass(git_treebuilder_insert(NULL, builder, "folder", &valid_tree_id, GIT_FILEMODE_TREE));

	/* replace valid files and folders with invalid ones */
	assert_allowed(git_treebuilder_insert(NULL, builder, "file.txt", &invalid_blob_id, GIT_FILEMODE_BLOB));
	assert_allowed(git_treebuilder_insert(NULL, builder, "folder", &invalid_blob_id, GIT_FILEMODE_BLOB));

	/* insert new invalid files and folders */
	assert_allowed(git_treebuilder_insert(NULL, builder, "invalid_file.txt", &invalid_blob_id, GIT_FILEMODE_BLOB));
	assert_allowed(git_treebuilder_insert(NULL, builder, "invalid_folder", &invalid_blob_id, GIT_FILEMODE_BLOB));

	/* insert valid blobs as trees and trees as blobs */
	assert_allowed(git_treebuilder_insert(NULL, builder, "file_as_folder", &valid_blob_id, GIT_FILEMODE_TREE));
	assert_allowed(git_treebuilder_insert(NULL, builder, "folder_as_file.txt", &valid_tree_id, GIT_FILEMODE_BLOB));

#undef assert_allowed

	git_treebuilder_free(builder);
}

static void test_inserting_submodule(void)
{
	git_treebuilder *bld;
	git_oid sm_id;

	cl_git_pass(git_oid_fromstr(&sm_id, "da39a3ee5e6b4b0d3255bfef95601890afd80709"));
	cl_git_pass(git_treebuilder_new(&bld, g_repo, NULL));
	cl_git_pass(git_treebuilder_insert(NULL, bld, "sm", &sm_id, GIT_FILEMODE_COMMIT));
	git_treebuilder_free(bld);
}

void test_object_tree_write__object_validity(void)
{
	/* Ensure that we cannot add invalid objects by default */
	test_invalid_objects(false);
	test_inserting_submodule();

	/* Ensure that we can turn off validation */
	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_OBJECT_CREATION, 0));
	test_invalid_objects(true);
	test_inserting_submodule();
}

void test_object_tree_write__invalid_null_oid(void)
{
	git_treebuilder *bld;
	git_oid null_oid = {{0}};

	cl_git_pass(git_treebuilder_new(&bld, g_repo, NULL));
	cl_git_fail(git_treebuilder_insert(NULL, bld, "null_oid_file", &null_oid, GIT_FILEMODE_BLOB));
	cl_assert(git_error_last() && strstr(git_error_last()->message, "null OID") != NULL);

	git_treebuilder_free(bld);
}
