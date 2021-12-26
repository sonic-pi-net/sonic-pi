#include "clar_libgit2.h"

static git_repository *g_repo = NULL;
static git_index *g_index = NULL;

static const char *valid_blob_id = "fa49b077972391ad58037050f2a75f74e3671e92";
static const char *valid_tree_id = "181037049a54a1eb5fab404658a3a250b44335d7";
static const char *valid_commit_id = "763d71aadf09a7951596c9746c024e7eece7c7af";
static const char *invalid_id = "1234567890123456789012345678901234567890";

void test_index_add__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");
	cl_git_pass(git_repository_index(&g_index, g_repo));
}

void test_index_add__cleanup(void)
{
	git_index_free(g_index);
	cl_git_sandbox_cleanup();
	g_repo = NULL;

	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_OBJECT_CREATION, 1));
}

static void test_add_entry(
	bool should_succeed, const char *idstr, git_filemode_t mode)
{
	git_index_entry entry = {{0}};

	cl_git_pass(git_oid_fromstr(&entry.id, idstr));

	entry.path = mode == GIT_FILEMODE_TREE ? "test_folder" : "test_file";
	entry.mode = mode;

	if (should_succeed)
		cl_git_pass(git_index_add(g_index, &entry));
	else
		cl_git_fail(git_index_add(g_index, &entry));
}

void test_index_add__invalid_entries_succeeds_by_default(void)
{
	/*
	 * Ensure that there is validation on object ids by default
	 */

	/* ensure that we can add some actually good entries */
	test_add_entry(true, valid_blob_id, GIT_FILEMODE_BLOB);
	test_add_entry(true, valid_blob_id, GIT_FILEMODE_BLOB_EXECUTABLE);
	test_add_entry(true, valid_blob_id, GIT_FILEMODE_LINK);

	/* test that we fail to add some invalid (missing) blobs and trees */
	test_add_entry(false, invalid_id, GIT_FILEMODE_BLOB);
	test_add_entry(false, invalid_id, GIT_FILEMODE_BLOB_EXECUTABLE);
	test_add_entry(false, invalid_id, GIT_FILEMODE_LINK);

	/* test that we validate the types of objects */
	test_add_entry(false, valid_commit_id, GIT_FILEMODE_BLOB);
	test_add_entry(false, valid_tree_id, GIT_FILEMODE_BLOB_EXECUTABLE);
	test_add_entry(false, valid_commit_id, GIT_FILEMODE_LINK);

	/*
	 * Ensure that there we can disable validation
	 */

	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_OBJECT_CREATION, 0));

	/* ensure that we can add some actually good entries */
	test_add_entry(true, valid_blob_id, GIT_FILEMODE_BLOB);
	test_add_entry(true, valid_blob_id, GIT_FILEMODE_BLOB_EXECUTABLE);
	test_add_entry(true, valid_blob_id, GIT_FILEMODE_LINK);

	/* test that we can now add some invalid (missing) blobs and trees */
	test_add_entry(true, invalid_id, GIT_FILEMODE_BLOB);
	test_add_entry(true, invalid_id, GIT_FILEMODE_BLOB_EXECUTABLE);
	test_add_entry(true, invalid_id, GIT_FILEMODE_LINK);

	/* test that we do not validate the types of objects */
	test_add_entry(true, valid_commit_id, GIT_FILEMODE_BLOB);
	test_add_entry(true, valid_tree_id, GIT_FILEMODE_BLOB_EXECUTABLE);
	test_add_entry(true, valid_commit_id, GIT_FILEMODE_LINK);
}

