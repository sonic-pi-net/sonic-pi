#include "clar_libgit2.h"
#include "repository.h"
#include "../submodule/submodule_helpers.h"

static git_repository *g_repo;
static git_index *g_idx;

void test_index_bypath__initialize(void)
{
	g_repo = setup_fixture_submod2();
	cl_git_pass(git_repository_index__weakptr(&g_idx, g_repo));
}

void test_index_bypath__cleanup(void)
{
	g_repo = NULL;
	g_idx = NULL;
}

void test_index_bypath__add_directory(void)
{
	cl_git_fail_with(GIT_EDIRECTORY, git_index_add_bypath(g_idx, "just_a_dir"));
}

void test_index_bypath__add_submodule(void)
{
	unsigned int status;
	const char *sm_name = "sm_changed_head";

	cl_git_pass(git_submodule_status(&status, g_repo, sm_name, 0));
	cl_assert_equal_i(GIT_SUBMODULE_STATUS_WD_MODIFIED, status & GIT_SUBMODULE_STATUS_WD_MODIFIED);
	cl_git_pass(git_index_add_bypath(g_idx, sm_name));
	cl_git_pass(git_submodule_status(&status, g_repo, sm_name, 0));
	cl_assert_equal_i(0, status & GIT_SUBMODULE_STATUS_WD_MODIFIED);
}

void test_index_bypath__add_submodule_unregistered(void)
{
	const char *sm_name = "not-submodule";
	const char *sm_head = "68e92c611b80ee1ed8f38314ff9577f0d15b2444";
	const git_index_entry *entry;

	cl_git_pass(git_index_add_bypath(g_idx, sm_name));

	cl_assert(entry = git_index_get_bypath(g_idx, sm_name, 0));
	cl_assert_equal_s(sm_head, git_oid_tostr_s(&entry->id));
	cl_assert_equal_s(sm_name, entry->path);
}

void test_index_bypath__add_hidden(void)
{
#ifdef GIT_WIN32
	const git_index_entry *entry;
	bool hidden;

	cl_git_mkfile("submod2/hidden_file", "you can't see me");

	cl_git_pass(git_win32__hidden(&hidden, "submod2/hidden_file"));
	cl_assert(!hidden);

	cl_git_pass(git_win32__set_hidden("submod2/hidden_file", true));

	cl_git_pass(git_win32__hidden(&hidden, "submod2/hidden_file"));
	cl_assert(hidden);

	cl_git_pass(git_index_add_bypath(g_idx, "hidden_file"));

	cl_assert(entry = git_index_get_bypath(g_idx, "hidden_file", 0));
	cl_assert_equal_i(GIT_FILEMODE_BLOB, entry->mode);
#endif
}

void test_index_bypath__add_keeps_existing_case(void)
{
	const git_index_entry *entry;

	if (!cl_repo_get_bool(g_repo, "core.ignorecase"))
		clar__skip();

	cl_git_mkfile("submod2/just_a_dir/file1.txt", "This is a file");
	cl_git_pass(git_index_add_bypath(g_idx, "just_a_dir/file1.txt"));

	cl_assert(entry = git_index_get_bypath(g_idx, "just_a_dir/file1.txt", 0));
	cl_assert_equal_s("just_a_dir/file1.txt", entry->path);

	cl_git_rewritefile("submod2/just_a_dir/file1.txt", "Updated!");
	cl_git_pass(git_index_add_bypath(g_idx, "just_a_dir/FILE1.txt"));

	cl_assert(entry = git_index_get_bypath(g_idx, "just_a_dir/FILE1.txt", 0));
	cl_assert_equal_s("just_a_dir/file1.txt", entry->path);
}

void test_index_bypath__add_honors_existing_case(void)
{
	const git_index_entry *entry;

	if (!cl_repo_get_bool(g_repo, "core.ignorecase"))
		clar__skip();

	cl_git_mkfile("submod2/just_a_dir/file1.txt", "This is a file");
	cl_git_mkfile("submod2/just_a_dir/file2.txt", "This is another file");
	cl_git_mkfile("submod2/just_a_dir/file3.txt", "This is another file");
	cl_git_mkfile("submod2/just_a_dir/file4.txt", "And another file");

	cl_git_pass(git_index_add_bypath(g_idx, "just_a_dir/File1.txt"));
	cl_git_pass(git_index_add_bypath(g_idx, "JUST_A_DIR/file2.txt"));
	cl_git_pass(git_index_add_bypath(g_idx, "Just_A_Dir/FILE3.txt"));

	cl_assert(entry = git_index_get_bypath(g_idx, "just_a_dir/File1.txt", 0));
	cl_assert_equal_s("just_a_dir/File1.txt", entry->path);

	cl_assert(entry = git_index_get_bypath(g_idx, "JUST_A_DIR/file2.txt", 0));
	cl_assert_equal_s("just_a_dir/file2.txt", entry->path);

	cl_assert(entry = git_index_get_bypath(g_idx, "Just_A_Dir/FILE3.txt", 0));
	cl_assert_equal_s("just_a_dir/FILE3.txt", entry->path);

	cl_git_rewritefile("submod2/just_a_dir/file3.txt", "Rewritten");
	cl_git_pass(git_index_add_bypath(g_idx, "Just_A_Dir/file3.txt"));

	cl_assert(entry = git_index_get_bypath(g_idx, "Just_A_Dir/file3.txt", 0));
	cl_assert_equal_s("just_a_dir/FILE3.txt", entry->path);
}

void test_index_bypath__add_honors_existing_case_2(void)
{
	git_index_entry dummy = { { 0 } };
	const git_index_entry *entry;

	if (!cl_repo_get_bool(g_repo, "core.ignorecase"))
		clar__skip();

	dummy.mode = GIT_FILEMODE_BLOB;
	cl_git_pass(git_oid__fromstr(&dummy.id, "f990a25a74d1a8281ce2ab018ea8df66795cd60b", GIT_OID_SHA1));

	/* note that `git_index_add` does no checking to canonical directories */
	dummy.path = "Just_a_dir/file0.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	dummy.path = "just_a_dir/fileA.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	dummy.path = "Just_A_Dir/fileB.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	dummy.path = "JUST_A_DIR/fileC.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	dummy.path = "just_A_dir/fileD.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	dummy.path = "JUST_a_DIR/fileE.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	cl_git_mkfile("submod2/just_a_dir/file1.txt", "This is a file");
	cl_git_mkfile("submod2/just_a_dir/file2.txt", "This is another file");
	cl_git_mkfile("submod2/just_a_dir/file3.txt", "This is another file");
	cl_git_mkfile("submod2/just_a_dir/file4.txt", "And another file");

	cl_git_pass(git_index_add_bypath(g_idx, "just_a_dir/File1.txt"));
	cl_git_pass(git_index_add_bypath(g_idx, "JUST_A_DIR/file2.txt"));
	cl_git_pass(git_index_add_bypath(g_idx, "Just_A_Dir/FILE3.txt"));
	cl_git_pass(git_index_add_bypath(g_idx, "JusT_A_DIR/FILE4.txt"));

	cl_assert(entry = git_index_get_bypath(g_idx, "just_a_dir/File1.txt", 0));
	cl_assert_equal_s("just_a_dir/File1.txt", entry->path);

	cl_assert(entry = git_index_get_bypath(g_idx, "JUST_A_DIR/file2.txt", 0));
	cl_assert_equal_s("JUST_A_DIR/file2.txt", entry->path);

	cl_assert(entry = git_index_get_bypath(g_idx, "Just_A_Dir/FILE3.txt", 0));
	cl_assert_equal_s("Just_A_Dir/FILE3.txt", entry->path);

	cl_git_rewritefile("submod2/just_a_dir/file3.txt", "Rewritten");
	cl_git_pass(git_index_add_bypath(g_idx, "Just_A_Dir/file3.txt"));

	cl_assert(entry = git_index_get_bypath(g_idx, "Just_A_Dir/file3.txt", 0));
	cl_assert_equal_s("Just_A_Dir/FILE3.txt", entry->path);
}

void test_index_bypath__add_honors_existing_case_3(void)
{
	git_index_entry dummy = { { 0 } };
	const git_index_entry *entry;

	if (!cl_repo_get_bool(g_repo, "core.ignorecase"))
		clar__skip();

	dummy.mode = GIT_FILEMODE_BLOB;
	cl_git_pass(git_oid__fromstr(&dummy.id, "f990a25a74d1a8281ce2ab018ea8df66795cd60b", GIT_OID_SHA1));

	dummy.path = "just_a_dir/filea.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	dummy.path = "Just_A_Dir/fileB.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	dummy.path = "just_A_DIR/FILEC.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	dummy.path = "Just_a_DIR/FileD.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	cl_git_mkfile("submod2/JuSt_A_DiR/fILEE.txt", "This is a file");

	cl_git_pass(git_index_add_bypath(g_idx, "just_a_dir/fILEE.txt"));

	cl_assert(entry = git_index_get_bypath(g_idx, "JUST_A_DIR/fILEE.txt", 0));
	cl_assert_equal_s("just_a_dir/fILEE.txt", entry->path);
}

void test_index_bypath__add_honors_existing_case_4(void)
{
	git_index_entry dummy = { { 0 } };
	const git_index_entry *entry;

	if (!cl_repo_get_bool(g_repo, "core.ignorecase"))
		clar__skip();

	dummy.mode = GIT_FILEMODE_BLOB;
	cl_git_pass(git_oid__fromstr(&dummy.id, "f990a25a74d1a8281ce2ab018ea8df66795cd60b", GIT_OID_SHA1));

	dummy.path = "just_a_dir/a/b/c/d/e/file1.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	dummy.path = "just_a_dir/a/B/C/D/E/file2.txt";
	cl_git_pass(git_index_add(g_idx, &dummy));

	cl_must_pass(p_mkdir("submod2/just_a_dir/a", 0777));
	cl_must_pass(p_mkdir("submod2/just_a_dir/a/b", 0777));
	cl_must_pass(p_mkdir("submod2/just_a_dir/a/b/z", 0777));
	cl_must_pass(p_mkdir("submod2/just_a_dir/a/b/z/y", 0777));
	cl_must_pass(p_mkdir("submod2/just_a_dir/a/b/z/y/x", 0777));

	cl_git_mkfile("submod2/just_a_dir/a/b/z/y/x/FOO.txt", "This is a file");

	cl_git_pass(git_index_add_bypath(g_idx, "just_a_dir/A/b/Z/y/X/foo.txt"));

	cl_assert(entry = git_index_get_bypath(g_idx, "just_a_dir/A/b/Z/y/X/foo.txt", 0));
	cl_assert_equal_s("just_a_dir/a/b/Z/y/X/foo.txt", entry->path);
}

void test_index_bypath__add_honors_mode(void)
{
	const git_index_entry *entry;
	git_index_entry new_entry;

	cl_assert((entry = git_index_get_bypath(g_idx, "README.txt", 0)) != NULL);

	memcpy(&new_entry, entry, sizeof(git_index_entry));
	new_entry.path = "README.txt";
	new_entry.mode = GIT_FILEMODE_BLOB_EXECUTABLE;

	cl_must_pass(p_chmod("submod2/README.txt", GIT_FILEMODE_BLOB_EXECUTABLE));

	cl_git_pass(git_index_add(g_idx, &new_entry));
	cl_git_pass(git_index_write(g_idx));

	cl_git_rewritefile("submod2/README.txt", "Modified but still executable");

	cl_git_pass(git_index_add_bypath(g_idx, "README.txt"));
	cl_git_pass(git_index_write(g_idx));

	cl_assert((entry = git_index_get_bypath(g_idx, "README.txt", 0)) != NULL);
	cl_assert_equal_i(GIT_FILEMODE_BLOB_EXECUTABLE, entry->mode);
}

void test_index_bypath__add_honors_conflict_mode(void)
{
	const git_index_entry *entry;
	git_index_entry new_entry;
	int stage = 0;

	cl_assert((entry = git_index_get_bypath(g_idx, "README.txt", 0)) != NULL);

	memcpy(&new_entry, entry, sizeof(git_index_entry));
	new_entry.path = "README.txt";
	new_entry.mode = GIT_FILEMODE_BLOB_EXECUTABLE;

	cl_must_pass(p_chmod("submod2/README.txt", GIT_FILEMODE_BLOB_EXECUTABLE));

	cl_git_pass(git_index_remove_bypath(g_idx, "README.txt"));

	for (stage = 1; stage <= 3; stage++) {
		new_entry.flags = stage << GIT_INDEX_ENTRY_STAGESHIFT;
		cl_git_pass(git_index_add(g_idx, &new_entry));
	}

	cl_git_pass(git_index_write(g_idx));

	cl_git_rewritefile("submod2/README.txt", "Modified but still executable");

	cl_git_pass(git_index_add_bypath(g_idx, "README.txt"));
	cl_git_pass(git_index_write(g_idx));

	cl_assert((entry = git_index_get_bypath(g_idx, "README.txt", 0)) != NULL);
	cl_assert_equal_i(GIT_FILEMODE_BLOB_EXECUTABLE, entry->mode);
}

void test_index_bypath__add_honors_conflict_case(void)
{
	const git_index_entry *entry;
	git_index_entry new_entry;
	int stage = 0;

	cl_assert((entry = git_index_get_bypath(g_idx, "README.txt", 0)) != NULL);

	memcpy(&new_entry, entry, sizeof(git_index_entry));
	new_entry.path = "README.txt";
	new_entry.mode = GIT_FILEMODE_BLOB_EXECUTABLE;

	cl_must_pass(p_chmod("submod2/README.txt", GIT_FILEMODE_BLOB_EXECUTABLE));

	cl_git_pass(git_index_remove_bypath(g_idx, "README.txt"));

	for (stage = 1; stage <= 3; stage++) {
		new_entry.flags = stage << GIT_INDEX_ENTRY_STAGESHIFT;
		cl_git_pass(git_index_add(g_idx, &new_entry));
	}

	cl_git_pass(git_index_write(g_idx));

	cl_git_rewritefile("submod2/README.txt", "Modified but still executable");

	cl_git_pass(git_index_add_bypath(g_idx, "README.txt"));
	cl_git_pass(git_index_write(g_idx));

	cl_assert((entry = git_index_get_bypath(g_idx, "README.txt", 0)) != NULL);
	cl_assert_equal_i(GIT_FILEMODE_BLOB_EXECUTABLE, entry->mode);
}

void test_index_bypath__add_honors_symlink(void)
{
	const git_index_entry *entry;
	git_index_entry new_entry;
	int symlinks;

	cl_git_pass(git_repository__configmap_lookup(&symlinks, g_repo, GIT_CONFIGMAP_SYMLINKS));

	if (symlinks)
		cl_skip();

	cl_assert((entry = git_index_get_bypath(g_idx, "README.txt", 0)) != NULL);

	memcpy(&new_entry, entry, sizeof(git_index_entry));
	new_entry.path = "README.txt";
	new_entry.mode = GIT_FILEMODE_LINK;

	cl_git_pass(git_index_add(g_idx, &new_entry));
	cl_git_pass(git_index_write(g_idx));

	cl_git_rewritefile("submod2/README.txt", "Modified but still a (fake) symlink");

	cl_git_pass(git_index_add_bypath(g_idx, "README.txt"));
	cl_git_pass(git_index_write(g_idx));

	cl_assert((entry = git_index_get_bypath(g_idx, "README.txt", 0)) != NULL);
	cl_assert_equal_i(GIT_FILEMODE_LINK, entry->mode);
}
