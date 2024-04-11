#include "clar_libgit2.h"
#include "checkout_helpers.h"

#include "git2/checkout.h"
#include "futils.h"
#include "repository.h"
#include "index.h"
#include "remote.h"
#include "repo/repo_helpers.h"

static git_repository *g_repo;
static git_str g_global_path = GIT_STR_INIT;

void test_checkout_index__initialize(void)
{
	git_tree *tree;

	g_repo = cl_git_sandbox_init("testrepo");

	cl_git_pass(git_repository_head_tree(&tree, g_repo));

	reset_index_to_treeish((git_object *)tree);
	git_tree_free(tree);

	cl_git_rewritefile(
		"./testrepo/.gitattributes",
		"* text eol=lf\n");

	git_libgit2_opts(GIT_OPT_GET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL,
		&g_global_path);
}

void test_checkout_index__cleanup(void)
{
	git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL,
		g_global_path.ptr);
	git_str_dispose(&g_global_path);

	cl_git_sandbox_cleanup();

	/* try to remove directories created by tests */
	cl_fixture_cleanup("alternative");
	cl_fixture_cleanup("symlink");
	cl_fixture_cleanup("symlink.git");
	cl_fixture_cleanup("tmp_global_path");
}

void test_checkout_index__cannot_checkout_a_bare_repository(void)
{
	cl_git_sandbox_cleanup();
	g_repo = cl_git_sandbox_init("testrepo.git");

	cl_git_fail(git_checkout_index(g_repo, NULL, NULL));
}

void test_checkout_index__can_create_missing_files(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/README"));
	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/branch_file.txt"));
	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/new.txt"));

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	check_file_contents("./testrepo/README", "hey there\n");
	check_file_contents("./testrepo/branch_file.txt", "hi\nbye!\n");
	check_file_contents("./testrepo/new.txt", "my new file\n");
}

void test_checkout_index__can_remove_untracked_files(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	git_futils_mkdir("./testrepo/dir/subdir/subsubdir", 0755, GIT_MKDIR_PATH);
	cl_git_mkfile("./testrepo/dir/one", "one\n");
	cl_git_mkfile("./testrepo/dir/subdir/two", "two\n");

	cl_assert_equal_i(true, git_fs_path_isdir("./testrepo/dir/subdir/subsubdir"));

	opts.checkout_strategy =
		GIT_CHECKOUT_SAFE |
		GIT_CHECKOUT_RECREATE_MISSING |
		GIT_CHECKOUT_REMOVE_UNTRACKED;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	cl_assert_equal_i(false, git_fs_path_isdir("./testrepo/dir"));
}

void test_checkout_index__can_disable_pathspec_match(void)
{
	char *files_to_checkout[] = { "test10.txt", "test11.txt"};
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_object *objects;
	git_index *index;

	/* reset to beginning of history (i.e. just a README file) */
	opts.checkout_strategy = GIT_CHECKOUT_FORCE | GIT_CHECKOUT_REMOVE_UNTRACKED;

	cl_git_pass(git_revparse_single(&objects, g_repo, "8496071c1b46c854b31185ea97743be6a8774479"));
	cl_git_pass(git_checkout_tree(g_repo, objects, &opts));
	cl_git_pass(git_repository_set_head_detached(g_repo, git_object_id(objects)));
	git_object_free(objects);

	cl_git_pass(git_repository_index(&index, g_repo));

	/* We create 4 files and commit them */
	cl_git_mkfile("testrepo/test9.txt", "original\n");
	cl_git_mkfile("testrepo/test10.txt", "original\n");
	cl_git_mkfile("testrepo/test11.txt", "original\n");
	cl_git_mkfile("testrepo/test12.txt", "original\n");

	cl_git_pass(git_index_add_bypath(index, "test9.txt"));
	cl_git_pass(git_index_add_bypath(index, "test10.txt"));
	cl_git_pass(git_index_add_bypath(index, "test11.txt"));
	cl_git_pass(git_index_add_bypath(index, "test12.txt"));
	cl_git_pass(git_index_write(index));

	cl_repo_commit_from_index(NULL, g_repo, NULL, 0, "commit our test files");

	/* We modify the content of all 4 of our files */
	cl_git_rewritefile("testrepo/test9.txt", "modified\n");
	cl_git_rewritefile("testrepo/test10.txt", "modified\n");
	cl_git_rewritefile("testrepo/test11.txt", "modified\n");
	cl_git_rewritefile("testrepo/test12.txt", "modified\n");

	/* We checkout only test10.txt and test11.txt */
	opts.checkout_strategy =
		GIT_CHECKOUT_FORCE |
		GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH;
	opts.paths.strings = files_to_checkout;
	opts.paths.count = ARRAY_SIZE(files_to_checkout);
	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	/* The only files that have been reverted to their original content
	   should be test10.txt and test11.txt */
	check_file_contents("testrepo/test9.txt", "modified\n");
	check_file_contents("testrepo/test10.txt", "original\n");
	check_file_contents("testrepo/test11.txt", "original\n");
	check_file_contents("testrepo/test12.txt", "modified\n");

	git_index_free(index);
}

void test_checkout_index__honor_the_specified_pathspecs(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	char *entries[] = { "*.txt" };

	opts.paths.strings = entries;
	opts.paths.count = 1;

	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/README"));
	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/branch_file.txt"));
	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/new.txt"));

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/README"));
	check_file_contents("./testrepo/branch_file.txt", "hi\nbye!\n");
	check_file_contents("./testrepo/new.txt", "my new file\n");
}

void test_checkout_index__honor_the_gitattributes_directives(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	const char *attributes =
		"branch_file.txt text eol=crlf\n"
		"new.txt text eol=lf\n";

	cl_git_mkfile("./testrepo/.gitattributes", attributes);
	cl_repo_set_bool(g_repo, "core.autocrlf", false);

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	check_file_contents("./testrepo/README", "hey there\n");
	check_file_contents("./testrepo/new.txt", "my new file\n");
	check_file_contents("./testrepo/branch_file.txt", "hi\r\nbye!\r\n");
}

void test_checkout_index__honor_coreautocrlf_setting_set_to_true(void)
{
#ifdef GIT_WIN32
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	const char *expected_readme_text = "hey there\r\n";

	cl_git_pass(p_unlink("./testrepo/.gitattributes"));
	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	check_file_contents("./testrepo/README", expected_readme_text);
#endif
}

static void populate_symlink_workdir(void)
{
	git_str path = GIT_STR_INIT;
	git_repository *repo;
	git_remote *origin;
	git_object *target;

	const char *url = git_repository_path(g_repo);

	cl_git_pass(git_str_joinpath(&path, clar_sandbox_path(), "symlink.git"));
	cl_git_pass(git_repository_init(&repo, path.ptr, true));
	cl_git_pass(git_repository_set_workdir(repo, "symlink", 1));

	/* Delete the `origin` repo (if it exists) so we can recreate it. */
	git_remote_delete(repo, GIT_REMOTE_ORIGIN);

	cl_git_pass(git_remote_create(&origin, repo, GIT_REMOTE_ORIGIN, url));
	cl_git_pass(git_remote_fetch(origin, NULL, NULL, NULL));
	git_remote_free(origin);

	cl_git_pass(git_revparse_single(&target, repo, "remotes/origin/master"));
	cl_git_pass(git_reset(repo, target, GIT_RESET_HARD, NULL));

	git_object_free(target);
	git_repository_free(repo);
	git_str_dispose(&path);
}

void test_checkout_index__honor_coresymlinks_default_true(void)
{
	char link_data[GIT_PATH_MAX];
	int link_size = GIT_PATH_MAX;

	cl_must_pass(p_mkdir("symlink", 0777));

	if (!git_fs_path_supports_symlinks("symlink/test"))
		cl_skip();

#ifdef GIT_WIN32
	/*
	 * Windows explicitly requires the global configuration to have
	 * core.symlinks=true in addition to actual filesystem support.
	 */
	create_tmp_global_config("tmp_global_path", "core.symlinks", "true");
#endif

	populate_symlink_workdir();

	link_size = p_readlink("./symlink/link_to_new.txt", link_data, link_size);
	cl_assert(link_size >= 0);

	link_data[link_size] = '\0';
	cl_assert_equal_i(link_size, strlen("new.txt"));
	cl_assert_equal_s(link_data, "new.txt");
	check_file_contents("./symlink/link_to_new.txt", "my new file\n");
}

void test_checkout_index__honor_coresymlinks_default_false(void)
{
	cl_must_pass(p_mkdir("symlink", 0777));

#ifndef GIT_WIN32
	/*
	 * This test is largely for Windows platforms to ensure that
	 * we respect an unset core.symlinks even when the platform
	 * supports symlinks.  Bail entirely on POSIX platforms that
	 * do support symlinks.
	 */
	if (git_fs_path_supports_symlinks("symlink/test"))
		cl_skip();
#endif

	populate_symlink_workdir();
	check_file_contents("./symlink/link_to_new.txt", "new.txt");
}

void test_checkout_index__coresymlinks_set_to_true_fails_when_unsupported(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	if (git_fs_path_supports_symlinks("testrepo/test")) {
		cl_skip();
	}

	cl_repo_set_bool(g_repo, "core.symlinks", true);

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;
	cl_git_fail(git_checkout_index(g_repo, NULL, &opts));
}

void test_checkout_index__honor_coresymlinks_setting_set_to_true(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	char link_data[GIT_PATH_MAX];
	size_t link_size = GIT_PATH_MAX;

	if (!git_fs_path_supports_symlinks("testrepo/test")) {
		cl_skip();
	}

	cl_repo_set_bool(g_repo, "core.symlinks", true);

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	link_size = p_readlink("./testrepo/link_to_new.txt", link_data, link_size);
	link_data[link_size] = '\0';
	cl_assert_equal_i(link_size, strlen("new.txt"));
	cl_assert_equal_s(link_data, "new.txt");
	check_file_contents("./testrepo/link_to_new.txt", "my new file\n");
}

void test_checkout_index__honor_coresymlinks_setting_set_to_false(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_repo_set_bool(g_repo, "core.symlinks", false);

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	check_file_contents("./testrepo/link_to_new.txt", "new.txt");
}

void test_checkout_index__donot_overwrite_modified_file_by_default(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_git_mkfile("./testrepo/new.txt", "This isn't what's stored!");

	/* set this up to not return an error code on conflicts, but it
	 * still will not have permission to overwrite anything...
	 */
	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_ALLOW_CONFLICTS;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	check_file_contents("./testrepo/new.txt", "This isn't what's stored!");
}

void test_checkout_index__can_overwrite_modified_file(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_git_mkfile("./testrepo/new.txt", "This isn't what's stored!");

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	check_file_contents("./testrepo/new.txt", "my new file\n");
}

void test_checkout_index__options_disable_filters(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_git_mkfile("./testrepo/.gitattributes", "*.txt text eol=crlf\n");

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;
	opts.disable_filters = false;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	check_file_contents("./testrepo/new.txt", "my new file\r\n");

	p_unlink("./testrepo/new.txt");

	opts.disable_filters = true;
	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	check_file_contents("./testrepo/new.txt", "my new file\n");
}

void test_checkout_index__options_dir_modes(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	struct stat st;
	git_oid oid;
	git_commit *commit;
	mode_t um;

	if (!cl_is_chmod_supported())
		return;

	cl_git_pass(git_reference_name_to_id(&oid, g_repo, "refs/heads/dir"));
	cl_git_pass(git_commit_lookup(&commit, g_repo, &oid));

	reset_index_to_treeish((git_object *)commit);

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;
	opts.dir_mode = 0701;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	/* umask will influence actual directory creation mode */
	(void)p_umask(um = p_umask(022));

	cl_git_pass(p_stat("./testrepo/a", &st));
	/* Haiku & Hurd use other mode bits, so we must mask them out */
	cl_assert_equal_i_fmt(st.st_mode & (S_IFMT | 07777), (GIT_FILEMODE_TREE | 0701) & ~um, "%07o");

	/* File-mode test, since we're on the 'dir' branch */
	cl_git_pass(p_stat("./testrepo/a/b.txt", &st));
	cl_assert_equal_i_fmt(st.st_mode & (S_IFMT | 07777), GIT_FILEMODE_BLOB_EXECUTABLE & ~um, "%07o");

	git_commit_free(commit);
}

void test_checkout_index__options_override_file_modes(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	struct stat st;

	if (!cl_is_chmod_supported())
		return;

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;
	opts.file_mode = 0700;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	cl_git_pass(p_stat("./testrepo/new.txt", &st));
	cl_assert_equal_i_fmt(st.st_mode & GIT_MODE_PERMS_MASK, 0700, "%07o");
}

void test_checkout_index__options_open_flags(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_git_mkfile("./testrepo/new.txt", "hi\n");

	opts.checkout_strategy =
		GIT_CHECKOUT_FORCE | GIT_CHECKOUT_DONT_REMOVE_EXISTING;
	opts.file_open_flags = O_CREAT | O_RDWR | O_APPEND;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	check_file_contents("./testrepo/new.txt", "hi\nmy new file\n");
}

struct notify_data {
	const char *file;
	const char *sha;
};

static int test_checkout_notify_cb(
	git_checkout_notify_t why,
	const char *path,
	const git_diff_file *baseline,
	const git_diff_file *target,
	const git_diff_file *workdir,
	void *payload)
{
	struct notify_data *expectations = (struct notify_data *)payload;

	GIT_UNUSED(workdir);

	cl_assert_equal_i(GIT_CHECKOUT_NOTIFY_CONFLICT, why);
	cl_assert_equal_s(expectations->file, path);
	cl_assert_equal_i(0, git_oid_streq(&baseline->id, expectations->sha));
	cl_assert_equal_i(0, git_oid_streq(&target->id, expectations->sha));

	return 0;
}

void test_checkout_index__can_notify_of_skipped_files(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	struct notify_data data;

	cl_git_mkfile("./testrepo/new.txt", "This isn't what's stored!");

	/*
	 * $ git ls-tree HEAD
	 * 100644 blob a8233120f6ad708f843d861ce2b7228ec4e3dec6    README
	 * 100644 blob 3697d64be941a53d4ae8f6a271e4e3fa56b022cc    branch_file.txt
	 * 100644 blob a71586c1dfe8a71c6cbf6c129f404c5642ff31bd    new.txt
	 */
	data.file = "new.txt";
	data.sha = "a71586c1dfe8a71c6cbf6c129f404c5642ff31bd";

	opts.checkout_strategy = GIT_CHECKOUT_SAFE |
		GIT_CHECKOUT_RECREATE_MISSING |
		GIT_CHECKOUT_ALLOW_CONFLICTS;
	opts.notify_flags = GIT_CHECKOUT_NOTIFY_CONFLICT;
	opts.notify_cb = test_checkout_notify_cb;
	opts.notify_payload = &data;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));
}

static int dont_notify_cb(
	git_checkout_notify_t why,
	const char *path,
	const git_diff_file *baseline,
	const git_diff_file *target,
	const git_diff_file *workdir,
	void *payload)
{
	GIT_UNUSED(why);
	GIT_UNUSED(path);
	GIT_UNUSED(baseline);
	GIT_UNUSED(target);
	GIT_UNUSED(workdir);
	GIT_UNUSED(payload);

	cl_assert(false);

	return 0;
}

void test_checkout_index__wont_notify_of_expected_line_ending_changes(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_git_pass(p_unlink("./testrepo/.gitattributes"));
	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	cl_git_mkfile("./testrepo/new.txt", "my new file\r\n");

	opts.checkout_strategy =
		GIT_CHECKOUT_SAFE |
		GIT_CHECKOUT_RECREATE_MISSING |
		GIT_CHECKOUT_ALLOW_CONFLICTS;
	opts.notify_flags = GIT_CHECKOUT_NOTIFY_CONFLICT;
	opts.notify_cb = dont_notify_cb;
	opts.notify_payload = NULL;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));
}

static void checkout_progress_counter(
	const char *path, size_t cur, size_t tot, void *payload)
{
	GIT_UNUSED(path); GIT_UNUSED(cur); GIT_UNUSED(tot);
	(*(int *)payload)++;
}

void test_checkout_index__calls_progress_callback(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	int calls = 0;

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;
	opts.progress_cb = checkout_progress_counter;
	opts.progress_payload = &calls;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));
	cl_assert(calls > 0);
}

void test_checkout_index__can_overcome_name_clashes(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;

	cl_git_pass(git_repository_index(&index, g_repo));
	git_index_clear(index);

	cl_git_mkfile("./testrepo/path0", "content\r\n");
	cl_git_pass(p_mkdir("./testrepo/path1", 0777));
	cl_git_mkfile("./testrepo/path1/file1", "content\r\n");

	cl_git_pass(git_index_add_bypath(index, "path0"));
	cl_git_pass(git_index_add_bypath(index, "path1/file1"));

	cl_git_pass(p_unlink("./testrepo/path0"));
	cl_git_pass(git_futils_rmdir_r(
		"./testrepo/path1", NULL, GIT_RMDIR_REMOVE_FILES));

	cl_git_mkfile("./testrepo/path1", "content\r\n");
	cl_git_pass(p_mkdir("./testrepo/path0", 0777));
	cl_git_mkfile("./testrepo/path0/file0", "content\r\n");

	cl_assert(git_fs_path_isfile("./testrepo/path1"));
	cl_assert(git_fs_path_isfile("./testrepo/path0/file0"));

	opts.checkout_strategy =
		GIT_CHECKOUT_SAFE |
		GIT_CHECKOUT_RECREATE_MISSING |
		GIT_CHECKOUT_ALLOW_CONFLICTS;
	cl_git_pass(git_checkout_index(g_repo, index, &opts));

	cl_assert(git_fs_path_isfile("./testrepo/path1"));
	cl_assert(git_fs_path_isfile("./testrepo/path0/file0"));

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;
	cl_git_pass(git_checkout_index(g_repo, index, &opts));

	cl_assert(git_fs_path_isfile("./testrepo/path0"));
	cl_assert(git_fs_path_isfile("./testrepo/path1/file1"));

	git_index_free(index);
}

void test_checkout_index__validates_struct_version(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	const git_error *err;

	opts.version = 1024;
	cl_git_fail(git_checkout_index(g_repo, NULL, &opts));

	err = git_error_last();
	cl_assert_equal_i(err->klass, GIT_ERROR_INVALID);

	opts.version = 0;
	git_error_clear();
	cl_git_fail(git_checkout_index(g_repo, NULL, &opts));

	err = git_error_last();
	cl_assert_equal_i(err->klass, GIT_ERROR_INVALID);
}

void test_checkout_index__can_update_prefixed_files(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/README"));
	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/branch_file.txt"));
	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/new.txt"));

	cl_git_mkfile("./testrepo/READ", "content\n");
	cl_git_mkfile("./testrepo/README.after", "content\n");
	cl_git_pass(p_mkdir("./testrepo/branch_file", 0777));
	cl_git_pass(p_mkdir("./testrepo/branch_file/contained_dir", 0777));
	cl_git_mkfile("./testrepo/branch_file/contained_file", "content\n");
	cl_git_pass(p_mkdir("./testrepo/branch_file.txt.after", 0777));

	opts.checkout_strategy =
		GIT_CHECKOUT_SAFE |
		GIT_CHECKOUT_RECREATE_MISSING |
		GIT_CHECKOUT_REMOVE_UNTRACKED;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	/* remove untracked will remove the .gitattributes file before the blobs
	 * were created, so they will have had crlf filtering applied on Windows
	 */
	check_file_contents_nocr("./testrepo/README", "hey there\n");
	check_file_contents_nocr("./testrepo/branch_file.txt", "hi\nbye!\n");
	check_file_contents_nocr("./testrepo/new.txt", "my new file\n");

	cl_assert(!git_fs_path_exists("testrepo/READ"));
	cl_assert(!git_fs_path_exists("testrepo/README.after"));
	cl_assert(!git_fs_path_exists("testrepo/branch_file"));
	cl_assert(!git_fs_path_exists("testrepo/branch_file.txt.after"));
}

void test_checkout_index__can_checkout_a_newly_initialized_repository(void)
{
	cl_git_sandbox_cleanup();
	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_remove_placeholders(git_repository_path(g_repo), "dummy-marker.txt");

	cl_git_pass(git_checkout_index(g_repo, NULL, NULL));
}

void test_checkout_index__issue_1397(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_git_sandbox_cleanup();
	g_repo = cl_git_sandbox_init("issue_1397");

	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	check_file_contents("./issue_1397/crlf_file.txt", "first line\r\nsecond line\r\nboth with crlf");
}

void test_checkout_index__target_directory(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	checkout_counts cts;
	memset(&cts, 0, sizeof(cts));

	opts.checkout_strategy = GIT_CHECKOUT_SAFE |
		GIT_CHECKOUT_RECREATE_MISSING;
	opts.target_directory = "alternative";
	cl_assert(!git_fs_path_isdir("alternative"));

	opts.notify_flags = GIT_CHECKOUT_NOTIFY_ALL;
	opts.notify_cb = checkout_count_callback;
	opts.notify_payload = &cts;

	/* create some files that *would* conflict if we were using the wd */
	cl_git_mkfile("testrepo/README", "I'm in the way!\n");
	cl_git_mkfile("testrepo/new.txt", "my new file\n");

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	cl_assert_equal_i(0, cts.n_untracked);
	cl_assert_equal_i(0, cts.n_ignored);
	cl_assert_equal_i(4, cts.n_updates);

	check_file_contents("./alternative/README", "hey there\n");
	check_file_contents("./alternative/branch_file.txt", "hi\nbye!\n");
	check_file_contents("./alternative/new.txt", "my new file\n");

	cl_git_pass(git_futils_rmdir_r(
		"alternative", NULL, GIT_RMDIR_REMOVE_FILES));
}

void test_checkout_index__target_directory_from_bare(void)
{
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_index *index;
	git_object *head = NULL;
	checkout_counts cts;
	memset(&cts, 0, sizeof(cts));

	cl_git_sandbox_cleanup();
	g_repo = cl_git_sandbox_init("testrepo.git");
	cl_assert(git_repository_is_bare(g_repo));

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_revparse_single(&head, g_repo, "HEAD^{tree}"));
	cl_git_pass(git_index_read_tree(index, (const git_tree *)head));
	cl_git_pass(git_index_write(index));
	git_index_free(index);

	opts.checkout_strategy = GIT_CHECKOUT_SAFE |
		GIT_CHECKOUT_RECREATE_MISSING;

	opts.notify_flags = GIT_CHECKOUT_NOTIFY_ALL;
	opts.notify_cb = checkout_count_callback;
	opts.notify_payload = &cts;

	/* fail to checkout a bare repo */
	cl_git_fail(git_checkout_index(g_repo, NULL, &opts));

	opts.target_directory = "alternative";
	cl_assert(!git_fs_path_isdir("alternative"));

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	cl_assert_equal_i(0, cts.n_untracked);
	cl_assert_equal_i(0, cts.n_ignored);
	cl_assert_equal_i(3, cts.n_updates);

	/* files will have been filtered if needed, so strip CR */
	check_file_contents_nocr("./alternative/README", "hey there\n");
	check_file_contents_nocr("./alternative/branch_file.txt", "hi\nbye!\n");
	check_file_contents_nocr("./alternative/new.txt", "my new file\n");

	cl_git_pass(git_futils_rmdir_r(
		"alternative", NULL, GIT_RMDIR_REMOVE_FILES));

	git_object_free(head);
}

void test_checkout_index__can_get_repo_from_index(void)
{
	git_index *index;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/README"));
	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/branch_file.txt"));
	cl_assert_equal_i(false, git_fs_path_isfile("./testrepo/new.txt"));

	opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_RECREATE_MISSING;

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(git_checkout_index(NULL, index, &opts));

	check_file_contents("./testrepo/README", "hey there\n");
	check_file_contents("./testrepo/branch_file.txt", "hi\nbye!\n");
	check_file_contents("./testrepo/new.txt", "my new file\n");

	git_index_free(index);
}

static void add_conflict(git_index *index, const char *path)
{
	git_index_entry entry;

	memset(&entry, 0, sizeof(git_index_entry));

	entry.mode = 0100644;
	entry.path = path;

	git_oid__fromstr(&entry.id, "d427e0b2e138501a3d15cc376077a3631e15bd46", GIT_OID_SHA1);
	GIT_INDEX_ENTRY_STAGE_SET(&entry, 1);
	cl_git_pass(git_index_add(index, &entry));

	git_oid__fromstr(&entry.id, "4e886e602529caa9ab11d71f86634bd1b6e0de10", GIT_OID_SHA1);
	GIT_INDEX_ENTRY_STAGE_SET(&entry, 2);
	cl_git_pass(git_index_add(index, &entry));

	git_oid__fromstr(&entry.id, "2bd0a343aeef7a2cf0d158478966a6e587ff3863", GIT_OID_SHA1);
	GIT_INDEX_ENTRY_STAGE_SET(&entry, 3);
	cl_git_pass(git_index_add(index, &entry));
}

void test_checkout_index__writes_conflict_file(void)
{
	git_index *index;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_str conflicting_buf = GIT_STR_INIT;

	cl_git_pass(git_repository_index(&index, g_repo));

	add_conflict(index, "conflicting.txt");
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	cl_git_pass(git_futils_readbuffer(&conflicting_buf, "testrepo/conflicting.txt"));
	cl_assert(strcmp(conflicting_buf.ptr,
		"<<<<<<< ours\n"
		"this file is changed in master and branch\n"
		"=======\n"
		"this file is changed in branch and master\n"
		">>>>>>> theirs\n") == 0);
	git_str_dispose(&conflicting_buf);

	git_index_free(index);
}

void test_checkout_index__adding_conflict_removes_stage_0(void)
{
	git_index *new_index, *index;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_git_pass(git_index__new(&new_index, GIT_OID_SHA1));

	add_conflict(new_index, "new.txt");
	cl_git_pass(git_checkout_index(g_repo, new_index, &opts));

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_assert(git_index_get_bypath(index, "new.txt", 0) == NULL);
	cl_assert(git_index_get_bypath(index, "new.txt", 1) != NULL);
	cl_assert(git_index_get_bypath(index, "new.txt", 2) != NULL);
	cl_assert(git_index_get_bypath(index, "new.txt", 3) != NULL);

	git_index_free(index);
	git_index_free(new_index);
}

void test_checkout_index__conflicts_honor_coreautocrlf(void)
{
#ifdef GIT_WIN32
	git_index *index;
	git_checkout_options opts = GIT_CHECKOUT_OPTIONS_INIT;
	git_str conflicting_buf = GIT_STR_INIT;

	cl_git_pass(p_unlink("./testrepo/.gitattributes"));
	cl_repo_set_bool(g_repo, "core.autocrlf", true);

	cl_git_pass(git_repository_index(&index, g_repo));

	add_conflict(index, "conflicting.txt");
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_checkout_index(g_repo, NULL, &opts));

	cl_git_pass(git_futils_readbuffer(&conflicting_buf, "testrepo/conflicting.txt"));
	cl_assert(strcmp(conflicting_buf.ptr,
		"<<<<<<< ours\r\n"
		"this file is changed in master and branch\r\n"
		"=======\r\n"
		"this file is changed in branch and master\r\n"
		">>>>>>> theirs\r\n") == 0);
	git_str_dispose(&conflicting_buf);

	git_index_free(index);
#endif
}
