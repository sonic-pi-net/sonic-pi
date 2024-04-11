#include "clar_libgit2.h"
#include "futils.h"
#include "repository.h"
#include "config.h"
#include "path.h"
#include "config/config_helpers.h"
#include "repo/repo_helpers.h"

enum repo_mode {
	STANDARD_REPOSITORY = 0,
	BARE_REPOSITORY = 1
};

static git_repository *g_repo = NULL;
static git_str g_global_path = GIT_STR_INIT;

void test_repo_init__initialize(void)
{
	g_repo = NULL;

	git_libgit2_opts(GIT_OPT_GET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL,
			 &g_global_path);
}

void test_repo_init__cleanup(void)
{
	git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL,
		g_global_path.ptr);
	git_str_dispose(&g_global_path);

	cl_fixture_cleanup("tmp_global_path");
}

static void cleanup_repository(void *path)
{
	git_repository_free(g_repo);
	g_repo = NULL;

	cl_fixture_cleanup((const char *)path);
}

static void ensure_repository_init(
	const char *working_directory,
	int is_bare,
	const char *expected_path_repository,
	const char *expected_working_directory)
{
	const char *workdir;

	cl_assert(!git_fs_path_isdir(working_directory));

	cl_git_pass(git_repository_init(&g_repo, working_directory, is_bare));

	workdir = git_repository_workdir(g_repo);
	if (workdir != NULL || expected_working_directory != NULL) {
		cl_assert(
			git__suffixcmp(workdir, expected_working_directory) == 0
		);
	}

	cl_assert(
		git__suffixcmp(git_repository_path(g_repo), expected_path_repository) == 0
	);

	cl_assert(git_repository_is_bare(g_repo) == is_bare);

#ifdef GIT_WIN32
	if (!is_bare) {
		DWORD fattrs = GetFileAttributes(git_repository_path(g_repo));
		cl_assert((fattrs & FILE_ATTRIBUTE_HIDDEN) != 0);
	}
#endif

	cl_assert(git_repository_is_empty(g_repo));
}

void test_repo_init__standard_repo(void)
{
	cl_set_cleanup(&cleanup_repository, "testrepo");
	ensure_repository_init("testrepo/", 0, "testrepo/.git/", "testrepo/");
}

void test_repo_init__standard_repo_noslash(void)
{
	cl_set_cleanup(&cleanup_repository, "testrepo");
	ensure_repository_init("testrepo", 0, "testrepo/.git/", "testrepo/");
}

void test_repo_init__bare_repo(void)
{
	cl_set_cleanup(&cleanup_repository, "testrepo.git");
	ensure_repository_init("testrepo.git/", 1, "testrepo.git/", NULL);
}

void test_repo_init__bare_repo_noslash(void)
{
	cl_set_cleanup(&cleanup_repository, "testrepo.git");
	ensure_repository_init("testrepo.git", 1, "testrepo.git/", NULL);
}

void test_repo_init__bare_repo_escaping_current_workdir(void)
{
	git_str path_repository = GIT_STR_INIT;
	git_str path_current_workdir = GIT_STR_INIT;

	cl_git_pass(git_fs_path_prettify_dir(&path_current_workdir, ".", NULL));

	cl_git_pass(git_str_joinpath(&path_repository, git_str_cstr(&path_current_workdir), "a/b/c"));
	cl_git_pass(git_futils_mkdir_r(git_str_cstr(&path_repository), GIT_DIR_MODE));

	/* Change the current working directory */
	cl_git_pass(chdir(git_str_cstr(&path_repository)));

	/* Initialize a bare repo with a relative path escaping out of the current working directory */
	cl_git_pass(git_repository_init(&g_repo, "../d/e.git", 1));
	cl_git_pass(git__suffixcmp(git_repository_path(g_repo), "/a/b/d/e.git/"));

	git_repository_free(g_repo);
	g_repo = NULL;

	/* Open a bare repo with a relative path escaping out of the current working directory */
	cl_git_pass(git_repository_open(&g_repo, "../d/e.git"));

	cl_git_pass(chdir(git_str_cstr(&path_current_workdir)));

	git_str_dispose(&path_current_workdir);
	git_str_dispose(&path_repository);

	cleanup_repository("a");
}

void test_repo_init__reinit_bare_repo(void)
{
	cl_set_cleanup(&cleanup_repository, "reinit.git");

	/* Initialize the repository */
	cl_git_pass(git_repository_init(&g_repo, "reinit.git", 1));
	git_repository_free(g_repo);
	g_repo = NULL;

	/* Reinitialize the repository */
	cl_git_pass(git_repository_init(&g_repo, "reinit.git", 1));
}

void test_repo_init__reinit_nondefault_version(void)
{
	git_config *config;

	cl_set_cleanup(&cleanup_repository, "reinit.git");

	/* Initialize the repository */
	cl_git_pass(git_repository_init(&g_repo, "reinit.git", 1));
	git_repository_config(&config, g_repo);

	/* Set the config to a supported but not default version */
	cl_repo_set_string(g_repo, "core.repositoryformatversion", "1");
	git_config_free(config);
	git_repository_free(g_repo);
	g_repo = NULL;

	/* Try to reinitialize the repository */
	cl_git_pass(git_repository_init(&g_repo, "reinit.git", 1));
	cl_assert_equal_i(1, cl_repo_get_int(g_repo, "core.repositoryformatversion"));

	cl_fixture_cleanup("reinit.git");
}

void test_repo_init__reinit_unsupported_version(void)
{
	cl_set_cleanup(&cleanup_repository, "reinit.git");

	/* Initialize the repository */
	cl_git_pass(git_repository_init(&g_repo, "reinit.git", 1));

	/*
	 * Hack the config of the repository to make it look like it has
	 * been created by a too new and unsupported version of git/libgit2
	 */
	cl_repo_set_string(g_repo, "core.repositoryformatversion", "42");
	git_repository_free(g_repo);
	g_repo = NULL;

	/* Try and fail to reinitialize the repository */
	cl_git_fail(git_repository_init(&g_repo, "reinit.git", 1));
	cl_fixture_cleanup("reinit.git");
}

void test_repo_init__additional_templates(void)
{
	git_str path = GIT_STR_INIT;

	cl_set_cleanup(&cleanup_repository, "tester");

	ensure_repository_init("tester", 0, "tester/.git/", "tester/");

	cl_git_pass(
		git_str_joinpath(&path, git_repository_path(g_repo), "description"));
	cl_assert(git_fs_path_isfile(git_str_cstr(&path)));

	cl_git_pass(
		git_str_joinpath(&path, git_repository_path(g_repo), "info/exclude"));
	cl_assert(git_fs_path_isfile(git_str_cstr(&path)));

	cl_git_pass(
		git_str_joinpath(&path, git_repository_path(g_repo), "hooks"));
	cl_assert(git_fs_path_isdir(git_str_cstr(&path)));
	/* won't confirm specific contents of hooks dir since it may vary */

	git_str_dispose(&path);
}

static void assert_config_entry_on_init_bytype(
	const char *config_key, int expected_value, bool is_bare)
{
	git_config *config;
	int error, current_value;
	const char *repo_path = is_bare ?
		"config_entry/test.bare.git" : "config_entry/test.non.bare.git";

	cl_set_cleanup(&cleanup_repository, "config_entry");

	cl_git_pass(git_repository_init(&g_repo, repo_path, is_bare));

	cl_git_pass(git_repository_config(&config, g_repo));
	error = git_config_get_bool(&current_value, config, config_key);
	git_config_free(config);

	if (expected_value >= 0) {
		cl_assert_equal_i(0, error);
		cl_assert_equal_i(expected_value, current_value);
	} else {
		cl_assert_equal_i(expected_value, error);
	}
}

static void assert_config_entry_on_init(
	const char *config_key, int expected_value)
{
	assert_config_entry_on_init_bytype(config_key, expected_value, true);
	git_repository_free(g_repo);
	g_repo = NULL;

	assert_config_entry_on_init_bytype(config_key, expected_value, false);
}

void test_repo_init__detect_filemode(void)
{
	assert_config_entry_on_init("core.filemode", cl_is_chmod_supported());
}

void test_repo_init__detect_ignorecase(void)
{
	struct stat st;
	bool found_without_match;

	cl_git_write2file("testCAPS", "whatever\n", 0, O_CREAT | O_WRONLY, 0666);
	found_without_match = (p_stat("Testcaps", &st) == 0);
	cl_must_pass(p_unlink("testCAPS"));

	assert_config_entry_on_init(
		"core.ignorecase", found_without_match ? true : GIT_ENOTFOUND);
}

/*
 * Windows: if the filesystem supports symlinks (because we're running
 * as administrator, or because the user has opted into it for normal
 * users) then we can also opt-in explicitly by settings `core.symlinks`
 * in the global config.  Symlinks remain off by default.
 */

void test_repo_init__symlinks_win32_enabled_by_global_config(void)
{
#ifndef GIT_WIN32
	cl_skip();
#else
	git_config *config, *repo_config;
	int val;

	if (!git_fs_path_supports_symlinks("link"))
		cl_skip();

	create_tmp_global_config("tmp_global_config", "core.symlinks", "true");

	/*
	 * Create a new repository (can't use `assert_config_on_init` since we
	 * want to examine configuration levels with more granularity.)
	 */
	cl_git_pass(git_repository_init(&g_repo, "config_entry/test.non.bare.git", false));

	/* Ensure that core.symlinks remains set (via the global config). */
	cl_git_pass(git_repository_config(&config, g_repo));
	cl_git_pass(git_config_get_bool(&val, config, "core.symlinks"));
	cl_assert_equal_i(1, val);

	/*
	 * Ensure that the repository config does not set core.symlinks.
	 * It should remain inherited.
	 */
	cl_git_pass(git_config_open_level(&repo_config, config, GIT_CONFIG_LEVEL_LOCAL));
	cl_git_fail_with(GIT_ENOTFOUND, git_config_get_bool(&val, repo_config, "core.symlinks"));
	git_config_free(repo_config);

	git_config_free(config);

	git_repository_free(g_repo);
	g_repo = NULL;
#endif
}

void test_repo_init__symlinks_win32_off_by_default(void)
{
#ifndef GIT_WIN32
	cl_skip();
#else
	assert_config_entry_on_init("core.symlinks", false);
#endif
}

void test_repo_init__symlinks_posix_detected(void)
{
#ifdef GIT_WIN32
	cl_skip();
#else
	assert_config_entry_on_init(
	    "core.symlinks", git_fs_path_supports_symlinks("link") ? GIT_ENOTFOUND : false);
#endif
}

void test_repo_init__detect_precompose_unicode_required(void)
{
#ifdef GIT_USE_ICONV
	char *composed = "ḱṷṓn", *decomposed = "ḱṷṓn";
	struct stat st;
	bool found_with_nfd;

	cl_git_write2file(composed, "whatever\n", 0, O_CREAT | O_WRONLY, 0666);
	found_with_nfd = (p_stat(decomposed, &st) == 0);
	cl_must_pass(p_unlink(composed));

	assert_config_entry_on_init("core.precomposeunicode", found_with_nfd);
#else
	assert_config_entry_on_init("core.precomposeunicode", GIT_ENOTFOUND);
#endif
}

void test_repo_init__reinit_doesnot_overwrite_ignorecase(void)
{
	git_config *config;
	int current_value;

	/* Init a new repo */
	cl_set_cleanup(&cleanup_repository, "not.overwrite.git");
	cl_git_pass(git_repository_init(&g_repo, "not.overwrite.git", 1));

	/* Change the "core.ignorecase" config value to something unlikely */
	git_repository_config(&config, g_repo);
	git_config_set_int32(config, "core.ignorecase", 42);
	git_config_free(config);
	git_repository_free(g_repo);
	g_repo = NULL;

	/* Reinit the repository */
	cl_git_pass(git_repository_init(&g_repo, "not.overwrite.git", 1));
	git_repository_config(&config, g_repo);

	/* Ensure the "core.ignorecase" config value hasn't been updated */
	cl_git_pass(git_config_get_int32(&current_value, config, "core.ignorecase"));
	cl_assert_equal_i(42, current_value);

	git_config_free(config);
}

void test_repo_init__reinit_overwrites_filemode(void)
{
	int expected = cl_is_chmod_supported(), current_value;

	/* Init a new repo */
	cl_set_cleanup(&cleanup_repository, "overwrite.git");
	cl_git_pass(git_repository_init(&g_repo, "overwrite.git", 1));

	/* Change the "core.filemode" config value to something unlikely */
	cl_repo_set_bool(g_repo, "core.filemode", !expected);

	git_repository_free(g_repo);
	g_repo = NULL;

	/* Reinit the repository */
	cl_git_pass(git_repository_init(&g_repo, "overwrite.git", 1));

	/* Ensure the "core.filemode" config value has been reset */
	current_value = cl_repo_get_bool(g_repo, "core.filemode");
	cl_assert_equal_i(expected, current_value);
}

void test_repo_init__sets_logAllRefUpdates_according_to_type_of_repository(void)
{
	assert_config_entry_on_init_bytype("core.logallrefupdates", GIT_ENOTFOUND, true);
	git_repository_free(g_repo);
	assert_config_entry_on_init_bytype("core.logallrefupdates", true, false);
}

void test_repo_init__extended_0(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;

	/* without MKDIR this should fail */
	cl_git_fail(git_repository_init_ext(&g_repo, "extended", &opts));

	/* make the directory first, then it should succeed */
	cl_git_pass(git_futils_mkdir("extended", 0775, 0));
	cl_git_pass(git_repository_init_ext(&g_repo, "extended", &opts));

	cl_assert(!git__suffixcmp(git_repository_workdir(g_repo), "/extended/"));
	cl_assert(!git__suffixcmp(git_repository_path(g_repo), "/extended/.git/"));
	cl_assert(!git_repository_is_bare(g_repo));
	cl_assert(git_repository_is_empty(g_repo));

	cleanup_repository("extended");
}

void test_repo_init__extended_1(void)
{
	git_reference *ref;
	git_remote *remote;
	struct stat st;
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;

	opts.flags = GIT_REPOSITORY_INIT_MKPATH |
		GIT_REPOSITORY_INIT_NO_DOTGIT_DIR;
	opts.mode = GIT_REPOSITORY_INIT_SHARED_GROUP;
	opts.workdir_path = "../c_wd";
	opts.description = "Awesomest test repository evah";
	opts.initial_head = "development";
	opts.origin_url = "https://github.com/libgit2/libgit2.git";

	cl_git_pass(git_repository_init_ext(&g_repo, "root/b/c.git", &opts));

	cl_assert(!git__suffixcmp(git_repository_workdir(g_repo), "/c_wd/"));
	cl_assert(!git__suffixcmp(git_repository_path(g_repo), "/c.git/"));
	cl_assert(git_fs_path_isfile("root/b/c_wd/.git"));
	cl_assert(!git_repository_is_bare(g_repo));
	/* repo will not be counted as empty because we set head to "development" */
	cl_assert(!git_repository_is_empty(g_repo));

	cl_git_pass(git_fs_path_lstat(git_repository_path(g_repo), &st));
	cl_assert(S_ISDIR(st.st_mode));
	if (cl_is_chmod_supported())
		cl_assert((S_ISGID & st.st_mode) == S_ISGID);
	else
		cl_assert((S_ISGID & st.st_mode) == 0);

	cl_git_pass(git_reference_lookup(&ref, g_repo, "HEAD"));
	cl_assert(git_reference_type(ref) == GIT_REFERENCE_SYMBOLIC);
	cl_assert_equal_s("refs/heads/development", git_reference_symbolic_target(ref));
	git_reference_free(ref);

	cl_git_pass(git_remote_lookup(&remote, g_repo, "origin"));
	cl_assert_equal_s("origin", git_remote_name(remote));
	cl_assert_equal_s(opts.origin_url, git_remote_url(remote));
	git_remote_free(remote);

	git_repository_free(g_repo);
	cl_fixture_cleanup("root");
}

void test_repo_init__relative_gitdir(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;
	git_str dot_git_content = GIT_STR_INIT;

	opts.workdir_path = "../c_wd";
	opts.flags =
		GIT_REPOSITORY_INIT_MKPATH |
		GIT_REPOSITORY_INIT_RELATIVE_GITLINK |
		GIT_REPOSITORY_INIT_NO_DOTGIT_DIR;

	/* make the directory first, then it should succeed */
	cl_git_pass(git_repository_init_ext(&g_repo, "root/b/my_repository", &opts));

	cl_assert(!git__suffixcmp(git_repository_workdir(g_repo), "root/b/c_wd/"));
	cl_assert(!git__suffixcmp(git_repository_path(g_repo), "root/b/my_repository/"));
	cl_assert(!git_repository_is_bare(g_repo));
	cl_assert(git_repository_is_empty(g_repo));

	/* Verify that the gitlink and worktree entries are relative */

	/* Verify worktree */
	assert_config_entry_value(g_repo, "core.worktree", "../c_wd/");

	/* Verify gitlink */
	cl_git_pass(git_futils_readbuffer(&dot_git_content, "root/b/c_wd/.git"));
	cl_assert_equal_s("gitdir: ../my_repository/", dot_git_content.ptr);

	git_str_dispose(&dot_git_content);
	cleanup_repository("root");
}

void test_repo_init__relative_gitdir_2(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;
	git_str dot_git_content = GIT_STR_INIT;
	git_str full_path = GIT_STR_INIT;

	cl_git_pass(git_fs_path_prettify(&full_path, ".", NULL));
	cl_git_pass(git_str_joinpath(&full_path, full_path.ptr, "root/b/c_wd"));

	opts.workdir_path = full_path.ptr;
	opts.flags =
		GIT_REPOSITORY_INIT_MKPATH |
		GIT_REPOSITORY_INIT_RELATIVE_GITLINK |
		GIT_REPOSITORY_INIT_NO_DOTGIT_DIR;

	/* make the directory first, then it should succeed */
	cl_git_pass(git_repository_init_ext(&g_repo, "root/b/my_repository", &opts));
	git_str_dispose(&full_path);

	cl_assert(!git__suffixcmp(git_repository_workdir(g_repo), "root/b/c_wd/"));
	cl_assert(!git__suffixcmp(git_repository_path(g_repo), "root/b/my_repository/"));
	cl_assert(!git_repository_is_bare(g_repo));
	cl_assert(git_repository_is_empty(g_repo));

	/* Verify that the gitlink and worktree entries are relative */

	/* Verify worktree */
	assert_config_entry_value(g_repo, "core.worktree", "../c_wd/");

	/* Verify gitlink */
	cl_git_pass(git_futils_readbuffer(&dot_git_content, "root/b/c_wd/.git"));
	cl_assert_equal_s("gitdir: ../my_repository/", dot_git_content.ptr);

	git_str_dispose(&dot_git_content);
	cleanup_repository("root");
}

void test_repo_init__can_reinit_an_initialized_repository(void)
{
	git_repository *reinit;

	cl_set_cleanup(&cleanup_repository, "extended");

	cl_git_pass(git_futils_mkdir("extended", 0775, 0));
	cl_git_pass(git_repository_init(&g_repo, "extended", false));

	cl_git_pass(git_repository_init(&reinit, "extended", false));

	cl_assert_equal_s(git_repository_path(g_repo), git_repository_path(reinit));

	git_repository_free(reinit);
}

void test_repo_init__init_with_initial_commit(void)
{
	git_index *index;

	cl_set_cleanup(&cleanup_repository, "committed");

	/* Initialize the repository */
	cl_git_pass(git_repository_init(&g_repo, "committed", 0));

	/* Index will be automatically created when requested for a new repo */
	cl_git_pass(git_repository_index(&index, g_repo));

	/* Create a file so we can commit it
	 *
	 * If you are writing code outside the test suite, you can create this
	 * file any way that you like, such as:
	 *      FILE *fp = fopen("committed/file.txt", "w");
	 *      fputs("some stuff\n", fp);
	 *      fclose(fp);
	 * We like to use the help functions because they do error detection
	 * in a way that's easily compatible with our test suite.
	 */
	cl_git_mkfile("committed/file.txt", "some stuff\n");

	/* Add file to the index */
	cl_git_pass(git_index_add_bypath(index, "file.txt"));
	cl_git_pass(git_index_write(index));

	/* Intentionally not using cl_repo_commit_from_index here so this code
	 * can be used as an example of how an initial commit is typically
	 * made to a repository...
	 */

	/* Make sure we're ready to use git_signature_default :-) */
	{
		git_config *cfg, *local;
		cl_git_pass(git_repository_config(&cfg, g_repo));
		cl_git_pass(git_config_open_level(&local, cfg, GIT_CONFIG_LEVEL_LOCAL));
		cl_git_pass(git_config_set_string(local, "user.name", "Test User"));
		cl_git_pass(git_config_set_string(local, "user.email", "t@example.com"));
		git_config_free(local);
		git_config_free(cfg);
	}

	/* Create a commit with the new contents of the index */
	{
		git_signature *sig;
		git_oid tree_id, commit_id;
		git_tree *tree;

		cl_git_pass(git_signature_default(&sig, g_repo));
		cl_git_pass(git_index_write_tree(&tree_id, index));
		cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

		cl_git_pass(git_commit_create_v(
			&commit_id, g_repo, "HEAD", sig, sig,
			NULL, "First", tree, 0));

		git_tree_free(tree);
		git_signature_free(sig);
	}

	git_index_free(index);
}

void test_repo_init__at_filesystem_root(void)
{
	git_repository *repo;
	const char *sandbox = clar_sandbox_path();
	git_str root = GIT_STR_INIT;
	int root_len;

	if (!cl_is_env_set("GITTEST_INVASIVE_FS_STRUCTURE"))
		cl_skip();

	root_len = git_fs_path_root(sandbox);
	cl_assert(root_len >= 0);

	git_str_put(&root, sandbox, root_len+1);
	git_str_joinpath(&root, root.ptr, "libgit2_test_dir");

	cl_assert(!git_fs_path_exists(root.ptr));

	cl_git_pass(git_repository_init(&repo, root.ptr, 0));
	cl_assert(git_fs_path_isdir(root.ptr));
	cl_git_pass(git_futils_rmdir_r(root.ptr, NULL, GIT_RMDIR_REMOVE_FILES));

	git_str_dispose(&root);
	git_repository_free(repo);
}

void test_repo_init__nonexisting_directory(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;
	git_repository *repo;

	/*
	 * If creating a repo with non-existing parent directories, then libgit2
	 * will by default create the complete directory hierarchy if using
	 * `git_repository_init`. Thus, let's use the extended version and not
	 * set the `GIT_REPOSITORY_INIT_MKPATH` flag.
	 */
	cl_git_fail(git_repository_init_ext(&repo, "nonexisting/path", &opts));
}

void test_repo_init__nonexisting_root(void)
{
#ifdef GIT_WIN32
	git_repository *repo;

	/*
	 * This really only depends on the nonexistence of the Q: drive. We
	 * cannot implement the equivalent test on Unix systems, as there is
	 * fundamentally no path that is disconnected from the root directory.
	 */
	cl_git_fail(git_repository_init(&repo, "Q:/non/existent/path", 0));
	cl_git_fail(git_repository_init(&repo, "Q:\\non\\existent\\path", 0));
#else
	clar__skip();
#endif
}

void test_repo_init__unwriteable_directory(void)
{
#ifndef GIT_WIN32
	git_repository *repo;

	if (geteuid() == 0)
		clar__skip();

	/*
	 * Create a non-writeable directory so that we cannot create directories
	 * inside of it. The root user has CAP_DAC_OVERRIDE, so he doesn't care
	 * for the directory permissions and thus we need to skip the test if
	 * run as root user.
	 */
	cl_must_pass(p_mkdir("unwriteable", 0444));
	cl_git_fail(git_repository_init(&repo, "unwriteable/repo", 0));
	cl_must_pass(p_rmdir("unwriteable"));
#else
	clar__skip();
#endif
}

void test_repo_init__defaultbranch_config(void)
{
	git_reference *head;

	cl_set_cleanup(&cleanup_repository, "repo");

	create_tmp_global_config("tmp_global_path", "init.defaultbranch", "my_default_branch");

	cl_git_pass(git_repository_init(&g_repo, "repo", 0));
	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));

	cl_assert_equal_s("refs/heads/my_default_branch", git_reference_symbolic_target(head));

	git_reference_free(head);
}

void test_repo_init__defaultbranch_config_empty(void)
{
	git_reference *head;

	cl_set_cleanup(&cleanup_repository, "repo");

	create_tmp_global_config("tmp_global_path", "init.defaultbranch", "");

	cl_git_pass(git_repository_init(&g_repo, "repo", 0));
	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));

	cl_assert_equal_s("refs/heads/master", git_reference_symbolic_target(head));

	git_reference_free(head);
}

void test_repo_init__longpath(void)
{
#ifdef GIT_WIN32
	size_t padding = CONST_STRLEN("objects/pack/pack-.pack.lock") + GIT_OID_MAX_HEXSIZE;
	size_t max, i;
	git_str path = GIT_STR_INIT;
	git_repository *one = NULL, *two = NULL;

	/*
	 * Files within repositories need to fit within MAX_PATH;
	 * that means a repo path must be at most (MAX_PATH - 18).
	 */
	cl_git_pass(git_str_puts(&path, clar_sandbox_path()));
	cl_git_pass(git_str_putc(&path, '/'));

	max = ((MAX_PATH) - path.size) - padding;

	for (i = 0; i < max - 1; i++)
		cl_git_pass(git_str_putc(&path, 'a'));

	cl_git_pass(git_repository_init(&one, path.ptr, 1));

	/* Paths longer than this are rejected */
	cl_git_pass(git_str_putc(&path, 'z'));
	cl_git_fail(git_repository_init(&two, path.ptr, 1));

	git_repository_free(one);
	git_repository_free(two);
	git_str_dispose(&path);
#endif
}
