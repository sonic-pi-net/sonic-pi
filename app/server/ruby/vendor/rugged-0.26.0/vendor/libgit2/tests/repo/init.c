#include "clar_libgit2.h"
#include "fileops.h"
#include "repository.h"
#include "config.h"
#include "path.h"
#include "config/config_helpers.h"

enum repo_mode {
	STANDARD_REPOSITORY = 0,
	BARE_REPOSITORY = 1
};

static git_repository *_repo = NULL;
static git_buf _global_path = GIT_BUF_INIT;
static git_buf _tmp_path = GIT_BUF_INIT;
static mode_t g_umask = 0;

void test_repo_init__initialize(void)
{
	_repo = NULL;

	/* load umask if not already loaded */
	if (!g_umask) {
		g_umask = p_umask(022);
		(void)p_umask(g_umask);
	}

    git_libgit2_opts(GIT_OPT_GET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL,
		&_global_path);
}

void test_repo_init__cleanup(void)
{
	git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL,
		_global_path.ptr);
	git_buf_free(&_global_path);

	if (_tmp_path.size > 0 && git_path_isdir(_tmp_path.ptr))
		git_futils_rmdir_r(_tmp_path.ptr, NULL, GIT_RMDIR_REMOVE_FILES);
	git_buf_free(&_tmp_path);
}

static void cleanup_repository(void *path)
{
	git_repository_free(_repo);
	_repo = NULL;

	cl_fixture_cleanup((const char *)path);
}

static void ensure_repository_init(
	const char *working_directory,
	int is_bare,
	const char *expected_path_repository,
	const char *expected_working_directory)
{
	const char *workdir;

	cl_assert(!git_path_isdir(working_directory));

	cl_git_pass(git_repository_init(&_repo, working_directory, is_bare));

	workdir = git_repository_workdir(_repo);
	if (workdir != NULL || expected_working_directory != NULL) {
		cl_assert(
			git__suffixcmp(workdir, expected_working_directory) == 0
		);
	}

	cl_assert(
		git__suffixcmp(git_repository_path(_repo), expected_path_repository) == 0
	);

	cl_assert(git_repository_is_bare(_repo) == is_bare);

#ifdef GIT_WIN32
	if (!is_bare) {
		DWORD fattrs = GetFileAttributes(git_repository_path(_repo));
		cl_assert((fattrs & FILE_ATTRIBUTE_HIDDEN) != 0);
	}
#endif

	cl_assert(git_repository_is_empty(_repo));
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
	git_buf path_repository = GIT_BUF_INIT;
	git_buf path_current_workdir = GIT_BUF_INIT;

	cl_git_pass(git_path_prettify_dir(&path_current_workdir, ".", NULL));

	cl_git_pass(git_buf_joinpath(&path_repository, git_buf_cstr(&path_current_workdir), "a/b/c"));
	cl_git_pass(git_futils_mkdir_r(git_buf_cstr(&path_repository), GIT_DIR_MODE));

	/* Change the current working directory */
	cl_git_pass(chdir(git_buf_cstr(&path_repository)));

	/* Initialize a bare repo with a relative path escaping out of the current working directory */
	cl_git_pass(git_repository_init(&_repo, "../d/e.git", 1));
	cl_git_pass(git__suffixcmp(git_repository_path(_repo), "/a/b/d/e.git/"));

	git_repository_free(_repo);

	/* Open a bare repo with a relative path escaping out of the current working directory */
	cl_git_pass(git_repository_open(&_repo, "../d/e.git"));

	cl_git_pass(chdir(git_buf_cstr(&path_current_workdir)));

	git_buf_free(&path_current_workdir);
	git_buf_free(&path_repository);

	cleanup_repository("a");
}

void test_repo_init__reinit_bare_repo(void)
{
	cl_set_cleanup(&cleanup_repository, "reinit.git");

	/* Initialize the repository */
	cl_git_pass(git_repository_init(&_repo, "reinit.git", 1));
	git_repository_free(_repo);

	/* Reinitialize the repository */
	cl_git_pass(git_repository_init(&_repo, "reinit.git", 1));
}

void test_repo_init__reinit_too_recent_bare_repo(void)
{
	git_config *config;

	/* Initialize the repository */
	cl_git_pass(git_repository_init(&_repo, "reinit.git", 1));
	git_repository_config(&config, _repo);

	/*
	 * Hack the config of the repository to make it look like it has
	 * been created by a recenter version of git/libgit2
	 */
	cl_git_pass(git_config_set_int32(config, "core.repositoryformatversion", 42));

	git_config_free(config);
	git_repository_free(_repo);

	/* Try to reinitialize the repository */
	cl_git_fail(git_repository_init(&_repo, "reinit.git", 1));

	cl_fixture_cleanup("reinit.git");
}

void test_repo_init__additional_templates(void)
{
	git_buf path = GIT_BUF_INIT;

	cl_set_cleanup(&cleanup_repository, "tester");

	ensure_repository_init("tester", 0, "tester/.git/", "tester/");

	cl_git_pass(
		git_buf_joinpath(&path, git_repository_path(_repo), "description"));
	cl_assert(git_path_isfile(git_buf_cstr(&path)));

	cl_git_pass(
		git_buf_joinpath(&path, git_repository_path(_repo), "info/exclude"));
	cl_assert(git_path_isfile(git_buf_cstr(&path)));

	cl_git_pass(
		git_buf_joinpath(&path, git_repository_path(_repo), "hooks"));
	cl_assert(git_path_isdir(git_buf_cstr(&path)));
	/* won't confirm specific contents of hooks dir since it may vary */

	git_buf_free(&path);
}

static void assert_config_entry_on_init_bytype(
	const char *config_key, int expected_value, bool is_bare)
{
	git_config *config;
	int error, current_value;
	const char *repo_path = is_bare ?
		"config_entry/test.bare.git" : "config_entry/test.non.bare.git";

	cl_set_cleanup(&cleanup_repository, "config_entry");

	cl_git_pass(git_repository_init(&_repo, repo_path, is_bare));

	cl_git_pass(git_repository_config(&config, _repo));
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
	git_repository_free(_repo);

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
	cl_git_pass(git_repository_init(&_repo, "not.overwrite.git", 1));

	/* Change the "core.ignorecase" config value to something unlikely */
	git_repository_config(&config, _repo);
	git_config_set_int32(config, "core.ignorecase", 42);
	git_config_free(config);
	git_repository_free(_repo);
	_repo = NULL;

	/* Reinit the repository */
	cl_git_pass(git_repository_init(&_repo, "not.overwrite.git", 1));
	git_repository_config(&config, _repo);

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
	cl_git_pass(git_repository_init(&_repo, "overwrite.git", 1));

	/* Change the "core.filemode" config value to something unlikely */
	cl_repo_set_bool(_repo, "core.filemode", !expected);

	git_repository_free(_repo);
	_repo = NULL;

	/* Reinit the repository */
	cl_git_pass(git_repository_init(&_repo, "overwrite.git", 1));

	/* Ensure the "core.filemode" config value has been reset */
	current_value = cl_repo_get_bool(_repo, "core.filemode");
	cl_assert_equal_i(expected, current_value);
}

void test_repo_init__sets_logAllRefUpdates_according_to_type_of_repository(void)
{
	assert_config_entry_on_init_bytype("core.logallrefupdates", GIT_ENOTFOUND, true);
	git_repository_free(_repo);
	assert_config_entry_on_init_bytype("core.logallrefupdates", true, false);
}

void test_repo_init__empty_template_path(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;
	opts.template_path = "";

	cl_git_pass(git_futils_mkdir("foo", 0755, 0));
	cl_git_pass(git_repository_init_ext(&_repo, "foo", &opts));

	cleanup_repository("foo");
}

void test_repo_init__extended_0(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;

	/* without MKDIR this should fail */
	cl_git_fail(git_repository_init_ext(&_repo, "extended", &opts));

	/* make the directory first, then it should succeed */
	cl_git_pass(git_futils_mkdir("extended", 0775, 0));
	cl_git_pass(git_repository_init_ext(&_repo, "extended", &opts));

	cl_assert(!git__suffixcmp(git_repository_workdir(_repo), "/extended/"));
	cl_assert(!git__suffixcmp(git_repository_path(_repo), "/extended/.git/"));
	cl_assert(!git_repository_is_bare(_repo));
	cl_assert(git_repository_is_empty(_repo));

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

	cl_git_pass(git_repository_init_ext(&_repo, "root/b/c.git", &opts));

	cl_assert(!git__suffixcmp(git_repository_workdir(_repo), "/c_wd/"));
	cl_assert(!git__suffixcmp(git_repository_path(_repo), "/c.git/"));
	cl_assert(git_path_isfile("root/b/c_wd/.git"));
	cl_assert(!git_repository_is_bare(_repo));
	/* repo will not be counted as empty because we set head to "development" */
	cl_assert(!git_repository_is_empty(_repo));

	cl_git_pass(git_path_lstat(git_repository_path(_repo), &st));
	cl_assert(S_ISDIR(st.st_mode));
	if (cl_is_chmod_supported())
		cl_assert((S_ISGID & st.st_mode) == S_ISGID);
	else
		cl_assert((S_ISGID & st.st_mode) == 0);

	cl_git_pass(git_reference_lookup(&ref, _repo, "HEAD"));
	cl_assert(git_reference_type(ref) == GIT_REF_SYMBOLIC);
	cl_assert_equal_s("refs/heads/development", git_reference_symbolic_target(ref));
	git_reference_free(ref);

	cl_git_pass(git_remote_lookup(&remote, _repo, "origin"));
	cl_assert_equal_s("origin", git_remote_name(remote));
	cl_assert_equal_s(opts.origin_url, git_remote_url(remote));
	git_remote_free(remote);

	git_repository_free(_repo);
	cl_fixture_cleanup("root");
}

void test_repo_init__relative_gitdir(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;
	git_buf dot_git_content = GIT_BUF_INIT;

	opts.workdir_path = "../c_wd";
	opts.flags =
		GIT_REPOSITORY_INIT_MKPATH |
		GIT_REPOSITORY_INIT_RELATIVE_GITLINK |
		GIT_REPOSITORY_INIT_NO_DOTGIT_DIR;

	/* make the directory first, then it should succeed */
	cl_git_pass(git_repository_init_ext(&_repo, "root/b/my_repository", &opts));

	cl_assert(!git__suffixcmp(git_repository_workdir(_repo), "root/b/c_wd/"));
	cl_assert(!git__suffixcmp(git_repository_path(_repo), "root/b/my_repository/"));
	cl_assert(!git_repository_is_bare(_repo));
	cl_assert(git_repository_is_empty(_repo));

	/* Verify that the gitlink and worktree entries are relative */

	/* Verify worktree */
	assert_config_entry_value(_repo, "core.worktree", "../c_wd/");

	/* Verify gitlink */
	cl_git_pass(git_futils_readbuffer(&dot_git_content, "root/b/c_wd/.git"));
	cl_assert_equal_s("gitdir: ../my_repository/", dot_git_content.ptr);

	git_buf_free(&dot_git_content);
	cleanup_repository("root");
}

void test_repo_init__relative_gitdir_2(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;
	git_buf dot_git_content = GIT_BUF_INIT;
	git_buf full_path = GIT_BUF_INIT;

	cl_git_pass(git_path_prettify(&full_path, ".", NULL));
	cl_git_pass(git_buf_joinpath(&full_path, full_path.ptr, "root/b/c_wd"));

	opts.workdir_path = full_path.ptr;
	opts.flags =
		GIT_REPOSITORY_INIT_MKPATH |
		GIT_REPOSITORY_INIT_RELATIVE_GITLINK |
		GIT_REPOSITORY_INIT_NO_DOTGIT_DIR;

	/* make the directory first, then it should succeed */
	cl_git_pass(git_repository_init_ext(&_repo, "root/b/my_repository", &opts));
	git_buf_free(&full_path);

	cl_assert(!git__suffixcmp(git_repository_workdir(_repo), "root/b/c_wd/"));
	cl_assert(!git__suffixcmp(git_repository_path(_repo), "root/b/my_repository/"));
	cl_assert(!git_repository_is_bare(_repo));
	cl_assert(git_repository_is_empty(_repo));

	/* Verify that the gitlink and worktree entries are relative */

	/* Verify worktree */
	assert_config_entry_value(_repo, "core.worktree", "../c_wd/");

	/* Verify gitlink */
	cl_git_pass(git_futils_readbuffer(&dot_git_content, "root/b/c_wd/.git"));
	cl_assert_equal_s("gitdir: ../my_repository/", dot_git_content.ptr);

	git_buf_free(&dot_git_content);
	cleanup_repository("root");
}

#define CLEAR_FOR_CORE_FILEMODE(M) ((M) &= ~0177)

static void assert_hooks_match(
	const char *template_dir,
	const char *repo_dir,
	const char *hook_path,
	bool core_filemode)
{
	git_buf expected = GIT_BUF_INIT;
	git_buf actual = GIT_BUF_INIT;
	struct stat expected_st, st;

	cl_git_pass(git_buf_joinpath(&expected, template_dir, hook_path));
	cl_git_pass(git_path_lstat(expected.ptr, &expected_st));

	cl_git_pass(git_buf_joinpath(&actual, repo_dir, hook_path));
	cl_git_pass(git_path_lstat(actual.ptr, &st));

	cl_assert(expected_st.st_size == st.st_size);

	if (GIT_MODE_TYPE(expected_st.st_mode) != GIT_FILEMODE_LINK) {
		mode_t expected_mode =
			GIT_MODE_TYPE(expected_st.st_mode) |
			(GIT_PERMS_FOR_WRITE(expected_st.st_mode) & ~g_umask);

		if (!core_filemode) {
			CLEAR_FOR_CORE_FILEMODE(expected_mode);
			CLEAR_FOR_CORE_FILEMODE(st.st_mode);
		}

		cl_assert_equal_i_fmt(expected_mode, st.st_mode, "%07o");
	}

	git_buf_free(&expected);
	git_buf_free(&actual);
}

static void assert_mode_seems_okay(
	const char *base, const char *path,
	git_filemode_t expect_mode, bool expect_setgid, bool core_filemode)
{
	git_buf full = GIT_BUF_INIT;
	struct stat st;

	cl_git_pass(git_buf_joinpath(&full, base, path));
	cl_git_pass(git_path_lstat(full.ptr, &st));
	git_buf_free(&full);

	if (!core_filemode) {
		CLEAR_FOR_CORE_FILEMODE(expect_mode);
		CLEAR_FOR_CORE_FILEMODE(st.st_mode);
		expect_setgid = false;
	}

	if (S_ISGID != 0)
		cl_assert_equal_b(expect_setgid, (st.st_mode & S_ISGID) != 0);

	cl_assert_equal_b(
		GIT_PERMS_IS_EXEC(expect_mode), GIT_PERMS_IS_EXEC(st.st_mode));

	cl_assert_equal_i_fmt(
		GIT_MODE_TYPE(expect_mode), GIT_MODE_TYPE(st.st_mode), "%07o");
}

static const char *template_sandbox(const char *name)
{
	git_buf hooks_path = GIT_BUF_INIT, link_path = GIT_BUF_INIT,
		dotfile_path = GIT_BUF_INIT;
	const char *path = cl_fixture(name);

	cl_fixture_sandbox(name);

	/* create a symlink from link.sample to update.sample if the filesystem
	 * supports it.
	 */

	cl_git_pass(git_buf_joinpath(&hooks_path, name, "hooks"));
	cl_git_pass(git_buf_joinpath(&link_path, hooks_path.ptr, "link.sample"));

#ifdef GIT_WIN32
	cl_git_mkfile(link_path.ptr, "#!/bin/sh\necho hello, world\n");
#else
	cl_must_pass(symlink("update.sample", link_path.ptr));
#endif

	/* create a file starting with a dot */
	cl_git_pass(git_buf_joinpath(&dotfile_path, hooks_path.ptr, ".dotfile"));
	cl_git_mkfile(dotfile_path.ptr, "something\n");
	git_buf_free(&dotfile_path);

	git_buf_free(&dotfile_path);
	git_buf_free(&link_path);
	git_buf_free(&hooks_path);

	return path;
}

static void configure_templatedir(const char *template_path)
{
	git_buf config_path = GIT_BUF_INIT;
	git_buf config_data = GIT_BUF_INIT;

    cl_git_pass(git_libgit2_opts(GIT_OPT_GET_SEARCH_PATH,
		GIT_CONFIG_LEVEL_GLOBAL, &_tmp_path));
	cl_git_pass(git_buf_puts(&_tmp_path, ".tmp"));
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH,
		GIT_CONFIG_LEVEL_GLOBAL, _tmp_path.ptr));

	cl_must_pass(p_mkdir(_tmp_path.ptr, 0777));

	cl_git_pass(git_buf_joinpath(&config_path, _tmp_path.ptr, ".gitconfig"));

	cl_git_pass(git_buf_printf(&config_data,
		"[init]\n\ttemplatedir = \"%s\"\n", template_path));

	cl_git_mkfile(config_path.ptr, config_data.ptr);

	git_buf_free(&config_path);
	git_buf_free(&config_data);
}

static void validate_templates(git_repository *repo, const char *template_path)
{
	git_buf template_description = GIT_BUF_INIT;
	git_buf repo_description = GIT_BUF_INIT;
	git_buf expected = GIT_BUF_INIT;
	git_buf actual = GIT_BUF_INIT;
	int filemode;

	cl_git_pass(git_buf_joinpath(&template_description, template_path,
		"description"));
	cl_git_pass(git_buf_joinpath(&repo_description, git_repository_path(repo),
		"description"));

	cl_git_pass(git_futils_readbuffer(&expected, template_description.ptr));
	cl_git_pass(git_futils_readbuffer(&actual, repo_description.ptr));

	cl_assert_equal_s(expected.ptr, actual.ptr);

	filemode = cl_repo_get_bool(repo, "core.filemode");

	assert_hooks_match(
		template_path, git_repository_path(repo),
		"hooks/update.sample", filemode);

	assert_hooks_match(
		template_path, git_repository_path(repo),
		"hooks/link.sample", filemode);

	assert_hooks_match(
		template_path, git_repository_path(repo),
		"hooks/.dotfile", filemode);

	git_buf_free(&expected);
	git_buf_free(&actual);
	git_buf_free(&repo_description);
	git_buf_free(&template_description);
}

void test_repo_init__external_templates_specified_in_options(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;

	cl_set_cleanup(&cleanup_repository, "templated.git");
	template_sandbox("template");

	opts.flags = GIT_REPOSITORY_INIT_MKPATH | GIT_REPOSITORY_INIT_BARE |
		GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;
	opts.template_path = "template";

	cl_git_pass(git_repository_init_ext(&_repo, "templated.git", &opts));

	cl_assert(git_repository_is_bare(_repo));

	cl_assert(!git__suffixcmp(git_repository_path(_repo), "/templated.git/"));

	validate_templates(_repo, "template");
	cl_fixture_cleanup("template");
}

void test_repo_init__external_templates_specified_in_config(void)
{
	git_buf template_path = GIT_BUF_INIT;

	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;

	cl_set_cleanup(&cleanup_repository, "templated.git");
	template_sandbox("template");

	cl_git_pass(git_buf_joinpath(&template_path, clar_sandbox_path(),
		"template"));

	configure_templatedir(template_path.ptr);

	opts.flags = GIT_REPOSITORY_INIT_MKPATH | GIT_REPOSITORY_INIT_BARE |
		GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;

	cl_git_pass(git_repository_init_ext(&_repo, "templated.git", &opts));

	validate_templates(_repo, "template");
	cl_fixture_cleanup("template");

	git_buf_free(&template_path);
}

void test_repo_init__external_templates_with_leading_dot(void)
{
	git_buf template_path = GIT_BUF_INIT;

	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;

	cl_set_cleanup(&cleanup_repository, "templated.git");
	template_sandbox("template");

	cl_must_pass(p_rename("template", ".template_with_leading_dot"));

	cl_git_pass(git_buf_joinpath(&template_path, clar_sandbox_path(),
		".template_with_leading_dot"));

	configure_templatedir(template_path.ptr);

	opts.flags = GIT_REPOSITORY_INIT_MKPATH | GIT_REPOSITORY_INIT_BARE |
		GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;

	cl_git_pass(git_repository_init_ext(&_repo, "templated.git", &opts));

	validate_templates(_repo, ".template_with_leading_dot");
	cl_fixture_cleanup(".template_with_leading_dot");

	git_buf_free(&template_path);
}

void test_repo_init__extended_with_template_and_shared_mode(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;
	int filemode = true;
	const char *repo_path = NULL;

	cl_set_cleanup(&cleanup_repository, "init_shared_from_tpl");
	template_sandbox("template");

	opts.flags = GIT_REPOSITORY_INIT_MKPATH |
		GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;
	opts.template_path = "template";
	opts.mode = GIT_REPOSITORY_INIT_SHARED_GROUP;

	cl_git_pass(git_repository_init_ext(&_repo, "init_shared_from_tpl", &opts));

	cl_assert(!git_repository_is_bare(_repo));
	cl_assert(!git__suffixcmp(git_repository_path(_repo), "/init_shared_from_tpl/.git/"));

	filemode = cl_repo_get_bool(_repo, "core.filemode");

	repo_path = git_repository_path(_repo);
	assert_mode_seems_okay(repo_path, "hooks",
		GIT_FILEMODE_TREE | GIT_REPOSITORY_INIT_SHARED_GROUP, true, filemode);
	assert_mode_seems_okay(repo_path, "info",
		GIT_FILEMODE_TREE | GIT_REPOSITORY_INIT_SHARED_GROUP, true, filemode);
	assert_mode_seems_okay(repo_path, "description",
		GIT_FILEMODE_BLOB, false, filemode);

	validate_templates(_repo, "template");

	cl_fixture_cleanup("template");
}

void test_repo_init__can_reinit_an_initialized_repository(void)
{
	git_repository *reinit;

	cl_set_cleanup(&cleanup_repository, "extended");

	cl_git_pass(git_futils_mkdir("extended", 0775, 0));
	cl_git_pass(git_repository_init(&_repo, "extended", false));

	cl_git_pass(git_repository_init(&reinit, "extended", false));

	cl_assert_equal_s(git_repository_path(_repo), git_repository_path(reinit));

	git_repository_free(reinit);
}

void test_repo_init__init_with_initial_commit(void)
{
	git_index *index;

	cl_set_cleanup(&cleanup_repository, "committed");

	/* Initialize the repository */
	cl_git_pass(git_repository_init(&_repo, "committed", 0));

	/* Init will be automatically created when requested for a new repo */
	cl_git_pass(git_repository_index(&index, _repo));

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
		cl_git_pass(git_repository_config(&cfg, _repo));
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

		cl_git_pass(git_signature_default(&sig, _repo));
		cl_git_pass(git_index_write_tree(&tree_id, index));
		cl_git_pass(git_tree_lookup(&tree, _repo, &tree_id));

		cl_git_pass(git_commit_create_v(
			&commit_id, _repo, "HEAD", sig, sig,
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
	git_buf root = GIT_BUF_INIT;
	int root_len;

	if (!cl_is_env_set("GITTEST_INVASIVE_FS_STRUCTURE"))
		cl_skip();

	root_len = git_path_root(sandbox);
	cl_assert(root_len >= 0);

	git_buf_put(&root, sandbox, root_len+1);
	git_buf_joinpath(&root, root.ptr, "libgit2_test_dir");

	cl_assert(!git_path_exists(root.ptr));

	cl_git_pass(git_repository_init(&repo, root.ptr, 0));
	cl_assert(git_path_isdir(root.ptr));
	cl_git_pass(git_futils_rmdir_r(root.ptr, NULL, GIT_RMDIR_REMOVE_FILES));

	git_buf_free(&root);
	git_repository_free(repo);
}
