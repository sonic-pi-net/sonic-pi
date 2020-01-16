#include "clar_libgit2.h"

#include "futils.h"
#include "repo/repo_helpers.h"

#define CLEAR_FOR_CORE_FILEMODE(M) ((M) &= ~0177)

static git_repository *_repo = NULL;
static mode_t g_umask = 0;
static git_buf _global_path = GIT_BUF_INIT;

static const char *fixture_repo;
static const char *fixture_templates;

void test_repo_template__initialize(void)
{
	_repo = NULL;

	/* load umask if not already loaded */
	if (!g_umask) {
		g_umask = p_umask(022);
		(void)p_umask(g_umask);
	}
}

void test_repo_template__cleanup(void)
{
	git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL,
		_global_path.ptr);
	git_buf_dispose(&_global_path);

	cl_fixture_cleanup("tmp_global_path");

	if (fixture_repo) {
		cl_fixture_cleanup(fixture_repo);
		fixture_repo = NULL;
	}

	if (fixture_templates) {
		cl_fixture_cleanup(fixture_templates);
		fixture_templates = NULL;
	}

	git_repository_free(_repo);
	_repo = NULL;
}

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

	git_buf_dispose(&expected);
	git_buf_dispose(&actual);
}

static void assert_mode_seems_okay(
	const char *base, const char *path,
	git_filemode_t expect_mode, bool expect_setgid, bool core_filemode)
{
	git_buf full = GIT_BUF_INIT;
	struct stat st;

	cl_git_pass(git_buf_joinpath(&full, base, path));
	cl_git_pass(git_path_lstat(full.ptr, &st));
	git_buf_dispose(&full);

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

static void setup_repo(const char *name, git_repository_init_options *opts)
{
	cl_git_pass(git_repository_init_ext(&_repo, name, opts));
	fixture_repo = name;
}

static void setup_templates(const char *name, bool setup_globally)
{
	git_buf path = GIT_BUF_INIT;

	cl_fixture_sandbox("template");
	if (strcmp(name, "template"))
		cl_must_pass(p_rename("template", name));

	fixture_templates = name;

	/*
	 * Create a symlink from link.sample to update.sample if the filesystem
	 * supports it.
	 */
	cl_git_pass(git_buf_join3(&path, '/', name, "hooks", "link.sample"));
#ifdef GIT_WIN32
	cl_git_mkfile(path.ptr, "#!/bin/sh\necho hello, world\n");
#else
	cl_must_pass(p_symlink("update.sample", path.ptr));
#endif

	git_buf_clear(&path);

	/* Create a file starting with a dot */
	cl_git_pass(git_buf_join3(&path, '/', name, "hooks", ".dotfile"));
	cl_git_mkfile(path.ptr, "something\n");

	git_buf_clear(&path);

	if (setup_globally) {
		cl_git_pass(git_buf_joinpath(&path, clar_sandbox_path(), name));
		create_tmp_global_config("tmp_global_path", "init.templatedir", path.ptr);
	}

	git_buf_dispose(&path);
}

static void validate_templates(git_repository *repo, const char *template_path)
{
	git_buf path = GIT_BUF_INIT, expected = GIT_BUF_INIT, actual = GIT_BUF_INIT;
	int filemode;

	cl_git_pass(git_buf_joinpath(&path, template_path, "description"));
	cl_git_pass(git_futils_readbuffer(&expected, path.ptr));

	git_buf_clear(&path);

	cl_git_pass(git_buf_joinpath(&path, git_repository_path(repo), "description"));
	cl_git_pass(git_futils_readbuffer(&actual, path.ptr));

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

	git_buf_dispose(&expected);
	git_buf_dispose(&actual);
	git_buf_dispose(&path);
}

void test_repo_template__external_templates_specified_in_options(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;

	opts.flags = GIT_REPOSITORY_INIT_MKPATH | GIT_REPOSITORY_INIT_BARE |
		GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;
	opts.template_path = "template";

	setup_templates("template", false);
	setup_repo("templated.git", &opts);

	validate_templates(_repo, "template");
}

void test_repo_template__external_templates_specified_in_config(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;

	opts.flags = GIT_REPOSITORY_INIT_MKPATH | GIT_REPOSITORY_INIT_BARE |
		GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;

	setup_templates("template", true);
	setup_repo("templated.git", &opts);

	validate_templates(_repo, "template");
}

void test_repo_template__external_templates_with_leading_dot(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;

	opts.flags = GIT_REPOSITORY_INIT_MKPATH | GIT_REPOSITORY_INIT_BARE |
		GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;

	setup_templates(".template_with_leading_dot", true);
	setup_repo("templated.git", &opts);

	validate_templates(_repo, ".template_with_leading_dot");
}

void test_repo_template__extended_with_template_and_shared_mode(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;
	const char *repo_path;
	int filemode;

	opts.flags = GIT_REPOSITORY_INIT_MKPATH |
		GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;
	opts.template_path = "template";
	opts.mode = GIT_REPOSITORY_INIT_SHARED_GROUP;

	setup_templates("template", false);
	setup_repo("init_shared_from_tpl", &opts);

	filemode = cl_repo_get_bool(_repo, "core.filemode");

	repo_path = git_repository_path(_repo);
	assert_mode_seems_okay(repo_path, "hooks",
		GIT_FILEMODE_TREE | GIT_REPOSITORY_INIT_SHARED_GROUP, true, filemode);
	assert_mode_seems_okay(repo_path, "info",
		GIT_FILEMODE_TREE | GIT_REPOSITORY_INIT_SHARED_GROUP, true, filemode);
	assert_mode_seems_okay(repo_path, "description",
		GIT_FILEMODE_BLOB, false, filemode);

	validate_templates(_repo, "template");
}

void test_repo_template__templated_head_is_used(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;
	git_buf head = GIT_BUF_INIT;

	opts.flags = GIT_REPOSITORY_INIT_MKPATH | GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;

	setup_templates("template", true);
	cl_git_mkfile("template/HEAD", "foobar\n");
	setup_repo("repo", &opts);

	cl_git_pass(git_futils_readbuffer(&head, "repo/.git/HEAD"));
	cl_assert_equal_s("foobar\n", head.ptr);

	git_buf_dispose(&head);
}

void test_repo_template__initial_head_option_overrides_template_head(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;
	git_buf head = GIT_BUF_INIT;

	opts.flags = GIT_REPOSITORY_INIT_MKPATH | GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;
	opts.initial_head = "manual";

	setup_templates("template", true);
	cl_git_mkfile("template/HEAD", "foobar\n");
	setup_repo("repo", &opts);

	cl_git_pass(git_futils_readbuffer(&head, "repo/.git/HEAD"));
	cl_assert_equal_s("ref: refs/heads/manual\n", head.ptr);

	git_buf_dispose(&head);
}

void test_repo_template__empty_template_path(void)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;

	opts.flags = GIT_REPOSITORY_INIT_MKPATH | GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE;
	opts.template_path = "";

	setup_repo("foo", &opts);
}
