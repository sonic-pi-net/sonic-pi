#include "clar_libgit2.h"
#include "buffer.h"
#include "fileops.h"

void test_config_global__initialize(void)
{
	git_buf path = GIT_BUF_INIT;

	cl_git_pass(git_futils_mkdir_r("home", NULL, 0777));
	cl_git_pass(git_path_prettify(&path, "home", NULL));
	cl_git_pass(git_libgit2_opts(
		GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL, path.ptr));

	cl_git_pass(git_futils_mkdir_r("xdg/git", NULL, 0777));
	cl_git_pass(git_path_prettify(&path, "xdg/git", NULL));
	cl_git_pass(git_libgit2_opts(
		GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_XDG, path.ptr));

	cl_git_pass(git_futils_mkdir_r("etc", NULL, 0777));
	cl_git_pass(git_path_prettify(&path, "etc", NULL));
	cl_git_pass(git_libgit2_opts(
		GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_SYSTEM, path.ptr));

	git_buf_free(&path);
}

void test_config_global__cleanup(void)
{
	cl_sandbox_set_search_path_defaults();
}

void test_config_global__open_global(void)
{
	git_config *cfg, *global, *selected, *dummy;

	cl_git_pass(git_config_open_default(&cfg));
	cl_git_pass(git_config_open_level(&global, cfg, GIT_CONFIG_LEVEL_GLOBAL));
	cl_git_fail(git_config_open_level(&dummy, cfg, GIT_CONFIG_LEVEL_XDG));
	cl_git_pass(git_config_open_global(&selected, cfg));

	git_config_free(selected);
	git_config_free(global);
	git_config_free(cfg);
}

void test_config_global__open_xdg(void)
{
	git_config *cfg, *xdg, *selected;
	const char *str = "teststring";
	const char *key = "this.variable";
	git_buf buf = {0};

	cl_git_mkfile("xdg/git/config", "# XDG config\n[core]\n  test = 1\n");

	cl_git_pass(git_config_open_default(&cfg));
	cl_git_pass(git_config_open_level(&xdg, cfg, GIT_CONFIG_LEVEL_XDG));
	cl_git_pass(git_config_open_global(&selected, cfg));

	cl_git_pass(git_config_set_string(xdg, key, str));
	cl_git_pass(git_config_get_string_buf(&buf, selected, key));
	cl_assert_equal_s(str, buf.ptr);

	git_buf_free(&buf);
	git_config_free(selected);
	git_config_free(xdg);
	git_config_free(cfg);
}
