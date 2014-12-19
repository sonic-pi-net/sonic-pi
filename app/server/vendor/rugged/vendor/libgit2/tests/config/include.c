#include "clar_libgit2.h"
#include "buffer.h"
#include "fileops.h"

void test_config_include__relative(void)
{
	git_config *cfg;
	const char *str;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config-include")));

	cl_git_pass(git_config_get_string(&str, cfg, "foo.bar.baz"));
	cl_assert_equal_s(str, "huzzah");

	git_config_free(cfg);
}

void test_config_include__absolute(void)
{
	git_config *cfg;
	const char *str;
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_buf_printf(&buf, "[include]\npath = %s/config-included", cl_fixture("config")));

	cl_git_mkfile("config-include-absolute", git_buf_cstr(&buf));
	git_buf_free(&buf);
	cl_git_pass(git_config_open_ondisk(&cfg, "config-include-absolute"));

	cl_git_pass(git_config_get_string(&str, cfg, "foo.bar.baz"));
	cl_assert_equal_s(str, "huzzah");

	git_config_free(cfg);
}

void test_config_include__homedir(void)
{
	git_config *cfg;
	const char *str;

	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL, cl_fixture("config")));
	cl_git_mkfile("config-include-homedir",  "[include]\npath = ~/config-included");

	cl_git_pass(git_config_open_ondisk(&cfg, "config-include-homedir"));

	cl_git_pass(git_config_get_string(&str, cfg, "foo.bar.baz"));
	cl_assert_equal_s(str, "huzzah");

	git_config_free(cfg);

	cl_sandbox_set_search_path_defaults();
}

/* We need to pretend that the variables were defined where the file was included */
void test_config_include__ordering(void)
{
	git_config *cfg;
	const char *str;

	cl_git_mkfile("included", "[foo \"bar\"]\nbaz = hurrah\nfrotz = hiya");
	cl_git_mkfile("including",
		      "[foo \"bar\"]\nfrotz = hello\n"
		      "[include]\npath = included\n"
		      "[foo \"bar\"]\nbaz = huzzah\n");

	cl_git_pass(git_config_open_ondisk(&cfg, "including"));

	cl_git_pass(git_config_get_string(&str, cfg, "foo.bar.frotz"));
	cl_assert_equal_s(str, "hiya");
	cl_git_pass(git_config_get_string(&str, cfg, "foo.bar.baz"));
	cl_assert_equal_s(str, "huzzah");

	git_config_free(cfg);
}

/* We need to pretend that the variables were defined where the file was included */
void test_config_include__depth(void)
{
	git_config *cfg;

	cl_git_mkfile("a", "[include]\npath = b");
	cl_git_mkfile("b", "[include]\npath = a");

	cl_git_fail(git_config_open_ondisk(&cfg, "a"));

	p_unlink("a");
	p_unlink("b");
}

void test_config_include__missing(void)
{
	git_config *cfg;
	const char *str;

	cl_git_mkfile("including", "[include]\npath = nonexistentfile\n[foo]\nbar = baz");

	giterr_clear();
	cl_git_pass(git_config_open_ondisk(&cfg, "including"));
	cl_assert(giterr_last() == NULL);
	cl_git_pass(git_config_get_string(&str, cfg, "foo.bar"));
	cl_assert_equal_s(str, "baz");

	git_config_free(cfg);
}
