#include "clar_libgit2.h"
#include "buffer.h"
#include "fileops.h"

static git_config *cfg;
static git_buf buf;

void test_config_include__initialize(void)
{
    cfg = NULL;
    git_buf_init(&buf, 0);
}

void test_config_include__cleanup(void)
{
    git_config_free(cfg);
    git_buf_free(&buf);
}

void test_config_include__relative(void)
{
	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config-include")));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar.baz"));
	cl_assert_equal_s("huzzah", git_buf_cstr(&buf));
}

void test_config_include__absolute(void)
{
	cl_git_pass(git_buf_printf(&buf, "[include]\npath = %s/config-included", cl_fixture("config")));

	cl_git_mkfile("config-include-absolute", git_buf_cstr(&buf));
	git_buf_free(&buf);
	cl_git_pass(git_config_open_ondisk(&cfg, "config-include-absolute"));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar.baz"));
	cl_assert_equal_s("huzzah", git_buf_cstr(&buf));
}

void test_config_include__homedir(void)
{
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL, cl_fixture("config")));
	cl_git_mkfile("config-include-homedir",  "[include]\npath = ~/config-included");

	cl_git_pass(git_config_open_ondisk(&cfg, "config-include-homedir"));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar.baz"));
	cl_assert_equal_s("huzzah", git_buf_cstr(&buf));

	cl_sandbox_set_search_path_defaults();
}

/* We need to pretend that the variables were defined where the file was included */
void test_config_include__ordering(void)
{
	cl_git_mkfile("included", "[foo \"bar\"]\nbaz = hurrah\nfrotz = hiya");
	cl_git_mkfile("including",
		      "[foo \"bar\"]\nfrotz = hello\n"
		      "[include]\npath = included\n"
		      "[foo \"bar\"]\nbaz = huzzah\n");

	cl_git_pass(git_config_open_ondisk(&cfg, "including"));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar.frotz"));
	cl_assert_equal_s("hiya", git_buf_cstr(&buf));
	git_buf_clear(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar.baz"));
	cl_assert_equal_s("huzzah", git_buf_cstr(&buf));
}

/* We need to pretend that the variables were defined where the file was included */
void test_config_include__depth(void)
{
	cl_git_mkfile("a", "[include]\npath = b");
	cl_git_mkfile("b", "[include]\npath = a");

	cl_git_fail(git_config_open_ondisk(&cfg, "a"));

	p_unlink("a");
	p_unlink("b");
}

void test_config_include__missing(void)
{
	cl_git_mkfile("including", "[include]\npath = nonexistentfile\n[foo]\nbar = baz");

	giterr_clear();
	cl_git_pass(git_config_open_ondisk(&cfg, "including"));
	cl_assert(giterr_last() == NULL);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar"));
	cl_assert_equal_s("baz", git_buf_cstr(&buf));
}

void test_config_include__missing_homedir(void)
{
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL, cl_fixture("config")));
	cl_git_mkfile("including", "[include]\npath = ~/.nonexistentfile\n[foo]\nbar = baz");

	giterr_clear();
	cl_git_pass(git_config_open_ondisk(&cfg, "including"));
	cl_assert(giterr_last() == NULL);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar"));
	cl_assert_equal_s("baz", git_buf_cstr(&buf));

	cl_sandbox_set_search_path_defaults();
}

#define replicate10(s) s s s s s s s s s s
void test_config_include__depth2(void)
{
	const char *content = "[include]\n" replicate10(replicate10("path=bottom\n"));

	cl_git_mkfile("top-level", "[include]\npath = middle\n[foo]\nbar = baz");
	cl_git_mkfile("middle", content);
	cl_git_mkfile("bottom", "[foo]\nbar2 = baz2");

	cl_git_pass(git_config_open_ondisk(&cfg, "top-level"));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar"));
	cl_assert_equal_s("baz", git_buf_cstr(&buf));

	git_buf_clear(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar2"));
	cl_assert_equal_s("baz2", git_buf_cstr(&buf));
}

void test_config_include__removing_include_removes_values(void)
{
	cl_git_mkfile("top-level", "[include]\npath = included");
	cl_git_mkfile("included", "[foo]\nbar = value");

	cl_git_pass(git_config_open_ondisk(&cfg, "top-level"));
	cl_git_mkfile("top-level", "");
	cl_git_fail(git_config_get_string_buf(&buf, cfg, "foo.bar"));
}

void test_config_include__rewriting_include_refreshes_values(void)
{
	cl_git_mkfile("top-level", "[include]\npath = first\n[include]\npath = second");
	cl_git_mkfile("first", "[first]\nfoo = bar");
	cl_git_mkfile("second", "[second]\nfoo = bar");

	cl_git_pass(git_config_open_ondisk(&cfg, "top-level"));
	cl_git_mkfile("first", "[first]\nother = value");
	cl_git_fail(git_config_get_string_buf(&buf, cfg, "foo.bar"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "first.other"));
	cl_assert_equal_s(buf.ptr, "value");
}

void test_config_include__included_variables_cannot_be_deleted(void)
{
	cl_git_mkfile("top-level", "[include]\npath = included\n");
	cl_git_mkfile("included", "[foo]\nbar = value");

	cl_git_pass(git_config_open_ondisk(&cfg, "top-level"));
	cl_git_fail(git_config_delete_entry(cfg, "foo.bar"));
}

void test_config_include__included_variables_cannot_be_modified(void)
{
	cl_git_mkfile("top-level", "[include]\npath = included\n");
	cl_git_mkfile("included", "[foo]\nbar = value");

	cl_git_pass(git_config_open_ondisk(&cfg, "top-level"));
	cl_git_fail(git_config_set_string(cfg, "foo.bar", "other-value"));
}
