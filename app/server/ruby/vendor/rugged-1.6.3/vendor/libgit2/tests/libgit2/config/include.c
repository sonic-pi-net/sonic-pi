#include "clar_libgit2.h"
#include "futils.h"

static git_config *cfg;
static git_buf buf;

void test_config_include__initialize(void)
{
	cfg = NULL;
	memset(&buf, 0, sizeof(git_buf));
}

void test_config_include__cleanup(void)
{
	git_config_free(cfg);
	git_buf_dispose(&buf);
}

void test_config_include__relative(void)
{
	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config-include")));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar.baz"));
	cl_assert_equal_s("huzzah", buf.ptr);
}

void test_config_include__absolute(void)
{
	git_str cfgdata = GIT_STR_INIT;
	cl_git_pass(git_str_printf(&cfgdata, "[include]\npath = %s/config-included", cl_fixture("config")));

	cl_git_mkfile("config-include-absolute", cfgdata.ptr);
	git_str_dispose(&cfgdata);

	cl_git_pass(git_config_open_ondisk(&cfg, "config-include-absolute"));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar.baz"));
	cl_assert_equal_s("huzzah", buf.ptr);

	cl_git_pass(p_unlink("config-include-absolute"));
}

void test_config_include__homedir(void)
{
	git_str homefile = GIT_STR_INIT;

	cl_fake_homedir(&homefile);
	cl_git_pass(git_str_joinpath(&homefile, homefile.ptr, "config-included"));

	cl_git_mkfile("config-include-homedir",  "[include]\npath = ~/config-included");
	cl_git_mkfile(homefile.ptr, "[foo \"bar\"]\n\tbaz = huzzah\n");

	cl_git_pass(git_config_open_ondisk(&cfg, "config-include-homedir"));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar.baz"));
	cl_assert_equal_s("huzzah", buf.ptr);

	cl_sandbox_set_search_path_defaults();

	cl_git_pass(p_unlink("config-include-homedir"));

	git_str_dispose(&homefile);
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
	cl_assert_equal_s("hiya", buf.ptr);
	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar.baz"));
	cl_assert_equal_s("huzzah", buf.ptr);

	cl_git_pass(p_unlink("included"));
	cl_git_pass(p_unlink("including"));
}

/* We need to pretend that the variables were defined where the file was included */
void test_config_include__depth(void)
{
	cl_git_mkfile("a", "[include]\npath = b");
	cl_git_mkfile("b", "[include]\npath = a");

	cl_git_fail(git_config_open_ondisk(&cfg, "a"));

	cl_git_pass(p_unlink("a"));
	cl_git_pass(p_unlink("b"));
}

void test_config_include__empty_path_sanely_handled(void)
{
	cl_git_mkfile("a", "[include]\npath");
	cl_git_pass(git_config_open_ondisk(&cfg, "a"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "include.path"));
	cl_assert_equal_s("", buf.ptr);

	cl_git_pass(p_unlink("a"));
}

void test_config_include__missing(void)
{
	cl_git_mkfile("including", "[include]\npath = nonexistentfile\n[foo]\nbar = baz");

	git_error_clear();
	cl_git_pass(git_config_open_ondisk(&cfg, "including"));
	cl_assert(git_error_last() == NULL);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar"));
	cl_assert_equal_s("baz", buf.ptr);

	cl_git_pass(p_unlink("including"));
}

void test_config_include__missing_homedir(void)
{
	cl_fake_homedir(NULL);

	cl_git_mkfile("including", "[include]\npath = ~/.nonexistentfile\n[foo]\nbar = baz");

	git_error_clear();
	cl_git_pass(git_config_open_ondisk(&cfg, "including"));
	cl_assert(git_error_last() == NULL);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar"));
	cl_assert_equal_s("baz", buf.ptr);

	cl_sandbox_set_search_path_defaults();
	cl_git_pass(p_unlink("including"));
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
	cl_assert_equal_s("baz", buf.ptr);

	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar2"));
	cl_assert_equal_s("baz2", buf.ptr);

	cl_git_pass(p_unlink("top-level"));
	cl_git_pass(p_unlink("middle"));
	cl_git_pass(p_unlink("bottom"));
}

void test_config_include__removing_include_removes_values(void)
{
	cl_git_mkfile("top-level", "[include]\npath = included");
	cl_git_mkfile("included", "[foo]\nbar = value");

	cl_git_pass(git_config_open_ondisk(&cfg, "top-level"));
	cl_git_mkfile("top-level", "");
	cl_git_fail(git_config_get_string_buf(&buf, cfg, "foo.bar"));

	cl_git_pass(p_unlink("top-level"));
	cl_git_pass(p_unlink("included"));
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

	cl_git_pass(p_unlink("top-level"));
	cl_git_pass(p_unlink("first"));
	cl_git_pass(p_unlink("second"));
}

void test_config_include__rewriting_include_twice_refreshes_values(void)
{
	cl_git_mkfile("top-level", "[include]\npath = included");
	cl_git_mkfile("included", "[foo]\nbar = first-value");

	cl_git_pass(git_config_open_ondisk(&cfg, "top-level"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar"));

	git_buf_dispose(&buf);
	cl_git_mkfile("included", "[foo]\nother = value2");
	cl_git_fail(git_config_get_string_buf(&buf, cfg, "foo.bar"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.other"));
	cl_assert_equal_s(buf.ptr, "value2");

	git_buf_dispose(&buf);
	cl_git_mkfile("included", "[foo]\nanother = bar");
	cl_git_fail(git_config_get_string_buf(&buf, cfg, "foo.other"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.another"));
	cl_assert_equal_s(buf.ptr, "bar");

	cl_git_pass(p_unlink("top-level"));
	cl_git_pass(p_unlink("included"));
}

void test_config_include__included_variables_cannot_be_deleted(void)
{
	cl_git_mkfile("top-level", "[include]\npath = included\n");
	cl_git_mkfile("included", "[foo]\nbar = value");

	cl_git_pass(git_config_open_ondisk(&cfg, "top-level"));
	cl_git_fail(git_config_delete_entry(cfg, "foo.bar"));

	cl_git_pass(p_unlink("top-level"));
	cl_git_pass(p_unlink("included"));
}

void test_config_include__included_variables_cannot_be_modified(void)
{
	cl_git_mkfile("top-level", "[include]\npath = included\n");

	cl_git_mkfile("included", "[foo]\nbar = value");

	cl_git_pass(git_config_open_ondisk(&cfg, "top-level"));
	cl_git_fail(git_config_set_string(cfg, "foo.bar", "other-value"));

	cl_git_pass(p_unlink("top-level"));
	cl_git_pass(p_unlink("included"));
}

void test_config_include__variables_in_included_override_including(void)
{
	int i;

	cl_git_mkfile("top-level", "[foo]\nbar = 1\n[include]\npath = included");
	cl_git_mkfile("included", "[foo]\nbar = 2");

	cl_git_pass(git_config_open_ondisk(&cfg, "top-level"));
	cl_git_pass(git_config_get_int32(&i, cfg, "foo.bar"));
	cl_assert_equal_i(i, 2);

	cl_git_pass(p_unlink("top-level"));
	cl_git_pass(p_unlink("included"));
}

void test_config_include__variables_in_including_override_included(void)
{
	int i;

	cl_git_mkfile("top-level", "[include]\npath = included\n[foo]\nbar = 1");
	cl_git_mkfile("included", "[foo]\nbar = 2");

	cl_git_pass(git_config_open_ondisk(&cfg, "top-level"));
	cl_git_pass(git_config_get_int32(&i, cfg, "foo.bar"));
	cl_assert_equal_i(i, 1);

	cl_git_pass(p_unlink("top-level"));
	cl_git_pass(p_unlink("included"));
}
