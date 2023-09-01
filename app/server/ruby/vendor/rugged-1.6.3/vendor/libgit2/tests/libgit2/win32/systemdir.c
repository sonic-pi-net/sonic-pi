#include "clar_libgit2.h"
#include "futils.h"
#include "sysdir.h"

#ifdef GIT_WIN32
static char *path_save;
static git_str gfw_path_root = GIT_STR_INIT;
static git_str gfw_registry_root = GIT_STR_INIT;
#endif

void test_win32_systemdir__initialize(void)
{
#ifdef GIT_WIN32
	git_str path_env = GIT_STR_INIT;

	path_save = cl_getenv("PATH");
	git_win32__set_registry_system_dir(L"");

	cl_git_pass(git_str_puts(&path_env, "C:\\GitTempTest\\Foo;\"c:\\program files\\doesnotexisttesttemp\";C:\\fakefakedoesnotexist"));
	cl_setenv("PATH", path_env.ptr);

	cl_git_pass(git_str_puts(&gfw_path_root, clar_sandbox_path()));
	cl_git_pass(git_str_puts(&gfw_path_root, "/fake_gfw_path_install"));

	cl_git_pass(git_str_puts(&gfw_registry_root, clar_sandbox_path()));
	cl_git_pass(git_str_puts(&gfw_registry_root, "/fake_gfw_registry_install"));

	git_str_dispose(&path_env);
#endif
}

void test_win32_systemdir__cleanup(void)
{
#ifdef GIT_WIN32
	cl_fixture_cleanup("fake_gfw_path_install");
	cl_fixture_cleanup("fake_gfw_registry_install");
	git_str_dispose(&gfw_path_root);
	git_str_dispose(&gfw_registry_root);

	cl_setenv("PATH", path_save);
	git__free(path_save);
	path_save = NULL;

	git_win32__set_registry_system_dir(NULL);
	cl_sandbox_set_search_path_defaults();
#endif
}

#ifdef GIT_WIN32
static void fix_path(git_str *s)
{
	char *c;

	for (c = s->ptr; *c; c++) {
		if (*c == '/')
			*c = '\\';
	}
}

static void populate_fake_gfw(
	git_str *expected_etc_dir,
	const char *root,
	const char *token,
	bool create_gitconfig,
	bool create_mingw64_gitconfig,
	bool add_to_path,
	bool add_to_registry)
{
	git_str bin_path = GIT_STR_INIT, exe_path = GIT_STR_INIT,
	etc_path = GIT_STR_INIT, mingw64_path = GIT_STR_INIT,
	config_path = GIT_STR_INIT, path_env = GIT_STR_INIT,
	config_data = GIT_STR_INIT;

	cl_git_pass(git_str_puts(&bin_path, root));
	cl_git_pass(git_str_puts(&bin_path, "/cmd"));
	cl_git_pass(git_futils_mkdir_r(bin_path.ptr, 0755));

	cl_git_pass(git_str_puts(&exe_path, bin_path.ptr));
	cl_git_pass(git_str_puts(&exe_path, "/git.cmd"));
	cl_git_mkfile(exe_path.ptr, "This is a fake executable.");

	cl_git_pass(git_str_puts(&etc_path, root));
	cl_git_pass(git_str_puts(&etc_path, "/etc"));
	cl_git_pass(git_futils_mkdir_r(etc_path.ptr, 0755));

	cl_git_pass(git_str_puts(&mingw64_path, root));
	cl_git_pass(git_str_puts(&mingw64_path, "/mingw64/etc"));
	cl_git_pass(git_futils_mkdir_r(mingw64_path.ptr, 0755));

	if (create_gitconfig) {
		git_str_clear(&config_data);
		git_str_printf(&config_data, "[gfw]\n\ttest = etc %s\n", token);

		cl_git_pass(git_str_puts(&config_path, etc_path.ptr));
		cl_git_pass(git_str_puts(&config_path, "/gitconfig"));
		cl_git_mkfile(config_path.ptr, config_data.ptr);
	}

	if (create_mingw64_gitconfig) {
		git_str_clear(&config_data);
		git_str_printf(&config_data, "[gfw]\n\ttest = mingw64 %s\n", token);

		git_str_clear(&config_path);
		cl_git_pass(git_str_puts(&config_path, mingw64_path.ptr));
		cl_git_pass(git_str_puts(&config_path, "/gitconfig"));
		cl_git_mkfile(config_path.ptr, config_data.ptr);
	}

	if (add_to_path) {
		fix_path(&bin_path);
		cl_git_pass(git_str_puts(&path_env, "C:\\GitTempTest\\Foo;\"c:\\program files\\doesnotexisttesttemp\";"));
		cl_git_pass(git_str_puts(&path_env, bin_path.ptr));
		cl_git_pass(git_str_puts(&path_env, ";C:\\fakefakedoesnotexist"));
		cl_setenv("PATH", path_env.ptr);
	}

	if (add_to_registry) {
		git_win32_path registry_path;
		size_t offset = 0;

		cl_assert(git_win32_path_from_utf8(registry_path, root) >= 0);
		if (wcsncmp(registry_path, L"\\\\?\\", CONST_STRLEN("\\\\?\\")) == 0)
		    offset = CONST_STRLEN("\\\\?\\");
		git_win32__set_registry_system_dir(registry_path + offset);
	}

	cl_git_pass(git_str_join(expected_etc_dir, GIT_PATH_LIST_SEPARATOR, expected_etc_dir->ptr, etc_path.ptr));
	cl_git_pass(git_str_join(expected_etc_dir, GIT_PATH_LIST_SEPARATOR, expected_etc_dir->ptr, mingw64_path.ptr));

	git_str_dispose(&bin_path);
	git_str_dispose(&exe_path);
	git_str_dispose(&etc_path);
	git_str_dispose(&mingw64_path);
	git_str_dispose(&config_path);
	git_str_dispose(&path_env);
	git_str_dispose(&config_data);
}

static void populate_fake_ecosystem(
	git_str *expected_etc_dir,
	bool create_gitconfig,
	bool create_mingw64_gitconfig,
	bool path,
	bool registry)
{
	if (path)
		populate_fake_gfw(expected_etc_dir, gfw_path_root.ptr, "path", create_gitconfig, create_mingw64_gitconfig, true, false);

	if (registry)
		populate_fake_gfw(expected_etc_dir, gfw_registry_root.ptr, "registry", create_gitconfig, create_mingw64_gitconfig, false, true);
}
#endif

void test_win32_systemdir__finds_etc_in_path(void)
{
#ifdef GIT_WIN32
	git_str expected = GIT_STR_INIT, out = GIT_STR_INIT;
	git_config *cfg;
	git_buf value = GIT_BUF_INIT;

	populate_fake_ecosystem(&expected, true, false, true, false);

	cl_git_pass(git_win32__find_system_dirs(&out, "etc"));
	cl_assert_equal_s(out.ptr, expected.ptr);

	git_sysdir_reset();

	cl_git_pass(git_config_open_default(&cfg));
	cl_git_pass(git_config_get_string_buf(&value, cfg, "gfw.test"));
	cl_assert_equal_s("etc path", value.ptr);

	git_buf_dispose(&value);
	git_str_dispose(&expected);
	git_str_dispose(&out);
	git_config_free(cfg);
#endif
}

void test_win32_systemdir__finds_mingw64_etc_in_path(void)
{
#ifdef GIT_WIN32
	git_str expected = GIT_STR_INIT, out = GIT_STR_INIT;
	git_config* cfg;
	git_buf value = GIT_BUF_INIT;

	populate_fake_ecosystem(&expected, false, true, true, false);

	cl_git_pass(git_win32__find_system_dirs(&out, "etc"));
	cl_assert_equal_s(out.ptr, expected.ptr);

	git_sysdir_reset();

	cl_git_pass(git_config_open_default(&cfg));
	cl_git_pass(git_config_get_string_buf(&value, cfg, "gfw.test"));
	cl_assert_equal_s("mingw64 path", value.ptr);

	git_buf_dispose(&value);
	git_str_dispose(&expected);
	git_str_dispose(&out);
	git_config_free(cfg);
#endif
}

void test_win32_systemdir__prefers_etc_to_mingw64_in_path(void)
{
#ifdef GIT_WIN32
	git_str expected = GIT_STR_INIT, out = GIT_STR_INIT;
	git_config* cfg;
	git_buf value = GIT_BUF_INIT;

	populate_fake_ecosystem(&expected, true, true, true, false);

	cl_git_pass(git_win32__find_system_dirs(&out, "etc"));
	cl_assert_equal_s(out.ptr, expected.ptr);

	git_sysdir_reset();

	cl_git_pass(git_config_open_default(&cfg));
	cl_git_pass(git_config_get_string_buf(&value, cfg, "gfw.test"));
	cl_assert_equal_s("etc path", value.ptr);

	git_buf_dispose(&value);
	git_str_dispose(&expected);
	git_str_dispose(&out);
	git_config_free(cfg);
#endif
}

void test_win32_systemdir__finds_etc_in_registry(void)
{
#ifdef GIT_WIN32
	git_str expected = GIT_STR_INIT, out = GIT_STR_INIT;
	git_config* cfg;
	git_buf value = GIT_BUF_INIT;

	populate_fake_ecosystem(&expected, true, false, false, true);

	cl_git_pass(git_win32__find_system_dirs(&out, "etc"));
	cl_assert_equal_s(out.ptr, expected.ptr);

	git_sysdir_reset();

	cl_git_pass(git_config_open_default(&cfg));
	cl_git_pass(git_config_get_string_buf(&value, cfg, "gfw.test"));
	cl_assert_equal_s("etc registry", value.ptr);

	git_buf_dispose(&value);
	git_str_dispose(&expected);
	git_str_dispose(&out);
	git_config_free(cfg);
#endif
}

void test_win32_systemdir__finds_mingw64_etc_in_registry(void)
{
#ifdef GIT_WIN32
	git_str expected = GIT_STR_INIT, out = GIT_STR_INIT;
	git_config* cfg;
	git_buf value = GIT_BUF_INIT;

	populate_fake_ecosystem(&expected, false, true, false, true);

	cl_git_pass(git_win32__find_system_dirs(&out, "etc"));
	cl_assert_equal_s(out.ptr, expected.ptr);

	git_sysdir_reset();

	cl_git_pass(git_config_open_default(&cfg));
	cl_git_pass(git_config_get_string_buf(&value, cfg, "gfw.test"));
	cl_assert_equal_s("mingw64 registry", value.ptr);

	git_buf_dispose(&value);
	git_str_dispose(&expected);
	git_str_dispose(&out);
	git_config_free(cfg);
#endif
}

void test_win32_systemdir__prefers_etc_to_mingw64_in_registry(void)
{
#ifdef GIT_WIN32
	git_str expected = GIT_STR_INIT, out = GIT_STR_INIT;
	git_config* cfg;
	git_buf value = GIT_BUF_INIT;

	populate_fake_ecosystem(&expected, true, true, false, true);

	cl_git_pass(git_win32__find_system_dirs(&out, "etc"));
	cl_assert_equal_s(out.ptr, expected.ptr);

	git_sysdir_reset();

	cl_git_pass(git_config_open_default(&cfg));
	cl_git_pass(git_config_get_string_buf(&value, cfg, "gfw.test"));
	cl_assert_equal_s("etc registry", value.ptr);

	git_buf_dispose(&value);
	git_str_dispose(&expected);
	git_str_dispose(&out);
	git_config_free(cfg);
#endif
}

void test_win32_systemdir__prefers_path_to_registry(void)
{
#ifdef GIT_WIN32
	git_str expected = GIT_STR_INIT, out = GIT_STR_INIT;
	git_config* cfg;
	git_buf value = GIT_BUF_INIT;

	populate_fake_ecosystem(&expected, true, true, true, true);

	cl_git_pass(git_win32__find_system_dirs(&out, "etc"));
	cl_assert_equal_s(out.ptr, expected.ptr);

	git_sysdir_reset();

	cl_git_pass(git_config_open_default(&cfg));
	cl_git_pass(git_config_get_string_buf(&value, cfg, "gfw.test"));
	cl_assert_equal_s("etc path", value.ptr);

	git_buf_dispose(&value);
	git_str_dispose(&expected);
	git_str_dispose(&out);
	git_config_free(cfg);
#endif
}

void test_win32_systemdir__no_git_installed(void)
{
#ifdef GIT_WIN32
	git_str out = GIT_STR_INIT;

	cl_git_pass(git_win32__find_system_dirs(&out, "etc"));
	cl_assert_equal_s(out.ptr, "");
#endif
}
