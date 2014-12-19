#include "clar_libgit2.h"

#include "filebuf.h"
#include "fileops.h"
#include "posix.h"

#define TEST_CONFIG "git-test-config"

void test_config_stress__initialize(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;

	cl_git_pass(git_filebuf_open(&file, TEST_CONFIG, 0, 0666));

	git_filebuf_printf(&file, "[color]\n\tui = auto\n");
	git_filebuf_printf(&file, "[core]\n\teditor = \n");

	cl_git_pass(git_filebuf_commit(&file));
}

void test_config_stress__cleanup(void)
{
	p_unlink(TEST_CONFIG);
}

void test_config_stress__dont_break_on_invalid_input(void)
{
	const char *editor, *color;
	git_config *config;

	cl_assert(git_path_exists(TEST_CONFIG));
	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));

	cl_git_pass(git_config_get_string(&color, config, "color.ui"));
	cl_git_pass(git_config_get_string(&editor, config, "core.editor"));

	git_config_free(config);
}

void test_config_stress__comments(void)
{
	git_config *config;
	const char *str;

	cl_git_pass(git_config_open_ondisk(&config, cl_fixture("config/config12")));

	cl_git_pass(git_config_get_string(&str, config, "some.section.test2"));
	cl_assert_equal_s("hello", str);

	cl_git_pass(git_config_get_string(&str, config, "some.section.test3"));
	cl_assert_equal_s("welcome", str);

	cl_git_pass(git_config_get_string(&str, config, "some.section.other"));
	cl_assert_equal_s("hello! \" ; ; ; ", str);

	cl_git_pass(git_config_get_string(&str, config, "some.section.other2"));
	cl_assert_equal_s("cool! \" # # # ", str);

	cl_git_pass(git_config_get_string(&str, config, "some.section.multi"));
	cl_assert_equal_s("hi, this is a ; multiline comment # with ;\n special chars and other stuff !@#", str);

	cl_git_pass(git_config_get_string(&str, config, "some.section.multi2"));
	cl_assert_equal_s("good, this is a ; multiline comment # with ;\n special chars and other stuff !@#", str);

	cl_git_pass(git_config_get_string(&str, config, "some.section.back"));
	cl_assert_equal_s("this is \ba phrase", str);

	git_config_free(config);
}

void test_config_stress__escape_subsection_names(void)
{
	git_config *config;
	const char *str;

	cl_assert(git_path_exists("git-test-config"));
	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));

	cl_git_pass(git_config_set_string(config, "some.sec\\tion.other", "foo"));
	git_config_free(config);

	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));

	cl_git_pass(git_config_get_string(&str, config, "some.sec\\tion.other"));
	cl_assert_equal_s("foo", str);
	git_config_free(config);
}

void test_config_stress__trailing_backslash(void)
{
	git_config *config;
	const char *str;
	const char *path =  "C:\\iam\\some\\windows\\path\\";

	cl_assert(git_path_exists("git-test-config"));
	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));
	cl_git_pass(git_config_set_string(config, "windows.path", path));
	git_config_free(config);

	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));
	cl_git_pass(git_config_get_string(&str, config, "windows.path"));
	cl_assert_equal_s(path, str);
	git_config_free(config);
}

void test_config_stress__complex(void)
{
	git_config *config;
	const char *str;
	const char *path = "./config-immediate-multiline";

	cl_git_mkfile(path, "[imm]\n multi = \"\\\nfoo\"");
	cl_git_pass(git_config_open_ondisk(&config, path));
	cl_git_pass(git_config_get_string(&str, config, "imm.multi"));
	cl_assert_equal_s(str, "foo");
	git_config_free(config);
}
