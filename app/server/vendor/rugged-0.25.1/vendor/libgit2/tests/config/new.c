#include "clar_libgit2.h"

#include "filebuf.h"
#include "fileops.h"
#include "posix.h"

#define TEST_CONFIG "git-new-config"

void test_config_new__write_new_config(void)
{
	git_config *config;
	git_buf buf = GIT_BUF_INIT;

	cl_git_mkfile(TEST_CONFIG, "");
	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));

	cl_git_pass(git_config_set_string(config, "color.ui", "auto"));
	cl_git_pass(git_config_set_string(config, "core.editor", "ed"));

	git_config_free(config);

	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));

	cl_git_pass(git_config_get_string_buf(&buf, config, "color.ui"));
	cl_assert_equal_s("auto", git_buf_cstr(&buf));
	git_buf_clear(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, config, "core.editor"));
	cl_assert_equal_s("ed", git_buf_cstr(&buf));

	git_buf_free(&buf);
	git_config_free(config);

	p_unlink(TEST_CONFIG);
}
