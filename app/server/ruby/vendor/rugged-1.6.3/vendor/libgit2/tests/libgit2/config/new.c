#include "clar_libgit2.h"

#include "filebuf.h"
#include "futils.h"
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
	cl_assert_equal_s("auto", buf.ptr);
	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, config, "core.editor"));
	cl_assert_equal_s("ed", buf.ptr);

	git_buf_dispose(&buf);
	git_config_free(config);

	cl_must_pass(p_unlink(TEST_CONFIG));
}
