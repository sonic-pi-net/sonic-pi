#include "clar_libgit2.h"

#define TEST_FILE "config.refresh"

void test_config_refresh__initialize(void)
{
}

void test_config_refresh__cleanup(void)
{
	cl_fixture_cleanup(TEST_FILE);
}

void test_config_refresh__update_value(void)
{
	git_config *cfg;
	int32_t v;

	cl_git_mkfile(TEST_FILE, "[section]\n\tvalue = 1\n\n");

	/* By freeing the config, we make sure we flush the values  */
	cl_git_pass(git_config_open_ondisk(&cfg, TEST_FILE));

	cl_git_pass(git_config_get_int32(&v, cfg, "section.value"));
	cl_assert_equal_i(1, v);

	cl_git_rewritefile(TEST_FILE, "[section]\n\tvalue = 10\n\n");

	cl_git_pass(git_config_refresh(cfg));

	cl_git_pass(git_config_get_int32(&v, cfg, "section.value"));
	cl_assert_equal_i(10, v);

	git_config_free(cfg);
}

void test_config_refresh__delete_value(void)
{
	git_config *cfg;
	int32_t v;

	cl_git_mkfile(TEST_FILE, "[section]\n\tvalue = 1\n\n");

	/* By freeing the config, we make sure we flush the values  */
	cl_git_pass(git_config_open_ondisk(&cfg, TEST_FILE));

	cl_git_pass(git_config_get_int32(&v, cfg, "section.value"));
	cl_assert_equal_i(1, v);
	cl_git_fail(git_config_get_int32(&v, cfg, "section.newval"));

	cl_git_rewritefile(TEST_FILE, "[section]\n\tnewval = 10\n\n");

	cl_git_fail_with(GIT_ENOTFOUND, git_config_get_int32(&v, cfg, "section.value"));

	cl_git_pass(git_config_get_int32(&v, cfg, "section.newval"));

	cl_git_pass(git_config_refresh(cfg));

	cl_git_fail(git_config_get_int32(&v, cfg, "section.value"));
	cl_git_pass(git_config_get_int32(&v, cfg, "section.newval"));
	cl_assert_equal_i(10, v);

	git_config_free(cfg);
}
