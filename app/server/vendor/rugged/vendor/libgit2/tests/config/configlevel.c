#include "clar_libgit2.h"

void test_config_configlevel__adding_the_same_level_twice_returns_EEXISTS(void)
{
	int error;
	git_config *cfg;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config9"),
		GIT_CONFIG_LEVEL_LOCAL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config15"),
		GIT_CONFIG_LEVEL_GLOBAL, 0));
	error = git_config_add_file_ondisk(cfg, cl_fixture("config/config16"),
		GIT_CONFIG_LEVEL_GLOBAL, 0);

	cl_git_fail(error);
	cl_assert_equal_i(GIT_EEXISTS, error);

	git_config_free(cfg);
}

void test_config_configlevel__can_replace_a_config_file_at_an_existing_level(void)
{
	git_config *cfg;
	const char *s;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config18"),
		GIT_CONFIG_LEVEL_LOCAL, 1));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config19"),
		GIT_CONFIG_LEVEL_LOCAL, 1));

	cl_git_pass(git_config_get_string(&s, cfg, "core.stringglobal"));
	cl_assert_equal_s("don't find me!", s);

	git_config_free(cfg);
}

void test_config_configlevel__can_read_from_a_single_level_focused_file_after_parent_config_has_been_freed(void)
{
	git_config *cfg;
	git_config *single_level_cfg;
	const char *s;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config18"),
		GIT_CONFIG_LEVEL_GLOBAL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config19"),
		GIT_CONFIG_LEVEL_LOCAL, 0));

	cl_git_pass(git_config_open_level(&single_level_cfg, cfg, GIT_CONFIG_LEVEL_LOCAL));

	git_config_free(cfg);

	cl_git_pass(git_config_get_string(&s, single_level_cfg, "core.stringglobal"));
	cl_assert_equal_s("don't find me!", s);

	git_config_free(single_level_cfg);
}

void test_config_configlevel__fetching_a_level_from_an_empty_compound_config_returns_ENOTFOUND(void)
{
	git_config *cfg;
	git_config *local_cfg;

	cl_git_pass(git_config_new(&cfg));

	cl_assert_equal_i(GIT_ENOTFOUND, git_config_open_level(&local_cfg, cfg, GIT_CONFIG_LEVEL_LOCAL));

	git_config_free(cfg);
}
