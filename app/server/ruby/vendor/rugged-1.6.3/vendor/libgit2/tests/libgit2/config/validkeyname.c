#include "clar_libgit2.h"

#include "config.h"

static git_config *cfg;

void test_config_validkeyname__initialize(void)
{
	cl_fixture_sandbox("config/config10");

	cl_git_pass(git_config_open_ondisk(&cfg, "config10"));
}

void test_config_validkeyname__cleanup(void)
{
	git_config_free(cfg);
	cfg = NULL;

	cl_fixture_cleanup("config10");
}

static void assert_invalid_config_key_name(const char *name)
{
	git_buf buf = GIT_BUF_INIT;

	cl_git_fail_with(git_config_get_string_buf(&buf, cfg, name),
		GIT_EINVALIDSPEC);
	cl_git_fail_with(git_config_set_string(cfg, name, "42"),
		GIT_EINVALIDSPEC);
	cl_git_fail_with(git_config_delete_entry(cfg, name),
		GIT_EINVALIDSPEC);
	cl_git_fail_with(git_config_get_multivar_foreach(cfg, name, "*", NULL, NULL),
		GIT_EINVALIDSPEC);
	cl_git_fail_with(git_config_set_multivar(cfg, name, "*", "42"),
		GIT_EINVALIDSPEC);
}

void test_config_validkeyname__accessing_requires_a_valid_name(void)
{
	assert_invalid_config_key_name("");
	assert_invalid_config_key_name(".");
	assert_invalid_config_key_name("..");
	assert_invalid_config_key_name("core.");
	assert_invalid_config_key_name("d#ff.dirstat.lines");
	assert_invalid_config_key_name("diff.dirstat.lines#");
	assert_invalid_config_key_name("dif\nf.dirstat.lines");
	assert_invalid_config_key_name("dif.dir\nstat.lines");
	assert_invalid_config_key_name("dif.dirstat.li\nes");
}
