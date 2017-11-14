#include "clar_libgit2.h"

void test_config_add__initialize(void)
{
	cl_fixture_sandbox("config/config10");
}

void test_config_add__cleanup(void)
{
	cl_fixture_cleanup("config10");
}

void test_config_add__to_existing_section(void)
{
	git_config *cfg;
	int32_t i;

	cl_git_pass(git_config_open_ondisk(&cfg, "config10"));
	cl_git_pass(git_config_set_int32(cfg, "empty.tmp", 5));
	cl_git_pass(git_config_get_int32(&i, cfg, "empty.tmp"));
	cl_assert(i == 5);
	cl_git_pass(git_config_delete_entry(cfg, "empty.tmp"));
	git_config_free(cfg);
}

void test_config_add__to_new_section(void)
{
	git_config *cfg;
	int32_t i;

	cl_git_pass(git_config_open_ondisk(&cfg, "config10"));
	cl_git_pass(git_config_set_int32(cfg, "section.tmp", 5));
	cl_git_pass(git_config_get_int32(&i, cfg, "section.tmp"));
	cl_assert(i == 5);
	cl_git_pass(git_config_delete_entry(cfg, "section.tmp"));
	git_config_free(cfg);
}
