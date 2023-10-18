#include "clar_libgit2.h"
#include "config_backend.h"
#include "config.h"
#include "path.h"

static git_config *cfg;

void test_config_readonly__initialize(void)
{
	cl_git_pass(git_config_new(&cfg));
}

void test_config_readonly__cleanup(void)
{
	git_config_free(cfg);
	cfg = NULL;
}

void test_config_readonly__writing_to_readonly_fails(void)
{
	git_config_backend *backend;

	cl_git_pass(git_config_backend_from_file(&backend, "global"));
	backend->readonly = 1;
	cl_git_pass(git_config_add_backend(cfg, backend, GIT_CONFIG_LEVEL_GLOBAL, NULL, 0));

	cl_git_fail_with(GIT_ENOTFOUND, git_config_set_string(cfg, "foo.bar", "baz"));
	cl_assert(!git_fs_path_exists("global"));
}

void test_config_readonly__writing_to_cfg_with_rw_precedence_succeeds(void)
{
	git_config_backend *backend;

	cl_git_pass(git_config_backend_from_file(&backend, "global"));
	backend->readonly = 1;
	cl_git_pass(git_config_add_backend(cfg, backend, GIT_CONFIG_LEVEL_GLOBAL, NULL, 0));

	cl_git_pass(git_config_backend_from_file(&backend, "local"));
	cl_git_pass(git_config_add_backend(cfg, backend, GIT_CONFIG_LEVEL_LOCAL, NULL, 0));

	cl_git_pass(git_config_set_string(cfg, "foo.bar", "baz"));

	cl_assert(git_fs_path_exists("local"));
	cl_assert(!git_fs_path_exists("global"));
	cl_git_pass(p_unlink("local"));
}

void test_config_readonly__writing_to_cfg_with_ro_precedence_succeeds(void)
{
	git_config_backend *backend;

	cl_git_pass(git_config_backend_from_file(&backend, "local"));
	backend->readonly = 1;
	cl_git_pass(git_config_add_backend(cfg, backend, GIT_CONFIG_LEVEL_LOCAL, NULL, 0));

	cl_git_pass(git_config_backend_from_file(&backend, "global"));
	cl_git_pass(git_config_add_backend(cfg, backend, GIT_CONFIG_LEVEL_GLOBAL, NULL, 0));

	cl_git_pass(git_config_set_string(cfg, "foo.bar", "baz"));

	cl_assert(!git_fs_path_exists("local"));
	cl_assert(git_fs_path_exists("global"));
	cl_git_pass(p_unlink("global"));
}
