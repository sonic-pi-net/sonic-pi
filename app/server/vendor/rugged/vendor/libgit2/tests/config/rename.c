#include "clar_libgit2.h"
#include "config.h"

static git_repository *g_repo = NULL;
static git_config *g_config = NULL;

void test_config_rename__initialize(void)
{
    g_repo = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_repository_config(&g_config, g_repo));
}

void test_config_rename__cleanup(void)
{
	git_config_free(g_config);
	g_config = NULL;

	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

void test_config_rename__can_rename(void)
{
	const git_config_entry *ce;

	cl_git_pass(git_config_get_entry(
		&ce, g_config, "branch.track-local.remote"));
	cl_assert_equal_s(".", ce->value);

	cl_git_fail(git_config_get_entry(
		&ce, g_config, "branch.local-track.remote"));

	cl_git_pass(git_config_rename_section(
		g_repo, "branch.track-local", "branch.local-track"));

	cl_git_pass(git_config_get_entry(
		&ce, g_config, "branch.local-track.remote"));
	cl_assert_equal_s(".", ce->value);

	cl_git_fail(git_config_get_entry(
		&ce, g_config, "branch.track-local.remote"));
}

void test_config_rename__prevent_overwrite(void)
{
	const git_config_entry *ce;

	cl_git_pass(git_config_set_string(
		g_config, "branch.local-track.remote", "yellow"));

	cl_git_pass(git_config_get_entry(
		&ce, g_config, "branch.local-track.remote"));
	cl_assert_equal_s("yellow", ce->value);

	cl_git_pass(git_config_rename_section(
		g_repo, "branch.track-local", "branch.local-track"));

	cl_git_pass(git_config_get_entry(
		&ce, g_config, "branch.local-track.remote"));
	cl_assert_equal_s(".", ce->value);

	/* so, we don't currently prevent overwrite... */
	/* {
		const git_error *err;
		cl_assert((err = giterr_last()) != NULL);
		cl_assert(err->message != NULL);
	} */
}

static void assert_invalid_config_section_name(
	git_repository *repo, const char *name)
{
	cl_git_fail_with(
		git_config_rename_section(repo, "branch.remoteless", name),
		GIT_EINVALIDSPEC);
}

void test_config_rename__require_a_valid_new_name(void)
{
	assert_invalid_config_section_name(g_repo, "");
	assert_invalid_config_section_name(g_repo, "bra\nch");
	assert_invalid_config_section_name(g_repo, "branc#");
	assert_invalid_config_section_name(g_repo, "bra\nch.duh");
	assert_invalid_config_section_name(g_repo, "branc#.duh");
}
