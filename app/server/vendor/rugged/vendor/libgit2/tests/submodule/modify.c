#include "clar_libgit2.h"
#include "posix.h"
#include "path.h"
#include "submodule_helpers.h"

static git_repository *g_repo = NULL;

#define SM_LIBGIT2_URL "https://github.com/libgit2/libgit2.git"
#define SM_LIBGIT2     "sm_libgit2"

void test_submodule_modify__initialize(void)
{
	g_repo = setup_fixture_submod2();
}

static int delete_one_config(const git_config_entry *entry, void *payload)
{
	git_config *cfg = payload;
	return git_config_delete_entry(cfg, entry->name);
}

static int init_one_submodule(
	git_submodule *sm, const char *name, void *payload)
{
	GIT_UNUSED(name);
	GIT_UNUSED(payload);
	return git_submodule_init(sm, false);
}

void test_submodule_modify__init(void)
{
	git_config *cfg;
	const char *str;

	/* erase submodule data from .git/config */
	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_pass(
		git_config_foreach_match(cfg, "submodule\\..*", delete_one_config, cfg));
	git_config_free(cfg);

	/* confirm no submodule data in config */
	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_fail(git_config_get_string(&str, cfg, "submodule.sm_unchanged.url"));
	cl_git_fail(git_config_get_string(&str, cfg, "submodule.sm_changed_head.url"));
	cl_git_fail(git_config_get_string(&str, cfg, "submodule.sm_added_and_uncommited.url"));
	git_config_free(cfg);

	/* call init and see that settings are copied */
	cl_git_pass(git_submodule_foreach(g_repo, init_one_submodule, NULL));

	git_submodule_reload_all(g_repo, 1);

	/* confirm submodule data in config */
	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_pass(git_config_get_string(&str, cfg, "submodule.sm_unchanged.url"));
	cl_assert(git__suffixcmp(str, "/submod2_target") == 0);
	cl_git_pass(git_config_get_string(&str, cfg, "submodule.sm_changed_head.url"));
	cl_assert(git__suffixcmp(str, "/submod2_target") == 0);
	cl_git_pass(git_config_get_string(&str, cfg, "submodule.sm_added_and_uncommited.url"));
	cl_assert(git__suffixcmp(str, "/submod2_target") == 0);
	git_config_free(cfg);
}

static int sync_one_submodule(
	git_submodule *sm, const char *name, void *payload)
{
	GIT_UNUSED(name);
	GIT_UNUSED(payload);
	return git_submodule_sync(sm);
}

static void assert_submodule_url_is_synced(
	git_submodule *sm, const char *parent_key, const char *child_key)
{
	git_config *cfg;
	const char *str;
	git_repository *smrepo;

	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_pass(git_config_get_string(&str, cfg, parent_key));
	cl_assert_equal_s(git_submodule_url(sm), str);
	git_config_free(cfg);

	cl_git_pass(git_submodule_open(&smrepo, sm));
	cl_git_pass(git_repository_config(&cfg, smrepo));
	cl_git_pass(git_config_get_string(&str, cfg, child_key));
	cl_assert_equal_s(git_submodule_url(sm), str);
	git_config_free(cfg);
	git_repository_free(smrepo);
}

void test_submodule_modify__sync(void)
{
	git_submodule *sm1, *sm2, *sm3;
	git_config *cfg;
	const char *str;

#define SM1 "sm_unchanged"
#define SM2 "sm_changed_head"
#define SM3 "sm_added_and_uncommited"

	/* look up some submodules */
	cl_git_pass(git_submodule_lookup(&sm1, g_repo, SM1));
	cl_git_pass(git_submodule_lookup(&sm2, g_repo, SM2));
	cl_git_pass(git_submodule_lookup(&sm3, g_repo, SM3));

	/* At this point, the .git/config URLs for the submodules have
	 * not be rewritten with the absolute paths (although the
	 * .gitmodules have.  Let's confirm that they DO NOT match
	 * yet, then we can do a sync to make them match...
	 */

	/* check submodule info does not match before sync */
	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_pass(git_config_get_string(&str, cfg, "submodule."SM1".url"));
	cl_assert(strcmp(git_submodule_url(sm1), str) != 0);
	cl_git_pass(git_config_get_string(&str, cfg, "submodule."SM2".url"));
	cl_assert(strcmp(git_submodule_url(sm2), str) != 0);
	cl_git_pass(git_config_get_string(&str, cfg, "submodule."SM3".url"));
	cl_assert(strcmp(git_submodule_url(sm3), str) != 0);
	git_config_free(cfg);

	/* sync all the submodules */
	cl_git_pass(git_submodule_foreach(g_repo, sync_one_submodule, NULL));

	/* check that submodule config is updated */
	assert_submodule_url_is_synced(
		sm1, "submodule."SM1".url", "branch.origin.remote");
	assert_submodule_url_is_synced(
		sm2, "submodule."SM2".url", "branch.origin.remote");
	assert_submodule_url_is_synced(
		sm3, "submodule."SM3".url", "branch.origin.remote");

	git_submodule_free(sm1);
	git_submodule_free(sm2);
	git_submodule_free(sm3);
}

void test_submodule_modify__edit_and_save(void)
{
	git_submodule *sm1, *sm2;
	char *old_url;
	git_submodule_ignore_t old_ignore;
	git_submodule_update_t old_update;
	git_repository *r2;
	git_submodule_recurse_t old_fetchrecurse;

	cl_git_pass(git_submodule_lookup(&sm1, g_repo, "sm_changed_head"));

	old_url = git__strdup(git_submodule_url(sm1));

	/* modify properties of submodule */
	cl_git_pass(git_submodule_set_url(sm1, SM_LIBGIT2_URL));
	old_ignore = git_submodule_set_ignore(sm1, GIT_SUBMODULE_IGNORE_UNTRACKED);
	old_update = git_submodule_set_update(sm1, GIT_SUBMODULE_UPDATE_REBASE);
	old_fetchrecurse = git_submodule_set_fetch_recurse_submodules(
		sm1, GIT_SUBMODULE_RECURSE_YES);

	cl_assert_equal_s(SM_LIBGIT2_URL, git_submodule_url(sm1));
	cl_assert_equal_i(
		GIT_SUBMODULE_IGNORE_UNTRACKED, git_submodule_ignore(sm1));
	cl_assert_equal_i(
		GIT_SUBMODULE_UPDATE_REBASE, git_submodule_update(sm1));
	cl_assert_equal_i(
		GIT_SUBMODULE_RECURSE_YES, git_submodule_fetch_recurse_submodules(sm1));

	/* revert without saving (and confirm setters return old value) */
	cl_git_pass(git_submodule_set_url(sm1, old_url));
	cl_assert_equal_i(
		GIT_SUBMODULE_IGNORE_UNTRACKED,
		git_submodule_set_ignore(sm1, GIT_SUBMODULE_IGNORE_RESET));
	cl_assert_equal_i(
		GIT_SUBMODULE_UPDATE_REBASE,
		git_submodule_set_update(sm1, GIT_SUBMODULE_UPDATE_RESET));
	cl_assert_equal_i(
		GIT_SUBMODULE_RECURSE_YES, git_submodule_set_fetch_recurse_submodules(
			sm1, GIT_SUBMODULE_RECURSE_RESET));

	/* check that revert was successful */
	cl_assert_equal_s(old_url, git_submodule_url(sm1));
	cl_assert_equal_i((int)old_ignore, (int)git_submodule_ignore(sm1));
	cl_assert_equal_i((int)old_update, (int)git_submodule_update(sm1));
	cl_assert_equal_i(
		old_fetchrecurse, git_submodule_fetch_recurse_submodules(sm1));

	/* modify properties of submodule (again) */
	cl_git_pass(git_submodule_set_url(sm1, SM_LIBGIT2_URL));
	git_submodule_set_ignore(sm1, GIT_SUBMODULE_IGNORE_UNTRACKED);
	git_submodule_set_update(sm1, GIT_SUBMODULE_UPDATE_REBASE);
	git_submodule_set_fetch_recurse_submodules(sm1, GIT_SUBMODULE_RECURSE_YES);

	/* call save */
	cl_git_pass(git_submodule_save(sm1));

	/* attempt to "revert" values */
	git_submodule_set_ignore(sm1, GIT_SUBMODULE_IGNORE_RESET);
	git_submodule_set_update(sm1, GIT_SUBMODULE_UPDATE_RESET);

	/* but ignore and update should NOT revert because the RESET
	 * should now be the newly saved value...
	 */
	cl_assert_equal_i(
		(int)GIT_SUBMODULE_IGNORE_UNTRACKED, (int)git_submodule_ignore(sm1));
	cl_assert_equal_i(
		(int)GIT_SUBMODULE_UPDATE_REBASE, (int)git_submodule_update(sm1));
	cl_assert_equal_i(GIT_SUBMODULE_RECURSE_YES, git_submodule_fetch_recurse_submodules(sm1));

	/* call reload and check that the new values are loaded */
	cl_git_pass(git_submodule_reload(sm1, 0));

	cl_assert_equal_s(SM_LIBGIT2_URL, git_submodule_url(sm1));
	cl_assert_equal_i(
		(int)GIT_SUBMODULE_IGNORE_UNTRACKED, (int)git_submodule_ignore(sm1));
	cl_assert_equal_i(
		(int)GIT_SUBMODULE_UPDATE_REBASE, (int)git_submodule_update(sm1));
	cl_assert_equal_i(GIT_SUBMODULE_RECURSE_YES, git_submodule_fetch_recurse_submodules(sm1));

	/* open a second copy of the repo and compare submodule */
	cl_git_pass(git_repository_open(&r2, "submod2"));
	cl_git_pass(git_submodule_lookup(&sm2, r2, "sm_changed_head"));

	cl_assert_equal_s(SM_LIBGIT2_URL, git_submodule_url(sm2));
	cl_assert_equal_i(
		GIT_SUBMODULE_IGNORE_UNTRACKED, git_submodule_ignore(sm2));
	cl_assert_equal_i(
		GIT_SUBMODULE_UPDATE_REBASE, git_submodule_update(sm2));
	cl_assert_equal_i(
		GIT_SUBMODULE_RECURSE_NO, git_submodule_fetch_recurse_submodules(sm2));

	/* set fetchRecurseSubmodules on-demand */
	cl_git_pass(git_submodule_reload(sm1, 0));
	git_submodule_set_fetch_recurse_submodules(sm1, GIT_SUBMODULE_RECURSE_ONDEMAND);
	cl_assert_equal_i(
		GIT_SUBMODULE_RECURSE_ONDEMAND, git_submodule_fetch_recurse_submodules(sm1));
	/* call save */
	cl_git_pass(git_submodule_save(sm1));
	cl_git_pass(git_submodule_reload(sm1, 0));
	cl_assert_equal_i(
		GIT_SUBMODULE_RECURSE_ONDEMAND, git_submodule_fetch_recurse_submodules(sm1));

	git_submodule_free(sm1);
	git_submodule_free(sm2);
	git_repository_free(r2);
	git__free(old_url);
}
