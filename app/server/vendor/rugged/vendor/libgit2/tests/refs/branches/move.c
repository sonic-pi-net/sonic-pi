#include "clar_libgit2.h"
#include "refs.h"
#include "config/config_helpers.h"

static git_repository *repo;

void test_refs_branches_move__initialize(void)
{
	repo = cl_git_sandbox_init("testrepo.git");
}

void test_refs_branches_move__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

#define NEW_BRANCH_NAME "new-branch-on-the-block"

void test_refs_branches_move__can_move_a_local_branch(void)
{
	git_reference *original_ref, *new_ref;

	cl_git_pass(git_reference_lookup(&original_ref, repo, "refs/heads/br2"));

	cl_git_pass(git_branch_move(&new_ref, original_ref, NEW_BRANCH_NAME, 0, NULL, NULL));
	cl_assert_equal_s(GIT_REFS_HEADS_DIR NEW_BRANCH_NAME, git_reference_name(new_ref));

	git_reference_free(original_ref);
	git_reference_free(new_ref);
}

void test_refs_branches_move__can_move_a_local_branch_to_a_different_namespace(void)
{
	git_reference *original_ref, *new_ref, *newer_ref;

	cl_git_pass(git_reference_lookup(&original_ref, repo, "refs/heads/br2"));

	/* Downward */
	cl_git_pass(git_branch_move(&new_ref, original_ref, "somewhere/" NEW_BRANCH_NAME, 0, NULL, NULL));
	git_reference_free(original_ref);

	/* Upward */
	cl_git_pass(git_branch_move(&newer_ref, new_ref, "br2", 0, NULL, NULL));
	git_reference_free(new_ref);

	git_reference_free(newer_ref);
}

void test_refs_branches_move__can_move_a_local_branch_to_a_partially_colliding_namespace(void)
{
	git_reference *original_ref, *new_ref, *newer_ref;

	cl_git_pass(git_reference_lookup(&original_ref, repo, "refs/heads/br2"));

	/* Downward */
	cl_git_pass(git_branch_move(&new_ref, original_ref, "br2/" NEW_BRANCH_NAME, 0, NULL, NULL));
	git_reference_free(original_ref);

	/* Upward */
	cl_git_pass(git_branch_move(&newer_ref, new_ref, "br2", 0, NULL, NULL));
	git_reference_free(new_ref);

	git_reference_free(newer_ref);
}

void test_refs_branches_move__can_not_move_a_branch_if_its_destination_name_collide_with_an_existing_one(void)
{
	git_reference *original_ref, *new_ref;
	git_config *config;
	const git_config_entry *ce;
	char *original_remote, *original_merge;

	cl_git_pass(git_repository_config(&config, repo));

	cl_git_pass(git_config_get_entry(&ce, config, "branch.master.remote"));
	original_remote = strdup(ce->value);
	cl_git_pass(git_config_get_entry(&ce, config, "branch.master.merge"));
	original_merge  = strdup(ce->value);


	cl_git_pass(git_reference_lookup(&original_ref, repo, "refs/heads/br2"));

	cl_assert_equal_i(GIT_EEXISTS,
		git_branch_move(&new_ref, original_ref, "master", 0, NULL, NULL));

	cl_assert(giterr_last()->message != NULL);
	cl_git_pass(git_config_get_entry(&ce, config, "branch.master.remote"));
	cl_assert_equal_s(original_remote, ce->value);
	cl_git_pass(git_config_get_entry(&ce, config, "branch.master.merge"));
	cl_assert_equal_s(original_merge,  ce->value);


	cl_assert_equal_i(GIT_EEXISTS,
		git_branch_move(&new_ref, original_ref, "cannot-fetch", 0, NULL, NULL));

	cl_assert(giterr_last()->message != NULL);
	cl_git_pass(git_config_get_entry(&ce, config, "branch.master.remote"));
	cl_assert_equal_s(original_remote, ce->value);
	cl_git_pass(git_config_get_entry(&ce, config, "branch.master.merge"));
	cl_assert_equal_s(original_merge,  ce->value);

	git_reference_free(original_ref);
	cl_git_pass(git_reference_lookup(&original_ref, repo, "refs/heads/track-local"));

	cl_assert_equal_i(GIT_EEXISTS,
		git_branch_move(&new_ref, original_ref, "master", 0, NULL, NULL));

	cl_assert(giterr_last()->message != NULL);
	cl_git_pass(git_config_get_entry(&ce, config, "branch.master.remote"));
	cl_assert_equal_s(original_remote, ce->value);
	cl_git_pass(git_config_get_entry(&ce, config, "branch.master.merge"));
	cl_assert_equal_s(original_merge,  ce->value);

	free(original_remote); free(original_merge);
	git_reference_free(original_ref);
	git_config_free(config);
}

void test_refs_branches_move__moving_a_branch_with_an_invalid_name_returns_EINVALIDSPEC(void)
{
	git_reference *original_ref, *new_ref;

	cl_git_pass(git_reference_lookup(&original_ref, repo, "refs/heads/br2"));

	cl_assert_equal_i(GIT_EINVALIDSPEC, git_branch_move(&new_ref, original_ref, "Inv@{id", 0, NULL, NULL));

	git_reference_free(original_ref);
}

void test_refs_branches_move__can_not_move_a_non_branch(void)
{
	git_reference *tag, *new_ref;

	cl_git_pass(git_reference_lookup(&tag, repo, "refs/tags/e90810b"));
	cl_git_fail(git_branch_move(&new_ref, tag, NEW_BRANCH_NAME, 0, NULL, NULL));

	git_reference_free(tag);
}

void test_refs_branches_move__can_force_move_over_an_existing_branch(void)
{
	git_reference *original_ref, *new_ref;

	cl_git_pass(git_reference_lookup(&original_ref, repo, "refs/heads/br2"));

	cl_git_pass(git_branch_move(&new_ref, original_ref, "master", 1, NULL, NULL));

	git_reference_free(original_ref);
	git_reference_free(new_ref);
}

void test_refs_branches_move__moving_a_branch_moves_related_configuration_data(void)
{
	git_reference *branch;
	git_reference *new_branch;

	cl_git_pass(git_branch_lookup(&branch, repo, "track-local", GIT_BRANCH_LOCAL));

	assert_config_entry_existence(repo, "branch.track-local.remote", true);
	assert_config_entry_existence(repo, "branch.track-local.merge", true);
	assert_config_entry_existence(repo, "branch.moved.remote", false);
	assert_config_entry_existence(repo, "branch.moved.merge", false);

	cl_git_pass(git_branch_move(&new_branch, branch, "moved", 0, NULL, NULL));
	git_reference_free(branch);

	assert_config_entry_existence(repo, "branch.track-local.remote", false);
	assert_config_entry_existence(repo, "branch.track-local.merge", false);
	assert_config_entry_existence(repo, "branch.moved.remote", true);
	assert_config_entry_existence(repo, "branch.moved.merge", true);

	git_reference_free(new_branch);
}

void test_refs_branches_move__moving_the_branch_pointed_at_by_HEAD_updates_HEAD(void)
{
	git_reference *branch;
	git_reference *new_branch;

	cl_git_pass(git_reference_lookup(&branch, repo, "refs/heads/master"));
	cl_git_pass(git_branch_move(&new_branch, branch, "master2", 0, NULL, NULL));
	git_reference_free(branch);
	git_reference_free(new_branch);

	cl_git_pass(git_repository_head(&branch, repo));
	cl_assert_equal_s("refs/heads/master2", git_reference_name(branch));
	git_reference_free(branch);
}

void test_refs_branches_move__updates_the_reflog(void)
{
	git_reference *branch;
	git_reference *new_branch;
	git_reflog *log;
	const git_reflog_entry *entry;
	git_signature *sig;

	cl_git_pass(git_signature_now(&sig, "me", "foo@example.com"));

	cl_git_pass(git_reference_lookup(&branch, repo, "refs/heads/master"));
	cl_git_pass(git_branch_move(&new_branch, branch, "master2", 0, sig, "message"));

	cl_git_pass(git_reflog_read(&log, repo, git_reference_name(new_branch)));
	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s("message", git_reflog_entry_message(entry));
	cl_assert_equal_s("foo@example.com", git_reflog_entry_committer(entry)->email);

	git_reference_free(branch);
	git_reference_free(new_branch);
	git_reflog_free(log);
	git_signature_free(sig);
}

void test_refs_branches_move__default_reflog_message(void)
{
	git_reference *branch;
	git_reference *new_branch;
	git_reflog *log;
	const git_reflog_entry *entry;
	git_signature *sig;
	git_config *cfg;

	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_set_string(cfg, "user.name", "Foo Bar"));
	cl_git_pass(git_config_set_string(cfg, "user.email", "foo@example.com"));
	git_config_free(cfg);

	cl_git_pass(git_signature_default(&sig, repo));

	cl_git_pass(git_reference_lookup(&branch, repo, "refs/heads/master"));
	cl_git_pass(git_branch_move(&new_branch, branch, "master2", 0, NULL, NULL));

	cl_git_pass(git_reflog_read(&log, repo, git_reference_name(new_branch)));
	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s("Branch: renamed refs/heads/master to refs/heads/master2",
			git_reflog_entry_message(entry));
	cl_assert_equal_s(sig->email, git_reflog_entry_committer(entry)->email);

	git_reference_free(branch);
	git_reference_free(new_branch);
	git_reflog_free(log);
	git_signature_free(sig);
}
