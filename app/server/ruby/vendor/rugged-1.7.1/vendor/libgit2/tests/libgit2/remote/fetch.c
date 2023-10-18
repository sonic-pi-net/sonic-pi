#include "clar_libgit2.h"

#include "remote.h"
#include "repository.h"

static git_repository *repo1;
static git_repository *repo2;
static char* repo1_path;
static char* repo2_path;

static const char *REPO1_REFNAME = "refs/heads/main";
static const char *REPO2_REFNAME = "refs/remotes/repo1/main";
static char *FORCE_FETCHSPEC = "+refs/heads/main:refs/remotes/repo1/main";
static char *NON_FORCE_FETCHSPEC = "refs/heads/main:refs/remotes/repo1/main";

void test_remote_fetch__initialize(void) {
	git_config *c;
	git_str repo1_path_buf = GIT_STR_INIT;
	git_str repo2_path_buf = GIT_STR_INIT;
	const char *sandbox = clar_sandbox_path();

	cl_git_pass(git_str_joinpath(&repo1_path_buf, sandbox, "fetchtest_repo1"));
	repo1_path = git_str_detach(&repo1_path_buf);
	cl_git_pass(git_repository_init(&repo1, repo1_path, true));

	cl_git_pass(git_str_joinpath(&repo2_path_buf, sandbox, "fetchtest_repo2"));
	repo2_path = git_str_detach(&repo2_path_buf);
	cl_git_pass(git_repository_init(&repo2, repo2_path, true));

	cl_git_pass(git_repository_config(&c, repo1));
	cl_git_pass(git_config_set_string(c, "user.email", "some@email"));
	cl_git_pass(git_config_set_string(c, "user.name", "some@name"));
	git_config_free(c);
	git_str_dispose(&repo1_path_buf);
	git_str_dispose(&repo2_path_buf);
}

void test_remote_fetch__cleanup(void) {
	git_repository_free(repo1);
	git_repository_free(repo2);

	cl_git_pass(git_futils_rmdir_r(repo1_path, NULL, GIT_RMDIR_REMOVE_FILES));
	free(repo1_path);

	cl_git_pass(git_futils_rmdir_r(repo2_path, NULL, GIT_RMDIR_REMOVE_FILES));
	free(repo2_path);
}


/**
 * This checks that the '+' flag on fetchspecs is respected. We create a
 * repository that has a reference to two commits, one a child of the other.
 * We fetch this repository into a second repository. Then we reset the
 * reference in the first repository and run the fetch again. If the '+' flag
 * is used then the reference in the second repository will change, but if it
 * is not then it should stay the same.
 *
 * @param commit1id A pointer to an OID which will be populated with the first
 *                  commit.
 * @param commit2id A pointer to an OID which will be populated with the second
 *                  commit, which is a descendant of the first.
 * @param force     Whether to use a spec with '+' prefixed to force the refs
 *                  to update
 */
static void do_time_travelling_fetch(git_oid *commit1id, git_oid *commit2id,
		bool force) {
	char *refspec_strs = {
		force ? FORCE_FETCHSPEC : NON_FORCE_FETCHSPEC,
	};
	git_strarray refspecs = {
		.count = 1,
		.strings = &refspec_strs,
	};

	/* create two commits in repo 1 and a reference to them */
	{
		git_oid empty_tree_id;
		git_tree *empty_tree;
		git_signature *sig;
		git_treebuilder *tb;
		cl_git_pass(git_treebuilder_new(&tb, repo1, NULL));
		cl_git_pass(git_treebuilder_write(&empty_tree_id, tb));
		cl_git_pass(git_tree_lookup(&empty_tree, repo1, &empty_tree_id));
		cl_git_pass(git_signature_default(&sig, repo1));
		cl_git_pass(git_commit_create(commit1id, repo1, REPO1_REFNAME, sig,
					sig, NULL, "one", empty_tree, 0, NULL));
		cl_git_pass(git_commit_create_v(commit2id, repo1, REPO1_REFNAME, sig,
					sig, NULL, "two", empty_tree, 1, commit1id));

		git_tree_free(empty_tree);
		git_signature_free(sig);
		git_treebuilder_free(tb);
	}

	/* fetch the reference via the remote */
	{
		git_remote *remote;

		cl_git_pass(git_remote_create_anonymous(&remote, repo2,
					git_repository_path(repo1)));
		cl_git_pass(git_remote_fetch(remote, &refspecs, NULL, "some message"));

		git_remote_free(remote);
	}

	/* assert that repo2 references the second commit */
	{
		const git_oid *target;
		git_reference *ref;
		cl_git_pass(git_reference_lookup(&ref, repo2, REPO2_REFNAME));
		target = git_reference_target(ref);
		cl_assert_equal_b(git_oid_cmp(target, commit2id), 0);
		git_reference_free(ref);
	}

	/* set the reference in repo1 to point to the older commit */
	{
		git_reference *ref;
		git_reference *ref2;
		cl_git_pass(git_reference_lookup(&ref, repo1, REPO1_REFNAME));
		cl_git_pass(git_reference_set_target(&ref2, ref, commit1id,
					"rollback"));
		git_reference_free(ref);
		git_reference_free(ref2);
	}

	/* fetch the reference again */
	{
		git_remote *remote;

		cl_git_pass(git_remote_create_anonymous(&remote, repo2,
					git_repository_path(repo1)));
		cl_git_pass(git_remote_fetch(remote, &refspecs, NULL, "some message"));

		git_remote_free(remote);
	}
}

void test_remote_fetch__dont_update_refs_if_not_descendant_and_not_force(void) {
	const git_oid *target;
	git_oid commit1id;
	git_oid commit2id;
	git_reference *ref;

	do_time_travelling_fetch(&commit1id, &commit2id, false);

	/* assert that the reference in repo2 has not changed */
	cl_git_pass(git_reference_lookup(&ref, repo2, REPO2_REFNAME));
	target = git_reference_target(ref);
	cl_assert_equal_b(git_oid_cmp(target, &commit2id), 0);

	git_reference_free(ref);
}

void test_remote_fetch__do_update_refs_if_not_descendant_and_force(void) {
	const git_oid *target;
	git_oid commit1id;
	git_oid commit2id;
	git_reference *ref;

	do_time_travelling_fetch(&commit1id, &commit2id, true);

	/* assert that the reference in repo2 has changed */
	cl_git_pass(git_reference_lookup(&ref, repo2, REPO2_REFNAME));
	target = git_reference_target(ref);
	cl_assert_equal_b(git_oid_cmp(target, &commit1id), 0);

	git_reference_free(ref);
}
