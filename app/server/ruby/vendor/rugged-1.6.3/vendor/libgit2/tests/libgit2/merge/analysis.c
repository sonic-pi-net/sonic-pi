/*
NOTE: this is the implementation for both merge/trees/analysis.c and merge/workdir/analysis.c
You probably want to make changes to both files.
*/

#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "git2/annotated_commit.h"
#include "git2/sys/index.h"
#include "merge.h"
#include "merge_helpers.h"
#include "refs.h"
#include "posix.h"

#define TEST_REPO_PATH "merge-resolve"

#define UPTODATE_BRANCH         "master"
#define PREVIOUS_BRANCH         "previous"

#define FASTFORWARD_BRANCH      "ff_branch"
#define FASTFORWARD_ID          "fd89f8cffb663ac89095a0f9764902e93ceaca6a"

#define NOFASTFORWARD_BRANCH    "branch"
#define NOFASTFORWARD_ID        "7cb63eed597130ba4abb87b3e544b85021905520"

static git_repository *sandbox;
static git_repository *repo;

void test_merge_analysis__initialize_with_bare_repository(void)
{
	sandbox = cl_git_sandbox_init(TEST_REPO_PATH);
	cl_git_pass(git_repository_open_ext(&repo, git_repository_path(sandbox),
					    GIT_REPOSITORY_OPEN_BARE, NULL));
}

void test_merge_analysis__initialize_with_nonbare_repository(void)
{
	sandbox = cl_git_sandbox_init(TEST_REPO_PATH);
	cl_git_pass(git_repository_open_ext(&repo, git_repository_workdir(sandbox),
					    0, NULL));
}

void test_merge_analysis__cleanup(void)
{
	git_repository_free(repo);
	cl_git_sandbox_cleanup();
}

static void analysis_from_branch(
	git_merge_analysis_t *merge_analysis,
	git_merge_preference_t *merge_pref,
	const char *our_branchname,
	const char *their_branchname)
{
	git_str our_refname = GIT_STR_INIT;
	git_str their_refname = GIT_STR_INIT;
	git_reference *our_ref;
	git_reference *their_ref;
	git_annotated_commit *their_head;

	if (our_branchname != NULL) {
		cl_git_pass(git_str_printf(&our_refname, "%s%s", GIT_REFS_HEADS_DIR, our_branchname));
		cl_git_pass(git_reference_lookup(&our_ref, repo, git_str_cstr(&our_refname)));
	} else {
		cl_git_pass(git_reference_lookup(&our_ref, repo, GIT_HEAD_FILE));
	}

	cl_git_pass(git_str_printf(&their_refname, "%s%s", GIT_REFS_HEADS_DIR, their_branchname));

	cl_git_pass(git_reference_lookup(&their_ref, repo, git_str_cstr(&their_refname)));
	cl_git_pass(git_annotated_commit_from_ref(&their_head, repo, their_ref));

	cl_git_pass(git_merge_analysis_for_ref(merge_analysis, merge_pref, repo, our_ref, (const git_annotated_commit **)&their_head, 1));

	git_str_dispose(&our_refname);
	git_str_dispose(&their_refname);
	git_annotated_commit_free(their_head);
	git_reference_free(our_ref);
	git_reference_free(their_ref);
}

void test_merge_analysis__fastforward(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	analysis_from_branch(&merge_analysis, &merge_pref, NULL, FASTFORWARD_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_NORMAL|GIT_MERGE_ANALYSIS_FASTFORWARD, merge_analysis);
}

void test_merge_analysis__no_fastforward(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	analysis_from_branch(&merge_analysis, &merge_pref, NULL, NOFASTFORWARD_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_NORMAL, merge_analysis);
}

void test_merge_analysis__uptodate(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	analysis_from_branch(&merge_analysis, &merge_pref, NULL, UPTODATE_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_UP_TO_DATE, merge_analysis);
}

void test_merge_analysis__uptodate_merging_prev_commit(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	analysis_from_branch(&merge_analysis, &merge_pref, NULL, PREVIOUS_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_UP_TO_DATE, merge_analysis);
}

void test_merge_analysis__unborn(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;
	git_str master = GIT_STR_INIT;

	cl_git_pass(git_str_joinpath(&master, git_repository_path(repo), "refs/heads/master"));
	cl_must_pass(p_unlink(git_str_cstr(&master)));

	analysis_from_branch(&merge_analysis, &merge_pref, NULL, NOFASTFORWARD_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_FASTFORWARD|GIT_MERGE_ANALYSIS_UNBORN, merge_analysis);

	git_str_dispose(&master);
}

void test_merge_analysis__fastforward_with_config_noff(void)
{
	git_config *config;
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_string(config, "merge.ff", "false"));

	analysis_from_branch(&merge_analysis, &merge_pref, NULL, FASTFORWARD_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_NORMAL|GIT_MERGE_ANALYSIS_FASTFORWARD, merge_analysis);

	cl_assert_equal_i(GIT_MERGE_PREFERENCE_NO_FASTFORWARD, (merge_pref & GIT_MERGE_PREFERENCE_NO_FASTFORWARD));

	git_config_free(config);
}

void test_merge_analysis__no_fastforward_with_config_ffonly(void)
{
	git_config *config;
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_string(config, "merge.ff", "only"));

	analysis_from_branch(&merge_analysis, &merge_pref, NULL, NOFASTFORWARD_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_NORMAL, merge_analysis);

	cl_assert_equal_i(GIT_MERGE_PREFERENCE_FASTFORWARD_ONLY, (merge_pref & GIT_MERGE_PREFERENCE_FASTFORWARD_ONLY));

	git_config_free(config);
}

void test_merge_analysis__between_uptodate_refs(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	analysis_from_branch(&merge_analysis, &merge_pref, NOFASTFORWARD_BRANCH, PREVIOUS_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_UP_TO_DATE, merge_analysis);
}

void test_merge_analysis__between_noff_refs(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	analysis_from_branch(&merge_analysis, &merge_pref, "branch", FASTFORWARD_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_NORMAL, merge_analysis);
}
