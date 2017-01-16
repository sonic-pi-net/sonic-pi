#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "git2/annotated_commit.h"
#include "git2/sys/index.h"
#include "merge.h"
#include "../merge_helpers.h"
#include "refs.h"
#include "posix.h"

static git_repository *repo;
static git_index *repo_index;

#define TEST_REPO_PATH "merge-resolve"
#define TEST_INDEX_PATH TEST_REPO_PATH "/.git/index"

#define UPTODATE_BRANCH			"master"
#define PREVIOUS_BRANCH			"previous"

#define FASTFORWARD_BRANCH		"ff_branch"
#define FASTFORWARD_ID			"fd89f8cffb663ac89095a0f9764902e93ceaca6a"

#define NOFASTFORWARD_BRANCH	"branch"
#define NOFASTFORWARD_ID		"7cb63eed597130ba4abb87b3e544b85021905520"


// Fixture setup and teardown
void test_merge_workdir_analysis__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
	git_repository_index(&repo_index, repo);
}

void test_merge_workdir_analysis__cleanup(void)
{
	git_index_free(repo_index);
	cl_git_sandbox_cleanup();
}

static void analysis_from_branch(
	git_merge_analysis_t *merge_analysis,
	git_merge_preference_t *merge_pref,
	const char *branchname)
{
	git_buf refname = GIT_BUF_INIT;
	git_reference *their_ref;
	git_annotated_commit *their_head;

	git_buf_printf(&refname, "%s%s", GIT_REFS_HEADS_DIR, branchname);

	cl_git_pass(git_reference_lookup(&their_ref, repo, git_buf_cstr(&refname)));
	cl_git_pass(git_annotated_commit_from_ref(&their_head, repo, their_ref));

	cl_git_pass(git_merge_analysis(merge_analysis, merge_pref, repo, (const git_annotated_commit **)&their_head, 1));

	git_buf_free(&refname);
	git_annotated_commit_free(their_head);
	git_reference_free(their_ref);
}

void test_merge_workdir_analysis__fastforward(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	analysis_from_branch(&merge_analysis, &merge_pref, FASTFORWARD_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_FASTFORWARD, (merge_analysis & GIT_MERGE_ANALYSIS_FASTFORWARD));
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_NORMAL, (merge_analysis & GIT_MERGE_ANALYSIS_NORMAL));
}

void test_merge_workdir_analysis__no_fastforward(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	analysis_from_branch(&merge_analysis, &merge_pref, NOFASTFORWARD_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_NORMAL, merge_analysis);
}

void test_merge_workdir_analysis__uptodate(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	analysis_from_branch(&merge_analysis, &merge_pref, UPTODATE_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_UP_TO_DATE, merge_analysis);
}

void test_merge_workdir_analysis__uptodate_merging_prev_commit(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	analysis_from_branch(&merge_analysis, &merge_pref, PREVIOUS_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_UP_TO_DATE, merge_analysis);
}

void test_merge_workdir_analysis__unborn(void)
{
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;
	git_buf master = GIT_BUF_INIT;

	git_buf_joinpath(&master, git_repository_path(repo), "refs/heads/master");
	p_unlink(git_buf_cstr(&master));

	analysis_from_branch(&merge_analysis, &merge_pref, NOFASTFORWARD_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_FASTFORWARD, (merge_analysis & GIT_MERGE_ANALYSIS_FASTFORWARD));
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_UNBORN, (merge_analysis & GIT_MERGE_ANALYSIS_UNBORN));

	git_buf_free(&master);
}

void test_merge_workdir_analysis__fastforward_with_config_noff(void)
{
	git_config *config;
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	git_repository_config(&config, repo);
	git_config_set_string(config, "merge.ff", "false");

	analysis_from_branch(&merge_analysis, &merge_pref, FASTFORWARD_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_FASTFORWARD, (merge_analysis & GIT_MERGE_ANALYSIS_FASTFORWARD));
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_NORMAL, (merge_analysis & GIT_MERGE_ANALYSIS_NORMAL));
	cl_assert_equal_i(GIT_MERGE_PREFERENCE_NO_FASTFORWARD, (merge_pref & GIT_MERGE_PREFERENCE_NO_FASTFORWARD));
}

void test_merge_workdir_analysis__no_fastforward_with_config_ffonly(void)
{
	git_config *config;
	git_merge_analysis_t merge_analysis;
	git_merge_preference_t merge_pref;

	git_repository_config(&config, repo);
	git_config_set_string(config, "merge.ff", "only");

	analysis_from_branch(&merge_analysis, &merge_pref, NOFASTFORWARD_BRANCH);
	cl_assert_equal_i(GIT_MERGE_ANALYSIS_NORMAL, (merge_analysis & GIT_MERGE_ANALYSIS_NORMAL));
	cl_assert_equal_i(GIT_MERGE_PREFERENCE_FASTFORWARD_ONLY, (merge_pref & GIT_MERGE_PREFERENCE_FASTFORWARD_ONLY));
}
