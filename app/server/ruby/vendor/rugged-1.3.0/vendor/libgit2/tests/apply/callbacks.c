#include "clar_libgit2.h"
#include "apply_helpers.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-recursive"

void test_apply_callbacks__initialize(void)
{
	git_oid oid;
	git_commit *commit;

	repo = cl_git_sandbox_init(TEST_REPO_PATH);

	git_oid_fromstr(&oid, "539bd011c4822c560c1d17cab095006b7a10f707");
	cl_git_pass(git_commit_lookup(&commit, repo, &oid));
	cl_git_pass(git_reset(repo, (git_object *)commit, GIT_RESET_HARD, NULL));
	git_commit_free(commit);
}

void test_apply_callbacks__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static int delta_abort_cb(const git_diff_delta *delta, void *payload)
{
	GIT_UNUSED(payload);

	if (!strcmp(delta->old_file.path, "veal.txt"))
		return -99;

	return 0;
}

void test_apply_callbacks__delta_aborts(void)
{
	git_diff *diff;
	git_apply_options opts = GIT_APPLY_OPTIONS_INIT;

	opts.delta_cb = delta_abort_cb;

	cl_git_pass(git_diff_from_buffer(&diff,
		DIFF_MODIFY_TWO_FILES, strlen(DIFF_MODIFY_TWO_FILES)));
	cl_git_fail_with(-99,
		git_apply(repo, diff, GIT_APPLY_LOCATION_INDEX, &opts));

	validate_index_unchanged(repo);
	validate_workdir_unchanged(repo);

	git_diff_free(diff);
}

static int delta_skip_cb(const git_diff_delta *delta, void *payload)
{
	GIT_UNUSED(payload);

	if (!strcmp(delta->old_file.path, "asparagus.txt"))
		return 1;

	return 0;
}

void test_apply_callbacks__delta_can_skip(void)
{
	git_diff *diff;
	git_apply_options opts = GIT_APPLY_OPTIONS_INIT;

	struct merge_index_entry workdir_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};
	size_t workdir_expected_cnt = sizeof(workdir_expected) /
	    sizeof(struct merge_index_entry);

	opts.delta_cb = delta_skip_cb;

	cl_git_pass(git_diff_from_buffer(&diff,
		DIFF_MODIFY_TWO_FILES, strlen(DIFF_MODIFY_TWO_FILES)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_WORKDIR, &opts));

	validate_index_unchanged(repo);
	validate_apply_workdir(repo, workdir_expected, workdir_expected_cnt);

	git_diff_free(diff);
}

static int hunk_skip_odds_cb(const git_diff_hunk *hunk, void *payload)
{
	int *count = (int *)payload;
	GIT_UNUSED(hunk);

	return ((*count)++ % 2 == 1);
}

void test_apply_callbacks__hunk_can_skip(void)
{
	git_diff *diff;
	git_apply_options opts = GIT_APPLY_OPTIONS_INIT;
	int count = 0;

	struct merge_index_entry workdir_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "06f751b6ba4f017ddbf4248015768300268e092a", 0, "veal.txt" },
	};
	size_t workdir_expected_cnt = sizeof(workdir_expected) /
	    sizeof(struct merge_index_entry);

	opts.hunk_cb = hunk_skip_odds_cb;
	opts.payload = &count;

	cl_git_pass(git_diff_from_buffer(&diff,
		DIFF_MANY_CHANGES_ONE, strlen(DIFF_MANY_CHANGES_ONE)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_WORKDIR, &opts));

	validate_index_unchanged(repo);
	validate_apply_workdir(repo, workdir_expected, workdir_expected_cnt);

	git_diff_free(diff);
}
