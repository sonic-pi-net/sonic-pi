#include "clar_libgit2.h"
#include "apply_helpers.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-recursive"

void test_apply_both__initialize(void)
{
	git_oid oid;
	git_commit *commit;

	repo = cl_git_sandbox_init(TEST_REPO_PATH);

	git_oid_fromstr(&oid, "539bd011c4822c560c1d17cab095006b7a10f707");
	cl_git_pass(git_commit_lookup(&commit, repo, &oid));
	cl_git_pass(git_reset(repo, (git_object *)commit, GIT_RESET_HARD, NULL));
	git_commit_free(commit);
}

void test_apply_both__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_apply_both__generated_diff(void)
{
	git_oid a_oid, b_oid;
	git_commit *a_commit, *b_commit;
	git_tree *a_tree, *b_tree;
	git_diff *diff;
	git_diff_options diff_opts = GIT_DIFF_OPTIONS_INIT;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	git_oid_fromstr(&a_oid, "539bd011c4822c560c1d17cab095006b7a10f707");
	git_oid_fromstr(&b_oid, "7c7bf85e978f1d18c0566f702d2cb7766b9c8d4f");
	cl_git_pass(git_commit_lookup(&a_commit, repo, &a_oid));
	cl_git_pass(git_commit_lookup(&b_commit, repo, &b_oid));

	cl_git_pass(git_commit_tree(&a_tree, a_commit));
	cl_git_pass(git_commit_tree(&b_tree, b_commit));

	cl_git_pass(git_diff_tree_to_tree(&diff, repo, a_tree, b_tree, &diff_opts));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
	git_tree_free(a_tree);
	git_tree_free(b_tree);
	git_commit_free(a_commit);
	git_commit_free(b_commit);
}

void test_apply_both__parsed_diff(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff,
		DIFF_MODIFY_TWO_FILES, strlen(DIFF_MODIFY_TWO_FILES)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__removes_file(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" },
	};
	size_t both_expected_cnt = sizeof(both_expected) /
	    sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_DELETE_FILE,
		strlen(DIFF_DELETE_FILE)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__adds_file(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "6370543fcfedb3e6516ec53b06158f3687dc1447", 0, "newfile.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" },
	};
	size_t both_expected_cnt = sizeof(both_expected) /
	    sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff,
		DIFF_ADD_FILE, strlen(DIFF_ADD_FILE)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__application_failure_leaves_index_unmodified(void)
{
	git_diff *diff;
	git_index *index;

	const char *diff_file = DIFF_MODIFY_TWO_FILES;

	struct merge_index_entry index_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
	};
	size_t index_expected_cnt = sizeof(index_expected) /
		sizeof(struct merge_index_entry);

	/* mutate the index */
	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_remove(index, "veal.txt", 0));
	cl_git_pass(git_index_write(index));
	git_index_free(index);

	cl_git_pass(git_diff_from_buffer(&diff, diff_file, strlen(diff_file)));
	cl_git_fail_with(GIT_EAPPLYFAIL, git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, index_expected, index_expected_cnt);
	validate_workdir_unchanged(repo);

	git_diff_free(diff);
}

void test_apply_both__index_must_match_workdir(void)
{
	git_diff *diff;
	git_index *index;
	git_index_entry idx_entry;

	const char *diff_file = DIFF_MODIFY_TWO_FILES;

	/*
	 * Append a line to the end of the file in both the index and the
	 * working directory.  Although the appended line would allow for
	 * patch application in each, the line appended is different in
	 * each, so the application should not be allowed.
	 */
	cl_git_append2file("merge-recursive/asparagus.txt",
	    "This is a modification.\n");

	cl_git_pass(git_repository_index(&index, repo));

	memset(&idx_entry, 0, sizeof(git_index_entry));
	idx_entry.mode = 0100644;
	idx_entry.path = "asparagus.txt";
	cl_git_pass(git_oid_fromstr(&idx_entry.id, "06d3fefb8726ab1099acc76e02dfb85e034b2538"));
	cl_git_pass(git_index_add(index, &idx_entry));

	cl_git_pass(git_index_write(index));
	git_index_free(index);

	cl_git_pass(git_diff_from_buffer(&diff, diff_file, strlen(diff_file)));
	cl_git_fail_with(GIT_EAPPLYFAIL, git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	git_diff_free(diff);
}

void test_apply_both__index_mode_must_match_workdir(void)
{
	git_diff *diff;

	if (!cl_is_chmod_supported())
		clar__skip();

	/* Set a file in the working directory executable. */
	cl_must_pass(p_chmod("merge-recursive/asparagus.txt", 0755));

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_MODIFY_TWO_FILES,
	    strlen(DIFF_MODIFY_TWO_FILES)));
	cl_git_fail_with(GIT_EAPPLYFAIL, git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	git_diff_free(diff);
}

void test_apply_both__application_failure_leaves_workdir_unmodified(void)
{
	git_diff *diff;
	git_index *index;

	const char *diff_file = DIFF_MODIFY_TWO_FILES;

	struct merge_index_entry workdir_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "8684724651336001c5dbce74bed6736d2443958d", 0, "veal.txt" },
	};
	size_t workdir_expected_cnt = sizeof(workdir_expected) /
		sizeof(struct merge_index_entry);

	/* mutate the workdir */
	cl_git_rewritefile("merge-recursive/veal.txt",
	    "This is a modification.\n");

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, "veal.txt"));
	cl_git_pass(git_index_write(index));
	git_index_free(index);

	cl_git_pass(git_diff_from_buffer(&diff, diff_file, strlen(diff_file)));
	cl_git_fail_with(GIT_EAPPLYFAIL, git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_workdir(repo, workdir_expected, workdir_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__keeps_nonconflicting_changes(void)
{
	git_diff *diff;
	git_index *index;
	git_index_entry idx_entry;

	const char *diff_file = DIFF_MODIFY_TWO_FILES;

	struct merge_index_entry index_expected[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "898d12687fb35be271c27c795a6b32c8b51da79e", 0, "beef.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};
	size_t index_expected_cnt = sizeof(index_expected) /
		sizeof(struct merge_index_entry);

	struct merge_index_entry workdir_expected[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "f75ba05f340c51065cbea2e1fdbfe5fe13144c97", 0, "gravy.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};
	size_t workdir_expected_cnt = sizeof(workdir_expected) /
		sizeof(struct merge_index_entry);

	/* mutate the index */
	cl_git_pass(git_repository_index(&index, repo));

	memset(&idx_entry, 0, sizeof(git_index_entry));
	idx_entry.mode = 0100644;
	idx_entry.path = "beef.txt";
	cl_git_pass(git_oid_fromstr(&idx_entry.id, "898d12687fb35be271c27c795a6b32c8b51da79e"));
	cl_git_pass(git_index_add(index, &idx_entry));

	cl_git_pass(git_index_remove(index, "bouilli.txt", 0));
	cl_git_pass(git_index_write(index));
	git_index_free(index);

	/* and mutate the working directory */
	cl_git_rmfile("merge-recursive/oyster.txt");
	cl_git_rewritefile("merge-recursive/gravy.txt", "Hello, world.\n");

	cl_git_pass(git_diff_from_buffer(&diff, diff_file, strlen(diff_file)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, index_expected, index_expected_cnt);
	validate_apply_workdir(repo, workdir_expected, workdir_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__can_apply_nonconflicting_file_changes(void)
{
	git_diff *diff;
	git_index *index;

	const char *diff_file = DIFF_MODIFY_TWO_FILES;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "f8a701c8a1a22c1729ee50faff1111f2d64f96fc", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	/*
	 * Replace the workdir file with a version that is different than
	 * HEAD but such that the patch still applies cleanly.  This item
	 * has a new line appended.
	 */
	cl_git_append2file("merge-recursive/asparagus.txt",
	    "This line is added in the index and the workdir.\n");

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, "asparagus.txt"));
	cl_git_pass(git_index_write(index));
	git_index_free(index);

	cl_git_pass(git_diff_from_buffer(&diff, diff_file, strlen(diff_file)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__honors_crlf_attributes(void)
{
	git_diff *diff;
	git_oid oid;
	git_commit *commit;

	const char *diff_file = DIFF_MODIFY_TWO_FILES;

	struct merge_index_entry index_expected[] = {
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};
	size_t index_expected_cnt = sizeof(index_expected) /
		sizeof(struct merge_index_entry);

	struct merge_index_entry workdir_expected[] = {
		{ 0100644, "176a458f94e0ea5272ce67c36bf30b6be9caf623", 0, ".gitattributes" },
		{ 0100644, "ffb36e513f5fdf8a6ba850a20142676a2ac4807d", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "a7b066537e6be7109abfe4ff97b675d4e077da20", 0, "veal.txt" },
	};
	size_t workdir_expected_cnt = sizeof(workdir_expected) /
		sizeof(struct merge_index_entry);

	cl_git_mkfile("merge-recursive/.gitattributes", "* text=auto\n");

	cl_git_rmfile("merge-recursive/asparagus.txt");
	cl_git_rmfile("merge-recursive/veal.txt");

	git_oid_fromstr(&oid, "539bd011c4822c560c1d17cab095006b7a10f707");
	cl_git_pass(git_commit_lookup(&commit, repo, &oid));
	cl_git_pass(git_reset(repo, (git_object *)commit, GIT_RESET_HARD, NULL));
	git_commit_free(commit);

	cl_git_pass(git_diff_from_buffer(&diff, diff_file, strlen(diff_file)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, index_expected, index_expected_cnt);
	validate_apply_workdir(repo, workdir_expected, workdir_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__rename(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "notbeef.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" },
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_RENAME_FILE,
		strlen(DIFF_RENAME_FILE)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__rename_and_modify(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "6fa10147f00fe1fab1d5e835529a9dad53db8552", 0, "notbeef.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" },
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_RENAME_AND_MODIFY_FILE,
		strlen(DIFF_RENAME_AND_MODIFY_FILE)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

    validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__rename_a_to_b_to_c(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "notbeef.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" },
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_RENAME_A_TO_B_TO_C,
		strlen(DIFF_RENAME_A_TO_B_TO_C)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__rename_a_to_b_to_c_exact(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "notbeef.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" },
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_RENAME_A_TO_B_TO_C_EXACT,
		strlen(DIFF_RENAME_A_TO_B_TO_C_EXACT)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__rename_circular(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "asparagus.txt" },
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" }
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_RENAME_CIRCULAR,
		strlen(DIFF_RENAME_CIRCULAR)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__rename_2_to_1(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "2.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" }
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_RENAME_2_TO_1,
		strlen(DIFF_RENAME_2_TO_1)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__rename_1_to_2(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "1.txt" },
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "2.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" }
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_RENAME_1_TO_2,
		strlen(DIFF_RENAME_1_TO_2)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__two_deltas_one_file(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "0a9fd4415635e72573f0f6b5e68084cfe18f5075", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" }
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_TWO_DELTAS_ONE_FILE,
		strlen(DIFF_TWO_DELTAS_ONE_FILE)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__two_deltas_one_new_file(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "08d4c445cf0078f3d9b604b82f32f4d87e083325", 0, "newfile.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" }
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_TWO_DELTAS_ONE_NEW_FILE,
		strlen(DIFF_TWO_DELTAS_ONE_NEW_FILE)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__rename_and_modify_deltas(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "61c686bed39684eee8a2757ceb1291004a21333f", 0, "asdf.txt" },
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_RENAME_AND_MODIFY_DELTAS,
		strlen(DIFF_RENAME_AND_MODIFY_DELTAS)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__rename_delta_after_modify_delta(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "f51658077d85f2264fa179b4d0848268cb3475c3", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "292cb60ce5e25c337c5b6e12957bbbfe1be4bf49", 0, "other.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "c8c120f466591bbe3b8867361d5ec3cdd9fda756", 0, "veal.txt" }
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_RENAME_AFTER_MODIFY,
		strlen(DIFF_RENAME_AFTER_MODIFY)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__cant_rename_after_modify_nonexistent_target_path(void)
{
	git_diff *diff;

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_RENAME_AFTER_MODIFY_TARGET_PATH,
		strlen(DIFF_RENAME_AFTER_MODIFY_TARGET_PATH)));
	cl_git_fail(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	git_diff_free(diff);
}

void test_apply_both__cant_modify_source_path_after_rename(void)
{
	git_diff *diff;

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_RENAME_AND_MODIFY_SOURCE_PATH,
		strlen(DIFF_RENAME_AND_MODIFY_SOURCE_PATH)));
	cl_git_fail(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	git_diff_free(diff);
}

void test_apply_both__readd_deleted_file(void)
{
	git_diff *diff;

	struct merge_index_entry both_expected[] = {
		{ 0100644, "2dc7f8b24ba27f3888368bd180df03ff4c6c6fab", 0, "asparagus.txt" },
		{ 0100644, "68f6182f4c85d39e1309d97c7e456156dc9c0096", 0, "beef.txt" },
		{ 0100644, "4b7c5650008b2e747fe1809eeb5a1dde0e80850a", 0, "bouilli.txt" },
		{ 0100644, "c4e6cca3ec6ae0148ed231f97257df8c311e015f", 0, "gravy.txt" },
		{ 0100644, "68af1fc7407fd9addf1701a87eb1c95c7494c598", 0, "oyster.txt" },
		{ 0100644, "94d2c01087f48213bd157222d54edfefd77c9bba", 0, "veal.txt" }
	};
	size_t both_expected_cnt = sizeof(both_expected) /
		sizeof(struct merge_index_entry);

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_DELETE_AND_READD_FILE,
		strlen(DIFF_DELETE_AND_READD_FILE)));
	cl_git_pass(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	validate_apply_index(repo, both_expected, both_expected_cnt);
	validate_apply_workdir(repo, both_expected, both_expected_cnt);

	git_diff_free(diff);
}

void test_apply_both__cant_remove_file_twice(void)
{
	git_diff *diff;

	cl_git_pass(git_diff_from_buffer(&diff, DIFF_REMOVE_FILE_TWICE,
		strlen(DIFF_REMOVE_FILE_TWICE)));
	cl_git_fail(git_apply(repo, diff, GIT_APPLY_LOCATION_BOTH, NULL));

	git_diff_free(diff);
}
