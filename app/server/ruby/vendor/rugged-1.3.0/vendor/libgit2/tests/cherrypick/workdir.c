#include "clar.h"
#include "clar_libgit2.h"

#include "buffer.h"
#include "futils.h"
#include "git2/cherrypick.h"

#include "../merge/merge_helpers.h"

#define TEST_REPO_PATH "cherrypick"

static git_repository *repo;
static git_index *repo_index;

/* Fixture setup and teardown */
void test_cherrypick_workdir__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
	git_repository_index(&repo_index, repo);
}

void test_cherrypick_workdir__cleanup(void)
{
	git_index_free(repo_index);
	cl_git_sandbox_cleanup();
}

/* git reset --hard d3d77487660ee3c0194ee01dc5eaf478782b1c7e
 * git cherry-pick cfc4f0999a8367568e049af4f72e452d40828a15
 * git cherry-pick 964ea3da044d9083181a88ba6701de9e35778bf4
 * git cherry-pick a43a050c588d4e92f11a6b139680923e9728477d
 */
void test_cherrypick_workdir__automerge(void)
{
	git_oid head_oid;
	git_signature *signature = NULL;
	size_t i;

	const char *cherrypick_oids[] = {
		"cfc4f0999a8367568e049af4f72e452d40828a15",
		"964ea3da044d9083181a88ba6701de9e35778bf4",
		"a43a050c588d4e92f11a6b139680923e9728477d",
	};

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "38c05a857e831a7e759d83778bfc85d003e21c45", 0, "file1.txt" },
		{ 0100644, "a661b5dec1004e2c62654ded3762370c27cf266b", 0, "file2.txt" },
		{ 0100644, "df6b290e0bd6a89b01d69f66687e8abf385283ca", 0, "file3.txt" },

		{ 0100644, "38c05a857e831a7e759d83778bfc85d003e21c45", 0, "file1.txt" },
		{ 0100644, "bd8fc3c59fb52d3c8b5907ace7defa5803f82419", 0, "file2.txt" },
		{ 0100644, "df6b290e0bd6a89b01d69f66687e8abf385283ca", 0, "file3.txt" },

		{ 0100644, "f06427bee380364bc7e0cb26a9245158e4726ce0", 0, "file1.txt" },
		{ 0100644, "bd8fc3c59fb52d3c8b5907ace7defa5803f82419", 0, "file2.txt" },
		{ 0100644, "df6b290e0bd6a89b01d69f66687e8abf385283ca", 0, "file3.txt" },
	};

	cl_git_pass(git_signature_new(&signature, "Picker", "picker@example.org", time(NULL), 0));

	git_oid_fromstr(&head_oid, "d3d77487660ee3c0194ee01dc5eaf478782b1c7e");

	for (i = 0; i < 3; ++i) {
		git_commit *head = NULL, *commit = NULL;
		git_oid cherry_oid, cherrypicked_oid, cherrypicked_tree_oid;
		git_tree *cherrypicked_tree = NULL;

		cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
		cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

		git_oid_fromstr(&cherry_oid, cherrypick_oids[i]);
		cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));
		cl_git_pass(git_cherrypick(repo, commit, NULL));

		cl_assert(git_path_exists(TEST_REPO_PATH "/.git/CHERRY_PICK_HEAD"));
		cl_assert(git_path_exists(TEST_REPO_PATH "/.git/MERGE_MSG"));

		cl_git_pass(git_index_write_tree(&cherrypicked_tree_oid, repo_index));
		cl_git_pass(git_tree_lookup(&cherrypicked_tree, repo, &cherrypicked_tree_oid));
		cl_git_pass(git_commit_create(&cherrypicked_oid, repo, "HEAD", signature, signature, NULL,
			"Cherry picked!", cherrypicked_tree, 1, (const git_commit **)&head));

		cl_assert(merge_test_index(repo_index, merge_index_entries + i * 3, 3));

		git_oid_cpy(&head_oid, &cherrypicked_oid);

		git_tree_free(cherrypicked_tree);
		git_commit_free(head);
		git_commit_free(commit);
	}

	git_signature_free(signature);
}

/* git reset --hard cfc4f0999a8367568e049af4f72e452d40828a15
 * git cherry-pick a43a050c588d4e92f11a6b139680923e9728477d*/
void test_cherrypick_workdir__empty_result(void)
{
	git_oid head_oid;
	git_signature *signature = NULL;
	git_commit *head = NULL, *commit = NULL;
	git_oid cherry_oid;

	const char *cherrypick_oid = "a43a050c588d4e92f11a6b139680923e9728477d";

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "19c5c7207054604b69c84d08a7571ef9672bb5c2", 0, "file1.txt" },
		{ 0100644, "a58ca3fee5eb68b11adc2703e5843f968c9dad1e", 0, "file2.txt" },
		{ 0100644, "28d9eb4208074ad1cc84e71ccc908b34573f05d2", 0, "file3.txt" },
	};

	cl_git_pass(git_signature_new(&signature, "Picker", "picker@example.org", time(NULL), 0));

	git_oid_fromstr(&head_oid, "cfc4f0999a8367568e049af4f72e452d40828a15");

	/* Create an untracked file that should not conflict */
	cl_git_mkfile(TEST_REPO_PATH "/file4.txt", "");
	cl_assert(git_path_exists(TEST_REPO_PATH "/file4.txt"));

	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&cherry_oid, cherrypick_oid);
	cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));
	cl_git_pass(git_cherrypick(repo, commit, NULL));

	/* The resulting tree should not have changed, the change was already on HEAD */
	cl_assert(merge_test_index(repo_index, merge_index_entries, 3));

	git_commit_free(head);
	git_commit_free(commit);

	git_signature_free(signature);
}

/* git reset --hard bafbf6912c09505ac60575cd43d3f2aba3bd84d8
 * git cherry-pick e9b63f3655b2ad80c0ff587389b5a9589a3a7110
 */
void test_cherrypick_workdir__conflicts(void)
{
	git_commit *head = NULL, *commit = NULL;
	git_oid head_oid, cherry_oid;
	git_buf conflicting_buf = GIT_BUF_INIT, mergemsg_buf = GIT_BUF_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "242e7977ba73637822ffb265b46004b9b0e5153b", 0, "file1.txt" },
		{ 0100644, "a58ca3fee5eb68b11adc2703e5843f968c9dad1e", 1, "file2.txt" },
		{ 0100644, "bd6ffc8c6c41f0f85ff9e3d61c9479516bac0024", 2, "file2.txt" },
		{ 0100644, "563f6473a3858f99b80e5f93c660512ed38e1e6f", 3, "file2.txt" },
		{ 0100644, "28d9eb4208074ad1cc84e71ccc908b34573f05d2", 1, "file3.txt" },
		{ 0100644, "1124c2c1ae07b26fded662d6c3f3631d9dc16f88", 2, "file3.txt" },
		{ 0100644, "e233b9ed408a95e9d4b65fec7fc34943a556deb2", 3, "file3.txt" },
	};

	git_oid_fromstr(&head_oid, "bafbf6912c09505ac60575cd43d3f2aba3bd84d8");

	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&cherry_oid, "e9b63f3655b2ad80c0ff587389b5a9589a3a7110");
	cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));
	cl_git_pass(git_cherrypick(repo, commit, NULL));

	cl_assert(git_path_exists(TEST_REPO_PATH "/.git/CHERRY_PICK_HEAD"));
	cl_assert(git_path_exists(TEST_REPO_PATH "/.git/MERGE_MSG"));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 7));

	cl_git_pass(git_futils_readbuffer(&mergemsg_buf,
		TEST_REPO_PATH "/.git/MERGE_MSG"));
	cl_assert(strcmp(git_buf_cstr(&mergemsg_buf),
		"Change all files\n" \
		"\n" \
		"Conflicts:\n" \
		"\tfile2.txt\n" \
		"\tfile3.txt\n") == 0);

	cl_git_pass(git_futils_readbuffer(&conflicting_buf,
		TEST_REPO_PATH "/file2.txt"));

	cl_assert(strcmp(git_buf_cstr(&conflicting_buf),
		"!File 2\n" \
		"File 2\n" \
		"File 2\n" \
		"File 2\n" \
		"File 2\n" \
		"File 2\n" \
		"File 2\n" \
		"File 2\n" \
		"File 2\n" \
		"File 2\n" \
		"File 2!!\n" \
		"File 2\n" \
		"File 2\n" \
		"File 2\n" \
		"<<<<<<< HEAD\n" \
		"File 2\n" \
		"=======\n" \
		"File 2!\n" \
		"File 2\n" \
		"File 2!\n" \
		">>>>>>> e9b63f3... Change all files\n") == 0);

	cl_git_pass(git_futils_readbuffer(&conflicting_buf,
		TEST_REPO_PATH "/file3.txt"));

	cl_assert(strcmp(git_buf_cstr(&conflicting_buf),
		"!File 3\n" \
		"File 3\n" \
		"File 3\n" \
		"File 3\n" \
		"File 3\n" \
		"File 3\n" \
		"File 3\n" \
		"File 3\n" \
		"File 3\n" \
		"File 3\n" \
		"File 3\n" \
		"File 3!!\n" \
		"File 3\n" \
		"File 3\n" \
		"File 3\n" \
		"<<<<<<< HEAD\n" \
		"=======\n" \
		"File 3!\n" \
		"File 3!\n" \
		">>>>>>> e9b63f3... Change all files\n") == 0);

	git_commit_free(commit);
	git_commit_free(head);
	git_buf_dispose(&mergemsg_buf);
	git_buf_dispose(&conflicting_buf);
}

/* git reset --hard bafbf6912c09505ac60575cd43d3f2aba3bd84d8
 * git cherry-pick -X ours e9b63f3655b2ad80c0ff587389b5a9589a3a7110
 */
void test_cherrypick_workdir__conflict_use_ours(void)
{
	git_commit *head = NULL, *commit = NULL;
	git_oid head_oid, cherry_oid;
	git_cherrypick_options opts = GIT_CHERRYPICK_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "242e7977ba73637822ffb265b46004b9b0e5153b", 0, "file1.txt" },
		{ 0100644, "a58ca3fee5eb68b11adc2703e5843f968c9dad1e", 1, "file2.txt" },
		{ 0100644, "bd6ffc8c6c41f0f85ff9e3d61c9479516bac0024", 2, "file2.txt" },
		{ 0100644, "563f6473a3858f99b80e5f93c660512ed38e1e6f", 3, "file2.txt" },
		{ 0100644, "28d9eb4208074ad1cc84e71ccc908b34573f05d2", 1, "file3.txt" },
		{ 0100644, "1124c2c1ae07b26fded662d6c3f3631d9dc16f88", 2, "file3.txt" },
		{ 0100644, "e233b9ed408a95e9d4b65fec7fc34943a556deb2", 3, "file3.txt" },
	};

	struct merge_index_entry merge_filesystem_entries[] = {
		{ 0100644, "242e7977ba73637822ffb265b46004b9b0e5153b", 0, "file1.txt" },
		{ 0100644, "bd6ffc8c6c41f0f85ff9e3d61c9479516bac0024", 0, "file2.txt" },
		{ 0100644, "1124c2c1ae07b26fded662d6c3f3631d9dc16f88", 0, "file3.txt" },
	};

	/* leave the index in a conflicted state, but checkout "ours" to the workdir */
	opts.checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_USE_OURS;

	git_oid_fromstr(&head_oid, "bafbf6912c09505ac60575cd43d3f2aba3bd84d8");

	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&cherry_oid, "e9b63f3655b2ad80c0ff587389b5a9589a3a7110");
	cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));
	cl_git_pass(git_cherrypick(repo, commit, &opts));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 7));
	cl_assert(merge_test_workdir(repo, merge_filesystem_entries, 3));

	/* resolve conflicts in the index by taking "ours" */
	opts.merge_opts.file_favor = GIT_MERGE_FILE_FAVOR_OURS;

	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));
	cl_git_pass(git_cherrypick(repo, commit, &opts));

	cl_assert(merge_test_index(repo_index, merge_filesystem_entries, 3));
	cl_assert(merge_test_workdir(repo, merge_filesystem_entries, 3));

	git_commit_free(commit);
	git_commit_free(head);
}

/* git reset --hard cfc4f0999a8367568e049af4f72e452d40828a15
 * git cherry-pick 2a26c7e88b285613b302ba76712bc998863f3cbc
 */
void test_cherrypick_workdir__rename(void)
{
	git_commit *head, *commit;
	git_oid head_oid, cherry_oid;
	git_cherrypick_options opts = GIT_CHERRYPICK_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "19c5c7207054604b69c84d08a7571ef9672bb5c2", 0, "file1.txt" },
		{ 0100644, "a58ca3fee5eb68b11adc2703e5843f968c9dad1e", 0, "file2.txt" },
		{ 0100644, "28d9eb4208074ad1cc84e71ccc908b34573f05d2", 0, "file3.txt.renamed" },
	};

	opts.merge_opts.flags |= GIT_MERGE_FIND_RENAMES;
	opts.merge_opts.rename_threshold = 50;

	git_oid_fromstr(&head_oid, "cfc4f0999a8367568e049af4f72e452d40828a15");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&cherry_oid, "2a26c7e88b285613b302ba76712bc998863f3cbc");
	cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));
	cl_git_pass(git_cherrypick(repo, commit, &opts));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 3));

	git_commit_free(commit);
	git_commit_free(head);
}

/* git reset --hard 44cd2ed2052c9c68f9a439d208e9614dc2a55c70
 * git cherry-pick 2a26c7e88b285613b302ba76712bc998863f3cbc
 */
void test_cherrypick_workdir__both_renamed(void)
{
	git_commit *head, *commit;
	git_oid head_oid, cherry_oid;
	git_buf mergemsg_buf = GIT_BUF_INIT;
	git_cherrypick_options opts = GIT_CHERRYPICK_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "19c5c7207054604b69c84d08a7571ef9672bb5c2", 0, "file1.txt" },
		{ 0100644, "a58ca3fee5eb68b11adc2703e5843f968c9dad1e", 0, "file2.txt" },
		{ 0100644, "e233b9ed408a95e9d4b65fec7fc34943a556deb2", 1, "file3.txt" },
		{ 0100644, "e233b9ed408a95e9d4b65fec7fc34943a556deb2", 3, "file3.txt.renamed" },
		{ 0100644, "28d9eb4208074ad1cc84e71ccc908b34573f05d2", 2, "file3.txt.renamed_on_branch" },
	};

	opts.merge_opts.flags |= GIT_MERGE_FIND_RENAMES;
	opts.merge_opts.rename_threshold = 50;

	git_oid_fromstr(&head_oid, "44cd2ed2052c9c68f9a439d208e9614dc2a55c70");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&cherry_oid, "2a26c7e88b285613b302ba76712bc998863f3cbc");
	cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));
	cl_git_pass(git_cherrypick(repo, commit, &opts));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 5));

	cl_git_pass(git_futils_readbuffer(&mergemsg_buf,
		TEST_REPO_PATH "/.git/MERGE_MSG"));
	cl_assert(strcmp(git_buf_cstr(&mergemsg_buf),
		"Renamed file3.txt -> file3.txt.renamed\n" \
		"\n" \
		"Conflicts:\n" \
		"\tfile3.txt\n" \
		"\tfile3.txt.renamed\n" \
		"\tfile3.txt.renamed_on_branch\n") == 0);

	git_buf_dispose(&mergemsg_buf);
	git_commit_free(commit);
	git_commit_free(head);
}

void test_cherrypick_workdir__nonmerge_fails_mainline_specified(void)
{
	git_reference *head;
	git_commit *commit;
	git_cherrypick_options opts = GIT_CHERRYPICK_OPTIONS_INIT;

	cl_git_pass(git_repository_head(&head, repo));
	cl_git_pass(git_reference_peel((git_object **)&commit, head, GIT_OBJECT_COMMIT));

	opts.mainline = 1;
	cl_must_fail(git_cherrypick(repo, commit, &opts));
	cl_assert(!git_path_exists(TEST_REPO_PATH "/.git/CHERRY_PICK_HEAD"));
	cl_assert(!git_path_exists(TEST_REPO_PATH "/.git/MERGE_MSG"));

	git_reference_free(head);
	git_commit_free(commit);
}

/* git reset --hard cfc4f0999a8367568e049af4f72e452d40828a15
 * git cherry-pick abe4603bc7cd5b8167a267e0e2418fd2348f8cff
 */
void test_cherrypick_workdir__merge_fails_without_mainline_specified(void)
{
	git_commit *head, *commit;
	git_oid head_oid, cherry_oid;

	git_oid_fromstr(&head_oid, "cfc4f0999a8367568e049af4f72e452d40828a15");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&cherry_oid, "abe4603bc7cd5b8167a267e0e2418fd2348f8cff");
	cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));

	cl_must_fail(git_cherrypick(repo, commit, NULL));
	cl_assert(!git_path_exists(TEST_REPO_PATH "/.git/CHERRY_PICK_HEAD"));
	cl_assert(!git_path_exists(TEST_REPO_PATH "/.git/MERGE_MSG"));

	git_commit_free(commit);
	git_commit_free(head);
}

/* git reset --hard cfc4f0999a8367568e049af4f72e452d40828a15
 * git cherry-pick -m1 abe4603bc7cd5b8167a267e0e2418fd2348f8cff
 */
void test_cherrypick_workdir__merge_first_parent(void)
{
	git_commit *head, *commit;
	git_oid head_oid, cherry_oid;
	git_cherrypick_options opts = GIT_CHERRYPICK_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "f90f9dcbdac2cce5cc166346160e19cb693ef4e8", 0, "file1.txt" },
		{ 0100644, "563f6473a3858f99b80e5f93c660512ed38e1e6f", 0, "file2.txt" },
		{ 0100644, "e233b9ed408a95e9d4b65fec7fc34943a556deb2", 0, "file3.txt" },
	};

	opts.mainline = 1;

	git_oid_fromstr(&head_oid, "cfc4f0999a8367568e049af4f72e452d40828a15");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&cherry_oid, "abe4603bc7cd5b8167a267e0e2418fd2348f8cff");
	cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));

	cl_git_pass(git_cherrypick(repo, commit, &opts));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 3));

	git_commit_free(commit);
	git_commit_free(head);
}

/* git reset --hard cfc4f0999a8367568e049af4f72e452d40828a15
 * git cherry-pick -m2 abe4603bc7cd5b8167a267e0e2418fd2348f8cff
 */
void test_cherrypick_workdir__merge_second_parent(void)
{
	git_commit *head, *commit;
	git_oid head_oid, cherry_oid;
	git_cherrypick_options opts = GIT_CHERRYPICK_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "487434cace79238a7091e2220611d4f20a765690", 0, "file1.txt" },
		{ 0100644, "e5183bfd18e3a0a691fadde2f0d5610b73282d31", 0, "file2.txt" },
		{ 0100644, "409a1bec58bf35348e8b62b72bb9c1f45cf5a587", 0, "file3.txt" },
	};

	opts.mainline = 2;

	git_oid_fromstr(&head_oid, "cfc4f0999a8367568e049af4f72e452d40828a15");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));
	cl_git_pass(git_reset(repo, (git_object *)head, GIT_RESET_HARD, NULL));

	git_oid_fromstr(&cherry_oid, "abe4603bc7cd5b8167a267e0e2418fd2348f8cff");
	cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));

	cl_git_pass(git_cherrypick(repo, commit, &opts));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 3));

	git_commit_free(commit);
	git_commit_free(head);
}

