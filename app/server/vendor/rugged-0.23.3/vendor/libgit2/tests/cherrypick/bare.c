#include "clar.h"
#include "clar_libgit2.h"

#include "buffer.h"
#include "fileops.h"
#include "git2/cherrypick.h"

#include "../merge/merge_helpers.h"

#define TEST_REPO_PATH "cherrypick"

static git_repository *repo;

void test_cherrypick_bare__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
}

void test_cherrypick_bare__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_cherrypick_bare__automerge(void)
{
	git_commit *head = NULL, *commit = NULL;
	git_index *index = NULL;
	git_oid head_oid, cherry_oid;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "38c05a857e831a7e759d83778bfc85d003e21c45", 0, "file1.txt" },
		{ 0100644, "a661b5dec1004e2c62654ded3762370c27cf266b", 0, "file2.txt" },
		{ 0100644, "df6b290e0bd6a89b01d69f66687e8abf385283ca", 0, "file3.txt" },
	};

	git_oid_fromstr(&head_oid, "d3d77487660ee3c0194ee01dc5eaf478782b1c7e");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));

	git_oid_fromstr(&cherry_oid, "cfc4f0999a8367568e049af4f72e452d40828a15");
	cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));

	cl_git_pass(git_cherrypick_commit(&index, repo, commit, head, 0, NULL));
	cl_assert(merge_test_index(index, merge_index_entries, 3));

	git_index_free(index);
	git_commit_free(head);
	git_commit_free(commit);
}

void test_cherrypick_bare__conflicts(void)
{
	git_commit *head = NULL, *commit = NULL;
	git_index *index = NULL;
	git_oid head_oid, cherry_oid;

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

	git_oid_fromstr(&cherry_oid, "e9b63f3655b2ad80c0ff587389b5a9589a3a7110");
	cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));

	cl_git_pass(git_cherrypick_commit(&index, repo, commit, head, 0, NULL));
	cl_assert(merge_test_index(index, merge_index_entries, 7));

	git_index_free(index);
	git_commit_free(head);
	git_commit_free(commit);
}

void test_cherrypick_bare__orphan(void)
{
	git_commit *head = NULL, *commit = NULL;
	git_index *index = NULL;
	git_oid head_oid, cherry_oid;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "38c05a857e831a7e759d83778bfc85d003e21c45", 0, "file1.txt" },
		{ 0100644, "a661b5dec1004e2c62654ded3762370c27cf266b", 0, "file2.txt" },
		{ 0100644, "85a4a1d791973644f24c72f5e89420d3064cc452", 0, "file3.txt" },
		{ 0100644, "9ccb9bf50c011fd58dcbaa65df917bf79539717f", 0, "orphan.txt" },
	};

	git_oid_fromstr(&head_oid, "d3d77487660ee3c0194ee01dc5eaf478782b1c7e");
	cl_git_pass(git_commit_lookup(&head, repo, &head_oid));

	git_oid_fromstr(&cherry_oid, "74f06b5bfec6d33d7264f73606b57a7c0b963819");
	cl_git_pass(git_commit_lookup(&commit, repo, &cherry_oid));

	cl_git_pass(git_cherrypick_commit(&index, repo, commit, head, 0, NULL));
	cl_assert(merge_test_index(index, merge_index_entries, 4));

	git_index_free(index);
	git_commit_free(head);
	git_commit_free(commit);
}

