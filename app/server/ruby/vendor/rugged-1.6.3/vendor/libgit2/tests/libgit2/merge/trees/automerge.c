#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "merge.h"
#include "futils.h"
#include "../merge_helpers.h"
#include "../conflict_data.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-resolve"
#define TEST_INDEX_PATH TEST_REPO_PATH "/.git/index"

#define THEIRS_AUTOMERGE_BRANCH        "branch"

#define THEIRS_UNRELATED_BRANCH		"unrelated"
#define THEIRS_UNRELATED_OID		"55b4e4687e7a0d9ca367016ed930f385d4022e6f"
#define THEIRS_UNRELATED_PARENT		"d6cf6c7741b3316826af1314042550c97ded1d50"

#define OURS_DIRECTORY_FILE			"df_side1"
#define THEIRS_DIRECTORY_FILE		"df_side2"

/* Non-conflicting files, index entries are common to every merge operation */
#define ADDED_IN_MASTER_INDEX_ENTRY	\
	{ 0100644, "233c0919c998ed110a4b6ff36f353aec8b713487", 0,  "added-in-master.txt" }
#define AUTOMERGEABLE_INDEX_ENTRY \
	{ 0100644, "f2e1550a0c9e53d5811175864a29536642ae3821", 0,  "automergeable.txt" }
#define CHANGED_IN_BRANCH_INDEX_ENTRY \
	{ 0100644, "4eb04c9e79e88f6640d01ff5b25ca2a60764f216", 0,  "changed-in-branch.txt" }
#define CHANGED_IN_MASTER_INDEX_ENTRY \
	{ 0100644, "11deab00b2d3a6f5a3073988ac050c2d7b6655e2", 0,  "changed-in-master.txt" }
#define UNCHANGED_INDEX_ENTRY \
	{ 0100644, "c8f06f2e3bb2964174677e91f0abead0e43c9e5d", 0,  "unchanged.txt" }

/* Expected REUC entries */
#define AUTOMERGEABLE_REUC_ENTRY \
	{ "automergeable.txt", 0100644, 0100644, 0100644, \
	  "6212c31dab5e482247d7977e4f0dd3601decf13b", \
	  "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf", \
	  "058541fc37114bfc1dddf6bd6bffc7fae5c2e6fe" }
#define CONFLICTING_REUC_ENTRY \
	{ "conflicting.txt", 0100644, 0100644, 0100644, \
	  "d427e0b2e138501a3d15cc376077a3631e15bd46", \
	  "4e886e602529caa9ab11d71f86634bd1b6e0de10", \
	  "2bd0a343aeef7a2cf0d158478966a6e587ff3863" }
#define REMOVED_IN_BRANCH_REUC_ENTRY \
	{ "removed-in-branch.txt", 0100644, 0100644, 0, \
	  "dfe3f22baa1f6fce5447901c3086bae368de6bdd", \
	  "dfe3f22baa1f6fce5447901c3086bae368de6bdd", \
	  "" }
#define REMOVED_IN_MASTER_REUC_ENTRY \
	{ "removed-in-master.txt", 0100644, 0, 0100644, \
	  "5c3b68a71fc4fa5d362fd3875e53137c6a5ab7a5", \
	  "", \
	  "5c3b68a71fc4fa5d362fd3875e53137c6a5ab7a5" }

/* Fixture setup and teardown */
void test_merge_trees_automerge__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
}

void test_merge_trees_automerge__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_merge_trees_automerge__automerge(void)
{
	git_index *index;
	const git_index_entry *entry;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;
	git_blob *blob;

	struct merge_index_entry merge_index_entries[] = {
		ADDED_IN_MASTER_INDEX_ENTRY,
		AUTOMERGEABLE_INDEX_ENTRY,
		CHANGED_IN_BRANCH_INDEX_ENTRY,
		CHANGED_IN_MASTER_INDEX_ENTRY,

		{ 0100644, "d427e0b2e138501a3d15cc376077a3631e15bd46", 1, "conflicting.txt" },
		{ 0100644, "4e886e602529caa9ab11d71f86634bd1b6e0de10", 2, "conflicting.txt" },
		{ 0100644, "2bd0a343aeef7a2cf0d158478966a6e587ff3863", 3, "conflicting.txt" },

		UNCHANGED_INDEX_ENTRY,
	};

	struct merge_reuc_entry merge_reuc_entries[] = {
		AUTOMERGEABLE_REUC_ENTRY,
		REMOVED_IN_BRANCH_REUC_ENTRY,
		REMOVED_IN_MASTER_REUC_ENTRY
	};

	cl_git_pass(merge_trees_from_branches(&index, repo, "master", THEIRS_AUTOMERGE_BRANCH, &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 8));
	cl_assert(merge_test_reuc(index, merge_reuc_entries, 3));

	cl_assert((entry = git_index_get_bypath(index, "automergeable.txt", 0)) != NULL);
	cl_assert(entry->file_size == strlen(AUTOMERGEABLE_MERGED_FILE));

	cl_git_pass(git_object_lookup((git_object **)&blob, repo, &entry->id, GIT_OBJECT_BLOB));
	cl_assert(memcmp(git_blob_rawcontent(blob), AUTOMERGEABLE_MERGED_FILE, (size_t)entry->file_size) == 0);

	git_index_free(index);
	git_blob_free(blob);
}

void test_merge_trees_automerge__favor_ours(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		ADDED_IN_MASTER_INDEX_ENTRY,
		AUTOMERGEABLE_INDEX_ENTRY,
		CHANGED_IN_BRANCH_INDEX_ENTRY,
		CHANGED_IN_MASTER_INDEX_ENTRY,
		{ 0100644, "4e886e602529caa9ab11d71f86634bd1b6e0de10", 0, "conflicting.txt" },
		UNCHANGED_INDEX_ENTRY,
	};

	struct merge_reuc_entry merge_reuc_entries[] = {
		AUTOMERGEABLE_REUC_ENTRY,
		CONFLICTING_REUC_ENTRY,
		REMOVED_IN_BRANCH_REUC_ENTRY,
		REMOVED_IN_MASTER_REUC_ENTRY,
	};

	opts.file_favor = GIT_MERGE_FILE_FAVOR_OURS;

	cl_git_pass(merge_trees_from_branches(&index, repo, "master", THEIRS_AUTOMERGE_BRANCH, &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 6));
	cl_assert(merge_test_reuc(index, merge_reuc_entries, 4));

	git_index_free(index);
}

void test_merge_trees_automerge__favor_theirs(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		ADDED_IN_MASTER_INDEX_ENTRY,
		AUTOMERGEABLE_INDEX_ENTRY,
		CHANGED_IN_BRANCH_INDEX_ENTRY,
		CHANGED_IN_MASTER_INDEX_ENTRY,
		{ 0100644, "2bd0a343aeef7a2cf0d158478966a6e587ff3863", 0, "conflicting.txt" },
		UNCHANGED_INDEX_ENTRY,
	};

	struct merge_reuc_entry merge_reuc_entries[] = {
		AUTOMERGEABLE_REUC_ENTRY,
		CONFLICTING_REUC_ENTRY,
		REMOVED_IN_BRANCH_REUC_ENTRY,
		REMOVED_IN_MASTER_REUC_ENTRY,
	};

	opts.file_favor = GIT_MERGE_FILE_FAVOR_THEIRS;

	cl_git_pass(merge_trees_from_branches(&index, repo, "master", THEIRS_AUTOMERGE_BRANCH, &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 6));
	cl_assert(merge_test_reuc(index, merge_reuc_entries, 4));

	git_index_free(index);
}

void test_merge_trees_automerge__unrelated(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "233c0919c998ed110a4b6ff36f353aec8b713487", 0, "added-in-master.txt" },
		{ 0100644, "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf", 2, "automergeable.txt" },
		{ 0100644, "d07ec190c306ec690bac349e87d01c4358e49bb2", 3, "automergeable.txt" },
		{ 0100644, "ab6c44a2e84492ad4b41bb6bac87353e9d02ac8b", 0, "changed-in-branch.txt" },
		{ 0100644, "11deab00b2d3a6f5a3073988ac050c2d7b6655e2", 0, "changed-in-master.txt" },
		{ 0100644, "4e886e602529caa9ab11d71f86634bd1b6e0de10", 2, "conflicting.txt" },
		{ 0100644, "4b253da36a0ae8bfce63aeabd8c5b58429925594", 3, "conflicting.txt" },
		{ 0100644, "ef58fdd8086c243bdc81f99e379acacfd21d32d6", 0, "new-in-unrelated1.txt" },
		{ 0100644, "948ba6e701c1edab0c2d394fb7c5538334129793", 0, "new-in-unrelated2.txt" },
		{ 0100644, "dfe3f22baa1f6fce5447901c3086bae368de6bdd", 0, "removed-in-branch.txt" },
		{ 0100644, "c8f06f2e3bb2964174677e91f0abead0e43c9e5d", 0, "unchanged.txt" },
	};

	cl_git_pass(merge_trees_from_branches(&index, repo, "master", THEIRS_UNRELATED_BRANCH, &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 11));

	git_index_free(index);
}
