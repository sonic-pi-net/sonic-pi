#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "merge.h"
#include "../merge_helpers.h"
#include "../conflict_data.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-resolve"

void test_merge_trees_commits__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
}

void test_merge_trees_commits__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_merge_trees_commits__automerge(void)
{
	git_index *index;
	const git_index_entry *entry;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;
	git_blob *blob;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "233c0919c998ed110a4b6ff36f353aec8b713487", 0,  "added-in-master.txt" },
		{ 0100644, "f2e1550a0c9e53d5811175864a29536642ae3821", 0,  "automergeable.txt" },
		{ 0100644, "4eb04c9e79e88f6640d01ff5b25ca2a60764f216", 0,  "changed-in-branch.txt" },
		{ 0100644, "11deab00b2d3a6f5a3073988ac050c2d7b6655e2", 0,  "changed-in-master.txt" },

		{ 0100644, "d427e0b2e138501a3d15cc376077a3631e15bd46", 1, "conflicting.txt" },
		{ 0100644, "4e886e602529caa9ab11d71f86634bd1b6e0de10", 2, "conflicting.txt" },
		{ 0100644, "2bd0a343aeef7a2cf0d158478966a6e587ff3863", 3, "conflicting.txt" },

		{ 0100644, "c8f06f2e3bb2964174677e91f0abead0e43c9e5d", 0,  "unchanged.txt" },
	};

	struct merge_reuc_entry merge_reuc_entries[] = {
		{ "automergeable.txt", 0100644, 0100644, 0100644, \
			"6212c31dab5e482247d7977e4f0dd3601decf13b", \
			"ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf", \
			"058541fc37114bfc1dddf6bd6bffc7fae5c2e6fe" },
		{ "removed-in-branch.txt", 0100644, 0100644, 0, \
			"dfe3f22baa1f6fce5447901c3086bae368de6bdd", \
			"dfe3f22baa1f6fce5447901c3086bae368de6bdd", \
			"" },
		{ "removed-in-master.txt", 0100644, 0, 0100644, \
			"5c3b68a71fc4fa5d362fd3875e53137c6a5ab7a5", \
			"", \
			"5c3b68a71fc4fa5d362fd3875e53137c6a5ab7a5" },
	};

	cl_git_pass(merge_commits_from_branches(&index, repo, "master", "branch", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 8));
	cl_assert(merge_test_reuc(index, merge_reuc_entries, 3));

	cl_assert((entry = git_index_get_bypath(index, "automergeable.txt", 0)) != NULL);
	cl_assert(entry->file_size == strlen(AUTOMERGEABLE_MERGED_FILE));

	cl_git_pass(git_object_lookup((git_object **)&blob, repo, &entry->id, GIT_OBJECT_BLOB));
	cl_assert(memcmp(git_blob_rawcontent(blob), AUTOMERGEABLE_MERGED_FILE, (size_t)entry->file_size) == 0);

	git_index_free(index);
	git_blob_free(blob);
}

void test_merge_trees_commits__no_ancestor(void)
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

	cl_git_pass(merge_commits_from_branches(&index, repo, "master", "unrelated", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 11));

	git_index_free(index);
}

void test_merge_trees_commits__df_conflict(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "49130a28ef567af9a6a6104c38773fedfa5f9742", 2, "dir-10" },
		{ 0100644, "6c06dcd163587c2cc18be44857e0b71116382aeb", 3, "dir-10" },
		{ 0100644, "43aafd43bea779ec74317dc361f45ae3f532a505", 0, "dir-6" },
		{ 0100644, "a031a28ae70e33a641ce4b8a8f6317f1ab79dee4", 3, "dir-7" },
		{ 0100644, "5012fd565b1393bdfda1805d4ec38ce6619e1fd1", 1, "dir-7/file.txt" },
		{ 0100644, "a5563304ddf6caba25cb50323a2ea6f7dbfcadca", 2, "dir-7/file.txt" },
		{ 0100644, "e9ad6ec3e38364a3d07feda7c4197d4d845c53b5", 0, "dir-8" },
		{ 0100644, "3ef4d30382ca33fdeba9fda895a99e0891ba37aa", 2, "dir-9" },
		{ 0100644, "fc4c636d6515e9e261f9260dbcf3cc6eca97ea08", 1, "dir-9/file.txt" },
		{ 0100644, "76ab0e2868197ec158ddd6c78d8a0d2fd73d38f9", 3, "dir-9/file.txt" },
		{ 0100644, "5c2411f8075f48a6b2fdb85ebc0d371747c4df15", 0, "file-1/new" },
		{ 0100644, "a39a620dae5bc8b4e771cd4d251b7d080401a21e", 1, "file-2" },
		{ 0100644, "d963979c237d08b6ba39062ee7bf64c7d34a27f8", 2, "file-2" },
		{ 0100644, "5c341ead2ba6f2af98ce5ec3fe84f6b6d2899c0d", 0, "file-2/new" },
		{ 0100644, "9efe7723802d4305142eee177e018fee1572c4f4", 0, "file-3/new" },
		{ 0100644, "bacac9b3493509aa15e1730e1545fc0919d1dae0", 1, "file-4" },
		{ 0100644, "7663fce0130db092936b137cabd693ec234eb060", 3, "file-4" },
		{ 0100644, "e49f917b448d1340b31d76e54ba388268fd4c922", 0, "file-4/new" },
		{ 0100644, "cab2cf23998b40f1af2d9d9a756dc9e285a8df4b", 2, "file-5/new" },
		{ 0100644, "f5504f36e6f4eb797a56fc5bac6c6c7f32969bf2", 3, "file-5/new" },
	};

	cl_git_pass(merge_trees_from_branches(&index, repo, "df_side1", "df_side2", &opts));

	cl_assert(merge_test_index(index, merge_index_entries, 20));

	git_index_free(index);
}

void test_merge_trees_commits__fail_on_conflict(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	opts.flags |= GIT_MERGE_FAIL_ON_CONFLICT;

	cl_git_fail_with(GIT_EMERGECONFLICT,
		merge_trees_from_branches(&index, repo, "df_side1", "df_side2", &opts));

	cl_git_fail_with(GIT_EMERGECONFLICT,
		merge_commits_from_branches(&index, repo, "master", "unrelated", &opts));
	cl_git_fail_with(GIT_EMERGECONFLICT,
		merge_commits_from_branches(&index, repo, "master", "branch", &opts));
}

