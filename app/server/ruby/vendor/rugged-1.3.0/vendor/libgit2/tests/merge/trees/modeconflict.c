#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "buffer.h"
#include "merge.h"
#include "../merge_helpers.h"
#include "futils.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-resolve"

#define DF_SIDE1_BRANCH		"df_side1"
#define DF_SIDE2_BRANCH		"df_side2"

/* Fixture setup and teardown */
void test_merge_trees_modeconflict__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
}

void test_merge_trees_modeconflict__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_merge_trees_modeconflict__df_conflict(void)
{
	git_index *index;

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

	cl_git_pass(merge_trees_from_branches(&index, repo, DF_SIDE1_BRANCH, DF_SIDE2_BRANCH, NULL));

	cl_assert(merge_test_index(index, merge_index_entries, 20));

	git_index_free(index);
}
