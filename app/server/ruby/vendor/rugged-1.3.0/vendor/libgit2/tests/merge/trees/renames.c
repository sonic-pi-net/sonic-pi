#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "buffer.h"
#include "merge.h"
#include "../merge_helpers.h"
#include "futils.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-resolve"

#define BRANCH_RENAME_OURS					"rename_conflict_ours"
#define BRANCH_RENAME_THEIRS				"rename_conflict_theirs"

/* Fixture setup and teardown */
void test_merge_trees_renames__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
}

void test_merge_trees_renames__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_merge_trees_renames__index(void)
{
	git_index *index;
	git_merge_options *opts = NULL;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "68c6c84b091926c7d90aa6a79b2bc3bb6adccd8e", 0, "0a-no-change.txt" },
		{ 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 0, "0b-duplicated-in-ours.txt" },
		{ 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 1, "0b-rewritten-in-ours.txt" },
		{ 0100644, "e376fbdd06ebf021c92724da9f26f44212734e3e", 2, "0b-rewritten-in-ours.txt" },
		{ 0100644, "b2d399ae15224e1d58066e3c8df70ce37de7a656", 3, "0b-rewritten-in-ours.txt" },
		{ 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 0, "0c-duplicated-in-theirs.txt" },
		{ 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 1, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "efc9121fdedaf08ba180b53ebfbcf71bd488ed09", 2, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "712ebba6669ea847d9829e4f1059d6c830c8b531", 3, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "0d872f8e871a30208305978ecbf9e66d864f1638", 0, "1a-newname-in-ours-edited-in-theirs.txt" },
		{ 0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb", 0, "1a-newname-in-ours.txt" },
		{ 0100644, "ed9523e62e453e50dd9be1606af19399b96e397a", 0, "1b-newname-in-theirs-edited-in-ours.txt" },
		{ 0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136", 0, "1b-newname-in-theirs.txt" },
		{ 0100644, "178940b450f238a56c0d75b7955cb57b38191982", 0, "2-newname-in-both.txt" },
		{ 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 2, "3a-newname-in-ours-deleted-in-theirs.txt" },
		{ 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 1, "3a-renamed-in-ours-deleted-in-theirs.txt" },
		{ 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 3, "3b-newname-in-theirs-deleted-in-ours.txt" },
		{ 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 1, "3b-renamed-in-theirs-deleted-in-ours.txt" },
		{ 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 2, "4a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "8b5b53cb2aa9ceb1139f5312fcfa3cc3c5a47c9a", 3, "4a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 1, "4a-renamed-in-ours-added-in-theirs.txt" },
		{ 0100644, "de872ee3618b894992e9d1e18ba2ebe256a112f9", 2, "4b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db", 3, "4b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db", 1, "4b-renamed-in-theirs-added-in-ours.txt" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 2, "5a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "98ba4205fcf31f5dd93c916d35fe3f3b3d0e6714", 3, "5a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 1, "5a-renamed-in-ours-added-in-theirs.txt" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 3, "5a-renamed-in-ours-added-in-theirs.txt" },
		{ 0100644, "385c8a0f26ddf79e9041e15e17dc352ed2c4cced", 2, "5b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 3, "5b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 1, "5b-renamed-in-theirs-added-in-ours.txt" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 2, "5b-renamed-in-theirs-added-in-ours.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 2, "6-both-renamed-1-to-2-ours.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 3, "6-both-renamed-1-to-2-theirs.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 1, "6-both-renamed-1-to-2.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 1, "7-both-renamed-side-1.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 3, "7-both-renamed-side-1.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 1, "7-both-renamed-side-2.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 2, "7-both-renamed-side-2.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 2, "7-both-renamed.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 3, "7-both-renamed.txt" },
	};

	struct merge_name_entry merge_name_entries[] = {
		{
			"3a-renamed-in-ours-deleted-in-theirs.txt",
			"3a-newname-in-ours-deleted-in-theirs.txt",
			""
		},

		{
			"3b-renamed-in-theirs-deleted-in-ours.txt",
			"",
			"3b-newname-in-theirs-deleted-in-ours.txt",
		},

		{
			"4a-renamed-in-ours-added-in-theirs.txt",
			"4a-newname-in-ours-added-in-theirs.txt",
			"",
		},

		{
			"4b-renamed-in-theirs-added-in-ours.txt",
			"",
			"4b-newname-in-theirs-added-in-ours.txt",
		},

		{
			"5a-renamed-in-ours-added-in-theirs.txt",
			"5a-newname-in-ours-added-in-theirs.txt",
			"5a-renamed-in-ours-added-in-theirs.txt",
		},

		{
			"5b-renamed-in-theirs-added-in-ours.txt",
			"5b-renamed-in-theirs-added-in-ours.txt",
			"5b-newname-in-theirs-added-in-ours.txt",
		},

		{
			"6-both-renamed-1-to-2.txt",
			"6-both-renamed-1-to-2-ours.txt",
			"6-both-renamed-1-to-2-theirs.txt",
		},

		{
			"7-both-renamed-side-1.txt",
			"7-both-renamed.txt",
			"7-both-renamed-side-1.txt",
		},

		{
			"7-both-renamed-side-2.txt",
			"7-both-renamed-side-2.txt",
			"7-both-renamed.txt",
		},
	};

	struct merge_reuc_entry merge_reuc_entries[] = {
		{ "1a-newname-in-ours-edited-in-theirs.txt",
			0, 0100644, 0,
			"",
			"c3d02eeef75183df7584d8d13ac03053910c1301",
			"" },

		{ "1a-newname-in-ours.txt",
			0, 0100644, 0,
			"",
			"d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb",
			"" },

		{ "1a-renamed-in-ours-edited-in-theirs.txt",
			0100644, 0, 0100644,
			"c3d02eeef75183df7584d8d13ac03053910c1301",
			"",
			"0d872f8e871a30208305978ecbf9e66d864f1638" },

		{ "1a-renamed-in-ours.txt",
			0100644, 0, 0100644,
			"d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb",
			"",
			"d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb" },

		{ "1b-newname-in-theirs-edited-in-ours.txt",
			0, 0, 0100644,
			"",
			"",
			"241a1005cd9b980732741b74385b891142bcba28" },

		{ "1b-newname-in-theirs.txt",
			0, 0, 0100644,
			"",
			"",
			"2b5f1f181ee3b58ea751f5dd5d8f9b445520a136" },

		{ "1b-renamed-in-theirs-edited-in-ours.txt",
			0100644, 0100644, 0,
			"241a1005cd9b980732741b74385b891142bcba28",
			"ed9523e62e453e50dd9be1606af19399b96e397a",
			"" },

		{ "1b-renamed-in-theirs.txt",
			0100644, 0100644, 0,
			"2b5f1f181ee3b58ea751f5dd5d8f9b445520a136",
			"2b5f1f181ee3b58ea751f5dd5d8f9b445520a136",
			"" },

		{ "2-newname-in-both.txt",
			0, 0100644, 0100644,
			"",
			"178940b450f238a56c0d75b7955cb57b38191982",
			"178940b450f238a56c0d75b7955cb57b38191982" },

		{ "2-renamed-in-both.txt",
			0100644, 0, 0,
			"178940b450f238a56c0d75b7955cb57b38191982",
			"",
			"" },
	};

	cl_git_pass(merge_trees_from_branches(&index, repo,
		BRANCH_RENAME_OURS, BRANCH_RENAME_THEIRS,
		opts));

	cl_assert(merge_test_index(index, merge_index_entries, 41));
	cl_assert(merge_test_names(index, merge_name_entries, 9));
	cl_assert(merge_test_reuc(index, merge_reuc_entries, 10));

	git_index_free(index);
}

void test_merge_trees_renames__no_rename_index(void)
{
	git_index *index;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "68c6c84b091926c7d90aa6a79b2bc3bb6adccd8e", 0, "0a-no-change.txt" },
		{ 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 0, "0b-duplicated-in-ours.txt" },
		{ 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 1, "0b-rewritten-in-ours.txt" },
		{ 0100644, "e376fbdd06ebf021c92724da9f26f44212734e3e", 2, "0b-rewritten-in-ours.txt" },
		{ 0100644, "b2d399ae15224e1d58066e3c8df70ce37de7a656", 3, "0b-rewritten-in-ours.txt" },
		{ 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 0, "0c-duplicated-in-theirs.txt" },
		{ 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 1, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "efc9121fdedaf08ba180b53ebfbcf71bd488ed09", 2, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "712ebba6669ea847d9829e4f1059d6c830c8b531", 3, "0c-rewritten-in-theirs.txt" },
		{ 0100644, "c3d02eeef75183df7584d8d13ac03053910c1301", 0, "1a-newname-in-ours-edited-in-theirs.txt" },
		{ 0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb", 0, "1a-newname-in-ours.txt" },
		{ 0100644, "c3d02eeef75183df7584d8d13ac03053910c1301", 1, "1a-renamed-in-ours-edited-in-theirs.txt" },
		{ 0100644, "0d872f8e871a30208305978ecbf9e66d864f1638", 3, "1a-renamed-in-ours-edited-in-theirs.txt" },
		{ 0100644, "241a1005cd9b980732741b74385b891142bcba28", 0, "1b-newname-in-theirs-edited-in-ours.txt" },
		{ 0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136", 0, "1b-newname-in-theirs.txt" },
		{ 0100644, "241a1005cd9b980732741b74385b891142bcba28", 1, "1b-renamed-in-theirs-edited-in-ours.txt" },
		{ 0100644, "ed9523e62e453e50dd9be1606af19399b96e397a", 2, "1b-renamed-in-theirs-edited-in-ours.txt" },
		{ 0100644, "178940b450f238a56c0d75b7955cb57b38191982", 0, "2-newname-in-both.txt" },
		{ 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 0, "3a-newname-in-ours-deleted-in-theirs.txt" },
		{ 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 0, "3b-newname-in-theirs-deleted-in-ours.txt" },
		{ 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 2, "4a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "8b5b53cb2aa9ceb1139f5312fcfa3cc3c5a47c9a", 3, "4a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "de872ee3618b894992e9d1e18ba2ebe256a112f9", 2, "4b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db", 3, "4b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "d3719a5ae8e4d92276b5313ce976f6ee5af2b436", 2, "5a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "98ba4205fcf31f5dd93c916d35fe3f3b3d0e6714", 3, "5a-newname-in-ours-added-in-theirs.txt" },
		{ 0100644, "385c8a0f26ddf79e9041e15e17dc352ed2c4cced", 2, "5b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "63247125386de9ec90a27ad36169307bf8a11a38", 3, "5b-newname-in-theirs-added-in-ours.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 0, "6-both-renamed-1-to-2-ours.txt" },
		{ 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 0, "6-both-renamed-1-to-2-theirs.txt" },
		{ 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 2, "7-both-renamed.txt" },
		{ 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 3, "7-both-renamed.txt" },
	};

	opts.flags &= ~GIT_MERGE_FIND_RENAMES;

	cl_git_pass(merge_trees_from_branches(&index, repo,
		BRANCH_RENAME_OURS, BRANCH_RENAME_THEIRS,
		&opts));

	cl_assert(merge_test_index(index, merge_index_entries, 32));

	git_index_free(index);
}

void test_merge_trees_renames__submodules(void)
{
	git_index *index;
	git_merge_options *opts = NULL;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "cd3e8d4aa06bdc781f264171030bc28f2b370fee", 0, ".gitmodules" },
		{ 0100644, "4dd1ef7569b18d92d93c0a35bb6b93049137b355", 1, "file.txt" },
		{ 0100644, "a2d8d1824c68541cca94ffb90f79291eba495921", 2, "file.txt" },
		{ 0100644, "63ec604d491161ddafdae4179843c26d54bd999a", 3, "file.txt" },
		{ 0160000, "0000000000000000000000000000000000000001", 1, "submodule1" },
		{ 0160000, "0000000000000000000000000000000000000002", 3, "submodule1" },
		{ 0160000, "0000000000000000000000000000000000000003", 0, "submodule2" },
	};

	cl_git_pass(merge_trees_from_branches(&index, repo,
		"submodule_rename1", "submodule_rename2",
		opts));
	cl_assert(merge_test_index(index, merge_index_entries, 7));
	git_index_free(index);
}

void test_merge_trees_renames__cache_recomputation(void)
{
	git_oid blob, binary, ancestor_oid, theirs_oid, ours_oid;
	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;
	git_buf path = GIT_BUF_INIT;
	git_treebuilder *builder;
	git_tree *ancestor_tree, *their_tree, *our_tree;
	git_index *index;
	size_t blob_size;
	void *data;
	size_t i;

	cl_git_pass(git_oid_fromstr(&blob, "a2d8d1824c68541cca94ffb90f79291eba495921"));

	/*
	 * Create a 50MB blob that consists of NUL bytes only. It is important
	 * that this blob is of a special format, most importantly it cannot
	 * contain more than four non-consecutive newlines or NUL bytes. This
	 * is because of git_hashsig's inner workings where all files with less
	 * than four "lines" are deemed to small.
	 */
	blob_size = 50 * 1024 * 1024;
	cl_assert(data = git__calloc(blob_size, 1));
	cl_git_pass(git_blob_create_from_buffer(&binary, repo, data, blob_size));

	/*
	 * Create the common ancestor, which has 1000 dummy blobs and the binary
	 * blob. The dummy blobs serve as potential rename targets for the
	 * dummy blob.
	 */
	cl_git_pass(git_treebuilder_new(&builder, repo, NULL));
	for (i = 0; i < 1000; i++) {
		cl_git_pass(git_buf_printf(&path, "%"PRIuZ".txt", i));
		cl_git_pass(git_treebuilder_insert(NULL, builder, path.ptr, &blob, GIT_FILEMODE_BLOB));
		git_buf_clear(&path);
	}
	cl_git_pass(git_treebuilder_insert(NULL, builder, "original.bin", &binary, GIT_FILEMODE_BLOB));
	cl_git_pass(git_treebuilder_write(&ancestor_oid, builder));

	/* We now the binary blob in our tree. */
	cl_git_pass(git_treebuilder_remove(builder, "original.bin"));
	cl_git_pass(git_treebuilder_insert(NULL, builder, "renamed.bin", &binary, GIT_FILEMODE_BLOB));
	cl_git_pass(git_treebuilder_write(&ours_oid, builder));

	git_treebuilder_free(builder);

	/* And move everything into a subdirectory in their tree. */
	cl_git_pass(git_treebuilder_new(&builder, repo, NULL));
	cl_git_pass(git_treebuilder_insert(NULL, builder, "subdir", &ancestor_oid, GIT_FILEMODE_TREE));
	cl_git_pass(git_treebuilder_write(&theirs_oid, builder));

	/*
	 * Now merge ancestor, ours and theirs. As `git_hashsig` refuses to
	 * create a hash signature for the 50MB binary file, we historically
	 * didn't cache the hashsig computation for it. As a result, we now
	 * started looking up the 50MB blob and scanning it at least 1000
	 * times, which takes a long time.
	 *
	 * The number of 1000 blobs is chosen in such a way that it's
	 * noticeable when the bug creeps in again, as it takes around 12
	 * minutes on my machine to compute the following merge.
	 */
	opts.target_limit = 5000;
	cl_git_pass(git_tree_lookup(&ancestor_tree, repo, &ancestor_oid));
	cl_git_pass(git_tree_lookup(&their_tree, repo, &theirs_oid));
	cl_git_pass(git_tree_lookup(&our_tree, repo, &ours_oid));
	cl_git_pass(git_merge_trees(&index, repo, ancestor_tree, our_tree, their_tree, &opts));

	git_treebuilder_free(builder);
	git_buf_dispose(&path);
	git_index_free(index);
	git_tree_free(ancestor_tree);
	git_tree_free(their_tree);
	git_tree_free(our_tree);
	git__free(data);
}
