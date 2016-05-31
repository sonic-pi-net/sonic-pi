#include "clar_libgit2.h"
#include "git2/tree.h"
#include "merge.h"
#include "../merge_helpers.h"
#include "diff.h"
#include "git2/sys/hashsig.h"

static git_repository *repo;

#define TEST_REPO_PATH "merge-resolve"

#define TREE_OID_ANCESTOR		"0d52e3a556e189ba0948ae56780918011c1b167d"
#define TREE_OID_MASTER			"1f81433e3161efbf250576c58fede7f6b836f3d3"
#define TREE_OID_BRANCH			"eea9286df54245fea72c5b557291470eb825f38f"
#define TREE_OID_RENAMES1		"f5f9dd5886a6ee20272be0aafc790cba43b31931"
#define TREE_OID_RENAMES2		"5fbfbdc04b4eca46f54f4853a3c5a1dce28f5165"

#define TREE_OID_DF_ANCESTOR	"b8a3a806d3950e8c0a03a34f234a92eff0e2c68d"
#define TREE_OID_DF_SIDE1		"ee1d6f164893c1866a323f072eeed36b855656be"
#define TREE_OID_DF_SIDE2		"6178885b38fe96e825ac0f492c0a941f288b37f6"

#define TREE_OID_RENAME_CONFLICT_ANCESTOR	"476dbb3e207313d1d8aaa120c6ad204bf1295e53"
#define TREE_OID_RENAME_CONFLICT_OURS		"c4efe31e9decccc8b2b4d3df9aac2cdfe2995618"
#define TREE_OID_RENAME_CONFLICT_THEIRS		"9e7f4359c469f309b6057febf4c6e80742cbed5b"

void test_merge_trees_treediff__initialize(void)
{
	repo = cl_git_sandbox_init(TEST_REPO_PATH);
}

void test_merge_trees_treediff__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void test_find_differences(
    const char *ancestor_oidstr,
    const char *ours_oidstr,
    const char *theirs_oidstr,
    struct merge_index_conflict_data *treediff_conflict_data,
    size_t treediff_conflict_data_len)
{
    git_merge_diff_list *merge_diff_list = git_merge_diff_list__alloc(repo);
    git_oid ancestor_oid, ours_oid, theirs_oid;
    git_tree *ancestor_tree, *ours_tree, *theirs_tree;
	git_iterator *ancestor_iter, *ours_iter, *theirs_iter;
	git_iterator_options iter_opts = GIT_ITERATOR_OPTIONS_INIT;

	git_merge_options opts = GIT_MERGE_OPTIONS_INIT;
	opts.flags |= GIT_MERGE_FIND_RENAMES;
	opts.target_limit = 1000;
	opts.rename_threshold = 50;

	opts.metric = git__malloc(sizeof(git_diff_similarity_metric));
	cl_assert(opts.metric != NULL);

	opts.metric->file_signature = git_diff_find_similar__hashsig_for_file;
	opts.metric->buffer_signature = git_diff_find_similar__hashsig_for_buf;
	opts.metric->free_signature = git_diff_find_similar__hashsig_free;
	opts.metric->similarity = git_diff_find_similar__calc_similarity;
	opts.metric->payload = (void *)GIT_HASHSIG_SMART_WHITESPACE;

	cl_git_pass(git_oid_fromstr(&ancestor_oid, ancestor_oidstr));
	cl_git_pass(git_oid_fromstr(&ours_oid, ours_oidstr));
	cl_git_pass(git_oid_fromstr(&theirs_oid, theirs_oidstr));

	cl_git_pass(git_tree_lookup(&ancestor_tree, repo, &ancestor_oid));
	cl_git_pass(git_tree_lookup(&ours_tree, repo, &ours_oid));
	cl_git_pass(git_tree_lookup(&theirs_tree, repo, &theirs_oid));

	iter_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

	cl_git_pass(git_iterator_for_tree(&ancestor_iter, ancestor_tree, &iter_opts));
	cl_git_pass(git_iterator_for_tree(&ours_iter, ours_tree, &iter_opts));
	cl_git_pass(git_iterator_for_tree(&theirs_iter, theirs_tree, &iter_opts));

	cl_git_pass(git_merge_diff_list__find_differences(merge_diff_list, ancestor_iter, ours_iter, theirs_iter));
	cl_git_pass(git_merge_diff_list__find_renames(repo, merge_diff_list, &opts));

	/*
	dump_merge_index(merge_index);
	 */

	cl_assert(treediff_conflict_data_len == merge_diff_list->conflicts.length);

	cl_assert(merge_test_merge_conflicts(&merge_diff_list->conflicts, treediff_conflict_data, treediff_conflict_data_len));

	git_iterator_free(ancestor_iter);
	git_iterator_free(ours_iter);
	git_iterator_free(theirs_iter);

	git_tree_free(ancestor_tree);
	git_tree_free(ours_tree);
	git_tree_free(theirs_tree);

	git_merge_diff_list__free(merge_diff_list);

	git__free(opts.metric);
}

void test_merge_trees_treediff__simple(void)
{
	struct merge_index_conflict_data treediff_conflict_data[] = {
		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "233c0919c998ed110a4b6ff36f353aec8b713487", 0, "added-in-master.txt" }, GIT_DELTA_ADDED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE
		},

		{
			{ { 0100644, "6212c31dab5e482247d7977e4f0dd3601decf13b", 0, "automergeable.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf", 0, "automergeable.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "058541fc37114bfc1dddf6bd6bffc7fae5c2e6fe", 0, "automergeable.txt" }, GIT_DELTA_MODIFIED },
			GIT_MERGE_DIFF_BOTH_MODIFIED
		},

		{
			{ { 0100644, "ab6c44a2e84492ad4b41bb6bac87353e9d02ac8b", 0, "changed-in-branch.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "ab6c44a2e84492ad4b41bb6bac87353e9d02ac8b", 0, "changed-in-branch.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "4eb04c9e79e88f6640d01ff5b25ca2a60764f216", 0, "changed-in-branch.txt" }, GIT_DELTA_MODIFIED },
			GIT_MERGE_DIFF_NONE
		},

		{
			{ { 0100644, "ab6c44a2e84492ad4b41bb6bac87353e9d02ac8b", 0, "changed-in-master.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "11deab00b2d3a6f5a3073988ac050c2d7b6655e2", 0, "changed-in-master.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "ab6c44a2e84492ad4b41bb6bac87353e9d02ac8b", 0, "changed-in-master.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE
        },

		{
			{ { 0100644, "d427e0b2e138501a3d15cc376077a3631e15bd46", 0, "conflicting.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "4e886e602529caa9ab11d71f86634bd1b6e0de10", 0, "conflicting.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "2bd0a343aeef7a2cf0d158478966a6e587ff3863", 0, "conflicting.txt" }, GIT_DELTA_MODIFIED },
			GIT_MERGE_DIFF_BOTH_MODIFIED
        },

		{
			{ { 0100644, "dfe3f22baa1f6fce5447901c3086bae368de6bdd", 0, "removed-in-branch.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "dfe3f22baa1f6fce5447901c3086bae368de6bdd", 0, "removed-in-branch.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			GIT_MERGE_DIFF_NONE
        },

		{
			{ { 0100644, "5c3b68a71fc4fa5d362fd3875e53137c6a5ab7a5", 0, "removed-in-master.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			{ { 0100644, "5c3b68a71fc4fa5d362fd3875e53137c6a5ab7a5", 0, "removed-in-master.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE
		},
    };

    test_find_differences(TREE_OID_ANCESTOR, TREE_OID_MASTER, TREE_OID_BRANCH, treediff_conflict_data, 7);
}

void test_merge_trees_treediff__df_conflicts(void)
{
	struct merge_index_conflict_data treediff_conflict_data[] = {
		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "49130a28ef567af9a6a6104c38773fedfa5f9742", 0, "dir-10" }, GIT_DELTA_ADDED },
			{ { 0100644, "6c06dcd163587c2cc18be44857e0b71116382aeb", 0, "dir-10" }, GIT_DELTA_ADDED },
			GIT_MERGE_DIFF_BOTH_ADDED,
		},

		{
			{ { 0100644, "242591eb280ee9eeb2ce63524b9a8b9bc4cb515d", 0, "dir-10/file.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			GIT_MERGE_DIFF_BOTH_DELETED,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "43aafd43bea779ec74317dc361f45ae3f532a505", 0, "dir-6" }, GIT_DELTA_ADDED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "cf8c5cc8a85a1ff5a4ba51e0bc7cf5665669924d", 0, "dir-6/file.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "cf8c5cc8a85a1ff5a4ba51e0bc7cf5665669924d", 0, "dir-6/file.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "a031a28ae70e33a641ce4b8a8f6317f1ab79dee4", 0, "dir-7" }, GIT_DELTA_ADDED },
			GIT_MERGE_DIFF_DIRECTORY_FILE,
		},

		{
			{ { 0100644, "5012fd565b1393bdfda1805d4ec38ce6619e1fd1", 0, "dir-7/file.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "a5563304ddf6caba25cb50323a2ea6f7dbfcadca", 0, "dir-7/file.txt" }, GIT_DELTA_MODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			GIT_MERGE_DIFF_DF_CHILD,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "e9ad6ec3e38364a3d07feda7c4197d4d845c53b5", 0, "dir-8" }, GIT_DELTA_ADDED },
			{ {0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "f20c9063fa0bda9a397c96947a7b687305c49753", 0, "dir-8/file.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			{ { 0100644, "f20c9063fa0bda9a397c96947a7b687305c49753", 0, "dir-8/file.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "3ef4d30382ca33fdeba9fda895a99e0891ba37aa", 0, "dir-9" }, GIT_DELTA_ADDED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_DIRECTORY_FILE,
		},

		{
			{ { 0100644, "fc4c636d6515e9e261f9260dbcf3cc6eca97ea08", 0, "dir-9/file.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			{ { 0100644, "76ab0e2868197ec158ddd6c78d8a0d2fd73d38f9", 0, "dir-9/file.txt" }, GIT_DELTA_MODIFIED },
			GIT_MERGE_DIFF_DF_CHILD,
		},

		{
			{ { 0100644, "1e4ff029aee68d0d69ef9eb6efa6cbf1ec732f99", 0, "file-1" },  GIT_DELTA_UNMODIFIED },
			{ { 0100644, "1e4ff029aee68d0d69ef9eb6efa6cbf1ec732f99", 0, "file-1" },  GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "5c2411f8075f48a6b2fdb85ebc0d371747c4df15", 0, "file-1/new" }, GIT_DELTA_ADDED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "a39a620dae5bc8b4e771cd4d251b7d080401a21e", 0, "file-2" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "d963979c237d08b6ba39062ee7bf64c7d34a27f8", 0, "file-2" }, GIT_DELTA_MODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			GIT_MERGE_DIFF_DIRECTORY_FILE,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "5c341ead2ba6f2af98ce5ec3fe84f6b6d2899c0d", 0, "file-2/new" }, GIT_DELTA_ADDED },
			GIT_MERGE_DIFF_DF_CHILD,
		},

		{
			{ { 0100644, "032ebc5ab85d9553bb187d3cd40875ff23a63ed0", 0, "file-3" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			{ { 0100644, "032ebc5ab85d9553bb187d3cd40875ff23a63ed0", 0, "file-3" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "9efe7723802d4305142eee177e018fee1572c4f4", 0, "file-3/new" }, GIT_DELTA_ADDED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "bacac9b3493509aa15e1730e1545fc0919d1dae0", 0, "file-4" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			{ { 0100644, "7663fce0130db092936b137cabd693ec234eb060", 0, "file-4" }, GIT_DELTA_MODIFIED },
			GIT_MERGE_DIFF_DIRECTORY_FILE,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "e49f917b448d1340b31d76e54ba388268fd4c922", 0, "file-4/new" }, GIT_DELTA_ADDED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_DF_CHILD,
		},

		{
			{ { 0100644, "ac4045f965119e6998f4340ed0f411decfb3ec05", 0, "file-5" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			GIT_MERGE_DIFF_BOTH_DELETED,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "cab2cf23998b40f1af2d9d9a756dc9e285a8df4b", 0, "file-5/new" }, GIT_DELTA_ADDED },
			{ { 0100644, "f5504f36e6f4eb797a56fc5bac6c6c7f32969bf2", 0, "file-5/new" }, GIT_DELTA_ADDED },
			GIT_MERGE_DIFF_BOTH_ADDED,
		},
	};

	test_find_differences(TREE_OID_DF_ANCESTOR, TREE_OID_DF_SIDE1, TREE_OID_DF_SIDE2, treediff_conflict_data, 20);
}

void test_merge_trees_treediff__strict_renames(void)
{
	struct merge_index_conflict_data treediff_conflict_data[] = {
		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "233c0919c998ed110a4b6ff36f353aec8b713487", 0, "added-in-master.txt" }, GIT_DELTA_ADDED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "6212c31dab5e482247d7977e4f0dd3601decf13b", 0, "automergeable.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf", 0, "automergeable.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "6212c31dab5e482247d7977e4f0dd3601decf13b", 0, "automergeable.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "ab6c44a2e84492ad4b41bb6bac87353e9d02ac8b", 0, "changed-in-master.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "11deab00b2d3a6f5a3073988ac050c2d7b6655e2", 0, "changed-in-master.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "ab6c44a2e84492ad4b41bb6bac87353e9d02ac8b", 0, "changed-in-master.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "d427e0b2e138501a3d15cc376077a3631e15bd46", 0, "conflicting.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "4e886e602529caa9ab11d71f86634bd1b6e0de10", 0, "conflicting.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "d427e0b2e138501a3d15cc376077a3631e15bd46", 0, "conflicting.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "dfe3f22baa1f6fce5447901c3086bae368de6bdd", 0, "removed-in-branch.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "dfe3f22baa1f6fce5447901c3086bae368de6bdd", 0, "removed-in-branch.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "dfe3f22baa1f6fce5447901c3086bae368de6bdd", 0, "renamed-in-branch.txt" }, GIT_DELTA_RENAMED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "5c3b68a71fc4fa5d362fd3875e53137c6a5ab7a5", 0, "removed-in-master.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			{ { 0100644,  "5c3b68a71fc4fa5d362fd3875e53137c6a5ab7a5", 0, "removed-in-master.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "c8f06f2e3bb2964174677e91f0abead0e43c9e5d", 0, "renamed.txt" }, GIT_DELTA_ADDED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "c8f06f2e3bb2964174677e91f0abead0e43c9e5d", 0, "unchanged.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "c8f06f2e3bb2964174677e91f0abead0e43c9e5d", 0, "unchanged.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "c8f06f2e3bb2964174677e91f0abead0e43c9e5d", 0, "copied.txt" }, GIT_DELTA_RENAMED },
			GIT_MERGE_DIFF_NONE,
		},
    };

	test_find_differences(TREE_OID_ANCESTOR, TREE_OID_MASTER, TREE_OID_RENAMES1, treediff_conflict_data, 8);
}

void test_merge_trees_treediff__rename_conflicts(void)
{
	struct merge_index_conflict_data treediff_conflict_data[] = {
		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 0, "0b-duplicated-in-ours.txt" }, GIT_DELTA_ADDED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "f0ce2b8e4986084d9b308fb72709e414c23eb5e6", 0, "0b-rewritten-in-ours.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "e376fbdd06ebf021c92724da9f26f44212734e3e", 0, "0b-rewritten-in-ours.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "b2d399ae15224e1d58066e3c8df70ce37de7a656", 0, "0b-rewritten-in-ours.txt" }, GIT_DELTA_MODIFIED },
			GIT_MERGE_DIFF_BOTH_MODIFIED,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 0, "0c-duplicated-in-theirs.txt" }, GIT_DELTA_ADDED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "2f56120107d680129a5d9791b521cb1e73a2ed31", 0, "0c-rewritten-in-theirs.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "efc9121fdedaf08ba180b53ebfbcf71bd488ed09", 0, "0c-rewritten-in-theirs.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "712ebba6669ea847d9829e4f1059d6c830c8b531", 0, "0c-rewritten-in-theirs.txt" }, GIT_DELTA_MODIFIED },
			GIT_MERGE_DIFF_BOTH_MODIFIED,
		},

		{
			{ { 0100644, "c3d02eeef75183df7584d8d13ac03053910c1301", 0, "1a-renamed-in-ours-edited-in-theirs.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "c3d02eeef75183df7584d8d13ac03053910c1301", 0, "1a-newname-in-ours-edited-in-theirs.txt" }, GIT_DELTA_RENAMED },
			{ { 0100644, "0d872f8e871a30208305978ecbf9e66d864f1638", 0, "1a-renamed-in-ours-edited-in-theirs.txt" }, GIT_DELTA_MODIFIED },
			GIT_MERGE_DIFF_RENAMED_MODIFIED,
		},

		{
			{ { 0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb", 0, "1a-renamed-in-ours.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb", 0, "1a-newname-in-ours.txt" }, GIT_DELTA_RENAMED },
			{ { 0100644, "d0d4594e16f2e19107e3fa7ea63e7aaaff305ffb", 0, "1a-renamed-in-ours.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "241a1005cd9b980732741b74385b891142bcba28", 0, "1b-renamed-in-theirs-edited-in-ours.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "ed9523e62e453e50dd9be1606af19399b96e397a", 0, "1b-renamed-in-theirs-edited-in-ours.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "241a1005cd9b980732741b74385b891142bcba28", 0, "1b-newname-in-theirs-edited-in-ours.txt" }, GIT_DELTA_RENAMED },
			GIT_MERGE_DIFF_RENAMED_MODIFIED,
		},

		{
			{ { 0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136", 0, "1b-renamed-in-theirs.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136", 0, "1b-renamed-in-theirs.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "2b5f1f181ee3b58ea751f5dd5d8f9b445520a136", 0, "1b-newname-in-theirs.txt" }, GIT_DELTA_RENAMED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "178940b450f238a56c0d75b7955cb57b38191982", 0, "2-renamed-in-both.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "178940b450f238a56c0d75b7955cb57b38191982", 0, "2-newname-in-both.txt" }, GIT_DELTA_RENAMED },
			{ { 0100644, "178940b450f238a56c0d75b7955cb57b38191982", 0, "2-newname-in-both.txt" }, GIT_DELTA_RENAMED },
			GIT_MERGE_DIFF_BOTH_RENAMED,
		},

		{
			{ { 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 0, "3a-renamed-in-ours-deleted-in-theirs.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "18cb316b1cefa0f8a6946f0e201a8e1a6f845ab9", 0, "3a-newname-in-ours-deleted-in-theirs.txt" }, GIT_DELTA_RENAMED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			GIT_MERGE_DIFF_RENAMED_DELETED,
		},

		{
			{ { 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 0, "3b-renamed-in-theirs-deleted-in-ours.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			{ { 0100644, "36219b49367146cb2e6a1555b5a9ebd4d0328495", 0, "3b-newname-in-theirs-deleted-in-ours.txt" }, GIT_DELTA_RENAMED },
			GIT_MERGE_DIFF_RENAMED_DELETED,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "8b5b53cb2aa9ceb1139f5312fcfa3cc3c5a47c9a", 0, "4a-newname-in-ours-added-in-theirs.txt" }, GIT_DELTA_ADDED },
			GIT_MERGE_DIFF_RENAMED_ADDED,
		},

		{
			{ { 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 0, "4a-renamed-in-ours-added-in-theirs.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "227792b52aaa0b238bea00ec7e509b02623f168c", 0, "4a-newname-in-ours-added-in-theirs.txt" }, GIT_DELTA_RENAMED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			GIT_MERGE_DIFF_RENAMED_ADDED,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "de872ee3618b894992e9d1e18ba2ebe256a112f9", 0, "4b-newname-in-theirs-added-in-ours.txt" }, GIT_DELTA_ADDED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_RENAMED_ADDED,
		},

		{
			{ { 0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db", 0, "4b-renamed-in-theirs-added-in-ours.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			{ { 0100644, "98d52d07c0b0bbf2b46548f6aa521295c2cb55db", 0, "4b-newname-in-theirs-added-in-ours.txt" }, GIT_DELTA_RENAMED },
			GIT_MERGE_DIFF_RENAMED_ADDED,
		},

		{
			{ { 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 0, "5-both-renamed-1-to-2.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 0, "5-both-renamed-1-to-2-ours.txt" }, GIT_DELTA_RENAMED },
			{ { 0100644, "d8fa77b6833082c1ea36b7828a582d4c43882450", 0, "5-both-renamed-1-to-2-theirs.txt" }, GIT_DELTA_RENAMED },
			GIT_MERGE_DIFF_BOTH_RENAMED_1_TO_2,
		},

		{
			{ { 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 0, "6-both-renamed-side-1.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 0, "6-both-renamed.txt" }, GIT_DELTA_RENAMED },
			{ { 0100644, "b42712cfe99a1a500b2a51fe984e0b8a7702ba11", 0, "6-both-renamed-side-1.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_BOTH_RENAMED_2_TO_1,
		},

		{
			{ { 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 0, "6-both-renamed-side-2.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 0, "6-both-renamed-side-2.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "b69fe837e4cecfd4c9a40cdca7c138468687df07", 0, "6-both-renamed.txt" }, GIT_DELTA_RENAMED },
			GIT_MERGE_DIFF_BOTH_RENAMED_2_TO_1,
		},
    };
	test_find_differences(TREE_OID_RENAME_CONFLICT_ANCESTOR,
		TREE_OID_RENAME_CONFLICT_OURS, TREE_OID_RENAME_CONFLICT_THEIRS, treediff_conflict_data, 18);
}

void test_merge_trees_treediff__best_renames(void)
{
	struct merge_index_conflict_data treediff_conflict_data[] = {
		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "233c0919c998ed110a4b6ff36f353aec8b713487", 0, "added-in-master.txt" }, GIT_DELTA_ADDED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "6212c31dab5e482247d7977e4f0dd3601decf13b", 0, "automergeable.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf", 0, "automergeable.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "45299c1ca5e07bba1fd90843056fb559f96b1f5a", 0, "renamed-90.txt" }, GIT_DELTA_RENAMED },
			GIT_MERGE_DIFF_RENAMED_MODIFIED,
        },

		{
			{ { 0100644, "ab6c44a2e84492ad4b41bb6bac87353e9d02ac8b", 0, "changed-in-master.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "11deab00b2d3a6f5a3073988ac050c2d7b6655e2", 0, "changed-in-master.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "ab6c44a2e84492ad4b41bb6bac87353e9d02ac8b", 0, "changed-in-master.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
        },

		{
			{ { 0100644, "d427e0b2e138501a3d15cc376077a3631e15bd46", 0, "conflicting.txt" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "4e886e602529caa9ab11d71f86634bd1b6e0de10", 0, "conflicting.txt" }, GIT_DELTA_MODIFIED },
			{ { 0100644, "d427e0b2e138501a3d15cc376077a3631e15bd46", 0, "conflicting.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0100644, "5c3b68a71fc4fa5d362fd3875e53137c6a5ab7a5", 0, "removed-in-master.txt" },GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_DELETED },
			{ { 0100644, "5c3b68a71fc4fa5d362fd3875e53137c6a5ab7a5", 0, "removed-in-master.txt" }, GIT_DELTA_UNMODIFIED },
			GIT_MERGE_DIFF_MODIFIED_DELETED,
        },

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "5843febcb23480df0b5edb22a21c59c772bb8e29", 0, "renamed-50.txt" }, GIT_DELTA_ADDED },
			GIT_MERGE_DIFF_NONE,
		},

		{
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0, "", 0, "" }, GIT_DELTA_UNMODIFIED },
			{ { 0100644, "a77a56a49f8f3ae242e02717f18ebbc60c5cc543", 0, "renamed-75.txt" }, GIT_DELTA_ADDED },
			GIT_MERGE_DIFF_NONE,
		},
    };

	test_find_differences(TREE_OID_ANCESTOR, TREE_OID_MASTER, TREE_OID_RENAMES2, treediff_conflict_data, 7);
}
