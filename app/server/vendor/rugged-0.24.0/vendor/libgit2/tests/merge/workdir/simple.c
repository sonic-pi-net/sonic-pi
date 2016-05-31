#include "clar_libgit2.h"
#include "git2/repository.h"
#include "git2/merge.h"
#include "buffer.h"
#include "merge.h"
#include "../merge_helpers.h"
#include "../conflict_data.h"
#include "refs.h"
#include "fileops.h"

static git_repository *repo;
static git_index *repo_index;

#define TEST_REPO_PATH "merge-resolve"
#define TEST_INDEX_PATH TEST_REPO_PATH "/.git/index"

#define THEIRS_SIMPLE_BRANCH		"branch"
#define THEIRS_SIMPLE_OID			"7cb63eed597130ba4abb87b3e544b85021905520"

#define THEIRS_UNRELATED_BRANCH		"unrelated"
#define THEIRS_UNRELATED_OID		"55b4e4687e7a0d9ca367016ed930f385d4022e6f"
#define THEIRS_UNRELATED_PARENT		"d6cf6c7741b3316826af1314042550c97ded1d50"

#define OURS_DIRECTORY_FILE			"df_side1"
#define THEIRS_DIRECTORY_FILE		"fc90237dc4891fa6c69827fc465632225e391618"


/* Non-conflicting files, index entries are common to every merge operation */
#define ADDED_IN_MASTER_INDEX_ENTRY	\
	{ 0100644, "233c0919c998ed110a4b6ff36f353aec8b713487", 0, \
	  "added-in-master.txt" }
#define AUTOMERGEABLE_INDEX_ENTRY \
	{ 0100644, "f2e1550a0c9e53d5811175864a29536642ae3821", 0, \
	  "automergeable.txt" }
#define CHANGED_IN_BRANCH_INDEX_ENTRY \
	{ 0100644, "4eb04c9e79e88f6640d01ff5b25ca2a60764f216", 0, \
	  "changed-in-branch.txt" }
#define CHANGED_IN_MASTER_INDEX_ENTRY \
	{ 0100644, "11deab00b2d3a6f5a3073988ac050c2d7b6655e2", 0, \
	  "changed-in-master.txt" }
#define UNCHANGED_INDEX_ENTRY \
	{ 0100644, "c8f06f2e3bb2964174677e91f0abead0e43c9e5d", 0, \
	  "unchanged.txt" }

/* Unrelated files */
#define UNRELATED_NEW1 \
	{ 0100644, "ef58fdd8086c243bdc81f99e379acacfd21d32d6", 0, \
	  "new-in-unrelated1.txt" }
#define UNRELATED_NEW2 \
	{ 0100644, "948ba6e701c1edab0c2d394fb7c5538334129793", 0, \
	  "new-in-unrelated2.txt" }

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


// Fixture setup and teardown
void test_merge_workdir_simple__initialize(void)
{
	git_config *cfg;

	repo = cl_git_sandbox_init(TEST_REPO_PATH);
	git_repository_index(&repo_index, repo);

	/* Ensure that the user's merge.conflictstyle doesn't interfere */
	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_set_string(cfg, "merge.conflictstyle", "merge"));
	git_config_free(cfg);
}

void test_merge_workdir_simple__cleanup(void)
{
	git_index_free(repo_index);
	cl_git_sandbox_cleanup();
}

static void merge_simple_branch(int merge_file_favor, int addl_checkout_strategy)
{
	git_oid their_oids[1];
	git_annotated_commit *their_heads[1];
	git_merge_options merge_opts = GIT_MERGE_OPTIONS_INIT;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;

	cl_git_pass(git_oid_fromstr(&their_oids[0], THEIRS_SIMPLE_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[0], repo, &their_oids[0]));

	merge_opts.file_favor = merge_file_favor;
	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE | GIT_CHECKOUT_ALLOW_CONFLICTS |
		addl_checkout_strategy;

	cl_git_pass(git_merge(repo, (const git_annotated_commit **)their_heads, 1, &merge_opts, &checkout_opts));

	git_annotated_commit_free(their_heads[0]);
}

static void set_core_autocrlf_to(git_repository *repo, bool value)
{
	git_config *cfg;

	cl_git_pass(git_repository_config(&cfg, repo));
	cl_git_pass(git_config_set_bool(cfg, "core.autocrlf", value));

	git_config_free(cfg);
}

void test_merge_workdir_simple__automerge(void)
{
	git_index *index;
	const git_index_entry *entry;
	git_buf automergeable_buf = GIT_BUF_INIT;

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


	set_core_autocrlf_to(repo, false);

	merge_simple_branch(0, 0);

	cl_git_pass(git_futils_readbuffer(&automergeable_buf,
		TEST_REPO_PATH "/automergeable.txt"));
	cl_assert(strcmp(automergeable_buf.ptr, AUTOMERGEABLE_MERGED_FILE) == 0);
	git_buf_free(&automergeable_buf);

	cl_assert(merge_test_index(repo_index, merge_index_entries, 8));
	cl_assert(merge_test_reuc(repo_index, merge_reuc_entries, 3));

	git_repository_index(&index, repo);

	cl_assert((entry = git_index_get_bypath(index, "automergeable.txt", 0)) != NULL);
	cl_assert(entry->file_size == strlen(AUTOMERGEABLE_MERGED_FILE));

	git_index_free(index);
}

void test_merge_workdir_simple__automerge_crlf(void)
{
#ifdef GIT_WIN32
	git_index *index;
	const git_index_entry *entry;
	git_buf automergeable_buf = GIT_BUF_INIT;

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

	set_core_autocrlf_to(repo, true);

	merge_simple_branch(0, 0);

	cl_git_pass(git_futils_readbuffer(&automergeable_buf,
		TEST_REPO_PATH "/automergeable.txt"));
	cl_assert(strcmp(automergeable_buf.ptr, AUTOMERGEABLE_MERGED_FILE_CRLF) == 0);
	git_buf_free(&automergeable_buf);

	cl_assert(merge_test_index(repo_index, merge_index_entries, 8));
	cl_assert(merge_test_reuc(repo_index, merge_reuc_entries, 3));

	git_repository_index(&index, repo);

	cl_assert((entry = git_index_get_bypath(index, "automergeable.txt", 0)) != NULL);
	cl_assert(entry->file_size == strlen(AUTOMERGEABLE_MERGED_FILE_CRLF));

	git_index_free(index);
#endif /* GIT_WIN32 */
}

void test_merge_workdir_simple__mergefile(void)
{
	git_buf conflicting_buf = GIT_BUF_INIT, mergemsg_buf = GIT_BUF_INIT;

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

	set_core_autocrlf_to(repo, false);

	merge_simple_branch(0, 0);

	cl_git_pass(git_futils_readbuffer(&conflicting_buf,
		TEST_REPO_PATH "/conflicting.txt"));
	cl_assert(strcmp(conflicting_buf.ptr, CONFLICTING_MERGE_FILE) == 0);
	cl_git_pass(git_futils_readbuffer(&mergemsg_buf,
		TEST_REPO_PATH "/.git/MERGE_MSG"));
	cl_assert(strcmp(git_buf_cstr(&mergemsg_buf),
		"Merge commit '7cb63eed597130ba4abb87b3e544b85021905520'\n" \
		"\n" \
		"Conflicts:\n" \
		"\tconflicting.txt\n") == 0);
	git_buf_free(&conflicting_buf);
	git_buf_free(&mergemsg_buf);

	cl_assert(merge_test_index(repo_index, merge_index_entries, 8));
	cl_assert(merge_test_reuc(repo_index, merge_reuc_entries, 3));
}

void test_merge_workdir_simple__diff3(void)
{
	git_buf conflicting_buf = GIT_BUF_INIT;

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

	set_core_autocrlf_to(repo, false);

	merge_simple_branch(0, GIT_CHECKOUT_CONFLICT_STYLE_DIFF3);

	cl_git_pass(git_futils_readbuffer(&conflicting_buf,
		TEST_REPO_PATH "/conflicting.txt"));
	cl_assert(strcmp(conflicting_buf.ptr, CONFLICTING_DIFF3_FILE) == 0);
	git_buf_free(&conflicting_buf);

	cl_assert(merge_test_index(repo_index, merge_index_entries, 8));
	cl_assert(merge_test_reuc(repo_index, merge_reuc_entries, 3));
}

void test_merge_workdir_simple__union(void)
{
	git_buf conflicting_buf = GIT_BUF_INIT;

	struct merge_index_entry merge_index_entries[] = {
		ADDED_IN_MASTER_INDEX_ENTRY,
		AUTOMERGEABLE_INDEX_ENTRY,
		CHANGED_IN_BRANCH_INDEX_ENTRY,
		CHANGED_IN_MASTER_INDEX_ENTRY,

		{ 0100644, "72cdb057b340205164478565e91eb71647e66891", 0, "conflicting.txt" },

		UNCHANGED_INDEX_ENTRY,
	};

	struct merge_reuc_entry merge_reuc_entries[] = {
		AUTOMERGEABLE_REUC_ENTRY,
		CONFLICTING_REUC_ENTRY,
		REMOVED_IN_BRANCH_REUC_ENTRY,
		REMOVED_IN_MASTER_REUC_ENTRY
	};

	set_core_autocrlf_to(repo, false);

	merge_simple_branch(GIT_MERGE_FILE_FAVOR_UNION, 0);

	cl_git_pass(git_futils_readbuffer(&conflicting_buf,
		TEST_REPO_PATH "/conflicting.txt"));
	cl_assert(strcmp(conflicting_buf.ptr, CONFLICTING_UNION_FILE) == 0);
	git_buf_free(&conflicting_buf);

	cl_assert(merge_test_index(repo_index, merge_index_entries, 6));
	cl_assert(merge_test_reuc(repo_index, merge_reuc_entries, 4));
}

void test_merge_workdir_simple__diff3_from_config(void)
{
	git_config *config;
	git_buf conflicting_buf = GIT_BUF_INIT;

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

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_string(config, "merge.conflictstyle", "diff3"));

	set_core_autocrlf_to(repo, false);

	merge_simple_branch(0, 0);

	cl_git_pass(git_futils_readbuffer(&conflicting_buf,
		TEST_REPO_PATH "/conflicting.txt"));
	cl_assert(strcmp(conflicting_buf.ptr, CONFLICTING_DIFF3_FILE) == 0);
	git_buf_free(&conflicting_buf);

	cl_assert(merge_test_index(repo_index, merge_index_entries, 8));
	cl_assert(merge_test_reuc(repo_index, merge_reuc_entries, 3));

	git_config_free(config);
}

void test_merge_workdir_simple__merge_overrides_config(void)
{
	git_config *config;
	git_buf conflicting_buf = GIT_BUF_INIT;

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

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_string(config, "merge.conflictstyle", "diff3"));

	set_core_autocrlf_to(repo, false);

	merge_simple_branch(0, GIT_CHECKOUT_CONFLICT_STYLE_MERGE);

	cl_git_pass(git_futils_readbuffer(&conflicting_buf,
		TEST_REPO_PATH "/conflicting.txt"));
	cl_assert(strcmp(conflicting_buf.ptr, CONFLICTING_MERGE_FILE) == 0);
	git_buf_free(&conflicting_buf);

	cl_assert(merge_test_index(repo_index, merge_index_entries, 8));
	cl_assert(merge_test_reuc(repo_index, merge_reuc_entries, 3));

	git_config_free(config);
}

void test_merge_workdir_simple__checkout_ours(void)
{
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

	merge_simple_branch(0, GIT_CHECKOUT_SAFE | GIT_CHECKOUT_USE_OURS);

	cl_assert(merge_test_index(repo_index, merge_index_entries, 8));
	cl_assert(merge_test_reuc(repo_index, merge_reuc_entries, 3));

	cl_assert(git_path_exists(TEST_REPO_PATH "/conflicting.txt"));
}

void test_merge_workdir_simple__favor_ours(void)
{
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

	merge_simple_branch(GIT_MERGE_FILE_FAVOR_OURS, 0);

	cl_assert(merge_test_index(repo_index, merge_index_entries, 6));
	cl_assert(merge_test_reuc(repo_index, merge_reuc_entries, 4));
}

void test_merge_workdir_simple__favor_theirs(void)
{
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

	merge_simple_branch(GIT_MERGE_FILE_FAVOR_THEIRS, 0);

	cl_assert(merge_test_index(repo_index, merge_index_entries, 6));
	cl_assert(merge_test_reuc(repo_index, merge_reuc_entries, 4));
}

void test_merge_workdir_simple__directory_file(void)
{
	git_reference *head;
	git_oid their_oids[1], head_commit_id;
	git_annotated_commit *their_heads[1];
	git_merge_options merge_opts = GIT_MERGE_OPTIONS_INIT;
	git_commit *head_commit;

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

	cl_git_pass(git_reference_symbolic_create(&head, repo, GIT_HEAD_FILE, GIT_REFS_HEADS_DIR OURS_DIRECTORY_FILE, 1, NULL));
	cl_git_pass(git_reference_name_to_id(&head_commit_id, repo, GIT_HEAD_FILE));
	cl_git_pass(git_commit_lookup(&head_commit, repo, &head_commit_id));
	cl_git_pass(git_reset(repo, (git_object *)head_commit, GIT_RESET_HARD, NULL));

	cl_git_pass(git_oid_fromstr(&their_oids[0], THEIRS_DIRECTORY_FILE));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[0], repo, &their_oids[0]));

	merge_opts.file_favor = 0;
	cl_git_pass(git_merge(repo, (const git_annotated_commit **)their_heads, 1, &merge_opts, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 20));

	git_reference_free(head);
	git_commit_free(head_commit);
	git_annotated_commit_free(their_heads[0]);
}

void test_merge_workdir_simple__unrelated(void)
{
	git_oid their_oids[1];
	git_annotated_commit *their_heads[1];
	git_merge_options merge_opts = GIT_MERGE_OPTIONS_INIT;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "233c0919c998ed110a4b6ff36f353aec8b713487", 0, "added-in-master.txt" },
		{ 0100644, "ee3fa1b8c00aff7fe02065fdb50864bb0d932ccf", 0, "automergeable.txt" },
		{ 0100644, "ab6c44a2e84492ad4b41bb6bac87353e9d02ac8b", 0, "changed-in-branch.txt" },
		{ 0100644, "11deab00b2d3a6f5a3073988ac050c2d7b6655e2", 0, "changed-in-master.txt" },
		{ 0100644, "4e886e602529caa9ab11d71f86634bd1b6e0de10", 0, "conflicting.txt" },
		{ 0100644, "ef58fdd8086c243bdc81f99e379acacfd21d32d6", 0, "new-in-unrelated1.txt" },
		{ 0100644, "948ba6e701c1edab0c2d394fb7c5538334129793", 0, "new-in-unrelated2.txt" },
		{ 0100644, "dfe3f22baa1f6fce5447901c3086bae368de6bdd", 0, "removed-in-branch.txt" },
		{ 0100644, "c8f06f2e3bb2964174677e91f0abead0e43c9e5d", 0, "unchanged.txt" },
	};

	cl_git_pass(git_oid_fromstr(&their_oids[0], THEIRS_UNRELATED_PARENT));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[0], repo, &their_oids[0]));

	merge_opts.file_favor = 0;
	cl_git_pass(git_merge(repo, (const git_annotated_commit **)their_heads, 1, &merge_opts, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 9));

	git_annotated_commit_free(their_heads[0]);
}

void test_merge_workdir_simple__unrelated_with_conflicts(void)
{
	git_oid their_oids[1];
	git_annotated_commit *their_heads[1];
	git_merge_options merge_opts = GIT_MERGE_OPTIONS_INIT;

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

	cl_git_pass(git_oid_fromstr(&their_oids[0], THEIRS_UNRELATED_OID));
	cl_git_pass(git_annotated_commit_lookup(&their_heads[0], repo, &their_oids[0]));

	merge_opts.file_favor = 0;
	cl_git_pass(git_merge(repo, (const git_annotated_commit **)their_heads, 1, &merge_opts, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 11));

	git_annotated_commit_free(their_heads[0]);
}

void test_merge_workdir_simple__binary(void)
{
	git_oid our_oid, their_oid, our_file_oid;
	git_commit *our_commit;
	git_annotated_commit *their_head;
	const git_index_entry *binary_entry;

	struct merge_index_entry merge_index_entries[] = {
		{ 0100644, "1c51d885170f57a0c4e8c69ff6363d91a5b51f85", 1, "binary" },
		{ 0100644, "23ed141a6ae1e798b2f721afedbe947c119111ba", 2, "binary" },
		{ 0100644, "836b8b82b26cab22eaaed8820877c76d6c8bca19", 3, "binary" },
	};

	cl_git_pass(git_oid_fromstr(&our_oid, "cc338e4710c9b257106b8d16d82f86458d5beaf1"));
	cl_git_pass(git_oid_fromstr(&their_oid, "ad01aebfdf2ac13145efafe3f9fcf798882f1730"));

	cl_git_pass(git_commit_lookup(&our_commit, repo, &our_oid));
	cl_git_pass(git_reset(repo, (git_object *)our_commit, GIT_RESET_HARD, NULL));

	cl_git_pass(git_annotated_commit_lookup(&their_head, repo, &their_oid));

	cl_git_pass(git_merge(repo, (const git_annotated_commit **)&their_head, 1, NULL, NULL));

	cl_assert(merge_test_index(repo_index, merge_index_entries, 3));

	cl_git_pass(git_index_add_bypath(repo_index, "binary"));
	cl_assert((binary_entry = git_index_get_bypath(repo_index, "binary", 0)) != NULL);

	cl_git_pass(git_oid_fromstr(&our_file_oid, "23ed141a6ae1e798b2f721afedbe947c119111ba"));
	cl_assert(git_oid_cmp(&binary_entry->id, &our_file_oid) == 0);

	git_annotated_commit_free(their_head);
	git_commit_free(our_commit);
}
