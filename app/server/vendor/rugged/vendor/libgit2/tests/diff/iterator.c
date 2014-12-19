#include "clar_libgit2.h"
#include "diff_helpers.h"
#include "iterator.h"
#include "tree.h"

void test_diff_iterator__initialize(void)
{
	/* since we are doing tests with different sandboxes, defer setup
	 * to the actual tests.  cleanup will still be done in the global
	 * cleanup function so that assertion failures don't result in a
	 * missed cleanup.
	 */
}

void test_diff_iterator__cleanup(void)
{
	cl_git_sandbox_cleanup();
}


/* -- TREE ITERATOR TESTS -- */

static void tree_iterator_test(
	const char *sandbox,
	const char *treeish,
	const char *start,
	const char *end,
	int expected_count,
	const char **expected_values)
{
	git_tree *t;
	git_iterator *i;
	const git_index_entry *entry;
	int error, count = 0, count_post_reset = 0;
	git_repository *repo = cl_git_sandbox_init(sandbox);

	cl_assert(t = resolve_commit_oid_to_tree(repo, treeish));
	cl_git_pass(git_iterator_for_tree(
		&i, t, GIT_ITERATOR_DONT_IGNORE_CASE, start, end));

	/* test loop */
	while (!(error = git_iterator_advance(&entry, i))) {
		cl_assert(entry);
		if (expected_values != NULL)
			cl_assert_equal_s(expected_values[count], entry->path);
		count++;
	}
	cl_assert_equal_i(GIT_ITEROVER, error);
	cl_assert(!entry);
	cl_assert_equal_i(expected_count, count);

	/* test reset */
	cl_git_pass(git_iterator_reset(i, NULL, NULL));

	while (!(error = git_iterator_advance(&entry, i))) {
		cl_assert(entry);
		if (expected_values != NULL)
			cl_assert_equal_s(expected_values[count_post_reset], entry->path);
		count_post_reset++;
	}
	cl_assert_equal_i(GIT_ITEROVER, error);
	cl_assert(!entry);
	cl_assert_equal_i(count, count_post_reset);

	git_iterator_free(i);
	git_tree_free(t);
}

/* results of: git ls-tree -r --name-only 605812a */
const char *expected_tree_0[] = {
	".gitattributes",
	"attr0",
	"attr1",
	"attr2",
	"attr3",
	"binfile",
	"macro_test",
	"root_test1",
	"root_test2",
	"root_test3",
	"root_test4.txt",
	"subdir/.gitattributes",
	"subdir/abc",
	"subdir/subdir_test1",
	"subdir/subdir_test2.txt",
	"subdir2/subdir2_test1",
	NULL
};

void test_diff_iterator__tree_0(void)
{
	tree_iterator_test("attr", "605812a", NULL, NULL, 16, expected_tree_0);
}

/* results of: git ls-tree -r --name-only 6bab5c79 */
const char *expected_tree_1[] = {
	".gitattributes",
	"attr0",
	"attr1",
	"attr2",
	"attr3",
	"root_test1",
	"root_test2",
	"root_test3",
	"root_test4.txt",
	"subdir/.gitattributes",
	"subdir/subdir_test1",
	"subdir/subdir_test2.txt",
	"subdir2/subdir2_test1",
	NULL
};

void test_diff_iterator__tree_1(void)
{
	tree_iterator_test("attr", "6bab5c79cd5", NULL, NULL, 13, expected_tree_1);
}

/* results of: git ls-tree -r --name-only 26a125ee1 */
const char *expected_tree_2[] = {
	"current_file",
	"file_deleted",
	"modified_file",
	"staged_changes",
	"staged_changes_file_deleted",
	"staged_changes_modified_file",
	"staged_delete_file_deleted",
	"staged_delete_modified_file",
	"subdir.txt",
	"subdir/current_file",
	"subdir/deleted_file",
	"subdir/modified_file",
	NULL
};

void test_diff_iterator__tree_2(void)
{
	tree_iterator_test("status", "26a125ee1", NULL, NULL, 12, expected_tree_2);
}

/* $ git ls-tree -r --name-only 0017bd4ab1e */
const char *expected_tree_3[] = {
	"current_file",
	"file_deleted",
	"modified_file",
	"staged_changes",
	"staged_changes_file_deleted",
	"staged_changes_modified_file",
	"staged_delete_file_deleted",
	"staged_delete_modified_file"
};

void test_diff_iterator__tree_3(void)
{
	tree_iterator_test("status", "0017bd4ab1e", NULL, NULL, 8, expected_tree_3);
}

/* $ git ls-tree -r --name-only 24fa9a9fc4e202313e24b648087495441dab432b */
const char *expected_tree_4[] = {
	"attr0",
	"attr1",
	"attr2",
	"attr3",
	"binfile",
	"gitattributes",
	"macro_bad",
	"macro_test",
	"root_test1",
	"root_test2",
	"root_test3",
	"root_test4.txt",
	"sub/abc",
	"sub/file",
	"sub/sub/file",
	"sub/sub/subsub.txt",
	"sub/subdir_test1",
	"sub/subdir_test2.txt",
	"subdir/.gitattributes",
	"subdir/abc",
	"subdir/subdir_test1",
	"subdir/subdir_test2.txt",
	"subdir2/subdir2_test1",
	NULL
};

void test_diff_iterator__tree_4(void)
{
	tree_iterator_test(
		"attr", "24fa9a9fc4e202313e24b648087495441dab432b", NULL, NULL,
		23, expected_tree_4);
}

void test_diff_iterator__tree_4_ranged(void)
{
	tree_iterator_test(
		"attr", "24fa9a9fc4e202313e24b648087495441dab432b",
		"sub", "sub",
		11, &expected_tree_4[12]);
}

const char *expected_tree_ranged_0[] = {
	"gitattributes",
	"macro_bad",
	"macro_test",
	"root_test1",
	"root_test2",
	"root_test3",
	"root_test4.txt",
	NULL
};

void test_diff_iterator__tree_ranged_0(void)
{
	tree_iterator_test(
		"attr", "24fa9a9fc4e202313e24b648087495441dab432b",
		"git", "root",
		7, expected_tree_ranged_0);
}

const char *expected_tree_ranged_1[] = {
	"sub/subdir_test2.txt",
	NULL
};

void test_diff_iterator__tree_ranged_1(void)
{
	tree_iterator_test(
		"attr", "24fa9a9fc4e202313e24b648087495441dab432b",
		"sub/subdir_test2.txt", "sub/subdir_test2.txt",
		1, expected_tree_ranged_1);
}

void test_diff_iterator__tree_range_empty_0(void)
{
	tree_iterator_test(
		"attr", "24fa9a9fc4e202313e24b648087495441dab432b",
		"empty", "empty", 0, NULL);
}

void test_diff_iterator__tree_range_empty_1(void)
{
	tree_iterator_test(
		"attr", "24fa9a9fc4e202313e24b648087495441dab432b",
		"z_empty_after", NULL, 0, NULL);
}

void test_diff_iterator__tree_range_empty_2(void)
{
	tree_iterator_test(
		"attr", "24fa9a9fc4e202313e24b648087495441dab432b",
		NULL, ".aaa_empty_before", 0, NULL);
}

static void check_tree_entry(
	git_iterator *i,
	const char *oid,
	const char *oid_p,
	const char *oid_pp,
	const char *oid_ppp)
{
	const git_index_entry *ie;
	const git_tree_entry *te;
	const git_tree *tree;
	git_buf path = GIT_BUF_INIT;

	cl_git_pass(git_iterator_current_tree_entry(&te, i));
	cl_assert(te);
	cl_assert(git_oid_streq(&te->oid, oid) == 0);

	cl_git_pass(git_iterator_current(&ie, i));
	cl_git_pass(git_buf_sets(&path, ie->path));

	if (oid_p) {
		git_buf_rtruncate_at_char(&path, '/');
		cl_git_pass(git_iterator_current_parent_tree(&tree, i, path.ptr));
		cl_assert(tree);
		cl_assert(git_oid_streq(git_tree_id(tree), oid_p) == 0);
	}

	if (oid_pp) {
		git_buf_rtruncate_at_char(&path, '/');
		cl_git_pass(git_iterator_current_parent_tree(&tree, i, path.ptr));
		cl_assert(tree);
		cl_assert(git_oid_streq(git_tree_id(tree), oid_pp) == 0);
	}

	if (oid_ppp) {
		git_buf_rtruncate_at_char(&path, '/');
		cl_git_pass(git_iterator_current_parent_tree(&tree, i, path.ptr));
		cl_assert(tree);
		cl_assert(git_oid_streq(git_tree_id(tree), oid_ppp) == 0);
	}

	git_buf_free(&path);
}

void test_diff_iterator__tree_special_functions(void)
{
	git_tree *t;
	git_iterator *i;
	const git_index_entry *entry;
	git_repository *repo = cl_git_sandbox_init("attr");
	int error, cases = 0;
	const char *rootoid = "ce39a97a7fb1fa90bcf5e711249c1e507476ae0e";

	t = resolve_commit_oid_to_tree(
		repo, "24fa9a9fc4e202313e24b648087495441dab432b");
	cl_assert(t != NULL);

	cl_git_pass(git_iterator_for_tree(
		&i, t, GIT_ITERATOR_DONT_IGNORE_CASE, NULL, NULL));

	while (!(error = git_iterator_advance(&entry, i))) {
		cl_assert(entry);

		if (strcmp(entry->path, "sub/file") == 0) {
			cases++;
			check_tree_entry(
				i, "45b983be36b73c0788dc9cbcb76cbb80fc7bb057",
				"ecb97df2a174987475ac816e3847fc8e9f6c596b",
				rootoid, NULL);
		}
		else if (strcmp(entry->path, "sub/sub/subsub.txt") == 0) {
			cases++;
			check_tree_entry(
				i, "9e5bdc47d6a80f2be0ea3049ad74231b94609242",
				"4e49ba8c5b6c32ff28cd9dcb60be34df50fcc485",
				"ecb97df2a174987475ac816e3847fc8e9f6c596b", rootoid);
		}
		else if (strcmp(entry->path, "subdir/.gitattributes") == 0) {
			cases++;
			check_tree_entry(
				i, "99eae476896f4907224978b88e5ecaa6c5bb67a9",
				"9fb40b6675dde60b5697afceae91b66d908c02d9",
				rootoid, NULL);
		}
		else if (strcmp(entry->path, "subdir2/subdir2_test1") == 0) {
			cases++;
			check_tree_entry(
				i, "dccada462d3df8ac6de596fb8c896aba9344f941",
				"2929de282ce999e95183aedac6451d3384559c4b",
				rootoid, NULL);
		}
	}
	cl_assert_equal_i(GIT_ITEROVER, error);
	cl_assert(!entry);
	cl_assert_equal_i(4, cases);

	git_iterator_free(i);
	git_tree_free(t);
}

/* -- INDEX ITERATOR TESTS -- */

static void index_iterator_test(
	const char *sandbox,
	const char *start,
	const char *end,
	git_iterator_flag_t flags,
	int expected_count,
	const char **expected_names,
	const char **expected_oids)
{
	git_index *index;
	git_iterator *i;
	const git_index_entry *entry;
	int error, count = 0, caps;
	git_repository *repo = cl_git_sandbox_init(sandbox);

	cl_git_pass(git_repository_index(&index, repo));
	caps = git_index_caps(index);

	cl_git_pass(git_iterator_for_index(&i, index, flags, start, end));

	while (!(error = git_iterator_advance(&entry, i))) {
		cl_assert(entry);

		if (expected_names != NULL)
			cl_assert_equal_s(expected_names[count], entry->path);

		if (expected_oids != NULL) {
			git_oid oid;
			cl_git_pass(git_oid_fromstr(&oid, expected_oids[count]));
			cl_assert_equal_oid(&oid, &entry->id);
		}

		count++;
	}

	cl_assert_equal_i(GIT_ITEROVER, error);
	cl_assert(!entry);
	cl_assert_equal_i(expected_count, count);

	git_iterator_free(i);

	cl_assert(caps == git_index_caps(index));
	git_index_free(index);
}

static const char *expected_index_0[] = {
	"attr0",
	"attr1",
	"attr2",
	"attr3",
	"binfile",
	"gitattributes",
	"macro_bad",
	"macro_test",
	"root_test1",
	"root_test2",
	"root_test3",
	"root_test4.txt",
	"sub/abc",
	"sub/file",
	"sub/sub/file",
	"sub/sub/subsub.txt",
	"sub/subdir_test1",
	"sub/subdir_test2.txt",
	"subdir/.gitattributes",
	"subdir/abc",
	"subdir/subdir_test1",
	"subdir/subdir_test2.txt",
	"subdir2/subdir2_test1",
};

static const char *expected_index_oids_0[] = {
	"556f8c827b8e4a02ad5cab77dca2bcb3e226b0b3",
	"3b74db7ab381105dc0d28f8295a77f6a82989292",
	"2c66e14f77196ea763fb1e41612c1aa2bc2d8ed2",
	"c485abe35abd4aa6fd83b076a78bbea9e2e7e06c",
	"d800886d9c86731ae5c4a62b0b77c437015e00d2",
	"2b40c5aca159b04ea8d20ffe36cdf8b09369b14a",
	"5819a185d77b03325aaf87cafc771db36f6ddca7",
	"ff69f8639ce2e6010b3f33a74160aad98b48da2b",
	"45141a79a77842c59a63229403220a4e4be74e3d",
	"4d713dc48e6b1bd75b0d61ad078ba9ca3a56745d",
	"108bb4e7fd7b16490dc33ff7d972151e73d7166e",
	"a0f7217ae99f5ac3e88534f5cea267febc5fa85b",
	"3e42ffc54a663f9401cc25843d6c0e71a33e4249",
	"45b983be36b73c0788dc9cbcb76cbb80fc7bb057",
	"45b983be36b73c0788dc9cbcb76cbb80fc7bb057",
	"9e5bdc47d6a80f2be0ea3049ad74231b94609242",
	"e563cf4758f0d646f1b14b76016aa17fa9e549a4",
	"fb5067b1aef3ac1ada4b379dbcb7d17255df7d78",
	"99eae476896f4907224978b88e5ecaa6c5bb67a9",
	"3e42ffc54a663f9401cc25843d6c0e71a33e4249",
	"e563cf4758f0d646f1b14b76016aa17fa9e549a4",
	"fb5067b1aef3ac1ada4b379dbcb7d17255df7d78",
	"dccada462d3df8ac6de596fb8c896aba9344f941"
};

void test_diff_iterator__index_0(void)
{
	index_iterator_test(
		"attr", NULL, NULL, 0, ARRAY_SIZE(expected_index_0),
		expected_index_0, expected_index_oids_0);
}

static const char *expected_index_range[] = {
	"root_test1",
	"root_test2",
	"root_test3",
	"root_test4.txt",
};

static const char *expected_index_oids_range[] = {
	"45141a79a77842c59a63229403220a4e4be74e3d",
	"4d713dc48e6b1bd75b0d61ad078ba9ca3a56745d",
	"108bb4e7fd7b16490dc33ff7d972151e73d7166e",
	"a0f7217ae99f5ac3e88534f5cea267febc5fa85b",
};

void test_diff_iterator__index_range(void)
{
	index_iterator_test(
		"attr", "root", "root", 0, ARRAY_SIZE(expected_index_range),
		expected_index_range, expected_index_oids_range);
}

void test_diff_iterator__index_range_empty_0(void)
{
	index_iterator_test(
		"attr", "empty", "empty", 0, 0, NULL, NULL);
}

void test_diff_iterator__index_range_empty_1(void)
{
	index_iterator_test(
		"attr", "z_empty_after", NULL, 0, 0, NULL, NULL);
}

void test_diff_iterator__index_range_empty_2(void)
{
	index_iterator_test(
		"attr", NULL, ".aaa_empty_before", 0, 0, NULL, NULL);
}

static const char *expected_index_1[] = {
	"current_file",
	"file_deleted",
	"modified_file",
	"staged_changes",
	"staged_changes_file_deleted",
	"staged_changes_modified_file",
	"staged_new_file",
	"staged_new_file_deleted_file",
	"staged_new_file_modified_file",
	"subdir.txt",
	"subdir/current_file",
	"subdir/deleted_file",
	"subdir/modified_file",
};

static const char* expected_index_oids_1[] = {
	"a0de7e0ac200c489c41c59dfa910154a70264e6e",
	"5452d32f1dd538eb0405e8a83cc185f79e25e80f",
	"452e4244b5d083ddf0460acf1ecc74db9dcfa11a",
	"55d316c9ba708999f1918e9677d01dfcae69c6b9",
	"a6be623522ce87a1d862128ac42672604f7b468b",
	"906ee7711f4f4928ddcb2a5f8fbc500deba0d2a8",
	"529a16e8e762d4acb7b9636ff540a00831f9155a",
	"90b8c29d8ba39434d1c63e1b093daaa26e5bd972",
	"ed062903b8f6f3dccb2fa81117ba6590944ef9bd",
	"e8ee89e15bbe9b20137715232387b3de5b28972e",
	"53ace0d1cc1145a5f4fe4f78a186a60263190733",
	"1888c805345ba265b0ee9449b8877b6064592058",
	"a6191982709b746d5650e93c2acf34ef74e11504"
};

void test_diff_iterator__index_1(void)
{
	index_iterator_test(
		"status", NULL, NULL, 0, ARRAY_SIZE(expected_index_1),
		expected_index_1, expected_index_oids_1);
}

static const char *expected_index_cs[] = {
	"B", "D", "F", "H", "J", "L/1", "L/B", "L/D", "L/a", "L/c",
	"a", "c", "e", "g", "i", "k/1", "k/B", "k/D", "k/a", "k/c",
};

static const char *expected_index_ci[] = {
	"a", "B", "c", "D", "e", "F", "g", "H", "i", "J",
	"k/1", "k/a", "k/B", "k/c", "k/D", "L/1", "L/a", "L/B", "L/c", "L/D",
};

void test_diff_iterator__index_case_folding(void)
{
	git_buf path = GIT_BUF_INIT;
	int fs_is_ci = 0;

	cl_git_pass(git_buf_joinpath(&path, cl_fixture("icase"), ".gitted/CoNfIg"));
	fs_is_ci = git_path_exists(path.ptr);
	git_buf_free(&path);

	index_iterator_test(
		"icase", NULL, NULL, 0, ARRAY_SIZE(expected_index_cs),
		fs_is_ci ? expected_index_ci : expected_index_cs, NULL);

	cl_git_sandbox_cleanup();

	index_iterator_test(
		"icase", NULL, NULL, GIT_ITERATOR_IGNORE_CASE,
		ARRAY_SIZE(expected_index_ci), expected_index_ci, NULL);

	cl_git_sandbox_cleanup();

	index_iterator_test(
		"icase", NULL, NULL, GIT_ITERATOR_DONT_IGNORE_CASE,
		ARRAY_SIZE(expected_index_cs), expected_index_cs, NULL);
}

/* -- WORKDIR ITERATOR TESTS -- */

static void workdir_iterator_test(
	const char *sandbox,
	const char *start,
	const char *end,
	int expected_count,
	int expected_ignores,
	const char **expected_names,
	const char *an_ignored_name)
{
	git_iterator *i;
	const git_index_entry *entry;
	int error, count = 0, count_all = 0, count_all_post_reset = 0;
	git_repository *repo = cl_git_sandbox_init(sandbox);

	cl_git_pass(git_iterator_for_workdir(
		&i, repo, NULL, NULL, GIT_ITERATOR_DONT_AUTOEXPAND, start, end));

	error = git_iterator_current(&entry, i);
	cl_assert((error == 0 && entry != NULL) ||
			  (error == GIT_ITEROVER && entry == NULL));

	while (entry != NULL) {
		int ignored = git_iterator_current_is_ignored(i);

		if (S_ISDIR(entry->mode)) {
			cl_git_pass(git_iterator_advance_into(&entry, i));
			continue;
		}

		if (expected_names != NULL)
			cl_assert_equal_s(expected_names[count_all], entry->path);

		if (an_ignored_name && strcmp(an_ignored_name,entry->path)==0)
			cl_assert(ignored);

		if (!ignored)
			count++;
		count_all++;

		error = git_iterator_advance(&entry, i);

		cl_assert((error == 0 && entry != NULL) ||
				  (error == GIT_ITEROVER && entry == NULL));
	}

	cl_assert_equal_i(expected_count, count);
	cl_assert_equal_i(expected_count + expected_ignores, count_all);

	cl_git_pass(git_iterator_reset(i, NULL, NULL));

	error = git_iterator_current(&entry, i);
	cl_assert((error == 0 && entry != NULL) ||
			  (error == GIT_ITEROVER && entry == NULL));

	while (entry != NULL) {
		if (S_ISDIR(entry->mode)) {
			cl_git_pass(git_iterator_advance_into(&entry, i));
			continue;
		}

		if (expected_names != NULL)
			cl_assert_equal_s(
				expected_names[count_all_post_reset], entry->path);
		count_all_post_reset++;

		error = git_iterator_advance(&entry, i);
		cl_assert(error == 0 || error == GIT_ITEROVER);
	}

	cl_assert_equal_i(count_all, count_all_post_reset);

	git_iterator_free(i);
}

void test_diff_iterator__workdir_0(void)
{
	workdir_iterator_test("attr", NULL, NULL, 23, 5, NULL, "ign");
}

static const char *status_paths[] = {
	"current_file",
	"ignored_file",
	"modified_file",
	"new_file",
	"staged_changes",
	"staged_changes_modified_file",
	"staged_delete_modified_file",
	"staged_new_file",
	"staged_new_file_modified_file",
	"subdir.txt",
	"subdir/current_file",
	"subdir/modified_file",
	"subdir/new_file",
	"\xe8\xbf\x99",
	NULL
};

void test_diff_iterator__workdir_1(void)
{
	workdir_iterator_test(
		"status", NULL, NULL, 13, 1, status_paths, "ignored_file");
}

static const char *status_paths_range_0[] = {
	"staged_changes",
	"staged_changes_modified_file",
	"staged_delete_modified_file",
	"staged_new_file",
	"staged_new_file_modified_file",
	NULL
};

void test_diff_iterator__workdir_1_ranged_0(void)
{
	workdir_iterator_test(
		"status", "staged", "staged", 5, 0, status_paths_range_0, NULL);
}

static const char *status_paths_range_1[] = {
	"modified_file", NULL
};

void test_diff_iterator__workdir_1_ranged_1(void)
{
	workdir_iterator_test(
		"status", "modified_file", "modified_file",
		1, 0, status_paths_range_1, NULL);
}

static const char *status_paths_range_3[] = {
	"subdir.txt",
	"subdir/current_file",
	"subdir/modified_file",
	NULL
};

void test_diff_iterator__workdir_1_ranged_3(void)
{
	workdir_iterator_test(
		"status", "subdir", "subdir/modified_file",
		3, 0, status_paths_range_3, NULL);
}

static const char *status_paths_range_4[] = {
	"subdir/current_file",
	"subdir/modified_file",
	"subdir/new_file",
	"\xe8\xbf\x99",
	NULL
};

void test_diff_iterator__workdir_1_ranged_4(void)
{
	workdir_iterator_test(
		"status", "subdir/", NULL, 4, 0, status_paths_range_4, NULL);
}

static const char *status_paths_range_5[] = {
	"subdir/modified_file",
	NULL
};

void test_diff_iterator__workdir_1_ranged_5(void)
{
	workdir_iterator_test(
		"status", "subdir/modified_file", "subdir/modified_file",
		1, 0, status_paths_range_5, NULL);
}

void test_diff_iterator__workdir_1_ranged_empty_0(void)
{
	workdir_iterator_test(
		"status", "\xff_does_not_exist", NULL,
		0, 0, NULL, NULL);
}

void test_diff_iterator__workdir_1_ranged_empty_1(void)
{
	workdir_iterator_test(
		"status", "empty", "empty",
		0, 0, NULL, NULL);
}

void test_diff_iterator__workdir_1_ranged_empty_2(void)
{
	workdir_iterator_test(
		"status", NULL, "aaaa_empty_before",
		0, 0, NULL, NULL);
}

void test_diff_iterator__workdir_builtin_ignores(void)
{
	git_repository *repo = cl_git_sandbox_init("attr");
	git_iterator *i;
	const git_index_entry *entry;
	int idx;
	static struct {
		const char *path;
		bool ignored;
	} expected[] = {
		{ "dir/", true },
		{ "file", false },
		{ "ign", true },
		{ "macro_bad", false },
		{ "macro_test", false },
		{ "root_test1", false },
		{ "root_test2", false },
		{ "root_test3", false },
		{ "root_test4.txt", false },
		{ "sub/", false },
		{ "sub/.gitattributes", false },
		{ "sub/abc", false },
		{ "sub/dir/", true },
		{ "sub/file", false },
		{ "sub/ign/", true },
		{ "sub/sub/", false },
		{ "sub/sub/.gitattributes", false },
		{ "sub/sub/dir", false }, /* file is not actually a dir */
		{ "sub/sub/file", false },
		{ NULL, false }
	};

	cl_git_pass(p_mkdir("attr/sub/sub/.git", 0777));
	cl_git_mkfile("attr/sub/.git", "whatever");

	cl_git_pass(git_iterator_for_workdir(
		&i, repo, NULL, NULL, GIT_ITERATOR_DONT_AUTOEXPAND, "dir", "sub/sub/file"));
	cl_git_pass(git_iterator_current(&entry, i));

	for (idx = 0; entry != NULL; ++idx) {
		int ignored = git_iterator_current_is_ignored(i);

		cl_assert_equal_s(expected[idx].path, entry->path);
		cl_assert_(ignored == expected[idx].ignored, expected[idx].path);

		if (!ignored &&
			(entry->mode == GIT_FILEMODE_TREE ||
			 entry->mode == GIT_FILEMODE_COMMIT))
		{
			/* it is possible to advance "into" a submodule */
			cl_git_pass(git_iterator_advance_into(&entry, i));
		} else {
			int error = git_iterator_advance(&entry, i);
			cl_assert(!error || error == GIT_ITEROVER);
		}
	}

	cl_assert(expected[idx].path == NULL);

	git_iterator_free(i);
}

static void check_wd_first_through_third_range(
	git_repository *repo, const char *start, const char *end)
{
	git_iterator *i;
	const git_index_entry *entry;
	int error, idx;
	static const char *expected[] = { "FIRST", "second", "THIRD", NULL };

	cl_git_pass(git_iterator_for_workdir(
		&i, repo, NULL, NULL, GIT_ITERATOR_IGNORE_CASE, start, end));
	cl_git_pass(git_iterator_current(&entry, i));

	for (idx = 0; entry != NULL; ++idx) {
		cl_assert_equal_s(expected[idx], entry->path);

		error = git_iterator_advance(&entry, i);
		cl_assert(!error || error == GIT_ITEROVER);
	}

	cl_assert(expected[idx] == NULL);

	git_iterator_free(i);
}

void test_diff_iterator__workdir_handles_icase_range(void)
{
	git_repository *repo;

	repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_remove_placeholders(git_repository_path(repo), "dummy-marker.txt");

	cl_git_mkfile("empty_standard_repo/before", "whatever\n");
	cl_git_mkfile("empty_standard_repo/FIRST", "whatever\n");
	cl_git_mkfile("empty_standard_repo/second", "whatever\n");
	cl_git_mkfile("empty_standard_repo/THIRD", "whatever\n");
	cl_git_mkfile("empty_standard_repo/zafter", "whatever\n");
	cl_git_mkfile("empty_standard_repo/Zlast", "whatever\n");

	check_wd_first_through_third_range(repo, "first", "third");
	check_wd_first_through_third_range(repo, "FIRST", "THIRD");
	check_wd_first_through_third_range(repo, "first", "THIRD");
	check_wd_first_through_third_range(repo, "FIRST", "third");
	check_wd_first_through_third_range(repo, "FirSt", "tHiRd");
}

static void check_tree_range(
	git_repository *repo,
	const char *start,
	const char *end,
	bool ignore_case,
	int expected_count)
{
	git_tree *head;
	git_iterator *i;
	int error, count;

	cl_git_pass(git_repository_head_tree(&head, repo));

	cl_git_pass(git_iterator_for_tree(
		&i, head,
		ignore_case ? GIT_ITERATOR_IGNORE_CASE : GIT_ITERATOR_DONT_IGNORE_CASE,
		start, end));

	for (count = 0; !(error = git_iterator_advance(NULL, i)); ++count)
		/* count em up */;

	cl_assert_equal_i(GIT_ITEROVER, error);
	cl_assert_equal_i(expected_count, count);

	git_iterator_free(i);
	git_tree_free(head);
}

void test_diff_iterator__tree_handles_icase_range(void)
{
	git_repository *repo;

	repo = cl_git_sandbox_init("testrepo");

	check_tree_range(repo, "B", "C", false, 0);
	check_tree_range(repo, "B", "C", true, 1);
	check_tree_range(repo, "b", "c", false, 1);
	check_tree_range(repo, "b", "c", true, 1);

	check_tree_range(repo, "a", "z", false, 3);
	check_tree_range(repo, "a", "z", true, 4);
	check_tree_range(repo, "A", "Z", false, 1);
	check_tree_range(repo, "A", "Z", true, 4);
	check_tree_range(repo, "a", "Z", false, 0);
	check_tree_range(repo, "a", "Z", true, 4);
	check_tree_range(repo, "A", "z", false, 4);
	check_tree_range(repo, "A", "z", true, 4);

	check_tree_range(repo, "new.txt", "new.txt", true, 1);
	check_tree_range(repo, "new.txt", "new.txt", false, 1);
	check_tree_range(repo, "README", "README", true, 1);
	check_tree_range(repo, "README", "README", false, 1);
}

static void check_index_range(
	git_repository *repo,
	const char *start,
	const char *end,
	bool ignore_case,
	int expected_count)
{
	git_index *index;
	git_iterator *i;
	int error, count, caps;
	bool is_ignoring_case;

	cl_git_pass(git_repository_index(&index, repo));

	caps = git_index_caps(index);
	is_ignoring_case = ((caps & GIT_INDEXCAP_IGNORE_CASE) != 0);

	if (ignore_case != is_ignoring_case)
		cl_git_pass(git_index_set_caps(index, caps ^ GIT_INDEXCAP_IGNORE_CASE));

	cl_git_pass(git_iterator_for_index(&i, index, 0, start, end));

	cl_assert(git_iterator_ignore_case(i) == ignore_case);

	for (count = 0; !(error = git_iterator_advance(NULL, i)); ++count)
		/* count em up */;

	cl_assert_equal_i(GIT_ITEROVER, error);
	cl_assert_equal_i(expected_count, count);

	git_iterator_free(i);
	git_index_free(index);
}

void test_diff_iterator__index_handles_icase_range(void)
{
	git_repository *repo;
	git_index *index;
	git_tree *head;

	repo = cl_git_sandbox_init("testrepo");

	/* reset index to match HEAD */
	cl_git_pass(git_repository_head_tree(&head, repo));
	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_read_tree(index, head));
	cl_git_pass(git_index_write(index));
	git_tree_free(head);
	git_index_free(index);

	/* do some ranged iterator checks toggling case sensitivity */
	check_index_range(repo, "B", "C", false, 0);
	check_index_range(repo, "B", "C", true, 1);
	check_index_range(repo, "a", "z", false, 3);
	check_index_range(repo, "a", "z", true, 4);
}
