#include "clar_libgit2.h"
#include "iterator.h"
#include "repository.h"
#include "fileops.h"
#include "iterator_helpers.h"
#include "../submodule/submodule_helpers.h"
#include <stdarg.h>

static git_repository *g_repo;

void test_iterator_index__initialize(void)
{
}

void test_iterator_index__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

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
	git_iterator_options iter_opts = GIT_ITERATOR_OPTIONS_INIT;

	g_repo = cl_git_sandbox_init(sandbox);

	cl_git_pass(git_repository_index(&index, g_repo));
	caps = git_index_caps(index);

	iter_opts.flags = flags;
	iter_opts.start = start;
	iter_opts.end = end;

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &iter_opts));

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

void test_iterator_index__0(void)
{
	index_iterator_test(
		"attr", NULL, NULL, 0, ARRAY_SIZE(expected_index_0),
		expected_index_0, expected_index_oids_0);
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

void test_iterator_index__1(void)
{
	index_iterator_test(
		"status", NULL, NULL, 0, ARRAY_SIZE(expected_index_1),
		expected_index_1, expected_index_oids_1);
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

void test_iterator_index__range(void)
{
	index_iterator_test(
		"attr", "root", "root", 0, ARRAY_SIZE(expected_index_range),
		expected_index_range, expected_index_oids_range);
}

void test_iterator_index__range_empty_0(void)
{
	index_iterator_test(
		"attr", "empty", "empty", 0, 0, NULL, NULL);
}

void test_iterator_index__range_empty_1(void)
{
	index_iterator_test(
		"attr", "z_empty_after", NULL, 0, 0, NULL, NULL);
}

void test_iterator_index__range_empty_2(void)
{
	index_iterator_test(
		"attr", NULL, ".aaa_empty_before", 0, 0, NULL, NULL);
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
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	int error, count, caps;
	bool is_ignoring_case;

	cl_git_pass(git_repository_index(&index, repo));

	caps = git_index_caps(index);
	is_ignoring_case = ((caps & GIT_INDEXCAP_IGNORE_CASE) != 0);

	if (ignore_case != is_ignoring_case)
		cl_git_pass(git_index_set_caps(index, caps ^ GIT_INDEXCAP_IGNORE_CASE));

	i_opts.flags = 0;
	i_opts.start = start;
	i_opts.end = end;

	cl_git_pass(git_iterator_for_index(&i, repo, index, &i_opts));

	cl_assert(git_iterator_ignore_case(i) == ignore_case);

	for (count = 0; !(error = git_iterator_advance(NULL, i)); ++count)
		/* count em up */;

	cl_assert_equal_i(GIT_ITEROVER, error);
	cl_assert_equal_i(expected_count, count);

	git_iterator_free(i);
	git_index_free(index);
}

void test_iterator_index__range_icase(void)
{
	git_index *index;
	git_tree *head;

	g_repo = cl_git_sandbox_init("testrepo");

	/* reset index to match HEAD */
	cl_git_pass(git_repository_head_tree(&head, g_repo));
	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_read_tree(index, head));
	cl_git_pass(git_index_write(index));
	git_tree_free(head);
	git_index_free(index);

	/* do some ranged iterator checks toggling case sensitivity */
	check_index_range(g_repo, "B", "C", false, 0);
	check_index_range(g_repo, "B", "C", true, 1);
	check_index_range(g_repo, "a", "z", false, 3);
	check_index_range(g_repo, "a", "z", true, 4);
}

static const char *expected_index_cs[] = {
	"B", "D", "F", "H", "J", "L/1", "L/B", "L/D", "L/a", "L/c",
	"a", "c", "e", "g", "i", "k/1", "k/B", "k/D", "k/a", "k/c",
};

static const char *expected_index_ci[] = {
	"a", "B", "c", "D", "e", "F", "g", "H", "i", "J",
	"k/1", "k/a", "k/B", "k/c", "k/D", "L/1", "L/a", "L/B", "L/c", "L/D",
};

void test_iterator_index__case_folding(void)
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

/* Index contents (including pseudotrees):
 *
 * 0: a     5: F     10: k/      16: L/
 * 1: B     6: g     11: k/1     17: L/1
 * 2: c     7: H     12: k/a     18: L/a
 * 3: D     8: i     13: k/B     19: L/B
 * 4: e     9: J     14: k/c     20: L/c
 *                   15: k/D     21: L/D
 *
 * 0: B     5: L/    11: a       16: k/
 * 1: D     6: L/1   12: c       17: k/1
 * 2: F     7: L/B   13: e       18: k/B
 * 3: H     8: L/D   14: g       19: k/D
 * 4: J     9: L/a   15: i       20: k/a
 *         10: L/c               21: k/c
 */

void test_iterator_index__icase_0(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_repository_index(&index, g_repo));

	/* autoexpand with no tree entries for index */
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, NULL));
	expect_iterator_items(i, 20, NULL, 20, NULL);
	git_iterator_free(i);

	/* auto expand with tree entries */
	i_opts.flags = GIT_ITERATOR_INCLUDE_TREES;
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 22, NULL, 22, NULL);
	git_iterator_free(i);

	/* no auto expand (implies trees included) */
	i_opts.flags = GIT_ITERATOR_DONT_AUTOEXPAND;
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 12, NULL, 22, NULL);
	git_iterator_free(i);

	git_index_free(index);
}

void test_iterator_index__icase_1(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;
	int caps;

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_repository_index(&index, g_repo));
	caps = git_index_caps(index);

	/* force case sensitivity */
	cl_git_pass(git_index_set_caps(index, caps & ~GIT_INDEXCAP_IGNORE_CASE));

	/* autoexpand with no tree entries over range */
	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 7, NULL, 7, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 3, NULL, 3, NULL);
	git_iterator_free(i);

	/* auto expand with tree entries */
	i_opts.flags = GIT_ITERATOR_INCLUDE_TREES;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 8, NULL, 8, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 4, NULL, 4, NULL);
	git_iterator_free(i);

	/* no auto expand (implies trees included) */
	i_opts.flags = GIT_ITERATOR_DONT_AUTOEXPAND;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 5, NULL, 8, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 1, NULL, 4, NULL);
	git_iterator_free(i);

	/* force case insensitivity */
	cl_git_pass(git_index_set_caps(index, caps | GIT_INDEXCAP_IGNORE_CASE));

	/* autoexpand with no tree entries over range */
	i_opts.flags = 0;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 13, NULL, 13, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 5, NULL, 5, NULL);
	git_iterator_free(i);

	/* auto expand with tree entries */
	i_opts.flags = GIT_ITERATOR_INCLUDE_TREES;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 14, NULL, 14, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 6, NULL, 6, NULL);
	git_iterator_free(i);

	/* no auto expand (implies trees included) */
	i_opts.flags = GIT_ITERATOR_DONT_AUTOEXPAND;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 9, NULL, 14, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 1, NULL, 6, NULL);
	git_iterator_free(i);

	cl_git_pass(git_index_set_caps(index, caps));
	git_index_free(index);
}

void test_iterator_index__pathlist(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;
	git_vector filelist;

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "a"));
	cl_git_pass(git_vector_insert(&filelist, "B"));
	cl_git_pass(git_vector_insert(&filelist, "c"));
	cl_git_pass(git_vector_insert(&filelist, "D"));
	cl_git_pass(git_vector_insert(&filelist, "e"));
	cl_git_pass(git_vector_insert(&filelist, "k/1"));
	cl_git_pass(git_vector_insert(&filelist, "k/a"));
	cl_git_pass(git_vector_insert(&filelist, "L/1"));

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_repository_index(&index, g_repo));

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	/* Case sensitive */
	{
		const char *expected[] = {
			"B", "D", "L/1", "a", "c", "e", "k/1", "k/a" };
		size_t expected_len = 8;

		i_opts.start = NULL;
		i_opts.end = NULL;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Case INsensitive */
	{
		const char *expected[] = {
			"a", "B", "c", "D", "e", "k/1", "k/a", "L/1" };
		size_t expected_len = 8;

		i_opts.start = NULL;
		i_opts.end = NULL;
		i_opts.flags = GIT_ITERATOR_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Set a start, but no end.  Case sensitive. */
	{
		const char *expected[] = { "c", "e", "k/1", "k/a" };
		size_t expected_len = 4;

		i_opts.start = "c";
		i_opts.end = NULL;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Set a start, but no end.  Case INsensitive. */
	{
		const char *expected[] = { "c", "D", "e", "k/1", "k/a", "L/1" };
		size_t expected_len = 6;

		i_opts.start = "c";
		i_opts.end = NULL;
		i_opts.flags = GIT_ITERATOR_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Set no start, but an end.  Case sensitive. */
	{
		const char *expected[] = { "B", "D", "L/1", "a", "c", "e" };
		size_t expected_len = 6;

		i_opts.start = NULL;
		i_opts.end = "e";
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Set no start, but an end.  Case INsensitive. */
	{
		const char *expected[] = { "a", "B", "c", "D", "e" };
		size_t expected_len = 5;

		i_opts.start = NULL;
		i_opts.end = "e";
		i_opts.flags = GIT_ITERATOR_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Start and an end, case sensitive */
	{
		const char *expected[] = { "c", "e", "k/1" };
		size_t expected_len = 3;

		i_opts.start = "c";
		i_opts.end = "k/D";
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Start and an end, case sensitive */
	{
		const char *expected[] = { "k/1" };
		size_t expected_len = 1;

		i_opts.start = "k";
		i_opts.end = "k/D";
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Start and an end, case INsensitive */
	{
		const char *expected[] = { "c", "D", "e", "k/1", "k/a" };
		size_t expected_len = 5;

		i_opts.start = "c";
		i_opts.end = "k/D";
		i_opts.flags = GIT_ITERATOR_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Start and an end, case INsensitive */
	{
		const char *expected[] = { "k/1", "k/a" };
		size_t expected_len = 2;

		i_opts.start = "k";
		i_opts.end = "k/D";
		i_opts.flags = GIT_ITERATOR_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	git_index_free(index);
	git_vector_free(&filelist);
}

void test_iterator_index__pathlist_with_dirs(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;
	git_vector filelist;

	cl_git_pass(git_vector_init(&filelist, 5, NULL));

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_repository_index(&index, g_repo));

	/* Test that a prefix `k` matches folders, even without trailing slash */
	{
		const char *expected[] = { "k/1", "k/B", "k/D", "k/a", "k/c" };
		size_t expected_len = 5;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "k"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Test that a `k/` matches a folder */
	{
		const char *expected[] = { "k/1", "k/B", "k/D", "k/a", "k/c" };
		size_t expected_len = 5;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "k/"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* When the iterator is case sensitive, ensure we can't lookup the
	 * directory with the wrong case.
	 */
	{
		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "K/"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		cl_git_fail_with(GIT_ITEROVER, git_iterator_advance(NULL, i));
		git_iterator_free(i);
	}

	/* Test that case insensitive matching works. */
	{
		const char *expected[] = { "k/1", "k/a", "k/B", "k/c", "k/D" };
		size_t expected_len = 5;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "K/"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Test that case insensitive matching works without trailing slash. */
	{
		const char *expected[] = { "k/1", "k/a", "k/B", "k/c", "k/D" };
		size_t expected_len = 5;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "K"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	git_index_free(index);
	git_vector_free(&filelist);
}

void test_iterator_index__pathlist_with_dirs_include_trees(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;
	git_vector filelist;

	const char *expected[] = { "k/", "k/1", "k/B", "k/D", "k/a", "k/c" };
	size_t expected_len = 6;

	cl_git_pass(git_vector_init(&filelist, 5, NULL));

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_repository_index(&index, g_repo));

	git_vector_clear(&filelist);
	cl_git_pass(git_vector_insert(&filelist, "k"));

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;
	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, expected_len, expected, expected_len, expected);
	git_iterator_free(i);

	git_index_free(index);
	git_vector_free(&filelist);
}

void test_iterator_index__pathlist_1(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;
	git_vector filelist = GIT_VECTOR_INIT;
	int default_icase, expect;

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "0"));
	cl_git_pass(git_vector_insert(&filelist, "c"));
	cl_git_pass(git_vector_insert(&filelist, "D"));
	cl_git_pass(git_vector_insert(&filelist, "e"));
	cl_git_pass(git_vector_insert(&filelist, "k/1"));
	cl_git_pass(git_vector_insert(&filelist, "k/a"));

	/* In this test we DO NOT force a case setting on the index. */
	default_icase = ((git_index_caps(index) & GIT_INDEXCAP_IGNORE_CASE) != 0);

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	i_opts.start = "b";
	i_opts.end = "k/D";

	/* (c D e k/1 k/a ==> 5) vs (c e k/1 ==> 3) */
	expect = default_icase ? 5 : 3;

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, expect, NULL, expect, NULL);
	git_iterator_free(i);

	git_index_free(index);
	git_vector_free(&filelist);
}

void test_iterator_index__pathlist_2(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;
	git_vector filelist = GIT_VECTOR_INIT;
	int default_icase, expect;

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "0"));
	cl_git_pass(git_vector_insert(&filelist, "c"));
	cl_git_pass(git_vector_insert(&filelist, "D"));
	cl_git_pass(git_vector_insert(&filelist, "e"));
	cl_git_pass(git_vector_insert(&filelist, "k/"));
	cl_git_pass(git_vector_insert(&filelist, "k.a"));
	cl_git_pass(git_vector_insert(&filelist, "k.b"));
	cl_git_pass(git_vector_insert(&filelist, "kZZZZZZZ"));

	/* In this test we DO NOT force a case setting on the index. */
	default_icase = ((git_index_caps(index) & GIT_INDEXCAP_IGNORE_CASE) != 0);

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	i_opts.start = "b";
	i_opts.end = "k/D";

	/* (c D e k/1 k/a k/B k/c k/D) vs (c e k/1 k/B k/D) */
	expect = default_icase ? 8 : 5;

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, expect, NULL, expect, NULL);
	git_iterator_free(i);

	git_index_free(index);
	git_vector_free(&filelist);
}

void test_iterator_index__pathlist_four(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;
	git_vector filelist = GIT_VECTOR_INIT;
	int default_icase, expect;

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "0"));
	cl_git_pass(git_vector_insert(&filelist, "c"));
	cl_git_pass(git_vector_insert(&filelist, "D"));
	cl_git_pass(git_vector_insert(&filelist, "e"));
	cl_git_pass(git_vector_insert(&filelist, "k"));
	cl_git_pass(git_vector_insert(&filelist, "k.a"));
	cl_git_pass(git_vector_insert(&filelist, "k.b"));
	cl_git_pass(git_vector_insert(&filelist, "kZZZZZZZ"));

	/* In this test we DO NOT force a case setting on the index. */
	default_icase = ((git_index_caps(index) & GIT_INDEXCAP_IGNORE_CASE) != 0);

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	i_opts.start = "b";
	i_opts.end = "k/D";

	/* (c D e k/1 k/a k/B k/c k/D) vs (c e k/1 k/B k/D) */
	expect = default_icase ? 8 : 5;

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, expect, NULL, expect, NULL);
	git_iterator_free(i);

	git_index_free(index);
	git_vector_free(&filelist);
}

void test_iterator_index__pathlist_icase(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;
	int caps;
	git_vector filelist;

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "a"));
	cl_git_pass(git_vector_insert(&filelist, "B"));
	cl_git_pass(git_vector_insert(&filelist, "c"));
	cl_git_pass(git_vector_insert(&filelist, "D"));
	cl_git_pass(git_vector_insert(&filelist, "e"));
	cl_git_pass(git_vector_insert(&filelist, "k/1"));
	cl_git_pass(git_vector_insert(&filelist, "k/a"));
	cl_git_pass(git_vector_insert(&filelist, "L/1"));

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_repository_index(&index, g_repo));
	caps = git_index_caps(index);

	/* force case sensitivity */
	cl_git_pass(git_index_set_caps(index, caps & ~GIT_INDEXCAP_IGNORE_CASE));

	/* All indexfilelist iterator tests are "autoexpand with no tree entries" */

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 3, NULL, 3, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 1, NULL, 1, NULL);
	git_iterator_free(i);

	/* force case insensitivity */
	cl_git_pass(git_index_set_caps(index, caps | GIT_INDEXCAP_IGNORE_CASE));

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 5, NULL, 5, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 2, NULL, 2, NULL);
	git_iterator_free(i);

	cl_git_pass(git_index_set_caps(index, caps));
	git_index_free(index);
	git_vector_free(&filelist);
}

void test_iterator_index__pathlist_with_directory(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;
	git_tree *tree;
	git_index *index;

	g_repo = cl_git_sandbox_init("testrepo2");
	git_repository_head_tree(&tree, g_repo);

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "subdir"));

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 4, NULL, 4, NULL);
	git_iterator_free(i);

	git_index_free(index);
	git_tree_free(tree);
	git_vector_free(&filelist);
}

static void create_paths(git_index *index, const char *root, int depth)
{
	git_buf fullpath = GIT_BUF_INIT;
	git_index_entry entry;
	size_t root_len;
	int i;

	if (root) {
		cl_git_pass(git_buf_puts(&fullpath, root));
		cl_git_pass(git_buf_putc(&fullpath, '/'));
	}

	root_len = fullpath.size;

	for (i = 0; i < 8; i++) {
		bool file = (depth == 0 || (i % 2) == 0);
		git_buf_truncate(&fullpath, root_len);
		cl_git_pass(git_buf_printf(&fullpath, "item%d", i));

		if (file) {
			memset(&entry, 0, sizeof(git_index_entry));
			entry.path = fullpath.ptr;
			entry.mode = GIT_FILEMODE_BLOB;
			git_oid_fromstr(&entry.id, "d44e18fb93b7107b5cd1b95d601591d77869a1b6");

			cl_git_pass(git_index_add(index, &entry));
		} else if (depth > 0) {
			create_paths(index, fullpath.ptr, (depth - 1));
		}
	}

	git_buf_free(&fullpath);
}

void test_iterator_index__pathlist_for_deeply_nested_item(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;
	git_vector filelist;

	cl_git_pass(git_vector_init(&filelist, 5, NULL));

	g_repo = cl_git_sandbox_init("icase");
	cl_git_pass(git_repository_index(&index, g_repo));

	create_paths(index, NULL, 3);

	/* Ensure that we find the single path we're interested in */
	{
		const char *expected[] = { "item1/item3/item5/item7" };
		size_t expected_len = 1;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "item1/item3/item5/item7"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	{
		const char *expected[] = {
			"item1/item3/item5/item0", "item1/item3/item5/item1",
			"item1/item3/item5/item2", "item1/item3/item5/item3",
			"item1/item3/item5/item4", "item1/item3/item5/item5",
			"item1/item3/item5/item6", "item1/item3/item5/item7",
		};
		size_t expected_len = 8;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "item1/item3/item5/"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	{
		const char *expected[] = {
			"item1/item3/item0",
			"item1/item3/item1/item0", "item1/item3/item1/item1",
			"item1/item3/item1/item2", "item1/item3/item1/item3",
			"item1/item3/item1/item4", "item1/item3/item1/item5",
			"item1/item3/item1/item6", "item1/item3/item1/item7",
			"item1/item3/item2",
			"item1/item3/item3/item0", "item1/item3/item3/item1",
			"item1/item3/item3/item2", "item1/item3/item3/item3",
			"item1/item3/item3/item4", "item1/item3/item3/item5",
			"item1/item3/item3/item6", "item1/item3/item3/item7",
			"item1/item3/item4",
			"item1/item3/item5/item0", "item1/item3/item5/item1",
			"item1/item3/item5/item2", "item1/item3/item5/item3",
			"item1/item3/item5/item4", "item1/item3/item5/item5",
			"item1/item3/item5/item6", "item1/item3/item5/item7",
			"item1/item3/item6",
			"item1/item3/item7/item0", "item1/item3/item7/item1",
			"item1/item3/item7/item2", "item1/item3/item7/item3",
			"item1/item3/item7/item4", "item1/item3/item7/item5",
			"item1/item3/item7/item6", "item1/item3/item7/item7",
		};
		size_t expected_len = 36;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "item1/item3/"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Ensure that we find the single path we're interested in, and we find
	 * it efficiently, and don't stat the entire world to get there.
	 */
	{
		const char *expected[] = {
			"item0", "item1/item2", "item5/item7/item4", "item6",
			"item7/item3/item1/item6" };
		size_t expected_len = 5;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "item7/item3/item1/item6"));
		cl_git_pass(git_vector_insert(&filelist, "item6"));
		cl_git_pass(git_vector_insert(&filelist, "item5/item7/item4"));
		cl_git_pass(git_vector_insert(&filelist, "item1/item2"));
		cl_git_pass(git_vector_insert(&filelist, "item0"));

		/* also add some things that don't exist or don't match the right type */
		cl_git_pass(git_vector_insert(&filelist, "item2/"));
		cl_git_pass(git_vector_insert(&filelist, "itemN"));
		cl_git_pass(git_vector_insert(&filelist, "item1/itemA"));
		cl_git_pass(git_vector_insert(&filelist, "item5/item3/item4/"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	git_index_free(index);
	git_vector_free(&filelist);
}

void test_iterator_index__advance_over(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;

	i_opts.flags |= GIT_ITERATOR_DONT_IGNORE_CASE |
		GIT_ITERATOR_DONT_AUTOEXPAND;

	g_repo = cl_git_sandbox_init("icase");
	cl_git_pass(git_repository_index(&index, g_repo));

	create_paths(index, NULL, 1);

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));

	expect_advance_over(i, "B", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "D", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "F", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "H", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "J", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "L/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "a", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "c", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "e", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "g", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "i", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item0", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item1/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item2", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item3/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item4", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item5/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item6", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item7/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "k/", GIT_ITERATOR_STATUS_NORMAL);

	cl_git_fail_with(GIT_ITEROVER, git_iterator_advance(NULL, i));
	git_iterator_free(i);
	git_index_free(index);
}

void test_iterator_index__advance_into(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;

	g_repo = cl_git_sandbox_init("icase");

	i_opts.flags |= GIT_ITERATOR_DONT_IGNORE_CASE |
		GIT_ITERATOR_DONT_AUTOEXPAND;

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_advance_into(i, "B");
	expect_advance_into(i, "D");
	expect_advance_into(i, "F");
	expect_advance_into(i, "H");
	expect_advance_into(i, "J");
	expect_advance_into(i, "L/");
	expect_advance_into(i, "L/1");
	expect_advance_into(i, "L/B");
	expect_advance_into(i, "L/D");
	expect_advance_into(i, "L/a");
	expect_advance_into(i, "L/c");
	expect_advance_into(i, "a");
	expect_advance_into(i, "c");
	expect_advance_into(i, "e");
	expect_advance_into(i, "g");
	expect_advance_into(i, "i");
	expect_advance_into(i, "k/");
	expect_advance_into(i, "k/1");
	expect_advance_into(i, "k/B");
	expect_advance_into(i, "k/D");
	expect_advance_into(i, "k/a");
	expect_advance_into(i, "k/c");

	cl_git_fail_with(GIT_ITEROVER, git_iterator_advance(NULL, i));
	git_iterator_free(i);
	git_index_free(index);
}

void test_iterator_index__advance_into_and_over(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;

	g_repo = cl_git_sandbox_init("icase");

	i_opts.flags |= GIT_ITERATOR_DONT_IGNORE_CASE |
	GIT_ITERATOR_DONT_AUTOEXPAND;

	cl_git_pass(git_repository_index(&index, g_repo));

	create_paths(index, NULL, 2);

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_advance_into(i, "B");
	expect_advance_into(i, "D");
	expect_advance_into(i, "F");
	expect_advance_into(i, "H");
	expect_advance_into(i, "J");
	expect_advance_into(i, "L/");
	expect_advance_into(i, "L/1");
	expect_advance_into(i, "L/B");
	expect_advance_into(i, "L/D");
	expect_advance_into(i, "L/a");
	expect_advance_into(i, "L/c");
	expect_advance_into(i, "a");
	expect_advance_into(i, "c");
	expect_advance_into(i, "e");
	expect_advance_into(i, "g");
	expect_advance_into(i, "i");
	expect_advance_into(i, "item0");
	expect_advance_into(i, "item1/");
	expect_advance_into(i, "item1/item0");
	expect_advance_into(i, "item1/item1/");
	expect_advance_into(i, "item1/item1/item0");
	expect_advance_into(i, "item1/item1/item1");
	expect_advance_into(i, "item1/item1/item2");
	expect_advance_into(i, "item1/item1/item3");
	expect_advance_into(i, "item1/item1/item4");
	expect_advance_into(i, "item1/item1/item5");
	expect_advance_into(i, "item1/item1/item6");
	expect_advance_into(i, "item1/item1/item7");
	expect_advance_into(i, "item1/item2");
	expect_advance_over(i, "item1/item3/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item1/item4", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item1/item5/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item1/item6", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item1/item7/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_into(i, "item2");
	expect_advance_over(i, "item3/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item4", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item5/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item6", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "item7/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_into(i, "k/");
	expect_advance_into(i, "k/1");
	expect_advance_into(i, "k/B");
	expect_advance_into(i, "k/D");
	expect_advance_into(i, "k/a");
	expect_advance_into(i, "k/c");

	cl_git_fail_with(GIT_ITEROVER, git_iterator_advance(NULL, i));
	git_iterator_free(i);
	git_index_free(index);
}

static void add_conflict(
	git_index *index,
	const char *ancestor_path,
	const char *our_path,
	const char *their_path)
{
	git_index_entry ancestor = {{0}}, ours = {{0}}, theirs = {{0}};

	ancestor.path = ancestor_path;
	ancestor.mode = GIT_FILEMODE_BLOB;
	git_oid_fromstr(&ancestor.id, "d44e18fb93b7107b5cd1b95d601591d77869a1b6");
	GIT_IDXENTRY_STAGE_SET(&ancestor, 1);

	ours.path = our_path;
	ours.mode = GIT_FILEMODE_BLOB;
	git_oid_fromstr(&ours.id, "d44e18fb93b7107b5cd1b95d601591d77869a1b6");
	GIT_IDXENTRY_STAGE_SET(&ours, 2);

	theirs.path = their_path;
	theirs.mode = GIT_FILEMODE_BLOB;
	git_oid_fromstr(&theirs.id, "d44e18fb93b7107b5cd1b95d601591d77869a1b6");
	GIT_IDXENTRY_STAGE_SET(&theirs, 3);

	cl_git_pass(git_index_conflict_add(index, &ancestor, &ours, &theirs));
}

void test_iterator_index__include_conflicts(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;

	i_opts.flags |= GIT_ITERATOR_DONT_IGNORE_CASE |
		GIT_ITERATOR_DONT_AUTOEXPAND;

	g_repo = cl_git_sandbox_init("icase");
	cl_git_pass(git_repository_index(&index, g_repo));

	add_conflict(index, "CONFLICT1", "CONFLICT1" ,"CONFLICT1");
	add_conflict(index, "ZZZ-CONFLICT2.ancestor", "ZZZ-CONFLICT2.ours", "ZZZ-CONFLICT2.theirs");
	add_conflict(index, "ancestor.conflict3", "ours.conflict3", "theirs.conflict3");
	add_conflict(index, "zzz-conflict4", "zzz-conflict4", "zzz-conflict4");

	/* Iterate the index, ensuring that conflicts are not included */
	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));

	expect_advance_over(i, "B", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "D", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "F", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "H", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "J", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "L/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "a", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "c", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "e", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "g", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "i", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "k/", GIT_ITERATOR_STATUS_NORMAL);

	cl_git_fail_with(GIT_ITEROVER, git_iterator_advance(NULL, i));
	git_iterator_free(i);

	/* Try again, returning conflicts */
	i_opts.flags |= GIT_ITERATOR_INCLUDE_CONFLICTS;

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));

	expect_advance_over(i, "B", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "CONFLICT1", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "CONFLICT1", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "CONFLICT1", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "D", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "F", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "H", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "J", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "L/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "ZZZ-CONFLICT2.ancestor", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "ZZZ-CONFLICT2.ours", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "ZZZ-CONFLICT2.theirs", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "a", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "ancestor.conflict3", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "c", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "e", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "g", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "i", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "k/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "ours.conflict3", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "theirs.conflict3", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "zzz-conflict4", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "zzz-conflict4", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "zzz-conflict4", GIT_ITERATOR_STATUS_NORMAL);

	cl_git_fail_with(GIT_ITEROVER, git_iterator_advance(NULL, i));
	git_iterator_free(i);

	git_index_free(index);
}
