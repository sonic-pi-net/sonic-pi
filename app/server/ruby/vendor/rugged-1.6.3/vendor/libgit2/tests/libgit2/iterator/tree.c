#include "clar_libgit2.h"
#include "iterator.h"
#include "repository.h"
#include "futils.h"
#include "tree.h"
#include "../submodule/submodule_helpers.h"
#include "../diff/diff_helpers.h"
#include "iterator_helpers.h"
#include <stdarg.h>

static git_repository *g_repo;

void test_iterator_tree__initialize(void)
{
}

void test_iterator_tree__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

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
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	const git_index_entry *entry;
	int error, count = 0, count_post_reset = 0;

	g_repo = cl_git_sandbox_init(sandbox);

	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;
	i_opts.start = start;
	i_opts.end = end;

	cl_assert(t = resolve_commit_oid_to_tree(g_repo, treeish));
	cl_git_pass(git_iterator_for_tree(&i, t, &i_opts));

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
	cl_git_pass(git_iterator_reset(i));

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

void test_iterator_tree__0(void)
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

void test_iterator_tree__1(void)
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

void test_iterator_tree__2(void)
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

void test_iterator_tree__3(void)
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

void test_iterator_tree__4(void)
{
	tree_iterator_test(
		"attr", "24fa9a9fc4e202313e24b648087495441dab432b", NULL, NULL,
		23, expected_tree_4);
}

void test_iterator_tree__4_ranged(void)
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

void test_iterator_tree__ranged_0(void)
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

void test_iterator_tree__ranged_1(void)
{
	tree_iterator_test(
		"attr", "24fa9a9fc4e202313e24b648087495441dab432b",
		"sub/subdir_test2.txt", "sub/subdir_test2.txt",
		1, expected_tree_ranged_1);
}

void test_iterator_tree__range_empty_0(void)
{
	tree_iterator_test(
		"attr", "24fa9a9fc4e202313e24b648087495441dab432b",
		"empty", "empty", 0, NULL);
}

void test_iterator_tree__range_empty_1(void)
{
	tree_iterator_test(
		"attr", "24fa9a9fc4e202313e24b648087495441dab432b",
		"z_empty_after", NULL, 0, NULL);
}

void test_iterator_tree__range_empty_2(void)
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

	cl_git_pass(git_iterator_current_tree_entry(&te, i));
	cl_assert(te);
	cl_assert(git_oid_streq(&te->oid, oid) == 0);

	cl_git_pass(git_iterator_current(&ie, i));

	if (oid_p) {
		cl_git_pass(git_iterator_current_parent_tree(&tree, i, 0));
		cl_assert(tree);
		cl_assert(git_oid_streq(git_tree_id(tree), oid_p) == 0);
	}

	if (oid_pp) {
		cl_git_pass(git_iterator_current_parent_tree(&tree, i, 1));
		cl_assert(tree);
		cl_assert(git_oid_streq(git_tree_id(tree), oid_pp) == 0);
	}

	if (oid_ppp) {
		cl_git_pass(git_iterator_current_parent_tree(&tree, i, 2));
		cl_assert(tree);
		cl_assert(git_oid_streq(git_tree_id(tree), oid_ppp) == 0);
	}
}

void test_iterator_tree__special_functions(void)
{
	git_tree *t;
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	const git_index_entry *entry;
	int error, cases = 0;
	const char *rootoid = "ce39a97a7fb1fa90bcf5e711249c1e507476ae0e";

	g_repo = cl_git_sandbox_init("attr");

	t = resolve_commit_oid_to_tree(
		g_repo, "24fa9a9fc4e202313e24b648087495441dab432b");
	cl_assert(t != NULL);

	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

	cl_git_pass(git_iterator_for_tree(&i, t, &i_opts));

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

static void check_tree_range(
	git_repository *repo,
	const char *start,
	const char *end,
	bool ignore_case,
	int expected_count)
{
	git_tree *head;
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	int error, count;

	i_opts.flags = ignore_case ? GIT_ITERATOR_IGNORE_CASE : GIT_ITERATOR_DONT_IGNORE_CASE;
	i_opts.start = start;
	i_opts.end = end;

	cl_git_pass(git_repository_head_tree(&head, repo));

	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));

	for (count = 0; !(error = git_iterator_advance(NULL, i)); ++count)
		/* count em up */;

	cl_assert_equal_i(GIT_ITEROVER, error);
	cl_assert_equal_i(expected_count, count);

	git_iterator_free(i);
	git_tree_free(head);
}

void test_iterator_tree__range_icase(void)
{
	g_repo = cl_git_sandbox_init("testrepo");

	check_tree_range(g_repo, "B", "C", false, 0);
	check_tree_range(g_repo, "B", "C", true, 1);
	check_tree_range(g_repo, "b", "c", false, 1);
	check_tree_range(g_repo, "b", "c", true, 1);

	check_tree_range(g_repo, "a", "z", false, 3);
	check_tree_range(g_repo, "a", "z", true, 4);
	check_tree_range(g_repo, "A", "Z", false, 1);
	check_tree_range(g_repo, "A", "Z", true, 4);
	check_tree_range(g_repo, "a", "Z", false, 0);
	check_tree_range(g_repo, "a", "Z", true, 4);
	check_tree_range(g_repo, "A", "z", false, 4);
	check_tree_range(g_repo, "A", "z", true, 4);

	check_tree_range(g_repo, "new.txt", "new.txt", true, 1);
	check_tree_range(g_repo, "new.txt", "new.txt", false, 1);
	check_tree_range(g_repo, "README", "README", true, 1);
	check_tree_range(g_repo, "README", "README", false, 1);
}

void test_iterator_tree__icase_0(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_tree *head;

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_repository_head_tree(&head, g_repo));

	/* auto expand with no tree entries */
	cl_git_pass(git_iterator_for_tree(&i, head, NULL));
	expect_iterator_items(i, 20, NULL, 20, NULL);
	git_iterator_free(i);

	/* auto expand with tree entries */
	i_opts.flags = GIT_ITERATOR_INCLUDE_TREES;

	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 22, NULL, 22, NULL);
	git_iterator_free(i);

	/* no auto expand (implies trees included) */
	i_opts.flags = GIT_ITERATOR_DONT_AUTOEXPAND;

	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 12, NULL, 22, NULL);
	git_iterator_free(i);

	git_tree_free(head);
}

void test_iterator_tree__icase_1(void)
{
	git_iterator *i;
	git_tree *head;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_repository_head_tree(&head, g_repo));

	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

	/* auto expand with no tree entries */
	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 7, NULL, 7, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 3, NULL, 3, NULL);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;

	/* auto expand with tree entries */
	i_opts.start = "c";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 8, NULL, 8, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 4, NULL, 4, NULL);
	git_iterator_free(i);

	/* no auto expand (implies trees included) */
	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE | GIT_ITERATOR_DONT_AUTOEXPAND;
	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 5, NULL, 8, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 1, NULL, 4, NULL);
	git_iterator_free(i);

	/* auto expand with no tree entries */
	i_opts.flags = GIT_ITERATOR_IGNORE_CASE;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 13, NULL, 13, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 5, NULL, 5, NULL);
	git_iterator_free(i);

	/* auto expand with tree entries */
	i_opts.flags = GIT_ITERATOR_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 14, NULL, 14, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 6, NULL, 6, NULL);
	git_iterator_free(i);

	/* no auto expand (implies trees included) */
	i_opts.flags = GIT_ITERATOR_IGNORE_CASE | GIT_ITERATOR_DONT_AUTOEXPAND;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 9, NULL, 14, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 1, NULL, 6, NULL);
	git_iterator_free(i);

	git_tree_free(head);
}

void test_iterator_tree__icase_2(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_tree *head;
	static const char *expect_basic[] = {
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
		NULL,
	};
	static const char *expect_trees[] = {
		"current_file",
		"file_deleted",
		"modified_file",
		"staged_changes",
		"staged_changes_file_deleted",
		"staged_changes_modified_file",
		"staged_delete_file_deleted",
		"staged_delete_modified_file",
		"subdir.txt",
		"subdir/",
		"subdir/current_file",
		"subdir/deleted_file",
		"subdir/modified_file",
		NULL,
	};
	static const char *expect_noauto[] = {
		"current_file",
		"file_deleted",
		"modified_file",
		"staged_changes",
		"staged_changes_file_deleted",
		"staged_changes_modified_file",
		"staged_delete_file_deleted",
		"staged_delete_modified_file",
		"subdir.txt",
		"subdir/",
		NULL
	};

	g_repo = cl_git_sandbox_init("status");

	cl_git_pass(git_repository_head_tree(&head, g_repo));

	/* auto expand with no tree entries */
	cl_git_pass(git_iterator_for_tree(&i, head, NULL));
	expect_iterator_items(i, 12, expect_basic, 12, expect_basic);
	git_iterator_free(i);

	/* auto expand with tree entries */
	i_opts.flags = GIT_ITERATOR_INCLUDE_TREES;

	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 13, expect_trees, 13, expect_trees);
	git_iterator_free(i);

	/* no auto expand (implies trees included) */
	i_opts.flags = GIT_ITERATOR_DONT_AUTOEXPAND;

	cl_git_pass(git_iterator_for_tree(&i, head, &i_opts));
	expect_iterator_items(i, 10, expect_noauto, 13, expect_trees);
	git_iterator_free(i);

	git_tree_free(head);
}

/* "b=name,t=name", blob_id, tree_id */
static void build_test_tree(
	git_oid *out, git_repository *repo, const char *fmt, ...)
{
	git_oid *id;
	git_treebuilder *builder;
	const char *scan = fmt, *next;
	char type, delimiter;
	git_filemode_t mode = GIT_FILEMODE_BLOB;
	git_str name = GIT_STR_INIT;
	va_list arglist;

	cl_git_pass(git_treebuilder_new(&builder, repo, NULL)); /* start builder */

	va_start(arglist, fmt);
	while (*scan) {
		switch (type = *scan++) {
		case 't': case 'T': mode = GIT_FILEMODE_TREE; break;
		case 'b': case 'B': mode = GIT_FILEMODE_BLOB; break;
		default:
			cl_assert(type == 't' || type == 'T' || type == 'b' || type == 'B');
		}

		delimiter = *scan++; /* read and skip delimiter */
		for (next = scan; *next && *next != delimiter; ++next)
			/* seek end */;
		cl_git_pass(git_str_set(&name, scan, (size_t)(next - scan)));
		for (scan = next; *scan && (*scan == delimiter || *scan == ','); ++scan)
			/* skip delimiter and optional comma */;

		id = va_arg(arglist, git_oid *);

		cl_git_pass(git_treebuilder_insert(NULL, builder, name.ptr, id, mode));
	}
	va_end(arglist);

	cl_git_pass(git_treebuilder_write(out, builder));

	git_treebuilder_free(builder);
	git_str_dispose(&name);
}

void test_iterator_tree__case_conflicts_0(void)
{
	const char *blob_sha = "d44e18fb93b7107b5cd1b95d601591d77869a1b6";
	git_tree *tree;
	git_oid blob_id, biga_id, littlea_id, tree_id;
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;

	const char *expect_cs[] = {
		"A/1.file", "A/3.file", "a/2.file", "a/4.file" };
	const char *expect_ci[] = {
		"A/1.file", "a/2.file", "A/3.file", "a/4.file" };
	const char *expect_cs_trees[] = {
		"A/", "A/1.file", "A/3.file", "a/", "a/2.file", "a/4.file" };
	const char *expect_ci_trees[] = {
		"A/", "A/1.file", "a/2.file", "A/3.file", "a/4.file" };

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_oid__fromstr(&blob_id, blob_sha, GIT_OID_SHA1)); /* lookup blob */

	/* create tree with: A/1.file, A/3.file, a/2.file, a/4.file */
	build_test_tree(
		&biga_id, g_repo, "b|1.file|,b|3.file|", &blob_id, &blob_id);
	build_test_tree(
		&littlea_id, g_repo, "b|2.file|,b|4.file|", &blob_id, &blob_id);
	build_test_tree(
		&tree_id, g_repo, "t|A|,t|a|", &biga_id, &littlea_id);

	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 4, expect_cs, 4, expect_cs);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 4, expect_ci, 4, expect_ci);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 6, expect_cs_trees, 6, expect_cs_trees);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 5, expect_ci_trees, 5, expect_ci_trees);
	git_iterator_free(i);

	git_tree_free(tree);
}

void test_iterator_tree__case_conflicts_1(void)
{
	const char *blob_sha = "d44e18fb93b7107b5cd1b95d601591d77869a1b6";
	git_tree *tree;
	git_oid blob_id, Ab_id, biga_id, littlea_id, tree_id;
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;

	const char *expect_cs[] = {
		"A/a", "A/b/1", "A/c", "a/C", "a/a", "a/b" };
	const char *expect_ci[] = {
		"A/a", "a/b", "A/b/1", "A/c" };
	const char *expect_cs_trees[] = {
		"A/", "A/a", "A/b/", "A/b/1", "A/c", "a/", "a/C", "a/a", "a/b" };
	const char *expect_ci_trees[] = {
		"A/", "A/a", "a/b", "A/b/", "A/b/1", "A/c" };

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_oid__fromstr(&blob_id, blob_sha, GIT_OID_SHA1)); /* lookup blob */

	/* create: A/a A/b/1 A/c a/a a/b a/C */
	build_test_tree(&Ab_id, g_repo, "b|1|", &blob_id);
	build_test_tree(
		&biga_id, g_repo, "b|a|,t|b|,b|c|", &blob_id, &Ab_id, &blob_id);
	build_test_tree(
		&littlea_id, g_repo, "b|a|,b|b|,b|C|", &blob_id, &blob_id, &blob_id);
	build_test_tree(
		&tree_id, g_repo, "t|A|,t|a|", &biga_id, &littlea_id);

	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 6, expect_cs, 6, expect_cs);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 4, expect_ci, 4, expect_ci);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 9, expect_cs_trees, 9, expect_cs_trees);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 6, expect_ci_trees, 6, expect_ci_trees);
	git_iterator_free(i);

	git_tree_free(tree);
}

void test_iterator_tree__case_conflicts_2(void)
{
	const char *blob_sha = "d44e18fb93b7107b5cd1b95d601591d77869a1b6";
	git_tree *tree;
	git_oid blob_id, d1, d2, c1, c2, b1, b2, a1, a2, tree_id;
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;

	const char *expect_cs[] = {
		"A/B/C/D/16", "A/B/C/D/foo", "A/B/C/d/15",  "A/B/C/d/FOO",
		"A/B/c/D/14", "A/B/c/D/foo", "A/B/c/d/13",  "A/B/c/d/FOO",
		"A/b/C/D/12", "A/b/C/D/foo", "A/b/C/d/11",  "A/b/C/d/FOO",
		"A/b/c/D/10", "A/b/c/D/foo", "A/b/c/d/09",  "A/b/c/d/FOO",
		"a/B/C/D/08", "a/B/C/D/foo", "a/B/C/d/07", "a/B/C/d/FOO",
		"a/B/c/D/06", "a/B/c/D/foo", "a/B/c/d/05", "a/B/c/d/FOO",
		"a/b/C/D/04", "a/b/C/D/foo", "a/b/C/d/03", "a/b/C/d/FOO",
		"a/b/c/D/02", "a/b/c/D/foo", "a/b/c/d/01", "a/b/c/d/FOO", };
	const char *expect_ci[] = {
		"a/b/c/d/01", "a/b/c/D/02", "a/b/C/d/03", "a/b/C/D/04",
		"a/B/c/d/05", "a/B/c/D/06", "a/B/C/d/07", "a/B/C/D/08",
		"A/b/c/d/09", "A/b/c/D/10", "A/b/C/d/11", "A/b/C/D/12",
		"A/B/c/d/13", "A/B/c/D/14", "A/B/C/d/15", "A/B/C/D/16",
		"A/B/C/D/foo", };
	const char *expect_ci_trees[] = {
		"A/", "A/B/", "A/B/C/", "A/B/C/D/",
		"a/b/c/d/01", "a/b/c/D/02", "a/b/C/d/03", "a/b/C/D/04",
		"a/B/c/d/05", "a/B/c/D/06", "a/B/C/d/07", "a/B/C/D/08",
		"A/b/c/d/09", "A/b/c/D/10", "A/b/C/d/11", "A/b/C/D/12",
		"A/B/c/d/13", "A/B/c/D/14", "A/B/C/d/15", "A/B/C/D/16",
		"A/B/C/D/foo", };

	g_repo = cl_git_sandbox_init("icase");

	cl_git_pass(git_oid__fromstr(&blob_id, blob_sha, GIT_OID_SHA1)); /* lookup blob */

	build_test_tree(&d1, g_repo, "b|16|,b|foo|", &blob_id, &blob_id);
	build_test_tree(&d2, g_repo, "b|15|,b|FOO|", &blob_id, &blob_id);
	build_test_tree(&c1, g_repo, "t|D|,t|d|", &d1, &d2);
	build_test_tree(&d1, g_repo, "b|14|,b|foo|", &blob_id, &blob_id);
	build_test_tree(&d2, g_repo, "b|13|,b|FOO|", &blob_id, &blob_id);
	build_test_tree(&c2, g_repo, "t|D|,t|d|", &d1, &d2);
	build_test_tree(&b1, g_repo, "t|C|,t|c|", &c1, &c2);

	build_test_tree(&d1, g_repo, "b|12|,b|foo|", &blob_id, &blob_id);
	build_test_tree(&d2, g_repo, "b|11|,b|FOO|", &blob_id, &blob_id);
	build_test_tree(&c1, g_repo, "t|D|,t|d|", &d1, &d2);
	build_test_tree(&d1, g_repo, "b|10|,b|foo|", &blob_id, &blob_id);
	build_test_tree(&d2, g_repo, "b|09|,b|FOO|", &blob_id, &blob_id);
	build_test_tree(&c2, g_repo, "t|D|,t|d|", &d1, &d2);
	build_test_tree(&b2, g_repo, "t|C|,t|c|", &c1, &c2);

	build_test_tree(&a1, g_repo, "t|B|,t|b|", &b1, &b2);

	build_test_tree(&d1, g_repo, "b|08|,b|foo|", &blob_id, &blob_id);
	build_test_tree(&d2, g_repo, "b|07|,b|FOO|", &blob_id, &blob_id);
	build_test_tree(&c1, g_repo, "t|D|,t|d|", &d1, &d2);
	build_test_tree(&d1, g_repo, "b|06|,b|foo|", &blob_id, &blob_id);
	build_test_tree(&d2, g_repo, "b|05|,b|FOO|", &blob_id, &blob_id);
	build_test_tree(&c2, g_repo, "t|D|,t|d|", &d1, &d2);
	build_test_tree(&b1, g_repo, "t|C|,t|c|", &c1, &c2);

	build_test_tree(&d1, g_repo, "b|04|,b|foo|", &blob_id, &blob_id);
	build_test_tree(&d2, g_repo, "b|03|,b|FOO|", &blob_id, &blob_id);
	build_test_tree(&c1, g_repo, "t|D|,t|d|", &d1, &d2);
	build_test_tree(&d1, g_repo, "b|02|,b|foo|", &blob_id, &blob_id);
	build_test_tree(&d2, g_repo, "b|01|,b|FOO|", &blob_id, &blob_id);
	build_test_tree(&c2, g_repo, "t|D|,t|d|", &d1, &d2);
	build_test_tree(&b2, g_repo, "t|C|,t|c|", &c1, &c2);

	build_test_tree(&a2, g_repo, "t|B|,t|b|", &b1, &b2);

	build_test_tree(&tree_id, g_repo, "t/A/,t/a/", &a1, &a2);

	cl_git_pass(git_tree_lookup(&tree, g_repo, &tree_id));

	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 32, expect_cs, 32, expect_cs);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 17, expect_ci, 17, expect_ci);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 21, expect_ci_trees, 21, expect_ci_trees);
	git_iterator_free(i);

	git_tree_free(tree);
}

void test_iterator_tree__pathlist(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;
	git_tree *tree;
	bool default_icase;
	int expect;

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "a"));
	cl_git_pass(git_vector_insert(&filelist, "B"));
	cl_git_pass(git_vector_insert(&filelist, "c"));
	cl_git_pass(git_vector_insert(&filelist, "D"));
	cl_git_pass(git_vector_insert(&filelist, "e"));
	cl_git_pass(git_vector_insert(&filelist, "k.a"));
	cl_git_pass(git_vector_insert(&filelist, "k.b"));
	cl_git_pass(git_vector_insert(&filelist, "k/1"));
	cl_git_pass(git_vector_insert(&filelist, "k/a"));
	cl_git_pass(git_vector_insert(&filelist, "kZZZZZZZ"));
	cl_git_pass(git_vector_insert(&filelist, "L/1"));

	g_repo = cl_git_sandbox_init("icase");
	git_repository_head_tree(&tree, g_repo);

	/* All indexfilelist iterator tests are "autoexpand with no tree entries" */
	/* In this test we DO NOT force a case on the iterators and verify default behavior. */

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 8, NULL, 8, NULL);
	git_iterator_free(i);

	i_opts.start = "c";
	i_opts.end = NULL;
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	default_icase = git_iterator_ignore_case(i);
	/* (c D e k/1 k/a L ==> 6) vs (c e k/1 k/a ==> 4) */
	expect = ((default_icase) ? 6 : 4);
	expect_iterator_items(i, expect, NULL, expect, NULL);
	git_iterator_free(i);

	i_opts.start = NULL;
	i_opts.end = "e";
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	default_icase = git_iterator_ignore_case(i);
	/* (a B c D e ==> 5) vs (B D L/1 a c e ==> 6) */
	expect = ((default_icase) ? 5 : 6);
	expect_iterator_items(i, expect, NULL, expect, NULL);
	git_iterator_free(i);

	git_vector_free(&filelist);
	git_tree_free(tree);
}

void test_iterator_tree__pathlist_icase(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;
	git_tree *tree;

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "a"));
	cl_git_pass(git_vector_insert(&filelist, "B"));
	cl_git_pass(git_vector_insert(&filelist, "c"));
	cl_git_pass(git_vector_insert(&filelist, "D"));
	cl_git_pass(git_vector_insert(&filelist, "e"));
	cl_git_pass(git_vector_insert(&filelist, "k.a"));
	cl_git_pass(git_vector_insert(&filelist, "k.b"));
	cl_git_pass(git_vector_insert(&filelist, "k/1"));
	cl_git_pass(git_vector_insert(&filelist, "k/a"));
	cl_git_pass(git_vector_insert(&filelist, "kZZZZ"));
	cl_git_pass(git_vector_insert(&filelist, "L/1"));

	g_repo = cl_git_sandbox_init("icase");
	git_repository_head_tree(&tree, g_repo);

	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;
	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 3, NULL, 3, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 1, NULL, 1, NULL);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 5, NULL, 5, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, 2, NULL, 2, NULL);
	git_iterator_free(i);

	git_vector_free(&filelist);
	git_tree_free(tree);
}

void test_iterator_tree__pathlist_with_directory(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;
	git_tree *tree;

	const char *expected[] = { "subdir/README", "subdir/new.txt",
		"subdir/subdir2/README", "subdir/subdir2/new.txt" };
	size_t expected_len = 4;

	const char *expected2[] = { "subdir/subdir2/README", "subdir/subdir2/new.txt" };
	size_t expected_len2 = 2;

	g_repo = cl_git_sandbox_init("testrepo2");
	git_repository_head_tree(&tree, g_repo);

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "subdir"));

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;
	i_opts.flags |= GIT_ITERATOR_DONT_IGNORE_CASE;

	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, expected_len, expected, expected_len, expected);
	git_iterator_free(i);

	git_vector_clear(&filelist);
	cl_git_pass(git_vector_insert(&filelist, "subdir/"));

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, expected_len, expected, expected_len, expected);
	git_iterator_free(i);

	git_vector_clear(&filelist);
	cl_git_pass(git_vector_insert(&filelist, "subdir/subdir2"));

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, expected_len2, expected2, expected_len2, expected2);
	git_iterator_free(i);

	git_tree_free(tree);
	git_vector_free(&filelist);
}

void test_iterator_tree__pathlist_with_directory_include_tree_nodes(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;
	git_tree *tree;

	const char *expected[] = { "subdir/", "subdir/README", "subdir/new.txt",
		"subdir/subdir2/", "subdir/subdir2/README", "subdir/subdir2/new.txt" };
	size_t expected_len = 6;

	g_repo = cl_git_sandbox_init("testrepo2");
	git_repository_head_tree(&tree, g_repo);

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "subdir"));

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;
	i_opts.flags |= GIT_ITERATOR_DONT_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;

	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	expect_iterator_items(i, expected_len, expected, expected_len, expected);
	git_iterator_free(i);

	git_tree_free(tree);
	git_vector_free(&filelist);
}

void test_iterator_tree__pathlist_no_match(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;
	git_tree *tree;
	const git_index_entry *entry;

	g_repo = cl_git_sandbox_init("testrepo2");
	git_repository_head_tree(&tree, g_repo);

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "nonexistent/"));

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	cl_git_pass(git_iterator_for_tree(&i, tree, &i_opts));
	cl_assert_equal_i(GIT_ITEROVER, git_iterator_current(&entry, i));
	git_iterator_free(i);

	git_tree_free(tree);
	git_vector_free(&filelist);
}

