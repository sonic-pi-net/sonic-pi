#include "clar_libgit2.h"
#include "iterator.h"
#include "repository.h"
#include "fileops.h"
#include <stdarg.h>

static git_repository *g_repo;

void test_repo_iterator__initialize(void)
{
}

void test_repo_iterator__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

static void expect_iterator_items(
	git_iterator *i,
	int expected_flat,
	const char **expected_flat_paths,
	int expected_total,
	const char **expected_total_paths)
{
	const git_index_entry *entry;
	int count, error;
	int no_trees = !(git_iterator_flags(i) & GIT_ITERATOR_INCLUDE_TREES);
	bool v = false;

	if (expected_flat < 0) { v = true; expected_flat = -expected_flat; }
	if (expected_total < 0) { v = true; expected_total = -expected_total; }

	if (v) fprintf(stderr, "== %s ==\n", no_trees ? "notrees" : "trees");

	count = 0;

	while (!git_iterator_advance(&entry, i)) {
		if (v) fprintf(stderr, "  %s %07o\n", entry->path, (int)entry->mode);

		if (no_trees)
			cl_assert(entry->mode != GIT_FILEMODE_TREE);

		if (expected_flat_paths) {
			const char *expect_path = expected_flat_paths[count];
			size_t expect_len = strlen(expect_path);

			cl_assert_equal_s(expect_path, entry->path);

			if (expect_path[expect_len - 1] == '/')
				cl_assert_equal_i(GIT_FILEMODE_TREE, entry->mode);
			else
				cl_assert(entry->mode != GIT_FILEMODE_TREE);
		}

		if (++count > expected_flat)
			break;
	}

	cl_assert_equal_i(expected_flat, count);

	cl_git_pass(git_iterator_reset(i, NULL, NULL));

	count = 0;
	cl_git_pass(git_iterator_current(&entry, i));

	if (v) fprintf(stderr, "-- %s --\n", no_trees ? "notrees" : "trees");

	while (entry != NULL) {
		if (v) fprintf(stderr, "  %s %07o\n", entry->path, (int)entry->mode);

		if (no_trees)
			cl_assert(entry->mode != GIT_FILEMODE_TREE);

		if (expected_total_paths) {
			const char *expect_path = expected_total_paths[count];
			size_t expect_len = strlen(expect_path);

			cl_assert_equal_s(expect_path, entry->path);

			if (expect_path[expect_len - 1] == '/')
				cl_assert_equal_i(GIT_FILEMODE_TREE, entry->mode);
			else
				cl_assert(entry->mode != GIT_FILEMODE_TREE);
		}

		if (entry->mode == GIT_FILEMODE_TREE) {
			error = git_iterator_advance_into(&entry, i);

			/* could return NOTFOUND if directory is empty */
			cl_assert(!error || error == GIT_ENOTFOUND);

			if (error == GIT_ENOTFOUND) {
				error = git_iterator_advance(&entry, i);
				cl_assert(!error || error == GIT_ITEROVER);
			}
		} else {
			error = git_iterator_advance(&entry, i);
			cl_assert(!error || error == GIT_ITEROVER);
		}

		if (++count > expected_total)
			break;
	}

	cl_assert_equal_i(expected_total, count);
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

void test_repo_iterator__index(void)
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

void test_repo_iterator__index_icase(void)
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

void test_repo_iterator__tree(void)
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

void test_repo_iterator__tree_icase(void)
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

void test_repo_iterator__tree_more(void)
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
	git_buf name = GIT_BUF_INIT;
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
		cl_git_pass(git_buf_set(&name, scan, (size_t)(next - scan)));
		for (scan = next; *scan && (*scan == delimiter || *scan == ','); ++scan)
			/* skip delimiter and optional comma */;

		id = va_arg(arglist, git_oid *);

		cl_git_pass(git_treebuilder_insert(NULL, builder, name.ptr, id, mode));
	}
	va_end(arglist);

	cl_git_pass(git_treebuilder_write(out, builder));

	git_treebuilder_free(builder);
	git_buf_free(&name);
}

void test_repo_iterator__tree_case_conflicts_0(void)
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

	cl_git_pass(git_oid_fromstr(&blob_id, blob_sha)); /* lookup blob */

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

void test_repo_iterator__tree_case_conflicts_1(void)
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

	cl_git_pass(git_oid_fromstr(&blob_id, blob_sha)); /* lookup blob */

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

void test_repo_iterator__tree_case_conflicts_2(void)
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

	cl_git_pass(git_oid_fromstr(&blob_id, blob_sha)); /* lookup blob */

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

void test_repo_iterator__workdir(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;

	g_repo = cl_git_sandbox_init("icase");

	/* auto expand with no tree entries */
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 20, NULL, 20, NULL);
	git_iterator_free(i);

	/* auto expand with tree entries */
	i_opts.flags = GIT_ITERATOR_INCLUDE_TREES;
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 22, NULL, 22, NULL);
	git_iterator_free(i);

	/* no auto expand (implies trees included) */
	i_opts.flags = GIT_ITERATOR_DONT_AUTOEXPAND;
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 12, NULL, 22, NULL);
	git_iterator_free(i);
}

void test_repo_iterator__workdir_icase(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;

	g_repo = cl_git_sandbox_init("icase");

	/* auto expand with no tree entries */
	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 7, NULL, 7, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 3, NULL, 3, NULL);
	git_iterator_free(i);

	/* auto expand with tree entries */
	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 8, NULL, 8, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 4, NULL, 4, NULL);
	git_iterator_free(i);

	/* no auto expand (implies trees included) */
	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE | GIT_ITERATOR_DONT_AUTOEXPAND;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 5, NULL, 8, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 1, NULL, 4, NULL);
	git_iterator_free(i);

	/* auto expand with no tree entries */
	i_opts.flags = GIT_ITERATOR_IGNORE_CASE;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 13, NULL, 13, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 5, NULL, 5, NULL);
	git_iterator_free(i);

	/* auto expand with tree entries */
	i_opts.flags = GIT_ITERATOR_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 14, NULL, 14, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 6, NULL, 6, NULL);
	git_iterator_free(i);

	/* no auto expand (implies trees included) */
	i_opts.flags = GIT_ITERATOR_IGNORE_CASE | GIT_ITERATOR_DONT_AUTOEXPAND;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 9, NULL, 14, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 1, NULL, 6, NULL);
	git_iterator_free(i);
}

static void build_workdir_tree(const char *root, int dirs, int subs)
{
	int i, j;
	char buf[64], sub[64];

	for (i = 0; i < dirs; ++i) {
		if (i % 2 == 0) {
			p_snprintf(buf, sizeof(buf), "%s/dir%02d", root, i);
			cl_git_pass(git_futils_mkdir(buf, 0775, GIT_MKDIR_PATH));

			p_snprintf(buf, sizeof(buf), "%s/dir%02d/file", root, i);
			cl_git_mkfile(buf, buf);
			buf[strlen(buf) - 5] = '\0';
		} else {
			p_snprintf(buf, sizeof(buf), "%s/DIR%02d", root, i);
			cl_git_pass(git_futils_mkdir(buf, 0775, GIT_MKDIR_PATH));
		}

		for (j = 0; j < subs; ++j) {
			switch (j % 4) {
			case 0: p_snprintf(sub, sizeof(sub), "%s/sub%02d", buf, j); break;
			case 1: p_snprintf(sub, sizeof(sub), "%s/sUB%02d", buf, j); break;
			case 2: p_snprintf(sub, sizeof(sub), "%s/Sub%02d", buf, j); break;
			case 3: p_snprintf(sub, sizeof(sub), "%s/SUB%02d", buf, j); break;
			}
			cl_git_pass(git_futils_mkdir(sub, 0775, GIT_MKDIR_PATH));

			if (j % 2 == 0) {
				size_t sublen = strlen(sub);
				memcpy(&sub[sublen], "/file", sizeof("/file"));
				cl_git_mkfile(sub, sub);
				sub[sublen] = '\0';
			}
		}
	}
}

void test_repo_iterator__workdir_depth(void)
{
	git_iterator *iter;
	git_iterator_options iter_opts = GIT_ITERATOR_OPTIONS_INIT;

	g_repo = cl_git_sandbox_init("icase");

	build_workdir_tree("icase", 10, 10);
	build_workdir_tree("icase/DIR01/sUB01", 50, 0);
	build_workdir_tree("icase/dir02/sUB01", 50, 0);

	/* auto expand with no tree entries */
	cl_git_pass(git_iterator_for_workdir(&iter, g_repo, NULL, NULL, &iter_opts));
	expect_iterator_items(iter, 125, NULL, 125, NULL);
	git_iterator_free(iter);

	/* auto expand with tree entries (empty dirs silently skipped) */
	iter_opts.flags = GIT_ITERATOR_INCLUDE_TREES;
	cl_git_pass(git_iterator_for_workdir(&iter, g_repo, NULL, NULL, &iter_opts));
	expect_iterator_items(iter, 337, NULL, 337, NULL);
	git_iterator_free(iter);
}

void test_repo_iterator__fs(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;

	static const char *expect_base[] = {
		"DIR01/Sub02/file",
		"DIR01/sub00/file",
		"current_file",
		"dir00/Sub02/file",
		"dir00/file",
		"dir00/sub00/file",
		"modified_file",
		"new_file",
		NULL,
	};
	static const char *expect_trees[] = {
		"DIR01/",
		"DIR01/SUB03/",
		"DIR01/Sub02/",
		"DIR01/Sub02/file",
		"DIR01/sUB01/",
		"DIR01/sub00/",
		"DIR01/sub00/file",
		"current_file",
		"dir00/",
		"dir00/SUB03/",
		"dir00/Sub02/",
		"dir00/Sub02/file",
		"dir00/file",
		"dir00/sUB01/",
		"dir00/sub00/",
		"dir00/sub00/file",
		"modified_file",
		"new_file",
		NULL,
	};
	static const char *expect_noauto[] = {
		"DIR01/",
		"current_file",
		"dir00/",
		"modified_file",
		"new_file",
		NULL,
	};

	g_repo = cl_git_sandbox_init("status");

	build_workdir_tree("status/subdir", 2, 4);

	cl_git_pass(git_iterator_for_filesystem(&i, "status/subdir", NULL));
	expect_iterator_items(i, 8, expect_base, 8, expect_base);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_INCLUDE_TREES;
	cl_git_pass(git_iterator_for_filesystem(&i, "status/subdir", &i_opts));
	expect_iterator_items(i, 18, expect_trees, 18, expect_trees);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_DONT_AUTOEXPAND;
	cl_git_pass(git_iterator_for_filesystem(&i, "status/subdir", &i_opts));
	expect_iterator_items(i, 5, expect_noauto, 18, expect_trees);
	git_iterator_free(i);

	git__tsort((void **)expect_base, 8, (git__tsort_cmp)git__strcasecmp);
	git__tsort((void **)expect_trees, 18, (git__tsort_cmp)git__strcasecmp);
	git__tsort((void **)expect_noauto, 5, (git__tsort_cmp)git__strcasecmp);

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE;
	cl_git_pass(git_iterator_for_filesystem(&i, "status/subdir", &i_opts));
	expect_iterator_items(i, 8, expect_base, 8, expect_base);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;
	cl_git_pass(git_iterator_for_filesystem(&i, "status/subdir", &i_opts));
	expect_iterator_items(i, 18, expect_trees, 18, expect_trees);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE | GIT_ITERATOR_DONT_AUTOEXPAND;
	cl_git_pass(git_iterator_for_filesystem(&i, "status/subdir", &i_opts));
	expect_iterator_items(i, 5, expect_noauto, 18, expect_trees);
	git_iterator_free(i);
}

void test_repo_iterator__fs2(void)
{
	git_iterator *i;
	static const char *expect_base[] = {
		"heads/br2",
		"heads/dir",
		"heads/ident",
		"heads/long-file-name",
		"heads/master",
		"heads/packed-test",
		"heads/subtrees",
		"heads/test",
		"tags/e90810b",
		"tags/foo/bar",
		"tags/foo/foo/bar",
		"tags/point_to_blob",
		"tags/test",
		NULL,
	};

	g_repo = cl_git_sandbox_init("testrepo");

	cl_git_pass(git_iterator_for_filesystem(
		&i, "testrepo/.git/refs", NULL));
	expect_iterator_items(i, 13, expect_base, 13, expect_base);
	git_iterator_free(i);
}

void test_repo_iterator__unreadable_dir(void)
{
	git_iterator *i;
	const git_index_entry *e;

	if (!cl_is_chmod_supported())
		return;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_must_pass(p_mkdir("empty_standard_repo/r", 0777));
	cl_git_mkfile("empty_standard_repo/r/a", "hello");
	cl_must_pass(p_mkdir("empty_standard_repo/r/b", 0777));
	cl_git_mkfile("empty_standard_repo/r/b/problem", "not me");
	cl_must_pass(p_chmod("empty_standard_repo/r/b", 0000));
	cl_must_pass(p_mkdir("empty_standard_repo/r/c", 0777));
	cl_git_mkfile("empty_standard_repo/r/d", "final");

	cl_git_pass(git_iterator_for_filesystem(
		&i, "empty_standard_repo/r", NULL));

	cl_git_pass(git_iterator_advance(&e, i)); /* a */
	cl_git_fail(git_iterator_advance(&e, i)); /* b */
	cl_assert_equal_i(GIT_ITEROVER, git_iterator_advance(&e, i));

	cl_must_pass(p_chmod("empty_standard_repo/r/b", 0777));

	git_iterator_free(i);
}

void test_repo_iterator__skips_fifos_and_such(void)
{
#ifndef GIT_WIN32
	git_iterator *i;
	const git_index_entry *e;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_must_pass(p_mkdir("empty_standard_repo/dir", 0777));
	cl_git_mkfile("empty_standard_repo/file", "not me");

	cl_assert(!mkfifo("empty_standard_repo/fifo", 0777));
	cl_assert(!access("empty_standard_repo/fifo", F_OK));

	i_opts.flags = GIT_ITERATOR_INCLUDE_TREES |
		GIT_ITERATOR_DONT_AUTOEXPAND;

	cl_git_pass(git_iterator_for_filesystem(
		&i, "empty_standard_repo", &i_opts));

	cl_git_pass(git_iterator_advance(&e, i)); /* .git */
	cl_assert(S_ISDIR(e->mode));
	cl_git_pass(git_iterator_advance(&e, i)); /* dir */
	cl_assert(S_ISDIR(e->mode));
	/* skips fifo */
	cl_git_pass(git_iterator_advance(&e, i)); /* file */
	cl_assert(S_ISREG(e->mode));

	cl_assert_equal_i(GIT_ITEROVER, git_iterator_advance(&e, i));

	git_iterator_free(i);
#endif
}

void test_repo_iterator__indexfilelist(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_index *index;
	git_vector filelist;
	int default_icase;
	int expect;

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

	/* In this test we DO NOT force a case setting on the index. */
	default_icase = ((git_index_caps(index) & GIT_INDEXCAP_IGNORE_CASE) != 0);

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	/* All indexfilelist iterator tests are "autoexpand with no tree entries" */

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	expect_iterator_items(i, 8, NULL, 8, NULL);
	git_iterator_free(i);

	i_opts.start = "c";
	i_opts.end = NULL;

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	/* (c D e k/1 k/a L ==> 6) vs (c e k/1 k/a ==> 4) */
	expect = ((default_icase) ? 6 : 4);
	expect_iterator_items(i, expect, NULL, expect, NULL);
	git_iterator_free(i);

	i_opts.start = NULL;
	i_opts.end = "e";

	cl_git_pass(git_iterator_for_index(&i, g_repo, index, &i_opts));
	/* (a B c D e ==> 5) vs (B D L/1 a c e ==> 6) */
	expect = ((default_icase) ? 5 : 6);
	expect_iterator_items(i, expect, NULL, expect, NULL);
	git_iterator_free(i);

	git_index_free(index);
	git_vector_free(&filelist);
}

void test_repo_iterator__indexfilelist_2(void)
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

void test_repo_iterator__indexfilelist_3(void)
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

void test_repo_iterator__indexfilelist_4(void)
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

void test_repo_iterator__indexfilelist_icase(void)
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

void test_repo_iterator__workdirfilelist(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;
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

	/* All indexfilelist iterator tests are "autoexpand with no tree entries" */
	/* In this test we DO NOT force a case on the iteratords and verify default behavior. */

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 8, NULL, 8, NULL);
	git_iterator_free(i);

	i_opts.start = "c";
	i_opts.end = NULL;
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	default_icase = git_iterator_ignore_case(i);
	/* (c D e k/1 k/a L ==> 6) vs (c e k/1 k/a ==> 4) */
	expect = ((default_icase) ? 6 : 4);
	expect_iterator_items(i, expect, NULL, expect, NULL);
	git_iterator_free(i);

	i_opts.start = NULL;
	i_opts.end = "e";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	default_icase = git_iterator_ignore_case(i);
	/* (a B c D e ==> 5) vs (B D L/1 a c e ==> 6) */
	expect = ((default_icase) ? 5 : 6);
	expect_iterator_items(i, expect, NULL, expect, NULL);
	git_iterator_free(i);

	git_vector_free(&filelist);
}

void test_repo_iterator__workdirfilelist_icase(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;

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

	i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;
	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 3, NULL, 3, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 1, NULL, 1, NULL);
	git_iterator_free(i);

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE;

	i_opts.start = "c";
	i_opts.end = "k/D";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 5, NULL, 5, NULL);
	git_iterator_free(i);

	i_opts.start = "k";
	i_opts.end = "k/Z";
	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, 2, NULL, 2, NULL);
	git_iterator_free(i);

	git_vector_free(&filelist);
}

void test_repo_iterator__treefilelist(void)
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
	/* In this test we DO NOT force a case on the iteratords and verify default behavior. */

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

void test_repo_iterator__treefilelist_icase(void)
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
