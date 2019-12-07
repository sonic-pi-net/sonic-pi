#include "clar_libgit2.h"
#include "iterator.h"
#include "repository.h"
#include "fileops.h"
#include "../submodule/submodule_helpers.h"
#include "iterator_helpers.h"
#include <stdarg.h>

static git_repository *g_repo;

void test_iterator_workdir__initialize(void)
{
}

void test_iterator_workdir__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

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
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	const git_index_entry *entry;
	int error, count = 0, count_all = 0, count_all_post_reset = 0;

	g_repo = cl_git_sandbox_init(sandbox);

	i_opts.flags = GIT_ITERATOR_DONT_AUTOEXPAND;
	i_opts.start = start;
	i_opts.end = end;

	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));

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

	cl_git_pass(git_iterator_reset(i));

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

void test_iterator_workdir__0(void)
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

void test_iterator_workdir__1(void)
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

void test_iterator_workdir__1_ranged_0(void)
{
	workdir_iterator_test(
		"status", "staged", "staged", 5, 0, status_paths_range_0, NULL);
}

static const char *status_paths_range_1[] = {
	"modified_file", NULL
};

void test_iterator_workdir__1_ranged_1(void)
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

void test_iterator_workdir__1_ranged_3(void)
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

void test_iterator_workdir__1_ranged_4(void)
{
	workdir_iterator_test(
		"status", "subdir/", NULL, 4, 0, status_paths_range_4, NULL);
}

static const char *status_paths_range_5[] = {
	"subdir/modified_file",
	NULL
};

void test_iterator_workdir__1_ranged_5(void)
{
	workdir_iterator_test(
		"status", "subdir/modified_file", "subdir/modified_file",
		1, 0, status_paths_range_5, NULL);
}

void test_iterator_workdir__1_ranged_5_1_ranged_empty_0(void)
{
	workdir_iterator_test(
		"status", "\xff_does_not_exist", NULL,
		0, 0, NULL, NULL);
}

void test_iterator_workdir__1_ranged_empty_1(void)
{
	workdir_iterator_test(
		"status", "empty", "empty",
		0, 0, NULL, NULL);
}

void test_iterator_workdir__1_ranged_empty_2(void)
{
	workdir_iterator_test(
		"status", NULL, "aaaa_empty_before",
		0, 0, NULL, NULL);
}

void test_iterator_workdir__builtin_ignores(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
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

	g_repo = cl_git_sandbox_init("attr");

	cl_git_pass(p_mkdir("attr/sub/sub/.git", 0777));
	cl_git_mkfile("attr/sub/.git", "whatever");

	i_opts.flags = GIT_ITERATOR_DONT_AUTOEXPAND;
	i_opts.start = "dir";
	i_opts.end = "sub/sub/file";

	cl_git_pass(git_iterator_for_workdir(
		&i, g_repo, NULL, NULL, &i_opts));
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
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	const git_index_entry *entry;
	int error, idx;
	static const char *expected[] = { "FIRST", "second", "THIRD", NULL };

	i_opts.flags = GIT_ITERATOR_IGNORE_CASE;
	i_opts.start = start;
	i_opts.end = end;

	cl_git_pass(git_iterator_for_workdir(
		&i, repo, NULL, NULL, &i_opts));
	cl_git_pass(git_iterator_current(&entry, i));

	for (idx = 0; entry != NULL; ++idx) {
		cl_assert_equal_s(expected[idx], entry->path);

		error = git_iterator_advance(&entry, i);
		cl_assert(!error || error == GIT_ITEROVER);
	}

	cl_assert(expected[idx] == NULL);

	git_iterator_free(i);
}

void test_iterator_workdir__handles_icase_range(void)
{
	g_repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_remove_placeholders(git_repository_path(g_repo), "dummy-marker.txt");

	cl_git_mkfile("empty_standard_repo/before", "whatever\n");
	cl_git_mkfile("empty_standard_repo/FIRST", "whatever\n");
	cl_git_mkfile("empty_standard_repo/second", "whatever\n");
	cl_git_mkfile("empty_standard_repo/THIRD", "whatever\n");
	cl_git_mkfile("empty_standard_repo/zafter", "whatever\n");
	cl_git_mkfile("empty_standard_repo/Zlast", "whatever\n");

	check_wd_first_through_third_range(g_repo, "first", "third");
	check_wd_first_through_third_range(g_repo, "FIRST", "THIRD");
	check_wd_first_through_third_range(g_repo, "first", "THIRD");
	check_wd_first_through_third_range(g_repo, "FIRST", "third");
	check_wd_first_through_third_range(g_repo, "FirSt", "tHiRd");
}

/*
 * The workdir iterator is like the filesystem iterator, but honors
 * special git type constructs (ignores, submodules, etc).
 */

void test_iterator_workdir__icase(void)
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

void test_iterator_workdir__icase_starts_and_ends(void)
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

void test_iterator_workdir__depth(void)
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

/* The filesystem iterator is a workdir iterator without any special
 * workdir handling capabilities (ignores, submodules, etc).
 */
void test_iterator_workdir__filesystem(void)
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

void test_iterator_workdir__filesystem2(void)
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

/* Lots of empty dirs, or nearly empty ones, make the old workdir
 * iterator cry.  Also, segfault.
 */
void test_iterator_workdir__filesystem_gunk(void)
{
	git_iterator *i;
	git_buf parent = GIT_BUF_INIT;
	int n;

	if (!cl_is_env_set("GITTEST_INVASIVE_SPEED"))
		cl_skip();

	g_repo = cl_git_sandbox_init("testrepo");

	for (n = 0; n < 100000; n++) {
		git_buf_clear(&parent);
		git_buf_printf(&parent, "%s/refs/heads/foo/%d/subdir",
			git_repository_path(g_repo), n);
		cl_assert(!git_buf_oom(&parent));

		cl_git_pass(git_futils_mkdir(parent.ptr, 0775, GIT_MKDIR_PATH));
	}

	cl_git_pass(git_iterator_for_filesystem(&i, "testrepo/.git/refs", NULL));

	/* should only have 13 items, since we're not asking for trees to be
	 * returned.  the goal of this test is simply to not crash.
	 */
	expect_iterator_items(i, 13, NULL, 13, NULL);
	git_iterator_free(i);
	git_buf_free(&parent);
}

void test_iterator_workdir__skips_unreadable_dirs(void)
{
	git_iterator *i;
	const git_index_entry *e;

	if (!cl_is_chmod_supported())
		return;

#ifndef GIT_WIN32
	if (geteuid() == 0)
		cl_skip();
#endif

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_must_pass(p_mkdir("empty_standard_repo/r", 0777));
	cl_git_mkfile("empty_standard_repo/r/a", "hello");
	cl_must_pass(p_mkdir("empty_standard_repo/r/b", 0777));
	cl_git_mkfile("empty_standard_repo/r/b/problem", "not me");
	cl_must_pass(p_chmod("empty_standard_repo/r/b", 0000));
	cl_must_pass(p_mkdir("empty_standard_repo/r/c", 0777));
	cl_git_mkfile("empty_standard_repo/r/c/foo", "aloha");
	cl_git_mkfile("empty_standard_repo/r/d", "final");

	cl_git_pass(git_iterator_for_filesystem(
		&i, "empty_standard_repo/r", NULL));

	cl_git_pass(git_iterator_advance(&e, i)); /* a */
	cl_assert_equal_s("a", e->path);

	cl_git_pass(git_iterator_advance(&e, i)); /* c/foo */
	cl_assert_equal_s("c/foo", e->path);

	cl_git_pass(git_iterator_advance(&e, i)); /* d */
	cl_assert_equal_s("d", e->path);

	cl_must_pass(p_chmod("empty_standard_repo/r/b", 0777));

	cl_assert_equal_i(GIT_ITEROVER, git_iterator_advance(&e, i));
	git_iterator_free(i);
}

void test_iterator_workdir__skips_fifos_and_special_files(void)
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

void test_iterator_workdir__pathlist(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;

	cl_git_pass(git_vector_init(&filelist, 100, NULL));
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

	/* Test iterators without returning tree entries (but autoexpanding.) */

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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	git_vector_free(&filelist);
}

void test_iterator_workdir__pathlist_with_dirs(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;

	cl_git_pass(git_vector_init(&filelist, 5, NULL));

	g_repo = cl_git_sandbox_init("icase");

	/* Test that a prefix `k` matches folders, even without trailing slash */
	{
		const char *expected[] = { "k/1", "k/B", "k/D", "k/a", "k/c" };
		size_t expected_len = 5;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "k"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	git_vector_free(&filelist);
}

static void create_paths(const char *root, int depth)
{
	git_buf fullpath = GIT_BUF_INIT;
	size_t root_len;
	int i;

	cl_git_pass(git_buf_puts(&fullpath, root));
	cl_git_pass(git_buf_putc(&fullpath, '/'));

	root_len = fullpath.size;

	for (i = 0; i < 8; i++) {
		bool file = (depth == 0 || (i % 2) == 0);
		git_buf_truncate(&fullpath, root_len);
		cl_git_pass(git_buf_printf(&fullpath, "item%d", i));

		if (file) {
			cl_git_rewritefile(fullpath.ptr, "This is a file!\n");
		} else {
			cl_must_pass(p_mkdir(fullpath.ptr, 0777));

			if (depth > 0)
				create_paths(fullpath.ptr, (depth - 1));
		}
	}

	git_buf_free(&fullpath);
}

void test_iterator_workdir__pathlist_for_deeply_nested_item(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;

	cl_git_pass(git_vector_init(&filelist, 5, NULL));

	g_repo = cl_git_sandbox_init("icase");
	create_paths(git_repository_workdir(g_repo), 3);

	/* Ensure that we find the single path we're interested in, and we find
	 * it efficiently, and don't stat the entire world to get there.
	 */
	{
		const char *expected[] = { "item1/item3/item5/item7" };
		size_t expected_len = 1;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "item1/item3/item5/item7"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		cl_assert_equal_i(4, i->stat_calls);
		git_iterator_free(i);
	}

	/* Ensure that we find the single path we're interested in, and we find
	 * it efficiently, and don't stat the entire world to get there.
	 */
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		cl_assert_equal_i(11, i->stat_calls);
		git_iterator_free(i);
	}

	/* Ensure that we find the single path we're interested in, and we find
	 * it efficiently, and don't stat the entire world to get there.
	 */
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		cl_assert_equal_i(42, i->stat_calls);
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

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		cl_assert_equal_i(14, i->stat_calls);
		git_iterator_free(i);
	}

	git_vector_free(&filelist);
}

void test_iterator_workdir__bounded_submodules(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;
	git_index *index;
	git_tree *head;

	cl_git_pass(git_vector_init(&filelist, 5, NULL));

	g_repo = setup_fixture_submod2();
	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_repository_head_tree(&head, g_repo));

	/* Test that a submodule matches */
	{
		const char *expected[] = { "sm_changed_head" };
		size_t expected_len = 1;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "sm_changed_head"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, index, head, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Test that a submodule still matches when suffixed with a '/' */
	{
		const char *expected[] = { "sm_changed_head" };
		size_t expected_len = 1;

		git_vector_clear(&filelist);
		cl_git_pass(git_vector_insert(&filelist, "sm_changed_head/"));

		i_opts.pathlist.strings = (char **)filelist.contents;
		i_opts.pathlist.count = filelist.length;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, index, head, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Test that start/end work with a submodule */
	{
		const char *expected[] = { "sm_changed_head", "sm_changed_index" };
		size_t expected_len = 2;

		i_opts.start = "sm_changed_head";
		i_opts.end = "sm_changed_index";
		i_opts.pathlist.strings = NULL;
		i_opts.pathlist.count = 0;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, index, head, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	/* Test that start and end allow '/' suffixes of submodules */
	{
		const char *expected[] = { "sm_changed_head", "sm_changed_index" };
		size_t expected_len = 2;

		i_opts.start = "sm_changed_head";
		i_opts.end = "sm_changed_index";
		i_opts.pathlist.strings = NULL;
		i_opts.pathlist.count = 0;
		i_opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE;

		cl_git_pass(git_iterator_for_workdir(&i, g_repo, index, head, &i_opts));
		expect_iterator_items(i, expected_len, expected, expected_len, expected);
		git_iterator_free(i);
	}

	git_vector_free(&filelist);
	git_index_free(index);
	git_tree_free(head);
}

void test_iterator_workdir__advance_over(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;

	i_opts.flags |= GIT_ITERATOR_DONT_IGNORE_CASE |
		GIT_ITERATOR_DONT_AUTOEXPAND;

	g_repo = cl_git_sandbox_init("icase");

	/* create an empty directory */
	cl_must_pass(p_mkdir("icase/empty", 0777));

	/* create a directory in which all contents are ignored */
	cl_must_pass(p_mkdir("icase/all_ignored", 0777));
	cl_git_rewritefile("icase/all_ignored/one", "This is ignored\n");
	cl_git_rewritefile("icase/all_ignored/two", "This, too, is ignored\n");
	cl_git_rewritefile("icase/all_ignored/.gitignore", ".gitignore\none\ntwo\n");

	/* create a directory in which not all contents are ignored */
	cl_must_pass(p_mkdir("icase/some_ignored", 0777));
	cl_git_rewritefile("icase/some_ignored/one", "This is ignored\n");
	cl_git_rewritefile("icase/some_ignored/two", "This is not ignored\n");
	cl_git_rewritefile("icase/some_ignored/.gitignore", ".gitignore\none\n");

	/* create a directory which has some empty children */
	cl_must_pass(p_mkdir("icase/empty_children", 0777));
	cl_must_pass(p_mkdir("icase/empty_children/empty1", 0777));
	cl_must_pass(p_mkdir("icase/empty_children/empty2", 0777));
	cl_must_pass(p_mkdir("icase/empty_children/empty3", 0777));

	/* create a directory which will disappear! */
	cl_must_pass(p_mkdir("icase/missing_directory", 0777));

	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));

	cl_must_pass(p_rmdir("icase/missing_directory"));

	expect_advance_over(i, "B", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "D", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "F", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "H", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "J", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "L/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "a", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "all_ignored/", GIT_ITERATOR_STATUS_IGNORED);
	expect_advance_over(i, "c", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "e", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "empty/", GIT_ITERATOR_STATUS_EMPTY);
	expect_advance_over(i, "empty_children/", GIT_ITERATOR_STATUS_EMPTY);
	expect_advance_over(i, "g", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "i", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "k/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "missing_directory/", GIT_ITERATOR_STATUS_EMPTY);
	expect_advance_over(i, "some_ignored/", GIT_ITERATOR_STATUS_NORMAL);

	cl_git_fail_with(GIT_ITEROVER, git_iterator_advance(NULL, i));
	git_iterator_free(i);
}

void test_iterator_workdir__advance_over_with_pathlist(void)
{
	git_vector pathlist = GIT_VECTOR_INIT;
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;

	git_vector_insert(&pathlist, "dirA/subdir1/subdir2/file");
	git_vector_insert(&pathlist, "dirB/subdir1/subdir2");
	git_vector_insert(&pathlist, "dirC/subdir1/nonexistent");
	git_vector_insert(&pathlist, "dirD/subdir1/nonexistent");
	git_vector_insert(&pathlist, "dirD/subdir1/subdir2");
	git_vector_insert(&pathlist, "dirD/nonexistent");

	i_opts.pathlist.strings = (char **)pathlist.contents;
	i_opts.pathlist.count = pathlist.length;
	i_opts.flags |= GIT_ITERATOR_DONT_IGNORE_CASE |
		GIT_ITERATOR_DONT_AUTOEXPAND;

	g_repo = cl_git_sandbox_init("icase");

	/* Create a directory that has a file that is included in our pathlist */
	cl_must_pass(p_mkdir("icase/dirA", 0777));
	cl_must_pass(p_mkdir("icase/dirA/subdir1", 0777));
	cl_must_pass(p_mkdir("icase/dirA/subdir1/subdir2", 0777));
	cl_git_rewritefile("icase/dirA/subdir1/subdir2/file", "foo!");

	/* Create a directory that has a directory that is included in our pathlist */
	cl_must_pass(p_mkdir("icase/dirB", 0777));
	cl_must_pass(p_mkdir("icase/dirB/subdir1", 0777));
	cl_must_pass(p_mkdir("icase/dirB/subdir1/subdir2", 0777));
	cl_git_rewritefile("icase/dirB/subdir1/subdir2/file", "foo!");

	/* Create a directory that would contain an entry in our pathlist, but
	 * that entry does not actually exist.  We don't know this until we
	 * advance_over it.  We want to distinguish this from an actually empty
	 * or ignored directory.
	 */
	cl_must_pass(p_mkdir("icase/dirC", 0777));
	cl_must_pass(p_mkdir("icase/dirC/subdir1", 0777));
	cl_must_pass(p_mkdir("icase/dirC/subdir1/subdir2", 0777));
	cl_git_rewritefile("icase/dirC/subdir1/subdir2/file", "foo!");

	/* Create a directory that has a mix of actual and nonexistent paths */
	cl_must_pass(p_mkdir("icase/dirD", 0777));
	cl_must_pass(p_mkdir("icase/dirD/subdir1", 0777));
	cl_must_pass(p_mkdir("icase/dirD/subdir1/subdir2", 0777));
	cl_git_rewritefile("icase/dirD/subdir1/subdir2/file", "foo!");

	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));

	expect_advance_over(i, "dirA/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "dirB/", GIT_ITERATOR_STATUS_NORMAL);
	expect_advance_over(i, "dirC/", GIT_ITERATOR_STATUS_FILTERED);
	expect_advance_over(i, "dirD/", GIT_ITERATOR_STATUS_NORMAL);

	cl_git_fail_with(GIT_ITEROVER, git_iterator_advance(NULL, i));
	git_iterator_free(i);
	git_vector_free(&pathlist);
}

void test_iterator_workdir__advance_into(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;

	g_repo = cl_git_sandbox_init("icase");

	i_opts.flags |= GIT_ITERATOR_DONT_IGNORE_CASE |
		GIT_ITERATOR_DONT_AUTOEXPAND;

	cl_must_pass(p_mkdir("icase/Empty", 0777));

	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_advance_into(i, "B");
	expect_advance_into(i, "D");
	expect_advance_into(i, "Empty/");
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
}

void test_iterator_workdir__pathlist_with_directory(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;

	const char *expected[] = { "subdir/README", "subdir/new.txt",
	                           "subdir/subdir2/README", "subdir/subdir2/new.txt" };
	size_t expected_len = 4;

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "subdir/"));

	g_repo = cl_git_sandbox_init("testrepo2");

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;
	i_opts.flags |= GIT_ITERATOR_DONT_IGNORE_CASE;

	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, expected_len, expected, expected_len, expected);
	git_iterator_free(i);

	git_vector_free(&filelist);
}

void test_iterator_workdir__pathlist_with_directory_include_trees(void)
{
	git_iterator *i;
	git_iterator_options i_opts = GIT_ITERATOR_OPTIONS_INIT;
	git_vector filelist;

	const char *expected[] = { "subdir/", "subdir/README", "subdir/new.txt",
	                           "subdir/subdir2/", "subdir/subdir2/README", "subdir/subdir2/new.txt", };
	size_t expected_len = 6;

	cl_git_pass(git_vector_init(&filelist, 100, &git__strcmp_cb));
	cl_git_pass(git_vector_insert(&filelist, "subdir/"));

	g_repo = cl_git_sandbox_init("testrepo2");

	i_opts.pathlist.strings = (char **)filelist.contents;
	i_opts.pathlist.count = filelist.length;
	i_opts.flags |= GIT_ITERATOR_DONT_IGNORE_CASE | GIT_ITERATOR_INCLUDE_TREES;

	cl_git_pass(git_iterator_for_workdir(&i, g_repo, NULL, NULL, &i_opts));
	expect_iterator_items(i, expected_len, expected, expected_len, expected);
	git_iterator_free(i);

	git_vector_free(&filelist);
}

