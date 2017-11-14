#include "clar_libgit2.h"
#include "fileops.h"
#include "git2/attr.h"
#include "ignore.h"
#include "attr.h"
#include "status_helpers.h"

static git_repository *g_repo = NULL;

void test_status_ignore__initialize(void)
{
}

void test_status_ignore__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void assert_ignored_(
	bool expected, const char *filepath, const char *file, int line)
{
	int is_ignored = 0;
	cl_git_expect(
		git_status_should_ignore(&is_ignored, g_repo, filepath), 0, file, line);
	clar__assert(
		(expected != 0) == (is_ignored != 0),
		file, line, "expected != is_ignored", filepath, 1);
}
#define assert_ignored(expected, filepath) \
	assert_ignored_(expected, filepath, __FILE__, __LINE__)
#define assert_is_ignored(filepath) \
	assert_ignored_(true, filepath, __FILE__, __LINE__)
#define refute_is_ignored(filepath) \
	assert_ignored_(false, filepath, __FILE__, __LINE__)

void test_status_ignore__0(void)
{
	struct {
		const char *path;
		int expected;
	} test_cases[] = {
		/* pattern "ign" from .gitignore */
		{ "file", 0 },
		{ "ign", 1 },
		{ "sub", 0 },
		{ "sub/file", 0 },
		{ "sub/ign", 1 },
		{ "sub/ign/file", 1 },
		{ "sub/ign/sub", 1 },
		{ "sub/ign/sub/file", 1 },
		{ "sub/sub", 0 },
		{ "sub/sub/file", 0 },
		{ "sub/sub/ign", 1 },
		{ "sub/sub/sub", 0 },
		/* pattern "dir/" from .gitignore */
		{ "dir", 1 },
		{ "dir/", 1 },
		{ "sub/dir", 1 },
		{ "sub/dir/", 1 },
		{ "sub/dir/file", 1 }, /* contained in ignored parent */
		{ "sub/sub/dir", 0 }, /* dir is not actually a dir, but a file */
		{ NULL, 0 }
	}, *one_test;

	g_repo = cl_git_sandbox_init("attr");

	for (one_test = test_cases; one_test->path != NULL; one_test++)
		assert_ignored(one_test->expected, one_test->path);

	/* confirm that ignore files were cached */
	cl_assert(git_attr_cache__is_cached(
		g_repo, GIT_ATTR_FILE__FROM_FILE, ".git/info/exclude"));
	cl_assert(git_attr_cache__is_cached(
		g_repo, GIT_ATTR_FILE__FROM_FILE, ".gitignore"));
}


void test_status_ignore__1(void)
{
	g_repo = cl_git_sandbox_init("attr");

	cl_git_rewritefile("attr/.gitignore", "/*.txt\n/dir/\n");
	git_attr_cache_flush(g_repo);

	assert_is_ignored("root_test4.txt");
	refute_is_ignored("sub/subdir_test2.txt");
	assert_is_ignored("dir");
	assert_is_ignored("dir/");
	refute_is_ignored("sub/dir");
	refute_is_ignored("sub/dir/");
}

void test_status_ignore__empty_repo_with_gitignore_rewrite(void)
{
	status_entry_single st;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_mkfile(
		"empty_standard_repo/look-ma.txt", "I'm going to be ignored!");

	memset(&st, 0, sizeof(st));
	cl_git_pass(git_status_foreach(g_repo, cb_status__single, &st));
	cl_assert(st.count == 1);
	cl_assert(st.status == GIT_STATUS_WT_NEW);

	cl_git_pass(git_status_file(&st.status, g_repo, "look-ma.txt"));
	cl_assert(st.status == GIT_STATUS_WT_NEW);

	refute_is_ignored("look-ma.txt");

	cl_git_rewritefile("empty_standard_repo/.gitignore", "*.nomatch\n");

	memset(&st, 0, sizeof(st));
	cl_git_pass(git_status_foreach(g_repo, cb_status__single, &st));
	cl_assert(st.count == 2);
	cl_assert(st.status == GIT_STATUS_WT_NEW);

	cl_git_pass(git_status_file(&st.status, g_repo, "look-ma.txt"));
	cl_assert(st.status == GIT_STATUS_WT_NEW);

	refute_is_ignored("look-ma.txt");

	cl_git_rewritefile("empty_standard_repo/.gitignore", "*.txt\n");

	memset(&st, 0, sizeof(st));
	cl_git_pass(git_status_foreach(g_repo, cb_status__single, &st));
	cl_assert(st.count == 2);
	cl_assert(st.status == GIT_STATUS_IGNORED);

	cl_git_pass(git_status_file(&st.status, g_repo, "look-ma.txt"));
	cl_assert(st.status == GIT_STATUS_IGNORED);

	assert_is_ignored("look-ma.txt");
}

void test_status_ignore__ignore_pattern_contains_space(void)
{
	unsigned int flags;
	const mode_t mode = 0777;

	g_repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_rewritefile("empty_standard_repo/.gitignore", "foo bar.txt\n");

	cl_git_mkfile(
		"empty_standard_repo/foo bar.txt", "I'm going to be ignored!");

	cl_git_pass(git_status_file(&flags, g_repo, "foo bar.txt"));
	cl_assert(flags == GIT_STATUS_IGNORED);

	cl_git_pass(git_futils_mkdir_r("empty_standard_repo/foo", mode));
	cl_git_mkfile("empty_standard_repo/foo/look-ma.txt", "I'm not going to be ignored!");

	cl_git_pass(git_status_file(&flags, g_repo, "foo/look-ma.txt"));
	cl_assert(flags == GIT_STATUS_WT_NEW);
}

void test_status_ignore__ignore_pattern_ignorecase(void)
{
	unsigned int flags;
	bool ignore_case;
	git_index *index;

	g_repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_rewritefile("empty_standard_repo/.gitignore", "a.txt\n");

	cl_git_mkfile("empty_standard_repo/A.txt", "Differs in case");

	cl_git_pass(git_repository_index(&index, g_repo));
	ignore_case = (git_index_caps(index) & GIT_INDEXCAP_IGNORE_CASE) != 0;
	git_index_free(index);

	cl_git_pass(git_status_file(&flags, g_repo, "A.txt"));
	cl_assert(flags == ignore_case ? GIT_STATUS_IGNORED : GIT_STATUS_WT_NEW);
}

void test_status_ignore__subdirectories(void)
{
	status_entry_single st;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_mkfile(
		"empty_standard_repo/ignore_me", "I'm going to be ignored!");

	cl_git_rewritefile("empty_standard_repo/.gitignore", "ignore_me\n");

	memset(&st, 0, sizeof(st));
	cl_git_pass(git_status_foreach(g_repo, cb_status__single, &st));
	cl_assert_equal_i(2, st.count);
	cl_assert(st.status == GIT_STATUS_IGNORED);

	cl_git_pass(git_status_file(&st.status, g_repo, "ignore_me"));
	cl_assert(st.status == GIT_STATUS_IGNORED);

	assert_is_ignored("ignore_me");

	/* I've changed libgit2 so that the behavior here now differs from
	 * core git but seems to make more sense.  In core git, the following
	 * items are skipped completed, even if --ignored is passed to status.
	 * It you mirror these steps and run "git status -uall --ignored" then
	 * you will not see "test/ignore_me/" in the results.
	 *
	 * However, we had a couple reports of this as a bug, plus there is a
	 * similar circumstance where we were differing for core git when you
	 * used a rooted path for an ignore, so I changed this behavior.
	 */
	cl_git_pass(git_futils_mkdir_r(
		"empty_standard_repo/test/ignore_me", 0775));
	cl_git_mkfile(
		"empty_standard_repo/test/ignore_me/file", "I'm going to be ignored!");
	cl_git_mkfile(
		"empty_standard_repo/test/ignore_me/file2", "Me, too!");

	memset(&st, 0, sizeof(st));
	cl_git_pass(git_status_foreach(g_repo, cb_status__single, &st));
	cl_assert_equal_i(3, st.count);

	cl_git_pass(git_status_file(&st.status, g_repo, "test/ignore_me/file"));
	cl_assert(st.status == GIT_STATUS_IGNORED);

	assert_is_ignored("test/ignore_me/file");
}

static void make_test_data(const char *reponame, const char **files)
{
	const char **scan;
	size_t repolen = strlen(reponame) + 1;

	g_repo = cl_git_sandbox_init(reponame);

	for (scan = files; *scan != NULL; ++scan) {
		cl_git_pass(git_futils_mkdir_relative(
			*scan + repolen, reponame,
			0777, GIT_MKDIR_PATH | GIT_MKDIR_SKIP_LAST, NULL));
		cl_git_mkfile(*scan, "contents");
	}
}

static const char *test_repo_1 = "empty_standard_repo";
static const char *test_files_1[] = {
	"empty_standard_repo/dir/a/ignore_me",
	"empty_standard_repo/dir/b/ignore_me",
	"empty_standard_repo/dir/ignore_me",
	"empty_standard_repo/ignore_also/file",
	"empty_standard_repo/ignore_me",
	"empty_standard_repo/test/ignore_me/file",
	"empty_standard_repo/test/ignore_me/file2",
	"empty_standard_repo/test/ignore_me/and_me/file",
	NULL
};

void test_status_ignore__subdirectories_recursion(void)
{
	/* Let's try again with recursing into ignored dirs turned on */
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts;
	static const char *paths_r[] = {
		".gitignore",
		"dir/a/ignore_me",
		"dir/b/ignore_me",
		"dir/ignore_me",
		"ignore_also/file",
		"ignore_me",
		"test/ignore_me/and_me/file",
		"test/ignore_me/file",
		"test/ignore_me/file2",
	};
	static const unsigned int statuses_r[] = {
		GIT_STATUS_WT_NEW,  GIT_STATUS_IGNORED, GIT_STATUS_IGNORED,
		GIT_STATUS_IGNORED, GIT_STATUS_IGNORED, GIT_STATUS_IGNORED,
		GIT_STATUS_IGNORED, GIT_STATUS_IGNORED, GIT_STATUS_IGNORED,
	};
	static const char *paths_nr[] = {
		".gitignore",
		"dir/a/ignore_me",
		"dir/b/ignore_me",
		"dir/ignore_me",
		"ignore_also/",
		"ignore_me",
		"test/ignore_me/",
	};
	static const unsigned int statuses_nr[] = {
		GIT_STATUS_WT_NEW,
		GIT_STATUS_IGNORED, GIT_STATUS_IGNORED, GIT_STATUS_IGNORED,
		GIT_STATUS_IGNORED, GIT_STATUS_IGNORED, GIT_STATUS_IGNORED,
	};

	make_test_data(test_repo_1, test_files_1);
	cl_git_rewritefile("empty_standard_repo/.gitignore", "ignore_me\n/ignore_also\n");

	memset(&counts, 0x0, sizeof(status_entry_counts));
	counts.expected_entry_count = 9;
	counts.expected_paths = paths_r;
	counts.expected_statuses = statuses_r;

	opts.flags = GIT_STATUS_OPT_DEFAULTS | GIT_STATUS_OPT_RECURSE_IGNORED_DIRS;

	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__normal, &counts));

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);


	memset(&counts, 0x0, sizeof(status_entry_counts));
	counts.expected_entry_count = 7;
	counts.expected_paths = paths_nr;
	counts.expected_statuses = statuses_nr;

	opts.flags = GIT_STATUS_OPT_DEFAULTS;

	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__normal, &counts));

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

void test_status_ignore__subdirectories_not_at_root(void)
{
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts;
	static const char *paths_1[] = {
		"dir/.gitignore",
		"dir/a/ignore_me",
		"dir/b/ignore_me",
		"dir/ignore_me",
		"ignore_also/file",
		"ignore_me",
		"test/.gitignore",
		"test/ignore_me/and_me/file",
		"test/ignore_me/file",
		"test/ignore_me/file2",
	};
	static const unsigned int statuses_1[] = {
		GIT_STATUS_WT_NEW,  GIT_STATUS_IGNORED, GIT_STATUS_IGNORED,
		GIT_STATUS_IGNORED, GIT_STATUS_WT_NEW, GIT_STATUS_WT_NEW,
		GIT_STATUS_WT_NEW, GIT_STATUS_IGNORED, GIT_STATUS_WT_NEW, GIT_STATUS_WT_NEW,
	};

	make_test_data(test_repo_1, test_files_1);
	cl_git_rewritefile("empty_standard_repo/dir/.gitignore", "ignore_me\n/ignore_also\n");
	cl_git_rewritefile("empty_standard_repo/test/.gitignore", "and_me\n");

	memset(&counts, 0x0, sizeof(status_entry_counts));
	counts.expected_entry_count = 10;
	counts.expected_paths = paths_1;
	counts.expected_statuses = statuses_1;

	opts.flags = GIT_STATUS_OPT_DEFAULTS | GIT_STATUS_OPT_RECURSE_IGNORED_DIRS;

	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__normal, &counts));

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

void test_status_ignore__leading_slash_ignores(void)
{
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts;
	static const char *paths_2[] = {
		"dir/.gitignore",
		"dir/a/ignore_me",
		"dir/b/ignore_me",
		"dir/ignore_me",
		"ignore_also/file",
		"ignore_me",
		"test/.gitignore",
		"test/ignore_me/and_me/file",
		"test/ignore_me/file",
		"test/ignore_me/file2",
	};
	static const unsigned int statuses_2[] = {
		GIT_STATUS_WT_NEW,  GIT_STATUS_WT_NEW,  GIT_STATUS_WT_NEW,
		GIT_STATUS_IGNORED, GIT_STATUS_IGNORED, GIT_STATUS_IGNORED,
		GIT_STATUS_WT_NEW, GIT_STATUS_WT_NEW, GIT_STATUS_WT_NEW, GIT_STATUS_WT_NEW,
	};

	make_test_data(test_repo_1, test_files_1);

	cl_fake_home();
	cl_git_mkfile("home/.gitignore", "/ignore_me\n");
	{
		git_config *cfg;
		cl_git_pass(git_repository_config(&cfg, g_repo));
		cl_git_pass(git_config_set_string(
			cfg, "core.excludesfile", "~/.gitignore"));
		git_config_free(cfg);
	}

	cl_git_rewritefile("empty_standard_repo/.git/info/exclude", "/ignore_also\n");
	cl_git_rewritefile("empty_standard_repo/dir/.gitignore", "/ignore_me\n");
	cl_git_rewritefile("empty_standard_repo/test/.gitignore", "/and_me\n");

	memset(&counts, 0x0, sizeof(status_entry_counts));
	counts.expected_entry_count = 10;
	counts.expected_paths = paths_2;
	counts.expected_statuses = statuses_2;

	opts.flags = GIT_STATUS_OPT_DEFAULTS | GIT_STATUS_OPT_RECURSE_IGNORED_DIRS;

	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__normal, &counts));

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

void test_status_ignore__contained_dir_with_matching_name(void)
{
	static const char *test_files[] = {
		"empty_standard_repo/subdir_match/aaa/subdir_match/file",
		"empty_standard_repo/subdir_match/zzz_ignoreme",
		NULL
	};
	static const char *expected_paths[] = {
		"subdir_match/.gitignore",
		"subdir_match/aaa/subdir_match/file",
		"subdir_match/zzz_ignoreme",
	};
	static const unsigned int expected_statuses[] = {
		GIT_STATUS_WT_NEW,  GIT_STATUS_WT_NEW,  GIT_STATUS_IGNORED
	};
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts;

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/subdir_match/.gitignore", "*_ignoreme\n");

	refute_is_ignored("subdir_match/aaa/subdir_match/file");
	assert_is_ignored("subdir_match/zzz_ignoreme");

	memset(&counts, 0x0, sizeof(status_entry_counts));
	counts.expected_entry_count = 3;
	counts.expected_paths = expected_paths;
	counts.expected_statuses = expected_statuses;

	opts.flags = GIT_STATUS_OPT_DEFAULTS | GIT_STATUS_OPT_RECURSE_IGNORED_DIRS;

	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__normal, &counts));

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

void test_status_ignore__trailing_slash_star(void)
{
	static const char *test_files[] = {
		"empty_standard_repo/file",
		"empty_standard_repo/subdir/file",
		"empty_standard_repo/subdir/sub2/sub3/file",
		NULL
	};

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/subdir/.gitignore", "/**/*\n");

	refute_is_ignored("file");
	assert_is_ignored("subdir/sub2/sub3/file");
	assert_is_ignored("subdir/file");
}

void test_status_ignore__adding_internal_ignores(void)
{
	g_repo = cl_git_sandbox_init("empty_standard_repo");

	refute_is_ignored("one.txt");
	refute_is_ignored("two.bar");

	cl_git_pass(git_ignore_add_rule(g_repo, "*.nomatch\n"));

	refute_is_ignored("one.txt");
	refute_is_ignored("two.bar");

	cl_git_pass(git_ignore_add_rule(g_repo, "*.txt\n"));

	assert_is_ignored("one.txt");
	refute_is_ignored("two.bar");

	cl_git_pass(git_ignore_add_rule(g_repo, "*.bar\n"));

	assert_is_ignored("one.txt");
	assert_is_ignored("two.bar");

	cl_git_pass(git_ignore_clear_internal_rules(g_repo));

	refute_is_ignored("one.txt");
	refute_is_ignored("two.bar");

	cl_git_pass(git_ignore_add_rule(
		g_repo, "multiple\n*.rules\n# comment line\n*.bar\n"));

	refute_is_ignored("one.txt");
	assert_is_ignored("two.bar");
}

void test_status_ignore__add_internal_as_first_thing(void)
{
	const char *add_me = "\n#################\n## Eclipse\n#################\n\n*.pydevproject\n.project\n.metadata\nbin/\ntmp/\n*.tmp\n\n";

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_pass(git_ignore_add_rule(g_repo, add_me));

	assert_is_ignored("one.tmp");
	refute_is_ignored("two.bar");
}

void test_status_ignore__internal_ignores_inside_deep_paths(void)
{
	const char *add_me = "Debug\nthis/is/deep\npatterned*/dir\n";

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_pass(git_ignore_add_rule(g_repo, add_me));

	assert_is_ignored("Debug");
	assert_is_ignored("and/Debug");
	assert_is_ignored("really/Debug/this/file");
	assert_is_ignored("Debug/what/I/say");

	refute_is_ignored("and/NoDebug");
	refute_is_ignored("NoDebug/this");
	refute_is_ignored("please/NoDebug/this");

	assert_is_ignored("this/is/deep");
	/* pattern containing slash gets FNM_PATHNAME so all slashes must match */
	refute_is_ignored("and/this/is/deep");
	assert_is_ignored("this/is/deep/too");
	/* pattern containing slash gets FNM_PATHNAME so all slashes must match */
	refute_is_ignored("but/this/is/deep/and/ignored");

	refute_is_ignored("this/is/not/deep");
	refute_is_ignored("is/this/not/as/deep");
	refute_is_ignored("this/is/deepish");
	refute_is_ignored("xthis/is/deep");
}

void test_status_ignore__automatically_ignore_bad_files(void)
{
	g_repo = cl_git_sandbox_init("empty_standard_repo");

	assert_is_ignored(".git");
	assert_is_ignored("this/file/.");
	assert_is_ignored("path/../funky");
	refute_is_ignored("path/whatever.c");

	cl_git_pass(git_ignore_add_rule(g_repo, "*.c\n"));

	assert_is_ignored(".git");
	assert_is_ignored("this/file/.");
	assert_is_ignored("path/../funky");
	assert_is_ignored("path/whatever.c");

	cl_git_pass(git_ignore_clear_internal_rules(g_repo));

	assert_is_ignored(".git");
	assert_is_ignored("this/file/.");
	assert_is_ignored("path/../funky");
	refute_is_ignored("path/whatever.c");
}

void test_status_ignore__filenames_with_special_prefixes_do_not_interfere_with_status_retrieval(void)
{
	status_entry_single st;
	char *test_cases[] = {
		"!file",
		"#blah",
		"[blah]",
		"[attr]",
		"[attr]blah",
		NULL
	};
	int i;

	for (i = 0; *(test_cases + i) != NULL; i++) {
		git_buf file = GIT_BUF_INIT;
		char *file_name = *(test_cases + i);
		git_repository *repo = cl_git_sandbox_init("empty_standard_repo");

		cl_git_pass(git_buf_joinpath(&file, "empty_standard_repo", file_name));
		cl_git_mkfile(git_buf_cstr(&file), "Please don't ignore me!");

		memset(&st, 0, sizeof(st));
		cl_git_pass(git_status_foreach(repo, cb_status__single, &st));
		cl_assert(st.count == 1);
		cl_assert(st.status == GIT_STATUS_WT_NEW);

		cl_git_pass(git_status_file(&st.status, repo, file_name));
		cl_assert(st.status == GIT_STATUS_WT_NEW);

		cl_git_sandbox_cleanup();
		git_buf_free(&file);
	}
}

void test_status_ignore__issue_1766_negated_ignores(void)
{
	unsigned int status;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_pass(git_futils_mkdir_r(
		"empty_standard_repo/a", 0775));
	cl_git_mkfile(
		"empty_standard_repo/a/.gitignore", "*\n!.gitignore\n");
	cl_git_mkfile(
		"empty_standard_repo/a/ignoreme", "I should be ignored\n");

	refute_is_ignored("a/.gitignore");
	assert_is_ignored("a/ignoreme");

	cl_git_pass(git_futils_mkdir_r(
		"empty_standard_repo/b", 0775));
	cl_git_mkfile(
		"empty_standard_repo/b/.gitignore", "*\n!.gitignore\n");
	cl_git_mkfile(
		"empty_standard_repo/b/ignoreme", "I should be ignored\n");

	refute_is_ignored("b/.gitignore");
	assert_is_ignored("b/ignoreme");

	/* shouldn't have changed results from first couple either */
	refute_is_ignored("a/.gitignore");
	assert_is_ignored("a/ignoreme");

	/* status should find the two ignore files and nothing else */

	cl_git_pass(git_status_file(&status, g_repo, "a/.gitignore"));
	cl_assert_equal_i(GIT_STATUS_WT_NEW, (int)status);

	cl_git_pass(git_status_file(&status, g_repo, "a/ignoreme"));
	cl_assert_equal_i(GIT_STATUS_IGNORED, (int)status);

	cl_git_pass(git_status_file(&status, g_repo, "b/.gitignore"));
	cl_assert_equal_i(GIT_STATUS_WT_NEW, (int)status);

	cl_git_pass(git_status_file(&status, g_repo, "b/ignoreme"));
	cl_assert_equal_i(GIT_STATUS_IGNORED, (int)status);

	{
		git_status_options opts = GIT_STATUS_OPTIONS_INIT;
		status_entry_counts counts;
		static const char *paths[] = {
			"a/.gitignore",
			"a/ignoreme",
			"b/.gitignore",
			"b/ignoreme",
		};
		static const unsigned int statuses[] = {
			GIT_STATUS_WT_NEW,
			GIT_STATUS_IGNORED,
			GIT_STATUS_WT_NEW,
			GIT_STATUS_IGNORED,
		};

		memset(&counts, 0x0, sizeof(status_entry_counts));
		counts.expected_entry_count = 4;
		counts.expected_paths = paths;
		counts.expected_statuses = statuses;

		opts.flags = GIT_STATUS_OPT_DEFAULTS;

		cl_git_pass(git_status_foreach_ext(
			g_repo, &opts, cb_status__normal, &counts));

		cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
		cl_assert_equal_i(0, counts.wrong_status_flags_count);
		cl_assert_equal_i(0, counts.wrong_sorted_path);
	}
}

static void add_one_to_index(const char *file)
{
	git_index *index;
	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_add_bypath(index, file));
	git_index_free(index);
}

/* Some further broken scenarios that have been reported */
void test_status_ignore__more_breakage(void)
{
	static const char *test_files[] = {
		"empty_standard_repo/d1/pfx-d2/d3/d4/d5/tracked",
		"empty_standard_repo/d1/pfx-d2/d3/d4/d5/untracked",
		"empty_standard_repo/d1/pfx-d2/d3/d4/untracked",
		NULL
	};

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/.gitignore",
		"/d1/pfx-*\n"
		"!/d1/pfx-d2/\n"
		"/d1/pfx-d2/*\n"
		"!/d1/pfx-d2/d3/\n"
		"/d1/pfx-d2/d3/*\n"
		"!/d1/pfx-d2/d3/d4/\n");
	add_one_to_index("d1/pfx-d2/d3/d4/d5/tracked");

	{
		git_status_options opts = GIT_STATUS_OPTIONS_INIT;
		status_entry_counts counts;
		static const char *files[] = {
			".gitignore",
			"d1/pfx-d2/d3/d4/d5/tracked",
			"d1/pfx-d2/d3/d4/d5/untracked",
			"d1/pfx-d2/d3/d4/untracked",
		};
		static const unsigned int statuses[] = {
			GIT_STATUS_WT_NEW,
			GIT_STATUS_INDEX_NEW,
			GIT_STATUS_WT_NEW,
			GIT_STATUS_WT_NEW,
		};

		memset(&counts, 0x0, sizeof(status_entry_counts));
		counts.expected_entry_count = 4;
		counts.expected_paths = files;
		counts.expected_statuses = statuses;
		opts.flags = GIT_STATUS_OPT_DEFAULTS |
			GIT_STATUS_OPT_INCLUDE_IGNORED |
			GIT_STATUS_OPT_RECURSE_IGNORED_DIRS;
		cl_git_pass(git_status_foreach_ext(
			g_repo, &opts, cb_status__normal, &counts));

		cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
		cl_assert_equal_i(0, counts.wrong_status_flags_count);
		cl_assert_equal_i(0, counts.wrong_sorted_path);
	}

	refute_is_ignored("d1/pfx-d2/d3/d4/d5/tracked");
	refute_is_ignored("d1/pfx-d2/d3/d4/d5/untracked");
	refute_is_ignored("d1/pfx-d2/d3/d4/untracked");
}

void test_status_ignore__negative_ignores_inside_ignores(void)
{
	static const char *test_files[] = {
		"empty_standard_repo/top/mid/btm/tracked",
		"empty_standard_repo/top/mid/btm/untracked",
		"empty_standard_repo/zoo/bar",
		"empty_standard_repo/zoo/foo/bar",
		NULL
	};

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/.gitignore",
		"top\n"
		"!top/mid/btm\n"
		"zoo/*\n"
		"!zoo/bar\n"
		"!zoo/foo/bar\n");
	add_one_to_index("top/mid/btm/tracked");

	{
		git_status_options opts = GIT_STATUS_OPTIONS_INIT;
		status_entry_counts counts;
		static const char *files[] = {
			".gitignore", "top/mid/btm/tracked", "top/mid/btm/untracked",
			"zoo/bar", "zoo/foo/bar",
		};
		static const unsigned int statuses[] = {
			GIT_STATUS_WT_NEW, GIT_STATUS_INDEX_NEW, GIT_STATUS_IGNORED,
			GIT_STATUS_WT_NEW, GIT_STATUS_IGNORED,
		};

		memset(&counts, 0x0, sizeof(status_entry_counts));
		counts.expected_entry_count = 5;
		counts.expected_paths = files;
		counts.expected_statuses = statuses;
		opts.flags = GIT_STATUS_OPT_DEFAULTS |
			GIT_STATUS_OPT_INCLUDE_IGNORED |
			GIT_STATUS_OPT_RECURSE_IGNORED_DIRS;
		cl_git_pass(git_status_foreach_ext(
			g_repo, &opts, cb_status__normal, &counts));

		cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
		cl_assert_equal_i(0, counts.wrong_status_flags_count);
		cl_assert_equal_i(0, counts.wrong_sorted_path);
	}

	assert_is_ignored("top/mid/btm/tracked");
	assert_is_ignored("top/mid/btm/untracked");
	refute_is_ignored("foo/bar");
}

void test_status_ignore__negative_ignores_in_slash_star(void)
{
	git_status_options status_opts = GIT_STATUS_OPTIONS_INIT;
	git_status_list *list;
	int found_look_ma = 0, found_what_about = 0;
	size_t i;
	static const char *test_files[] = {
		"empty_standard_repo/bin/look-ma.txt",
		"empty_standard_repo/bin/what-about-me.txt",
		NULL
	};

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/.gitignore",
		"bin/*\n"
		"!bin/w*\n");

	assert_is_ignored("bin/look-ma.txt");
	refute_is_ignored("bin/what-about-me.txt");

	status_opts.flags = GIT_STATUS_OPT_DEFAULTS;
	cl_git_pass(git_status_list_new(&list, g_repo, &status_opts));
	for (i = 0; i < git_status_list_entrycount(list); i++) {
		const git_status_entry *entry = git_status_byindex(list, i);

		if (!strcmp("bin/look-ma.txt", entry->index_to_workdir->new_file.path))
			found_look_ma = 1;

		if (!strcmp("bin/what-about-me.txt", entry->index_to_workdir->new_file.path))
			found_what_about = 1;
	}
	git_status_list_free(list);

	cl_assert(found_look_ma);
	cl_assert(found_what_about);
}

void test_status_ignore__negative_ignores_without_trailing_slash_inside_ignores(void)
{
	git_status_options status_opts = GIT_STATUS_OPTIONS_INIT;
	git_status_list *list;
	int found_parent_file = 0, found_parent_child1_file = 0, found_parent_child2_file = 0;
	size_t i;
	static const char *test_files[] = {
		"empty_standard_repo/parent/file.txt",
		"empty_standard_repo/parent/force.txt",
		"empty_standard_repo/parent/child1/file.txt",
		"empty_standard_repo/parent/child2/file.txt",
		NULL
	};

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/.gitignore",
		"parent/*\n"
		"!parent/force.txt\n"
		"!parent/child1\n"
		"!parent/child2/\n");

	add_one_to_index("parent/force.txt");

	assert_is_ignored("parent/file.txt");
	refute_is_ignored("parent/force.txt");
	refute_is_ignored("parent/child1/file.txt");
	refute_is_ignored("parent/child2/file.txt");

	status_opts.flags = GIT_STATUS_OPT_DEFAULTS;
	cl_git_pass(git_status_list_new(&list, g_repo, &status_opts));
	for (i = 0; i < git_status_list_entrycount(list); i++) {
		const git_status_entry *entry = git_status_byindex(list, i);

		if (!entry->index_to_workdir)
			continue;

		if (!strcmp("parent/file.txt", entry->index_to_workdir->new_file.path))
			found_parent_file = 1;

		if (!strcmp("parent/force.txt", entry->index_to_workdir->new_file.path))
			found_parent_file = 1;

		if (!strcmp("parent/child1/file.txt", entry->index_to_workdir->new_file.path))
			found_parent_child1_file = 1;

		if (!strcmp("parent/child2/file.txt", entry->index_to_workdir->new_file.path))
			found_parent_child2_file = 1;
	}
	git_status_list_free(list);

	cl_assert(found_parent_file);
	cl_assert(found_parent_child1_file);
	cl_assert(found_parent_child2_file);
}

void test_status_ignore__negative_directory_ignores(void)
{
	static const char *test_files[] = {
		"empty_standard_repo/parent/child1/bar.txt",
		"empty_standard_repo/parent/child2/bar.txt",
		"empty_standard_repo/parent/child3/foo.txt",
		"empty_standard_repo/parent/child4/bar.txt",
		"empty_standard_repo/parent/nested/child5/bar.txt",
		"empty_standard_repo/parent/nested/child6/bar.txt",
		"empty_standard_repo/parent/nested/child7/bar.txt",
		"empty_standard_repo/padded_parent/child8/bar.txt",
		NULL
	};

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/.gitignore",
		"foo.txt\n"
		"parent/child1\n"
		"parent/child2\n"
		"parent/child4\n"
		"parent/nested/child5\n"
		"nested/child6\n"
		"nested/child7\n"
		"padded_parent/child8\n"
		/* test simple exact match */
		"!parent/child1\n"
		/* test negating file without negating dir */
		"!parent/child2/bar.txt\n"
		/* test negative pattern on dir with its content
		 * being ignored */
		"!parent/child3\n"
		/* test with partial match at end */
		"!child4\n"
		/* test with partial match with '/' at end */
		"!nested/child5\n"
		/* test with complete match */
		"!nested/child6\n"
		/* test with trailing '/' */
		"!child7/\n"
		/* test with partial dir match */
		"!_parent/child8\n");

	refute_is_ignored("parent/child1/bar.txt");
	assert_is_ignored("parent/child2/bar.txt");
	assert_is_ignored("parent/child3/foo.txt");
	refute_is_ignored("parent/child4/bar.txt");
	assert_is_ignored("parent/nested/child5/bar.txt");
	refute_is_ignored("parent/nested/child6/bar.txt");
	refute_is_ignored("parent/nested/child7/bar.txt");
	assert_is_ignored("padded_parent/child8/bar.txt");
}

void test_status_ignore__unignore_entry_in_ignored_dir(void)
{
	static const char *test_files[] = {
		"empty_standard_repo/bar.txt",
		"empty_standard_repo/parent/bar.txt",
		"empty_standard_repo/parent/child/bar.txt",
		"empty_standard_repo/nested/parent/child/bar.txt",
		NULL
	};

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/.gitignore",
		"bar.txt\n"
		"!parent/child/bar.txt\n");

	assert_is_ignored("bar.txt");
	assert_is_ignored("parent/bar.txt");
	refute_is_ignored("parent/child/bar.txt");
	assert_is_ignored("nested/parent/child/bar.txt");
}

void test_status_ignore__do_not_unignore_basename_prefix(void)
{
	static const char *test_files[] = {
		"empty_standard_repo/foo_bar.txt",
		NULL
	};

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/.gitignore",
		"foo_bar.txt\n"
		"!bar.txt\n");

	assert_is_ignored("foo_bar.txt");
}

void test_status_ignore__filename_with_cr(void)
{
	int ignored;

	g_repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_mkfile("empty_standard_repo/.gitignore", "Icon\r\r\n");

	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "Icon\r"));
	cl_assert_equal_i(1, ignored);

	cl_git_mkfile("empty_standard_repo/.gitignore", "Ico\rn\n");
	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "Ico\rn"));
	cl_assert_equal_i(1, ignored);

	cl_git_mkfile("empty_standard_repo/.gitignore", "Ico\rn\r\n");
	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "Ico\rn"));
	cl_assert_equal_i(1, ignored);
	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "Ico\rn\r"));
	cl_assert_equal_i(0, ignored);

	cl_git_mkfile("empty_standard_repo/.gitignore", "Ico\rn\r\r\n");
	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "Ico\rn\r"));
	cl_assert_equal_i(1, ignored);
	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "Icon\r"));
	cl_assert_equal_i(0, ignored);

	cl_git_mkfile("empty_standard_repo/.gitignore", "Icon\r\n");
	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "Icon\r"));
	cl_assert_equal_i(0, ignored);
	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "Icon"));
	cl_assert_equal_i(1, ignored);
}

void test_status_ignore__subdir_doesnt_match_above(void)
{
	int ignored, icase = 0, error;
	git_config *cfg;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_pass(git_repository_config_snapshot(&cfg, g_repo));
	error = git_config_get_bool(&icase, cfg, "core.ignorecase");
	git_config_free(cfg);
	if (error == GIT_ENOTFOUND)
		error = 0;

	cl_git_pass(error);

	cl_git_pass(p_mkdir("empty_standard_repo/src", 0777));
	cl_git_pass(p_mkdir("empty_standard_repo/src/src", 0777));
	cl_git_mkfile("empty_standard_repo/src/.gitignore", "src\n");
	cl_git_mkfile("empty_standard_repo/.gitignore", "");

	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "src/test.txt"));
	cl_assert_equal_i(0, ignored);
	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "src/src/test.txt"));
	cl_assert_equal_i(1, ignored);
	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "src/foo/test.txt"));
	cl_assert_equal_i(0, ignored);

	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "SRC/src/test.txt"));
	cl_assert_equal_i(icase, ignored);
	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "src/SRC/test.txt"));
	cl_assert_equal_i(icase, ignored);
}

void test_status_ignore__negate_exact_previous(void)
{
	int ignored;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_mkfile("empty_standard_repo/.gitignore", "*.com\ntags\n!tags/\n.buildpath");
	cl_git_mkfile("empty_standard_repo/.buildpath", "");
	cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, ".buildpath"));
	cl_assert_equal_i(1, ignored);
}

void test_status_ignore__negate_starstar(void)
{
    int ignored;

    g_repo = cl_git_sandbox_init("empty_standard_repo");

    cl_git_mkfile("empty_standard_repo/.gitignore",
              "code/projects/**/packages/*\n"
              "!code/projects/**/packages/repositories.config");

    cl_git_pass(git_futils_mkdir_r("empty_standard_repo/code/projects/foo/bar/packages", 0777));
    cl_git_mkfile("empty_standard_repo/code/projects/foo/bar/packages/repositories.config", "");

    cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "code/projects/foo/bar/packages/repositories.config"));
    cl_assert_equal_i(0, ignored);
}

void test_status_ignore__ignore_all_toplevel_dirs_include_files(void)
{
	static const char *test_files[] = {
		"empty_standard_repo/README.md",
		"empty_standard_repo/src/main.c",
		"empty_standard_repo/src/foo/foo.c",
		"empty_standard_repo/dist/foo.o",
		"empty_standard_repo/dist/main.o",
		NULL
	};

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/.gitignore",
		"/*/\n"
		"!/src\n");

	assert_is_ignored("dist/foo.o");
	assert_is_ignored("dist/main.o");

	refute_is_ignored("README.md");
	refute_is_ignored("src/foo.c");
	refute_is_ignored("src/foo/foo.c");
}

void test_status_ignore__subdir_ignore_all_toplevel_dirs_include_files(void)
{
	static const char *test_files[] = {
		"empty_standard_repo/project/README.md",
		"empty_standard_repo/project/src/main.c",
		"empty_standard_repo/project/src/foo/foo.c",
		"empty_standard_repo/project/dist/foo.o",
		"empty_standard_repo/project/dist/main.o",
		NULL
	};

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/project/.gitignore",
		"/*/\n"
		"!/src\n");

	assert_is_ignored("project/dist/foo.o");
	assert_is_ignored("project/dist/main.o");

	refute_is_ignored("project/src/foo.c");
	refute_is_ignored("project/src/foo/foo.c");
	refute_is_ignored("project/README.md");
}

void test_status_ignore__subdir_ignore_everything_except_certain_files(void)
{
	static const char *test_files[] = {
		"empty_standard_repo/project/README.md",
		"empty_standard_repo/project/some_file",
		"empty_standard_repo/project/src/main.c",
		"empty_standard_repo/project/src/foo/foo.c",
		"empty_standard_repo/project/dist/foo.o",
		"empty_standard_repo/project/dist/main.o",
		NULL
	};

	make_test_data("empty_standard_repo", test_files);
	cl_git_mkfile(
		"empty_standard_repo/project/.gitignore",
		"/*\n"
		"!/src\n"
		"!README.md\n");

	assert_is_ignored("project/some_file");
	assert_is_ignored("project/dist/foo.o");
	assert_is_ignored("project/dist/main.o");

	refute_is_ignored("project/README.md");
	refute_is_ignored("project/src/foo.c");
	refute_is_ignored("project/src/foo/foo.c");
}

void test_status_ignore__deeper(void)
{
   int ignored;

    g_repo = cl_git_sandbox_init("empty_standard_repo");

    cl_git_mkfile("empty_standard_repo/.gitignore",
          "*.data\n"
          "!dont_ignore/*.data\n");

    cl_git_pass(p_mkdir("empty_standard_repo/dont_ignore", 0777));
    cl_git_mkfile("empty_standard_repo/foo.data", "");
    cl_git_mkfile("empty_standard_repo/bar.data", "");
    cl_git_mkfile("empty_standard_repo/dont_ignore/foo.data", "");
    cl_git_mkfile("empty_standard_repo/dont_ignore/bar.data", "");

    cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "foo.data"));
    cl_assert_equal_i(1, ignored);
    cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "bar.data"));
    cl_assert_equal_i(1, ignored);

    cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "dont_ignore/foo.data"));
    cl_assert_equal_i(0, ignored);
    cl_git_pass(git_ignore_path_is_ignored(&ignored, g_repo, "dont_ignore/bar.data"));
    cl_assert_equal_i(0, ignored);
}
