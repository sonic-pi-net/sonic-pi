#include "clar_libgit2.h"
#include "fileops.h"
#include "status_helpers.h"
#include "../submodule/submodule_helpers.h"

static git_repository *g_repo = NULL;

void test_status_submodules__initialize(void)
{
}

void test_status_submodules__cleanup(void)
{
}

void test_status_submodules__api(void)
{
	git_submodule *sm;

	g_repo = setup_fixture_submodules();

	cl_assert(git_submodule_lookup(NULL, g_repo, "nonexistent") == GIT_ENOTFOUND);

	cl_assert(git_submodule_lookup(NULL, g_repo, "modified") == GIT_ENOTFOUND);

	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));
	cl_assert(sm != NULL);
	cl_assert_equal_s("testrepo", git_submodule_name(sm));
	cl_assert_equal_s("testrepo", git_submodule_path(sm));
	git_submodule_free(sm);
}

void test_status_submodules__0(void)
{
	int counts = 0;

	g_repo = setup_fixture_submodules();

	cl_assert(git_path_isdir("submodules/.git"));
	cl_assert(git_path_isdir("submodules/testrepo/.git"));
	cl_assert(git_path_isfile("submodules/.gitmodules"));

	cl_git_pass(
		git_status_foreach(g_repo, cb_status__count, &counts)
	);

	cl_assert_equal_i(6, counts);
}

static const char *expected_files[] = {
	".gitmodules",
	"added",
	"deleted",
	"ignored",
	"modified",
	"untracked"
};

static unsigned int expected_status[] = {
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_INDEX_NEW,
	GIT_STATUS_INDEX_DELETED,
	GIT_STATUS_IGNORED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_NEW
};

static int cb_status__match(const char *p, unsigned int s, void *payload)
{
	status_entry_counts *counts = payload;
	int idx = counts->entry_count++;

	clar__assert_equal(
		counts->file, counts->line,
		"Status path mismatch", 1,
		"%s", counts->expected_paths[idx], p);

	clar__assert_equal(
		counts->file, counts->line,
		"Status code mismatch", 1,
		"%o", counts->expected_statuses[idx], s);

	return 0;
}

void test_status_submodules__1(void)
{
	status_entry_counts counts;

	g_repo = setup_fixture_submodules();

	cl_assert(git_path_isdir("submodules/.git"));
	cl_assert(git_path_isdir("submodules/testrepo/.git"));
	cl_assert(git_path_isfile("submodules/.gitmodules"));

	status_counts_init(counts, expected_files, expected_status);

	cl_git_pass( git_status_foreach(g_repo, cb_status__match, &counts) );

	cl_assert_equal_i(6, counts.entry_count);
}

void test_status_submodules__single_file(void)
{
	unsigned int status = 0;
	g_repo = setup_fixture_submodules();
	cl_git_pass( git_status_file(&status, g_repo, "testrepo") );
	cl_assert(!status);
}

void test_status_submodules__moved_head(void)
{
	git_submodule *sm;
	git_repository *smrepo;
	git_oid oid;
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts;
	static const char *expected_files_with_sub[] = {
		".gitmodules",
		"added",
		"deleted",
		"ignored",
		"modified",
		"testrepo",
		"untracked"
	};
	static unsigned int expected_status_with_sub[] = {
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_INDEX_NEW,
		GIT_STATUS_INDEX_DELETED,
		GIT_STATUS_IGNORED,
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_WT_NEW
	};

	g_repo = setup_fixture_submodules();

	cl_git_pass(git_submodule_lookup(&sm, g_repo, "testrepo"));
	cl_git_pass(git_submodule_open(&smrepo, sm));
	git_submodule_free(sm);

	/* move submodule HEAD to c47800c7266a2be04c571c04d5a6614691ea99bd */
	cl_git_pass(
		git_oid_fromstr(&oid, "c47800c7266a2be04c571c04d5a6614691ea99bd"));
	cl_git_pass(git_repository_set_head_detached(smrepo, &oid, NULL, NULL));

	/* first do a normal status, which should now include the submodule */

	opts.flags = GIT_STATUS_OPT_DEFAULTS;

	status_counts_init(
		counts, expected_files_with_sub, expected_status_with_sub);
	cl_git_pass(
		git_status_foreach_ext(g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(7, counts.entry_count);

	/* try again with EXCLUDE_SUBMODULES which should skip it */

	opts.flags = GIT_STATUS_OPT_DEFAULTS | GIT_STATUS_OPT_EXCLUDE_SUBMODULES;

	status_counts_init(counts, expected_files, expected_status);
	cl_git_pass(
		git_status_foreach_ext(g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(6, counts.entry_count);

	git_repository_free(smrepo);
}

void test_status_submodules__dirty_workdir_only(void)
{
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts;
	static const char *expected_files_with_sub[] = {
		".gitmodules",
		"added",
		"deleted",
		"ignored",
		"modified",
		"testrepo",
		"untracked"
	};
	static unsigned int expected_status_with_sub[] = {
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_INDEX_NEW,
		GIT_STATUS_INDEX_DELETED,
		GIT_STATUS_IGNORED,
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_WT_NEW
	};

	g_repo = setup_fixture_submodules();

	cl_git_rewritefile("submodules/testrepo/README", "heyheyhey");
	cl_git_mkfile("submodules/testrepo/all_new.txt", "never seen before");

	/* first do a normal status, which should now include the submodule */

	opts.flags = GIT_STATUS_OPT_DEFAULTS;

	status_counts_init(
		counts, expected_files_with_sub, expected_status_with_sub);
	cl_git_pass(
		git_status_foreach_ext(g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(7, counts.entry_count);

	/* try again with EXCLUDE_SUBMODULES which should skip it */

	opts.flags = GIT_STATUS_OPT_DEFAULTS | GIT_STATUS_OPT_EXCLUDE_SUBMODULES;

	status_counts_init(counts, expected_files, expected_status);
	cl_git_pass(
		git_status_foreach_ext(g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(6, counts.entry_count);
}

void test_status_submodules__uninitialized(void)
{
	git_repository *cloned_repo;
	git_status_list *statuslist;

	g_repo = cl_git_sandbox_init("submod2");

	cl_git_pass(git_clone(&cloned_repo, "submod2", "submod2-clone", NULL));

	cl_git_pass(git_status_list_new(&statuslist, cloned_repo, NULL));
	cl_assert_equal_i(0, git_status_list_entrycount(statuslist));

	git_status_list_free(statuslist);
	git_repository_free(cloned_repo);
	cl_git_sandbox_cleanup();
}

void test_status_submodules__contained_untracked_repo(void)
{
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts;
	git_repository *contained;
	static const char *expected_files_not_ignored[] = {
		".gitmodules",
		"added",
		"deleted",
		"modified",
		"untracked"
	};
	static unsigned int expected_status_not_ignored[] = {
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_INDEX_NEW,
		GIT_STATUS_INDEX_DELETED,
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_WT_NEW,
	};
	static const char *expected_files_with_untracked[] = {
		".gitmodules",
		"added",
		"deleted",
		"dir/file.md",
		"modified",
		"untracked"
	};
	static const char *expected_files_with_untracked_dir[] = {
		".gitmodules",
		"added",
		"deleted",
		"dir/",
		"modified",
		"untracked"
	};
	static unsigned int expected_status_with_untracked[] = {
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_INDEX_NEW,
		GIT_STATUS_INDEX_DELETED,
		GIT_STATUS_WT_NEW,
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_WT_NEW
	};

	g_repo = setup_fixture_submodules();

	/* skip empty directory */

	cl_must_pass(p_mkdir("submodules/dir", 0777));
	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED;

	status_counts_init(
		counts, expected_files_not_ignored, expected_status_not_ignored);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(5, counts.entry_count);

	/* still skipping because empty == ignored */

	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;

	status_counts_init(
		counts, expected_files_not_ignored, expected_status_not_ignored);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(5, counts.entry_count);

	/* find non-ignored contents of directory */

	cl_git_mkfile("submodules/dir/file.md", "hello");
	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;

	status_counts_init(
		counts, expected_files_with_untracked, expected_status_with_untracked);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(6, counts.entry_count);

	/* but skip if all content is ignored */

	cl_git_append2file("submodules/.git/info/exclude", "\n*.md\n\n");
	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;

	status_counts_init(
		counts, expected_files_not_ignored, expected_status_not_ignored);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(5, counts.entry_count);

	/* same is true if it contains a git link */

	cl_git_mkfile("submodules/dir/.git", "gitlink: ../.git");
	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;

	status_counts_init(
		counts, expected_files_not_ignored, expected_status_not_ignored);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(5, counts.entry_count);

	/* but if it contains tracked files, it should just show up as a
	 * directory and exclude the files in it
	 */

	cl_git_mkfile("submodules/dir/another_file", "hello");
	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;

	status_counts_init(
		counts, expected_files_with_untracked_dir,
		expected_status_with_untracked);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(6, counts.entry_count);

	/* that applies to a git repo with a .git directory too */

	cl_must_pass(p_unlink("submodules/dir/.git"));
	cl_git_pass(git_repository_init(&contained, "submodules/dir", false));
	git_repository_free(contained);
	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;

	status_counts_init(
		counts, expected_files_with_untracked_dir,
		expected_status_with_untracked);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(6, counts.entry_count);

	/* same result even if we don't recurse into subdirectories */

	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED;

	status_counts_init(
		counts, expected_files_with_untracked_dir,
		expected_status_with_untracked);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(6, counts.entry_count);

	/* and if we remove the untracked file, it goes back to ignored */

	cl_must_pass(p_unlink("submodules/dir/another_file"));

	status_counts_init(
		counts, expected_files_not_ignored, expected_status_not_ignored);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(5, counts.entry_count);
}

void test_status_submodules__broken_stuff_that_git_allows(void)
{
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts;
	git_repository *contained;
	static const char *expected_files_with_broken[] = {
		".gitmodules",
		"added",
		"broken/tracked",
		"deleted",
		"ignored",
		"modified",
		"untracked"
	};
	static unsigned int expected_status_with_broken[] = {
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_INDEX_NEW,
		GIT_STATUS_INDEX_NEW,
		GIT_STATUS_INDEX_DELETED,
		GIT_STATUS_IGNORED,
		GIT_STATUS_WT_MODIFIED,
		GIT_STATUS_WT_NEW,
	};

	g_repo = setup_fixture_submodules();

	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS |
		GIT_STATUS_OPT_INCLUDE_IGNORED;

	/* make a directory and stick a tracked item into the index */
	{
		git_index *idx;
		cl_must_pass(p_mkdir("submodules/broken", 0777));
		cl_git_mkfile("submodules/broken/tracked", "tracked content");
		cl_git_pass(git_repository_index(&idx, g_repo));
		cl_git_pass(git_index_add_bypath(idx, "broken/tracked"));
		cl_git_pass(git_index_write(idx));
		git_index_free(idx);
	}

	status_counts_init(
		counts, expected_files_with_broken, expected_status_with_broken);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(7, counts.entry_count);

	/* directory with tracked items that looks a little bit like a repo */

	cl_must_pass(p_mkdir("submodules/broken/.git", 0777));
	cl_must_pass(p_mkdir("submodules/broken/.git/info", 0777));
	cl_git_mkfile("submodules/broken/.git/info/exclude", "# bogus");

	status_counts_init(
		counts, expected_files_with_broken, expected_status_with_broken);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(7, counts.entry_count);

	/* directory with tracked items that is a repo */

	cl_git_pass(git_futils_rmdir_r(
		"submodules/broken/.git", NULL, GIT_RMDIR_REMOVE_FILES));
	cl_git_pass(git_repository_init(&contained, "submodules/broken", false));
	git_repository_free(contained);

	status_counts_init(
		counts, expected_files_with_broken, expected_status_with_broken);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(7, counts.entry_count);

	/* directory with tracked items that claims to be a submodule but is not */

	cl_git_pass(git_futils_rmdir_r(
		"submodules/broken/.git", NULL, GIT_RMDIR_REMOVE_FILES));
	cl_git_append2file("submodules/.gitmodules",
		"\n[submodule \"broken\"]\n"
		"\tpath = broken\n"
		"\turl = https://github.com/not/used\n\n");

	status_counts_init(
		counts, expected_files_with_broken, expected_status_with_broken);
	cl_git_pass(git_status_foreach_ext(
		g_repo, &opts, cb_status__match, &counts));
	cl_assert_equal_i(7, counts.entry_count);
}

