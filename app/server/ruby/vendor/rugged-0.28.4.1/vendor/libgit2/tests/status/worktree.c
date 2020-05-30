#include "clar_libgit2.h"
#include "futils.h"
#include "ignore.h"
#include "status_data.h"
#include "posix.h"
#include "util.h"
#include "path.h"
#include "../diff/diff_helpers.h"
#include "../checkout/checkout_helpers.h"
#include "git2/sys/diff.h"

/**
 * Cleanup
 *
 * This will be called once after each test finishes, even
 * if the test failed
 */
void test_status_worktree__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

/**
 * Tests - Status determination on a working tree
 */
/* this test is equivalent to t18-status.c:statuscb0 */
void test_status_worktree__whole_repository(void)
{
	status_entry_counts counts;
	git_repository *repo = cl_git_sandbox_init("status");

	memset(&counts, 0x0, sizeof(status_entry_counts));
	counts.expected_entry_count = entry_count0;
	counts.expected_paths = entry_paths0;
	counts.expected_statuses = entry_statuses0;

	cl_git_pass(
		git_status_foreach(repo, cb_status__normal, &counts)
	);

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

void assert_show(
	const int entry_counts,
	const char *entry_paths[],
	const unsigned int entry_statuses[],
	git_repository *repo,
	git_status_show_t show,
	unsigned int extra_flags)
{
	status_entry_counts counts;
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;

	memset(&counts, 0x0, sizeof(status_entry_counts));
	counts.expected_entry_count = entry_counts;
	counts.expected_paths = entry_paths;
	counts.expected_statuses = entry_statuses;

	opts.flags = GIT_STATUS_OPT_DEFAULTS | extra_flags;
	opts.show = show;

	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts)
	);

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

void test_status_worktree__show_index_and_workdir(void)
{
	assert_show(entry_count0, entry_paths0, entry_statuses0,
		cl_git_sandbox_init("status"), GIT_STATUS_SHOW_INDEX_AND_WORKDIR, 0);
}

void test_status_worktree__show_index_only(void)
{
	assert_show(entry_count5, entry_paths5, entry_statuses5,
		cl_git_sandbox_init("status"), GIT_STATUS_SHOW_INDEX_ONLY, 0);
}

void test_status_worktree__show_workdir_only(void)
{
	assert_show(entry_count6, entry_paths6, entry_statuses6,
		cl_git_sandbox_init("status"), GIT_STATUS_SHOW_WORKDIR_ONLY, 0);
}

/* this test is equivalent to t18-status.c:statuscb1 */
void test_status_worktree__empty_repository(void)
{
	int count = 0;
	git_repository *repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_pass(git_status_foreach(repo, cb_status__count, &count));

	cl_assert_equal_i(0, count);
}

static int remove_file_cb(void *data, git_buf *file)
{
	const char *filename = git_buf_cstr(file);

	GIT_UNUSED(data);

	if (git__suffixcmp(filename, ".git") == 0)
		return 0;

	if (git_path_isdir(filename))
		cl_git_pass(git_futils_rmdir_r(filename, NULL, GIT_RMDIR_REMOVE_FILES));
	else
		cl_git_pass(p_unlink(git_buf_cstr(file)));

	return 0;
}

/* this test is equivalent to t18-status.c:statuscb2 */
void test_status_worktree__purged_worktree(void)
{
	status_entry_counts counts;
	git_repository *repo = cl_git_sandbox_init("status");
	git_buf workdir = GIT_BUF_INIT;

	/* first purge the contents of the worktree */
	cl_git_pass(git_buf_sets(&workdir, git_repository_workdir(repo)));
	cl_git_pass(git_path_direach(&workdir, 0, remove_file_cb, NULL));
	git_buf_dispose(&workdir);

	/* now get status */
	memset(&counts, 0x0, sizeof(status_entry_counts));
	counts.expected_entry_count = entry_count2;
	counts.expected_paths = entry_paths2;
	counts.expected_statuses = entry_statuses2;

	cl_git_pass(
		git_status_foreach(repo, cb_status__normal, &counts)
	);

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

/* this test is similar to t18-status.c:statuscb3 */
void test_status_worktree__swap_subdir_and_file(void)
{
	status_entry_counts counts;
	git_repository *repo = cl_git_sandbox_init("status");
	git_index *index;
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	bool ignore_case;

	cl_git_pass(git_repository_index(&index, repo));
	ignore_case = (git_index_caps(index) & GIT_INDEX_CAPABILITY_IGNORE_CASE) != 0;
	git_index_free(index);

	/* first alter the contents of the worktree */
	cl_git_pass(p_rename("status/current_file", "status/swap"));
	cl_git_pass(p_rename("status/subdir", "status/current_file"));
	cl_git_pass(p_rename("status/swap", "status/subdir"));

	cl_git_mkfile("status/.HEADER", "dummy");
	cl_git_mkfile("status/42-is-not-prime.sigh", "dummy");
	cl_git_mkfile("status/README.md", "dummy");

	/* now get status */
	memset(&counts, 0x0, sizeof(status_entry_counts));
	counts.expected_entry_count = entry_count3;
	counts.expected_paths = ignore_case ? entry_paths3_icase : entry_paths3;
	counts.expected_statuses = ignore_case ? entry_statuses3_icase : entry_statuses3;

	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_INCLUDE_IGNORED;

	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts)
	);

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

void test_status_worktree__swap_subdir_with_recurse_and_pathspec(void)
{
	status_entry_counts counts;
	git_repository *repo = cl_git_sandbox_init("status");
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;

	/* first alter the contents of the worktree */
	cl_git_pass(p_rename("status/current_file", "status/swap"));
	cl_git_pass(p_rename("status/subdir", "status/current_file"));
	cl_git_pass(p_rename("status/swap", "status/subdir"));
	cl_git_mkfile("status/.new_file", "dummy");
	cl_git_pass(git_futils_mkdir_r("status/zzz_new_dir", 0777));
	cl_git_mkfile("status/zzz_new_dir/new_file", "dummy");
	cl_git_mkfile("status/zzz_new_file", "dummy");

	/* now get status */
	memset(&counts, 0x0, sizeof(status_entry_counts));
	counts.expected_entry_count = entry_count4;
	counts.expected_paths = entry_paths4;
	counts.expected_statuses = entry_statuses4;

	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;
	/* TODO: set pathspec to "current_file" eventually */

	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts)
	);

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

static void stage_and_commit(git_repository *repo, const char *path)
{
	git_index *index;

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, path));
	cl_repo_commit_from_index(NULL, repo, NULL, 1323847743, "Initial commit\n");
	git_index_free(index);
}

void test_status_worktree__within_subdir(void)
{
	status_entry_counts counts;
	git_repository *repo = cl_git_sandbox_init("status");
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	char *paths[] = { "zzz_new_dir" };
	git_strarray pathsArray;

	/* first alter the contents of the worktree */
	cl_git_mkfile("status/.new_file", "dummy");
	cl_git_pass(git_futils_mkdir_r("status/zzz_new_dir", 0777));
	cl_git_mkfile("status/zzz_new_dir/new_file", "dummy");
	cl_git_mkfile("status/zzz_new_file", "dummy");
	cl_git_mkfile("status/wut", "dummy");

	stage_and_commit(repo, "zzz_new_dir/new_file");

	/* now get status */
	memset(&counts, 0x0, sizeof(status_entry_counts));
	counts.expected_entry_count = entry_count4;
	counts.expected_paths = entry_paths4;
	counts.expected_statuses = entry_statuses4;
	counts.debug = true;

	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS |
		GIT_STATUS_OPT_DISABLE_PATHSPEC_MATCH;

	pathsArray.count = 1;
	pathsArray.strings = paths;
	opts.pathspec = pathsArray;

	/* We committed zzz_new_dir/new_file above. It shouldn't be reported. */
	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts)
	);

	cl_assert_equal_i(0, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

/* this test is equivalent to t18-status.c:singlestatus0 */
void test_status_worktree__single_file(void)
{
	int i;
	unsigned int status_flags;
	git_repository *repo = cl_git_sandbox_init("status");

	for (i = 0; i < (int)entry_count0; i++) {
		cl_git_pass(
			git_status_file(&status_flags, repo, entry_paths0[i])
		);
		cl_assert(entry_statuses0[i] == status_flags);
	}
}

/* this test is equivalent to t18-status.c:singlestatus1 */
void test_status_worktree__single_nonexistent_file(void)
{
	int error;
	unsigned int status_flags;
	git_repository *repo = cl_git_sandbox_init("status");

	error = git_status_file(&status_flags, repo, "nonexistent");
	cl_git_fail(error);
	cl_assert(error == GIT_ENOTFOUND);
}

/* this test is equivalent to t18-status.c:singlestatus2 */
void test_status_worktree__single_nonexistent_file_empty_repo(void)
{
	int error;
	unsigned int status_flags;
	git_repository *repo = cl_git_sandbox_init("empty_standard_repo");

	error = git_status_file(&status_flags, repo, "nonexistent");
	cl_git_fail(error);
	cl_assert(error == GIT_ENOTFOUND);
}

/* this test is equivalent to t18-status.c:singlestatus3 */
void test_status_worktree__single_file_empty_repo(void)
{
	unsigned int status_flags;
	git_repository *repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_mkfile("empty_standard_repo/new_file", "new_file\n");

	cl_git_pass(git_status_file(&status_flags, repo, "new_file"));
	cl_assert(status_flags == GIT_STATUS_WT_NEW);
}

/* this test is equivalent to t18-status.c:singlestatus4 */
void test_status_worktree__single_folder(void)
{
	int error;
	unsigned int status_flags;
	git_repository *repo = cl_git_sandbox_init("status");

	error = git_status_file(&status_flags, repo, "subdir");
	cl_git_fail(error);
	cl_assert(error != GIT_ENOTFOUND);
}


void test_status_worktree__ignores(void)
{
	int i, ignored;
	git_repository *repo = cl_git_sandbox_init("status");

	for (i = 0; i < (int)entry_count0; i++) {
		cl_git_pass(
			git_status_should_ignore(&ignored, repo, entry_paths0[i])
		);
		cl_assert(ignored == (entry_statuses0[i] == GIT_STATUS_IGNORED));
	}

	cl_git_pass(
		git_status_should_ignore(&ignored, repo, "nonexistent_file")
	);
	cl_assert(!ignored);

	cl_git_pass(
		git_status_should_ignore(&ignored, repo, "ignored_nonexistent_file")
	);
	cl_assert(ignored);
}

static int cb_status__check_592(const char *p, unsigned int s, void *payload)
{
	if (s != GIT_STATUS_WT_DELETED ||
		(payload != NULL && strcmp(p, (const char *)payload) != 0))
		return -1;

	return 0;
}

void test_status_worktree__issue_592(void)
{
	git_repository *repo;
	git_buf path = GIT_BUF_INIT;

	repo = cl_git_sandbox_init("issue_592");
	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(repo), "l.txt"));
	cl_git_pass(p_unlink(git_buf_cstr(&path)));
	cl_assert(!git_path_exists("issue_592/l.txt"));

	cl_git_pass(git_status_foreach(repo, cb_status__check_592, "l.txt"));

	git_buf_dispose(&path);
}

void test_status_worktree__issue_592_2(void)
{
	git_repository *repo;
	git_buf path = GIT_BUF_INIT;

	repo = cl_git_sandbox_init("issue_592");
	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(repo), "c/a.txt"));
	cl_git_pass(p_unlink(git_buf_cstr(&path)));
	cl_assert(!git_path_exists("issue_592/c/a.txt"));

	cl_git_pass(git_status_foreach(repo, cb_status__check_592, "c/a.txt"));

	git_buf_dispose(&path);
}

void test_status_worktree__issue_592_3(void)
{
	git_repository *repo;
	git_buf path = GIT_BUF_INIT;

	repo = cl_git_sandbox_init("issue_592");

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(repo), "c"));
	cl_git_pass(git_futils_rmdir_r(git_buf_cstr(&path), NULL, GIT_RMDIR_REMOVE_FILES));
	cl_assert(!git_path_exists("issue_592/c/a.txt"));

	cl_git_pass(git_status_foreach(repo, cb_status__check_592, "c/a.txt"));

	git_buf_dispose(&path);
}

void test_status_worktree__issue_592_4(void)
{
	git_repository *repo;
	git_buf path = GIT_BUF_INIT;

	repo = cl_git_sandbox_init("issue_592");

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(repo), "t/b.txt"));
	cl_git_pass(p_unlink(git_buf_cstr(&path)));

	cl_git_pass(git_status_foreach(repo, cb_status__check_592, "t/b.txt"));

	git_buf_dispose(&path);
}

void test_status_worktree__issue_592_5(void)
{
	git_repository *repo;
	git_buf path = GIT_BUF_INIT;

	repo = cl_git_sandbox_init("issue_592");

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(repo), "t"));
	cl_git_pass(git_futils_rmdir_r(git_buf_cstr(&path), NULL, GIT_RMDIR_REMOVE_FILES));
	cl_git_pass(p_mkdir(git_buf_cstr(&path), 0777));

	cl_git_pass(git_status_foreach(repo, cb_status__check_592, NULL));

	git_buf_dispose(&path);
}

void test_status_worktree__issue_592_ignores_0(void)
{
	int count = 0;
	status_entry_single st;
	git_repository *repo = cl_git_sandbox_init("issue_592");

	cl_git_pass(git_status_foreach(repo, cb_status__count, &count));
	cl_assert_equal_i(0, count);

	cl_git_rewritefile("issue_592/.gitignore",
		".gitignore\n*.txt\nc/\n[tT]*/\n");

	cl_git_pass(git_status_foreach(repo, cb_status__count, &count));
	cl_assert_equal_i(1, count);

	/* This is a situation where the behavior of libgit2 is
	 * different from core git.  Core git will show ignored.txt
	 * in the list of ignored files, even though the directory
	 * "t" is ignored and the file is untracked because we have
	 * the explicit "*.txt" ignore rule.  Libgit2 just excludes
	 * all untracked files that are contained within ignored
	 * directories without explicitly listing them.
	 */
	cl_git_rewritefile("issue_592/t/ignored.txt", "ping");

	memset(&st, 0, sizeof(st));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &st));
	cl_assert_equal_i(1, st.count);
	cl_assert(st.status == GIT_STATUS_IGNORED);

	cl_git_rewritefile("issue_592/c/ignored_by_dir", "ping");

	memset(&st, 0, sizeof(st));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &st));
	cl_assert_equal_i(1, st.count);
	cl_assert(st.status == GIT_STATUS_IGNORED);

	cl_git_rewritefile("issue_592/t/ignored_by_dir_pattern", "ping");

	memset(&st, 0, sizeof(st));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &st));
	cl_assert_equal_i(1, st.count);
	cl_assert(st.status == GIT_STATUS_IGNORED);
}

void test_status_worktree__issue_592_ignored_dirs_with_tracked_content(void)
{
	int count = 0;
	git_repository *repo = cl_git_sandbox_init("issue_592b");

	cl_git_pass(git_status_foreach(repo, cb_status__count, &count));
	cl_assert_equal_i(1, count);

	/* if we are really mimicking core git, then only ignored1.txt
	 * at the top level will show up in the ignores list here.
	 * everything else will be unmodified or skipped completely.
	 */
}

void test_status_worktree__conflict_with_diff3(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_index *index;
	unsigned int status;
	git_index_entry ancestor_entry, our_entry, their_entry;

	memset(&ancestor_entry, 0x0, sizeof(git_index_entry));
	memset(&our_entry, 0x0, sizeof(git_index_entry));
	memset(&their_entry, 0x0, sizeof(git_index_entry));

	ancestor_entry.path = "modified_file";
	ancestor_entry.mode = 0100644;
	git_oid_fromstr(&ancestor_entry.id,
		"452e4244b5d083ddf0460acf1ecc74db9dcfa11a");

	our_entry.path = "modified_file";
	our_entry.mode = 0100644;
	git_oid_fromstr(&our_entry.id,
		"452e4244b5d083ddf0460acf1ecc74db9dcfa11a");

	their_entry.path = "modified_file";
	their_entry.mode = 0100644;
	git_oid_fromstr(&their_entry.id,
		"452e4244b5d083ddf0460acf1ecc74db9dcfa11a");

	cl_git_pass(git_status_file(&status, repo, "modified_file"));
	cl_assert_equal_i(GIT_STATUS_WT_MODIFIED, status);

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_remove(index, "modified_file", 0));
	cl_git_pass(git_index_conflict_add(
		index, &ancestor_entry, &our_entry, &their_entry));
	cl_git_pass(git_index_write(index));
	git_index_free(index);

	cl_git_pass(git_status_file(&status, repo, "modified_file"));

	cl_assert_equal_i(GIT_STATUS_CONFLICTED, status);
}

static const char *filemode_paths[] = {
	"exec_off",
	"exec_off2on_staged",
	"exec_off2on_workdir",
	"exec_off_untracked",
	"exec_on",
	"exec_on2off_staged",
	"exec_on2off_workdir",
	"exec_on_untracked",
};

static unsigned int filemode_statuses[] = {
	GIT_STATUS_CURRENT,
	GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_NEW,
	GIT_STATUS_CURRENT,
	GIT_STATUS_INDEX_MODIFIED,
	GIT_STATUS_WT_MODIFIED,
	GIT_STATUS_WT_NEW
};

static const int filemode_count = 8;

void test_status_worktree__filemode_changes(void)
{
	git_repository *repo = cl_git_sandbox_init("filemodes");
	status_entry_counts counts;
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;

	/* overwrite stored filemode with platform appropriate value */
	if (cl_is_chmod_supported())
		cl_repo_set_bool(repo, "core.filemode", true);
	else {
		int i;

		cl_repo_set_bool(repo, "core.filemode", false);

		/* won't trust filesystem mode diffs, so these will appear unchanged */
		for (i = 0; i < filemode_count; ++i)
			if (filemode_statuses[i] == GIT_STATUS_WT_MODIFIED)
				filemode_statuses[i] = GIT_STATUS_CURRENT;
	}

	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_INCLUDE_IGNORED |
		GIT_STATUS_OPT_INCLUDE_UNMODIFIED;

	memset(&counts, 0, sizeof(counts));
	counts.expected_entry_count = filemode_count;
	counts.expected_paths = filemode_paths;
	counts.expected_statuses = filemode_statuses;

	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts)
	);

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

static int cb_status__interrupt(const char *p, unsigned int s, void *payload)
{
	volatile int *count = (int *)payload;

	GIT_UNUSED(p);
	GIT_UNUSED(s);

	(*count)++;

	return (*count == 8) ? -111 : 0;
}

void test_status_worktree__interruptable_foreach(void)
{
	int count = 0;
	git_repository *repo = cl_git_sandbox_init("status");

	cl_assert_equal_i(
		-111, git_status_foreach(repo, cb_status__interrupt, &count)
	);

	cl_assert_equal_i(8, count);
}

void test_status_worktree__line_endings_dont_count_as_changes_with_autocrlf(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	unsigned int status;

	cl_repo_set_bool(repo, "core.autocrlf", true);

	cl_git_rewritefile("status/current_file", "current_file\r\n");

	cl_git_pass(git_status_file(&status, repo, "current_file"));

	/* stat data on file should no longer match stat cache, even though
	 * file diff will be empty because of line-ending conversion - matches
	 * the Git command-line behavior here.
	 */
	cl_assert_equal_i(GIT_STATUS_WT_MODIFIED, status);
}

void test_status_worktree__line_endings_dont_count_as_changes_with_autocrlf_issue_1397(void)
{
	git_repository *repo = cl_git_sandbox_init("issue_1397");
	unsigned int status;

	cl_repo_set_bool(repo, "core.autocrlf", true);

	cl_git_pass(git_status_file(&status, repo, "crlf_file.txt"));

	cl_assert_equal_i(GIT_STATUS_CURRENT, status);
}

void test_status_worktree__conflicted_item(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_index *index;
	unsigned int status;
	git_index_entry ancestor_entry, our_entry, their_entry;

	memset(&ancestor_entry, 0x0, sizeof(git_index_entry));
	memset(&our_entry, 0x0, sizeof(git_index_entry));
	memset(&their_entry, 0x0, sizeof(git_index_entry));

	ancestor_entry.mode = 0100644;
	ancestor_entry.path = "modified_file";
	git_oid_fromstr(&ancestor_entry.id,
		"452e4244b5d083ddf0460acf1ecc74db9dcfa11a");

	our_entry.mode = 0100644;
	our_entry.path = "modified_file";
	git_oid_fromstr(&our_entry.id,
		"452e4244b5d083ddf0460acf1ecc74db9dcfa11a");

	their_entry.mode = 0100644;
	their_entry.path = "modified_file";
	git_oid_fromstr(&their_entry.id,
		"452e4244b5d083ddf0460acf1ecc74db9dcfa11a");

	cl_git_pass(git_status_file(&status, repo, "modified_file"));
	cl_assert_equal_i(GIT_STATUS_WT_MODIFIED, status);

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_conflict_add(index, &ancestor_entry,
		&our_entry, &their_entry));

	cl_git_pass(git_status_file(&status, repo, "modified_file"));
	cl_assert_equal_i(GIT_STATUS_CONFLICTED, status);

	git_index_free(index);
}

void test_status_worktree__conflict_has_no_oid(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_index *index;
	git_index_entry entry = {{0}};
	git_status_list *statuslist;
	const git_status_entry *status;
	git_oid zero_id = {{0}};

	entry.mode = 0100644;
	entry.path = "modified_file";
	git_oid_fromstr(&entry.id, "452e4244b5d083ddf0460acf1ecc74db9dcfa11a");

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_conflict_add(index, &entry, &entry, &entry));

	git_status_list_new(&statuslist, repo, NULL);

	cl_assert_equal_i(16, git_status_list_entrycount(statuslist));

	status = git_status_byindex(statuslist, 2);

	cl_assert_equal_i(GIT_STATUS_CONFLICTED, status->status);
	cl_assert_equal_s("modified_file", status->head_to_index->old_file.path);
	cl_assert(!git_oid_equal(&zero_id, &status->head_to_index->old_file.id));
	cl_assert(0 != status->head_to_index->old_file.mode);
	cl_assert_equal_s("modified_file", status->head_to_index->new_file.path);
	cl_assert_equal_oid(&zero_id, &status->head_to_index->new_file.id);
	cl_assert_equal_i(0, status->head_to_index->new_file.mode);
	cl_assert_equal_i(0, status->head_to_index->new_file.size);

	cl_assert_equal_s("modified_file", status->index_to_workdir->old_file.path);
	cl_assert_equal_oid(&zero_id, &status->index_to_workdir->old_file.id);
	cl_assert_equal_i(0, status->index_to_workdir->old_file.mode);
	cl_assert_equal_i(0, status->index_to_workdir->old_file.size);
	cl_assert_equal_s("modified_file", status->index_to_workdir->new_file.path);
	cl_assert(
		!git_oid_equal(&zero_id, &status->index_to_workdir->new_file.id) ||
		!(status->index_to_workdir->new_file.flags & GIT_DIFF_FLAG_VALID_ID));
	cl_assert(0 != status->index_to_workdir->new_file.mode);
	cl_assert(0 != status->index_to_workdir->new_file.size);

	git_index_free(index);
	git_status_list_free(statuslist);
}

static void assert_ignore_case(
	bool should_ignore_case,
	int expected_lower_cased_file_status,
	int expected_camel_cased_file_status)
{
	unsigned int status;
	git_buf lower_case_path = GIT_BUF_INIT, camel_case_path = GIT_BUF_INIT;
	git_repository *repo, *repo2;

	repo = cl_git_sandbox_init("empty_standard_repo");
	cl_git_remove_placeholders(git_repository_path(repo), "dummy-marker.txt");

	cl_repo_set_bool(repo, "core.ignorecase", should_ignore_case);

	cl_git_pass(git_buf_joinpath(&lower_case_path,
		git_repository_workdir(repo), "plop"));

	cl_git_mkfile(git_buf_cstr(&lower_case_path), "");

	stage_and_commit(repo, "plop");

	cl_git_pass(git_repository_open(&repo2, "./empty_standard_repo"));

	cl_git_pass(git_status_file(&status, repo2, "plop"));
	cl_assert_equal_i(GIT_STATUS_CURRENT, status);

	cl_git_pass(git_buf_joinpath(&camel_case_path,
		git_repository_workdir(repo), "Plop"));

	cl_git_pass(p_rename(git_buf_cstr(&lower_case_path), git_buf_cstr(&camel_case_path)));

	cl_git_pass(git_status_file(&status, repo2, "plop"));
	cl_assert_equal_i(expected_lower_cased_file_status, status);

	cl_git_pass(git_status_file(&status, repo2, "Plop"));
	cl_assert_equal_i(expected_camel_cased_file_status, status);

	git_repository_free(repo2);
	git_buf_dispose(&lower_case_path);
	git_buf_dispose(&camel_case_path);
}

void test_status_worktree__file_status_honors_core_ignorecase_true(void)
{
	assert_ignore_case(true, GIT_STATUS_CURRENT, GIT_STATUS_CURRENT);
}

void test_status_worktree__file_status_honors_core_ignorecase_false(void)
{
	assert_ignore_case(false, GIT_STATUS_WT_DELETED, GIT_STATUS_WT_NEW);
}

void test_status_worktree__file_status_honors_case_ignorecase_regarding_untracked_files(void)
{
    git_repository *repo = cl_git_sandbox_init("status");
    unsigned int status;
    git_index *index;

    cl_repo_set_bool(repo, "core.ignorecase", false);

	repo = cl_git_sandbox_reopen();

    /* Actually returns GIT_STATUS_IGNORED on Windows */
    cl_git_fail_with(git_status_file(&status, repo, "NEW_FILE"), GIT_ENOTFOUND);

    cl_git_pass(git_repository_index(&index, repo));

    cl_git_pass(git_index_add_bypath(index, "new_file"));
    cl_git_pass(git_index_write(index));
    git_index_free(index);

    /* Actually returns GIT_STATUS_IGNORED on Windows */
    cl_git_fail_with(git_status_file(&status, repo, "NEW_FILE"), GIT_ENOTFOUND);
}

void test_status_worktree__simple_delete(void)
{
    git_repository *repo = cl_git_sandbox_init("renames");
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	int count;

	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_DISABLE_PATHSPEC_MATCH |
		GIT_STATUS_OPT_EXCLUDE_SUBMODULES |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;

	count = 0;
	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__count, &count) );
	cl_assert_equal_i(0, count);

	cl_must_pass(p_unlink("renames/untimely.txt"));

	count = 0;
	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__count, &count) );
	cl_assert_equal_i(1, count);
}

void test_status_worktree__simple_delete_indexed(void)
{
	git_repository *repo = cl_git_sandbox_init("renames");
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	git_status_list *status;

	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED |
		GIT_STATUS_OPT_DISABLE_PATHSPEC_MATCH |
		GIT_STATUS_OPT_EXCLUDE_SUBMODULES |
		GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS;

	cl_git_pass(git_status_list_new(&status, repo, &opts));
	cl_assert_equal_sz(0, git_status_list_entrycount(status));
	git_status_list_free(status);

	cl_must_pass(p_unlink("renames/untimely.txt"));

	cl_git_pass(git_status_list_new(&status, repo, &opts));
	cl_assert_equal_sz(1, git_status_list_entrycount(status));
	cl_assert_equal_i(
		GIT_STATUS_WT_DELETED, git_status_byindex(status, 0)->status);
	git_status_list_free(status);
}

static const char *icase_paths[] = { "B", "c", "g", "H" };
static unsigned int icase_statuses[] = {
	GIT_STATUS_WT_MODIFIED, GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_MODIFIED, GIT_STATUS_WT_DELETED,
};

static const char *case_paths[] = { "B", "H", "c", "g" };
static unsigned int case_statuses[] = {
	GIT_STATUS_WT_MODIFIED, GIT_STATUS_WT_DELETED,
	GIT_STATUS_WT_DELETED, GIT_STATUS_WT_MODIFIED,
};

void test_status_worktree__sorting_by_case(void)
{
	git_repository *repo = cl_git_sandbox_init("icase");
	git_index *index;
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	bool native_ignore_case;
	status_entry_counts counts;

	cl_git_pass(git_repository_index(&index, repo));
	native_ignore_case =
		(git_index_caps(index) & GIT_INDEX_CAPABILITY_IGNORE_CASE) != 0;
	git_index_free(index);

	memset(&counts, 0, sizeof(counts));
	counts.expected_entry_count = 0;
	counts.expected_paths = NULL;
	counts.expected_statuses = NULL;
	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts));
	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);

	cl_git_rewritefile("icase/B", "new stuff");
	cl_must_pass(p_unlink("icase/c"));
	cl_git_rewritefile("icase/g", "new stuff");
	cl_must_pass(p_unlink("icase/H"));

	memset(&counts, 0, sizeof(counts));
	counts.expected_entry_count = 4;
	if (native_ignore_case) {
		counts.expected_paths = icase_paths;
		counts.expected_statuses = icase_statuses;
	} else {
		counts.expected_paths = case_paths;
		counts.expected_statuses = case_statuses;
	}
	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts));
	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);

	opts.flags = GIT_STATUS_OPT_SORT_CASE_SENSITIVELY;

	memset(&counts, 0, sizeof(counts));
	counts.expected_entry_count = 4;
	counts.expected_paths = case_paths;
	counts.expected_statuses = case_statuses;
	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts));
	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);

	opts.flags = GIT_STATUS_OPT_SORT_CASE_INSENSITIVELY;

	memset(&counts, 0, sizeof(counts));
	counts.expected_entry_count = 4;
	counts.expected_paths = icase_paths;
	counts.expected_statuses = icase_statuses;
	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts));
	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

void test_status_worktree__long_filenames(void)
{
	char path[260*4+1];
	const char *expected_paths[] = {path};
	const unsigned int expected_statuses[] = {GIT_STATUS_WT_NEW};

	git_repository *repo = cl_git_sandbox_init("empty_standard_repo");
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts = {0};

	/* Create directory with amazingly long filename */
	sprintf(path, "empty_standard_repo/%s", longname);
	cl_git_pass(git_futils_mkdir_r(path, 0777));
	sprintf(path, "empty_standard_repo/%s/foo", longname);
	cl_git_mkfile(path, "dummy");

	sprintf(path, "%s/foo", longname);
	counts.expected_entry_count = 1;
	counts.expected_paths = expected_paths;
	counts.expected_statuses = expected_statuses;

	opts.show = GIT_STATUS_SHOW_WORKDIR_ONLY;
	opts.flags = GIT_STATUS_OPT_DEFAULTS;

	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts) );
	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

/* The update stat cache tests mostly just mirror other tests and try
 * to make sure that updating the stat cache doesn't change the results
 * while reducing the amount of work that needs to be done
 */

static void check_status0(git_status_list *status)
{
	size_t i, max_i = git_status_list_entrycount(status);
	cl_assert_equal_sz(entry_count0, max_i);
	for (i = 0; i < max_i; ++i) {
		const git_status_entry *entry = git_status_byindex(status, i);
		cl_assert_equal_i(entry_statuses0[i], entry->status);
	}
}

void test_status_worktree__update_stat_cache_0(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	git_status_list *status;
	git_diff_perfdata perf = GIT_DIFF_PERFDATA_INIT;
	git_index *index;

	opts.flags = GIT_STATUS_OPT_DEFAULTS;

	cl_git_pass(git_status_list_new(&status, repo, &opts));
	check_status0(status);
	cl_git_pass(git_status_list_get_perfdata(&perf, status));
	cl_assert_equal_sz(13 + 3, perf.stat_calls);
	cl_assert_equal_sz(5, perf.oid_calculations);

	git_status_list_free(status);

	/* tick the index so we avoid recalculating racily-clean entries */
	cl_git_pass(git_repository_index__weakptr(&index, repo));
	tick_index(index);

	opts.flags |= GIT_STATUS_OPT_UPDATE_INDEX;

	cl_git_pass(git_status_list_new(&status, repo, &opts));
	check_status0(status);
	cl_git_pass(git_status_list_get_perfdata(&perf, status));
	cl_assert_equal_sz(13 + 3, perf.stat_calls);
	cl_assert_equal_sz(5, perf.oid_calculations);

	git_status_list_free(status);

	opts.flags &= ~GIT_STATUS_OPT_UPDATE_INDEX;

	/* tick again as the index updating from the previous diff might have reset the timestamp */
	tick_index(index);
	cl_git_pass(git_status_list_new(&status, repo, &opts));
	check_status0(status);
	cl_git_pass(git_status_list_get_perfdata(&perf, status));
	cl_assert_equal_sz(13 + 3, perf.stat_calls);
	cl_assert_equal_sz(0, perf.oid_calculations);

	git_status_list_free(status);
}

void test_status_worktree__unreadable(void)
{
#ifndef GIT_WIN32
	const char *expected_paths[] = { "no_permission/foo" };
	const unsigned int expected_statuses[] = {GIT_STATUS_WT_UNREADABLE};

	git_repository *repo = cl_git_sandbox_init("empty_standard_repo");
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts = {0};

	if (geteuid() == 0)
		cl_skip();

	/* Create directory with no read permission */
	cl_git_pass(git_futils_mkdir_r("empty_standard_repo/no_permission", 0777));
	cl_git_mkfile("empty_standard_repo/no_permission/foo", "dummy");
	p_chmod("empty_standard_repo/no_permission", 0644);

	counts.expected_entry_count = 1;
	counts.expected_paths = expected_paths;
	counts.expected_statuses = expected_statuses;

	opts.show = GIT_STATUS_SHOW_WORKDIR_ONLY;
	opts.flags = GIT_STATUS_OPT_DEFAULTS | GIT_STATUS_OPT_INCLUDE_UNREADABLE;

	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts) );

	/* Restore permissions so we can cleanup :) */
	p_chmod("empty_standard_repo/no_permission", 0777);

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
#else
	cl_skip();
#endif
}

void test_status_worktree__unreadable_not_included(void)
{
#ifndef GIT_WIN32
	const char *expected_paths[] = { "no_permission/" };
	const unsigned int expected_statuses[] = {GIT_STATUS_WT_NEW};

	git_repository *repo = cl_git_sandbox_init("empty_standard_repo");
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts = {0};

	/* Create directory with no read permission */
	cl_git_pass(git_futils_mkdir_r("empty_standard_repo/no_permission", 0777));
	cl_git_mkfile("empty_standard_repo/no_permission/foo", "dummy");
	p_chmod("empty_standard_repo/no_permission", 0644);

	counts.expected_entry_count = 1;
	counts.expected_paths = expected_paths;
	counts.expected_statuses = expected_statuses;

	opts.show = GIT_STATUS_SHOW_WORKDIR_ONLY;
	opts.flags = (GIT_STATUS_OPT_INCLUDE_IGNORED | GIT_STATUS_OPT_INCLUDE_UNTRACKED);

	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts) );

	/* Restore permissions so we can cleanup :) */
	p_chmod("empty_standard_repo/no_permission", 0777);

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
#else
	cl_skip();
#endif
}

void test_status_worktree__unreadable_as_untracked(void)
{
	const char *expected_paths[] = { "no_permission/foo" };
	const unsigned int expected_statuses[] = {GIT_STATUS_WT_NEW};

	git_repository *repo = cl_git_sandbox_init("empty_standard_repo");
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts = {0};

	/* Create directory with no read permission */
	cl_git_pass(git_futils_mkdir_r("empty_standard_repo/no_permission", 0777));
	cl_git_mkfile("empty_standard_repo/no_permission/foo", "dummy");
	p_chmod("empty_standard_repo/no_permission", 0644);

	counts.expected_entry_count = 1;
	counts.expected_paths = expected_paths;
	counts.expected_statuses = expected_statuses;

	opts.show = GIT_STATUS_SHOW_WORKDIR_ONLY;
	opts.flags = GIT_STATUS_OPT_DEFAULTS |
		GIT_STATUS_OPT_INCLUDE_UNREADABLE |
		GIT_STATUS_OPT_INCLUDE_UNREADABLE_AS_UNTRACKED;

	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts) );

	/* Restore permissions so we can cleanup :) */
	p_chmod("empty_standard_repo/no_permission", 0777);

	cl_assert_equal_i(counts.expected_entry_count, counts.entry_count);
	cl_assert_equal_i(0, counts.wrong_status_flags_count);
	cl_assert_equal_i(0, counts.wrong_sorted_path);
}

void test_status_worktree__update_index_with_symlink_doesnt_change_mode(void)
{
	git_repository *repo = cl_git_sandbox_init("testrepo");
	git_reference *head;
	git_object *head_object;
	git_index *index;
	const git_index_entry *idx_entry;
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	status_entry_counts counts = {0};
	const char *expected_paths[] = { "README" };
	const unsigned int expected_statuses[] = {GIT_STATUS_WT_NEW};

	opts.show = GIT_STATUS_SHOW_INDEX_AND_WORKDIR;
	opts.flags = GIT_STATUS_OPT_DEFAULTS | GIT_STATUS_OPT_UPDATE_INDEX;

	cl_git_pass(git_repository_head(&head, repo));
	cl_git_pass(git_reference_peel(&head_object, head, GIT_OBJECT_COMMIT));

	cl_git_pass(git_reset(repo, head_object, GIT_RESET_HARD, NULL));

	cl_git_rewritefile("testrepo/README", "This was rewritten.");

	/* this status rewrites the index because we have changed the
	 * contents of a tracked file
	 */
	counts.expected_entry_count = 1;
	counts.expected_paths = expected_paths;
	counts.expected_statuses = expected_statuses;

	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__normal, &counts));
	cl_assert_equal_i(1, counts.entry_count);

	/* now ensure that the status's rewrite of the index did not screw
	 * up the mode of the symlink `link_to_new.txt`, particularly
	 * on platforms that don't support symlinks
	 */
	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_read(index, true));

	cl_assert(idx_entry = git_index_get_bypath(index, "link_to_new.txt", 0));
	cl_assert(S_ISLNK(idx_entry->mode));

	git_index_free(index);
	git_object_free(head_object);
	git_reference_free(head);
}

static const char *testrepo2_subdir_paths[] = {
		"subdir/README",
		"subdir/new.txt",
		"subdir/subdir2/README",
		"subdir/subdir2/new.txt",
};

static const char *testrepo2_subdir_paths_icase[] = {
		"subdir/new.txt",
		"subdir/README",
		"subdir/subdir2/new.txt",
		"subdir/subdir2/README"
};

void test_status_worktree__with_directory_in_pathlist(void)
{
	git_repository *repo = cl_git_sandbox_init("testrepo2");
	git_index *index;
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	git_status_list *statuslist;
	const git_status_entry *status;
	size_t i, entrycount;
	bool native_ignore_case;
	char *subdir_path = "subdir";

	cl_git_pass(git_repository_index(&index, repo));
	native_ignore_case =
			(git_index_caps(index) & GIT_INDEX_CAPABILITY_IGNORE_CASE) != 0;
	git_index_free(index);

	opts.pathspec.strings = &subdir_path;
	opts.pathspec.count = 1;
	opts.flags =
			GIT_STATUS_OPT_DEFAULTS |
			GIT_STATUS_OPT_INCLUDE_UNMODIFIED |
			GIT_STATUS_OPT_DISABLE_PATHSPEC_MATCH;

	opts.show = GIT_STATUS_SHOW_WORKDIR_ONLY;
	git_status_list_new(&statuslist, repo, &opts);

	entrycount = git_status_list_entrycount(statuslist);
	cl_assert_equal_i(4, entrycount);

	for (i = 0; i < entrycount; i++) {
		status = git_status_byindex(statuslist, i);
		cl_assert_equal_i(0, status->status);
		cl_assert_equal_s(native_ignore_case ?
			testrepo2_subdir_paths_icase[i] :
			testrepo2_subdir_paths[i],
			status->index_to_workdir->old_file.path);
	}

	git_status_list_free(statuslist);

	opts.show = GIT_STATUS_SHOW_INDEX_ONLY;
	git_status_list_new(&statuslist, repo, &opts);

	entrycount = git_status_list_entrycount(statuslist);
	cl_assert_equal_i(4, entrycount);

	for (i = 0; i < entrycount; i++) {
		status = git_status_byindex(statuslist, i);
		cl_assert_equal_i(0, status->status);
		cl_assert_equal_s(native_ignore_case ?
			testrepo2_subdir_paths_icase[i] :
			testrepo2_subdir_paths[i],
			status->head_to_index->old_file.path);
	}

	git_status_list_free(statuslist);

	opts.show = GIT_STATUS_SHOW_INDEX_AND_WORKDIR;
	git_status_list_new(&statuslist, repo, &opts);

	entrycount = git_status_list_entrycount(statuslist);
	cl_assert_equal_i(4, entrycount);

	for (i = 0; i < entrycount; i++) {
		status = git_status_byindex(statuslist, i);
		cl_assert_equal_i(0, status->status);
		cl_assert_equal_s(native_ignore_case ?
			testrepo2_subdir_paths_icase[i] :
			testrepo2_subdir_paths[i],
			status->index_to_workdir->old_file.path);
	}

	git_status_list_free(statuslist);
}

void test_status_worktree__at_head_parent(void)
{
	git_repository *repo = cl_git_sandbox_init("empty_standard_repo");
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	git_status_list *statuslist;
	git_tree *parent_tree;
	const git_status_entry *status;

	cl_git_mkfile("empty_standard_repo/file1", "ping");
	stage_and_commit(repo, "file1");

	cl_git_pass(git_repository_head_tree(&parent_tree, repo));

	cl_git_mkfile("empty_standard_repo/file2", "pong");
	stage_and_commit(repo, "file2");

	cl_git_rewritefile("empty_standard_repo/file2", "pyng");

	opts.show = GIT_STATUS_SHOW_INDEX_AND_WORKDIR;
	opts.baseline = parent_tree;
	cl_git_pass(git_status_list_new(&statuslist, repo, &opts));

	cl_assert_equal_sz(1, git_status_list_entrycount(statuslist));
	status = git_status_byindex(statuslist, 0);
	cl_assert(status != NULL);
	cl_assert_equal_s("file2", status->index_to_workdir->old_file.path);
	cl_assert_equal_i(GIT_STATUS_WT_MODIFIED | GIT_STATUS_INDEX_NEW, status->status);

	git_tree_free(parent_tree);
	git_status_list_free(statuslist);
}
