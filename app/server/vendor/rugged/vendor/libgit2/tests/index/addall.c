#include "clar_libgit2.h"
#include "../status/status_helpers.h"
#include "posix.h"
#include "fileops.h"

static git_repository *g_repo = NULL;
#define TEST_DIR "addall"

void test_index_addall__initialize(void)
{
}

void test_index_addall__cleanup(void)
{
	git_repository_free(g_repo);
	g_repo = NULL;

	cl_fixture_cleanup(TEST_DIR);
}

#define STATUS_INDEX_FLAGS \
	(GIT_STATUS_INDEX_NEW | GIT_STATUS_INDEX_MODIFIED | \
	 GIT_STATUS_INDEX_DELETED | GIT_STATUS_INDEX_RENAMED | \
	 GIT_STATUS_INDEX_TYPECHANGE)

#define STATUS_WT_FLAGS \
	(GIT_STATUS_WT_NEW | GIT_STATUS_WT_MODIFIED | \
	 GIT_STATUS_WT_DELETED | GIT_STATUS_WT_TYPECHANGE | \
	 GIT_STATUS_WT_RENAMED)

typedef struct {
	size_t index_adds;
	size_t index_dels;
	size_t index_mods;
	size_t wt_adds;
	size_t wt_dels;
	size_t wt_mods;
	size_t ignores;
} index_status_counts;

static int index_status_cb(
	const char *path, unsigned int status_flags, void *payload)
{
	index_status_counts *vals = payload;

	/* cb_status__print(path, status_flags, NULL); */

	GIT_UNUSED(path);

	if (status_flags & GIT_STATUS_INDEX_NEW)
		vals->index_adds++;
	if (status_flags & GIT_STATUS_INDEX_MODIFIED)
		vals->index_mods++;
	if (status_flags & GIT_STATUS_INDEX_DELETED)
		vals->index_dels++;
	if (status_flags & GIT_STATUS_INDEX_TYPECHANGE)
		vals->index_mods++;

	if (status_flags & GIT_STATUS_WT_NEW)
		vals->wt_adds++;
	if (status_flags & GIT_STATUS_WT_MODIFIED)
		vals->wt_mods++;
	if (status_flags & GIT_STATUS_WT_DELETED)
		vals->wt_dels++;
	if (status_flags & GIT_STATUS_WT_TYPECHANGE)
		vals->wt_mods++;

	if (status_flags & GIT_STATUS_IGNORED)
		vals->ignores++;

	return 0;
}

static void check_status_at_line(
	git_repository *repo,
	size_t index_adds, size_t index_dels, size_t index_mods,
	size_t wt_adds, size_t wt_dels, size_t wt_mods, size_t ignores,
	const char *file, int line)
{
	index_status_counts vals;

	memset(&vals, 0, sizeof(vals));

	cl_git_pass(git_status_foreach(repo, index_status_cb, &vals));

	clar__assert_equal(
		file,line,"wrong index adds", 1, "%"PRIuZ, index_adds, vals.index_adds);
	clar__assert_equal(
		file,line,"wrong index dels", 1, "%"PRIuZ, index_dels, vals.index_dels);
	clar__assert_equal(
		file,line,"wrong index mods", 1, "%"PRIuZ, index_mods, vals.index_mods);
	clar__assert_equal(
		file,line,"wrong workdir adds", 1, "%"PRIuZ, wt_adds, vals.wt_adds);
	clar__assert_equal(
		file,line,"wrong workdir dels", 1, "%"PRIuZ, wt_dels, vals.wt_dels);
	clar__assert_equal(
		file,line,"wrong workdir mods", 1, "%"PRIuZ, wt_mods, vals.wt_mods);
	clar__assert_equal(
		file,line,"wrong ignores", 1, "%"PRIuZ, ignores, vals.ignores);
}

#define check_status(R,IA,ID,IM,WA,WD,WM,IG) \
	check_status_at_line(R,IA,ID,IM,WA,WD,WM,IG,__FILE__,__LINE__)

static void check_stat_data(git_index *index, const char *path, bool match)
{
	const git_index_entry *entry;
	struct stat st;

	cl_must_pass(p_lstat(path, &st));

	/* skip repo base dir name */
	while (*path != '/')
		++path;
	++path;

	entry = git_index_get_bypath(index, path, 0);
	cl_assert(entry);

	if (match) {
		cl_assert(st.st_ctime == entry->ctime.seconds);
		cl_assert(st.st_mtime == entry->mtime.seconds);
		cl_assert(st.st_size == entry->file_size);
		cl_assert(st.st_uid  == entry->uid);
		cl_assert(st.st_gid  == entry->gid);
		cl_assert_equal_i_fmt(
			GIT_MODE_TYPE(st.st_mode), GIT_MODE_TYPE(entry->mode), "%07o");
		if (cl_is_chmod_supported())
			cl_assert_equal_b(
				GIT_PERMS_IS_EXEC(st.st_mode), GIT_PERMS_IS_EXEC(entry->mode));
	} else {
		/* most things will still match */
		cl_assert(st.st_size != entry->file_size);
		/* would check mtime, but with second resolution it won't work :( */
	}
}

static void addall_create_test_repo(bool check_every_step)
{
	cl_git_pass(git_repository_init(&g_repo, TEST_DIR, false));
	if (check_every_step)
		check_status(g_repo, 0, 0, 0, 0, 0, 0, 0);

	cl_git_mkfile(TEST_DIR "/file.foo", "a file");
	if (check_every_step)
		check_status(g_repo, 0, 0, 0, 1, 0, 0, 0);

	cl_git_mkfile(TEST_DIR "/.gitignore", "*.foo\n");
	if (check_every_step)
		check_status(g_repo, 0, 0, 0, 1, 0, 0, 1);

	cl_git_mkfile(TEST_DIR "/file.bar", "another file");
	if (check_every_step)
		check_status(g_repo, 0, 0, 0, 2, 0, 0, 1);
}

void test_index_addall__repo_lifecycle(void)
{
	int error;
	git_index *index;
	git_strarray paths = { NULL, 0 };
	char *strs[1];

	addall_create_test_repo(true);

	cl_git_pass(git_repository_index(&index, g_repo));

	strs[0] = "file.*";
	paths.strings = strs;
	paths.count   = 1;

	cl_git_pass(git_index_add_all(index, &paths, 0, NULL, NULL));
	check_stat_data(index, TEST_DIR "/file.bar", true);
	check_status(g_repo, 1, 0, 0, 1, 0, 0, 1);

	cl_git_rewritefile(TEST_DIR "/file.bar", "new content for file");
	check_stat_data(index, TEST_DIR "/file.bar", false);
	check_status(g_repo, 1, 0, 0, 1, 0, 1, 1);

	cl_git_mkfile(TEST_DIR "/file.zzz", "yet another one");
	cl_git_mkfile(TEST_DIR "/other.zzz", "yet another one");
	cl_git_mkfile(TEST_DIR "/more.zzz", "yet another one");
	check_status(g_repo, 1, 0, 0, 4, 0, 1, 1);

	cl_git_pass(git_index_update_all(index, NULL, NULL, NULL));
	check_stat_data(index, TEST_DIR "/file.bar", true);
	check_status(g_repo, 1, 0, 0, 4, 0, 0, 1);

	cl_git_pass(git_index_add_all(index, &paths, 0, NULL, NULL));
	check_stat_data(index, TEST_DIR "/file.zzz", true);
	check_status(g_repo, 2, 0, 0, 3, 0, 0, 1);

	cl_repo_commit_from_index(NULL, g_repo, NULL, 0, "first commit");
	check_status(g_repo, 0, 0, 0, 3, 0, 0, 1);

	/* attempt to add an ignored file - does nothing */
	strs[0] = "file.foo";
	cl_git_pass(git_index_add_all(index, &paths, 0, NULL, NULL));
	check_status(g_repo, 0, 0, 0, 3, 0, 0, 1);

	/* add with check - should generate error */
	error = git_index_add_all(
		index, &paths, GIT_INDEX_ADD_CHECK_PATHSPEC, NULL, NULL);
	cl_assert_equal_i(GIT_EINVALIDSPEC, error);
	check_status(g_repo, 0, 0, 0, 3, 0, 0, 1);

	/* add with force - should allow */
	cl_git_pass(git_index_add_all(
		index, &paths, GIT_INDEX_ADD_FORCE, NULL, NULL));
	check_stat_data(index, TEST_DIR "/file.foo", true);
	check_status(g_repo, 1, 0, 0, 3, 0, 0, 0);

	/* now it's in the index, so regular add should work */
	cl_git_rewritefile(TEST_DIR "/file.foo", "new content for file");
	check_stat_data(index, TEST_DIR "/file.foo", false);
	check_status(g_repo, 1, 0, 0, 3, 0, 1, 0);

	cl_git_pass(git_index_add_all(index, &paths, 0, NULL, NULL));
	check_stat_data(index, TEST_DIR "/file.foo", true);
	check_status(g_repo, 1, 0, 0, 3, 0, 0, 0);

	cl_git_pass(git_index_add_bypath(index, "more.zzz"));
	check_stat_data(index, TEST_DIR "/more.zzz", true);
	check_status(g_repo, 2, 0, 0, 2, 0, 0, 0);

	cl_git_rewritefile(TEST_DIR "/file.zzz", "new content for file");
	check_status(g_repo, 2, 0, 0, 2, 0, 1, 0);

	cl_git_pass(git_index_add_bypath(index, "file.zzz"));
	check_stat_data(index, TEST_DIR "/file.zzz", true);
	check_status(g_repo, 2, 0, 1, 2, 0, 0, 0);

	strs[0] = "*.zzz";
	cl_git_pass(git_index_remove_all(index, &paths, NULL, NULL));
	check_status(g_repo, 1, 1, 0, 4, 0, 0, 0);

	cl_git_pass(git_index_add_bypath(index, "file.zzz"));
	check_status(g_repo, 1, 0, 1, 3, 0, 0, 0);

	cl_repo_commit_from_index(NULL, g_repo, NULL, 0, "second commit");
	check_status(g_repo, 0, 0, 0, 3, 0, 0, 0);

	cl_must_pass(p_unlink(TEST_DIR "/file.zzz"));
	check_status(g_repo, 0, 0, 0, 3, 1, 0, 0);

	/* update_all should be able to remove entries */
	cl_git_pass(git_index_update_all(index, NULL, NULL, NULL));
	check_status(g_repo, 0, 1, 0, 3, 0, 0, 0);

	strs[0] = "*";
	cl_git_pass(git_index_add_all(index, &paths, 0, NULL, NULL));
	check_status(g_repo, 3, 1, 0, 0, 0, 0, 0);

	/* must be able to remove at any position while still updating other files */
	cl_must_pass(p_unlink(TEST_DIR "/.gitignore"));
	cl_git_rewritefile(TEST_DIR "/file.zzz", "reconstructed file");
	cl_git_rewritefile(TEST_DIR "/more.zzz", "altered file reality");
	check_status(g_repo, 3, 1, 0, 1, 1, 1, 0);

	cl_git_pass(git_index_update_all(index, NULL, NULL, NULL));
	check_status(g_repo, 2, 1, 0, 1, 0, 0, 0);
	/* this behavior actually matches 'git add -u' where "file.zzz" has
	 * been removed from the index, so when you go to update, even though
	 * it exists in the HEAD, it is not re-added to the index, leaving it
	 * as a DELETE when comparing HEAD to index and as an ADD comparing
	 * index to worktree
	 */

	git_index_free(index);
}

static int addall_match_prefix(
	const char *path, const char *matched_pathspec, void *payload)
{
	GIT_UNUSED(matched_pathspec);
	return !git__prefixcmp(path, payload) ? 0 : 1;
}

static int addall_match_suffix(
	const char *path, const char *matched_pathspec, void *payload)
{
	GIT_UNUSED(matched_pathspec);
	return !git__suffixcmp(path, payload) ? 0 : 1;
}

static int addall_cancel_at(
	const char *path, const char *matched_pathspec, void *payload)
{
	GIT_UNUSED(matched_pathspec);
	return !strcmp(path, payload) ? -123 : 0;
}

void test_index_addall__callback_filtering(void)
{
	git_index *index;

	addall_create_test_repo(false);

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(
		git_index_add_all(index, NULL, 0, addall_match_prefix, "file."));
	check_stat_data(index, TEST_DIR "/file.bar", true);
	check_status(g_repo, 1, 0, 0, 1, 0, 0, 1);

	cl_git_mkfile(TEST_DIR "/file.zzz", "yet another one");
	cl_git_mkfile(TEST_DIR "/more.zzz", "yet another one");
	cl_git_mkfile(TEST_DIR "/other.zzz", "yet another one");

	cl_git_pass(git_index_update_all(index, NULL, NULL, NULL));
	check_stat_data(index, TEST_DIR "/file.bar", true);
	check_status(g_repo, 1, 0, 0, 4, 0, 0, 1);

	cl_git_pass(
		git_index_add_all(index, NULL, 0, addall_match_prefix, "other"));
	check_stat_data(index, TEST_DIR "/other.zzz", true);
	check_status(g_repo, 2, 0, 0, 3, 0, 0, 1);

	cl_git_pass(
		git_index_add_all(index, NULL, 0, addall_match_suffix, ".zzz"));
	check_status(g_repo, 4, 0, 0, 1, 0, 0, 1);

	cl_git_pass(
		git_index_remove_all(index, NULL, addall_match_suffix, ".zzz"));
	check_status(g_repo, 1, 0, 0, 4, 0, 0, 1);

	cl_git_fail_with(
		git_index_add_all(index, NULL, 0, addall_cancel_at, "more.zzz"), -123);
	check_status(g_repo, 3, 0, 0, 2, 0, 0, 1);

	cl_git_fail_with(
		git_index_add_all(index, NULL, 0, addall_cancel_at, "other.zzz"), -123);
	check_status(g_repo, 4, 0, 0, 1, 0, 0, 1);

	cl_git_pass(
		git_index_add_all(index, NULL, 0, addall_match_suffix, ".zzz"));
	check_status(g_repo, 5, 0, 0, 0, 0, 0, 1);

	cl_must_pass(p_unlink(TEST_DIR "/file.zzz"));
	cl_must_pass(p_unlink(TEST_DIR "/more.zzz"));
	cl_must_pass(p_unlink(TEST_DIR "/other.zzz"));

	cl_git_fail_with(
		git_index_update_all(index, NULL, addall_cancel_at, "more.zzz"), -123);
	/* file.zzz removed from index (so Index Adds 5 -> 4) and
	 * more.zzz + other.zzz removed (so Worktree Dels 0 -> 2) */
	check_status(g_repo, 4, 0, 0, 0, 2, 0, 1);

	cl_git_fail_with(
		git_index_update_all(index, NULL, addall_cancel_at, "other.zzz"), -123);
	/* more.zzz removed from index (so Index Adds 4 -> 3) and
	 * Just other.zzz removed (so Worktree Dels 2 -> 1) */
	check_status(g_repo, 3, 0, 0, 0, 1, 0, 1);

	git_index_free(index);
}
