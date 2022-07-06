#include "clar_libgit2.h"
#include "../checkout/checkout_helpers.h"

#include "buffer.h"
#include "index.h"
#include "repository.h"

static git_repository *g_repo;

void test_index_racy__initialize(void)
{
	cl_git_pass(git_repository_init(&g_repo, "diff_racy", false));
}

void test_index_racy__cleanup(void)
{
	git_repository_free(g_repo);
	g_repo = NULL;

	cl_fixture_cleanup("diff_racy");
}

void test_index_racy__diff(void)
{
	git_index *index;
	git_diff *diff;
	git_buf path = GIT_BUF_INIT;

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(g_repo), "A"));
	cl_git_mkfile(path.ptr, "A");

	/* Put 'A' into the index */
	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_add_bypath(index, "A"));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_diff_index_to_workdir(&diff, g_repo, index, NULL));
	cl_assert_equal_i(0, git_diff_num_deltas(diff));
	git_diff_free(diff);

	/* Change its contents quickly, so we get the same timestamp */
	cl_git_mkfile(path.ptr, "B");

	cl_git_pass(git_diff_index_to_workdir(&diff, g_repo, index, NULL));
	cl_assert_equal_i(1, git_diff_num_deltas(diff));

	git_index_free(index);
	git_diff_free(diff);
	git_buf_dispose(&path);
}

void test_index_racy__write_index_just_after_file(void)
{
	git_index *index;
	git_diff *diff;
	git_buf path = GIT_BUF_INIT;
	struct p_timeval times[2];

	/* Make sure we do have a timestamp */
	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(g_repo), "A"));
	cl_git_mkfile(path.ptr, "A");
	/* Force the file's timestamp to be a second after we wrote the index */
	times[0].tv_sec = index->stamp.mtime.tv_sec + 1;
	times[0].tv_usec = index->stamp.mtime.tv_nsec / 1000;
	times[1].tv_sec = index->stamp.mtime.tv_sec + 1;
	times[1].tv_usec = index->stamp.mtime.tv_nsec / 1000;
	cl_git_pass(p_utimes(path.ptr, times));

	/*
	 * Put 'A' into the index, the size field will be filled,
	 * because the index' on-disk timestamp does not match the
	 * file's timestamp.
	 */
	cl_git_pass(git_index_add_bypath(index, "A"));
	cl_git_pass(git_index_write(index));

	cl_git_mkfile(path.ptr, "B");
	/*
	 * Pretend this index' modification happened a second after the
	 * file update, and rewrite the file in that same second.
	 */
	times[0].tv_sec = index->stamp.mtime.tv_sec + 2;
	times[0].tv_usec = index->stamp.mtime.tv_nsec / 1000;
	times[1].tv_sec = index->stamp.mtime.tv_sec + 2;
	times[0].tv_usec = index->stamp.mtime.tv_nsec / 1000;

	cl_git_pass(p_utimes(git_index_path(index), times));
	cl_git_pass(p_utimes(path.ptr, times));

	cl_git_pass(git_index_read(index, true));

	cl_git_pass(git_diff_index_to_workdir(&diff, g_repo, index, NULL));
	cl_assert_equal_i(1, git_diff_num_deltas(diff));

	git_buf_dispose(&path);
	git_diff_free(diff);
	git_index_free(index);
}


static void setup_race(void)
{
	git_buf path = GIT_BUF_INIT;
	git_index *index;
	git_index_entry *entry;
	struct stat st;

	/* Make sure we do have a timestamp */
	cl_git_pass(git_repository_index__weakptr(&index, g_repo));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(g_repo), "A"));

	cl_git_mkfile(path.ptr, "A");
	cl_git_pass(git_index_add_bypath(index, "A"));

	cl_git_mkfile(path.ptr, "B");
	cl_git_pass(git_index_write(index));

	cl_git_mkfile(path.ptr, "");

	cl_git_pass(p_stat(path.ptr, &st));
	cl_assert(entry = (git_index_entry *)git_index_get_bypath(index, "A", 0));

	/* force a race */
	entry->mtime.seconds = (int32_t)st.st_mtime;
	entry->mtime.nanoseconds = (int32_t)st.st_mtime_nsec;

	git_buf_dispose(&path);
}

void test_index_racy__smudges_index_entry_on_save(void)
{
	git_index *index;
	const git_index_entry *entry;

	setup_race();

	/* write the index, which will smudge anything that had the same timestamp
	 * as the index when the index was loaded.  that way future loads of the
	 * index (with the new timestamp) will know that these files were not
	 * clean.
	 */

	cl_git_pass(git_repository_index__weakptr(&index, g_repo));
	cl_git_pass(git_index_write(index));

	cl_assert(entry = git_index_get_bypath(index, "A", 0));
	cl_assert_equal_i(0, entry->file_size);
}

void test_index_racy__detects_diff_of_change_in_identical_timestamp(void)
{
	git_index *index;
	git_diff *diff;

	cl_git_pass(git_repository_index__weakptr(&index, g_repo));

	setup_race();

	cl_git_pass(git_diff_index_to_workdir(&diff, g_repo, index, NULL));
	cl_assert_equal_i(1, git_diff_num_deltas(diff));

	git_diff_free(diff);
}

static void setup_uptodate_files(void)
{
	git_buf path = GIT_BUF_INIT;
	git_index *index;
	const git_index_entry *a_entry;
	git_index_entry new_entry = {{0}};

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(g_repo), "A"));
	cl_git_mkfile(path.ptr, "A");

	/* Put 'A' into the index */
	cl_git_pass(git_index_add_bypath(index, "A"));

	cl_assert((a_entry = git_index_get_bypath(index, "A", 0)));

	/* Put 'B' into the index */
	new_entry.path = "B";
	new_entry.mode = GIT_FILEMODE_BLOB;
	git_oid_cpy(&new_entry.id, &a_entry->id);
	cl_git_pass(git_index_add(index, &new_entry));

	/* Put 'C' into the index */
	new_entry.path = "C";
	new_entry.mode = GIT_FILEMODE_BLOB;
	cl_git_pass(git_index_add_from_buffer(index, &new_entry, "hello!\n", 7));

	git_index_free(index);
	git_buf_dispose(&path);
}

void test_index_racy__adding_to_index_is_uptodate(void)
{
	git_index *index;
	const git_index_entry *entry;

	setup_uptodate_files();

	cl_git_pass(git_repository_index(&index, g_repo));

	/* ensure that they're all uptodate */
	cl_assert((entry = git_index_get_bypath(index, "A", 0)));
	cl_assert_equal_i(GIT_INDEX_ENTRY_UPTODATE, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	cl_assert((entry = git_index_get_bypath(index, "B", 0)));
	cl_assert_equal_i(GIT_INDEX_ENTRY_UPTODATE, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	cl_assert((entry = git_index_get_bypath(index, "C", 0)));
	cl_assert_equal_i(GIT_INDEX_ENTRY_UPTODATE, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	cl_git_pass(git_index_write(index));

	git_index_free(index);
}

void test_index_racy__reading_clears_uptodate_bit(void)
{
	git_index *index;
	const git_index_entry *entry;

	setup_uptodate_files();

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_index_read(index, true));

	/* ensure that no files are uptodate */
	cl_assert((entry = git_index_get_bypath(index, "A", 0)));
	cl_assert_equal_i(0, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	cl_assert((entry = git_index_get_bypath(index, "B", 0)));
	cl_assert_equal_i(0, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	cl_assert((entry = git_index_get_bypath(index, "C", 0)));
	cl_assert_equal_i(0, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	git_index_free(index);
}

void test_index_racy__read_tree_clears_uptodate_bit(void)
{
	git_index *index;
	git_tree *tree;
	const git_index_entry *entry;
	git_oid id;

	setup_uptodate_files();

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_write_tree_to(&id, index, g_repo));
	cl_git_pass(git_tree_lookup(&tree, g_repo, &id));
	cl_git_pass(git_index_read_tree(index, tree));

	/* ensure that no files are uptodate */
	cl_assert((entry = git_index_get_bypath(index, "A", 0)));
	cl_assert_equal_i(0, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	cl_assert((entry = git_index_get_bypath(index, "B", 0)));
	cl_assert_equal_i(0, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	cl_assert((entry = git_index_get_bypath(index, "C", 0)));
	cl_assert_equal_i(0, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	git_tree_free(tree);
	git_index_free(index);
}

void test_index_racy__read_index_smudges(void)
{
	git_index *index, *newindex;
	const git_index_entry *entry;

	/* if we are reading an index into our new index, ensure that any
	 * racy entries in the index that we're reading are smudged so that
	 * we don't propagate their timestamps without further investigation.
	 */
	setup_race();

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_new(&newindex));
	cl_git_pass(git_index_read_index(newindex, index));

	cl_assert(entry = git_index_get_bypath(newindex, "A", 0));
	cl_assert_equal_i(0, entry->file_size);

	git_index_free(index);
	git_index_free(newindex);
}

void test_index_racy__read_index_clears_uptodate_bit(void)
{
	git_index *index, *newindex;
	const git_index_entry *entry;

	setup_uptodate_files();

	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_new(&newindex));
	cl_git_pass(git_index_read_index(newindex, index));

	/* ensure that files brought in from the other index are not uptodate */
	cl_assert((entry = git_index_get_bypath(newindex, "A", 0)));
	cl_assert_equal_i(0, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	cl_assert((entry = git_index_get_bypath(newindex, "B", 0)));
	cl_assert_equal_i(0, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	cl_assert((entry = git_index_get_bypath(newindex, "C", 0)));
	cl_assert_equal_i(0, (entry->flags_extended & GIT_INDEX_ENTRY_UPTODATE));

	git_index_free(index);
	git_index_free(newindex);
}
