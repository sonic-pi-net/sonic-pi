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
	git_buf_free(&path);
}

void test_index_racy__write_index_just_after_file(void)
{
	git_index *index;
	git_diff *diff;
	git_buf path = GIT_BUF_INIT;
	struct timeval times[2];

	/* Make sure we do have a timestamp */
	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(g_repo), "A"));
	cl_git_mkfile(path.ptr, "A");
	/* Force the file's timestamp to be a second after we wrote the index */
	times[0].tv_sec = index->stamp.mtime + 1;
	times[0].tv_usec = 0;
	times[1].tv_sec = index->stamp.mtime + 1;
	times[1].tv_usec = 0;
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
	 * Pretend this index' modification happend a second after the
	 * file update, and rewrite the file in that same second.
	 */
	times[0].tv_sec = index->stamp.mtime + 2;
	times[0].tv_usec = 0;
	times[1].tv_sec = index->stamp.mtime + 2;
	times[0].tv_usec = 0;

	cl_git_pass(p_utimes(git_index_path(index), times));
	cl_git_pass(p_utimes(path.ptr, times));

	cl_git_pass(git_index_read(index, true));

	cl_git_pass(git_diff_index_to_workdir(&diff, g_repo, index, NULL));
	cl_assert_equal_i(1, git_diff_num_deltas(diff));

	git_buf_free(&path);
	git_diff_free(diff);
	git_index_free(index);
}

void test_index_racy__empty_file_after_smudge(void)
{
	git_index *index;
	git_diff *diff;
	git_buf path = GIT_BUF_INIT;
	int i, found_race = 0;
	const git_index_entry *entry;

	/* Make sure we do have a timestamp */
	cl_git_pass(git_repository_index__weakptr(&index, g_repo));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_buf_joinpath(&path, git_repository_workdir(g_repo), "A"));

	/* Make sure writing the file, adding and rewriting happen in the same second */
	for (i = 0; i < 10; i++) {
		struct stat st;
		cl_git_mkfile(path.ptr, "A");

		cl_git_pass(git_index_add_bypath(index, "A"));
		cl_git_mkfile(path.ptr, "B");
		cl_git_pass(git_index_write(index));

		cl_git_mkfile(path.ptr, "");

		cl_git_pass(p_stat(path.ptr, &st));
		cl_assert(entry = git_index_get_bypath(index, "A", 0));
		if (entry->mtime.seconds == (int32_t) st.st_mtime) {
			found_race = 1;
			break;
		}

	}

	if (!found_race)
		cl_fail("failed to find race after 10 attempts");

	cl_assert_equal_i(0, entry->file_size);

	cl_git_pass(git_diff_index_to_workdir(&diff, g_repo, index, NULL));
	cl_assert_equal_i(1, git_diff_num_deltas(diff));

	git_buf_free(&path);
	git_diff_free(diff);
}
