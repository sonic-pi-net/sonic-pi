#include "clar_libgit2.h"
#include "mwindow.h"

#include <git2.h>
#include "git2/sys/commit.h"
#include "git2/sys/mempack.h"

static size_t expected_open_mwindow_files = 0;
static size_t original_mwindow_file_limit = 0;

extern git_mutex git__mwindow_mutex;
extern git_mwindow_ctl git_mwindow__mem_ctl;

void test_pack_filelimit__initialize_tiny(void)
{
	expected_open_mwindow_files = 1;
	cl_git_pass(git_libgit2_opts(GIT_OPT_GET_MWINDOW_FILE_LIMIT, &original_mwindow_file_limit));
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_MWINDOW_FILE_LIMIT, expected_open_mwindow_files));
}

void test_pack_filelimit__initialize_medium(void)
{
	expected_open_mwindow_files = 10;
	cl_git_pass(git_libgit2_opts(GIT_OPT_GET_MWINDOW_FILE_LIMIT, &original_mwindow_file_limit));
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_MWINDOW_FILE_LIMIT, expected_open_mwindow_files));
}

void test_pack_filelimit__initialize_unlimited(void)
{
	expected_open_mwindow_files = 15;
	cl_git_pass(git_libgit2_opts(GIT_OPT_GET_MWINDOW_FILE_LIMIT, &original_mwindow_file_limit));
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_MWINDOW_FILE_LIMIT, 0));
}

void test_pack_filelimit__cleanup(void)
{
	git_str path = GIT_STR_INIT;
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_MWINDOW_FILE_LIMIT, original_mwindow_file_limit));

	cl_git_pass(git_str_joinpath(&path, clar_sandbox_path(), "repo.git"));
	cl_fixture_cleanup(path.ptr);
	git_str_dispose(&path);
}

/*
 * Create a packfile with one commit, one tree, and two blobs. The first blob
 * (README.md) has the same content in all commits, but the second one
 * (file.txt) has a different content in each commit.
 */
static void create_packfile_commit(
		git_repository *repo,
		git_oid *out_commit_id,
		git_oid *parent_id,
		size_t commit_index,
		size_t commit_count)
{
	git_str file_contents = GIT_STR_INIT;
	git_treebuilder *treebuilder;
	git_packbuilder *packbuilder;
	git_signature *s;
	git_oid oid, tree_id, commit_id;
	const git_oid *parents[] = { parent_id };
	size_t parent_count = parent_id ? 1 : 0;

	cl_git_pass(git_treebuilder_new(&treebuilder, repo, NULL));

	cl_git_pass(git_blob_create_from_buffer(&oid, repo, "", 0));
	cl_git_pass(git_treebuilder_insert(NULL, treebuilder, "README.md", &oid, 0100644));

	cl_git_pass(git_str_printf(&file_contents, "Commit %zd/%zd", commit_index, commit_count));
	cl_git_pass(git_blob_create_from_buffer(&oid, repo, file_contents.ptr, file_contents.size));
	cl_git_pass(git_treebuilder_insert(NULL, treebuilder, "file.txt", &oid, 0100644));

	cl_git_pass(git_treebuilder_write(&tree_id, treebuilder));
	cl_git_pass(git_signature_now(&s, "alice", "alice@example.com"));
	cl_git_pass(git_commit_create_from_ids(&commit_id, repo, "refs/heads/master", s, s,
			NULL, file_contents.ptr, &tree_id, parent_count, parents));

	cl_git_pass(git_packbuilder_new(&packbuilder, repo));
	cl_git_pass(git_packbuilder_insert_commit(packbuilder, &commit_id));
	cl_git_pass(git_packbuilder_write(packbuilder, NULL, 0, NULL, NULL));

	cl_git_pass(git_oid_cpy(out_commit_id, &commit_id));

	git_str_dispose(&file_contents);
	git_treebuilder_free(treebuilder);
	git_packbuilder_free(packbuilder);
	git_signature_free(s);
}

void test_pack_filelimit__open_repo_with_multiple_packfiles(void)
{
	git_str path = GIT_STR_INIT;
	git_mwindow_ctl *ctl = &git_mwindow__mem_ctl;
	git_repository *repo;
	git_revwalk *walk;
	git_oid id, *parent_id = NULL;
	size_t i;
	const size_t commit_count = 16;
	unsigned int open_windows;

	/*
	 * Create a repository and populate it with 16 commits, each in its own
	 * packfile.
	 */
	cl_git_pass(git_str_joinpath(&path, clar_sandbox_path(), "repo.git"));
	cl_git_pass(git_repository_init(&repo, path.ptr, true));
	for (i = 0; i < commit_count; ++i) {
		create_packfile_commit(repo, &id, parent_id, i + 1, commit_count);
		parent_id = &id;
	}

	cl_git_pass(git_revwalk_new(&walk, repo));
	cl_git_pass(git_revwalk_sorting(walk, GIT_SORT_TOPOLOGICAL));
	cl_git_pass(git_revwalk_push_ref(walk, "refs/heads/master"));

	/* Walking the repository requires eventually opening each of the packfiles. */
	i = 0;
	while (git_revwalk_next(&id, walk) == 0)
		++i;
	cl_assert_equal_i(commit_count, i);

	cl_git_pass(git_mutex_lock(&git__mwindow_mutex));
	/*
	 * Adding an assert while holding a lock will cause the whole process to
	 * deadlock. Copy the value and do the assert after releasing the lock.
	 */
	open_windows = ctl->open_windows;
	cl_git_pass(git_mutex_unlock(&git__mwindow_mutex));

	cl_assert_equal_i(expected_open_mwindow_files, open_windows);

	git_str_dispose(&path);
	git_revwalk_free(walk);
	git_repository_free(repo);
}
