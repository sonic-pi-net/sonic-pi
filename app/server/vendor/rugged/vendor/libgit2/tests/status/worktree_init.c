#include "clar_libgit2.h"
#include "git2/sys/repository.h"

#include "fileops.h"
#include "ignore.h"
#include "status_helpers.h"
#include "posix.h"
#include "util.h"
#include "path.h"

static void cleanup_new_repo(void *path)
{
	cl_fixture_cleanup((char *)path);
}

void test_status_worktree_init__cannot_retrieve_the_status_of_a_bare_repository(void)
{
	git_repository *repo;
	unsigned int status = 0;

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));
	cl_assert_equal_i(GIT_EBAREREPO, git_status_file(&status, repo, "dummy"));
	git_repository_free(repo);
}

void test_status_worktree_init__first_commit_in_progress(void)
{
	git_repository *repo;
	git_index *index;
	status_entry_single result;

	cl_set_cleanup(&cleanup_new_repo, "getting_started");

	cl_git_pass(git_repository_init(&repo, "getting_started", 0));
	cl_git_mkfile("getting_started/testfile.txt", "content\n");

	memset(&result, 0, sizeof(result));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &result));
	cl_assert_equal_i(1, result.count);
	cl_assert(result.status == GIT_STATUS_WT_NEW);

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, "testfile.txt"));
	cl_git_pass(git_index_write(index));

	memset(&result, 0, sizeof(result));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &result));
	cl_assert_equal_i(1, result.count);
	cl_assert(result.status == GIT_STATUS_INDEX_NEW);

	git_index_free(index);
	git_repository_free(repo);
}



void test_status_worktree_init__status_file_without_index_or_workdir(void)
{
	git_repository *repo;
	unsigned int status = 0;
	git_index *index;

	cl_git_pass(p_mkdir("wd", 0777));

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_repository_set_workdir(repo, "wd", false));

	cl_git_pass(git_index_open(&index, "empty-index"));
	cl_assert_equal_i(0, (int)git_index_entrycount(index));
	git_repository_set_index(repo, index);

	cl_git_pass(git_status_file(&status, repo, "branch_file.txt"));

	cl_assert_equal_i(GIT_STATUS_INDEX_DELETED, status);

	git_repository_free(repo);
	git_index_free(index);
	cl_git_pass(p_rmdir("wd"));
}

static void fill_index_wth_head_entries(git_repository *repo, git_index *index)
{
	git_oid oid;
	git_commit *commit;
	git_tree *tree;

	cl_git_pass(git_reference_name_to_id(&oid, repo, "HEAD"));
	cl_git_pass(git_commit_lookup(&commit, repo, &oid));
	cl_git_pass(git_commit_tree(&tree, commit));

	cl_git_pass(git_index_read_tree(index, tree));
	cl_git_pass(git_index_write(index));

	git_tree_free(tree);
	git_commit_free(commit);
}

void test_status_worktree_init__status_file_with_clean_index_and_empty_workdir(void)
{
	git_repository *repo;
	unsigned int status = 0;
	git_index *index;

	cl_git_pass(p_mkdir("wd", 0777));

	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_repository_set_workdir(repo, "wd", false));

	cl_git_pass(git_index_open(&index, "my-index"));
	fill_index_wth_head_entries(repo, index);

	git_repository_set_index(repo, index);

	cl_git_pass(git_status_file(&status, repo, "branch_file.txt"));

	cl_assert_equal_i(GIT_STATUS_WT_DELETED, status);

	git_repository_free(repo);
	git_index_free(index);
	cl_git_pass(p_rmdir("wd"));
	cl_git_pass(p_unlink("my-index"));
}

void test_status_worktree_init__bracket_in_filename(void)
{
	git_repository *repo;
	git_index *index;
	status_entry_single result;
	unsigned int status_flags;
	int error;

	#define FILE_WITH_BRACKET "LICENSE[1].md"
	#define FILE_WITHOUT_BRACKET "LICENSE1.md"

	cl_set_cleanup(&cleanup_new_repo, "with_bracket");

	cl_git_pass(git_repository_init(&repo, "with_bracket", 0));
	cl_git_mkfile("with_bracket/" FILE_WITH_BRACKET, "I have a bracket in my name\n");

	/* file is new to working directory */

	memset(&result, 0, sizeof(result));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &result));
	cl_assert_equal_i(1, result.count);
	cl_assert(result.status == GIT_STATUS_WT_NEW);

	cl_git_pass(git_status_file(&status_flags, repo, FILE_WITH_BRACKET));
	cl_assert(status_flags == GIT_STATUS_WT_NEW);

	/* ignore the file */

	cl_git_rewritefile("with_bracket/.gitignore", "*.md\n.gitignore\n");

	memset(&result, 0, sizeof(result));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &result));
	cl_assert_equal_i(2, result.count);
	cl_assert(result.status == GIT_STATUS_IGNORED);

	cl_git_pass(git_status_file(&status_flags, repo, FILE_WITH_BRACKET));
	cl_assert(status_flags == GIT_STATUS_IGNORED);

	/* don't ignore the file */

	cl_git_rewritefile("with_bracket/.gitignore", ".gitignore\n");

	memset(&result, 0, sizeof(result));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &result));
	cl_assert_equal_i(2, result.count);
	cl_assert(result.status == GIT_STATUS_WT_NEW);

	cl_git_pass(git_status_file(&status_flags, repo, FILE_WITH_BRACKET));
	cl_assert(status_flags == GIT_STATUS_WT_NEW);

	/* add the file to the index */

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, FILE_WITH_BRACKET));
	cl_git_pass(git_index_write(index));

	memset(&result, 0, sizeof(result));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &result));
	cl_assert_equal_i(2, result.count);
	cl_assert(result.status == GIT_STATUS_INDEX_NEW);

	cl_git_pass(git_status_file(&status_flags, repo, FILE_WITH_BRACKET));
	cl_assert(status_flags == GIT_STATUS_INDEX_NEW);

	/* Create file without bracket */

	cl_git_mkfile("with_bracket/" FILE_WITHOUT_BRACKET, "I have no bracket in my name!\n");

	cl_git_pass(git_status_file(&status_flags, repo, FILE_WITHOUT_BRACKET));
	cl_assert(status_flags == GIT_STATUS_WT_NEW);

	cl_git_pass(git_status_file(&status_flags, repo, "LICENSE\\[1\\].md"));
	cl_assert(status_flags == GIT_STATUS_INDEX_NEW);

	error = git_status_file(&status_flags, repo, FILE_WITH_BRACKET);
	cl_git_fail(error);
	cl_assert_equal_i(GIT_EAMBIGUOUS, error);

	git_index_free(index);
	git_repository_free(repo);
}

void test_status_worktree_init__space_in_filename(void)
{
	git_repository *repo;
	git_index *index;
	status_entry_single result;
	unsigned int status_flags;

#define FILE_WITH_SPACE "LICENSE - copy.md"

	cl_set_cleanup(&cleanup_new_repo, "with_space");
	cl_git_pass(git_repository_init(&repo, "with_space", 0));
	cl_git_mkfile("with_space/" FILE_WITH_SPACE, "I have a space in my name\n");

	/* file is new to working directory */

	memset(&result, 0, sizeof(result));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &result));
	cl_assert_equal_i(1, result.count);
	cl_assert(result.status == GIT_STATUS_WT_NEW);

	cl_git_pass(git_status_file(&status_flags, repo, FILE_WITH_SPACE));
	cl_assert(status_flags == GIT_STATUS_WT_NEW);

	/* ignore the file */

	cl_git_rewritefile("with_space/.gitignore", "*.md\n.gitignore\n");

	memset(&result, 0, sizeof(result));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &result));
	cl_assert_equal_i(2, result.count);
	cl_assert(result.status == GIT_STATUS_IGNORED);

	cl_git_pass(git_status_file(&status_flags, repo, FILE_WITH_SPACE));
	cl_assert(status_flags == GIT_STATUS_IGNORED);

	/* don't ignore the file */

	cl_git_rewritefile("with_space/.gitignore", ".gitignore\n");

	memset(&result, 0, sizeof(result));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &result));
	cl_assert_equal_i(2, result.count);
	cl_assert(result.status == GIT_STATUS_WT_NEW);

	cl_git_pass(git_status_file(&status_flags, repo, FILE_WITH_SPACE));
	cl_assert(status_flags == GIT_STATUS_WT_NEW);

	/* add the file to the index */

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, FILE_WITH_SPACE));
	cl_git_pass(git_index_write(index));

	memset(&result, 0, sizeof(result));
	cl_git_pass(git_status_foreach(repo, cb_status__single, &result));
	cl_assert_equal_i(2, result.count);
	cl_assert(result.status == GIT_STATUS_INDEX_NEW);

	cl_git_pass(git_status_file(&status_flags, repo, FILE_WITH_SPACE));
	cl_assert(status_flags == GIT_STATUS_INDEX_NEW);

	git_index_free(index);
	git_repository_free(repo);
}

static int cb_status__expected_path(const char *p, unsigned int s, void *payload)
{
	const char *expected_path = (const char *)payload;

	GIT_UNUSED(s);

	if (payload == NULL)
		cl_fail("Unexpected path");

	cl_assert_equal_s(expected_path, p);

	return 0;
}

void test_status_worktree_init__disable_pathspec_match(void)
{
	git_repository *repo;
	git_status_options opts = GIT_STATUS_OPTIONS_INIT;
	char *file_with_bracket = "LICENSE[1].md", 
		*imaginary_file_with_bracket = "LICENSE[1-2].md";

	cl_set_cleanup(&cleanup_new_repo, "pathspec");
	cl_git_pass(git_repository_init(&repo, "pathspec", 0));
	cl_git_mkfile("pathspec/LICENSE[1].md", "screaming bracket\n");
	cl_git_mkfile("pathspec/LICENSE1.md", "no bracket\n");

	opts.flags = GIT_STATUS_OPT_INCLUDE_UNTRACKED | 
		GIT_STATUS_OPT_DISABLE_PATHSPEC_MATCH;
	opts.pathspec.count = 1;
	opts.pathspec.strings = &file_with_bracket;

	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__expected_path, 
		file_with_bracket)
	);

	/* Test passing a pathspec matching files in the workdir. */
	/* Must not match because pathspecs are disabled. */ 
	opts.pathspec.strings = &imaginary_file_with_bracket;
	cl_git_pass(
		git_status_foreach_ext(repo, &opts, cb_status__expected_path, NULL)
	);

	git_repository_free(repo);
}

void test_status_worktree_init__new_staged_file_must_handle_crlf(void)
{
	git_repository *repo;
	git_index *index;
	unsigned int status;

	cl_set_cleanup(&cleanup_new_repo, "getting_started");
	cl_git_pass(git_repository_init(&repo, "getting_started", 0));

	/* Ensure that repo has core.autocrlf=true */
	cl_repo_set_bool(repo, "core.autocrlf", true);

	cl_git_mkfile("getting_started/testfile.txt", "content\r\n");	/* Content with CRLF */

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, "testfile.txt"));
	cl_git_pass(git_index_write(index));

	cl_git_pass(git_status_file(&status, repo, "testfile.txt"));
	cl_assert_equal_i(GIT_STATUS_INDEX_NEW, status);

	git_index_free(index);
	git_repository_free(repo);
}

