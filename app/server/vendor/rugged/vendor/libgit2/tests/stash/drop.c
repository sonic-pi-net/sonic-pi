#include "clar_libgit2.h"
#include "fileops.h"
#include "stash_helpers.h"
#include "refs.h"

static git_repository *repo;
static git_signature *signature;

void test_stash_drop__initialize(void)
{
	cl_git_pass(git_repository_init(&repo, "stash", 0));
	cl_git_pass(git_signature_new(&signature, "nulltoken", "emeric.fermas@gmail.com", 1323847743, 60)); /* Wed Dec 14 08:29:03 2011 +0100 */
}

void test_stash_drop__cleanup(void)
{
	git_signature_free(signature);
	signature = NULL;

	git_repository_free(repo);
	repo = NULL;

	cl_git_pass(git_futils_rmdir_r("stash", NULL, GIT_RMDIR_REMOVE_FILES));
}

void test_stash_drop__cannot_drop_from_an_empty_stash(void)
{
	cl_git_fail_with(git_stash_drop(repo, 0), GIT_ENOTFOUND);
}

static void push_three_states(void)
{
	git_oid oid;
	git_index *index;

	cl_git_mkfile("stash/zero.txt", "content\n");
	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, "zero.txt"));
	cl_repo_commit_from_index(NULL, repo, signature, 0, "Initial commit");
	cl_assert(git_path_exists("stash/zero.txt"));
	git_index_free(index);

	cl_git_mkfile("stash/one.txt", "content\n");
	cl_git_pass(git_stash_save(
		&oid, repo, signature, "First", GIT_STASH_INCLUDE_UNTRACKED));
	cl_assert(!git_path_exists("stash/one.txt"));
	cl_assert(git_path_exists("stash/zero.txt"));

	cl_git_mkfile("stash/two.txt", "content\n");
	cl_git_pass(git_stash_save(
		&oid, repo, signature, "Second", GIT_STASH_INCLUDE_UNTRACKED));
	cl_assert(!git_path_exists("stash/two.txt"));
	cl_assert(git_path_exists("stash/zero.txt"));

	cl_git_mkfile("stash/three.txt", "content\n");
	cl_git_pass(git_stash_save(
		&oid, repo, signature, "Third", GIT_STASH_INCLUDE_UNTRACKED));
	cl_assert(!git_path_exists("stash/three.txt"));
	cl_assert(git_path_exists("stash/zero.txt"));
}

void test_stash_drop__cannot_drop_a_non_existing_stashed_state(void)
{
	push_three_states();

	cl_git_fail_with(git_stash_drop(repo, 666), GIT_ENOTFOUND);
	cl_git_fail_with(git_stash_drop(repo, 42), GIT_ENOTFOUND);
	cl_git_fail_with(git_stash_drop(repo, 3), GIT_ENOTFOUND);
}

void test_stash_drop__can_purge_the_stash_from_the_top(void)
{
	push_three_states();

	cl_git_pass(git_stash_drop(repo, 0));
	cl_git_pass(git_stash_drop(repo, 0));
	cl_git_pass(git_stash_drop(repo, 0));

	cl_git_fail_with(git_stash_drop(repo, 0), GIT_ENOTFOUND);
}

void test_stash_drop__can_purge_the_stash_from_the_bottom(void)
{
	push_three_states();

	cl_git_pass(git_stash_drop(repo, 2));
	cl_git_pass(git_stash_drop(repo, 1));
	cl_git_pass(git_stash_drop(repo, 0));

	cl_git_fail_with(git_stash_drop(repo, 0), GIT_ENOTFOUND);
}

void test_stash_drop__dropping_an_entry_rewrites_reflog_history(void)
{
	git_reference *stash;
	git_reflog *reflog;
	const git_reflog_entry *entry;
	git_oid oid;
	size_t count;

	push_three_states();

	cl_git_pass(git_reference_lookup(&stash, repo, GIT_REFS_STASH_FILE));

	cl_git_pass(git_reflog_read(&reflog, repo, GIT_REFS_STASH_FILE));
	entry = git_reflog_entry_byindex(reflog, 1);

	git_oid_cpy(&oid, git_reflog_entry_id_old(entry));
	count = git_reflog_entrycount(reflog);

	git_reflog_free(reflog);

	cl_git_pass(git_stash_drop(repo, 1));

	cl_git_pass(git_reflog_read(&reflog, repo, GIT_REFS_STASH_FILE));
	entry = git_reflog_entry_byindex(reflog, 0);

	cl_assert_equal_i(0, git_oid_cmp(&oid, git_reflog_entry_id_old(entry)));
	cl_assert_equal_sz(count - 1, git_reflog_entrycount(reflog));

	git_reflog_free(reflog);

	git_reference_free(stash);
}

void test_stash_drop__dropping_the_last_entry_removes_the_stash(void)
{
	git_reference *stash;

	push_three_states();

	cl_git_pass(git_reference_lookup(&stash, repo, GIT_REFS_STASH_FILE));
	git_reference_free(stash);

	cl_git_pass(git_stash_drop(repo, 0));
	cl_git_pass(git_stash_drop(repo, 0));
	cl_git_pass(git_stash_drop(repo, 0));

	cl_git_fail_with(
		git_reference_lookup(&stash, repo, GIT_REFS_STASH_FILE), GIT_ENOTFOUND);
}

void retrieve_top_stash_id(git_oid *out)
{
	git_object *top_stash;

	cl_git_pass(git_revparse_single(&top_stash, repo, "stash@{0}"));
	cl_git_pass(git_reference_name_to_id(out, repo, GIT_REFS_STASH_FILE));

	cl_assert_equal_i(true, git_oid_cmp(out, git_object_id(top_stash)) == 0);

	git_object_free(top_stash);
}

void test_stash_drop__dropping_the_top_stash_updates_the_stash_reference(void)
{
	git_object *next_top_stash;
	git_oid oid;

	push_three_states();

	retrieve_top_stash_id(&oid);

	cl_git_pass(git_revparse_single(&next_top_stash, repo, "stash@{1}"));
	cl_assert(git_oid_cmp(&oid, git_object_id(next_top_stash)) != 0);

	cl_git_pass(git_stash_drop(repo, 0));

	retrieve_top_stash_id(&oid);

	cl_git_pass(git_oid_cmp(&oid, git_object_id(next_top_stash)));

	git_object_free(next_top_stash);
}
