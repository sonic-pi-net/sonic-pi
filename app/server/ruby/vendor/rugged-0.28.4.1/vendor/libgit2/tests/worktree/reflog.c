#include "clar_libgit2.h"
#include "worktree_helpers.h"

#include "reflog.h"

#define COMMON_REPO "testrepo"
#define WORKTREE_REPO "testrepo-worktree"

#define REFLOG "refs/heads/testrepo-worktree"
#define REFLOG_MESSAGE "reflog message"

static worktree_fixture fixture =
	WORKTREE_FIXTURE_INIT(COMMON_REPO, WORKTREE_REPO);

void test_worktree_reflog__initialize(void)
{
	setup_fixture_worktree(&fixture);
}

void test_worktree_reflog__cleanup(void)
{
	cleanup_fixture_worktree(&fixture);
}

void test_worktree_reflog__read_worktree_HEAD(void)
{
	git_reflog *reflog;
	const git_reflog_entry *entry;

	cl_git_pass(git_reflog_read(&reflog, fixture.worktree, "HEAD"));
	cl_assert_equal_i(1, git_reflog_entrycount(reflog));

	entry = git_reflog_entry_byindex(reflog, 0);
	cl_assert(entry != NULL);
	cl_assert_equal_s("checkout: moving from 099fabac3a9ea935598528c27f866e34089c2eff to testrepo-worktree", git_reflog_entry_message(entry));

	git_reflog_free(reflog);
}

void test_worktree_reflog__read_parent_HEAD(void)
{
	git_reflog *reflog;

	cl_git_pass(git_reflog_read(&reflog, fixture.repo, "HEAD"));
	/* there is no logs/HEAD in the parent repo */
	cl_assert_equal_i(0, git_reflog_entrycount(reflog));

	git_reflog_free(reflog);
}

void test_worktree_reflog__read(void)
{
	git_reflog *reflog;
	const git_reflog_entry *entry;

	cl_git_pass(git_reflog_read(&reflog, fixture.worktree, REFLOG));
	cl_assert_equal_i(git_reflog_entrycount(reflog), 1);

	entry = git_reflog_entry_byindex(reflog, 0);
	cl_assert(entry != NULL);
	cl_assert_equal_s(git_reflog_entry_message(entry), "branch: Created from HEAD");

	git_reflog_free(reflog);
}

void test_worktree_reflog__append_then_read(void)
{
	git_reflog *reflog, *parent_reflog;
	const git_reflog_entry *entry;
	git_reference *head;
	git_signature *sig;
	const git_oid *oid;

	cl_git_pass(git_repository_head(&head, fixture.worktree));
	cl_assert((oid = git_reference_target(head)) != NULL);
	cl_git_pass(git_signature_now(&sig, "foo", "foo@bar"));

	cl_git_pass(git_reflog_read(&reflog, fixture.worktree, REFLOG));
	cl_git_pass(git_reflog_append(reflog, oid, sig, REFLOG_MESSAGE));
	git_reflog_write(reflog);

	cl_git_pass(git_reflog_read(&parent_reflog, fixture.repo, REFLOG));
	entry = git_reflog_entry_byindex(parent_reflog, 0);
	cl_assert(git_oid_cmp(oid, &entry->oid_old) == 0);
	cl_assert(git_oid_cmp(oid, &entry->oid_cur) == 0);

	git_reference_free(head);
	git_signature_free(sig);
	git_reflog_free(reflog);
	git_reflog_free(parent_reflog);
}
