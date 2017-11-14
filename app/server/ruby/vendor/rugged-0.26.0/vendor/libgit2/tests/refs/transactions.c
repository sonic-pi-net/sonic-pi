#include "clar_libgit2.h"
#include "git2/transaction.h"

static git_repository *g_repo;
static git_transaction *g_tx;

void test_refs_transactions__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
   cl_git_pass(git_transaction_new(&g_tx, g_repo));
}

void test_refs_transactions__cleanup(void)
{
	git_transaction_free(g_tx);
	cl_git_sandbox_cleanup();
}

void test_refs_transactions__single_ref_oid(void)
{
	git_reference *ref;
	git_oid id;

	git_oid_fromstr(&id, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");

	cl_git_pass(git_transaction_lock_ref(g_tx, "refs/heads/master"));
	cl_git_pass(git_transaction_set_target(g_tx, "refs/heads/master", &id, NULL, NULL));
	cl_git_pass(git_transaction_commit(g_tx));

	cl_git_pass(git_reference_lookup(&ref, g_repo, "refs/heads/master"));

	cl_assert(!git_oid_cmp(&id, git_reference_target(ref)));
	git_reference_free(ref);
}

void test_refs_transactions__single_ref_symbolic(void)
{
	git_reference *ref;

	cl_git_pass(git_transaction_lock_ref(g_tx, "HEAD"));
	cl_git_pass(git_transaction_set_symbolic_target(g_tx, "HEAD", "refs/heads/foo", NULL, NULL));
	cl_git_pass(git_transaction_commit(g_tx));

	cl_git_pass(git_reference_lookup(&ref, g_repo, "HEAD"));

	cl_assert_equal_s("refs/heads/foo", git_reference_symbolic_target(ref));
	git_reference_free(ref);
}

void test_refs_transactions__single_ref_mix_types(void)
{
	git_reference *ref;
	git_oid id;

	git_oid_fromstr(&id, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");

	cl_git_pass(git_transaction_lock_ref(g_tx, "refs/heads/master"));
	cl_git_pass(git_transaction_lock_ref(g_tx, "HEAD"));
	cl_git_pass(git_transaction_set_symbolic_target(g_tx, "refs/heads/master", "refs/heads/foo", NULL, NULL));
	cl_git_pass(git_transaction_set_target(g_tx, "HEAD", &id, NULL, NULL));
	cl_git_pass(git_transaction_commit(g_tx));

	cl_git_pass(git_reference_lookup(&ref, g_repo, "refs/heads/master"));
	cl_assert_equal_s("refs/heads/foo", git_reference_symbolic_target(ref));
	git_reference_free(ref);

	cl_git_pass(git_reference_lookup(&ref, g_repo, "HEAD"));
	cl_assert(!git_oid_cmp(&id, git_reference_target(ref)));
	git_reference_free(ref);
}

void test_refs_transactions__single_ref_delete(void)
{
	git_reference *ref;

	cl_git_pass(git_transaction_lock_ref(g_tx, "refs/heads/master"));
	cl_git_pass(git_transaction_remove(g_tx, "refs/heads/master"));
	cl_git_pass(git_transaction_commit(g_tx));

	cl_git_fail_with(GIT_ENOTFOUND, git_reference_lookup(&ref, g_repo, "refs/heads/master"));
}

void test_refs_transactions__single_create(void)
{
	git_reference *ref;
	const char *name = "refs/heads/new-branch";
	git_oid id;

	cl_git_fail_with(GIT_ENOTFOUND, git_reference_lookup(&ref, g_repo, name));

	cl_git_pass(git_transaction_lock_ref(g_tx, name));

	git_oid_fromstr(&id, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	cl_git_pass(git_transaction_set_target(g_tx, name, &id, NULL, NULL));
	cl_git_pass(git_transaction_commit(g_tx));

	cl_git_pass(git_reference_lookup(&ref, g_repo, name));
	cl_assert(!git_oid_cmp(&id, git_reference_target(ref)));
	git_reference_free(ref);
}

void test_refs_transactions__unlocked_set(void)
{
	git_oid id;

	cl_git_pass(git_transaction_lock_ref(g_tx, "refs/heads/master"));
	git_oid_fromstr(&id, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	cl_git_fail_with(GIT_ENOTFOUND, git_transaction_set_target(g_tx, "refs/heads/foo", &id, NULL, NULL));
	cl_git_pass(git_transaction_commit(g_tx));
}
