#include "clar_libgit2.h"

#include "repository.h"
#include "git2/reflog.h"
#include "reflog.h"
#include "ref_helpers.h"

static const char *current_master_tip = "a65fedf39aefe402d3bb6e24df4d4f5fe4547750";

static git_repository *g_repo;

void test_refs_createwithlog__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo.git");
}

void test_refs_createwithlog__cleanup(void)
{
   cl_git_sandbox_cleanup();
}

void test_refs_createwithlog__creating_a_direct_reference_adds_a_reflog_entry(void)
{
	git_reference *reference;
	git_oid id;
	git_signature *signature;
	git_reflog *reflog;
	const git_reflog_entry *entry;

	const char *name = "refs/heads/new-head";
	const char *message = "You've been logged, mate!";

	git_oid_fromstr(&id, current_master_tip);

	cl_git_pass(git_signature_now(&signature, "foo", "foo@bar"));

	cl_git_pass(
		git_reference_create(&reference, g_repo, name, &id, 0, signature, message));

	cl_git_pass(git_reflog_read(&reflog, g_repo, name));
	cl_assert_equal_sz(1, git_reflog_entrycount(reflog));

	entry = git_reflog_entry_byindex(reflog, 0);
	cl_assert(git_oid_streq(&entry->oid_old, GIT_OID_HEX_ZERO) == 0);
	cl_assert_equal_oid(&id, &entry->oid_cur);
	cl_assert_equal_s(message, entry->msg);

	git_reflog_free(reflog);
	git_reference_free(reference);
	git_signature_free(signature);
}
