#include "clar_libgit2.h"

#include "repository.h"
#include "git2/reflog.h"
#include "reflog.h"
#include "ref_helpers.h"

static const char *br2_tip = "a4a7dce85cf63874e984719f4fdd239f5145052f";
static const char *master_tip = "a65fedf39aefe402d3bb6e24df4d4f5fe4547750";
static const char *br2_name = "refs/heads/br2";

static git_repository *g_repo;

void test_refs_settargetwithlog__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo.git");
}

void test_refs_settargetwithlog__cleanup(void)
{
   cl_git_sandbox_cleanup();
}

void test_refs_settargetwithlog__updating_a_direct_reference_adds_a_reflog_entry(void)
{
	git_reference *reference, *reference_out;
	git_oid current_id, target_id;
	git_signature *signature;
	git_reflog *reflog;
	const git_reflog_entry *entry;

	const char *message = "You've been logged, mate!";

	git_oid_fromstr(&current_id, br2_tip);
	git_oid_fromstr(&target_id, master_tip);

	cl_git_pass(git_reference_lookup(&reference, g_repo, br2_name));

	cl_git_pass(git_signature_now(&signature, "foo", "foo@bar"));

	cl_git_pass(git_reference_set_target(
		&reference_out, reference, &target_id, signature, message));

	cl_git_pass(git_reflog_read(&reflog, g_repo, br2_name));

	entry = git_reflog_entry_byindex(reflog, 0);
	cl_assert_equal_oid(&current_id, &entry->oid_old);
	cl_assert_equal_oid(&target_id, &entry->oid_cur);
	cl_assert_equal_s(message, entry->msg);

	git_reflog_free(reflog);
	git_reference_free(reference_out);
	git_reference_free(reference);
	git_signature_free(signature);
}
