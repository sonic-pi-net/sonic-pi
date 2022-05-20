#include "clar_libgit2.h"

#include "repository.h"
#include "git2/reflog.h"
#include "reflog.h"
#include "ref_helpers.h"

static const char *commit_id = "099fabac3a9ea935598528c27f866e34089c2eff";
static const char *refname = "refs/heads/master";
static const char *other_refname = "refs/heads/foo";
static const char *other_commit_id = "a65fedf39aefe402d3bb6e24df4d4f5fe4547750";

static git_repository *g_repo;

void test_refs_races__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
}

void test_refs_races__cleanup(void)
{
   cl_git_sandbox_cleanup();
}

void test_refs_races__create_matching_zero_old(void)
{
	git_reference *ref;
	git_oid id, zero_id;

	git_oid_fromstr(&id, commit_id);
	git_oid_fromstr(&zero_id, "0000000000000000000000000000000000000000");

	cl_git_fail(git_reference_create_matching(&ref, g_repo, refname, &id, 1, &zero_id, NULL));
	git_reference_free(ref);

	cl_git_pass(git_reference_create_matching(&ref, g_repo, other_refname, &id, 1, &zero_id, NULL));
	git_reference_free(ref);

	cl_git_fail(git_reference_create_matching(&ref, g_repo, other_refname, &id, 1, &zero_id, NULL));
	git_reference_free(ref);
}

void test_refs_races__create_matching(void)
{
	git_reference *ref, *ref2, *ref3;
	git_oid id, other_id;

	git_oid_fromstr(&id, commit_id);
	git_oid_fromstr(&other_id, other_commit_id);

	cl_git_fail_with(GIT_EMODIFIED, git_reference_create_matching(&ref, g_repo, refname, &other_id, 1, &other_id, NULL));

	cl_git_pass(git_reference_lookup(&ref, g_repo, refname));
	cl_git_pass(git_reference_create_matching(&ref2, g_repo, refname, &other_id, 1, &id, NULL));
	cl_git_fail_with(GIT_EMODIFIED, git_reference_set_target(&ref3, ref, &other_id, NULL));

	git_reference_free(ref);
	git_reference_free(ref2);
	git_reference_free(ref3);
}

void test_refs_races__symbolic_create_matching(void)
{
	git_reference *ref, *ref2, *ref3;
	git_oid id, other_id;

	git_oid_fromstr(&id, commit_id);
	git_oid_fromstr(&other_id, other_commit_id);

	cl_git_fail_with(GIT_EMODIFIED, git_reference_symbolic_create_matching(&ref, g_repo, "HEAD", other_refname, 1, other_refname, NULL));

	cl_git_pass(git_reference_lookup(&ref, g_repo, "HEAD"));
	cl_git_pass(git_reference_symbolic_create_matching(&ref2, g_repo, "HEAD", other_refname, 1, NULL, refname));
	cl_git_fail_with(GIT_EMODIFIED, git_reference_symbolic_set_target(&ref3, ref, other_refname, NULL));

	git_reference_free(ref);
	git_reference_free(ref2);
	git_reference_free(ref3);
}

void test_refs_races__delete(void)
{
	git_reference *ref, *ref2;
	git_oid id, other_id;

	git_oid_fromstr(&id, commit_id);
	git_oid_fromstr(&other_id, other_commit_id);

	/* We can delete a value that matches */
	cl_git_pass(git_reference_lookup(&ref, g_repo, refname));
	cl_git_pass(git_reference_delete(ref));
	git_reference_free(ref);

	/* We cannot delete a symbolic value that doesn't match */
	cl_git_pass(git_reference_lookup(&ref, g_repo, "refs/symref"));
	cl_git_pass(git_reference_symbolic_create_matching(&ref2, g_repo, "refs/symref", other_refname, 1, NULL, refname));
	cl_git_fail_with(GIT_EMODIFIED, git_reference_delete(ref));

	git_reference_free(ref);
	git_reference_free(ref2);

	cl_git_pass(git_reference_create(&ref, g_repo, refname, &id, 1, NULL));
	git_reference_free(ref);

	/* We cannot delete an oid value that doesn't match */
	cl_git_pass(git_reference_lookup(&ref, g_repo, refname));
	cl_git_pass(git_reference_create_matching(&ref2, g_repo, refname, &other_id, 1, &id, NULL));
	cl_git_fail_with(GIT_EMODIFIED, git_reference_delete(ref));

	git_reference_free(ref);
	git_reference_free(ref2);
}

void test_refs_races__switch_oid_to_symbolic(void)
{
	git_reference *ref, *ref2, *ref3;
	git_oid id, other_id;

	git_oid_fromstr(&id, commit_id);
	git_oid_fromstr(&other_id, other_commit_id);

	/* Removing a direct ref when it's currently symbolic should fail */
	cl_git_pass(git_reference_lookup(&ref, g_repo, refname));
	cl_git_pass(git_reference_symbolic_create(&ref2, g_repo, refname, other_refname, 1, NULL));
	cl_git_fail_with(GIT_EMODIFIED, git_reference_delete(ref));

	git_reference_free(ref);
	git_reference_free(ref2);

	cl_git_pass(git_reference_create(&ref, g_repo, refname, &id, 1, NULL));
	git_reference_free(ref);

	/* Updating a direct ref when it's currently symbolic should fail */
	cl_git_pass(git_reference_lookup(&ref, g_repo, refname));
	cl_git_pass(git_reference_symbolic_create(&ref2, g_repo, refname, other_refname, 1, NULL));
	cl_git_fail_with(GIT_EMODIFIED, git_reference_set_target(&ref3, ref, &other_id, NULL));

	git_reference_free(ref);
	git_reference_free(ref2);
	git_reference_free(ref3);
}

void test_refs_races__switch_symbolic_to_oid(void)
{
	git_reference *ref, *ref2, *ref3;
	git_oid id, other_id;

	git_oid_fromstr(&id, commit_id);
	git_oid_fromstr(&other_id, other_commit_id);

	/* Removing a symbolic ref when it's currently direct should fail */
	cl_git_pass(git_reference_lookup(&ref, g_repo, "refs/symref"));
	cl_git_pass(git_reference_create(&ref2, g_repo, "refs/symref", &id, 1, NULL));
	cl_git_fail_with(GIT_EMODIFIED, git_reference_delete(ref));

	git_reference_free(ref);
	git_reference_free(ref2);

	cl_git_pass(git_reference_symbolic_create(&ref, g_repo, "refs/symref", refname, 1, NULL));
	git_reference_free(ref);

	/* Updating a symbolic ref when it's currently direct should fail */
	cl_git_pass(git_reference_lookup(&ref, g_repo, "refs/symref"));
	cl_git_pass(git_reference_create(&ref2, g_repo, "refs/symref", &id, 1, NULL));
	cl_git_fail_with(GIT_EMODIFIED, git_reference_symbolic_set_target(&ref3, ref, other_refname, NULL));

	git_reference_free(ref);
	git_reference_free(ref2);
	git_reference_free(ref3);
}
