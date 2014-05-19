#include "clar_libgit2.h"

#include "repository.h"
#include "git2/reflog.h"
#include "reflog.h"

static const char *ref_name = "refs/heads/other";
static const char *ref_master_name = "refs/heads/master";
static const char *ref_branch_name = "refs/heads/branch";
static const char *ref_test_name = "refs/heads/test";

static git_repository *g_repo;

void test_refs_overwrite__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
}

void test_refs_overwrite__cleanup(void)
{
   cl_git_sandbox_cleanup();
}

void test_refs_overwrite__symbolic(void)
{
   // Overwrite an existing symbolic reference
	git_reference *ref, *branch_ref;

	/* The target needds to exist and we need to check the name has changed */
	cl_git_pass(git_reference_symbolic_create(&branch_ref, g_repo, ref_branch_name, ref_master_name, 0, NULL, NULL));
	cl_git_pass(git_reference_symbolic_create(&ref, g_repo, ref_name, ref_branch_name, 0, NULL, NULL));
	git_reference_free(ref);

	/* Ensure it points to the right place*/
	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_name));
	cl_assert(git_reference_type(ref) & GIT_REF_SYMBOLIC);
	cl_assert_equal_s(git_reference_symbolic_target(ref), ref_branch_name);
	git_reference_free(ref);

	/* Ensure we can't create it unless we force it to */
	cl_git_fail(git_reference_symbolic_create(&ref, g_repo, ref_name, ref_master_name, 0, NULL, NULL));
	cl_git_pass(git_reference_symbolic_create(&ref, g_repo, ref_name, ref_master_name, 1, NULL, NULL));
	git_reference_free(ref);

	/* Ensure it points to the right place */
	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_name));
	cl_assert(git_reference_type(ref) & GIT_REF_SYMBOLIC);
	cl_assert_equal_s(git_reference_symbolic_target(ref), ref_master_name);

	git_reference_free(ref);
	git_reference_free(branch_ref);
}

void test_refs_overwrite__object_id(void)
{
   // Overwrite an existing object id reference
	git_reference *ref;
	git_oid id;

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_master_name));
	cl_assert(git_reference_type(ref) & GIT_REF_OID);
	git_oid_cpy(&id, git_reference_target(ref));
	git_reference_free(ref);

	/* Create it */
	cl_git_pass(git_reference_create(&ref, g_repo, ref_name, &id, 0, NULL, NULL));
	git_reference_free(ref);

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_test_name));
	cl_assert(git_reference_type(ref) & GIT_REF_OID);
	git_oid_cpy(&id, git_reference_target(ref));
	git_reference_free(ref);

	/* Ensure we can't overwrite unless we force it */
	cl_git_fail(git_reference_create(&ref, g_repo, ref_name, &id, 0, NULL, NULL));
	cl_git_pass(git_reference_create(&ref, g_repo, ref_name, &id, 1, NULL, NULL));
	git_reference_free(ref);

	/* Ensure it has been overwritten */
	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_name));
	cl_assert(!git_oid_cmp(&id, git_reference_target(ref)));

	git_reference_free(ref);
}

void test_refs_overwrite__object_id_with_symbolic(void)
{
   // Overwrite an existing object id reference with a symbolic one
	git_reference *ref;
	git_oid id;

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_master_name));
	cl_assert(git_reference_type(ref) & GIT_REF_OID);
	git_oid_cpy(&id, git_reference_target(ref));
	git_reference_free(ref);

	cl_git_pass(git_reference_create(&ref, g_repo, ref_name, &id, 0, NULL, NULL));
	git_reference_free(ref);
	cl_git_fail(git_reference_symbolic_create(&ref, g_repo, ref_name, ref_master_name, 0, NULL, NULL));
	cl_git_pass(git_reference_symbolic_create(&ref, g_repo, ref_name, ref_master_name, 1, NULL, NULL));
	git_reference_free(ref);

	/* Ensure it points to the right place */
	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_name));
	cl_assert(git_reference_type(ref) & GIT_REF_SYMBOLIC);
	cl_assert_equal_s(git_reference_symbolic_target(ref), ref_master_name);

	git_reference_free(ref);
}

void test_refs_overwrite__symbolic_with_object_id(void)
{
   // Overwrite an existing symbolic reference with an object id one
	git_reference *ref;
	git_oid id;

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_master_name));
	cl_assert(git_reference_type(ref) & GIT_REF_OID);
	git_oid_cpy(&id, git_reference_target(ref));
	git_reference_free(ref);

	/* Create the symbolic ref */
	cl_git_pass(git_reference_symbolic_create(&ref, g_repo, ref_name, ref_master_name, 0, NULL, NULL));
	git_reference_free(ref);
	/* It shouldn't overwrite unless we tell it to */
	cl_git_fail(git_reference_create(&ref, g_repo, ref_name, &id, 0, NULL, NULL));
	cl_git_pass(git_reference_create(&ref, g_repo, ref_name, &id, 1, NULL, NULL));
	git_reference_free(ref);

	/* Ensure it points to the right place */
	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_name));
	cl_assert(git_reference_type(ref) & GIT_REF_OID);
	cl_assert(!git_oid_cmp(git_reference_target(ref), &id));

	git_reference_free(ref);
}
