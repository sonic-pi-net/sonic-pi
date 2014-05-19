#include "clar_libgit2.h"

#include "repository.h"
#include "git2/reflog.h"
#include "reflog.h"
#include "git2/refs.h"

static const char *ref_name = "refs/heads/other";
static const char *ref_master_name = "refs/heads/master";
static const char *ref_test_name = "refs/heads/test";

static git_repository *g_repo;

void test_refs_setter__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");
}

void test_refs_setter__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_refs_setter__update_direct(void)
{
	git_reference *ref, *test_ref, *new_ref;
	git_oid id;

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_master_name));
	cl_assert(git_reference_type(ref) == GIT_REF_OID);
	git_oid_cpy(&id, git_reference_target(ref));
	git_reference_free(ref);

	cl_git_pass(git_reference_lookup(&test_ref, g_repo, ref_test_name));
	cl_assert(git_reference_type(test_ref) == GIT_REF_OID);

	cl_git_pass(git_reference_set_target(&new_ref, test_ref, &id, NULL, NULL));

	git_reference_free(test_ref);
	git_reference_free(new_ref);

	cl_git_pass(git_reference_lookup(&test_ref, g_repo, ref_test_name));
	cl_assert(git_reference_type(test_ref) == GIT_REF_OID);
	cl_assert(git_oid_cmp(&id, git_reference_target(test_ref)) == 0);
	git_reference_free(test_ref);
}

void test_refs_setter__update_symbolic(void)
{
	git_reference *head, *new_head;

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_assert(git_reference_type(head) == GIT_REF_SYMBOLIC);
	cl_assert(strcmp(git_reference_symbolic_target(head), ref_master_name) == 0);

	cl_git_pass(git_reference_symbolic_set_target(&new_head, head, ref_test_name, NULL, NULL));
	git_reference_free(new_head);
	git_reference_free(head);

	cl_git_pass(git_reference_lookup(&head, g_repo, "HEAD"));
	cl_assert(git_reference_type(head) == GIT_REF_SYMBOLIC);
	cl_assert(strcmp(git_reference_symbolic_target(head), ref_test_name) == 0);
	git_reference_free(head);
}

void test_refs_setter__cant_update_direct_with_symbolic(void)
{
	// Overwrite an existing object id reference with a symbolic one
	git_reference *ref, *new;
	git_oid id;

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_master_name));
	cl_assert(git_reference_type(ref) == GIT_REF_OID);
	git_oid_cpy(&id, git_reference_target(ref));

	cl_git_fail(git_reference_symbolic_set_target(&new, ref, ref_name, NULL, NULL));

	git_reference_free(ref);
}

void test_refs_setter__cant_update_symbolic_with_direct(void)
{
	// Overwrite an existing symbolic reference with an object id one
	git_reference *ref, *new;
	git_oid id;

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_master_name));
	cl_assert(git_reference_type(ref) == GIT_REF_OID);
	git_oid_cpy(&id, git_reference_target(ref));
	git_reference_free(ref);

	/* Create the symbolic ref */
	cl_git_pass(git_reference_symbolic_create(&ref, g_repo, ref_name, ref_master_name, 0, NULL, NULL));

	/* Can't set an OID on a direct ref */
	cl_git_fail(git_reference_set_target(&new, ref, &id, NULL, NULL));

	git_reference_free(ref);
}
