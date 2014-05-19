#include "clar_libgit2.h"

#include "repository.h"
#include "git2/reflog.h"
#include "reflog.h"
#include "ref_helpers.h"

static const char *current_master_tip = "099fabac3a9ea935598528c27f866e34089c2eff";
static const char *current_head_target = "refs/heads/master";

static git_repository *g_repo;

void test_refs_create__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
}

void test_refs_create__cleanup(void)
{
   cl_git_sandbox_cleanup();
}

void test_refs_create__symbolic(void)
{
   // create a new symbolic reference
	git_reference *new_reference, *looked_up_ref, *resolved_ref;
	git_repository *repo2;
	git_oid id;

	const char *new_head_tracker = "ANOTHER_HEAD_TRACKER";

	git_oid_fromstr(&id, current_master_tip);

	/* Create and write the new symbolic reference */
	cl_git_pass(git_reference_symbolic_create(&new_reference, g_repo, new_head_tracker, current_head_target, 0, NULL, NULL));

	/* Ensure the reference can be looked-up... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, new_head_tracker));
	cl_assert(git_reference_type(looked_up_ref) & GIT_REF_SYMBOLIC);
	cl_assert(reference_is_packed(looked_up_ref) == 0);
	cl_assert_equal_s(looked_up_ref->name, new_head_tracker);

	/* ...peeled.. */
	cl_git_pass(git_reference_resolve(&resolved_ref, looked_up_ref));
	cl_assert(git_reference_type(resolved_ref) == GIT_REF_OID);

	/* ...and that it points to the current master tip */
	cl_assert(git_oid_cmp(&id, git_reference_target(resolved_ref)) == 0);
	git_reference_free(looked_up_ref);
	git_reference_free(resolved_ref);

	/* Similar test with a fresh new repository */
	cl_git_pass(git_repository_open(&repo2, "testrepo"));

	cl_git_pass(git_reference_lookup(&looked_up_ref, repo2, new_head_tracker));
	cl_git_pass(git_reference_resolve(&resolved_ref, looked_up_ref));
	cl_assert(git_oid_cmp(&id, git_reference_target(resolved_ref)) == 0);

	git_repository_free(repo2);

	git_reference_free(new_reference);
	git_reference_free(looked_up_ref);
	git_reference_free(resolved_ref);
}

void test_refs_create__deep_symbolic(void)
{
   // create a deep symbolic reference
	git_reference *new_reference, *looked_up_ref, *resolved_ref;
	git_oid id;

	const char *new_head_tracker = "deep/rooted/tracker";

	git_oid_fromstr(&id, current_master_tip);

	cl_git_pass(git_reference_symbolic_create(&new_reference, g_repo, new_head_tracker, current_head_target, 0, NULL, NULL));
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, new_head_tracker));
	cl_git_pass(git_reference_resolve(&resolved_ref, looked_up_ref));
	cl_assert(git_oid_cmp(&id, git_reference_target(resolved_ref)) == 0);

	git_reference_free(new_reference);
	git_reference_free(looked_up_ref);
	git_reference_free(resolved_ref);
}

void test_refs_create__oid(void)
{
   // create a new OID reference
	git_reference *new_reference, *looked_up_ref;
	git_repository *repo2;
	git_oid id;

	const char *new_head = "refs/heads/new-head";

	git_oid_fromstr(&id, current_master_tip);

	/* Create and write the new object id reference */
	cl_git_pass(git_reference_create(&new_reference, g_repo, new_head, &id, 0, NULL, NULL));

	/* Ensure the reference can be looked-up... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, new_head));
	cl_assert(git_reference_type(looked_up_ref) & GIT_REF_OID);
	cl_assert(reference_is_packed(looked_up_ref) == 0);
	cl_assert_equal_s(looked_up_ref->name, new_head);

	/* ...and that it points to the current master tip */
	cl_assert(git_oid_cmp(&id, git_reference_target(looked_up_ref)) == 0);
	git_reference_free(looked_up_ref);

	/* Similar test with a fresh new repository */
	cl_git_pass(git_repository_open(&repo2, "testrepo"));

	cl_git_pass(git_reference_lookup(&looked_up_ref, repo2, new_head));
	cl_assert(git_oid_cmp(&id, git_reference_target(looked_up_ref)) == 0);

	git_repository_free(repo2);

	git_reference_free(new_reference);
	git_reference_free(looked_up_ref);
}

void test_refs_create__oid_unknown(void)
{
   // Can not create a new OID reference which targets at an unknown id
	git_reference *new_reference, *looked_up_ref;
	git_oid id;

	const char *new_head = "refs/heads/new-head";

	git_oid_fromstr(&id, "deadbeef3f795b2b4353bcce3a527ad0a4f7f644");

	/* Create and write the new object id reference */
	cl_git_fail(git_reference_create(&new_reference, g_repo, new_head, &id, 0, NULL, NULL));

	/* Ensure the reference can't be looked-up... */
	cl_git_fail(git_reference_lookup(&looked_up_ref, g_repo, new_head));
}

void test_refs_create__propagate_eexists(void)
{
	int error;
	git_oid oid;
	git_reference *ref;

	/* Make sure it works for oid and for symbolic both */
	git_oid_fromstr(&oid, current_master_tip);
	error = git_reference_create(&ref, g_repo, current_head_target, &oid, false, NULL, NULL);
	cl_assert(error == GIT_EEXISTS);

	error = git_reference_symbolic_create(&ref, g_repo, "HEAD", current_head_target, false, NULL, NULL);
	cl_assert(error == GIT_EEXISTS);
}

void test_refs_create__creating_a_reference_with_an_invalid_name_returns_EINVALIDSPEC(void)
{
	git_reference *new_reference;
	git_oid id;

	const char *name = "refs/heads/inv@{id";

	git_oid_fromstr(&id, current_master_tip);

	cl_assert_equal_i(GIT_EINVALIDSPEC, git_reference_create(
		&new_reference, g_repo, name, &id, 0, NULL, NULL));

	cl_assert_equal_i(GIT_EINVALIDSPEC, git_reference_symbolic_create(
		&new_reference, g_repo, name, current_head_target, 0, NULL, NULL));
}
