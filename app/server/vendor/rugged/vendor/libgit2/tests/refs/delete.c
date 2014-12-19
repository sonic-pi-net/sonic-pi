#include "clar_libgit2.h"

#include "fileops.h"
#include "git2/reflog.h"
#include "git2/refdb.h"
#include "reflog.h"
#include "ref_helpers.h"

static const char *packed_test_head_name = "refs/heads/packed-test";
static const char *current_master_tip = "a65fedf39aefe402d3bb6e24df4d4f5fe4547750";

static git_repository *g_repo;



void test_refs_delete__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
}

void test_refs_delete__cleanup(void)
{
   cl_git_sandbox_cleanup();
}



void test_refs_delete__packed_loose(void)
{
   // deleting a ref which is both packed and loose should remove both tracks in the filesystem
	git_reference *looked_up_ref, *another_looked_up_ref;
	git_buf temp_path = GIT_BUF_INIT;

	/* Ensure the loose reference exists on the file system */
	cl_git_pass(git_buf_joinpath(&temp_path, git_repository_path(g_repo), packed_test_head_name));
	cl_assert(git_path_exists(temp_path.ptr));

	/* Lookup the reference */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, packed_test_head_name));

	/* Ensure it's the loose version that has been found */
	cl_assert(reference_is_packed(looked_up_ref) == 0);

	/* Now that the reference is deleted... */
	cl_git_pass(git_reference_delete(looked_up_ref));
	git_reference_free(looked_up_ref);

	/* Looking up the reference once again should not retrieve it */
	cl_git_fail(git_reference_lookup(&another_looked_up_ref, g_repo, packed_test_head_name));

	/* Ensure the loose reference doesn't exist any longer on the file system */
	cl_assert(!git_path_exists(temp_path.ptr));

	git_reference_free(another_looked_up_ref);
	git_buf_free(&temp_path);
}

void test_refs_delete__packed_only(void)
{
   // can delete a just packed reference
	git_reference *ref;
	git_refdb *refdb;
	git_oid id;
	const char *new_ref = "refs/heads/new_ref";

	git_oid_fromstr(&id, current_master_tip);

	/* Create and write the new object id reference */
	cl_git_pass(git_reference_create(&ref, g_repo, new_ref, &id, 0, NULL, NULL));
	git_reference_free(ref);

	/* Lookup the reference */
	cl_git_pass(git_reference_lookup(&ref, g_repo, new_ref));

	/* Ensure it's a loose reference */
	cl_assert(reference_is_packed(ref) == 0);

	/* Pack all existing references */
	cl_git_pass(git_repository_refdb(&refdb, g_repo));
	cl_git_pass(git_refdb_compress(refdb));

	/* Reload the reference from disk */
	git_reference_free(ref);
	cl_git_pass(git_reference_lookup(&ref, g_repo, new_ref));

	/* Ensure it's a packed reference */
	cl_assert(reference_is_packed(ref) == 1);

	/* This should pass */
	cl_git_pass(git_reference_delete(ref));
	git_reference_free(ref);
	git_refdb_free(refdb);
}

void test_refs_delete__remove(void)
{
	git_reference *ref;

	/* Check that passing no old values lets us delete */

	cl_git_pass(git_reference_lookup(&ref, g_repo, packed_test_head_name));
	git_reference_free(ref);

	cl_git_pass(git_reference_remove(g_repo, packed_test_head_name));

	cl_git_fail(git_reference_lookup(&ref, g_repo, packed_test_head_name));
}
