#include "clar_libgit2.h"

#include "fileops.h"
#include "git2/reflog.h"
#include "reflog.h"
#include "refs.h"
#include "ref_helpers.h"

static const char *loose_tag_ref_name = "refs/tags/e90810b";
static const char *packed_head_name = "refs/heads/packed";
static const char *packed_test_head_name = "refs/heads/packed-test";
static const char *ref_one_name = "refs/heads/one/branch";
static const char *ref_one_name_new = "refs/heads/two/branch";
static const char *ref_two_name = "refs/heads/two";
static const char *ref_master_name = "refs/heads/master";
static const char *ref_two_name_new = "refs/heads/two/two";

static git_repository *g_repo;



void test_refs_rename__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");
}

void test_refs_rename__cleanup(void)
{
	cl_git_sandbox_cleanup();
}



void test_refs_rename__loose(void)
{
	// rename a loose reference
	git_reference *looked_up_ref, *new_ref, *another_looked_up_ref;
	git_buf temp_path = GIT_BUF_INIT;
	const char *new_name = "refs/tags/Nemo/knows/refs.kung-fu";

	/* Ensure the ref doesn't exist on the file system */
	cl_git_pass(git_buf_joinpath(&temp_path, git_repository_path(g_repo), new_name));
	cl_assert(!git_path_exists(temp_path.ptr));

	/* Retrieval of the reference to rename */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, loose_tag_ref_name));

	/* ... which is indeed loose */
	cl_assert(reference_is_packed(looked_up_ref) == 0);

	/* Now that the reference is renamed... */
	cl_git_pass(git_reference_rename(&new_ref, looked_up_ref, new_name, 0, NULL, NULL));
	cl_assert_equal_s(new_ref->name, new_name);
	git_reference_free(looked_up_ref);

	/* ...It can't be looked-up with the old name... */
	cl_git_fail(git_reference_lookup(&another_looked_up_ref, g_repo, loose_tag_ref_name));

	/* ...but the new name works ok... */
	cl_git_pass(git_reference_lookup(&another_looked_up_ref, g_repo, new_name));
	cl_assert_equal_s(new_ref->name, new_name);

	/* .. the new ref is loose... */
	cl_assert(reference_is_packed(another_looked_up_ref) == 0);
	cl_assert(reference_is_packed(new_ref) == 0);

	/* ...and the ref can be found in the file system */
	cl_git_pass(git_buf_joinpath(&temp_path, git_repository_path(g_repo), new_name));
	cl_assert(git_path_exists(temp_path.ptr));

	git_reference_free(new_ref);
	git_reference_free(another_looked_up_ref);
	git_buf_free(&temp_path);
}

void test_refs_rename__packed(void)
{
	// rename a packed reference (should make it loose)
	git_reference *looked_up_ref, *new_ref, *another_looked_up_ref;
	git_buf temp_path = GIT_BUF_INIT;
	const char *brand_new_name = "refs/heads/brand_new_name";

	/* Ensure the ref doesn't exist on the file system */
	cl_git_pass(git_buf_joinpath(&temp_path, git_repository_path(g_repo), packed_head_name));
	cl_assert(!git_path_exists(temp_path.ptr));

	/* The reference can however be looked-up... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, packed_head_name));

	/* .. and it's packed */
	cl_assert(reference_is_packed(looked_up_ref) != 0);

	/* Now that the reference is renamed... */
	cl_git_pass(git_reference_rename(&new_ref, looked_up_ref, brand_new_name, 0, NULL, NULL));
	cl_assert_equal_s(new_ref->name, brand_new_name);
	git_reference_free(looked_up_ref);

	/* ...It can't be looked-up with the old name... */
	cl_git_fail(git_reference_lookup(&another_looked_up_ref, g_repo, packed_head_name));

	/* ...but the new name works ok... */
	cl_git_pass(git_reference_lookup(&another_looked_up_ref, g_repo, brand_new_name));
	cl_assert_equal_s(another_looked_up_ref->name, brand_new_name);

	/* .. the ref is no longer packed... */
	cl_assert(reference_is_packed(another_looked_up_ref) == 0);
	cl_assert(reference_is_packed(new_ref) == 0);

	/* ...and the ref now happily lives in the file system */
	cl_git_pass(git_buf_joinpath(&temp_path, git_repository_path(g_repo), brand_new_name));
	cl_assert(git_path_exists(temp_path.ptr));

	git_reference_free(new_ref);
	git_reference_free(another_looked_up_ref);
	git_buf_free(&temp_path);
}

void test_refs_rename__packed_doesnt_pack_others(void)
{
	// renaming a packed reference does not pack another reference which happens to be in both loose and pack state
	git_reference *looked_up_ref, *another_looked_up_ref, *renamed_ref;
	git_buf temp_path = GIT_BUF_INIT;
	const char *brand_new_name = "refs/heads/brand_new_name";

	/* Ensure the other reference exists on the file system */
	cl_git_pass(git_buf_joinpath(&temp_path, git_repository_path(g_repo), packed_test_head_name));
	cl_assert(git_path_exists(temp_path.ptr));

	/* Lookup the other reference */
	cl_git_pass(git_reference_lookup(&another_looked_up_ref, g_repo, packed_test_head_name));

	/* Ensure it's loose */
	cl_assert(reference_is_packed(another_looked_up_ref) == 0);
	git_reference_free(another_looked_up_ref);

	/* Lookup the reference to rename */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, packed_head_name));

	/* Ensure it's packed */
	cl_assert(reference_is_packed(looked_up_ref) != 0);

	/* Now that the reference is renamed... */
	cl_git_pass(git_reference_rename(&renamed_ref, looked_up_ref, brand_new_name, 0, NULL, NULL));
	git_reference_free(looked_up_ref);

	/* Lookup the other reference */
	cl_git_pass(git_reference_lookup(&another_looked_up_ref, g_repo, packed_test_head_name));

	/* Ensure it's loose */
	cl_assert(reference_is_packed(another_looked_up_ref) == 0);

	/* Ensure the other ref still exists on the file system */
	cl_assert(git_path_exists(temp_path.ptr));

	git_reference_free(renamed_ref);
	git_reference_free(another_looked_up_ref);
	git_buf_free(&temp_path);
}

void test_refs_rename__name_collision(void)
{
	// can not rename a reference with the name of an existing reference
	git_reference *looked_up_ref, *renamed_ref;

	/* An existing reference... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, packed_head_name));

	/* Can not be renamed to the name of another existing reference. */
	cl_git_fail(git_reference_rename(&renamed_ref, looked_up_ref, packed_test_head_name, 0, NULL, NULL));
	git_reference_free(looked_up_ref);

	/* Failure to rename it hasn't corrupted its state */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, packed_head_name));
	cl_assert_equal_s(looked_up_ref->name, packed_head_name);

	git_reference_free(looked_up_ref);
}

void test_refs_rename__invalid_name(void)
{
	// can not rename a reference with an invalid name
	git_reference *looked_up_ref, *renamed_ref;

	/* An existing oid reference... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, packed_test_head_name));

	/* Can not be renamed with an invalid name. */
	cl_assert_equal_i(
		GIT_EINVALIDSPEC,
		git_reference_rename(&renamed_ref, looked_up_ref, "Hello! I'm a very invalid name.", 0, NULL, NULL));

	/* Can not be renamed outside of the refs hierarchy
	 * unless it's ALL_CAPS_AND_UNDERSCORES.
	 */
	cl_assert_equal_i(GIT_EINVALIDSPEC, git_reference_rename(&renamed_ref, looked_up_ref, "i-will-sudo-you", 0, NULL, NULL));

	/* Failure to rename it hasn't corrupted its state */
	git_reference_free(looked_up_ref);
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, packed_test_head_name));
	cl_assert_equal_s(looked_up_ref->name, packed_test_head_name);

	git_reference_free(looked_up_ref);
}

void test_refs_rename__force_loose_packed(void)
{
	// can force-rename a packed reference with the name of an existing loose and packed reference
	git_reference *looked_up_ref, *renamed_ref;
	git_oid oid;

	/* An existing reference... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, packed_head_name));
	git_oid_cpy(&oid, git_reference_target(looked_up_ref));

	/* Can be force-renamed to the name of another existing reference. */
	cl_git_pass(git_reference_rename(&renamed_ref, looked_up_ref, packed_test_head_name, 1, NULL, NULL));
	git_reference_free(looked_up_ref);
	git_reference_free(renamed_ref);

	/* Check we actually renamed it */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, packed_test_head_name));
	cl_assert_equal_s(looked_up_ref->name, packed_test_head_name);
	cl_assert_equal_oid(&oid, git_reference_target(looked_up_ref));
	git_reference_free(looked_up_ref);

	/* And that the previous one doesn't exist any longer */
	cl_git_fail(git_reference_lookup(&looked_up_ref, g_repo, packed_head_name));
}

void test_refs_rename__force_loose(void)
{
	// can force-rename a loose reference with the name of an existing loose reference
	git_reference *looked_up_ref, *renamed_ref;
	git_oid oid;

	/* An existing reference... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, "refs/heads/br2"));
	git_oid_cpy(&oid, git_reference_target(looked_up_ref));

	/* Can be force-renamed to the name of another existing reference. */
	cl_git_pass(git_reference_rename(&renamed_ref, looked_up_ref, "refs/heads/test", 1, NULL, NULL));
	git_reference_free(looked_up_ref);
	git_reference_free(renamed_ref);

	/* Check we actually renamed it */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, "refs/heads/test"));
	cl_assert_equal_s(looked_up_ref->name,  "refs/heads/test");
	cl_assert_equal_oid(&oid, git_reference_target(looked_up_ref));
	git_reference_free(looked_up_ref);

	/* And that the previous one doesn't exist any longer */
	cl_git_fail(git_reference_lookup(&looked_up_ref, g_repo, "refs/heads/br2"));

	git_reference_free(looked_up_ref);
}


void test_refs_rename__overwrite(void)
{
	// can not overwrite name of existing reference
	git_reference *ref, *ref_one, *ref_one_new, *ref_two;
	git_refdb *refdb;
	git_oid id;

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_master_name));
	cl_assert(git_reference_type(ref) & GIT_REF_OID);

	git_oid_cpy(&id, git_reference_target(ref));

	/* Create loose references */
	cl_git_pass(git_reference_create(&ref_one, g_repo, ref_one_name, &id, 0, NULL, NULL));
	cl_git_pass(git_reference_create(&ref_two, g_repo, ref_two_name, &id, 0, NULL, NULL));

	/* Pack everything */
	cl_git_pass(git_repository_refdb(&refdb, g_repo));
	cl_git_pass(git_refdb_compress(refdb));

	/* Attempt to create illegal reference */
	cl_git_fail(git_reference_create(&ref_one_new, g_repo, ref_one_name_new, &id, 0, NULL, NULL));

	/* Illegal reference couldn't be created so this is supposed to fail */
	cl_git_fail(git_reference_lookup(&ref_one_new, g_repo, ref_one_name_new));

	git_reference_free(ref);
	git_reference_free(ref_one);
	git_reference_free(ref_one_new);
	git_reference_free(ref_two);
	git_refdb_free(refdb);
}


void test_refs_rename__prefix(void)
{
	// can be renamed to a new name prefixed with the old name
	git_reference *ref, *ref_two, *looked_up_ref, *renamed_ref;
	git_oid id;

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_master_name));
	cl_assert(git_reference_type(ref) & GIT_REF_OID);

	git_oid_cpy(&id, git_reference_target(ref));

	/* Create loose references */
	cl_git_pass(git_reference_create(&ref_two, g_repo, ref_two_name, &id, 0, NULL, NULL));

	/* An existing reference... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, ref_two_name));

	/* Can be rename to a new name starting with the old name. */
	cl_git_pass(git_reference_rename(&renamed_ref, looked_up_ref, ref_two_name_new, 0, NULL, NULL));
	git_reference_free(looked_up_ref);
	git_reference_free(renamed_ref);

	/* Check we actually renamed it */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, ref_two_name_new));
	cl_assert_equal_s(looked_up_ref->name, ref_two_name_new);
	git_reference_free(looked_up_ref);
	cl_git_fail(git_reference_lookup(&looked_up_ref, g_repo, ref_two_name));

	git_reference_free(ref);
	git_reference_free(ref_two);
	git_reference_free(looked_up_ref);
}

void test_refs_rename__move_up(void)
{
	// can move a reference to a upper reference hierarchy
	git_reference *ref, *ref_two, *looked_up_ref, *renamed_ref;
	git_oid id;

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_master_name));
	cl_assert(git_reference_type(ref) & GIT_REF_OID);

	git_oid_cpy(&id, git_reference_target(ref));

	/* Create loose references */
	cl_git_pass(git_reference_create(&ref_two, g_repo, ref_two_name_new, &id, 0, NULL, NULL));
	git_reference_free(ref_two);

	/* An existing reference... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, ref_two_name_new));

	/* Can be renamed upward the reference tree. */
	cl_git_pass(git_reference_rename(&renamed_ref, looked_up_ref, ref_two_name, 0, NULL, NULL));
	git_reference_free(looked_up_ref);
	git_reference_free(renamed_ref);

	/* Check we actually renamed it */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, ref_two_name));
	cl_assert_equal_s(looked_up_ref->name, ref_two_name);
	git_reference_free(looked_up_ref);

	cl_git_fail(git_reference_lookup(&looked_up_ref, g_repo, ref_two_name_new));
	git_reference_free(ref);
	git_reference_free(looked_up_ref);
}

void test_refs_rename__propagate_eexists(void)
{
	git_reference *ref, *new_ref;

	cl_git_pass(git_reference_lookup(&ref, g_repo, packed_head_name));

	cl_assert_equal_i(GIT_EEXISTS, git_reference_rename(&new_ref, ref, packed_test_head_name, 0, NULL, NULL));

	git_reference_free(ref);
}

void test_refs_rename__writes_to_reflog(void)
{
	git_reference *ref, *new_ref;
	git_reflog *log;
	const git_reflog_entry *entry;
	git_signature *sig;

	cl_git_pass(git_signature_now(&sig, "me", "foo@example.com"));

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_master_name));
	cl_git_pass(git_reference_rename(&new_ref, ref, ref_one_name_new, false,
				sig, "message"));
	cl_git_pass(git_reflog_read(&log, g_repo, git_reference_name(new_ref)));
	entry = git_reflog_entry_byindex(log, 0);
	cl_assert_equal_s("message", git_reflog_entry_message(entry));
	cl_assert_equal_s("foo@example.com", git_reflog_entry_committer(entry)->email);

	git_reflog_free(log);
	git_reference_free(ref);
	git_reference_free(new_ref);
	git_signature_free(sig);
}
