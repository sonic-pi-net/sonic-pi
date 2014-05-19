#include "clar_libgit2.h"

#include "fileops.h"
#include "git2/reflog.h"
#include "git2/refdb.h"
#include "reflog.h"
#include "refs.h"
#include "ref_helpers.h"

static const char *loose_tag_ref_name = "refs/tags/e90810b";

static git_repository *g_repo;

void test_refs_pack__initialize(void)
{
   g_repo = cl_git_sandbox_init("testrepo");
}

void test_refs_pack__cleanup(void)
{
   cl_git_sandbox_cleanup();
}

static void packall(void)
{
	git_refdb *refdb;

	cl_git_pass(git_repository_refdb(&refdb, g_repo));
	cl_git_pass(git_refdb_compress(refdb));
	git_refdb_free(refdb);
}

void test_refs_pack__empty(void)
{
	/* create a packfile for an empty folder */
	git_buf temp_path = GIT_BUF_INIT;

	cl_git_pass(git_buf_join_n(&temp_path, '/', 3, git_repository_path(g_repo), GIT_REFS_HEADS_DIR, "empty_dir"));
	cl_git_pass(git_futils_mkdir_r(temp_path.ptr, NULL, GIT_REFS_DIR_MODE));
	git_buf_free(&temp_path);

	packall();
}

void test_refs_pack__loose(void)
{
	/* create a packfile from all the loose refs in a repo */
	git_reference *reference;
	git_buf temp_path = GIT_BUF_INIT;

	/* Ensure a known loose ref can be looked up */
	cl_git_pass(git_reference_lookup(&reference, g_repo, loose_tag_ref_name));
	cl_assert(reference_is_packed(reference) == 0);
	cl_assert_equal_s(reference->name, loose_tag_ref_name);
	git_reference_free(reference);

	/*
	 * We are now trying to pack also a loose reference
	 * called `points_to_blob`, to make sure we can properly
	 * pack weak tags
	 */
	packall();

	/* Ensure the packed-refs file exists */
	cl_git_pass(git_buf_joinpath(&temp_path, git_repository_path(g_repo), GIT_PACKEDREFS_FILE));
	cl_assert(git_path_exists(temp_path.ptr));

	/* Ensure the known ref can still be looked up but is now packed */
	cl_git_pass(git_reference_lookup(&reference, g_repo, loose_tag_ref_name));
	cl_assert(reference_is_packed(reference));
	cl_assert_equal_s(reference->name, loose_tag_ref_name);

	/* Ensure the known ref has been removed from the loose folder structure */
	cl_git_pass(git_buf_joinpath(&temp_path, git_repository_path(g_repo), loose_tag_ref_name));
	cl_assert(!git_path_exists(temp_path.ptr));

	git_reference_free(reference);
	git_buf_free(&temp_path);
}

void test_refs_pack__symbolic(void)
{
	/* create a packfile from loose refs skipping symbolic refs */
	int i;
	git_oid head;
	git_reference *ref;
	char name[128];

	cl_git_pass(git_reference_name_to_id(&head, g_repo, "HEAD"));

	/* make a bunch of references */

	for (i = 0; i < 100; ++i) {
		snprintf(name, sizeof(name), "refs/heads/symbolic-%03d", i);
		cl_git_pass(git_reference_symbolic_create(
			&ref, g_repo, name, "refs/heads/master", 0, NULL, NULL));
		git_reference_free(ref);

		snprintf(name, sizeof(name), "refs/heads/direct-%03d", i);
		cl_git_pass(git_reference_create(&ref, g_repo, name, &head, 0, NULL, NULL));
		git_reference_free(ref);
	}

	packall();
}
