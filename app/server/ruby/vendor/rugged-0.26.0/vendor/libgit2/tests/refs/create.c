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
	p_fsync__cnt = 0;
}

void test_refs_create__cleanup(void)
{
	cl_git_sandbox_cleanup();

	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_OBJECT_CREATION, 1));
	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_SYMBOLIC_REF_CREATION, 1));
	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_FSYNC_GITDIR, 0));
}

void test_refs_create__symbolic(void)
{
	/* create a new symbolic reference */
	git_reference *new_reference, *looked_up_ref, *resolved_ref;
	git_repository *repo2;
	git_oid id;

	const char *new_head_tracker = "ANOTHER_HEAD_TRACKER";

	git_oid_fromstr(&id, current_master_tip);

	/* Create and write the new symbolic reference */
	cl_git_pass(git_reference_symbolic_create(&new_reference, g_repo, new_head_tracker, current_head_target, 0, NULL));

	/* Ensure the reference can be looked-up... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, new_head_tracker));
	cl_assert(git_reference_type(looked_up_ref) & GIT_REF_SYMBOLIC);
	cl_assert(reference_is_packed(looked_up_ref) == 0);
	cl_assert_equal_s(looked_up_ref->name, new_head_tracker);

	/* ...peeled.. */
	cl_git_pass(git_reference_resolve(&resolved_ref, looked_up_ref));
	cl_assert(git_reference_type(resolved_ref) == GIT_REF_OID);

	/* ...and that it points to the current master tip */
	cl_assert_equal_oid(&id, git_reference_target(resolved_ref));
	git_reference_free(looked_up_ref);
	git_reference_free(resolved_ref);

	/* Similar test with a fresh new repository */
	cl_git_pass(git_repository_open(&repo2, "testrepo"));

	cl_git_pass(git_reference_lookup(&looked_up_ref, repo2, new_head_tracker));
	cl_git_pass(git_reference_resolve(&resolved_ref, looked_up_ref));
	cl_assert_equal_oid(&id, git_reference_target(resolved_ref));

	git_repository_free(repo2);

	git_reference_free(new_reference);
	git_reference_free(looked_up_ref);
	git_reference_free(resolved_ref);
}

void test_refs_create__symbolic_with_arbitrary_content(void)
{
	git_reference *new_reference, *looked_up_ref;
	git_repository *repo2;
	git_oid id;

	const char *new_head_tracker = "ANOTHER_HEAD_TRACKER";
	const char *arbitrary_target = "ARBITRARY DATA";

	git_oid_fromstr(&id, current_master_tip);

	/* Attempt to create symbolic ref with arbitrary data in target
	 * fails by default
	 */
	cl_git_fail(git_reference_symbolic_create(&new_reference, g_repo, new_head_tracker, arbitrary_target, 0, NULL));

	git_libgit2_opts(GIT_OPT_ENABLE_STRICT_SYMBOLIC_REF_CREATION, 0);

	/* With strict target validation disabled, ref creation succeeds */
	cl_git_pass(git_reference_symbolic_create(&new_reference, g_repo, new_head_tracker, arbitrary_target, 0, NULL));

	/* Ensure the reference can be looked-up... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, new_head_tracker));
	cl_assert(git_reference_type(looked_up_ref) & GIT_REF_SYMBOLIC);
	cl_assert(reference_is_packed(looked_up_ref) == 0);
	cl_assert_equal_s(looked_up_ref->name, new_head_tracker);
	git_reference_free(looked_up_ref);

	/* Ensure the target is what we expect it to be */
	cl_assert_equal_s(git_reference_symbolic_target(new_reference), arbitrary_target);

	/* Similar test with a fresh new repository object */
	cl_git_pass(git_repository_open(&repo2, "testrepo"));

	/* Ensure the reference can be looked-up... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, repo2, new_head_tracker));
	cl_assert(git_reference_type(looked_up_ref) & GIT_REF_SYMBOLIC);
	cl_assert(reference_is_packed(looked_up_ref) == 0);
	cl_assert_equal_s(looked_up_ref->name, new_head_tracker);

	/* Ensure the target is what we expect it to be */
	cl_assert_equal_s(git_reference_symbolic_target(new_reference), arbitrary_target);

	git_repository_free(repo2);
	git_reference_free(new_reference);
	git_reference_free(looked_up_ref);
}

void test_refs_create__deep_symbolic(void)
{
	/* create a deep symbolic reference */
	git_reference *new_reference, *looked_up_ref, *resolved_ref;
	git_oid id;

	const char *new_head_tracker = "deep/rooted/tracker";

	git_oid_fromstr(&id, current_master_tip);

	cl_git_pass(git_reference_symbolic_create(&new_reference, g_repo, new_head_tracker, current_head_target, 0, NULL));
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, new_head_tracker));
	cl_git_pass(git_reference_resolve(&resolved_ref, looked_up_ref));
	cl_assert_equal_oid(&id, git_reference_target(resolved_ref));

	git_reference_free(new_reference);
	git_reference_free(looked_up_ref);
	git_reference_free(resolved_ref);
}

void test_refs_create__oid(void)
{
	/* create a new OID reference */
	git_reference *new_reference, *looked_up_ref;
	git_repository *repo2;
	git_oid id;

	const char *new_head = "refs/heads/new-head";

	git_oid_fromstr(&id, current_master_tip);

	/* Create and write the new object id reference */
	cl_git_pass(git_reference_create(&new_reference, g_repo, new_head, &id, 0, NULL));

	/* Ensure the reference can be looked-up... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, new_head));
	cl_assert(git_reference_type(looked_up_ref) & GIT_REF_OID);
	cl_assert(reference_is_packed(looked_up_ref) == 0);
	cl_assert_equal_s(looked_up_ref->name, new_head);

	/* ...and that it points to the current master tip */
	cl_assert_equal_oid(&id, git_reference_target(looked_up_ref));
	git_reference_free(looked_up_ref);

	/* Similar test with a fresh new repository */
	cl_git_pass(git_repository_open(&repo2, "testrepo"));

	cl_git_pass(git_reference_lookup(&looked_up_ref, repo2, new_head));
	cl_assert_equal_oid(&id, git_reference_target(looked_up_ref));

	git_repository_free(repo2);

	git_reference_free(new_reference);
	git_reference_free(looked_up_ref);
}

/* Can by default create a reference that targets at an unknown id */
void test_refs_create__oid_unknown_succeeds_without_strict(void)
{
	git_reference *new_reference, *looked_up_ref;
	git_oid id;

	const char *new_head = "refs/heads/new-head";

	git_oid_fromstr(&id, "deadbeef3f795b2b4353bcce3a527ad0a4f7f644");

	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_OBJECT_CREATION, 0));

	/* Create and write the new object id reference */
	cl_git_pass(git_reference_create(&new_reference, g_repo, new_head, &id, 0, NULL));
	git_reference_free(new_reference);

	/* Ensure the reference can't be looked-up... */
	cl_git_pass(git_reference_lookup(&looked_up_ref, g_repo, new_head));
	git_reference_free(looked_up_ref);
}

/* Strict object enforcement enforces valid object id */
void test_refs_create__oid_unknown_fails_by_default(void)
{
	git_reference *new_reference, *looked_up_ref;
	git_oid id;

	const char *new_head = "refs/heads/new-head";

	git_oid_fromstr(&id, "deadbeef3f795b2b4353bcce3a527ad0a4f7f644");

	/* Create and write the new object id reference */
	cl_git_fail(git_reference_create(&new_reference, g_repo, new_head, &id, 0, NULL));

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
	error = git_reference_create(&ref, g_repo, current_head_target, &oid, false, NULL);
	cl_assert(error == GIT_EEXISTS);

	error = git_reference_symbolic_create(&ref, g_repo, "HEAD", current_head_target, false, NULL);
	cl_assert(error == GIT_EEXISTS);
}

void test_refs_create__existing_dir_propagates_edirectory(void)
{
	git_reference *new_reference, *fail_reference;
	git_oid id;
	const char *dir_head = "refs/heads/new-dir/new-head",
		*fail_head = "refs/heads/new-dir";

	git_oid_fromstr(&id, current_master_tip);

	/* Create and write the new object id reference */
	cl_git_pass(git_reference_create(&new_reference, g_repo, dir_head, &id, 1, NULL));
	cl_git_fail_with(GIT_EDIRECTORY,
		git_reference_create(&fail_reference, g_repo, fail_head, &id, false, NULL));

	git_reference_free(new_reference);
}

static void test_invalid_name(const char *name)
{
	git_reference *new_reference;
	git_oid id;

	git_oid_fromstr(&id, current_master_tip);

	cl_assert_equal_i(GIT_EINVALIDSPEC, git_reference_create(
		&new_reference, g_repo, name, &id, 0, NULL));

	cl_assert_equal_i(GIT_EINVALIDSPEC, git_reference_symbolic_create(
		&new_reference, g_repo, name, current_head_target, 0, NULL));
}

void test_refs_create__creating_a_reference_with_an_invalid_name_returns_EINVALIDSPEC(void)
{
	test_invalid_name("refs/heads/inv@{id");
	test_invalid_name("refs/heads/back\\slash");

	test_invalid_name("refs/heads/foo ");
	test_invalid_name("refs/heads/foo /bar");
	test_invalid_name("refs/heads/com1:bar/foo");

	test_invalid_name("refs/heads/e:");
	test_invalid_name("refs/heads/c:/foo");

	test_invalid_name("refs/heads/foo.");
}

static void test_win32_name(const char *name)
{
	git_reference *new_reference = NULL;
	git_oid id;
	int ret;

	git_oid_fromstr(&id, current_master_tip);

	ret = git_reference_create(&new_reference, g_repo, name, &id, 0, NULL);

#ifdef GIT_WIN32
	cl_assert_equal_i(GIT_EINVALIDSPEC, ret);
#else
	cl_git_pass(ret);
#endif

	git_reference_free(new_reference);
}

void test_refs_create__creating_a_loose_ref_with_invalid_windows_name(void)
{
	test_win32_name("refs/heads/foo./bar");

	test_win32_name("refs/heads/aux");
	test_win32_name("refs/heads/aux.foo/bar");

	test_win32_name("refs/heads/com1");
}

/* Creating a loose ref involves fsync'ing the reference, the
 * reflog and (on non-Windows) the containing directories.
 * Creating a packed ref involves fsync'ing the packed ref file
 * and (on non-Windows) the containing directory.
 */
#ifdef GIT_WIN32
static int expected_fsyncs_create = 2, expected_fsyncs_compress = 1;
#else
static int expected_fsyncs_create = 4, expected_fsyncs_compress = 2;
#endif

static void count_fsyncs(size_t *create_count, size_t *compress_count)
{
	git_reference *ref = NULL;
	git_refdb *refdb;
	git_oid id;

	p_fsync__cnt = 0;

	git_oid_fromstr(&id, current_master_tip);
	cl_git_pass(git_reference_create(&ref, g_repo, "refs/heads/fsync_test", &id, 0, "log message"));
	git_reference_free(ref);

	*create_count = p_fsync__cnt;
	p_fsync__cnt = 0;

	cl_git_pass(git_repository_refdb(&refdb, g_repo));
	cl_git_pass(git_refdb_compress(refdb));
	git_refdb_free(refdb);

	*compress_count = p_fsync__cnt;
	p_fsync__cnt = 0;
}

void test_refs_create__does_not_fsync_by_default(void)
{
	size_t create_count, compress_count;
	count_fsyncs(&create_count, &compress_count);

	cl_assert_equal_i(0, create_count);
	cl_assert_equal_i(0, compress_count);
}

void test_refs_create__fsyncs_when_global_opt_set(void)
{
	size_t create_count, compress_count;

	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_FSYNC_GITDIR, 1));
	count_fsyncs(&create_count, &compress_count);

	cl_assert_equal_i(expected_fsyncs_create, create_count);
	cl_assert_equal_i(expected_fsyncs_compress, compress_count);
}

void test_refs_create__fsyncs_when_repo_config_set(void)
{
	size_t create_count, compress_count;

	cl_repo_set_bool(g_repo, "core.fsyncObjectFiles", true);

	count_fsyncs(&create_count, &compress_count);

	cl_assert_equal_i(expected_fsyncs_create, create_count);
	cl_assert_equal_i(expected_fsyncs_compress, compress_count);
}
