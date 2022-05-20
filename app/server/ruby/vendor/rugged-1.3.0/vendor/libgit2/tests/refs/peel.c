#include "clar_libgit2.h"

static git_repository *g_repo;
static git_repository *g_peel_repo;

void test_refs_peel__initialize(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_repository_open(&g_peel_repo, cl_fixture("peeled.git")));
}

void test_refs_peel__cleanup(void)
{
	git_repository_free(g_repo);
	g_repo = NULL;
	git_repository_free(g_peel_repo);
	g_peel_repo = NULL;
}

static void assert_peel_generic(
	git_repository *repo,
	const char *ref_name,
	git_object_t requested_type,
	const char* expected_sha,
	git_object_t expected_type)
{
	git_oid expected_oid;
	git_reference *ref;
	git_object *peeled;

	cl_git_pass(git_reference_lookup(&ref, repo, ref_name));

	cl_git_pass(git_reference_peel(&peeled, ref, requested_type));

	cl_git_pass(git_oid_fromstr(&expected_oid, expected_sha));
	cl_assert_equal_oid(&expected_oid, git_object_id(peeled));

	cl_assert_equal_i(expected_type, git_object_type(peeled));

	git_object_free(peeled);
	git_reference_free(ref);
}

static void assert_peel(
	const char *ref_name,
	git_object_t requested_type,
	const char* expected_sha,
	git_object_t expected_type)
{
	assert_peel_generic(g_repo, ref_name, requested_type,
			    expected_sha, expected_type);
}

static void assert_peel_error(int error, const char *ref_name, git_object_t requested_type)
{
	git_reference *ref;
	git_object *peeled;

	cl_git_pass(git_reference_lookup(&ref, g_repo, ref_name));

	cl_assert_equal_i(error, git_reference_peel(&peeled, ref, requested_type));

	git_reference_free(ref);
}

void test_refs_peel__can_peel_a_tag(void)
{
	assert_peel("refs/tags/test", GIT_OBJECT_TAG,
		"b25fa35b38051e4ae45d4222e795f9df2e43f1d1", GIT_OBJECT_TAG);
	assert_peel("refs/tags/test", GIT_OBJECT_COMMIT,
		"e90810b8df3e80c413d903f631643c716887138d", GIT_OBJECT_COMMIT);
	assert_peel("refs/tags/test", GIT_OBJECT_TREE,
		"53fc32d17276939fc79ed05badaef2db09990016", GIT_OBJECT_TREE);
	assert_peel("refs/tags/point_to_blob", GIT_OBJECT_BLOB,
		"1385f264afb75a56a5bec74243be9b367ba4ca08", GIT_OBJECT_BLOB);
}

void test_refs_peel__can_peel_a_branch(void)
{
	assert_peel("refs/heads/master", GIT_OBJECT_COMMIT,
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750", GIT_OBJECT_COMMIT);
	assert_peel("refs/heads/master", GIT_OBJECT_TREE,
		"944c0f6e4dfa41595e6eb3ceecdb14f50fe18162", GIT_OBJECT_TREE);
}

void test_refs_peel__can_peel_a_symbolic_reference(void)
{
	assert_peel("HEAD", GIT_OBJECT_COMMIT,
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750", GIT_OBJECT_COMMIT);
	assert_peel("HEAD", GIT_OBJECT_TREE,
		"944c0f6e4dfa41595e6eb3ceecdb14f50fe18162", GIT_OBJECT_TREE);
}

void test_refs_peel__cannot_peel_into_a_non_existing_target(void)
{
	assert_peel_error(GIT_EINVALIDSPEC, "refs/tags/point_to_blob", GIT_OBJECT_TAG);
}

void test_refs_peel__can_peel_into_any_non_tag_object(void)
{
	assert_peel("refs/heads/master", GIT_OBJECT_ANY,
		"a65fedf39aefe402d3bb6e24df4d4f5fe4547750", GIT_OBJECT_COMMIT);
	assert_peel("refs/tags/point_to_blob", GIT_OBJECT_ANY,
		"1385f264afb75a56a5bec74243be9b367ba4ca08", GIT_OBJECT_BLOB);
	assert_peel("refs/tags/test", GIT_OBJECT_ANY,
		"e90810b8df3e80c413d903f631643c716887138d", GIT_OBJECT_COMMIT);
}

void test_refs_peel__can_peel_fully_peeled_packed_refs(void)
{
	assert_peel_generic(g_peel_repo,
			    "refs/tags/tag-inside-tags", GIT_OBJECT_ANY,
			    "0df1a5865c8abfc09f1f2182e6a31be550e99f07",
			    GIT_OBJECT_COMMIT);
	assert_peel_generic(g_peel_repo,
			    "refs/foo/tag-outside-tags", GIT_OBJECT_ANY,
			    "0df1a5865c8abfc09f1f2182e6a31be550e99f07",
			    GIT_OBJECT_COMMIT);
}

void test_refs_peel__can_peel_fully_peeled_tag_to_tag(void)
{
	assert_peel_generic(g_peel_repo,
			    "refs/tags/tag-inside-tags", GIT_OBJECT_TAG,
			    "c2596aa0151888587ec5c0187f261e63412d9e11",
			    GIT_OBJECT_TAG);
	assert_peel_generic(g_peel_repo,
			    "refs/foo/tag-outside-tags", GIT_OBJECT_TAG,
			    "c2596aa0151888587ec5c0187f261e63412d9e11",
			    GIT_OBJECT_TAG);
}
