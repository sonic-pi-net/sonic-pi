#include "clar_libgit2.h"

#include "repository.h"

static git_repository *g_repo;

void test_object_lookup__initialize(void)
{
   cl_git_pass(git_repository_open(&g_repo, cl_fixture("testrepo.git")));
}

void test_object_lookup__cleanup(void)
{
	git_repository_free(g_repo);
	g_repo = NULL;
}

void test_object_lookup__lookup_wrong_type_returns_enotfound(void)
{
	const char *commit = "e90810b8df3e80c413d903f631643c716887138d";
	git_oid oid;
	git_object *object;

	cl_git_pass(git_oid_fromstr(&oid, commit));
	cl_assert_equal_i(
		GIT_ENOTFOUND, git_object_lookup(&object, g_repo, &oid, GIT_OBJ_TAG));
}

void test_object_lookup__lookup_nonexisting_returns_enotfound(void)
{
	const char *unknown = "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef";
	git_oid oid;
	git_object *object;

	cl_git_pass(git_oid_fromstr(&oid, unknown));
	cl_assert_equal_i(
		GIT_ENOTFOUND, git_object_lookup(&object, g_repo, &oid, GIT_OBJ_ANY));
}

void test_object_lookup__lookup_wrong_type_by_abbreviated_id_returns_enotfound(void)
{
	const char *commit = "e90810b";
	git_oid oid;
	git_object *object;

	cl_git_pass(git_oid_fromstrn(&oid, commit, strlen(commit)));
	cl_assert_equal_i(
		GIT_ENOTFOUND, git_object_lookup_prefix(&object, g_repo, &oid, strlen(commit), GIT_OBJ_TAG));
}

void test_object_lookup__lookup_wrong_type_eventually_returns_enotfound(void)
{
	const char *commit = "e90810b8df3e80c413d903f631643c716887138d";
	git_oid oid;
	git_object *object;

	cl_git_pass(git_oid_fromstr(&oid, commit));

	cl_git_pass(git_object_lookup(&object, g_repo, &oid, GIT_OBJ_COMMIT));
	git_object_free(object);

	cl_assert_equal_i(
		GIT_ENOTFOUND, git_object_lookup(&object, g_repo, &oid, GIT_OBJ_TAG));
}

