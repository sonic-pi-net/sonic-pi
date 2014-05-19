#include "clar_libgit2.h"

static git_repository *g_repo;

void test_object_peel__initialize(void)
{
	cl_git_pass(git_repository_open(&g_repo, cl_fixture("testrepo.git")));
}

void test_object_peel__cleanup(void)
{
	git_repository_free(g_repo);
	g_repo = NULL;
}

static void assert_peel(
	const char *sha,
	git_otype requested_type,
	const char* expected_sha,
	git_otype expected_type)
{
	git_oid oid, expected_oid;
	git_object *obj;
	git_object *peeled;

	cl_git_pass(git_oid_fromstr(&oid, sha));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));
	
	cl_git_pass(git_object_peel(&peeled, obj, requested_type));

	cl_git_pass(git_oid_fromstr(&expected_oid, expected_sha));
	cl_assert_equal_i(0, git_oid_cmp(&expected_oid, git_object_id(peeled)));

	cl_assert_equal_i(expected_type, git_object_type(peeled));

	git_object_free(peeled);
	git_object_free(obj);
}

static void assert_peel_error(int error, const char *sha, git_otype requested_type)
{
	git_oid oid;
	git_object *obj;
	git_object *peeled;

	cl_git_pass(git_oid_fromstr(&oid, sha));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));
	
	cl_assert_equal_i(error, git_object_peel(&peeled, obj, requested_type));

	git_object_free(obj);
}

void test_object_peel__peeling_an_object_into_its_own_type_returns_another_instance_of_it(void)
{
	assert_peel("e90810b8df3e80c413d903f631643c716887138d", GIT_OBJ_COMMIT,
		"e90810b8df3e80c413d903f631643c716887138d", GIT_OBJ_COMMIT);
	assert_peel("7b4384978d2493e851f9cca7858815fac9b10980", GIT_OBJ_TAG,
		"7b4384978d2493e851f9cca7858815fac9b10980", GIT_OBJ_TAG);
	assert_peel("53fc32d17276939fc79ed05badaef2db09990016", GIT_OBJ_TREE,
		"53fc32d17276939fc79ed05badaef2db09990016", GIT_OBJ_TREE);
	assert_peel("0266163a49e280c4f5ed1e08facd36a2bd716bcf", GIT_OBJ_BLOB,
		"0266163a49e280c4f5ed1e08facd36a2bd716bcf", GIT_OBJ_BLOB);
}

void test_object_peel__can_peel_a_tag(void)
{
	assert_peel("7b4384978d2493e851f9cca7858815fac9b10980", GIT_OBJ_COMMIT,
		"e90810b8df3e80c413d903f631643c716887138d", GIT_OBJ_COMMIT);
	assert_peel("7b4384978d2493e851f9cca7858815fac9b10980", GIT_OBJ_TREE,
		"53fc32d17276939fc79ed05badaef2db09990016", GIT_OBJ_TREE);
}

void test_object_peel__can_peel_a_commit(void)
{
	assert_peel("e90810b8df3e80c413d903f631643c716887138d", GIT_OBJ_TREE,
		"53fc32d17276939fc79ed05badaef2db09990016", GIT_OBJ_TREE);
}

void test_object_peel__cannot_peel_a_tree(void)
{
	assert_peel_error(GIT_EAMBIGUOUS, "53fc32d17276939fc79ed05badaef2db09990016", GIT_OBJ_BLOB);
}

void test_object_peel__cannot_peel_a_blob(void)
{
	assert_peel_error(GIT_ENOTFOUND, "0266163a49e280c4f5ed1e08facd36a2bd716bcf", GIT_OBJ_COMMIT);
}

void test_object_peel__target_any_object_for_type_change(void)
{
	/* tag to commit */
	assert_peel("7b4384978d2493e851f9cca7858815fac9b10980", GIT_OBJ_ANY,
		"e90810b8df3e80c413d903f631643c716887138d", GIT_OBJ_COMMIT);

	/* commit to tree */
	assert_peel("e90810b8df3e80c413d903f631643c716887138d", GIT_OBJ_ANY,
		"53fc32d17276939fc79ed05badaef2db09990016", GIT_OBJ_TREE);

	/* fail to peel tree */
	assert_peel_error(GIT_EAMBIGUOUS, "53fc32d17276939fc79ed05badaef2db09990016", GIT_OBJ_ANY);

	/* fail to peel blob */
	assert_peel_error(GIT_ENOTFOUND, "0266163a49e280c4f5ed1e08facd36a2bd716bcf", GIT_OBJ_ANY);
}
