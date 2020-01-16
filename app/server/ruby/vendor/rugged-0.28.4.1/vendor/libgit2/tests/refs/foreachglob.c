#include "clar_libgit2.h"
#include "refs.h"

static git_repository *repo;
static git_reference *fake_remote;

void test_refs_foreachglob__initialize(void)
{
	git_oid id;

	cl_fixture_sandbox("testrepo.git");
	cl_git_pass(git_repository_open(&repo, "testrepo.git"));

	cl_git_pass(git_oid_fromstr(&id, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644"));
	cl_git_pass(git_reference_create(&fake_remote, repo, "refs/remotes/nulltoken/master", &id, 0, NULL));
}

void test_refs_foreachglob__cleanup(void)
{
	git_reference_free(fake_remote);
	fake_remote = NULL;

	git_repository_free(repo);
	repo = NULL;

	cl_fixture_cleanup("testrepo.git");
}

static int count_cb(const char *reference_name, void *payload)
{
	int *count = (int *)payload;

	GIT_UNUSED(reference_name);

	(*count)++;

	return 0;
}

static void assert_retrieval(const char *glob, int expected_count)
{
	int count = 0;

	cl_git_pass(git_reference_foreach_glob(repo, glob, count_cb, &count));

	cl_assert_equal_i(expected_count, count);
}

void test_refs_foreachglob__retrieve_all_refs(void)
{
	/* 12 heads (including one packed head) + 1 note + 2 remotes + 7 tags + 1 blob */
	assert_retrieval("*", 23);
}

void test_refs_foreachglob__retrieve_remote_branches(void)
{
	assert_retrieval("refs/remotes/*", 2);
}

void test_refs_foreachglob__retrieve_local_branches(void)
{
	assert_retrieval("refs/heads/*", 12);
}

void test_refs_foreachglob__retrieve_nonexistant(void)
{
	assert_retrieval("refs/nonexistent/*", 0);
}

void test_refs_foreachglob__retrieve_partially_named_references(void)
{
	/*
	 * refs/heads/packed-test, refs/heads/test
	 * refs/remotes/test/master, refs/tags/test
	 */

	assert_retrieval("*test*", 4);
}


static int interrupt_cb(const char *reference_name, void *payload)
{
	int *count = (int *)payload;

	GIT_UNUSED(reference_name);

	(*count)++;

	return (*count == 11) ? -1000 : 0;
}

void test_refs_foreachglob__can_cancel(void)
{
	int count = 0;

	cl_assert_equal_i(-1000, git_reference_foreach_glob(
		repo, "*", interrupt_cb, &count) );

	cl_assert_equal_i(11, count);
}
