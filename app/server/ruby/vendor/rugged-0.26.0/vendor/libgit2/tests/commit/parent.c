#include "clar_libgit2.h"

static git_repository *_repo;
static git_commit *commit;

void test_commit_parent__initialize(void)
{
	git_oid oid;

	cl_git_pass(git_repository_open(&_repo, cl_fixture("testrepo.git")));

	git_oid_fromstr(&oid, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	cl_git_pass(git_commit_lookup(&commit, _repo, &oid));
}

void test_commit_parent__cleanup(void)
{
	git_commit_free(commit);
	commit = NULL;

	git_repository_free(_repo);
	_repo = NULL;
}

static void assert_nth_gen_parent(unsigned int gen, const char *expected_oid)
{
	git_commit *parent = NULL;
	int error;
	
	error = git_commit_nth_gen_ancestor(&parent, commit, gen);

	if (expected_oid != NULL) {
		cl_assert_equal_i(0, error);
		cl_assert_equal_i(0, git_oid_streq(git_commit_id(parent), expected_oid));
	} else
		cl_assert_equal_i(GIT_ENOTFOUND, error);

	git_commit_free(parent);
}

/*
 * $ git show be35~0
 * commit be3563ae3f795b2b4353bcce3a527ad0a4f7f644
 *
 * $ git show be35~1
 * commit 9fd738e8f7967c078dceed8190330fc8648ee56a
 *
 * $ git show be35~3
 * commit 5b5b025afb0b4c913b4c338a42934a3863bf3644
 *
 * $ git show be35~42
 * fatal: ambiguous argument 'be35~42': unknown revision or path not in the working tree.
 */
void test_commit_parent__can_retrieve_nth_generation_parent(void)
{
	assert_nth_gen_parent(0, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	assert_nth_gen_parent(1, "9fd738e8f7967c078dceed8190330fc8648ee56a");
	assert_nth_gen_parent(3, "5b5b025afb0b4c913b4c338a42934a3863bf3644");
	assert_nth_gen_parent(42, NULL);
}
