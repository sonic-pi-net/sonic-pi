#include "clar_libgit2.h"

static git_repository *_repo;
static git_commit *commit;

void test_graph_descendant_of__initialize(void)
{
	git_oid oid;

	cl_git_pass(git_repository_open(&_repo, cl_fixture("testrepo.git")));

	git_oid_fromstr(&oid, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");
	cl_git_pass(git_commit_lookup(&commit, _repo, &oid));
}

void test_graph_descendant_of__cleanup(void)
{
	git_commit_free(commit);
	commit = NULL;

	git_repository_free(_repo);
	_repo = NULL;
}

void test_graph_descendant_of__returns_correct_result(void)
{
	git_commit *other;

	cl_assert_equal_i(0, git_graph_descendant_of(_repo, git_commit_id(commit), git_commit_id(commit)));


	cl_git_pass(git_commit_nth_gen_ancestor(&other, commit, 1));

	cl_assert_equal_i(1, git_graph_descendant_of(_repo, git_commit_id(commit), git_commit_id(other)));
	cl_assert_equal_i(0, git_graph_descendant_of(_repo, git_commit_id(other), git_commit_id(commit)));

	git_commit_free(other);


	cl_git_pass(git_commit_nth_gen_ancestor(&other, commit, 3));

	cl_assert_equal_i(1, git_graph_descendant_of(_repo, git_commit_id(commit), git_commit_id(other)));
	cl_assert_equal_i(0, git_graph_descendant_of(_repo, git_commit_id(other), git_commit_id(commit)));

	git_commit_free(other);

}

void test_graph_descendant_of__nopath(void)
{
	git_oid oid;

	git_oid_fromstr(&oid, "e90810b8df3e80c413d903f631643c716887138d");
	cl_assert_equal_i(0, git_graph_descendant_of(_repo, git_commit_id(commit), &oid));
}
