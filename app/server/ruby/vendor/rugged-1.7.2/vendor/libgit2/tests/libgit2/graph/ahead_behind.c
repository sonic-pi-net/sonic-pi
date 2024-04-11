#include "clar_libgit2.h"

static git_repository *_repo;
static git_commit *commit;
static size_t ahead;
static size_t behind;

void test_graph_ahead_behind__initialize(void)
{
	git_oid oid;
	cl_git_pass(git_repository_open(&_repo, cl_fixture("testrepo.git")));

	cl_git_pass(git_oid__fromstr(&oid, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644", GIT_OID_SHA1));
	cl_git_pass(git_commit_lookup(&commit, _repo, &oid));
}

void test_graph_ahead_behind__cleanup(void)
{
	git_commit_free(commit);
	commit = NULL;

	git_repository_free(_repo);
	_repo = NULL;
}

void test_graph_ahead_behind__returns_correct_result(void)
{	
	git_oid oid;
	git_oid oid2;
	git_commit *other;

	cl_git_pass(git_oid__fromstr(&oid, "e90810b8df3e80c413d903f631643c716887138d", GIT_OID_SHA1));
	cl_git_pass(git_oid__fromstr(&oid2, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644", GIT_OID_SHA1));
	
	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo, &oid, &oid2));
	cl_assert_equal_sz(2, ahead);
	cl_assert_equal_sz(6, behind);

	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo, git_commit_id(commit), git_commit_id(commit)));
	cl_assert_equal_sz(ahead, behind);
	cl_git_pass(git_commit_nth_gen_ancestor(&other, commit, 1));

	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo, git_commit_id(commit), git_commit_id(other)));
	cl_assert_equal_sz(ahead, behind + 2);
	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo, git_commit_id(other), git_commit_id(commit)));
	cl_assert_equal_sz(ahead + 2, behind);

	git_commit_free(other);

	cl_git_pass(git_commit_nth_gen_ancestor(&other, commit, 3));

	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo, git_commit_id(commit), git_commit_id(other)));
	cl_assert_equal_sz(ahead, behind + 4);
	cl_git_pass(git_graph_ahead_behind(&ahead, &behind, _repo, git_commit_id(other), git_commit_id(commit)));
	cl_assert_equal_sz(ahead + 4, behind);

	git_commit_free(other);
}
