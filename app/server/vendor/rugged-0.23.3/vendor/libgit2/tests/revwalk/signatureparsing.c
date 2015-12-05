#include "clar_libgit2.h"

static git_repository *_repo;
static git_revwalk *_walk;

void test_revwalk_signatureparsing__initialize(void)
{
	cl_git_pass(git_repository_open(&_repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_revwalk_new(&_walk, _repo));
}

void test_revwalk_signatureparsing__cleanup(void)
{
	git_revwalk_free(_walk);
	_walk = NULL;

	git_repository_free(_repo);
	_repo = NULL;
}

void test_revwalk_signatureparsing__do_not_choke_when_name_contains_angle_brackets(void)
{
	git_reference *ref;
	git_oid commit_oid;
	git_commit *commit;
	const git_signature *signature;

	/*
	 * The branch below points at a commit with angle brackets in the committer/author name
	 * committer <Yu V. Bin Haacked> <foo@example.com> 1323847743 +0100
	 */
	cl_git_pass(git_reference_lookup(&ref, _repo, "refs/heads/haacked"));

	git_revwalk_push(_walk, git_reference_target(ref));
	cl_git_pass(git_revwalk_next(&commit_oid, _walk));

	cl_git_pass(git_commit_lookup(&commit, _repo, git_reference_target(ref)));

	signature = git_commit_committer(commit);
	cl_assert_equal_s("foo@example.com", signature->email);
	cl_assert_equal_s("<Yu V. Bin Haacked>", signature->name);
	cl_assert_equal_i(1323847743, (int)signature->when.time);
	cl_assert_equal_i(60, signature->when.offset);

	git_commit_free(commit);
	git_reference_free(ref);
}
