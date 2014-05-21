#include "clar_libgit2.h"
#include "commit.h"
#include "git2/commit.h"

static git_repository *_repo;

void test_commit_commit__initialize(void)
{
	cl_fixture_sandbox("testrepo.git");
	cl_git_pass(git_repository_open(&_repo, "testrepo.git"));
}

void test_commit_commit__cleanup(void)
{
	git_repository_free(_repo);
	_repo = NULL;

	cl_fixture_cleanup("testrepo.git");
}

void test_commit_commit__create_unexisting_update_ref(void)
{
	git_oid oid;
	git_tree *tree;
	git_commit *commit;
	git_signature *s;
	git_reference *ref;

	git_oid_fromstr(&oid, "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	cl_git_pass(git_commit_lookup(&commit, _repo, &oid));

	git_oid_fromstr(&oid, "944c0f6e4dfa41595e6eb3ceecdb14f50fe18162");
	cl_git_pass(git_tree_lookup(&tree, _repo, &oid));

	cl_git_pass(git_signature_now(&s, "alice", "alice@example.com"));

	cl_git_fail(git_reference_lookup(&ref, _repo, "refs/heads/foo/bar"));
	cl_git_pass(git_commit_create(&oid, _repo, "refs/heads/foo/bar", s, s,
				      NULL, "some msg", tree, 1, (const git_commit **) &commit));

	/* fail because the parent isn't the tip of the branch anymore */
	cl_git_fail(git_commit_create(&oid, _repo, "refs/heads/foo/bar", s, s,
				      NULL, "some msg", tree, 1, (const git_commit **) &commit));

	cl_git_pass(git_reference_lookup(&ref, _repo, "refs/heads/foo/bar"));
	cl_assert(!git_oid_cmp(&oid, git_reference_target(ref)));

	git_tree_free(tree);
	git_commit_free(commit);
	git_signature_free(s);
	git_reference_free(ref);
}

void assert_commit_summary(const char *expected, const char *given)
{
	git_commit *dummy;

	cl_assert(dummy = git__calloc(1, sizeof(struct git_commit)));

	dummy->raw_message = git__strdup(given);
	cl_assert_equal_s(expected, git_commit_summary(dummy));

	git_commit__free(dummy);
}

void test_commit_commit__summary(void)
{
	assert_commit_summary("One-liner with no trailing newline", "One-liner with no trailing newline");
	assert_commit_summary("One-liner with trailing newline", "One-liner with trailing newline\n");
	assert_commit_summary("Trimmed leading&trailing newlines", "\n\nTrimmed leading&trailing newlines\n\n");
	assert_commit_summary("First paragraph only", "\nFirst paragraph only\n\n(There are more!)");
	assert_commit_summary("First paragraph with  unwrapped trailing\tlines", "\nFirst paragraph\nwith  unwrapped\ntrailing\tlines\n\n(Yes, unwrapped!)");
	assert_commit_summary("\tLeading \ttabs", "\tLeading\n\ttabs\n\nis preserved");
	assert_commit_summary(" Leading  Spaces", " Leading\n Spaces\n\nare preserved");
	assert_commit_summary("Trailing tabs\tare removed", "Trailing tabs\tare removed\t\t");
	assert_commit_summary("Trailing spaces  are removed", "Trailing spaces  are removed  ");
	assert_commit_summary("Trailing tabs", "Trailing tabs\t\n\nare removed");
	assert_commit_summary("Trailing spaces", "Trailing spaces \n\nare removed");
	assert_commit_summary("", "");
	assert_commit_summary("", " ");
	assert_commit_summary("", "\n");
	assert_commit_summary("", "\n \n");
}
