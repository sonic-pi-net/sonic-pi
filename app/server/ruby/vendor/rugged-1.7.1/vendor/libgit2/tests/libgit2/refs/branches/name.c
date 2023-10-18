#include "clar_libgit2.h"
#include "branch.h"

static git_repository *repo;
static git_reference *ref;

void test_refs_branches_name__initialize(void)
{
	cl_git_pass(git_repository_open(&repo, cl_fixture("testrepo.git")));
}

void test_refs_branches_name__cleanup(void)
{
	git_reference_free(ref);
	ref = NULL;

	git_repository_free(repo);
	repo = NULL;
}

void test_refs_branches_name__can_get_local_branch_name(void)
{
	const char *name;

	cl_git_pass(git_branch_lookup(&ref,repo,"master",GIT_BRANCH_LOCAL));
	cl_git_pass(git_branch_name(&name,ref));
	cl_assert_equal_s("master",name);
}

void test_refs_branches_name__can_get_remote_branch_name(void)
{
	const char *name;

	cl_git_pass(git_branch_lookup(&ref,repo,"test/master",GIT_BRANCH_REMOTE));
	cl_git_pass(git_branch_name(&name,ref));
	cl_assert_equal_s("test/master",name);
}

void test_refs_branches_name__error_when_ref_is_no_branch(void)
{
	const char *name;

	cl_git_pass(git_reference_lookup(&ref,repo,"refs/notes/fanout"));
	cl_git_fail(git_branch_name(&name,ref));
}

static int name_is_valid(const char *name)
{
	int valid;
	cl_git_pass(git_branch_name_is_valid(&valid, name));
	return valid;
}

void test_refs_branches_name__is_name_valid(void)
{
	cl_assert_equal_i(true, name_is_valid("master"));
	cl_assert_equal_i(true, name_is_valid("test/master"));

	cl_assert_equal_i(false, name_is_valid(""));
	cl_assert_equal_i(false, name_is_valid("HEAD"));
	cl_assert_equal_i(false, name_is_valid("-dash"));
}
