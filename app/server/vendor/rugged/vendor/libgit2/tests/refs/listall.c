#include "clar_libgit2.h"
#include "posix.h"

static git_repository *repo;
static git_strarray ref_list;

static void ensure_no_refname_starts_with_a_forward_slash(const char *path)
{
	size_t i;

	cl_git_pass(git_repository_open(&repo, path));
	cl_git_pass(git_reference_list(&ref_list, repo));

	cl_assert(ref_list.count > 0);

	for (i = 0; i < ref_list.count; i++)
		cl_assert(git__prefixcmp(ref_list.strings[i], "/") != 0);

	git_strarray_free(&ref_list);
	git_repository_free(repo);
}

void test_refs_listall__from_repository_opened_through_workdir_path(void)
{
	cl_fixture_sandbox("status");
	cl_git_pass(p_rename("status/.gitted", "status/.git"));

	ensure_no_refname_starts_with_a_forward_slash("status");

	cl_fixture_cleanup("status");
}

void test_refs_listall__from_repository_opened_through_gitdir_path(void)
{
	ensure_no_refname_starts_with_a_forward_slash(cl_fixture("testrepo.git"));
}

void test_refs_listall__from_repository_with_no_trailing_newline(void)
{
	cl_git_pass(git_repository_open(&repo, cl_fixture("bad_tag.git")));
	cl_git_pass(git_reference_list(&ref_list, repo));

	cl_assert(ref_list.count > 0);

	git_strarray_free(&ref_list);
	git_repository_free(repo);
}
