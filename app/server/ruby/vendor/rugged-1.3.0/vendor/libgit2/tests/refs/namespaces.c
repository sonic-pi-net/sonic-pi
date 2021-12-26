#include "clar_libgit2.h"

#include "repository.h"

static git_repository *g_repo;

void test_refs_namespaces__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");
}

void test_refs_namespaces__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_refs_namespaces__get_and_set(void)
{
	cl_assert_equal_s(NULL, git_repository_get_namespace(g_repo));

	cl_git_pass(git_repository_set_namespace(g_repo, "namespace"));
	cl_assert_equal_s("namespace", git_repository_get_namespace(g_repo));

	cl_git_pass(git_repository_set_namespace(g_repo, NULL));
	cl_assert_equal_s(NULL, git_repository_get_namespace(g_repo));
}

void test_refs_namespaces__namespace_doesnt_show_normal_refs(void)
{
	static git_strarray ref_list;

	cl_git_pass(git_repository_set_namespace(g_repo, "namespace"));
	cl_git_pass(git_reference_list(&ref_list, g_repo));
	cl_assert_equal_i(0, ref_list.count);
	git_strarray_dispose(&ref_list);
}
