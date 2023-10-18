#include "clar_libgit2.h"

#include "futils.h"
#include "grafts.h"
#include "repository.h"

static git_repository *g_repo;
static git_oid g_shallow_oid;

void test_grafts_shallow__initialize(void)
{
	cl_git_pass(git_oid__fromstr(&g_shallow_oid, "be3563ae3f795b2b4353bcce3a527ad0a4f7f644", GIT_OID_SHA1));
}

void test_grafts_shallow__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_grafts_shallow__no_shallow_file(void)
{
	g_repo = cl_git_sandbox_init("testrepo.git");
	cl_assert_equal_i(0, git_repository_is_shallow(g_repo));
}

void test_grafts_shallow__empty_shallow_file(void)
{
	g_repo = cl_git_sandbox_init("testrepo.git");
	cl_git_mkfile("testrepo.git/shallow", "");
	cl_assert_equal_i(0, git_repository_is_shallow(g_repo));
}

void test_grafts_shallow__shallow_repo(void)
{
	g_repo = cl_git_sandbox_init("shallow.git");
	cl_assert_equal_i(1, git_repository_is_shallow(g_repo));
}

void test_grafts_shallow__clears_errors(void)
{
	g_repo = cl_git_sandbox_init("testrepo.git");
	cl_assert_equal_i(0, git_repository_is_shallow(g_repo));
	cl_assert_equal_p(NULL, git_error_last());
}

void test_grafts_shallow__shallow_oids(void)
{
	git_commit_graft *graft;
	git_grafts *grafts;

	g_repo = cl_git_sandbox_init("shallow.git");

	cl_git_pass(git_repository_shallow_grafts__weakptr(&grafts, g_repo));
	cl_assert_equal_i(1, git_grafts_size(grafts));
	cl_git_pass(git_grafts_get(&graft, grafts, &g_shallow_oid));
}

void test_grafts_shallow__cache_clearing(void)
{
	git_commit_graft *graft;
	git_grafts *grafts;
	git_oid tmp_oid;

	cl_git_pass(git_oid__fromstr(&tmp_oid, "0000000000000000000000000000000000000000", GIT_OID_SHA1));
	g_repo = cl_git_sandbox_init("shallow.git");
	cl_git_pass(git_repository_shallow_grafts__weakptr(&grafts, g_repo));

	cl_assert_equal_i(1, git_grafts_size(grafts));
	cl_git_pass(git_grafts_get(&graft, grafts, &g_shallow_oid));

	cl_git_mkfile("shallow.git/shallow",
		"be3563ae3f795b2b4353bcce3a527ad0a4f7f644\n"
		"0000000000000000000000000000000000000000\n"
	);

	cl_git_pass(git_grafts_refresh(grafts));
	cl_assert_equal_i(2, git_grafts_size(grafts));
	cl_git_pass(git_grafts_get(&graft, grafts, &g_shallow_oid));
	cl_git_pass(git_grafts_get(&graft, grafts, &tmp_oid));

	cl_git_pass(p_unlink("shallow.git/shallow"));
	cl_git_pass(git_grafts_refresh(grafts));
	cl_assert_equal_i(0, git_grafts_size(grafts));
}

void test_grafts_shallow__errors_on_borked(void)
{
	git_grafts *grafts;

	g_repo = cl_git_sandbox_init("shallow.git");

	cl_git_mkfile("shallow.git/shallow", "lolno");
	cl_git_pass(git_repository_shallow_grafts__weakptr(&grafts, g_repo));
	cl_git_fail(git_grafts_refresh(grafts));
	cl_assert_equal_i(0, git_grafts_size(grafts));

	cl_git_mkfile("shallow.git/shallow", "lolno\n");
	cl_git_pass(git_repository_shallow_grafts__weakptr(&grafts, g_repo));
	cl_git_fail(git_grafts_refresh(grafts));
	cl_assert_equal_i(0, git_grafts_size(grafts));
}

void test_grafts_shallow__revwalk_behavior(void)
{
	git_revwalk *w;
	git_oid oid_1, oid_2, oid_3;

	g_repo = cl_git_sandbox_init("shallow.git");

	cl_git_pass(git_revwalk_new(&w, g_repo));
	cl_git_pass(git_revwalk_push_head(w));

	cl_git_pass(git_revwalk_next(&oid_1, w)); // a65fedf39aefe402d3bb6e24df4d4f5fe4547750
	cl_git_pass(git_revwalk_next(&oid_2, w)); // be3563ae3f795b2b4353bcce3a527ad0a4f7f644
	cl_git_fail_with(GIT_ITEROVER, git_revwalk_next(&oid_3, w));

	cl_assert_equal_s(git_oid_tostr_s(&oid_1), "a65fedf39aefe402d3bb6e24df4d4f5fe4547750");
	cl_assert_equal_s(git_oid_tostr_s(&oid_2), "be3563ae3f795b2b4353bcce3a527ad0a4f7f644");

	git_revwalk_free(w);
}

void test_grafts_shallow__grafted_object(void)
{
	git_commit *commit;

	g_repo = cl_git_sandbox_init("shallow.git");

	cl_git_pass(git_commit_lookup(&commit, g_repo, &g_shallow_oid));

	cl_assert_equal_i(0, git_commit_parentcount(commit));

	git_commit_free(commit);
}
