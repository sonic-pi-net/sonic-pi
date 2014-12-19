/* test the submodule APIs on repositories where there are no submodules */

#include "clar_libgit2.h"
#include "posix.h"
#include "fileops.h"

void test_submodule_nosubs__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_submodule_nosubs__lookup(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_submodule *sm = NULL;

	p_mkdir("status/subrepo", 0777);
	cl_git_mkfile("status/subrepo/.git", "gitdir: ../.git");

	cl_assert_equal_i(GIT_ENOTFOUND, git_submodule_lookup(&sm, repo, "subdir"));

	cl_assert_equal_i(GIT_EEXISTS, git_submodule_lookup(&sm, repo, "subrepo"));

	cl_git_pass(git_submodule_reload_all(repo, 0));

	cl_assert_equal_i(GIT_ENOTFOUND, git_submodule_lookup(&sm, repo, "subdir"));

	cl_assert_equal_i(GIT_EEXISTS, git_submodule_lookup(&sm, repo, "subrepo"));
}

void test_submodule_nosubs__immediate_reload(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	cl_git_pass(git_submodule_reload_all(repo, 0));
}

static int fake_submod_cb(git_submodule *sm, const char *n, void *p)
{
	GIT_UNUSED(sm); GIT_UNUSED(n); GIT_UNUSED(p);
	return 0;
}

void test_submodule_nosubs__foreach(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	cl_git_pass(git_submodule_foreach(repo, fake_submod_cb, NULL));
}

void test_submodule_nosubs__add(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_submodule *sm, *sm2;

	cl_git_pass(git_submodule_add_setup(&sm, repo, "https://github.com/libgit2/libgit2.git", "submodules/libgit2", 1));

	cl_git_pass(git_submodule_lookup(&sm2, repo, "submodules/libgit2"));
	git_submodule_free(sm2);

	cl_git_pass(git_submodule_foreach(repo, fake_submod_cb, NULL));
	cl_git_pass(git_submodule_reload_all(repo, 0));

	git_submodule_free(sm);
}

void test_submodule_nosubs__reload_add_reload(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_submodule *sm;

	cl_git_pass(git_submodule_reload_all(repo, 0));

	/* try one add with a reload (to make sure no errors happen) */

	cl_git_pass(git_submodule_add_setup(&sm, repo,
		"https://github.com/libgit2/libgit2.git", "submodules/libgit2", 1));

	cl_git_pass(git_submodule_reload_all(repo, 0));

	cl_assert_equal_s("submodules/libgit2", git_submodule_name(sm));
	git_submodule_free(sm);

	cl_git_pass(git_submodule_lookup(&sm, repo, "submodules/libgit2"));
	cl_assert_equal_s("submodules/libgit2", git_submodule_name(sm));
	git_submodule_free(sm);

	/* try one add without a reload (to make sure cache inval works, too) */

	cl_git_pass(git_submodule_add_setup(&sm, repo,
		"https://github.com/libgit2/libgit2.git", "libgit2-again", 1));
	cl_assert_equal_s("libgit2-again", git_submodule_name(sm));
	git_submodule_free(sm);

	cl_git_pass(git_submodule_lookup(&sm, repo, "libgit2-again"));
	cl_assert_equal_s("libgit2-again", git_submodule_name(sm));
	git_submodule_free(sm);
}

void test_submodule_nosubs__bad_gitmodules(void)
{
	git_repository *repo = cl_git_sandbox_init("status");

	cl_git_mkfile("status/.gitmodules", "[submodule \"foobar\"]\tpath=blargle\n\turl=\n\tbranch=\n\tupdate=flooble\n\n");
	cl_git_fail(git_submodule_reload_all(repo, 0));

	cl_git_rewritefile("status/.gitmodules", "[submodule \"foobar\"]\tpath=blargle\n\turl=\n\tbranch=\n\tupdate=rebase\n\n");
	cl_git_pass(git_submodule_reload_all(repo, 0));

	cl_git_pass(git_submodule_lookup(NULL, repo, "foobar"));
	cl_assert_equal_i(GIT_ENOTFOUND, git_submodule_lookup(NULL, repo, "subdir"));
}

void test_submodule_nosubs__add_and_delete(void)
{
	git_repository *repo = cl_git_sandbox_init("status");
	git_submodule *sm;
	git_buf buf = GIT_BUF_INIT;

	/* note lack of calls to git_submodule_reload_all - this *should* work */

	cl_git_fail(git_submodule_lookup(NULL, repo, "libgit2"));
	cl_git_fail(git_submodule_lookup(NULL, repo, "submodules/libgit2"));

	/* create */

	cl_git_pass(git_submodule_add_setup(
		&sm, repo, "https://github.com/libgit2/libgit2.git", "submodules/libgit2", 1));
	cl_assert_equal_s("submodules/libgit2", git_submodule_name(sm));
	cl_assert_equal_s("submodules/libgit2", git_submodule_path(sm));
	git_submodule_free(sm);

	cl_git_pass(git_futils_readbuffer(&buf, "status/.gitmodules"));
	cl_assert(strstr(buf.ptr, "[submodule \"submodules/libgit2\"]") != NULL);
	cl_assert(strstr(buf.ptr, "path = submodules/libgit2") != NULL);
	git_buf_free(&buf);

	/* lookup */

	cl_git_fail(git_submodule_lookup(&sm, repo, "libgit2"));
	cl_git_pass(git_submodule_lookup(&sm, repo, "submodules/libgit2"));
	cl_assert_equal_s("submodules/libgit2", git_submodule_name(sm));
	cl_assert_equal_s("submodules/libgit2", git_submodule_path(sm));
	git_submodule_free(sm);

	/* update name */

	cl_git_rewritefile(
		"status/.gitmodules",
		"[submodule \"libgit2\"]\n"
		"  path = submodules/libgit2\n"
		"  url = https://github.com/libgit2/libgit2.git\n");

	cl_git_pass(git_submodule_lookup(&sm, repo, "libgit2"));
	cl_assert_equal_s("libgit2", git_submodule_name(sm));
	cl_assert_equal_s("submodules/libgit2", git_submodule_path(sm));
	git_submodule_free(sm);
	cl_git_pass(git_submodule_lookup(&sm, repo, "submodules/libgit2"));
	git_submodule_free(sm);

	/* revert name update */

	cl_git_rewritefile(
		"status/.gitmodules",
		"[submodule \"submodules/libgit2\"]\n"
		"  path = submodules/libgit2\n"
		"  url = https://github.com/libgit2/libgit2.git\n");

	cl_git_fail(git_submodule_lookup(&sm, repo, "libgit2"));
	cl_git_pass(git_submodule_lookup(&sm, repo, "submodules/libgit2"));
	git_submodule_free(sm);

	/* remove completely */

	cl_must_pass(p_unlink("status/.gitmodules"));
	cl_git_fail(git_submodule_lookup(&sm, repo, "libgit2"));
	cl_git_fail(git_submodule_lookup(&sm, repo, "submodules/libgit2"));
}
