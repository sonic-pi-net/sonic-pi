#include "clar_libgit2.h"
#include "posix.h"
#include "path.h"
#include "submodule_helpers.h"
#include "config/config_helpers.h"
#include "futils.h"
#include "repository.h"
#include "git2/sys/commit.h"

static git_repository *g_repo = NULL;
static const char *valid_blob_id = "fa49b077972391ad58037050f2a75f74e3671e92";

void test_submodule_add__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void assert_submodule_url(const char* name, const char *url)
{
	git_buf key = GIT_BUF_INIT;


	cl_git_pass(git_buf_printf(&key, "submodule.%s.url", name));
	assert_config_entry_value(g_repo, git_buf_cstr(&key), url);

	git_buf_dispose(&key);
}

void test_submodule_add__url_absolute(void)
{
	git_submodule *sm;
	git_repository *repo;
	git_buf dot_git_content = GIT_BUF_INIT;

	g_repo = setup_fixture_submod2();

	/* re-add existing submodule */
	cl_git_fail_with(
		GIT_EEXISTS,
		git_submodule_add_setup(NULL, g_repo, "whatever", "sm_unchanged", 1));

	/* add a submodule using a gitlink */

	cl_git_pass(
		git_submodule_add_setup(&sm, g_repo, "https://github.com/libgit2/libgit2.git", "sm_libgit2", 1)
		);
	git_submodule_free(sm);

	cl_assert(git_path_isfile("submod2/" "sm_libgit2" "/.git"));

	cl_assert(git_path_isdir("submod2/.git/modules"));
	cl_assert(git_path_isdir("submod2/.git/modules/" "sm_libgit2"));
	cl_assert(git_path_isfile("submod2/.git/modules/" "sm_libgit2" "/HEAD"));
	assert_submodule_url("sm_libgit2", "https://github.com/libgit2/libgit2.git");

	cl_git_pass(git_repository_open(&repo, "submod2/" "sm_libgit2"));

	/* Verify worktree path is relative */
	assert_config_entry_value(repo, "core.worktree", "../../../sm_libgit2/");

	/* Verify gitdir path is relative */
	cl_git_pass(git_futils_readbuffer(&dot_git_content, "submod2/" "sm_libgit2" "/.git"));
	cl_assert_equal_s("gitdir: ../.git/modules/sm_libgit2/", dot_git_content.ptr);

	git_repository_free(repo);
	git_buf_dispose(&dot_git_content);

	/* add a submodule not using a gitlink */

	cl_git_pass(
		git_submodule_add_setup(&sm, g_repo, "https://github.com/libgit2/libgit2.git", "sm_libgit2b", 0)
		);
	git_submodule_free(sm);

	cl_assert(git_path_isdir("submod2/" "sm_libgit2b" "/.git"));
	cl_assert(git_path_isfile("submod2/" "sm_libgit2b" "/.git/HEAD"));
	cl_assert(!git_path_exists("submod2/.git/modules/" "sm_libgit2b"));
	assert_submodule_url("sm_libgit2b", "https://github.com/libgit2/libgit2.git");
}

void test_submodule_add__url_relative(void)
{
	git_submodule *sm;
	git_remote *remote;
	git_strarray problems = {0};

	/* default remote url is https://github.com/libgit2/false.git */
	g_repo = cl_git_sandbox_init("testrepo2");

	/* make sure we don't default to origin - rename origin -> test_remote */
	cl_git_pass(git_remote_rename(&problems, g_repo, "origin", "test_remote"));
	cl_assert_equal_i(0, problems.count);
	git_strarray_free(&problems);
	cl_git_fail(git_remote_lookup(&remote, g_repo, "origin"));

	cl_git_pass(
		git_submodule_add_setup(&sm, g_repo, "../TestGitRepository", "TestGitRepository", 1)
		);
	git_submodule_free(sm);

	assert_submodule_url("TestGitRepository", "https://github.com/libgit2/TestGitRepository");
}

void test_submodule_add__url_relative_to_origin(void)
{
	git_submodule *sm;

	/* default remote url is https://github.com/libgit2/false.git */
	g_repo = cl_git_sandbox_init("testrepo2");

	cl_git_pass(
		git_submodule_add_setup(&sm, g_repo, "../TestGitRepository", "TestGitRepository", 1)
		);
	git_submodule_free(sm);

	assert_submodule_url("TestGitRepository", "https://github.com/libgit2/TestGitRepository");
}

void test_submodule_add__url_relative_to_workdir(void)
{
	git_submodule *sm;

	/* In this repo, HEAD (master) has no remote tracking branc h*/
	g_repo = cl_git_sandbox_init("testrepo");

	cl_git_pass(
		git_submodule_add_setup(&sm, g_repo, "./", "TestGitRepository", 1)
		);
	git_submodule_free(sm);

	assert_submodule_url("TestGitRepository", git_repository_workdir(g_repo));
}

static void test_add_entry(
	git_index *index,
	const char *idstr,
	const char *path,
	git_filemode_t mode)
{
	git_index_entry entry = {{0}};

	cl_git_pass(git_oid_fromstr(&entry.id, idstr));

	entry.path = path;
	entry.mode = mode;

	cl_git_pass(git_index_add(index, &entry));
}

void test_submodule_add__path_exists_in_index(void)
{
	git_index *index;
	git_submodule *sm;
	git_buf filename = GIT_BUF_INIT;

	g_repo = cl_git_sandbox_init("testrepo");

	cl_git_pass(git_buf_joinpath(&filename, "subdirectory", "test.txt"));

	cl_git_pass(git_repository_index__weakptr(&index, g_repo));

	test_add_entry(index, valid_blob_id, filename.ptr, GIT_FILEMODE_BLOB);

	cl_git_fail_with(git_submodule_add_setup(&sm, g_repo, "./", "subdirectory", 1), GIT_EEXISTS);

	git_submodule_free(sm);
	git_buf_dispose(&filename);
}

void test_submodule_add__file_exists_in_index(void)
{
	git_index *index;
	git_submodule *sm;
	git_buf name = GIT_BUF_INIT;

	g_repo = cl_git_sandbox_init("testrepo");

	cl_git_pass(git_repository_index__weakptr(&index, g_repo));

	test_add_entry(index, valid_blob_id, "subdirectory", GIT_FILEMODE_BLOB);

	cl_git_fail_with(git_submodule_add_setup(&sm, g_repo, "./", "subdirectory", 1), GIT_EEXISTS);

	git_submodule_free(sm);
	git_buf_dispose(&name);
}

void test_submodule_add__submodule_clone(void)
{
	git_oid tree_id, commit_id;
	git_signature *sig;
	git_submodule *sm;
	git_index *index;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	/* Create the submodule structure, clone into it and finalize */
	cl_git_pass(git_submodule_add_setup(&sm, g_repo, cl_fixture("testrepo.git"), "testrepo-add", true));
	cl_git_pass(git_submodule_clone(NULL, sm, NULL));
	cl_git_pass(git_submodule_add_finalize(sm));

	/* Create the submodule commit */
	cl_git_pass(git_repository_index(&index, g_repo));
	cl_git_pass(git_index_write_tree(&tree_id, index));
	cl_git_pass(git_signature_now(&sig, "Submoduler", "submoduler@local"));
	cl_git_pass(git_commit_create_from_ids(&commit_id, g_repo, "HEAD", sig, sig, NULL, "A submodule\n",
					       &tree_id, 0, NULL));

	assert_submodule_exists(g_repo, "testrepo-add");

	git_signature_free(sig);
	git_submodule_free(sm);
	git_index_free(index);
}

void test_submodule_add__submodule_clone_into_nonempty_dir_succeeds(void)
{
	git_submodule *sm;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	cl_git_pass(p_mkdir("empty_standard_repo/sm", 0777));
	cl_git_mkfile("empty_standard_repo/sm/foobar", "");

	/* Create the submodule structure, clone into it and finalize */
	cl_git_pass(git_submodule_add_setup(&sm, g_repo, cl_fixture("testrepo.git"), "sm", true));
	cl_git_pass(git_submodule_clone(NULL, sm, NULL));
	cl_git_pass(git_submodule_add_finalize(sm));

	cl_assert(git_path_exists("empty_standard_repo/sm/foobar"));

	assert_submodule_exists(g_repo, "sm");

	git_submodule_free(sm);
}

void test_submodule_add__submodule_clone_twice_fails(void)
{
	git_submodule *sm;

	g_repo = cl_git_sandbox_init("empty_standard_repo");

	/* Create the submodule structure, clone into it and finalize */
	cl_git_pass(git_submodule_add_setup(&sm, g_repo, cl_fixture("testrepo.git"), "sm", true));
	cl_git_pass(git_submodule_clone(NULL, sm, NULL));
	cl_git_pass(git_submodule_add_finalize(sm));

	cl_git_fail(git_submodule_clone(NULL, sm, NULL));

	git_submodule_free(sm);
}
