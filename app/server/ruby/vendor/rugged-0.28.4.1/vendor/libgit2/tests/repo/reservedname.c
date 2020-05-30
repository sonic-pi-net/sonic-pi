#include "clar_libgit2.h"
#include "../submodule/submodule_helpers.h"
#include "repository.h"

void test_repo_reservedname__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_repo_reservedname__includes_shortname_on_win32(void)
{
	git_repository *repo;
	git_buf *reserved;
	size_t reserved_len;

	repo = cl_git_sandbox_init("nasty");
	cl_assert(git_repository__reserved_names(&reserved, &reserved_len, repo, false));

#ifdef GIT_WIN32
	cl_assert_equal_i(2, reserved_len);
	cl_assert_equal_s(".git", reserved[0].ptr);
	cl_assert_equal_s("GIT~1", reserved[1].ptr);
#else
	cl_assert_equal_i(1, reserved_len);
	cl_assert_equal_s(".git", reserved[0].ptr);
#endif
}

void test_repo_reservedname__includes_shortname_when_requested(void)
{
	git_repository *repo;
	git_buf *reserved;
	size_t reserved_len;

	repo = cl_git_sandbox_init("nasty");
	cl_assert(git_repository__reserved_names(&reserved, &reserved_len, repo, true));

	cl_assert_equal_i(2, reserved_len);
	cl_assert_equal_s(".git", reserved[0].ptr);
	cl_assert_equal_s("GIT~1", reserved[1].ptr);
}

/* Ensures that custom shortnames are included: creates a GIT~1 so that the
 * .git folder itself will have to be named GIT~2
 */
void test_repo_reservedname__custom_shortname_recognized(void)
{
#ifdef GIT_WIN32
	git_repository *repo;
	git_buf *reserved;
	size_t reserved_len;

	if (!cl_sandbox_supports_8dot3())
		clar__skip();

	repo = cl_git_sandbox_init("nasty");

	cl_must_pass(p_rename("nasty/.git", "nasty/_temp"));
	cl_git_write2file("nasty/git~1", "", 0, O_RDWR|O_CREAT, 0666);
	cl_must_pass(p_rename("nasty/_temp", "nasty/.git"));

	cl_assert(git_repository__reserved_names(&reserved, &reserved_len, repo, true));

	cl_assert_equal_i(3, reserved_len);
	cl_assert_equal_s(".git", reserved[0].ptr);
	cl_assert_equal_s("GIT~1", reserved[1].ptr);
	cl_assert_equal_s("GIT~2", reserved[2].ptr);
#endif
}

/* When looking at the short name for a submodule, we need to prevent
 * people from overwriting the `.git` file in the submodule working
 * directory itself.  We don't want to look at the actual repository
 * path, since it will be in the super's repository above us, and
 * typically named with the name of our subrepository.  Consequently,
 * preventing access to the short name of the actual repository path
 * would prevent us from creating files with the same name as the
 * subrepo.  (Eg, a submodule named "libgit2" could not contain a file
 * named "libgit2", which would be unfortunate.)
 */
void test_repo_reservedname__submodule_pointer(void)
{
#ifdef GIT_WIN32
	git_repository *super_repo, *sub_repo;
	git_submodule *sub;
	git_buf *sub_reserved;
	size_t sub_reserved_len;

	if (!cl_sandbox_supports_8dot3())
		clar__skip();

	super_repo = setup_fixture_submod2();

	assert_submodule_exists(super_repo, "sm_unchanged");

	cl_git_pass(git_submodule_lookup(&sub, super_repo, "sm_unchanged"));
	cl_git_pass(git_submodule_open(&sub_repo, sub));

	cl_assert(git_repository__reserved_names(&sub_reserved, &sub_reserved_len, sub_repo, true));

	cl_assert_equal_i(2, sub_reserved_len);
	cl_assert_equal_s(".git", sub_reserved[0].ptr);
	cl_assert_equal_s("GIT~1", sub_reserved[1].ptr);

	git_submodule_free(sub);
	git_repository_free(sub_repo);
#endif
}

/* Like the `submodule_pointer` test (above), this ensures that we do not
 * follow the gitlink to the submodule's repository location and treat that
 * as a reserved name.  This tests at an initial submodule update, where the
 * submodule repo is being created.
 */
void test_repo_reservedname__submodule_pointer_during_create(void)
{
	git_repository *repo;
	git_submodule *sm;
	git_submodule_update_options update_options = GIT_SUBMODULE_UPDATE_OPTIONS_INIT;
	git_buf url = GIT_BUF_INIT;

	repo = setup_fixture_super();

	cl_git_pass(git_buf_joinpath(&url, clar_sandbox_path(), "sub.git"));
	cl_repo_set_string(repo, "submodule.sub.url", url.ptr);

	cl_git_pass(git_submodule_lookup(&sm, repo, "sub"));
	cl_git_pass(git_submodule_update(sm, 1, &update_options));

	git_submodule_free(sm);
	git_buf_dispose(&url);
}
