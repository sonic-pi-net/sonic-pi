#include "clar_libgit2.h"
#include "futils.h"

static const char *empty_tmp_dir = "test_gitfo_rmdir_recurs_test";

void test_core_rmdir__initialize(void)
{
	git_buf path = GIT_BUF_INIT;

	cl_must_pass(p_mkdir(empty_tmp_dir, 0777));

	cl_git_pass(git_buf_joinpath(&path, empty_tmp_dir, "/one"));
	cl_must_pass(p_mkdir(path.ptr, 0777));

	cl_git_pass(git_buf_joinpath(&path, empty_tmp_dir, "/one/two_one"));
	cl_must_pass(p_mkdir(path.ptr, 0777));

	cl_git_pass(git_buf_joinpath(&path, empty_tmp_dir, "/one/two_two"));
	cl_must_pass(p_mkdir(path.ptr, 0777));

	cl_git_pass(git_buf_joinpath(&path, empty_tmp_dir, "/one/two_two/three"));
	cl_must_pass(p_mkdir(path.ptr, 0777));

	cl_git_pass(git_buf_joinpath(&path, empty_tmp_dir, "/two"));
	cl_must_pass(p_mkdir(path.ptr, 0777));

	git_buf_dispose(&path);
}

void test_core_rmdir__cleanup(void)
{
	if (git_path_exists(empty_tmp_dir))
		cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_REMOVE_FILES));
}

/* make sure empty dir can be deleted recusively */
void test_core_rmdir__delete_recursive(void)
{
	git_buf path = GIT_BUF_INIT;
	cl_git_pass(git_buf_joinpath(&path, empty_tmp_dir, "/one"));
	cl_assert(git_path_exists(git_buf_cstr(&path)));

	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_EMPTY_HIERARCHY));

	cl_assert(!git_path_exists(git_buf_cstr(&path)));

	git_buf_dispose(&path);
}

/* make sure non-empty dir cannot be deleted recusively */
void test_core_rmdir__fail_to_delete_non_empty_dir(void)
{
	git_buf file = GIT_BUF_INIT;

	cl_git_pass(git_buf_joinpath(&file, empty_tmp_dir, "/two/file.txt"));

	cl_git_mkfile(git_buf_cstr(&file), "dummy");

	cl_git_fail(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_EMPTY_HIERARCHY));

	cl_must_pass(p_unlink(file.ptr));
	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_EMPTY_HIERARCHY));

	cl_assert(!git_path_exists(empty_tmp_dir));

	git_buf_dispose(&file);
}

void test_core_rmdir__keep_base(void)
{
	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_SKIP_ROOT));
	cl_assert(git_path_exists(empty_tmp_dir));
}

void test_core_rmdir__can_skip_non_empty_dir(void)
{
	git_buf file = GIT_BUF_INIT;

	cl_git_pass(git_buf_joinpath(&file, empty_tmp_dir, "/two/file.txt"));

	cl_git_mkfile(git_buf_cstr(&file), "dummy");

	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_SKIP_NONEMPTY));
	cl_assert(git_path_exists(git_buf_cstr(&file)) == true);

	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_REMOVE_FILES));
	cl_assert(git_path_exists(empty_tmp_dir) == false);

	git_buf_dispose(&file);
}

void test_core_rmdir__can_remove_empty_parents(void)
{
	git_buf file = GIT_BUF_INIT;

	cl_git_pass(
		git_buf_joinpath(&file, empty_tmp_dir, "/one/two_two/three/file.txt"));
	cl_git_mkfile(git_buf_cstr(&file), "dummy");
	cl_assert(git_path_isfile(git_buf_cstr(&file)));

	cl_git_pass(git_futils_rmdir_r("one/two_two/three/file.txt", empty_tmp_dir,
		GIT_RMDIR_REMOVE_FILES | GIT_RMDIR_EMPTY_PARENTS));

	cl_assert(!git_path_exists(git_buf_cstr(&file)));

	git_buf_rtruncate_at_char(&file, '/'); /* three (only contained file.txt) */
	cl_assert(!git_path_exists(git_buf_cstr(&file)));

	git_buf_rtruncate_at_char(&file, '/'); /* two_two (only contained three) */
	cl_assert(!git_path_exists(git_buf_cstr(&file)));

	git_buf_rtruncate_at_char(&file, '/'); /* one (contained two_one also) */
	cl_assert(git_path_exists(git_buf_cstr(&file)));

	cl_assert(git_path_exists(empty_tmp_dir) == true);

	git_buf_dispose(&file);

	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_EMPTY_HIERARCHY));
}
