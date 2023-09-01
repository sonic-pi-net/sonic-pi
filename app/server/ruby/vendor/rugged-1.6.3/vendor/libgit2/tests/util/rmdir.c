#include "clar_libgit2.h"
#include "futils.h"

static const char *empty_tmp_dir = "test_gitfo_rmdir_recurs_test";

void test_rmdir__initialize(void)
{
	git_str path = GIT_STR_INIT;

	cl_must_pass(p_mkdir(empty_tmp_dir, 0777));

	cl_git_pass(git_str_joinpath(&path, empty_tmp_dir, "/one"));
	cl_must_pass(p_mkdir(path.ptr, 0777));

	cl_git_pass(git_str_joinpath(&path, empty_tmp_dir, "/one/two_one"));
	cl_must_pass(p_mkdir(path.ptr, 0777));

	cl_git_pass(git_str_joinpath(&path, empty_tmp_dir, "/one/two_two"));
	cl_must_pass(p_mkdir(path.ptr, 0777));

	cl_git_pass(git_str_joinpath(&path, empty_tmp_dir, "/one/two_two/three"));
	cl_must_pass(p_mkdir(path.ptr, 0777));

	cl_git_pass(git_str_joinpath(&path, empty_tmp_dir, "/two"));
	cl_must_pass(p_mkdir(path.ptr, 0777));

	git_str_dispose(&path);
}

void test_rmdir__cleanup(void)
{
	if (git_fs_path_exists(empty_tmp_dir))
		cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_REMOVE_FILES));
}

/* make sure empty dir can be deleted recursively */
void test_rmdir__delete_recursive(void)
{
	git_str path = GIT_STR_INIT;
	cl_git_pass(git_str_joinpath(&path, empty_tmp_dir, "/one"));
	cl_assert(git_fs_path_exists(git_str_cstr(&path)));

	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_EMPTY_HIERARCHY));

	cl_assert(!git_fs_path_exists(git_str_cstr(&path)));

	git_str_dispose(&path);
}

/* make sure non-empty dir cannot be deleted recursively */
void test_rmdir__fail_to_delete_non_empty_dir(void)
{
	git_str file = GIT_STR_INIT;

	cl_git_pass(git_str_joinpath(&file, empty_tmp_dir, "/two/file.txt"));

	cl_git_mkfile(git_str_cstr(&file), "dummy");

	cl_git_fail(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_EMPTY_HIERARCHY));

	cl_must_pass(p_unlink(file.ptr));
	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_EMPTY_HIERARCHY));

	cl_assert(!git_fs_path_exists(empty_tmp_dir));

	git_str_dispose(&file);
}

void test_rmdir__keep_base(void)
{
	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_SKIP_ROOT));
	cl_assert(git_fs_path_exists(empty_tmp_dir));
}

void test_rmdir__can_skip_non_empty_dir(void)
{
	git_str file = GIT_STR_INIT;

	cl_git_pass(git_str_joinpath(&file, empty_tmp_dir, "/two/file.txt"));

	cl_git_mkfile(git_str_cstr(&file), "dummy");

	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_SKIP_NONEMPTY));
	cl_assert(git_fs_path_exists(git_str_cstr(&file)) == true);

	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_REMOVE_FILES));
	cl_assert(git_fs_path_exists(empty_tmp_dir) == false);

	git_str_dispose(&file);
}

void test_rmdir__can_remove_empty_parents(void)
{
	git_str file = GIT_STR_INIT;

	cl_git_pass(
		git_str_joinpath(&file, empty_tmp_dir, "/one/two_two/three/file.txt"));
	cl_git_mkfile(git_str_cstr(&file), "dummy");
	cl_assert(git_fs_path_isfile(git_str_cstr(&file)));

	cl_git_pass(git_futils_rmdir_r("one/two_two/three/file.txt", empty_tmp_dir,
		GIT_RMDIR_REMOVE_FILES | GIT_RMDIR_EMPTY_PARENTS));

	cl_assert(!git_fs_path_exists(git_str_cstr(&file)));

	git_str_rtruncate_at_char(&file, '/'); /* three (only contained file.txt) */
	cl_assert(!git_fs_path_exists(git_str_cstr(&file)));

	git_str_rtruncate_at_char(&file, '/'); /* two_two (only contained three) */
	cl_assert(!git_fs_path_exists(git_str_cstr(&file)));

	git_str_rtruncate_at_char(&file, '/'); /* one (contained two_one also) */
	cl_assert(git_fs_path_exists(git_str_cstr(&file)));

	cl_assert(git_fs_path_exists(empty_tmp_dir) == true);

	git_str_dispose(&file);

	cl_git_pass(git_futils_rmdir_r(empty_tmp_dir, NULL, GIT_RMDIR_EMPTY_HIERARCHY));
}
