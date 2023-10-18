#include "clar_libgit2.h"
#include "futils.h"
#include "posix.h"

static void cleanup_basic_dirs(void *ref)
{
	GIT_UNUSED(ref);
	git_futils_rmdir_r("d0", NULL, GIT_RMDIR_EMPTY_HIERARCHY);
	git_futils_rmdir_r("d1", NULL, GIT_RMDIR_EMPTY_HIERARCHY);
	git_futils_rmdir_r("d2", NULL, GIT_RMDIR_EMPTY_HIERARCHY);
	git_futils_rmdir_r("d3", NULL, GIT_RMDIR_EMPTY_HIERARCHY);
	git_futils_rmdir_r("d4", NULL, GIT_RMDIR_EMPTY_HIERARCHY);
}

void test_mkdir__absolute(void)
{
	git_str path = GIT_STR_INIT;

	cl_set_cleanup(cleanup_basic_dirs, NULL);

	git_str_joinpath(&path, clar_sandbox_path(), "d0");

	/* make a directory */
	cl_assert(!git_fs_path_isdir(path.ptr));
	cl_git_pass(git_futils_mkdir(path.ptr, 0755, 0));
	cl_assert(git_fs_path_isdir(path.ptr));

	git_str_joinpath(&path, path.ptr, "subdir");
	cl_assert(!git_fs_path_isdir(path.ptr));
	cl_git_pass(git_futils_mkdir(path.ptr, 0755, 0));
	cl_assert(git_fs_path_isdir(path.ptr));

	/* ensure mkdir_r works for a single subdir */
	git_str_joinpath(&path, path.ptr, "another");
	cl_assert(!git_fs_path_isdir(path.ptr));
	cl_git_pass(git_futils_mkdir_r(path.ptr, 0755));
	cl_assert(git_fs_path_isdir(path.ptr));

	/* ensure mkdir_r works */
	git_str_joinpath(&path, clar_sandbox_path(), "d1/foo/bar/asdf");
	cl_assert(!git_fs_path_isdir(path.ptr));
	cl_git_pass(git_futils_mkdir_r(path.ptr, 0755));
	cl_assert(git_fs_path_isdir(path.ptr));

	/* ensure we don't imply recursive */
	git_str_joinpath(&path, clar_sandbox_path(), "d2/foo/bar/asdf");
	cl_assert(!git_fs_path_isdir(path.ptr));
	cl_git_fail(git_futils_mkdir(path.ptr, 0755, 0));
	cl_assert(!git_fs_path_isdir(path.ptr));

	git_str_dispose(&path);
}

void test_mkdir__basic(void)
{
	cl_set_cleanup(cleanup_basic_dirs, NULL);

	/* make a directory */
	cl_assert(!git_fs_path_isdir("d0"));
	cl_git_pass(git_futils_mkdir("d0", 0755, 0));
	cl_assert(git_fs_path_isdir("d0"));

	/* make a path */
	cl_assert(!git_fs_path_isdir("d1"));
	cl_git_pass(git_futils_mkdir("d1/d1.1/d1.2", 0755, GIT_MKDIR_PATH));
	cl_assert(git_fs_path_isdir("d1"));
	cl_assert(git_fs_path_isdir("d1/d1.1"));
	cl_assert(git_fs_path_isdir("d1/d1.1/d1.2"));

	/* make a dir exclusively */
	cl_assert(!git_fs_path_isdir("d2"));
	cl_git_pass(git_futils_mkdir("d2", 0755, GIT_MKDIR_EXCL));
	cl_assert(git_fs_path_isdir("d2"));

	/* make exclusive failure */
	cl_git_fail(git_futils_mkdir("d2", 0755, GIT_MKDIR_EXCL));

	/* make a path exclusively */
	cl_assert(!git_fs_path_isdir("d3"));
	cl_git_pass(git_futils_mkdir("d3/d3.1/d3.2", 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL));
	cl_assert(git_fs_path_isdir("d3"));
	cl_assert(git_fs_path_isdir("d3/d3.1/d3.2"));

	/* make exclusive path failure */
	cl_git_fail(git_futils_mkdir("d3/d3.1/d3.2", 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL));
	/* ??? Should EXCL only apply to the last item in the path? */

	/* path with trailing slash? */
	cl_assert(!git_fs_path_isdir("d4"));
	cl_git_pass(git_futils_mkdir("d4/d4.1/", 0755, GIT_MKDIR_PATH));
	cl_assert(git_fs_path_isdir("d4/d4.1"));
}

static void cleanup_basedir(void *ref)
{
	GIT_UNUSED(ref);
	git_futils_rmdir_r("base", NULL, GIT_RMDIR_EMPTY_HIERARCHY);
}

void test_mkdir__with_base(void)
{
#define BASEDIR "base/dir/here"

	cl_set_cleanup(cleanup_basedir, NULL);

	cl_git_pass(git_futils_mkdir(BASEDIR, 0755, GIT_MKDIR_PATH));

	cl_git_pass(git_futils_mkdir_relative("a", BASEDIR, 0755, 0, NULL));
	cl_assert(git_fs_path_isdir(BASEDIR "/a"));

	cl_git_pass(git_futils_mkdir_relative("b/b1/b2", BASEDIR, 0755, GIT_MKDIR_PATH, NULL));
	cl_assert(git_fs_path_isdir(BASEDIR "/b/b1/b2"));

	/* exclusive with existing base */
	cl_git_pass(git_futils_mkdir_relative("c/c1/c2", BASEDIR, 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL, NULL));

	/* fail: exclusive with duplicated suffix */
	cl_git_fail(git_futils_mkdir_relative("c/c1/c3", BASEDIR, 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL, NULL));

	/* fail: exclusive with any duplicated component */
	cl_git_fail(git_futils_mkdir_relative("c/cz/cz", BASEDIR, 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL, NULL));

	/* success: exclusive without path */
	cl_git_pass(git_futils_mkdir_relative("c/c1/c3", BASEDIR, 0755, GIT_MKDIR_EXCL, NULL));

	/* path with shorter base and existing dirs */
	cl_git_pass(git_futils_mkdir_relative("dir/here/d/", "base", 0755, GIT_MKDIR_PATH, NULL));
	cl_assert(git_fs_path_isdir("base/dir/here/d"));

	/* fail: path with shorter base and existing dirs */
	cl_git_fail(git_futils_mkdir_relative("dir/here/e/", "base", 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL, NULL));

	/* fail: base with missing components */
	cl_git_fail(git_futils_mkdir_relative("f/", "base/missing", 0755, GIT_MKDIR_PATH, NULL));

	/* success: shift missing component to path */
	cl_git_pass(git_futils_mkdir_relative("missing/f/", "base/", 0755, GIT_MKDIR_PATH, NULL));
}

static void cleanup_chmod_root(void *ref)
{
	mode_t *mode = ref;

	if (mode != NULL) {
		(void)p_umask(*mode);
		git__free(mode);
	}

	git_futils_rmdir_r("r", NULL, GIT_RMDIR_EMPTY_HIERARCHY);
}

#define check_mode(X,A) check_mode_at_line((X), (A), __FILE__, __func__, __LINE__)

static void check_mode_at_line(
	mode_t expected, mode_t actual,
	const char *file, const char *func, int line)
{
	/* FAT filesystems don't support exec bit, nor group/world bits */
	if (!cl_is_chmod_supported()) {
		expected &= 0600;
		actual &= 0600;
	}

	clar__assert_equal(
		file, func, line, "expected_mode != actual_mode", 1,
		"%07o", (int)expected, (int)(actual & 0777));
}

void test_mkdir__chmods(void)
{
	struct stat st;
	mode_t *old = git__malloc(sizeof(mode_t));
	*old = p_umask(022);

	cl_set_cleanup(cleanup_chmod_root, old);

	cl_git_pass(git_futils_mkdir("r", 0777, 0));

	cl_git_pass(git_futils_mkdir_relative("mode/is/important", "r", 0777, GIT_MKDIR_PATH, NULL));

	cl_git_pass(git_fs_path_lstat("r/mode", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_fs_path_lstat("r/mode/is", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_fs_path_lstat("r/mode/is/important", &st));
	check_mode(0755, st.st_mode);

	cl_git_pass(git_futils_mkdir_relative("mode2/is2/important2", "r", 0777, GIT_MKDIR_PATH | GIT_MKDIR_CHMOD, NULL));

	cl_git_pass(git_fs_path_lstat("r/mode2", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_fs_path_lstat("r/mode2/is2", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_fs_path_lstat("r/mode2/is2/important2", &st));
	check_mode(0777, st.st_mode);

	cl_git_pass(git_futils_mkdir_relative("mode3/is3/important3", "r", 0777, GIT_MKDIR_PATH | GIT_MKDIR_CHMOD_PATH, NULL));

	cl_git_pass(git_fs_path_lstat("r/mode3", &st));
	check_mode(0777, st.st_mode);
	cl_git_pass(git_fs_path_lstat("r/mode3/is3", &st));
	check_mode(0777, st.st_mode);
	cl_git_pass(git_fs_path_lstat("r/mode3/is3/important3", &st));
	check_mode(0777, st.st_mode);

	/* test that we chmod existing dir */

	cl_git_pass(git_futils_mkdir_relative("mode/is/important", "r", 0777, GIT_MKDIR_PATH | GIT_MKDIR_CHMOD, NULL));

	cl_git_pass(git_fs_path_lstat("r/mode", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_fs_path_lstat("r/mode/is", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_fs_path_lstat("r/mode/is/important", &st));
	check_mode(0777, st.st_mode);

	/* test that we chmod even existing dirs if CHMOD_PATH is set */

	cl_git_pass(git_futils_mkdir_relative("mode2/is2/important2.1", "r", 0777, GIT_MKDIR_PATH | GIT_MKDIR_CHMOD_PATH, NULL));

	cl_git_pass(git_fs_path_lstat("r/mode2", &st));
	check_mode(0777, st.st_mode);
	cl_git_pass(git_fs_path_lstat("r/mode2/is2", &st));
	check_mode(0777, st.st_mode);
	cl_git_pass(git_fs_path_lstat("r/mode2/is2/important2.1", &st));
	check_mode(0777, st.st_mode);
}

void test_mkdir__keeps_parent_symlinks(void)
{
#ifndef GIT_WIN32
	git_str path = GIT_STR_INIT;

	cl_set_cleanup(cleanup_basic_dirs, NULL);

	/* make a directory */
	cl_assert(!git_fs_path_isdir("d0"));
	cl_git_pass(git_futils_mkdir("d0", 0755, 0));
	cl_assert(git_fs_path_isdir("d0"));

	cl_must_pass(symlink("d0", "d1"));
	cl_assert(git_fs_path_islink("d1"));

	cl_git_pass(git_futils_mkdir("d1/foo/bar", 0755, GIT_MKDIR_PATH|GIT_MKDIR_REMOVE_SYMLINKS));
	cl_assert(git_fs_path_islink("d1"));
	cl_assert(git_fs_path_isdir("d1/foo/bar"));
	cl_assert(git_fs_path_isdir("d0/foo/bar"));

	cl_must_pass(symlink("d0", "d2"));
	cl_assert(git_fs_path_islink("d2"));

	git_str_joinpath(&path, clar_sandbox_path(), "d2/other/dir");

	cl_git_pass(git_futils_mkdir(path.ptr, 0755, GIT_MKDIR_PATH|GIT_MKDIR_REMOVE_SYMLINKS));
	cl_assert(git_fs_path_islink("d2"));
	cl_assert(git_fs_path_isdir("d2/other/dir"));
	cl_assert(git_fs_path_isdir("d0/other/dir"));

	git_str_dispose(&path);
#endif
}

void test_mkdir__mkdir_path_inside_unwriteable_parent(void)
{
	struct stat st;
	mode_t *old;

	/* FAT filesystems don't support exec bit, nor group/world bits */
	if (!cl_is_chmod_supported())
		return;

	cl_assert((old = git__malloc(sizeof(mode_t))) != NULL);
	*old = p_umask(022);
	cl_set_cleanup(cleanup_chmod_root, old);

	cl_git_pass(git_futils_mkdir("r", 0777, 0));
	cl_git_pass(git_futils_mkdir_relative("mode/is/important", "r", 0777, GIT_MKDIR_PATH, NULL));
	cl_git_pass(git_fs_path_lstat("r/mode", &st));
	check_mode(0755, st.st_mode);

	cl_must_pass(p_chmod("r/mode", 0111));
	cl_git_pass(git_fs_path_lstat("r/mode", &st));
	check_mode(0111, st.st_mode);

	cl_git_pass(
		git_futils_mkdir_relative("mode/is/okay/inside", "r", 0777, GIT_MKDIR_PATH, NULL));
	cl_git_pass(git_fs_path_lstat("r/mode/is/okay/inside", &st));
	check_mode(0755, st.st_mode);

	cl_must_pass(p_chmod("r/mode", 0777));
}
