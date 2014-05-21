#include "clar_libgit2.h"
#include "fileops.h"
#include "path.h"
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

void test_core_mkdir__basic(void)
{
	cl_set_cleanup(cleanup_basic_dirs, NULL);

	/* make a directory */
	cl_assert(!git_path_isdir("d0"));
	cl_git_pass(git_futils_mkdir("d0", NULL, 0755, 0));
	cl_assert(git_path_isdir("d0"));

	/* make a path */
	cl_assert(!git_path_isdir("d1"));
	cl_git_pass(git_futils_mkdir("d1/d1.1/d1.2", NULL, 0755, GIT_MKDIR_PATH));
	cl_assert(git_path_isdir("d1"));
	cl_assert(git_path_isdir("d1/d1.1"));
	cl_assert(git_path_isdir("d1/d1.1/d1.2"));

	/* make a dir exclusively */
	cl_assert(!git_path_isdir("d2"));
	cl_git_pass(git_futils_mkdir("d2", NULL, 0755, GIT_MKDIR_EXCL));
	cl_assert(git_path_isdir("d2"));

	/* make exclusive failure */
	cl_git_fail(git_futils_mkdir("d2", NULL, 0755, GIT_MKDIR_EXCL));

	/* make a path exclusively */
	cl_assert(!git_path_isdir("d3"));
	cl_git_pass(git_futils_mkdir("d3/d3.1/d3.2", NULL, 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL));
	cl_assert(git_path_isdir("d3"));
	cl_assert(git_path_isdir("d3/d3.1/d3.2"));

	/* make exclusive path failure */
	cl_git_fail(git_futils_mkdir("d3/d3.1/d3.2", NULL, 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL));
	/* ??? Should EXCL only apply to the last item in the path? */

	/* path with trailing slash? */
	cl_assert(!git_path_isdir("d4"));
	cl_git_pass(git_futils_mkdir("d4/d4.1/", NULL, 0755, GIT_MKDIR_PATH));
	cl_assert(git_path_isdir("d4/d4.1"));
}

static void cleanup_basedir(void *ref)
{
	GIT_UNUSED(ref);
	git_futils_rmdir_r("base", NULL, GIT_RMDIR_EMPTY_HIERARCHY);
}

void test_core_mkdir__with_base(void)
{
#define BASEDIR "base/dir/here"

	cl_set_cleanup(cleanup_basedir, NULL);

	cl_git_pass(git_futils_mkdir(BASEDIR, NULL, 0755, GIT_MKDIR_PATH));

	cl_git_pass(git_futils_mkdir("a", BASEDIR, 0755, 0));
	cl_assert(git_path_isdir(BASEDIR "/a"));

	cl_git_pass(git_futils_mkdir("b/b1/b2", BASEDIR, 0755, GIT_MKDIR_PATH));
	cl_assert(git_path_isdir(BASEDIR "/b/b1/b2"));

	/* exclusive with existing base */
	cl_git_pass(git_futils_mkdir("c/c1/c2", BASEDIR, 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL));

	/* fail: exclusive with duplicated suffix */
	cl_git_fail(git_futils_mkdir("c/c1/c3", BASEDIR, 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL));

	/* fail: exclusive with any duplicated component */
	cl_git_fail(git_futils_mkdir("c/cz/cz", BASEDIR, 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL));

	/* success: exclusive without path */
	cl_git_pass(git_futils_mkdir("c/c1/c3", BASEDIR, 0755, GIT_MKDIR_EXCL));

	/* path with shorter base and existing dirs */
	cl_git_pass(git_futils_mkdir("dir/here/d/", "base", 0755, GIT_MKDIR_PATH));
	cl_assert(git_path_isdir("base/dir/here/d"));

	/* fail: path with shorter base and existing dirs */
	cl_git_fail(git_futils_mkdir("dir/here/e/", "base", 0755, GIT_MKDIR_PATH | GIT_MKDIR_EXCL));

	/* fail: base with missing components */
	cl_git_fail(git_futils_mkdir("f/", "base/missing", 0755, GIT_MKDIR_PATH));

	/* success: shift missing component to path */
	cl_git_pass(git_futils_mkdir("missing/f/", "base/", 0755, GIT_MKDIR_PATH));
}

static void cleanup_chmod_root(void *ref)
{
	mode_t *mode = ref;

	if (*mode != 0) {
		(void)p_umask(*mode);
		git__free(mode);
	}

	git_futils_rmdir_r("r", NULL, GIT_RMDIR_EMPTY_HIERARCHY);
}

#define check_mode(X,A) check_mode_at_line((X), (A), __FILE__, __LINE__)

static void check_mode_at_line(
	mode_t expected, mode_t actual, const char *file, int line)
{
	/* FAT filesystems don't support exec bit, nor group/world bits */
	if (!cl_is_chmod_supported()) {
		expected &= 0600;
		actual &= 0600;
	}

	clar__assert_equal(
		file, line, "expected_mode != actual_mode", 1,
		"%07o", (int)expected, (int)(actual & 0777));
}

void test_core_mkdir__chmods(void)
{
	struct stat st;
	mode_t *old = git__malloc(sizeof(mode_t));
	*old = p_umask(022);

	cl_set_cleanup(cleanup_chmod_root, old);

	cl_git_pass(git_futils_mkdir("r", NULL, 0777, 0));

	cl_git_pass(git_futils_mkdir("mode/is/important", "r", 0777, GIT_MKDIR_PATH));

	cl_git_pass(git_path_lstat("r/mode", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_path_lstat("r/mode/is", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_path_lstat("r/mode/is/important", &st));
	check_mode(0755, st.st_mode);

	cl_git_pass(git_futils_mkdir("mode2/is2/important2", "r", 0777, GIT_MKDIR_PATH | GIT_MKDIR_CHMOD));

	cl_git_pass(git_path_lstat("r/mode2", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_path_lstat("r/mode2/is2", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_path_lstat("r/mode2/is2/important2", &st));
	check_mode(0777, st.st_mode);

	cl_git_pass(git_futils_mkdir("mode3/is3/important3", "r", 0777, GIT_MKDIR_PATH | GIT_MKDIR_CHMOD_PATH));

	cl_git_pass(git_path_lstat("r/mode3", &st));
	check_mode(0777, st.st_mode);
	cl_git_pass(git_path_lstat("r/mode3/is3", &st));
	check_mode(0777, st.st_mode);
	cl_git_pass(git_path_lstat("r/mode3/is3/important3", &st));
	check_mode(0777, st.st_mode);

	/* test that we chmod existing dir */

	cl_git_pass(git_futils_mkdir("mode/is/important", "r", 0777, GIT_MKDIR_PATH | GIT_MKDIR_CHMOD));

	cl_git_pass(git_path_lstat("r/mode", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_path_lstat("r/mode/is", &st));
	check_mode(0755, st.st_mode);
	cl_git_pass(git_path_lstat("r/mode/is/important", &st));
	check_mode(0777, st.st_mode);

	/* test that we chmod even existing dirs if CHMOD_PATH is set */

	cl_git_pass(git_futils_mkdir("mode2/is2/important2.1", "r", 0777, GIT_MKDIR_PATH | GIT_MKDIR_CHMOD_PATH));

	cl_git_pass(git_path_lstat("r/mode2", &st));
	check_mode(0777, st.st_mode);
	cl_git_pass(git_path_lstat("r/mode2/is2", &st));
	check_mode(0777, st.st_mode);
	cl_git_pass(git_path_lstat("r/mode2/is2/important2.1", &st));
	check_mode(0777, st.st_mode);
}
