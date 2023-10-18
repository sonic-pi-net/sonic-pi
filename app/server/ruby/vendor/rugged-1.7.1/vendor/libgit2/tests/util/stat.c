#include "clar_libgit2.h"
#include "futils.h"
#include "posix.h"

void test_stat__initialize(void)
{
	cl_git_pass(git_futils_mkdir("root/d1/d2", 0755, GIT_MKDIR_PATH));
	cl_git_mkfile("root/file", "whatever\n");
	cl_git_mkfile("root/d1/file", "whatever\n");
}

void test_stat__cleanup(void)
{
	git_futils_rmdir_r("root", NULL, GIT_RMDIR_REMOVE_FILES);
}

#define cl_assert_error(val) \
	do { err = errno; cl_assert_equal_i((val), err); } while (0)

void test_stat__0(void)
{
	struct stat st;
	int err;

	cl_assert_equal_i(0, p_lstat("root", &st));
	cl_assert(S_ISDIR(st.st_mode));
	cl_assert_error(0);

	cl_assert_equal_i(0, p_lstat("root/", &st));
	cl_assert(S_ISDIR(st.st_mode));
	cl_assert_error(0);

	cl_assert_equal_i(0, p_lstat("root/file", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_error(0);

	cl_assert_equal_i(0, p_lstat("root/d1", &st));
	cl_assert(S_ISDIR(st.st_mode));
	cl_assert_error(0);

	cl_assert_equal_i(0, p_lstat("root/d1/", &st));
	cl_assert(S_ISDIR(st.st_mode));
	cl_assert_error(0);

	cl_assert_equal_i(0, p_lstat("root/d1/file", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_error(0);

	cl_assert(p_lstat("root/missing", &st) < 0);
	cl_assert_error(ENOENT);

	cl_assert(p_lstat("root/missing/but/could/be/created", &st) < 0);
	cl_assert_error(ENOENT);

	cl_assert(p_lstat_posixly("root/missing/but/could/be/created", &st) < 0);
	cl_assert_error(ENOENT);

	cl_assert(p_lstat("root/d1/missing", &st) < 0);
	cl_assert_error(ENOENT);

	cl_assert(p_lstat("root/d1/missing/deeper/path", &st) < 0);
	cl_assert_error(ENOENT);

	cl_assert(p_lstat_posixly("root/d1/missing/deeper/path", &st) < 0);
	cl_assert_error(ENOENT);

	cl_assert(p_lstat_posixly("root/d1/file/deeper/path", &st) < 0);
	cl_assert_error(ENOTDIR);

	cl_assert(p_lstat("root/file/invalid", &st) < 0);
#ifdef GIT_WIN32
	cl_assert_error(ENOENT);
#else
	cl_assert_error(ENOTDIR);
#endif

	cl_assert(p_lstat_posixly("root/file/invalid", &st) < 0);
	cl_assert_error(ENOTDIR);

	cl_assert(p_lstat("root/file/invalid/deeper_path", &st) < 0);
#ifdef GIT_WIN32
	cl_assert_error(ENOENT);
#else
	cl_assert_error(ENOTDIR);
#endif

	cl_assert(p_lstat_posixly("root/file/invalid/deeper_path", &st) < 0);
	cl_assert_error(ENOTDIR);

	cl_assert(p_lstat_posixly("root/d1/file/extra", &st) < 0);
	cl_assert_error(ENOTDIR);

	cl_assert(p_lstat_posixly("root/d1/file/further/invalid/items", &st) < 0);
	cl_assert_error(ENOTDIR);
}

void test_stat__root(void)
{
	const char *sandbox = clar_sandbox_path();
	git_str root = GIT_STR_INIT;
	int root_len;
	struct stat st;

	root_len = git_fs_path_root(sandbox);
	cl_assert(root_len >= 0);

	git_str_set(&root, sandbox, root_len+1);

	cl_must_pass(p_stat(root.ptr, &st));
	cl_assert(S_ISDIR(st.st_mode));

	git_str_dispose(&root);
}
