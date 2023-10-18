#include "clar_libgit2.h"
#include "futils.h"

/* Fixture setup and teardown */
void test_futils__initialize(void)
{
	cl_must_pass(p_mkdir("futils", 0777));
}

void test_futils__cleanup(void)
{
	cl_fixture_cleanup("futils");
}

void test_futils__writebuffer(void)
{
	git_str out = GIT_STR_INIT,
		append = GIT_STR_INIT;

	/* create a new file */
	git_str_puts(&out, "hello!\n");
	git_str_printf(&out, "this is a %s\n", "test");

	cl_git_pass(git_futils_writebuffer(&out, "futils/test-file", O_RDWR|O_CREAT, 0666));

	cl_assert_equal_file(out.ptr, out.size, "futils/test-file");

	/* append some more data */
	git_str_puts(&append, "And some more!\n");
	git_str_put(&out, append.ptr, append.size);

	cl_git_pass(git_futils_writebuffer(&append, "futils/test-file", O_RDWR|O_APPEND, 0666));

	cl_assert_equal_file(out.ptr, out.size, "futils/test-file");

	git_str_dispose(&out);
	git_str_dispose(&append);
}

void test_futils__write_hidden_file(void)
{
#ifndef GIT_WIN32
	cl_skip();
#else
	git_str out = GIT_STR_INIT, append = GIT_STR_INIT;
	bool hidden;

	git_str_puts(&out, "hidden file.\n");
	git_futils_writebuffer(&out, "futils/test-file", O_RDWR | O_CREAT, 0666);

	cl_git_pass(git_win32__set_hidden("futils/test-file", true));

	/* append some more data */
	git_str_puts(&append, "And some more!\n");
	git_str_put(&out, append.ptr, append.size);

	cl_git_pass(git_futils_writebuffer(&append, "futils/test-file", O_RDWR | O_APPEND, 0666));

	cl_assert_equal_file(out.ptr, out.size, "futils/test-file");

	cl_git_pass(git_win32__hidden(&hidden, "futils/test-file"));
	cl_assert(hidden);

	git_str_dispose(&out);
	git_str_dispose(&append);
#endif
}

void test_futils__recursive_rmdir_keeps_symlink_targets(void)
{
	if (!git_fs_path_supports_symlinks(clar_sandbox_path()))
		cl_skip();

	cl_git_pass(git_futils_mkdir_r("a/b", 0777));
	cl_git_pass(git_futils_mkdir_r("dir-target", 0777));
	cl_git_mkfile("dir-target/file", "Contents");
	cl_git_mkfile("file-target", "Contents");
	cl_must_pass(p_symlink("dir-target", "a/symlink"));
	cl_must_pass(p_symlink("file-target", "a/b/symlink"));

	cl_git_pass(git_futils_rmdir_r("a", NULL, GIT_RMDIR_REMOVE_FILES));

	cl_assert(git_fs_path_exists("dir-target"));
	cl_assert(git_fs_path_exists("file-target"));

	cl_must_pass(p_unlink("dir-target/file"));
	cl_must_pass(p_rmdir("dir-target"));
	cl_must_pass(p_unlink("file-target"));
}

void test_futils__mktmp_umask(void)
{
#ifdef GIT_WIN32
	cl_skip();
#else
	git_str path = GIT_STR_INIT;
	struct stat st;
	int fd;

	umask(0);
	cl_assert((fd = git_futils_mktmp(&path, "foo", 0777)) >= 0);
	cl_must_pass(p_fstat(fd, &st));
	cl_assert_equal_i(st.st_mode & 0777, 0777);
	cl_must_pass(p_unlink(path.ptr));
	close(fd);

	umask(077);
	cl_assert((fd = git_futils_mktmp(&path, "foo", 0777)) >= 0);
	cl_must_pass(p_fstat(fd, &st));
	cl_assert_equal_i(st.st_mode & 0777, 0700);
	cl_must_pass(p_unlink(path.ptr));
	close(fd);
	git_str_dispose(&path);
#endif
}
