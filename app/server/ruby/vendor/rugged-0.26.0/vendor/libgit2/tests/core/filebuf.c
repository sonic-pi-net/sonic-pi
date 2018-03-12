#include "clar_libgit2.h"
#include "filebuf.h"

/* make sure git_filebuf_open doesn't delete an existing lock */
void test_core_filebuf__0(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	int fd;
	char test[] = "test", testlock[] = "test.lock";

	fd = p_creat(testlock, 0744); //-V536

	cl_must_pass(fd);
	cl_must_pass(p_close(fd));

	cl_git_fail(git_filebuf_open(&file, test, 0, 0666));
	cl_assert(git_path_exists(testlock));

	cl_must_pass(p_unlink(testlock));
}


/* make sure GIT_FILEBUF_APPEND works as expected */
void test_core_filebuf__1(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	char test[] = "test";

	cl_git_mkfile(test, "libgit2 rocks\n");

	cl_git_pass(git_filebuf_open(&file, test, GIT_FILEBUF_APPEND, 0666));
	cl_git_pass(git_filebuf_printf(&file, "%s\n", "libgit2 rocks"));
	cl_git_pass(git_filebuf_commit(&file));

	cl_assert_equal_file("libgit2 rocks\nlibgit2 rocks\n", 0, test);

	cl_must_pass(p_unlink(test));
}


/* make sure git_filebuf_write writes large buffer correctly */
void test_core_filebuf__2(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	char test[] = "test";
	unsigned char buf[4096 * 4]; /* 2 * WRITE_BUFFER_SIZE */

	memset(buf, 0xfe, sizeof(buf));

	cl_git_pass(git_filebuf_open(&file, test, 0, 0666));
	cl_git_pass(git_filebuf_write(&file, buf, sizeof(buf)));
	cl_git_pass(git_filebuf_commit(&file));

	cl_assert_equal_file((char *)buf, sizeof(buf), test);

	cl_must_pass(p_unlink(test));
}

/* make sure git_filebuf_cleanup clears the buffer */
void test_core_filebuf__4(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	char test[] = "test";

	cl_assert(file.buffer == NULL);

	cl_git_pass(git_filebuf_open(&file, test, 0, 0666));
	cl_assert(file.buffer != NULL);

	git_filebuf_cleanup(&file);
	cl_assert(file.buffer == NULL);
}


/* make sure git_filebuf_commit clears the buffer */
void test_core_filebuf__5(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	char test[] = "test";

	cl_assert(file.buffer == NULL);

	cl_git_pass(git_filebuf_open(&file, test, 0, 0666));
	cl_assert(file.buffer != NULL);
	cl_git_pass(git_filebuf_printf(&file, "%s\n", "libgit2 rocks"));
	cl_assert(file.buffer != NULL);

	cl_git_pass(git_filebuf_commit(&file));
	cl_assert(file.buffer == NULL);

	cl_must_pass(p_unlink(test));
}


/* make sure git_filebuf_commit takes umask into account */
void test_core_filebuf__umask(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	char test[] = "test";
	struct stat statbuf;
	mode_t mask, os_mask;

#ifdef GIT_WIN32
	os_mask = 0600;
#else
	os_mask = 0777;
#endif

	p_umask(mask = p_umask(0));

	cl_assert(file.buffer == NULL);

	cl_git_pass(git_filebuf_open(&file, test, 0, 0666));
	cl_assert(file.buffer != NULL);
	cl_git_pass(git_filebuf_printf(&file, "%s\n", "libgit2 rocks"));
	cl_assert(file.buffer != NULL);

	cl_git_pass(git_filebuf_commit(&file));
	cl_assert(file.buffer == NULL);

	cl_must_pass(p_stat("test", &statbuf));
	cl_assert_equal_i(statbuf.st_mode & os_mask, (0666 & ~mask) & os_mask);

	cl_must_pass(p_unlink(test));
}

void test_core_filebuf__rename_error(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	char *dir = "subdir",  *test = "subdir/test", *test_lock = "subdir/test.lock";
	int fd;

#ifndef GIT_WIN32
	cl_skip();
#endif

	cl_git_pass(p_mkdir(dir, 0666));
	cl_git_mkfile(test, "dummy content");
	fd = p_open(test, O_RDONLY);
	cl_assert(fd > 0);
	cl_git_pass(git_filebuf_open(&file, test, 0, 0666));

	cl_git_pass(git_filebuf_printf(&file, "%s\n", "libgit2 rocks"));

	cl_assert_equal_i(true, git_path_exists(test_lock));

	cl_git_fail(git_filebuf_commit(&file));
	p_close(fd);

	git_filebuf_cleanup(&file);

	cl_assert_equal_i(false, git_path_exists(test_lock));
}

void test_core_filebuf__symlink_follow(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	const char *dir = "linkdir", *source = "linkdir/link";

#ifdef GIT_WIN32
	cl_skip();
#endif

	cl_git_pass(p_mkdir(dir, 0777));
	cl_git_pass(p_symlink("target", source));

	cl_git_pass(git_filebuf_open(&file, source, 0, 0666));
	cl_git_pass(git_filebuf_printf(&file, "%s\n", "libgit2 rocks"));

	cl_assert_equal_i(true, git_path_exists("linkdir/target.lock"));

	cl_git_pass(git_filebuf_commit(&file));
	cl_assert_equal_i(true, git_path_exists("linkdir/target"));

	git_filebuf_cleanup(&file);

	/* The second time around, the target file does exist */
	cl_git_pass(git_filebuf_open(&file, source, 0, 0666));
	cl_git_pass(git_filebuf_printf(&file, "%s\n", "libgit2 rocks"));

	cl_assert_equal_i(true, git_path_exists("linkdir/target.lock"));

	cl_git_pass(git_filebuf_commit(&file));
	cl_assert_equal_i(true, git_path_exists("linkdir/target"));

	git_filebuf_cleanup(&file);
	cl_git_pass(git_futils_rmdir_r(dir, NULL, GIT_RMDIR_REMOVE_FILES));
}

void test_core_filebuf__symlink_follow_absolute_paths(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	git_buf source = GIT_BUF_INIT, target = GIT_BUF_INIT;

#ifdef GIT_WIN32
	cl_skip();
#endif

	cl_git_pass(git_buf_joinpath(&source, clar_sandbox_path(), "linkdir/link"));
	cl_git_pass(git_buf_joinpath(&target, clar_sandbox_path(), "linkdir/target"));
	cl_git_pass(p_mkdir("linkdir", 0777));
	cl_git_pass(p_symlink(target.ptr, source.ptr));

	cl_git_pass(git_filebuf_open(&file, source.ptr, 0, 0666));
	cl_git_pass(git_filebuf_printf(&file, "%s\n", "libgit2 rocks"));

	cl_assert_equal_i(true, git_path_exists("linkdir/target.lock"));

	cl_git_pass(git_filebuf_commit(&file));
	cl_assert_equal_i(true, git_path_exists("linkdir/target"));

	git_filebuf_cleanup(&file);
	git_buf_free(&source);
	git_buf_free(&target);

	cl_git_pass(git_futils_rmdir_r("linkdir", NULL, GIT_RMDIR_REMOVE_FILES));
}

void test_core_filebuf__symlink_depth(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	const char *dir = "linkdir", *source = "linkdir/link";

#ifdef GIT_WIN32
	cl_skip();
#endif

	cl_git_pass(p_mkdir(dir, 0777));
	/* Endless loop */
	cl_git_pass(p_symlink("link", source));

	cl_git_fail(git_filebuf_open(&file, source, 0, 0666));

	cl_git_pass(git_futils_rmdir_r(dir, NULL, GIT_RMDIR_REMOVE_FILES));
}

void test_core_filebuf__hidden_file(void)
{
#ifndef GIT_WIN32
	cl_skip();
#else
	git_filebuf file = GIT_FILEBUF_INIT;
	char *dir = "hidden", *test = "hidden/test";
	bool hidden;

	cl_git_pass(p_mkdir(dir, 0666));
	cl_git_mkfile(test, "dummy content");

	cl_git_pass(git_win32__set_hidden(test, true));
	cl_git_pass(git_win32__hidden(&hidden, test));
	cl_assert(hidden);

	cl_git_pass(git_filebuf_open(&file, test, 0, 0666));

	cl_git_pass(git_filebuf_printf(&file, "%s\n", "libgit2 rocks"));

	cl_git_pass(git_filebuf_commit(&file));

	git_filebuf_cleanup(&file);
#endif
}

void test_core_filebuf__detects_directory(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;

	cl_must_pass(p_mkdir("foo", 0777));
	cl_git_fail_with(GIT_EDIRECTORY, git_filebuf_open(&file, "foo", 0, 0666));
	cl_must_pass(p_rmdir("foo"));
}
