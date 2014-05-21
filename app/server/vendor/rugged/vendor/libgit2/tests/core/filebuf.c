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

