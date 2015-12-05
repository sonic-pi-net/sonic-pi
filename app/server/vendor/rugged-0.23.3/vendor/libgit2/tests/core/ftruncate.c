/**
 * Some tests for p_ftruncate() to ensure that
 * properly handles large (2Gb+) files.
 */

#include "clar_libgit2.h"

static const char *filename = "core_ftruncate.txt";
static int fd = -1;

void test_core_ftruncate__initialize(void)
{
	if (!cl_getenv("GITTEST_INVASIVE_FS_SIZE"))
		cl_skip();

	cl_must_pass((fd = p_open(filename, O_CREAT | O_RDWR, 0644)));
}

void test_core_ftruncate__cleanup(void)
{
	if (fd < 0)
		return;

	p_close(fd);
	fd = 0;

	p_unlink(filename);
}

static void _extend(git_off_t i64len)
{
	struct stat st;
	int error;

	cl_assert((error = p_ftruncate(fd, i64len)) == 0);
	cl_assert((error = p_fstat(fd, &st)) == 0);
	cl_assert(st.st_size == i64len);
}

void test_core_ftruncate__2gb(void)
{
	_extend(0x80000001);
}

void test_core_ftruncate__4gb(void)
{
	_extend(0x100000001);
}
