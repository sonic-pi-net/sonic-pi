#include "clar_libgit2.h"
#include "buffer.h"

static git_repository *_repo;

void test_repo_hashfile__initialize(void)
{
	_repo = cl_git_sandbox_init("status");
}

void test_repo_hashfile__cleanup(void)
{
	cl_git_sandbox_cleanup();
	_repo = NULL;
}

void test_repo_hashfile__simple(void)
{
	git_oid a, b;
	git_buf full = GIT_BUF_INIT;

	/* hash with repo relative path */
	cl_git_pass(git_odb_hashfile(&a, "status/current_file", GIT_OBJECT_BLOB));
	cl_git_pass(git_repository_hashfile(&b, _repo, "current_file", GIT_OBJECT_BLOB, NULL));
	cl_assert_equal_oid(&a, &b);

	cl_git_pass(git_buf_joinpath(&full, git_repository_workdir(_repo), "current_file"));

	/* hash with full path */
	cl_git_pass(git_odb_hashfile(&a, full.ptr, GIT_OBJECT_BLOB));
	cl_git_pass(git_repository_hashfile(&b, _repo, full.ptr, GIT_OBJECT_BLOB, NULL));
	cl_assert_equal_oid(&a, &b);

	/* hash with invalid type */
	cl_git_fail(git_odb_hashfile(&a, full.ptr, GIT_OBJECT_ANY));
	cl_git_fail(git_repository_hashfile(&b, _repo, full.ptr, GIT_OBJECT_OFS_DELTA, NULL));

	git_buf_dispose(&full);
}

void test_repo_hashfile__filtered(void)
{
	git_oid a, b;

	cl_repo_set_bool(_repo, "core.autocrlf", true);

	cl_git_append2file("status/.gitattributes", "*.txt text\n*.bin binary\n\n");

	/* create some sample content with CRLF in it */
	cl_git_mkfile("status/testfile.txt", "content\r\n");
	cl_git_mkfile("status/testfile.bin", "other\r\nstuff\r\n");

	/* not equal hashes because of filtering */
	cl_git_pass(git_odb_hashfile(&a, "status/testfile.txt", GIT_OBJECT_BLOB));
	cl_git_pass(git_repository_hashfile(&b, _repo, "testfile.txt", GIT_OBJECT_BLOB, NULL));
	cl_assert(git_oid_cmp(&a, &b));

	/* equal hashes because filter is binary */
	cl_git_pass(git_odb_hashfile(&a, "status/testfile.bin", GIT_OBJECT_BLOB));
	cl_git_pass(git_repository_hashfile(&b, _repo, "testfile.bin", GIT_OBJECT_BLOB, NULL));
	cl_assert_equal_oid(&a, &b);

	/* equal hashes when 'as_file' points to binary filtering */
	cl_git_pass(git_odb_hashfile(&a, "status/testfile.txt", GIT_OBJECT_BLOB));
	cl_git_pass(git_repository_hashfile(&b, _repo, "testfile.txt", GIT_OBJECT_BLOB, "foo.bin"));
	cl_assert_equal_oid(&a, &b);

	/* not equal hashes when 'as_file' points to text filtering */
	cl_git_pass(git_odb_hashfile(&a, "status/testfile.bin", GIT_OBJECT_BLOB));
	cl_git_pass(git_repository_hashfile(&b, _repo, "testfile.bin", GIT_OBJECT_BLOB, "foo.txt"));
	cl_assert(git_oid_cmp(&a, &b));

	/* equal hashes when 'as_file' is empty and turns off filtering */
	cl_git_pass(git_odb_hashfile(&a, "status/testfile.txt", GIT_OBJECT_BLOB));
	cl_git_pass(git_repository_hashfile(&b, _repo, "testfile.txt", GIT_OBJECT_BLOB, ""));
	cl_assert_equal_oid(&a, &b);

	cl_git_pass(git_odb_hashfile(&a, "status/testfile.bin", GIT_OBJECT_BLOB));
	cl_git_pass(git_repository_hashfile(&b, _repo, "testfile.bin", GIT_OBJECT_BLOB, ""));
	cl_assert_equal_oid(&a, &b);

	/* some hash type failures */
	cl_git_fail(git_odb_hashfile(&a, "status/testfile.txt", 0));
	cl_git_fail(git_repository_hashfile(&b, _repo, "testfile.txt", GIT_OBJECT_ANY, NULL));
}
