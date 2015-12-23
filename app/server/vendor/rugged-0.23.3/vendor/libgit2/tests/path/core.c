#include "clar_libgit2.h"
#include "path.h"

static void test_make_relative(
	const char *expected_path,
	const char *path,
	const char *parent,
	int expected_status)
{
	git_buf buf = GIT_BUF_INIT;
	git_buf_puts(&buf, path);
	cl_assert_equal_i(expected_status, git_path_make_relative(&buf, parent));
	cl_assert_equal_s(expected_path, buf.ptr);
	git_buf_free(&buf);
}

void test_path_core__make_relative(void)
{
	test_make_relative("foo.c", "/path/to/foo.c", "/path/to", 0);
	test_make_relative("bar/foo.c", "/path/to/bar/foo.c", "/path/to", 0);
	test_make_relative("foo.c", "/path/to/foo.c", "/path/to/", 0);

	test_make_relative("", "/path/to", "/path/to", 0);
	test_make_relative("", "/path/to", "/path/to/", 0);

	test_make_relative("../", "/path/to", "/path/to/foo", 0);

	test_make_relative("../foo.c", "/path/to/foo.c", "/path/to/bar", 0);
	test_make_relative("../bar/foo.c", "/path/to/bar/foo.c", "/path/to/baz", 0);

	test_make_relative("../../foo.c", "/path/to/foo.c", "/path/to/foo/bar", 0);
	test_make_relative("../../foo/bar.c", "/path/to/foo/bar.c", "/path/to/bar/foo", 0);

	test_make_relative("../../foo.c", "/foo.c", "/bar/foo", 0);

	test_make_relative("foo.c", "/path/to/foo.c", "/path/to/", 0);
	test_make_relative("../foo.c", "/path/to/foo.c", "/path/to/bar/", 0);

	test_make_relative("foo.c", "d:/path/to/foo.c", "d:/path/to", 0);

	test_make_relative("../foo", "/foo", "/bar", 0);
	test_make_relative("path/to/foo.c", "/path/to/foo.c", "/", 0);
	test_make_relative("../foo", "path/to/foo", "path/to/bar", 0);

	test_make_relative("/path/to/foo.c", "/path/to/foo.c", "d:/path/to", GIT_ENOTFOUND);
	test_make_relative("d:/path/to/foo.c", "d:/path/to/foo.c", "/path/to", GIT_ENOTFOUND);
	
	test_make_relative("/path/to/foo.c", "/path/to/foo.c", "not-a-rooted-path", GIT_ENOTFOUND);
	test_make_relative("not-a-rooted-path", "not-a-rooted-path", "/path/to", GIT_ENOTFOUND);
	
	test_make_relative("/path", "/path", "pathtofoo", GIT_ENOTFOUND);
	test_make_relative("path", "path", "pathtofoo", GIT_ENOTFOUND);
}

void test_path_core__isvalid_standard(void)
{
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/bar", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/bar/file.txt", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/bar/.file", 0));
}

void test_path_core__isvalid_empty_dir_component(void)
{
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo//bar", 0));

	/* leading slash */
	cl_assert_equal_b(false, git_path_isvalid(NULL, "/", 0));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "/foo", 0));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "/foo/bar", 0));

	/* trailing slash */
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/", 0));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/bar/", 0));
}

void test_path_core__isvalid_dot_and_dotdot(void)
{
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "./foo", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/.", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "./foo", 0));

	cl_assert_equal_b(true, git_path_isvalid(NULL, "..", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "../foo", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/..", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "../foo", 0));

	cl_assert_equal_b(false, git_path_isvalid(NULL, ".", GIT_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "./foo", GIT_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/.", GIT_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "./foo", GIT_PATH_REJECT_TRAVERSAL));

	cl_assert_equal_b(false, git_path_isvalid(NULL, "..", GIT_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "../foo", GIT_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/..", GIT_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "../foo", GIT_PATH_REJECT_TRAVERSAL));
}

void test_path_core__isvalid_dot_git(void)
{
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".git", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".git/foo", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/.git", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/.git/bar", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/.GIT/bar", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/bar/.Git", 0));

	cl_assert_equal_b(false, git_path_isvalid(NULL, ".git", GIT_PATH_REJECT_DOT_GIT));
	cl_assert_equal_b(false, git_path_isvalid(NULL, ".git/foo", GIT_PATH_REJECT_DOT_GIT));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/.git", GIT_PATH_REJECT_DOT_GIT));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/.git/bar", GIT_PATH_REJECT_DOT_GIT));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/.GIT/bar", GIT_PATH_REJECT_DOT_GIT));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/bar/.Git", GIT_PATH_REJECT_DOT_GIT));

	cl_assert_equal_b(true, git_path_isvalid(NULL, "!git", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/!git", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "!git/bar", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".tig", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/.tig", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".tig/bar", 0));
}

void test_path_core__isvalid_backslash(void)
{
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo\\file.txt", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/bar\\file.txt", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/bar\\", 0));

	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo\\file.txt", GIT_PATH_REJECT_BACKSLASH));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/bar\\file.txt", GIT_PATH_REJECT_BACKSLASH));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/bar\\", GIT_PATH_REJECT_BACKSLASH));
}

void test_path_core__isvalid_trailing_dot(void)
{
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo.", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo...", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/bar.", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo./bar", 0));

	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo.", GIT_PATH_REJECT_TRAILING_DOT));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo...", GIT_PATH_REJECT_TRAILING_DOT));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/bar.", GIT_PATH_REJECT_TRAILING_DOT));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo./bar", GIT_PATH_REJECT_TRAILING_DOT));
}

void test_path_core__isvalid_trailing_space(void)
{
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo ", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo   ", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/bar ", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, " ", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo /bar", 0));

	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo ", GIT_PATH_REJECT_TRAILING_SPACE));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo   ", GIT_PATH_REJECT_TRAILING_SPACE));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/bar ", GIT_PATH_REJECT_TRAILING_SPACE));
	cl_assert_equal_b(false, git_path_isvalid(NULL, " ", GIT_PATH_REJECT_TRAILING_SPACE));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo /bar", GIT_PATH_REJECT_TRAILING_SPACE));
}

void test_path_core__isvalid_trailing_colon(void)
{
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo:", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo/bar:", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ":", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "foo:/bar", 0));

	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo:", GIT_PATH_REJECT_TRAILING_COLON));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo/bar:", GIT_PATH_REJECT_TRAILING_COLON));
	cl_assert_equal_b(false, git_path_isvalid(NULL, ":", GIT_PATH_REJECT_TRAILING_COLON));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "foo:/bar", GIT_PATH_REJECT_TRAILING_COLON));
}

void test_path_core__isvalid_dotgit_ntfs(void)
{
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".git", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".git ", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".git.", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".git.. .", 0));

	cl_assert_equal_b(true, git_path_isvalid(NULL, "git~1", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "git~1 ", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "git~1.", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "git~1.. .", 0));

	cl_assert_equal_b(false, git_path_isvalid(NULL, ".git", GIT_PATH_REJECT_DOT_GIT_NTFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, ".git ", GIT_PATH_REJECT_DOT_GIT_NTFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, ".git.", GIT_PATH_REJECT_DOT_GIT_NTFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, ".git.. .", GIT_PATH_REJECT_DOT_GIT_NTFS));

	cl_assert_equal_b(false, git_path_isvalid(NULL, "git~1", GIT_PATH_REJECT_DOT_GIT_NTFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "git~1 ", GIT_PATH_REJECT_DOT_GIT_NTFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "git~1.", GIT_PATH_REJECT_DOT_GIT_NTFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "git~1.. .", GIT_PATH_REJECT_DOT_GIT_NTFS));
}

void test_path_core__isvalid_dos_paths(void)
{
	cl_assert_equal_b(true, git_path_isvalid(NULL, "aux", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "aux.", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "aux:", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "aux.asdf", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "aux.asdf\\zippy", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "aux:asdf\\foobar", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "con", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "prn", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "nul", 0));

	cl_assert_equal_b(false, git_path_isvalid(NULL, "aux", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "aux.", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "aux:", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "aux.asdf", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "aux.asdf\\zippy", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "aux:asdf\\foobar", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "con", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "prn", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "nul", GIT_PATH_REJECT_DOS_PATHS));

	cl_assert_equal_b(true, git_path_isvalid(NULL, "aux1", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "aux1", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "auxn", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "aux\\foo", GIT_PATH_REJECT_DOS_PATHS));
}

void test_path_core__isvalid_dos_paths_withnum(void)
{
	cl_assert_equal_b(true, git_path_isvalid(NULL, "com1", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "com1.", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "com1:", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "com1.asdf", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "com1.asdf\\zippy", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "com1:asdf\\foobar", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "com1\\foo", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "lpt1", 0));

	cl_assert_equal_b(false, git_path_isvalid(NULL, "com1", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "com1.", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "com1:", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "com1.asdf", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "com1.asdf\\zippy", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "com1:asdf\\foobar", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "com1/foo", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "lpt1", GIT_PATH_REJECT_DOS_PATHS));

	cl_assert_equal_b(true, git_path_isvalid(NULL, "com0", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "com0", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "com10", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "com10", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "comn", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "com1\\foo", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "lpt0", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "lpt10", GIT_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "lptn", GIT_PATH_REJECT_DOS_PATHS));
}

void test_path_core__isvalid_nt_chars(void)
{
	cl_assert_equal_b(true, git_path_isvalid(NULL, "asdf\001foo", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "asdf\037bar", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "asdf<bar", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "asdf>foo", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "asdf:foo", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "asdf\"bar", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "asdf|foo", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "asdf?bar", 0));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "asdf*bar", 0));

	cl_assert_equal_b(false, git_path_isvalid(NULL, "asdf\001foo", GIT_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "asdf\037bar", GIT_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "asdf<bar", GIT_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "asdf>foo", GIT_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "asdf:foo", GIT_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "asdf\"bar", GIT_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "asdf|foo", GIT_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "asdf?bar", GIT_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "asdf*bar", GIT_PATH_REJECT_NT_CHARS));
}

void test_path_core__isvalid_dotgit_with_hfs_ignorables(void)
{
	cl_assert_equal_b(false, git_path_isvalid(NULL, ".git", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, ".git\xe2\x80\x8c", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, ".gi\xe2\x80\x8dT", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, ".g\xe2\x80\x8eIt", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, ".\xe2\x80\x8fgIt", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "\xe2\x80\xaa.gIt", GIT_PATH_REJECT_DOT_GIT_HFS));

	cl_assert_equal_b(false, git_path_isvalid(NULL, "\xe2\x80\xab.\xe2\x80\xacG\xe2\x80\xadI\xe2\x80\xaet", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "\xe2\x81\xab.\xe2\x80\xaaG\xe2\x81\xabI\xe2\x80\xact", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(false, git_path_isvalid(NULL, "\xe2\x81\xad.\xe2\x80\xaeG\xef\xbb\xbfIT", GIT_PATH_REJECT_DOT_GIT_HFS));

	cl_assert_equal_b(true, git_path_isvalid(NULL, ".", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".g", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".gi", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, " .git", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "..git\xe2\x80\x8c", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".gi\xe2\x80\x8dT.", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".g\xe2\x80It", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".\xe2gIt", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, "\xe2\x80\xaa.gi", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".gi\x80\x8dT", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".gi\x8dT", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".g\xe2i\x80T\x8e", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".git\xe2\x80\xbf", GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(true, git_path_isvalid(NULL, ".git\xe2\xab\x81", GIT_PATH_REJECT_DOT_GIT_HFS));
}

static void test_join_unrooted(
	const char *expected_result,
	ssize_t expected_rootlen,
	const char *path,
	const char *base)
{
	git_buf result = GIT_BUF_INIT;
	ssize_t root_at;

	cl_git_pass(git_path_join_unrooted(&result, path, base, &root_at));
	cl_assert_equal_s(expected_result, result.ptr);
	cl_assert_equal_i(expected_rootlen, root_at);

	git_buf_free(&result);
}

void test_path_core__join_unrooted(void)
{
	git_buf out = GIT_BUF_INIT;

	test_join_unrooted("foo", 0, "foo", NULL);
	test_join_unrooted("foo/bar", 0, "foo/bar", NULL);

	/* Relative paths have base prepended */
	test_join_unrooted("/foo/bar", 4, "bar", "/foo");
	test_join_unrooted("/foo/bar/foobar", 4, "bar/foobar", "/foo");
	test_join_unrooted("c:/foo/bar/foobar", 6, "bar/foobar", "c:/foo");
	test_join_unrooted("c:/foo/bar/foobar", 10, "foobar", "c:/foo/bar");

	/* Absolute paths are not prepended with base */
	test_join_unrooted("/foo", 0, "/foo", "/asdf");
	test_join_unrooted("/foo/bar", 0, "/foo/bar", "/asdf");

	/* Drive letter is given as root length on Windows */
	test_join_unrooted("c:/foo", 2, "c:/foo", "c:/asdf");
	test_join_unrooted("c:/foo/bar", 2, "c:/foo/bar", "c:/asdf");

	/* Base is returned when it's provided and is the prefix */
	test_join_unrooted("c:/foo/bar/foobar", 6, "c:/foo/bar/foobar", "c:/foo");
	test_join_unrooted("c:/foo/bar/foobar", 10, "c:/foo/bar/foobar", "c:/foo/bar");

	/* Trailing slash in the base is ignored */
	test_join_unrooted("c:/foo/bar/foobar", 6, "c:/foo/bar/foobar", "c:/foo/");

	git_buf_free(&out);
}
