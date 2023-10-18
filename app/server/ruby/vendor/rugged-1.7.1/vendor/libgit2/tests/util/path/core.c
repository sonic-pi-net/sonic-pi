#include "clar_libgit2.h"
#include "fs_path.h"

static void test_make_relative(
	const char *expected_path,
	const char *path,
	const char *parent,
	int expected_status)
{
	git_str buf = GIT_STR_INIT;
	git_str_puts(&buf, path);
	cl_assert_equal_i(expected_status, git_fs_path_make_relative(&buf, parent));
	cl_assert_equal_s(expected_path, buf.ptr);
	git_str_dispose(&buf);
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
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/bar", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/bar/file.txt", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/bar/.file", 0));
}

/* Ensure that `is_valid_str` only reads str->size bytes */
void test_path_core__isvalid_standard_str(void)
{
	git_str str = GIT_STR_INIT_CONST("foo/bar//zap", 0);
	unsigned int flags = GIT_FS_PATH_REJECT_EMPTY_COMPONENT;

	str.size = 0;
	cl_assert_equal_b(false, git_fs_path_str_is_valid(&str, flags));

	str.size = 3;
	cl_assert_equal_b(true, git_fs_path_str_is_valid(&str, flags));

	str.size = 4;
	cl_assert_equal_b(false, git_fs_path_str_is_valid(&str, flags));

	str.size = 5;
	cl_assert_equal_b(true, git_fs_path_str_is_valid(&str, flags));

	str.size = 7;
	cl_assert_equal_b(true, git_fs_path_str_is_valid(&str, flags));

	str.size = 8;
	cl_assert_equal_b(false, git_fs_path_str_is_valid(&str, flags));

	str.size = strlen(str.ptr);
	cl_assert_equal_b(false, git_fs_path_str_is_valid(&str, flags));
}

void test_path_core__isvalid_empty_dir_component(void)
{
	unsigned int flags = GIT_FS_PATH_REJECT_EMPTY_COMPONENT;

	/* empty component */
	cl_assert_equal_b(true, git_fs_path_is_valid("foo//bar", 0));

	/* leading slash */
	cl_assert_equal_b(true, git_fs_path_is_valid("/", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("/foo", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("/foo/bar", 0));

	/* trailing slash */
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/bar/", 0));


	/* empty component */
	cl_assert_equal_b(false, git_fs_path_is_valid("foo//bar", flags));

	/* leading slash */
	cl_assert_equal_b(false, git_fs_path_is_valid("/", flags));
	cl_assert_equal_b(false, git_fs_path_is_valid("/foo", flags));
	cl_assert_equal_b(false, git_fs_path_is_valid("/foo/bar", flags));

	/* trailing slash */
	cl_assert_equal_b(false, git_fs_path_is_valid("foo/", flags));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo/bar/", flags));
}

void test_path_core__isvalid_dot_and_dotdot(void)
{
	cl_assert_equal_b(true, git_fs_path_is_valid(".", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("./foo", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/.", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("./foo", 0));

	cl_assert_equal_b(true, git_fs_path_is_valid("..", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("../foo", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/..", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("../foo", 0));

	cl_assert_equal_b(false, git_fs_path_is_valid(".", GIT_FS_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_fs_path_is_valid("./foo", GIT_FS_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo/.", GIT_FS_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_fs_path_is_valid("./foo", GIT_FS_PATH_REJECT_TRAVERSAL));

	cl_assert_equal_b(false, git_fs_path_is_valid("..", GIT_FS_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_fs_path_is_valid("../foo", GIT_FS_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo/..", GIT_FS_PATH_REJECT_TRAVERSAL));
	cl_assert_equal_b(false, git_fs_path_is_valid("../foo", GIT_FS_PATH_REJECT_TRAVERSAL));
}

void test_path_core__isvalid_backslash(void)
{
	cl_assert_equal_b(true, git_fs_path_is_valid("foo\\file.txt", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/bar\\file.txt", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/bar\\", 0));

	cl_assert_equal_b(false, git_fs_path_is_valid("foo\\file.txt", GIT_FS_PATH_REJECT_BACKSLASH));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo/bar\\file.txt", GIT_FS_PATH_REJECT_BACKSLASH));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo/bar\\", GIT_FS_PATH_REJECT_BACKSLASH));
}

void test_path_core__isvalid_trailing_dot(void)
{
	cl_assert_equal_b(true, git_fs_path_is_valid("foo.", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo...", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/bar.", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo./bar", 0));

	cl_assert_equal_b(false, git_fs_path_is_valid("foo.", GIT_FS_PATH_REJECT_TRAILING_DOT));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo...", GIT_FS_PATH_REJECT_TRAILING_DOT));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo/bar.", GIT_FS_PATH_REJECT_TRAILING_DOT));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo./bar", GIT_FS_PATH_REJECT_TRAILING_DOT));
}

void test_path_core__isvalid_trailing_space(void)
{
	cl_assert_equal_b(true, git_fs_path_is_valid("foo ", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo   ", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/bar ", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid(" ", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo /bar", 0));

	cl_assert_equal_b(false, git_fs_path_is_valid("foo ", GIT_FS_PATH_REJECT_TRAILING_SPACE));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo   ", GIT_FS_PATH_REJECT_TRAILING_SPACE));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo/bar ", GIT_FS_PATH_REJECT_TRAILING_SPACE));
	cl_assert_equal_b(false, git_fs_path_is_valid(" ", GIT_FS_PATH_REJECT_TRAILING_SPACE));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo /bar", GIT_FS_PATH_REJECT_TRAILING_SPACE));
}

void test_path_core__isvalid_trailing_colon(void)
{
	cl_assert_equal_b(true, git_fs_path_is_valid("foo:", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo/bar:", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid(":", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("foo:/bar", 0));

	cl_assert_equal_b(false, git_fs_path_is_valid("foo:", GIT_FS_PATH_REJECT_TRAILING_COLON));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo/bar:", GIT_FS_PATH_REJECT_TRAILING_COLON));
	cl_assert_equal_b(false, git_fs_path_is_valid(":", GIT_FS_PATH_REJECT_TRAILING_COLON));
	cl_assert_equal_b(false, git_fs_path_is_valid("foo:/bar", GIT_FS_PATH_REJECT_TRAILING_COLON));
}

void test_path_core__isvalid_dos_paths(void)
{
	cl_assert_equal_b(true, git_fs_path_is_valid("aux", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("aux.", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("aux:", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("aux.asdf", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("aux.asdf\\zippy", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("aux:asdf\\foobar", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("con", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("prn", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("nul", 0));

	cl_assert_equal_b(false, git_fs_path_is_valid("aux", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("aux.", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("aux:", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("aux.asdf", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("aux.asdf\\zippy", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("aux:asdf\\foobar", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("con", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("prn", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("nul", GIT_FS_PATH_REJECT_DOS_PATHS));

	cl_assert_equal_b(true, git_fs_path_is_valid("aux1", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("aux1", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_fs_path_is_valid("auxn", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_fs_path_is_valid("aux\\foo", GIT_FS_PATH_REJECT_DOS_PATHS));
}

void test_path_core__isvalid_dos_paths_withnum(void)
{
	cl_assert_equal_b(true, git_fs_path_is_valid("com1", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("com1.", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("com1:", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("com1.asdf", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("com1.asdf\\zippy", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("com1:asdf\\foobar", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("com1\\foo", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("lpt1", 0));

	cl_assert_equal_b(false, git_fs_path_is_valid("com1", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("com1.", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("com1:", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("com1.asdf", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("com1.asdf\\zippy", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("com1:asdf\\foobar", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("com1/foo", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(false, git_fs_path_is_valid("lpt1", GIT_FS_PATH_REJECT_DOS_PATHS));

	cl_assert_equal_b(true, git_fs_path_is_valid("com0", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("com0", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_fs_path_is_valid("com10", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("com10", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_fs_path_is_valid("comn", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_fs_path_is_valid("com1\\foo", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_fs_path_is_valid("lpt0", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_fs_path_is_valid("lpt10", GIT_FS_PATH_REJECT_DOS_PATHS));
	cl_assert_equal_b(true, git_fs_path_is_valid("lptn", GIT_FS_PATH_REJECT_DOS_PATHS));
}

void test_path_core__isvalid_nt_chars(void)
{
	cl_assert_equal_b(true, git_fs_path_is_valid("asdf\001foo", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("asdf\037bar", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("asdf<bar", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("asdf>foo", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("asdf:foo", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("asdf\"bar", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("asdf|foo", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("asdf?bar", 0));
	cl_assert_equal_b(true, git_fs_path_is_valid("asdf*bar", 0));

	cl_assert_equal_b(false, git_fs_path_is_valid("asdf\001foo", GIT_FS_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_fs_path_is_valid("asdf\037bar", GIT_FS_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_fs_path_is_valid("asdf<bar", GIT_FS_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_fs_path_is_valid("asdf>foo", GIT_FS_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_fs_path_is_valid("asdf:foo", GIT_FS_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_fs_path_is_valid("asdf\"bar", GIT_FS_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_fs_path_is_valid("asdf|foo", GIT_FS_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_fs_path_is_valid("asdf?bar", GIT_FS_PATH_REJECT_NT_CHARS));
	cl_assert_equal_b(false, git_fs_path_is_valid("asdf*bar", GIT_FS_PATH_REJECT_NT_CHARS));
}

static void test_join_unrooted(
	const char *expected_result,
	ssize_t expected_rootlen,
	const char *path,
	const char *base)
{
	git_str result = GIT_STR_INIT;
	ssize_t root_at;

	cl_git_pass(git_fs_path_join_unrooted(&result, path, base, &root_at));
	cl_assert_equal_s(expected_result, result.ptr);
	cl_assert_equal_i(expected_rootlen, root_at);

	git_str_dispose(&result);
}

void test_path_core__join_unrooted(void)
{
	git_str out = GIT_STR_INIT;

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

#ifdef GIT_WIN32
	/* Paths starting with '\\' are absolute */
	test_join_unrooted("\\bar", 0, "\\bar", "c:/foo/");
	test_join_unrooted("\\\\network\\bar", 9, "\\\\network\\bar", "c:/foo/");
#else
	/* Paths starting with '\\' are not absolute on non-Windows systems */
	test_join_unrooted("/foo/\\bar", 4, "\\bar", "/foo");
	test_join_unrooted("c:/foo/\\bar", 7, "\\bar", "c:/foo/");
#endif

	/* Base is returned when it's provided and is the prefix */
	test_join_unrooted("c:/foo/bar/foobar", 6, "c:/foo/bar/foobar", "c:/foo");
	test_join_unrooted("c:/foo/bar/foobar", 10, "c:/foo/bar/foobar", "c:/foo/bar");

	/* Trailing slash in the base is ignored */
	test_join_unrooted("c:/foo/bar/foobar", 6, "c:/foo/bar/foobar", "c:/foo/");

	git_str_dispose(&out);
}

void test_path_core__join_unrooted_respects_funny_windows_roots(void)
{
	test_join_unrooted("💩:/foo/bar/foobar", 9, "bar/foobar", "💩:/foo");
	test_join_unrooted("💩:/foo/bar/foobar", 13, "foobar", "💩:/foo/bar");
	test_join_unrooted("💩:/foo", 5, "💩:/foo", "💩:/asdf");
	test_join_unrooted("💩:/foo/bar", 5, "💩:/foo/bar", "💩:/asdf");
	test_join_unrooted("💩:/foo/bar/foobar", 9, "💩:/foo/bar/foobar", "💩:/foo");
	test_join_unrooted("💩:/foo/bar/foobar", 13, "💩:/foo/bar/foobar", "💩:/foo/bar");
	test_join_unrooted("💩:/foo/bar/foobar", 9, "💩:/foo/bar/foobar", "💩:/foo/");
}
