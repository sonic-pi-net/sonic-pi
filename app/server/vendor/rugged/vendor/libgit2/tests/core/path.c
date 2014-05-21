#include "clar_libgit2.h"
#include "fileops.h"

static void
check_dirname(const char *A, const char *B)
{
	git_buf dir = GIT_BUF_INIT;
	char *dir2;

	cl_assert(git_path_dirname_r(&dir, A) >= 0);
	cl_assert_equal_s(B, dir.ptr);
	git_buf_free(&dir);

	cl_assert((dir2 = git_path_dirname(A)) != NULL);
	cl_assert_equal_s(B, dir2);
	git__free(dir2);
}

static void
check_basename(const char *A, const char *B)
{
	git_buf base = GIT_BUF_INIT;
	char *base2;

	cl_assert(git_path_basename_r(&base, A) >= 0);
	cl_assert_equal_s(B, base.ptr);
	git_buf_free(&base);

	cl_assert((base2 = git_path_basename(A)) != NULL);
	cl_assert_equal_s(B, base2);
	git__free(base2);
}

static void
check_topdir(const char *A, const char *B)
{
	const char *dir;

	cl_assert((dir = git_path_topdir(A)) != NULL);
	cl_assert_equal_s(B, dir);
}

static void
check_joinpath(const char *path_a, const char *path_b, const char *expected_path)
{
	git_buf joined_path = GIT_BUF_INIT;

	cl_git_pass(git_buf_joinpath(&joined_path, path_a, path_b));
	cl_assert_equal_s(expected_path, joined_path.ptr);

	git_buf_free(&joined_path);
}

static void
check_joinpath_n(
	const char *path_a,
	const char *path_b,
	const char *path_c,
	const char *path_d,
	const char *expected_path)
{
	git_buf joined_path = GIT_BUF_INIT;

	cl_git_pass(git_buf_join_n(&joined_path, '/', 4,
							   path_a, path_b, path_c, path_d));
	cl_assert_equal_s(expected_path, joined_path.ptr);

	git_buf_free(&joined_path);
}


/* get the dirname of a path */
void test_core_path__00_dirname(void)
{
	check_dirname(NULL, ".");
	check_dirname("", ".");
	check_dirname("a", ".");
	check_dirname("/", "/");
	check_dirname("/usr", "/");
	check_dirname("/usr/", "/");
	check_dirname("/usr/lib", "/usr");
	check_dirname("/usr/lib/", "/usr");
	check_dirname("/usr/lib//", "/usr");
	check_dirname("usr/lib", "usr");
	check_dirname("usr/lib/", "usr");
	check_dirname("usr/lib//", "usr");
	check_dirname(".git/", ".");

	check_dirname(REP16("/abc"), REP15("/abc"));

#ifdef GIT_WIN32
	check_dirname("C:/path/", "C:/");
	check_dirname("C:/path", "C:/");
	check_dirname("//computername/path/", "//computername/");
	check_dirname("//computername/path", "//computername/");
	check_dirname("//computername/sub/path/", "//computername/sub");
	check_dirname("//computername/sub/path", "//computername/sub");
#endif
}

/* get the base name of a path */
void test_core_path__01_basename(void)
{
	check_basename(NULL, ".");
	check_basename("", ".");
	check_basename("a", "a");
	check_basename("/", "/");
	check_basename("/usr", "usr");
	check_basename("/usr/", "usr");
	check_basename("/usr/lib", "lib");
	check_basename("/usr/lib//", "lib");
	check_basename("usr/lib", "lib");

	check_basename(REP16("/abc"), "abc");
	check_basename(REP1024("/abc"), "abc");
}

/* get the latest component in a path */
void test_core_path__02_topdir(void)
{
	check_topdir(".git/", ".git/");
	check_topdir("/.git/", ".git/");
	check_topdir("usr/local/.git/", ".git/");
	check_topdir("./.git/", ".git/");
	check_topdir("/usr/.git/", ".git/");
	check_topdir("/", "/");
	check_topdir("a/", "a/");

	cl_assert(git_path_topdir("/usr/.git") == NULL);
	cl_assert(git_path_topdir(".") == NULL);
	cl_assert(git_path_topdir("") == NULL);
	cl_assert(git_path_topdir("a") == NULL);
}

/* properly join path components */
void test_core_path__05_joins(void)
{
	check_joinpath("", "", "");
	check_joinpath("", "a", "a");
	check_joinpath("", "/a", "/a");
	check_joinpath("a", "", "a/");
	check_joinpath("a", "/", "a/");
	check_joinpath("a", "b", "a/b");
	check_joinpath("/", "a", "/a");
	check_joinpath("/", "", "/");
	check_joinpath("/a", "/b", "/a/b");
	check_joinpath("/a", "/b/", "/a/b/");
	check_joinpath("/a/", "b/", "/a/b/");
	check_joinpath("/a/", "/b/", "/a/b/");

	check_joinpath("/abcd", "/defg", "/abcd/defg");
	check_joinpath("/abcd", "/defg/", "/abcd/defg/");
	check_joinpath("/abcd/", "defg/", "/abcd/defg/");
	check_joinpath("/abcd/", "/defg/", "/abcd/defg/");

	check_joinpath("/abcdefgh", "/12345678", "/abcdefgh/12345678");
	check_joinpath("/abcdefgh", "/12345678/", "/abcdefgh/12345678/");
	check_joinpath("/abcdefgh/", "12345678/", "/abcdefgh/12345678/");

	check_joinpath(REP1024("aaaa"), "", REP1024("aaaa") "/");
	check_joinpath(REP1024("aaaa/"), "", REP1024("aaaa/"));
	check_joinpath(REP1024("/aaaa"), "", REP1024("/aaaa") "/");

	check_joinpath(REP1024("aaaa"), REP1024("bbbb"),
				   REP1024("aaaa") "/" REP1024("bbbb"));
	check_joinpath(REP1024("/aaaa"), REP1024("/bbbb"),
				   REP1024("/aaaa") REP1024("/bbbb"));
}

/* properly join path components for more than one path */
void test_core_path__06_long_joins(void)
{
	check_joinpath_n("", "", "", "", "");
	check_joinpath_n("", "a", "", "", "a/");
	check_joinpath_n("a", "", "", "", "a/");
	check_joinpath_n("", "", "", "a", "a");
	check_joinpath_n("a", "b", "", "/c/d/", "a/b/c/d/");
	check_joinpath_n("a", "b", "", "/c/d", "a/b/c/d");
	check_joinpath_n("abcd", "efgh", "ijkl", "mnop", "abcd/efgh/ijkl/mnop");
	check_joinpath_n("abcd/", "efgh/", "ijkl/", "mnop/", "abcd/efgh/ijkl/mnop/");
	check_joinpath_n("/abcd/", "/efgh/", "/ijkl/", "/mnop/", "/abcd/efgh/ijkl/mnop/");

	check_joinpath_n(REP1024("a"), REP1024("b"), REP1024("c"), REP1024("d"),
					 REP1024("a") "/" REP1024("b") "/"
					 REP1024("c") "/" REP1024("d"));
	check_joinpath_n(REP1024("/a"), REP1024("/b"), REP1024("/c"), REP1024("/d"),
					 REP1024("/a") REP1024("/b")
					 REP1024("/c") REP1024("/d"));
}


static void
check_path_to_dir(
	const char* path,
    const char* expected)
{
	git_buf tgt = GIT_BUF_INIT;

	git_buf_sets(&tgt, path);
	cl_git_pass(git_path_to_dir(&tgt));
	cl_assert_equal_s(expected, tgt.ptr);

	git_buf_free(&tgt);
}

static void
check_string_to_dir(
	const char* path,
	size_t      maxlen,
    const char* expected)
{
	size_t len = strlen(path);
	char *buf = git__malloc(len + 2);
	cl_assert(buf);

	strncpy(buf, path, len + 2);

	git_path_string_to_dir(buf, maxlen);

	cl_assert_equal_s(expected, buf);

	git__free(buf);
}

/* convert paths to dirs */
void test_core_path__07_path_to_dir(void)
{
	check_path_to_dir("", "");
	check_path_to_dir(".", "./");
	check_path_to_dir("./", "./");
	check_path_to_dir("a/", "a/");
	check_path_to_dir("ab", "ab/");
	/* make sure we try just under and just over an expansion that will
	 * require a realloc
	 */
	check_path_to_dir("abcdef", "abcdef/");
	check_path_to_dir("abcdefg", "abcdefg/");
	check_path_to_dir("abcdefgh", "abcdefgh/");
	check_path_to_dir("abcdefghi", "abcdefghi/");
	check_path_to_dir(REP1024("abcd") "/", REP1024("abcd") "/");
	check_path_to_dir(REP1024("abcd"), REP1024("abcd") "/");

	check_string_to_dir("", 1, "");
	check_string_to_dir(".", 1, ".");
	check_string_to_dir(".", 2, "./");
	check_string_to_dir(".", 3, "./");
	check_string_to_dir("abcd", 3, "abcd");
	check_string_to_dir("abcd", 4, "abcd");
	check_string_to_dir("abcd", 5, "abcd/");
	check_string_to_dir("abcd", 6, "abcd/");
}

/* join path to itself */
void test_core_path__08_self_join(void)
{
	git_buf path = GIT_BUF_INIT;
	size_t asize = 0;

	asize = path.asize;
	cl_git_pass(git_buf_sets(&path, "/foo"));
	cl_assert_equal_s(path.ptr, "/foo");
	cl_assert(asize < path.asize);

	asize = path.asize;
	cl_git_pass(git_buf_joinpath(&path, path.ptr, "this is a new string"));
	cl_assert_equal_s(path.ptr, "/foo/this is a new string");
	cl_assert(asize < path.asize);

	asize = path.asize;
	cl_git_pass(git_buf_joinpath(&path, path.ptr, "/grow the buffer, grow the buffer, grow the buffer"));
	cl_assert_equal_s(path.ptr, "/foo/this is a new string/grow the buffer, grow the buffer, grow the buffer");
	cl_assert(asize < path.asize);

	git_buf_free(&path);
	cl_git_pass(git_buf_sets(&path, "/foo/bar"));

	cl_git_pass(git_buf_joinpath(&path, path.ptr + 4, "baz"));
	cl_assert_equal_s(path.ptr, "/bar/baz");

	asize = path.asize;
	cl_git_pass(git_buf_joinpath(&path, path.ptr + 4, "somethinglongenoughtorealloc"));
	cl_assert_equal_s(path.ptr, "/baz/somethinglongenoughtorealloc");
	cl_assert(asize < path.asize);
	
	git_buf_free(&path);
}

static void check_percent_decoding(const char *expected_result, const char *input)
{
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git__percent_decode(&buf, input));
	cl_assert_equal_s(expected_result, git_buf_cstr(&buf));

	git_buf_free(&buf);
}

void test_core_path__09_percent_decode(void)
{
	check_percent_decoding("abcd", "abcd");
	check_percent_decoding("a2%", "a2%");
	check_percent_decoding("a2%3", "a2%3");
	check_percent_decoding("a2%%3", "a2%%3");
	check_percent_decoding("a2%3z", "a2%3z");
	check_percent_decoding("a,", "a%2c");
	check_percent_decoding("a21", "a2%31");
	check_percent_decoding("a2%1", "a2%%31");
	check_percent_decoding("a bc ", "a%20bc%20");
	check_percent_decoding("Vicent Mart" "\355", "Vicent%20Mart%ED");
}

static void check_fromurl(const char *expected_result, const char *input, int should_fail)
{
	git_buf buf = GIT_BUF_INIT;

	assert(should_fail || expected_result);

	if (!should_fail) {
		cl_git_pass(git_path_fromurl(&buf, input));
		cl_assert_equal_s(expected_result, git_buf_cstr(&buf));
	} else
		cl_git_fail(git_path_fromurl(&buf, input));

	git_buf_free(&buf);
}

#ifdef GIT_WIN32
#define ABS_PATH_MARKER ""
#else
#define ABS_PATH_MARKER "/"
#endif

void test_core_path__10_fromurl(void)
{
	/* Failing cases */
	check_fromurl(NULL, "a", 1);
	check_fromurl(NULL, "http:///c:/Temp%20folder/note.txt", 1);
	check_fromurl(NULL, "file://c:/Temp%20folder/note.txt", 1);
	check_fromurl(NULL, "file:////c:/Temp%20folder/note.txt", 1);
	check_fromurl(NULL, "file:///", 1);
	check_fromurl(NULL, "file:////", 1);
	check_fromurl(NULL, "file://servername/c:/Temp%20folder/note.txt", 1);

	/* Passing cases */
	check_fromurl(ABS_PATH_MARKER "c:/Temp folder/note.txt", "file:///c:/Temp%20folder/note.txt", 0);
	check_fromurl(ABS_PATH_MARKER "c:/Temp folder/note.txt", "file://localhost/c:/Temp%20folder/note.txt", 0);
	check_fromurl(ABS_PATH_MARKER "c:/Temp+folder/note.txt", "file:///c:/Temp+folder/note.txt", 0);
	check_fromurl(ABS_PATH_MARKER "a", "file:///a", 0);
}

typedef struct {
	int expect_idx;
	int cancel_after;
	char **expect;
} check_walkup_info;

#define CANCEL_VALUE 1234

static int check_one_walkup_step(void *ref, git_buf *path)
{
	check_walkup_info *info = (check_walkup_info *)ref;

	if (!info->cancel_after) {
		cl_assert_equal_s(info->expect[info->expect_idx], "[CANCEL]");
		return CANCEL_VALUE;
	}
	info->cancel_after--;

	cl_assert(info->expect[info->expect_idx] != NULL);
	cl_assert_equal_s(info->expect[info->expect_idx], path->ptr);
	info->expect_idx++;

	return 0;
}

void test_core_path__11_walkup(void)
{
	git_buf p = GIT_BUF_INIT;
	char *expect[] = {
		"/a/b/c/d/e/", "/a/b/c/d/", "/a/b/c/", "/a/b/", "/a/", "/", NULL,
		"/a/b/c/d/e", "/a/b/c/d/", "/a/b/c/", "/a/b/", "/a/", "/", NULL,
		"/a/b/c/d/e", "/a/b/c/d/", "/a/b/c/", "/a/b/", "/a/", "/", NULL,
		"/a/b/c/d/e", "/a/b/c/d/", "/a/b/c/", "/a/b/", "/a/", "/", NULL,
		"/a/b/c/d/e", "/a/b/c/d/", "/a/b/c/", "/a/b/", NULL,
		"/a/b/c/d/e", "/a/b/c/d/", "/a/b/c/", "/a/b/", NULL,
		"this is a path", NULL,
		"///a///b///c///d///e///", "///a///b///c///d///", "///a///b///c///", "///a///b///", "///a///", "///", NULL,
		NULL
	};
	char *root[] = { NULL, NULL, "/", "", "/a/b", "/a/b/", NULL, NULL, NULL };
	int i, j;
	check_walkup_info info;

	info.expect = expect;
	info.cancel_after = -1;

	for (i = 0, j = 0; expect[i] != NULL; i++, j++) {

		git_buf_sets(&p, expect[i]);

		info.expect_idx = i;
		cl_git_pass(
			git_path_walk_up(&p, root[j], check_one_walkup_step, &info)
		);

		cl_assert_equal_s(p.ptr, expect[i]);

		/* skip to next run of expectations */
		while (expect[i] != NULL) i++;
	}

	git_buf_free(&p);
}

void test_core_path__11a_walkup_cancel(void)
{
	git_buf p = GIT_BUF_INIT;
	int cancel[] = { 3, 2, 1, 0 };
	char *expect[] = {
		"/a/b/c/d/e/", "/a/b/c/d/", "/a/b/c/", "[CANCEL]", NULL,
		"/a/b/c/d/e", "/a/b/c/d/", "[CANCEL]", NULL,
		"/a/b/c/d/e", "[CANCEL]", NULL,
		"[CANCEL]", NULL,
		NULL
	};
	char *root[] = { NULL, NULL, "/", "", NULL };
	int i, j;
	check_walkup_info info;

	info.expect = expect;

	for (i = 0, j = 0; expect[i] != NULL; i++, j++) {

		git_buf_sets(&p, expect[i]);

		info.cancel_after = cancel[j];
		info.expect_idx = i;

		cl_assert_equal_i(
			CANCEL_VALUE,
			git_path_walk_up(&p, root[j], check_one_walkup_step, &info)
		);

		/* skip to next run of expectations */
		while (expect[i] != NULL) i++;
	}

	git_buf_free(&p);
}

void test_core_path__12_offset_to_path_root(void)
{
	cl_assert(git_path_root("non/rooted/path") == -1);
	cl_assert(git_path_root("/rooted/path") == 0);

#ifdef GIT_WIN32
	/* Windows specific tests */
	cl_assert(git_path_root("C:non/rooted/path") == -1);
	cl_assert(git_path_root("C:/rooted/path") == 2);
	cl_assert(git_path_root("//computername/sharefolder/resource") == 14);
	cl_assert(git_path_root("//computername/sharefolder") == 14);
	cl_assert(git_path_root("//computername") == -1);
#endif
}

#define NON_EXISTING_FILEPATH "i_hope_i_do_not_exist"

void test_core_path__13_cannot_prettify_a_non_existing_file(void)
{
	git_buf p = GIT_BUF_INIT;

	cl_must_pass(git_path_exists(NON_EXISTING_FILEPATH) == false);
	cl_assert_equal_i(GIT_ENOTFOUND, git_path_prettify(&p, NON_EXISTING_FILEPATH, NULL));
	cl_assert_equal_i(GIT_ENOTFOUND, git_path_prettify(&p, NON_EXISTING_FILEPATH "/so-do-i", NULL));

	git_buf_free(&p);
}

void test_core_path__14_apply_relative(void)
{
	git_buf p = GIT_BUF_INIT;

	cl_git_pass(git_buf_sets(&p, "/this/is/a/base"));

	cl_git_pass(git_path_apply_relative(&p, "../test"));
	cl_assert_equal_s("/this/is/a/test", p.ptr);

	cl_git_pass(git_path_apply_relative(&p, "../../the/./end"));
	cl_assert_equal_s("/this/is/the/end", p.ptr);

	cl_git_pass(git_path_apply_relative(&p, "./of/this/../the/string"));
	cl_assert_equal_s("/this/is/the/end/of/the/string", p.ptr);

	cl_git_pass(git_path_apply_relative(&p, "../../../../../.."));
	cl_assert_equal_s("/this/", p.ptr);

	cl_git_pass(git_path_apply_relative(&p, "../"));
	cl_assert_equal_s("/", p.ptr);

	cl_git_fail(git_path_apply_relative(&p, "../../.."));


	cl_git_pass(git_buf_sets(&p, "d:/another/test"));

	cl_git_pass(git_path_apply_relative(&p, "../.."));
	cl_assert_equal_s("d:/", p.ptr);

	cl_git_pass(git_path_apply_relative(&p, "from/here/to/../and/./back/."));
	cl_assert_equal_s("d:/from/here/and/back/", p.ptr);


	cl_git_pass(git_buf_sets(&p, "https://my.url.com/test.git"));

	cl_git_pass(git_path_apply_relative(&p, "../another.git"));
	cl_assert_equal_s("https://my.url.com/another.git", p.ptr);

	cl_git_pass(git_path_apply_relative(&p, "../full/path/url.patch"));
	cl_assert_equal_s("https://my.url.com/full/path/url.patch", p.ptr);

	cl_git_pass(git_path_apply_relative(&p, ".."));
	cl_assert_equal_s("https://my.url.com/full/path/", p.ptr);

	cl_git_pass(git_path_apply_relative(&p, "../../../"));
	cl_assert_equal_s("https://", p.ptr);


	cl_git_pass(git_buf_sets(&p, "../../this/is/relative"));

	cl_git_pass(git_path_apply_relative(&p, "../../preserves/the/prefix"));
	cl_assert_equal_s("../../this/preserves/the/prefix", p.ptr);

	cl_git_pass(git_path_apply_relative(&p, "../../../../that"));
	cl_assert_equal_s("../../that", p.ptr);

	cl_git_pass(git_path_apply_relative(&p, "../there"));
	cl_assert_equal_s("../../there", p.ptr);
	git_buf_free(&p);
}

static void assert_resolve_relative(
	git_buf *buf, const char *expected, const char *path)
{
	cl_git_pass(git_buf_sets(buf, path));
	cl_git_pass(git_path_resolve_relative(buf, 0));
	cl_assert_equal_s(expected, buf->ptr);
}

void test_core_path__15_resolve_relative(void)
{
	git_buf buf = GIT_BUF_INIT;

	assert_resolve_relative(&buf, "", "");
	assert_resolve_relative(&buf, "", ".");
	assert_resolve_relative(&buf, "", "./");
	assert_resolve_relative(&buf, "..", "..");
	assert_resolve_relative(&buf, "../", "../");
	assert_resolve_relative(&buf, "..", "./..");
	assert_resolve_relative(&buf, "../", "./../");
	assert_resolve_relative(&buf, "../", "../.");
	assert_resolve_relative(&buf, "../", ".././");
	assert_resolve_relative(&buf, "../..", "../..");
	assert_resolve_relative(&buf, "../../", "../../");

	assert_resolve_relative(&buf, "/", "/");
	assert_resolve_relative(&buf, "/", "/.");

	assert_resolve_relative(&buf, "", "a/..");
	assert_resolve_relative(&buf, "", "a/../");
	assert_resolve_relative(&buf, "", "a/../.");

	assert_resolve_relative(&buf, "/a", "/a");
	assert_resolve_relative(&buf, "/a/", "/a/.");
	assert_resolve_relative(&buf, "/", "/a/../");
	assert_resolve_relative(&buf, "/", "/a/../.");
	assert_resolve_relative(&buf, "/", "/a/.././");

	assert_resolve_relative(&buf, "a", "a");
	assert_resolve_relative(&buf, "a/", "a/");
	assert_resolve_relative(&buf, "a/", "a/.");
	assert_resolve_relative(&buf, "a/", "a/./");

	assert_resolve_relative(&buf, "a/b", "a//b");
	assert_resolve_relative(&buf, "a/b/c", "a/b/c");
	assert_resolve_relative(&buf, "b/c", "./b/c");
	assert_resolve_relative(&buf, "a/c", "a/./c");
	assert_resolve_relative(&buf, "a/b/", "a/b/.");

	assert_resolve_relative(&buf, "/a/b/c", "///a/b/c");
	assert_resolve_relative(&buf, "/", "////");
	assert_resolve_relative(&buf, "/a", "///a");
	assert_resolve_relative(&buf, "/", "///.");
	assert_resolve_relative(&buf, "/", "///a/..");

	assert_resolve_relative(&buf, "../../path", "../../test//../././path");
	assert_resolve_relative(&buf, "../d", "a/b/../../../c/../d");

	cl_git_pass(git_buf_sets(&buf, "/.."));
	cl_git_fail(git_path_resolve_relative(&buf, 0));

	cl_git_pass(git_buf_sets(&buf, "/./.."));
	cl_git_fail(git_path_resolve_relative(&buf, 0));

	cl_git_pass(git_buf_sets(&buf, "/.//.."));
	cl_git_fail(git_path_resolve_relative(&buf, 0));

	cl_git_pass(git_buf_sets(&buf, "/../."));
	cl_git_fail(git_path_resolve_relative(&buf, 0));

	cl_git_pass(git_buf_sets(&buf, "/../.././../a"));
	cl_git_fail(git_path_resolve_relative(&buf, 0));

	cl_git_pass(git_buf_sets(&buf, "////.."));
	cl_git_fail(git_path_resolve_relative(&buf, 0));

	/* things that start with Windows network paths */
#ifdef GIT_WIN32
	assert_resolve_relative(&buf, "//a/b/c", "//a/b/c");
	assert_resolve_relative(&buf, "//a/", "//a/b/..");
	assert_resolve_relative(&buf, "//a/b/c", "//a/Q/../b/x/y/../../c");

	cl_git_pass(git_buf_sets(&buf, "//a/b/../.."));
	cl_git_fail(git_path_resolve_relative(&buf, 0));
#else
	assert_resolve_relative(&buf, "/a/b/c", "//a/b/c");
	assert_resolve_relative(&buf, "/a/", "//a/b/..");
	assert_resolve_relative(&buf, "/a/b/c", "//a/Q/../b/x/y/../../c");
	assert_resolve_relative(&buf, "/", "//a/b/../..");
#endif

	git_buf_free(&buf);
}
