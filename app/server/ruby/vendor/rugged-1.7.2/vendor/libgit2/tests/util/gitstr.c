#include "clar_libgit2.h"
#include "futils.h"

#define TESTSTR "Have you seen that? Have you seeeen that??"
const char *test_string = TESTSTR;
const char *test_string_x2 = TESTSTR TESTSTR;

#define TESTSTR_4096 REP1024("1234")
#define TESTSTR_8192 REP1024("12341234")
const char *test_4096 = TESTSTR_4096;
const char *test_8192 = TESTSTR_8192;

/* test basic data concatenation */
void test_gitstr__0(void)
{
	git_str buf = GIT_STR_INIT;

	cl_assert(buf.size == 0);

	git_str_puts(&buf, test_string);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(test_string, git_str_cstr(&buf));

	git_str_puts(&buf, test_string);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(test_string_x2, git_str_cstr(&buf));

	git_str_dispose(&buf);
}

/* test git_str_printf */
void test_gitstr__1(void)
{
	git_str buf = GIT_STR_INIT;

	git_str_printf(&buf, "%s %s %d ", "shoop", "da", 23);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s("shoop da 23 ", git_str_cstr(&buf));

	git_str_printf(&buf, "%s %d", "woop", 42);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s("shoop da 23 woop 42", git_str_cstr(&buf));

	git_str_dispose(&buf);
}

/* more thorough test of concatenation options */
void test_gitstr__2(void)
{
	git_str buf = GIT_STR_INIT;
	int i;
	char data[128];

	cl_assert(buf.size == 0);

	/* this must be safe to do */
	git_str_dispose(&buf);
	cl_assert(buf.size == 0);
	cl_assert(buf.asize == 0);

	/* empty buffer should be empty string */
	cl_assert_equal_s("", git_str_cstr(&buf));
	cl_assert(buf.size == 0);
	/* cl_assert(buf.asize == 0); -- should not assume what git_str does */

	/* free should set us back to the beginning */
	git_str_dispose(&buf);
	cl_assert(buf.size == 0);
	cl_assert(buf.asize == 0);

	/* add letter */
	git_str_putc(&buf, '+');
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s("+", git_str_cstr(&buf));

	/* add letter again */
	git_str_putc(&buf, '+');
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s("++", git_str_cstr(&buf));

	/* let's try that a few times */
	for (i = 0; i < 16; ++i) {
		git_str_putc(&buf, '+');
		cl_assert(git_str_oom(&buf) == 0);
	}
	cl_assert_equal_s("++++++++++++++++++", git_str_cstr(&buf));

	git_str_dispose(&buf);

	/* add data */
	git_str_put(&buf, "xo", 2);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s("xo", git_str_cstr(&buf));

	/* add letter again */
	git_str_put(&buf, "xo", 2);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s("xoxo", git_str_cstr(&buf));

	/* let's try that a few times */
	for (i = 0; i < 16; ++i) {
		git_str_put(&buf, "xo", 2);
		cl_assert(git_str_oom(&buf) == 0);
	}
	cl_assert_equal_s("xoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxoxo",
					   git_str_cstr(&buf));

	git_str_dispose(&buf);

	/* set to string */
	git_str_sets(&buf, test_string);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(test_string, git_str_cstr(&buf));

	/* append string */
	git_str_puts(&buf, test_string);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(test_string_x2, git_str_cstr(&buf));

	/* set to string again (should overwrite - not append) */
	git_str_sets(&buf, test_string);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(test_string, git_str_cstr(&buf));

	/* test clear */
	git_str_clear(&buf);
	cl_assert_equal_s("", git_str_cstr(&buf));

	git_str_dispose(&buf);

	/* test extracting data into buffer */
	git_str_puts(&buf, REP4("0123456789"));
	cl_assert(git_str_oom(&buf) == 0);

	git_str_copy_cstr(data, sizeof(data), &buf);
	cl_assert_equal_s(REP4("0123456789"), data);
	git_str_copy_cstr(data, 11, &buf);
	cl_assert_equal_s("0123456789", data);
	git_str_copy_cstr(data, 3, &buf);
	cl_assert_equal_s("01", data);
	git_str_copy_cstr(data, 1, &buf);
	cl_assert_equal_s("", data);

	git_str_copy_cstr(data, sizeof(data), &buf);
	cl_assert_equal_s(REP4("0123456789"), data);

	git_str_sets(&buf, REP256("x"));
	git_str_copy_cstr(data, sizeof(data), &buf);
	/* since sizeof(data) == 128, only 127 bytes should be copied */
	cl_assert_equal_s(REP4(REP16("x")) REP16("x") REP16("x")
					   REP16("x") "xxxxxxxxxxxxxxx", data);

	git_str_dispose(&buf);

	git_str_copy_cstr(data, sizeof(data), &buf);
	cl_assert_equal_s("", data);
}

/* let's do some tests with larger buffers to push our limits */
void test_gitstr__3(void)
{
	git_str buf = GIT_STR_INIT;

	/* set to string */
	git_str_set(&buf, test_4096, 4096);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(test_4096, git_str_cstr(&buf));

	/* append string */
	git_str_puts(&buf, test_4096);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(test_8192, git_str_cstr(&buf));

	/* set to string again (should overwrite - not append) */
	git_str_set(&buf, test_4096, 4096);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(test_4096, git_str_cstr(&buf));

	git_str_dispose(&buf);
}

/* let's try some producer/consumer tests */
void test_gitstr__4(void)
{
	git_str buf = GIT_STR_INIT;
	int i;

	for (i = 0; i < 10; ++i) {
		git_str_puts(&buf, "1234"); /* add 4 */
		cl_assert(git_str_oom(&buf) == 0);
		git_str_consume(&buf, buf.ptr + 2); /* eat the first two */
		cl_assert(strlen(git_str_cstr(&buf)) == (size_t)((i + 1) * 2));
	}
	/* we have appended 1234 10x and removed the first 20 letters */
	cl_assert_equal_s("12341234123412341234", git_str_cstr(&buf));

	git_str_consume(&buf, NULL);
	cl_assert_equal_s("12341234123412341234", git_str_cstr(&buf));

	git_str_consume(&buf, "invalid pointer");
	cl_assert_equal_s("12341234123412341234", git_str_cstr(&buf));

	git_str_consume(&buf, buf.ptr);
	cl_assert_equal_s("12341234123412341234", git_str_cstr(&buf));

	git_str_consume(&buf, buf.ptr + 1);
	cl_assert_equal_s("2341234123412341234", git_str_cstr(&buf));

	git_str_consume(&buf, buf.ptr + buf.size);
	cl_assert_equal_s("", git_str_cstr(&buf));

	git_str_dispose(&buf);
}


static void
check_buf_append(
	const char* data_a,
	const char* data_b,
	const char* expected_data,
	size_t expected_size,
	size_t expected_asize)
{
	git_str tgt = GIT_STR_INIT;

	git_str_sets(&tgt, data_a);
	cl_assert(git_str_oom(&tgt) == 0);
	git_str_puts(&tgt, data_b);
	cl_assert(git_str_oom(&tgt) == 0);
	cl_assert_equal_s(expected_data, git_str_cstr(&tgt));
	cl_assert_equal_i(tgt.size, expected_size);
	if (expected_asize > 0)
		cl_assert_equal_i(tgt.asize, expected_asize);

	git_str_dispose(&tgt);
}

static void
check_buf_append_abc(
	const char* buf_a,
	const char* buf_b,
	const char* buf_c,
	const char* expected_ab,
	const char* expected_abc,
	const char* expected_abca,
	const char* expected_abcab,
	const char* expected_abcabc)
{
	git_str buf = GIT_STR_INIT;

	git_str_sets(&buf, buf_a);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(buf_a, git_str_cstr(&buf));

	git_str_puts(&buf, buf_b);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(expected_ab, git_str_cstr(&buf));

	git_str_puts(&buf, buf_c);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(expected_abc, git_str_cstr(&buf));

	git_str_puts(&buf, buf_a);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(expected_abca, git_str_cstr(&buf));

	git_str_puts(&buf, buf_b);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(expected_abcab, git_str_cstr(&buf));

	git_str_puts(&buf, buf_c);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(expected_abcabc, git_str_cstr(&buf));

	git_str_dispose(&buf);
}

/* more variations on append tests */
void test_gitstr__5(void)
{
	check_buf_append("", "", "", 0, 0);
	check_buf_append("a", "", "a", 1, 0);
	check_buf_append("", "a", "a", 1, 8);
	check_buf_append("", "a", "a", 1, 8);
	check_buf_append("a", "b", "ab", 2, 8);
	check_buf_append("", "abcdefgh", "abcdefgh", 8, 16);
	check_buf_append("abcdefgh", "", "abcdefgh", 8, 16);

	/* buffer with starting asize will grow to:
	 *  1 ->  2,  2 ->  3,  3 ->  5,  4 ->  6,  5 ->  8,  6 ->  9,
	 *  7 -> 11,  8 -> 12,  9 -> 14, 10 -> 15, 11 -> 17, 12 -> 18,
	 * 13 -> 20, 14 -> 21, 15 -> 23, 16 -> 24, 17 -> 26, 18 -> 27,
	 * 19 -> 29, 20 -> 30, 21 -> 32, 22 -> 33, 23 -> 35, 24 -> 36,
	 * ...
	 * follow sequence until value > target size,
	 * then round up to nearest multiple of 8.
	 */

	check_buf_append("abcdefgh", "/", "abcdefgh/", 9, 16);
	check_buf_append("abcdefgh", "ijklmno", "abcdefghijklmno", 15, 16);
	check_buf_append("abcdefgh", "ijklmnop", "abcdefghijklmnop", 16, 24);
	check_buf_append("0123456789", "0123456789",
					 "01234567890123456789", 20, 24);
	check_buf_append(REP16("x"), REP16("o"),
					 REP16("x") REP16("o"), 32, 40);

	check_buf_append(test_4096, "", test_4096, 4096, 4104);
	check_buf_append(test_4096, test_4096, test_8192, 8192, 8200);

	/* check sequences of appends */
	check_buf_append_abc("a", "b", "c",
						 "ab", "abc", "abca", "abcab", "abcabc");
	check_buf_append_abc("a1", "b2", "c3",
						 "a1b2", "a1b2c3", "a1b2c3a1",
						 "a1b2c3a1b2", "a1b2c3a1b2c3");
	check_buf_append_abc("a1/", "b2/", "c3/",
						 "a1/b2/", "a1/b2/c3/", "a1/b2/c3/a1/",
						 "a1/b2/c3/a1/b2/", "a1/b2/c3/a1/b2/c3/");
}

/* test swap */
void test_gitstr__6(void)
{
	git_str a = GIT_STR_INIT;
	git_str b = GIT_STR_INIT;

	git_str_sets(&a, "foo");
	cl_assert(git_str_oom(&a) == 0);
	git_str_sets(&b, "bar");
	cl_assert(git_str_oom(&b) == 0);

	cl_assert_equal_s("foo", git_str_cstr(&a));
	cl_assert_equal_s("bar", git_str_cstr(&b));

	git_str_swap(&a, &b);

	cl_assert_equal_s("bar", git_str_cstr(&a));
	cl_assert_equal_s("foo", git_str_cstr(&b));

	git_str_dispose(&a);
	git_str_dispose(&b);
}


/* test detach/attach data */
void test_gitstr__7(void)
{
	const char *fun = "This is fun";
	git_str a = GIT_STR_INIT;
	char *b = NULL;

	git_str_sets(&a, "foo");
	cl_assert(git_str_oom(&a) == 0);
	cl_assert_equal_s("foo", git_str_cstr(&a));

	b = git_str_detach(&a);

	cl_assert_equal_s("foo", b);
	cl_assert_equal_s("", a.ptr);
	git__free(b);

	b = git_str_detach(&a);

	cl_assert_equal_s(NULL, b);
	cl_assert_equal_s("", a.ptr);

	git_str_dispose(&a);

	b = git__strdup(fun);
	git_str_attach(&a, b, 0);

	cl_assert_equal_s(fun, a.ptr);
	cl_assert(a.size == strlen(fun));
	cl_assert(a.asize == strlen(fun) + 1);

	git_str_dispose(&a);

	b = git__strdup(fun);
	git_str_attach(&a, b, strlen(fun) + 1);

	cl_assert_equal_s(fun, a.ptr);
	cl_assert(a.size == strlen(fun));
	cl_assert(a.asize == strlen(fun) + 1);

	git_str_dispose(&a);
}


static void
check_joinbuf_2(
	const char *a,
	const char *b,
	const char *expected)
{
	char sep = '/';
	git_str buf = GIT_STR_INIT;

	git_str_join(&buf, sep, a, b);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(expected, git_str_cstr(&buf));
	git_str_dispose(&buf);
}

static void
check_joinbuf_overlapped(
	const char *oldval,
	int ofs_a,
	const char *b,
	const char *expected)
{
	char sep = '/';
	git_str buf = GIT_STR_INIT;

	git_str_sets(&buf, oldval);
	git_str_join(&buf, sep, buf.ptr + ofs_a, b);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(expected, git_str_cstr(&buf));
	git_str_dispose(&buf);
}

static void
check_joinbuf_n_2(
	const char *a,
	const char *b,
	const char *expected)
{
	char sep = '/';
	git_str buf = GIT_STR_INIT;

	git_str_sets(&buf, a);
	cl_assert(git_str_oom(&buf) == 0);

	git_str_join_n(&buf, sep, 1, b);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(expected, git_str_cstr(&buf));

	git_str_dispose(&buf);
}

static void
check_joinbuf_n_4(
	const char *a,
	const char *b,
	const char *c,
	const char *d,
	const char *expected)
{
	char sep = ';';
	git_str buf = GIT_STR_INIT;
	git_str_join_n(&buf, sep, 4, a, b, c, d);
	cl_assert(git_str_oom(&buf) == 0);
	cl_assert_equal_s(expected, git_str_cstr(&buf));
	git_str_dispose(&buf);
}

/* test join */
void test_gitstr__8(void)
{
	git_str a = GIT_STR_INIT;

	git_str_join_n(&a, '/', 1, "foo");
	cl_assert(git_str_oom(&a) == 0);
	cl_assert_equal_s("foo", git_str_cstr(&a));

	git_str_join_n(&a, '/', 1, "bar");
	cl_assert(git_str_oom(&a) == 0);
	cl_assert_equal_s("foo/bar", git_str_cstr(&a));

	git_str_join_n(&a, '/', 1, "baz");
	cl_assert(git_str_oom(&a) == 0);
	cl_assert_equal_s("foo/bar/baz", git_str_cstr(&a));

	git_str_dispose(&a);

	check_joinbuf_2(NULL, "", "");
	check_joinbuf_2(NULL, "a", "a");
	check_joinbuf_2(NULL, "/a", "/a");
	check_joinbuf_2("", "", "");
	check_joinbuf_2("", "a", "a");
	check_joinbuf_2("", "/a", "/a");
	check_joinbuf_2("a", "", "a/");
	check_joinbuf_2("a", "/", "a/");
	check_joinbuf_2("a", "b", "a/b");
	check_joinbuf_2("/", "a", "/a");
	check_joinbuf_2("/", "", "/");
	check_joinbuf_2("/a", "/b", "/a/b");
	check_joinbuf_2("/a", "/b/", "/a/b/");
	check_joinbuf_2("/a/", "b/", "/a/b/");
	check_joinbuf_2("/a/", "/b/", "/a/b/");
	check_joinbuf_2("/a/", "//b/", "/a/b/");
	check_joinbuf_2("/abcd", "/defg", "/abcd/defg");
	check_joinbuf_2("/abcd", "/defg/", "/abcd/defg/");
	check_joinbuf_2("/abcd/", "defg/", "/abcd/defg/");
	check_joinbuf_2("/abcd/", "/defg/", "/abcd/defg/");

	check_joinbuf_overlapped("abcd", 0, "efg", "abcd/efg");
	check_joinbuf_overlapped("abcd", 1, "efg", "bcd/efg");
	check_joinbuf_overlapped("abcd", 2, "efg", "cd/efg");
	check_joinbuf_overlapped("abcd", 3, "efg", "d/efg");
	check_joinbuf_overlapped("abcd", 4, "efg", "efg");
	check_joinbuf_overlapped("abc/", 2, "efg", "c/efg");
	check_joinbuf_overlapped("abc/", 3, "efg", "/efg");
	check_joinbuf_overlapped("abc/", 4, "efg", "efg");
	check_joinbuf_overlapped("abcd", 3, "", "d/");
	check_joinbuf_overlapped("abcd", 4, "", "");
	check_joinbuf_overlapped("abc/", 2, "", "c/");
	check_joinbuf_overlapped("abc/", 3, "", "/");
	check_joinbuf_overlapped("abc/", 4, "", "");

	check_joinbuf_n_2("", "", "");
	check_joinbuf_n_2("", "a", "a");
	check_joinbuf_n_2("", "/a", "/a");
	check_joinbuf_n_2("a", "", "a/");
	check_joinbuf_n_2("a", "/", "a/");
	check_joinbuf_n_2("a", "b", "a/b");
	check_joinbuf_n_2("/", "a", "/a");
	check_joinbuf_n_2("/", "", "/");
	check_joinbuf_n_2("/a", "/b", "/a/b");
	check_joinbuf_n_2("/a", "/b/", "/a/b/");
	check_joinbuf_n_2("/a/", "b/", "/a/b/");
	check_joinbuf_n_2("/a/", "/b/", "/a/b/");
	check_joinbuf_n_2("/abcd", "/defg", "/abcd/defg");
	check_joinbuf_n_2("/abcd", "/defg/", "/abcd/defg/");
	check_joinbuf_n_2("/abcd/", "defg/", "/abcd/defg/");
	check_joinbuf_n_2("/abcd/", "/defg/", "/abcd/defg/");

	check_joinbuf_n_4("", "", "", "", "");
	check_joinbuf_n_4("", "a", "", "", "a;");
	check_joinbuf_n_4("a", "", "", "", "a;");
	check_joinbuf_n_4("", "", "", "a", "a");
	check_joinbuf_n_4("a", "b", "", ";c;d;", "a;b;c;d;");
	check_joinbuf_n_4("a", "b", "", ";c;d", "a;b;c;d");
	check_joinbuf_n_4("abcd", "efgh", "ijkl", "mnop", "abcd;efgh;ijkl;mnop");
	check_joinbuf_n_4("abcd;", "efgh;", "ijkl;", "mnop;", "abcd;efgh;ijkl;mnop;");
	check_joinbuf_n_4(";abcd;", ";efgh;", ";ijkl;", ";mnop;", ";abcd;efgh;ijkl;mnop;");
}

void test_gitstr__9(void)
{
	git_str buf = GIT_STR_INIT;

	/* just some exhaustive tests of various separator placement */
	char *a[] = { "", "-", "a-", "-a", "-a-" };
	char *b[] = { "", "-", "b-", "-b", "-b-" };
	char sep[] = { 0, '-', '/' };
	char *expect_null[] = { "",    "-",     "a-",     "-a",     "-a-",
							"-",   "--",    "a--",    "-a-",    "-a--",
							"b-",  "-b-",   "a-b-",   "-ab-",   "-a-b-",
							"-b",  "--b",   "a--b",   "-a-b",   "-a--b",
							"-b-", "--b-",  "a--b-",  "-a-b-",  "-a--b-" };
	char *expect_dash[] = { "",    "-",     "a-",     "-a-",    "-a-",
							"-",   "-",     "a-",     "-a-",    "-a-",
							"b-",  "-b-",   "a-b-",   "-a-b-",  "-a-b-",
							"-b",  "-b",    "a-b",    "-a-b",   "-a-b",
							"-b-", "-b-",   "a-b-",   "-a-b-",  "-a-b-" };
	char *expect_slas[] = { "",    "-/",    "a-/",    "-a/",    "-a-/",
							"-",   "-/-",   "a-/-",   "-a/-",   "-a-/-",
							"b-",  "-/b-",  "a-/b-",  "-a/b-",  "-a-/b-",
							"-b",  "-/-b",  "a-/-b",  "-a/-b",  "-a-/-b",
							"-b-", "-/-b-", "a-/-b-", "-a/-b-", "-a-/-b-" };
	char **expect_values[] = { expect_null, expect_dash, expect_slas };
	char separator, **expect;
	unsigned int s, i, j;

	for (s = 0; s < sizeof(sep) / sizeof(char); ++s) {
		separator = sep[s];
		expect = expect_values[s];

		for (j = 0; j < sizeof(b) / sizeof(char*); ++j) {
			for (i = 0; i < sizeof(a) / sizeof(char*); ++i) {
				git_str_join(&buf, separator, a[i], b[j]);
				cl_assert_equal_s(*expect, buf.ptr);
				expect++;
			}
		}
	}

	git_str_dispose(&buf);
}

void test_gitstr__10(void)
{
	git_str a = GIT_STR_INIT;

	cl_git_pass(git_str_join_n(&a, '/', 1, "test"));
	cl_assert_equal_s(a.ptr, "test");
	cl_git_pass(git_str_join_n(&a, '/', 1, "string"));
	cl_assert_equal_s(a.ptr, "test/string");
	git_str_clear(&a);
	cl_git_pass(git_str_join_n(&a, '/', 3, "test", "string", "join"));
	cl_assert_equal_s(a.ptr, "test/string/join");
	cl_git_pass(git_str_join_n(&a, '/', 2, a.ptr, "more"));
	cl_assert_equal_s(a.ptr, "test/string/join/test/string/join/more");

	git_str_dispose(&a);
}

void test_gitstr__join3(void)
{
	git_str a = GIT_STR_INIT;

	cl_git_pass(git_str_join3(&a, '/', "test", "string", "join"));
	cl_assert_equal_s("test/string/join", a.ptr);
	cl_git_pass(git_str_join3(&a, '/', "test/", "string", "join"));
	cl_assert_equal_s("test/string/join", a.ptr);
	cl_git_pass(git_str_join3(&a, '/', "test/", "/string", "join"));
	cl_assert_equal_s("test/string/join", a.ptr);
	cl_git_pass(git_str_join3(&a, '/', "test/", "/string/", "join"));
	cl_assert_equal_s("test/string/join", a.ptr);
	cl_git_pass(git_str_join3(&a, '/', "test/", "/string/", "/join"));
	cl_assert_equal_s("test/string/join", a.ptr);

	cl_git_pass(git_str_join3(&a, '/', "", "string", "join"));
	cl_assert_equal_s("string/join", a.ptr);
	cl_git_pass(git_str_join3(&a, '/', "", "string/", "join"));
	cl_assert_equal_s("string/join", a.ptr);
	cl_git_pass(git_str_join3(&a, '/', "", "string/", "/join"));
	cl_assert_equal_s("string/join", a.ptr);

	cl_git_pass(git_str_join3(&a, '/', "string", "", "join"));
	cl_assert_equal_s("string/join", a.ptr);
	cl_git_pass(git_str_join3(&a, '/', "string/", "", "join"));
	cl_assert_equal_s("string/join", a.ptr);
	cl_git_pass(git_str_join3(&a, '/', "string/", "", "/join"));
	cl_assert_equal_s("string/join", a.ptr);

	git_str_dispose(&a);
}

void test_gitstr__11(void)
{
	git_str a = GIT_STR_INIT;
	char *t1[] = { "nothing", "in", "common" };
	char *t2[] = { "something", "something else", "some other" };
	char *t3[] = { "something", "some fun", "no fun" };
	char *t4[] = { "happy", "happier", "happiest" };
	char *t5[] = { "happiest", "happier", "happy" };
	char *t6[] = { "no", "nope", "" };
	char *t7[] = { "", "doesn't matter" };

	cl_git_pass(git_str_common_prefix(&a, t1, 3));
	cl_assert_equal_s(a.ptr, "");

	cl_git_pass(git_str_common_prefix(&a, t2, 3));
	cl_assert_equal_s(a.ptr, "some");

	cl_git_pass(git_str_common_prefix(&a, t3, 3));
	cl_assert_equal_s(a.ptr, "");

	cl_git_pass(git_str_common_prefix(&a, t4, 3));
	cl_assert_equal_s(a.ptr, "happ");

	cl_git_pass(git_str_common_prefix(&a, t5, 3));
	cl_assert_equal_s(a.ptr, "happ");

	cl_git_pass(git_str_common_prefix(&a, t6, 3));
	cl_assert_equal_s(a.ptr, "");

	cl_git_pass(git_str_common_prefix(&a, t7, 3));
	cl_assert_equal_s(a.ptr, "");

	git_str_dispose(&a);
}

void test_gitstr__rfind_variants(void)
{
	git_str a = GIT_STR_INIT;
	ssize_t len;

	cl_git_pass(git_str_sets(&a, "/this/is/it/"));

	len = (ssize_t)git_str_len(&a);

	cl_assert(git_str_rfind(&a, '/') == len - 1);
	cl_assert(git_str_rfind_next(&a, '/') == len - 4);

	cl_assert(git_str_rfind(&a, 'i') == len - 3);
	cl_assert(git_str_rfind_next(&a, 'i') == len - 3);

	cl_assert(git_str_rfind(&a, 'h') == 2);
	cl_assert(git_str_rfind_next(&a, 'h') == 2);

	cl_assert(git_str_rfind(&a, 'q') == -1);
	cl_assert(git_str_rfind_next(&a, 'q') == -1);

	git_str_dispose(&a);
}

void test_gitstr__puts_escaped(void)
{
	git_str a = GIT_STR_INIT;

	git_str_clear(&a);
	cl_git_pass(git_str_puts_escaped(&a, "this is a test", "", ""));
	cl_assert_equal_s("this is a test", a.ptr);

	git_str_clear(&a);
	cl_git_pass(git_str_puts_escaped(&a, "this is a test", "t", "\\"));
	cl_assert_equal_s("\\this is a \\tes\\t", a.ptr);

	git_str_clear(&a);
	cl_git_pass(git_str_puts_escaped(&a, "this is a test", "i ", "__"));
	cl_assert_equal_s("th__is__ __is__ a__ test", a.ptr);

	git_str_clear(&a);
	cl_git_pass(git_str_puts_escape_regex(&a, "^match\\s*[A-Z]+.*"));
	cl_assert_equal_s("\\^match\\\\s\\*\\[A-Z\\]\\+\\.\\*", a.ptr);

	git_str_dispose(&a);
}

static void assert_unescape(char *expected, char *to_unescape) {
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_str_sets(&buf, to_unescape));
	git_str_unescape(&buf);
	cl_assert_equal_s(expected, buf.ptr);
	cl_assert_equal_sz(strlen(expected), buf.size);

	git_str_dispose(&buf);
}

void test_gitstr__unescape(void)
{
	assert_unescape("Escaped\\", "Es\\ca\\ped\\");
	assert_unescape("Es\\caped\\", "Es\\\\ca\\ped\\\\");
	assert_unescape("\\", "\\");
	assert_unescape("\\", "\\\\");
	assert_unescape("", "");
}

void test_gitstr__encode_base64(void)
{
	git_str buf = GIT_STR_INIT;

	/*     t  h  i  s
	 * 0x 74 68 69 73
     * 0b 01110100 01101000 01101001 01110011
	 * 0b 011101 000110 100001 101001 011100 110000
	 * 0x 1d 06 21 29 1c 30
	 *     d  G  h  p  c  w
	 */
	cl_git_pass(git_str_encode_base64(&buf, "this", 4));
	cl_assert_equal_s("dGhpcw==", buf.ptr);

	git_str_clear(&buf);
	cl_git_pass(git_str_encode_base64(&buf, "this!", 5));
	cl_assert_equal_s("dGhpcyE=", buf.ptr);

	git_str_clear(&buf);
	cl_git_pass(git_str_encode_base64(&buf, "this!\n", 6));
	cl_assert_equal_s("dGhpcyEK", buf.ptr);

	git_str_dispose(&buf);
}

void test_gitstr__decode_base64(void)
{
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_str_decode_base64(&buf, "dGhpcw==", 8));
	cl_assert_equal_s("this", buf.ptr);

	git_str_clear(&buf);
	cl_git_pass(git_str_decode_base64(&buf, "dGhpcyE=", 8));
	cl_assert_equal_s("this!", buf.ptr);

	git_str_clear(&buf);
	cl_git_pass(git_str_decode_base64(&buf, "dGhpcyEK", 8));
	cl_assert_equal_s("this!\n", buf.ptr);

	cl_git_fail(git_str_decode_base64(&buf, "This is not a valid base64 string!!!", 36));
	cl_assert_equal_s("this!\n", buf.ptr);

	git_str_dispose(&buf);
}

void test_gitstr__encode_base85(void)
{
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_str_encode_base85(&buf, "this", 4));
	cl_assert_equal_s("bZBXF", buf.ptr);
	git_str_clear(&buf);

	cl_git_pass(git_str_encode_base85(&buf, "two rnds", 8));
	cl_assert_equal_s("ba!tca&BaE", buf.ptr);
	git_str_clear(&buf);

	cl_git_pass(git_str_encode_base85(&buf, "this is base 85 encoded",
		strlen("this is base 85 encoded")));
	cl_assert_equal_s("bZBXFAZc?TVqtS-AUHK3Wo~0{WMyOk", buf.ptr);
	git_str_clear(&buf);

	git_str_dispose(&buf);
}

void test_gitstr__decode_base85(void)
{
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_str_decode_base85(&buf, "bZBXF", 5, 4));
	cl_assert_equal_sz(4, buf.size);
	cl_assert_equal_s("this", buf.ptr);
	git_str_clear(&buf);

	cl_git_pass(git_str_decode_base85(&buf, "ba!tca&BaE", 10, 8));
	cl_assert_equal_sz(8, buf.size);
	cl_assert_equal_s("two rnds", buf.ptr);
	git_str_clear(&buf);

	cl_git_pass(git_str_decode_base85(&buf, "bZBXFAZc?TVqtS-AUHK3Wo~0{WMyOk", 30, 23));
	cl_assert_equal_sz(23, buf.size);
	cl_assert_equal_s("this is base 85 encoded", buf.ptr);
	git_str_clear(&buf);

	git_str_dispose(&buf);
}

void test_gitstr__decode_base85_fails_gracefully(void)
{
	git_str buf = GIT_STR_INIT;

	git_str_puts(&buf, "foobar");

	cl_git_fail(git_str_decode_base85(&buf, "invalid charsZZ", 15, 42));
	cl_git_fail(git_str_decode_base85(&buf, "invalidchars__ ", 15, 42));
	cl_git_fail(git_str_decode_base85(&buf, "overflowZZ~~~~~", 15, 42));
	cl_git_fail(git_str_decode_base85(&buf, "truncated", 9, 42));
	cl_assert_equal_sz(6, buf.size);
	cl_assert_equal_s("foobar", buf.ptr);

	git_str_dispose(&buf);
}

void test_gitstr__classify_with_utf8(void)
{
	char *data0 = "Simple text\n";
	size_t data0len = 12;
	char *data1 = "Is that UTF-8 data I see…\nYep!\n";
	size_t data1len = 31;
	char *data2 = "Internal NUL!!!\000\n\nI see you!\n";
	size_t data2len = 29;
	char *data3 = "\xef\xbb\xbfThis is UTF-8 with a BOM.\n";
	size_t data3len = 20;
	git_str b;

	b.ptr = data0; b.size = b.asize = data0len;
	cl_assert(!git_str_is_binary(&b));
	cl_assert(!git_str_contains_nul(&b));

	b.ptr = data1; b.size = b.asize = data1len;
	cl_assert(!git_str_is_binary(&b));
	cl_assert(!git_str_contains_nul(&b));

	b.ptr = data2; b.size = b.asize = data2len;
	cl_assert(git_str_is_binary(&b));
	cl_assert(git_str_contains_nul(&b));

	b.ptr = data3; b.size = b.asize = data3len;
	cl_assert(!git_str_is_binary(&b));
	cl_assert(!git_str_contains_nul(&b));
}

#include "crlf.h"

#define check_buf(expected,buf) do { \
	cl_assert_equal_s(expected, buf.ptr); \
	cl_assert_equal_sz(strlen(expected), buf.size); } while (0)

void test_gitstr__lf_and_crlf_conversions(void)
{
	git_str src = GIT_STR_INIT, tgt = GIT_STR_INIT;

	/* LF source */

	git_str_sets(&src, "lf\nlf\nlf\nlf\n");

	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf("lf\r\nlf\r\nlf\r\nlf\r\n", tgt);

	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf(src.ptr, tgt);

	git_str_sets(&src, "\nlf\nlf\nlf\nlf\nlf");

	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf("\r\nlf\r\nlf\r\nlf\r\nlf\r\nlf", tgt);

	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf(src.ptr, tgt);

	/* CRLF source */

	git_str_sets(&src, "crlf\r\ncrlf\r\ncrlf\r\ncrlf\r\n");

	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf("crlf\r\ncrlf\r\ncrlf\r\ncrlf\r\n", tgt);

	git_str_sets(&src, "crlf\r\ncrlf\r\ncrlf\r\ncrlf\r\n");

	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf("crlf\ncrlf\ncrlf\ncrlf\n", tgt);

	git_str_sets(&src, "\r\ncrlf\r\ncrlf\r\ncrlf\r\ncrlf\r\ncrlf");

	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf("\r\ncrlf\r\ncrlf\r\ncrlf\r\ncrlf\r\ncrlf", tgt);

	git_str_sets(&src, "\r\ncrlf\r\ncrlf\r\ncrlf\r\ncrlf\r\ncrlf");

	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf("\ncrlf\ncrlf\ncrlf\ncrlf\ncrlf", tgt);

	/* CRLF in LF text */

	git_str_sets(&src, "\nlf\nlf\ncrlf\r\nlf\nlf\ncrlf\r\n");

	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf("\r\nlf\r\nlf\r\ncrlf\r\nlf\r\nlf\r\ncrlf\r\n", tgt);

	git_str_sets(&src, "\nlf\nlf\ncrlf\r\nlf\nlf\ncrlf\r\n");

	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf("\nlf\nlf\ncrlf\nlf\nlf\ncrlf\n", tgt);

	/* LF in CRLF text */

	git_str_sets(&src, "\ncrlf\r\ncrlf\r\nlf\ncrlf\r\ncrlf");

	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf("\r\ncrlf\r\ncrlf\r\nlf\r\ncrlf\r\ncrlf", tgt);

	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf("\ncrlf\ncrlf\nlf\ncrlf\ncrlf", tgt);

	/* bare CR test */

	git_str_sets(&src, "\rcrlf\r\nlf\nlf\ncr\rcrlf\r\nlf\ncr\r");

	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf("\rcrlf\r\nlf\r\nlf\r\ncr\rcrlf\r\nlf\r\ncr\r", tgt);

	git_str_sets(&src, "\rcrlf\r\nlf\nlf\ncr\rcrlf\r\nlf\ncr\r");

	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf("\rcrlf\nlf\nlf\ncr\rcrlf\nlf\ncr\r", tgt);

	git_str_sets(&src, "\rcr\r");
	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf(src.ptr, tgt);
	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf("\rcr\r", tgt);

	git_str_dispose(&src);
	git_str_dispose(&tgt);

	/* blob correspondence tests */

	git_str_sets(&src, ALL_CRLF_TEXT_RAW);
	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf(ALL_CRLF_TEXT_AS_CRLF, tgt);
	git_str_sets(&src, ALL_CRLF_TEXT_RAW);
	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf(ALL_CRLF_TEXT_AS_LF, tgt);
	git_str_dispose(&src);
	git_str_dispose(&tgt);

	git_str_sets(&src, ALL_LF_TEXT_RAW);
	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf(ALL_LF_TEXT_AS_CRLF, tgt);
	git_str_sets(&src, ALL_LF_TEXT_RAW);
	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf(ALL_LF_TEXT_AS_LF, tgt);
	git_str_dispose(&src);
	git_str_dispose(&tgt);

	git_str_sets(&src, MORE_CRLF_TEXT_RAW);
	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf(MORE_CRLF_TEXT_AS_CRLF, tgt);
	git_str_sets(&src, MORE_CRLF_TEXT_RAW);
	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf(MORE_CRLF_TEXT_AS_LF, tgt);
	git_str_dispose(&src);
	git_str_dispose(&tgt);

	git_str_sets(&src, MORE_LF_TEXT_RAW);
	cl_git_pass(git_str_lf_to_crlf(&tgt, &src));
	check_buf(MORE_LF_TEXT_AS_CRLF, tgt);
	git_str_sets(&src, MORE_LF_TEXT_RAW);
	cl_git_pass(git_str_crlf_to_lf(&tgt, &src));
	check_buf(MORE_LF_TEXT_AS_LF, tgt);
	git_str_dispose(&src);
	git_str_dispose(&tgt);
}

void test_gitstr__dont_grow_borrowed(void)
{
	const char *somestring = "blah blah";
	git_str buf = GIT_STR_INIT;

	git_str_attach_notowned(&buf, somestring, strlen(somestring) + 1);
	cl_assert_equal_p(somestring, buf.ptr);
	cl_assert_equal_i(0, buf.asize);
	cl_assert_equal_i(strlen(somestring) + 1, buf.size);

	cl_git_fail_with(GIT_EINVALID, git_str_grow(&buf, 1024));
}

void test_gitstr__dont_hit_infinite_loop_when_resizing(void)
{
	git_str buf = GIT_STR_INIT;

	cl_git_pass(git_str_puts(&buf, "foobar"));
	/*
	 * We do not care whether this succeeds or fails, which
	 * would depend on platform-specific allocation
	 * semantics. We only want to know that the function
	 * actually returns.
	 */
	(void)git_str_try_grow(&buf, SIZE_MAX, true);

	git_str_dispose(&buf);
}

void test_gitstr__avoid_printing_into_oom_buffer(void)
{
	git_str buf = GIT_STR_INIT;

	/* Emulate OOM situation with a previous allocation */
	buf.asize = 8;
	buf.ptr = git_str__oom;

	/*
	 * Print the same string again. As the buffer still has
	 * an `asize` of 8 due to the previous print,
	 * `ENSURE_SIZE` would not try to reallocate the array at
	 * all. As it didn't explicitly check for `git_str__oom`
	 * in earlier versions, this would've resulted in it
	 * returning successfully and thus `git_str_puts` would
	 * just print into the `git_str__oom` array.
	 */
	cl_git_fail(git_str_puts(&buf, "foobar"));
}
