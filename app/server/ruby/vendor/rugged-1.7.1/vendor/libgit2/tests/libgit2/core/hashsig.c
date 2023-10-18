#include "clar_libgit2.h"
#include "git2/sys/hashsig.h"
#include "futils.h"

#define SIMILARITY_TEST_DATA_1 \
	"000\n001\n002\n003\n004\n005\n006\n007\n008\n009\n" \
	"010\n011\n012\n013\n014\n015\n016\n017\n018\n019\n" \
	"020\n021\n022\n023\n024\n025\n026\n027\n028\n029\n" \
	"030\n031\n032\n033\n034\n035\n036\n037\n038\n039\n" \
	"040\n041\n042\n043\n044\n045\n046\n047\n048\n049\n"

void test_core_hashsig__similarity_metric(void)
{
	git_hashsig *a, *b;
	git_str buf = GIT_STR_INIT;
	int sim;

	/* in the first case, we compare data to itself and expect 100% match */

	cl_git_pass(git_str_sets(&buf, SIMILARITY_TEST_DATA_1));
	cl_git_pass(git_hashsig_create(&a, buf.ptr, buf.size, GIT_HASHSIG_NORMAL));
	cl_git_pass(git_hashsig_create(&b, buf.ptr, buf.size, GIT_HASHSIG_NORMAL));

	cl_assert_equal_i(100, git_hashsig_compare(a, b));

	git_hashsig_free(a);
	git_hashsig_free(b);

	/* if we change just a single byte, how much does that change magnify? */

	cl_git_pass(git_str_sets(&buf, SIMILARITY_TEST_DATA_1));
	cl_git_pass(git_hashsig_create(&a, buf.ptr, buf.size, GIT_HASHSIG_NORMAL));
	cl_git_pass(git_str_sets(&buf,
		"000\n001\n002\n003\n004\n005\n006\n007\n008\n009\n" \
		"010\n011\n012\n013\n014\n015\n016\n017\n018\n019\n" \
		"x020x\n021\n022\n023\n024\n025\n026\n027\n028\n029\n" \
		"030\n031\n032\n033\n034\n035\n036\n037\n038\n039\n" \
		"040\n041\n042\n043\n044\n045\n046\n047\n048\n049\n"
		));
	cl_git_pass(git_hashsig_create(&b, buf.ptr, buf.size, GIT_HASHSIG_NORMAL));

	sim = git_hashsig_compare(a, b);

	cl_assert_in_range(95, sim, 100); /* expect >95% similarity */

	git_hashsig_free(a);
	git_hashsig_free(b);

	/* let's try comparing data to a superset of itself */

	cl_git_pass(git_str_sets(&buf, SIMILARITY_TEST_DATA_1));
	cl_git_pass(git_hashsig_create(&a, buf.ptr, buf.size, GIT_HASHSIG_NORMAL));
	cl_git_pass(git_str_sets(&buf, SIMILARITY_TEST_DATA_1
		"050\n051\n052\n053\n054\n055\n056\n057\n058\n059\n"));
	cl_git_pass(git_hashsig_create(&b, buf.ptr, buf.size, GIT_HASHSIG_NORMAL));

	sim = git_hashsig_compare(a, b);
	/* 20% lines added ~= 10% lines changed */

	cl_assert_in_range(85, sim, 95); /* expect similarity around 90% */

	git_hashsig_free(a);
	git_hashsig_free(b);

	/* what if we keep about half the original data and add half new */

	cl_git_pass(git_str_sets(&buf, SIMILARITY_TEST_DATA_1));
	cl_git_pass(git_hashsig_create(&a, buf.ptr, buf.size, GIT_HASHSIG_NORMAL));
	cl_git_pass(git_str_sets(&buf,
		"000\n001\n002\n003\n004\n005\n006\n007\n008\n009\n" \
		"010\n011\n012\n013\n014\n015\n016\n017\n018\n019\n" \
		"020x\n021\n022\n023\n024\n" \
		"x25\nx26\nx27\nx28\nx29\n" \
		"x30\nx31\nx32\nx33\nx34\nx35\nx36\nx37\nx38\nx39\n" \
		"x40\nx41\nx42\nx43\nx44\nx45\nx46\nx47\nx48\nx49\n"
		));
	cl_git_pass(git_hashsig_create(&b, buf.ptr, buf.size, GIT_HASHSIG_NORMAL));

	sim = git_hashsig_compare(a, b);
	/* 50% lines changed */

	cl_assert_in_range(40, sim, 60); /* expect in the 40-60% similarity range */

	git_hashsig_free(a);
	git_hashsig_free(b);

	/* lastly, let's check that we can hash file content as well */

	cl_git_pass(git_str_sets(&buf, SIMILARITY_TEST_DATA_1));
	cl_git_pass(git_hashsig_create(&a, buf.ptr, buf.size, GIT_HASHSIG_NORMAL));

	cl_git_pass(git_futils_mkdir("scratch", 0755, GIT_MKDIR_PATH));
	cl_git_mkfile("scratch/testdata", SIMILARITY_TEST_DATA_1);
	cl_git_pass(git_hashsig_create_fromfile(
		&b, "scratch/testdata", GIT_HASHSIG_NORMAL));

	cl_assert_equal_i(100, git_hashsig_compare(a, b));

	git_hashsig_free(a);
	git_hashsig_free(b);

	git_str_dispose(&buf);
	git_futils_rmdir_r("scratch", NULL, GIT_RMDIR_REMOVE_FILES);
}

void test_core_hashsig__similarity_metric_whitespace(void)
{
	git_hashsig *a, *b;
	git_str buf = GIT_STR_INIT;
	int sim, i, j;
	git_hashsig_option_t opt;
	const char *tabbed =
		"	for (s = 0; s < sizeof(sep) / sizeof(char); ++s) {\n"
		"		separator = sep[s];\n"
		"		expect = expect_values[s];\n"
		"\n"
		"		for (j = 0; j < sizeof(b) / sizeof(char*); ++j) {\n"
		"			for (i = 0; i < sizeof(a) / sizeof(char*); ++i) {\n"
		"				git_str_join(&buf, separator, a[i], b[j]);\n"
		"				cl_assert_equal_s(*expect, buf.ptr);\n"
		"				expect++;\n"
		"			}\n"
		"		}\n"
		"	}\n";
	const char *spaced =
		"   for (s = 0; s < sizeof(sep) / sizeof(char); ++s) {\n"
		"       separator = sep[s];\n"
		"       expect = expect_values[s];\n"
		"\n"
		"       for (j = 0; j < sizeof(b) / sizeof(char*); ++j) {\n"
		"           for (i = 0; i < sizeof(a) / sizeof(char*); ++i) {\n"
		"               git_str_join(&buf, separator, a[i], b[j]);\n"
		"               cl_assert_equal_s(*expect, buf.ptr);\n"
		"               expect++;\n"
		"           }\n"
		"       }\n"
		"   }\n";
	const char *crlf_spaced2 =
		"  for (s = 0; s < sizeof(sep) / sizeof(char); ++s) {\r\n"
		"    separator = sep[s];\r\n"
		"    expect = expect_values[s];\r\n"
		"\r\n"
		"    for (j = 0; j < sizeof(b) / sizeof(char*); ++j) {\r\n"
		"      for (i = 0; i < sizeof(a) / sizeof(char*); ++i) {\r\n"
		"        git_str_join(&buf, separator, a[i], b[j]);\r\n"
		"        cl_assert_equal_s(*expect, buf.ptr);\r\n"
		"        expect++;\r\n"
		"      }\r\n"
		"    }\r\n"
		"  }\r\n";
	const char *text[3] = { tabbed, spaced, crlf_spaced2 };

	/* let's try variations of our own code with whitespace changes */

	for (opt = GIT_HASHSIG_NORMAL; opt <= GIT_HASHSIG_SMART_WHITESPACE; ++opt) {
		for (i = 0; i < 3; ++i) {
			for (j = 0; j < 3; ++j) {
				cl_git_pass(git_str_sets(&buf, text[i]));
				cl_git_pass(git_hashsig_create(&a, buf.ptr, buf.size, opt));

				cl_git_pass(git_str_sets(&buf, text[j]));
				cl_git_pass(git_hashsig_create(&b, buf.ptr, buf.size, opt));

				sim = git_hashsig_compare(a, b);

				if (opt == GIT_HASHSIG_NORMAL) {
					if (i == j)
						cl_assert_equal_i(100, sim);
					else
						cl_assert_in_range(0, sim, 30); /* pretty different */
				} else {
					cl_assert_equal_i(100, sim);
				}

				git_hashsig_free(a);
				git_hashsig_free(b);
			}
		}
	}

	git_str_dispose(&buf);
}
