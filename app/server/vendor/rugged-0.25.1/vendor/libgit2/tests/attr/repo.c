#include "clar_libgit2.h"
#include "fileops.h"
#include "git2/attr.h"
#include "attr.h"

#include "attr_expect.h"
#include "git2/sys/repository.h"

static git_repository *g_repo = NULL;

void test_attr_repo__initialize(void)
{
	g_repo = cl_git_sandbox_init("attr");
}

void test_attr_repo__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

static struct attr_expected get_one_test_cases[] = {
	{ "root_test1", "repoattr", EXPECT_TRUE, NULL },
	{ "root_test1", "rootattr", EXPECT_TRUE, NULL },
	{ "root_test1", "missingattr", EXPECT_UNDEFINED, NULL },
	{ "root_test1", "subattr", EXPECT_UNDEFINED, NULL },
	{ "root_test1", "negattr", EXPECT_UNDEFINED, NULL },
	{ "root_test2", "repoattr", EXPECT_TRUE, NULL },
	{ "root_test2", "rootattr", EXPECT_FALSE, NULL },
	{ "root_test2", "missingattr", EXPECT_UNDEFINED, NULL },
	{ "root_test2", "multiattr", EXPECT_FALSE, NULL },
	{ "root_test3", "repoattr", EXPECT_TRUE, NULL },
	{ "root_test3", "rootattr", EXPECT_UNDEFINED, NULL },
	{ "root_test3", "multiattr", EXPECT_STRING, "3" },
	{ "root_test3", "multi2", EXPECT_UNDEFINED, NULL },
	{ "sub/subdir_test1", "repoattr", EXPECT_TRUE, NULL },
	{ "sub/subdir_test1", "rootattr", EXPECT_TRUE, NULL },
	{ "sub/subdir_test1", "missingattr", EXPECT_UNDEFINED, NULL },
	{ "sub/subdir_test1", "subattr", EXPECT_STRING, "yes" },
	{ "sub/subdir_test1", "negattr", EXPECT_FALSE, NULL },
	{ "sub/subdir_test1", "another", EXPECT_UNDEFINED, NULL },
	{ "sub/subdir_test2.txt", "repoattr", EXPECT_TRUE, NULL },
	{ "sub/subdir_test2.txt", "rootattr", EXPECT_TRUE, NULL },
	{ "sub/subdir_test2.txt", "missingattr", EXPECT_UNDEFINED, NULL },
	{ "sub/subdir_test2.txt", "subattr", EXPECT_STRING, "yes" },
	{ "sub/subdir_test2.txt", "negattr", EXPECT_FALSE, NULL },
	{ "sub/subdir_test2.txt", "another", EXPECT_STRING, "zero" },
	{ "sub/subdir_test2.txt", "reposub", EXPECT_TRUE, NULL },
	{ "sub/sub/subdir.txt", "another", EXPECT_STRING, "one" },
	{ "sub/sub/subdir.txt", "reposubsub", EXPECT_TRUE, NULL },
	{ "sub/sub/subdir.txt", "reposub", EXPECT_UNDEFINED, NULL },
	{ "does-not-exist", "foo", EXPECT_STRING, "yes" },
	{ "sub/deep/file", "deepdeep", EXPECT_TRUE, NULL },
	{ "sub/sub/d/no", "test", EXPECT_STRING, "a/b/d/*" },
	{ "sub/sub/d/yes", "test", EXPECT_UNDEFINED, NULL },
};

void test_attr_repo__get_one(void)
{
	int i;

	for (i = 0; i < (int)ARRAY_SIZE(get_one_test_cases); ++i) {
		struct attr_expected *scan = &get_one_test_cases[i];
		const char *value;

		cl_git_pass(git_attr_get(&value, g_repo, 0, scan->path, scan->attr));
		attr_check_expected(
			scan->expected, scan->expected_str, scan->attr, value);
	}

	cl_assert(git_attr_cache__is_cached(
		g_repo, GIT_ATTR_FILE__FROM_FILE, ".git/info/attributes"));
	cl_assert(git_attr_cache__is_cached(
		g_repo, GIT_ATTR_FILE__FROM_FILE, ".gitattributes"));
	cl_assert(git_attr_cache__is_cached(
		g_repo, GIT_ATTR_FILE__FROM_FILE, "sub/.gitattributes"));
}

void test_attr_repo__get_one_start_deep(void)
{
	int i;

	for (i = (int)ARRAY_SIZE(get_one_test_cases) - 1; i >= 0; --i) {
		struct attr_expected *scan = &get_one_test_cases[i];
		const char *value;

		cl_git_pass(git_attr_get(&value, g_repo, 0, scan->path, scan->attr));
		attr_check_expected(
			scan->expected, scan->expected_str, scan->attr, value);
	}

	cl_assert(git_attr_cache__is_cached(
		g_repo, GIT_ATTR_FILE__FROM_FILE, ".git/info/attributes"));
	cl_assert(git_attr_cache__is_cached(
		g_repo, GIT_ATTR_FILE__FROM_FILE, ".gitattributes"));
	cl_assert(git_attr_cache__is_cached(
		g_repo, GIT_ATTR_FILE__FROM_FILE, "sub/.gitattributes"));
}

void test_attr_repo__get_many(void)
{
	const char *names[4] = { "repoattr", "rootattr", "missingattr", "subattr" };
	const char *values[4];

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "root_test1", 4, names));

	cl_assert(GIT_ATTR_TRUE(values[0]));
	cl_assert(GIT_ATTR_TRUE(values[1]));
	cl_assert(GIT_ATTR_UNSPECIFIED(values[2]));
	cl_assert(GIT_ATTR_UNSPECIFIED(values[3]));

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "root_test2", 4, names));

	cl_assert(GIT_ATTR_TRUE(values[0]));
	cl_assert(GIT_ATTR_FALSE(values[1]));
	cl_assert(GIT_ATTR_UNSPECIFIED(values[2]));
	cl_assert(GIT_ATTR_UNSPECIFIED(values[3]));

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "sub/subdir_test1", 4, names));

	cl_assert(GIT_ATTR_TRUE(values[0]));
	cl_assert(GIT_ATTR_TRUE(values[1]));
	cl_assert(GIT_ATTR_UNSPECIFIED(values[2]));
	cl_assert_equal_s("yes", values[3]);
}

void test_attr_repo__get_many_in_place(void)
{
	const char *vals[4] = { "repoattr", "rootattr", "missingattr", "subattr" };

	/* it should be legal to look up values into the same array that has
	 * the attribute names, overwriting each name as the value is found.
	 */

	cl_git_pass(git_attr_get_many(vals, g_repo, 0, "sub/subdir_test1", 4, vals));

	cl_assert(GIT_ATTR_TRUE(vals[0]));
	cl_assert(GIT_ATTR_TRUE(vals[1]));
	cl_assert(GIT_ATTR_UNSPECIFIED(vals[2]));
	cl_assert_equal_s("yes", vals[3]);
}

static int count_attrs(
	const char *name,
	const char *value,
	void *payload)
{
	GIT_UNUSED(name);
	GIT_UNUSED(value);

	*((int *)payload) += 1;

	return 0;
}

#define CANCEL_VALUE 12345

static int cancel_iteration(
	const char *name,
	const char *value,
	void *payload)
{
	GIT_UNUSED(name);
	GIT_UNUSED(value);

	*((int *)payload) -= 1;

	if (*((int *)payload) < 0)
		return CANCEL_VALUE;

	return 0;
}

void test_attr_repo__foreach(void)
{
	int count;

	count = 0;
	cl_git_pass(git_attr_foreach(
		g_repo, 0, "root_test1", &count_attrs, &count));
	cl_assert(count == 2);

	count = 0;
	cl_git_pass(git_attr_foreach(g_repo, 0, "sub/subdir_test1",
		&count_attrs, &count));
	cl_assert(count == 4); /* repoattr, rootattr, subattr, negattr */

	count = 0;
	cl_git_pass(git_attr_foreach(g_repo, 0, "sub/subdir_test2.txt",
		&count_attrs, &count));
	cl_assert(count == 6); /* repoattr, rootattr, subattr, reposub, negattr, another */

	count = 2;
	cl_assert_equal_i(
		CANCEL_VALUE, git_attr_foreach(
			g_repo, 0, "sub/subdir_test1", &cancel_iteration, &count)
	);
}

void test_attr_repo__manpage_example(void)
{
	const char *value;

	cl_git_pass(git_attr_get(&value, g_repo, 0, "sub/abc", "foo"));
	cl_assert(GIT_ATTR_TRUE(value));

	cl_git_pass(git_attr_get(&value, g_repo, 0, "sub/abc", "bar"));
	cl_assert(GIT_ATTR_UNSPECIFIED(value));

	cl_git_pass(git_attr_get(&value, g_repo, 0, "sub/abc", "baz"));
	cl_assert(GIT_ATTR_FALSE(value));

	cl_git_pass(git_attr_get(&value, g_repo, 0, "sub/abc", "merge"));
	cl_assert_equal_s("filfre", value);

	cl_git_pass(git_attr_get(&value, g_repo, 0, "sub/abc", "frotz"));
	cl_assert(GIT_ATTR_UNSPECIFIED(value));
}

void test_attr_repo__macros(void)
{
	const char *names[5] = { "rootattr", "binary", "diff", "crlf", "frotz" };
	const char *names2[5] = { "mymacro", "positive", "negative", "rootattr", "another" };
	const char *names3[3] = { "macro2", "multi2", "multi3" };
	const char *values[5];

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "binfile", 5, names));

	cl_assert(GIT_ATTR_TRUE(values[0]));
	cl_assert(GIT_ATTR_TRUE(values[1]));
	cl_assert(GIT_ATTR_FALSE(values[2]));
	cl_assert(GIT_ATTR_FALSE(values[3]));
	cl_assert(GIT_ATTR_UNSPECIFIED(values[4]));

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "macro_test", 5, names2));

	cl_assert(GIT_ATTR_TRUE(values[0]));
	cl_assert(GIT_ATTR_TRUE(values[1]));
	cl_assert(GIT_ATTR_FALSE(values[2]));
	cl_assert(GIT_ATTR_UNSPECIFIED(values[3]));
	cl_assert_equal_s("77", values[4]);

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "macro_test", 3, names3));

	cl_assert(GIT_ATTR_TRUE(values[0]));
	cl_assert(GIT_ATTR_FALSE(values[1]));
	cl_assert_equal_s("answer", values[2]);
}

void test_attr_repo__bad_macros(void)
{
	const char *names[6] = { "rootattr", "positive", "negative",
		"firstmacro", "secondmacro", "thirdmacro" };
	const char *values[6];

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "macro_bad", 6, names));

	/* these three just confirm that the "mymacro" rule ran */
	cl_assert(GIT_ATTR_UNSPECIFIED(values[0]));
	cl_assert(GIT_ATTR_TRUE(values[1]));
	cl_assert(GIT_ATTR_FALSE(values[2]));

	/* file contains:
	 *     # let's try some malicious macro defs
	 *     [attr]firstmacro -thirdmacro -secondmacro
	 *     [attr]secondmacro firstmacro -firstmacro
	 *     [attr]thirdmacro secondmacro=hahaha -firstmacro
	 *     macro_bad firstmacro secondmacro thirdmacro
	 *
	 * firstmacro assignment list ends up with:
	 *     -thirdmacro -secondmacro
	 * secondmacro assignment list expands "firstmacro" and ends up with:
	 *     -thirdmacro -secondmacro -firstmacro
	 * thirdmacro assignment don't expand so list ends up with:
	 *     secondmacro="hahaha"
	 *
	 * macro_bad assignment list ends up with:
	 *     -thirdmacro -secondmacro firstmacro &&
	 *     -thirdmacro -secondmacro -firstmacro secondmacro &&
	 *     secondmacro="hahaha" thirdmacro
	 *
	 * so summary results should be:
	 *     -firstmacro secondmacro="hahaha" thirdmacro
	 */
	cl_assert(GIT_ATTR_FALSE(values[3]));
	cl_assert_equal_s("hahaha", values[4]);
	cl_assert(GIT_ATTR_TRUE(values[5]));
}

#define CONTENT "I'm going to be dynamically processed\r\n" \
	"And my line endings...\r\n" \
	"...are going to be\n" \
	"normalized!\r\n"

#define GITATTR "* text=auto\n" \
	"*.txt text\n" \
	"*.data binary\n"

static void add_to_workdir(const char *filename, const char *content)
{
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_buf_joinpath(&buf, "attr", filename));
	cl_git_rewritefile(git_buf_cstr(&buf), content);

	git_buf_free(&buf);
}

static void assert_proper_normalization(git_index *index, const char *filename, const char *expected_sha)
{
	size_t index_pos;
	const git_index_entry *entry;

	add_to_workdir(filename, CONTENT);
	cl_git_pass(git_index_add_bypath(index, filename));

	cl_assert(!git_index_find(&index_pos, index, filename));

	entry = git_index_get_byindex(index, index_pos);
	cl_assert_equal_i(0, git_oid_streq(&entry->id, expected_sha));
}

void test_attr_repo__staging_properly_normalizes_line_endings_according_to_gitattributes_directives(void)
{
	git_index* index;

	cl_git_pass(git_repository_index(&index, g_repo));

	add_to_workdir(".gitattributes", GITATTR);

	assert_proper_normalization(index, "text.txt", "22c74203bace3c2e950278c7ab08da0fca9f4e9b");
	assert_proper_normalization(index, "huh.dunno", "22c74203bace3c2e950278c7ab08da0fca9f4e9b");
	assert_proper_normalization(index, "binary.data", "66eeff1fcbacf589e6d70aa70edd3fce5be2b37c");

	git_index_free(index);
}

void test_attr_repo__bare_repo_with_index(void)
{
	const char *names[4] = { "test1", "test2", "test3", "test4" };
	const char *values[4];
	git_index *index;

	cl_git_pass(git_repository_index(&index, g_repo));

	cl_git_mkfile(
		"attr/.gitattributes",
		"*.txt test1 test2=foobar -test3\n"
		"trial.txt -test1 test2=barfoo !test3 test4\n");
	cl_git_pass(git_index_add_bypath(index, ".gitattributes"));
	git_index_free(index);

	cl_must_pass(p_unlink("attr/.gitattributes"));
	cl_assert(!git_path_exists("attr/.gitattributes"));

	cl_git_pass(git_repository_set_bare(g_repo));

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "file.txt", 4, names));

	cl_assert(GIT_ATTR_TRUE(values[0]));
	cl_assert_equal_s("foobar", values[1]);
	cl_assert(GIT_ATTR_FALSE(values[2]));
	cl_assert(GIT_ATTR_UNSPECIFIED(values[3]));

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "trial.txt", 4, names));

	cl_assert(GIT_ATTR_FALSE(values[0]));
	cl_assert_equal_s("barfoo", values[1]);
	cl_assert(GIT_ATTR_UNSPECIFIED(values[2]));
	cl_assert(GIT_ATTR_TRUE(values[3]));

	cl_git_pass(git_attr_get_many(values, g_repo, 0, "sub/sub/subdir.txt", 4, names));

	cl_assert(GIT_ATTR_TRUE(values[0]));
	cl_assert_equal_s("foobar", values[1]);
	cl_assert(GIT_ATTR_FALSE(values[2]));
	cl_assert(GIT_ATTR_UNSPECIFIED(values[3]));
}
