#include "clar_libgit2.h"
#include "attr_file.h"

#include "attr_expect.h"

void test_attr_lookup__simple(void)
{
	git_attr_file *file;
	git_attr_path path;
	const char *value = NULL;

	cl_git_pass(git_attr_file__load_standalone(&file, cl_fixture("attr/attr0")));
	cl_assert_equal_s(cl_fixture("attr/attr0"), file->entry->path);
	cl_assert(file->rules.length == 1);

	cl_git_pass(git_attr_path__init(&path, "test", NULL, GIT_DIR_FLAG_UNKNOWN));
	cl_assert_equal_s("test", path.path);
	cl_assert_equal_s("test", path.basename);
	cl_assert(!path.is_dir);

	cl_git_pass(git_attr_file__lookup_one(file,&path,"binary",&value));
	cl_assert(GIT_ATTR_TRUE(value));

	cl_git_pass(git_attr_file__lookup_one(file,&path,"missing",&value));
	cl_assert(!value);

	git_attr_path__free(&path);
	git_attr_file__free(file);
}

static void run_test_cases(git_attr_file *file, struct attr_expected *cases, int force_dir)
{
	git_attr_path path;
	const char *value = NULL;
	struct attr_expected *c;
	int error;

	for (c = cases; c->path != NULL; c++) {
		cl_git_pass(git_attr_path__init(&path, c->path, NULL, GIT_DIR_FLAG_UNKNOWN));

		if (force_dir)
			path.is_dir = 1;

		error = git_attr_file__lookup_one(file,&path,c->attr,&value);
		cl_git_pass(error);

		attr_check_expected(c->expected, c->expected_str, c->attr, value);

		git_attr_path__free(&path);
	}
}

void test_attr_lookup__match_variants(void)
{
	git_attr_file *file;
	git_attr_path path;

	struct attr_expected dir_cases[] = {
		{ "pat2", "attr2", EXPECT_TRUE, NULL },
		{ "/testing/for/pat2", "attr2", EXPECT_TRUE, NULL },
		{ "/not/pat2/yousee", "attr2", EXPECT_UNDEFINED, NULL },
		{ "/fun/fun/fun/pat4.dir", "attr4", EXPECT_TRUE, NULL },
		{ "foo.pat5", "attr5", EXPECT_TRUE, NULL },
		{ NULL, NULL, 0, NULL }
	};

	struct attr_expected cases[] = {
		/* pat0 -> simple match */
		{ "pat0", "attr0", EXPECT_TRUE, NULL },
		{ "/testing/for/pat0", "attr0", EXPECT_TRUE, NULL },
		{ "relative/to/pat0", "attr0", EXPECT_TRUE, NULL },
		{ "this-contains-pat0-inside", "attr0", EXPECT_UNDEFINED, NULL },
		{ "this-aint-right", "attr0", EXPECT_UNDEFINED, NULL },
		{ "/this/pat0/dont/match", "attr0", EXPECT_UNDEFINED, NULL },
		/* negative match */
		{ "pat0", "attr1", EXPECT_TRUE, NULL },
		{ "pat1", "attr1", EXPECT_UNDEFINED, NULL },
		{ "/testing/for/pat1", "attr1", EXPECT_UNDEFINED, NULL },
		{ "/testing/for/pat0", "attr1", EXPECT_TRUE, NULL },
		{ "/testing/for/pat1/inside", "attr1", EXPECT_TRUE, NULL },
		{ "misc", "attr1", EXPECT_TRUE, NULL },
		/* dir match */
		{ "pat2", "attr2", EXPECT_UNDEFINED, NULL },
		{ "/testing/for/pat2", "attr2", EXPECT_UNDEFINED, NULL },
		{ "/not/pat2/yousee", "attr2", EXPECT_UNDEFINED, NULL },
		/* path match */
		{ "pat3file", "attr3", EXPECT_UNDEFINED, NULL },
		{ "/pat3dir/pat3file", "attr3", EXPECT_TRUE, NULL },
		{ "pat3dir/pat3file", "attr3", EXPECT_TRUE, NULL },
		/* pattern* match */
		{ "pat4.txt", "attr4", EXPECT_TRUE, NULL },
		{ "/fun/fun/fun/pat4.c", "attr4", EXPECT_TRUE, NULL },
		{ "pat4.", "attr4", EXPECT_TRUE, NULL },
		{ "pat4", "attr4", EXPECT_UNDEFINED, NULL },
		/* *pattern match */
		{ "foo.pat5", "attr5", EXPECT_TRUE, NULL },
		{ "/this/is/ok.pat5", "attr5", EXPECT_TRUE, NULL },
		{ "/this/is/bad.pat5/yousee.txt", "attr5", EXPECT_UNDEFINED, NULL },
		{ "foo.pat5", "attr100", EXPECT_UNDEFINED, NULL },
		/* glob match with slashes */
		{ "foo.pat6", "attr6", EXPECT_UNDEFINED, NULL },
		{ "pat6/pat6/foobar.pat6", "attr6", EXPECT_TRUE, NULL },
		{ "pat6/pat6/.pat6", "attr6", EXPECT_TRUE, NULL },
		{ "pat6/pat6/extra/foobar.pat6", "attr6", EXPECT_UNDEFINED, NULL },
		{ "/prefix/pat6/pat6/foobar.pat6", "attr6", EXPECT_UNDEFINED, NULL },
		{ "/pat6/pat6/foobar.pat6", "attr6", EXPECT_TRUE, NULL },
		/* complex pattern */
		{ "pat7a12z", "attr7", EXPECT_TRUE, NULL },
		{ "pat7e__x", "attr7", EXPECT_TRUE, NULL },
		{ "pat7b/1y", "attr7", EXPECT_UNDEFINED, NULL }, /* ? does not match / */
		{ "pat7e_x", "attr7", EXPECT_UNDEFINED, NULL },
		{ "pat7aaaa", "attr7", EXPECT_UNDEFINED, NULL },
		{ "pat7zzzz", "attr7", EXPECT_UNDEFINED, NULL },
		{ "/this/can/be/anything/pat7a12z", "attr7", EXPECT_TRUE, NULL },
		{ "but/it/still/must/match/pat7aaaa", "attr7", EXPECT_UNDEFINED, NULL },
		{ "pat7aaay.fail", "attr7", EXPECT_UNDEFINED, NULL },
		/* pattern with spaces */
		{ "pat8 with spaces", "attr8", EXPECT_TRUE, NULL },
		{ "/gotta love/pat8 with spaces", "attr8", EXPECT_TRUE, NULL },
		{ "failing pat8 with spaces", "attr8", EXPECT_UNDEFINED, NULL },
		{ "spaces", "attr8", EXPECT_UNDEFINED, NULL },
		/* pattern at eof */
		{ "pat9", "attr9", EXPECT_TRUE, NULL },
		{ "/eof/pat9", "attr9", EXPECT_TRUE, NULL },
		{ "pat", "attr9", EXPECT_UNDEFINED, NULL },
		{ "at9", "attr9", EXPECT_UNDEFINED, NULL },
		{ "pat9.fail", "attr9", EXPECT_UNDEFINED, NULL },
		/* sentinel at end */
		{ NULL, NULL, 0, NULL }
	};

	cl_git_pass(git_attr_file__load_standalone(&file, cl_fixture("attr/attr1")));
	cl_assert_equal_s(cl_fixture("attr/attr1"), file->entry->path);
	cl_assert(file->rules.length == 10);

	cl_git_pass(git_attr_path__init(&path, "/testing/for/pat0", NULL, GIT_DIR_FLAG_UNKNOWN));
	cl_assert_equal_s("pat0", path.basename);

	run_test_cases(file, cases, 0);
	run_test_cases(file, dir_cases, 1);

	git_attr_file__free(file);
	git_attr_path__free(&path);
}

void test_attr_lookup__assign_variants(void)
{
	git_attr_file *file;

	struct attr_expected cases[] = {
		/* pat0 -> simple assign */
		{ "pat0", "simple", EXPECT_TRUE, NULL },
		{ "/testing/pat0", "simple", EXPECT_TRUE, NULL },
		{ "pat0", "fail", EXPECT_UNDEFINED, NULL },
		{ "/testing/pat0", "fail", EXPECT_UNDEFINED, NULL },
		/* negative assign */
		{ "pat1", "neg", EXPECT_FALSE, NULL },
		{ "/testing/pat1", "neg", EXPECT_FALSE, NULL },
		{ "pat1", "fail", EXPECT_UNDEFINED, NULL },
		{ "/testing/pat1", "fail", EXPECT_UNDEFINED, NULL },
		/* forced undef */
		{ "pat1", "notundef", EXPECT_TRUE, NULL },
		{ "pat2", "notundef", EXPECT_UNDEFINED, NULL },
		{ "/lead/in/pat1", "notundef", EXPECT_TRUE, NULL },
		{ "/lead/in/pat2", "notundef", EXPECT_UNDEFINED, NULL },
		/* assign value */
		{ "pat3", "assigned", EXPECT_STRING, "test-value" },
		{ "pat3", "notassigned", EXPECT_UNDEFINED, NULL },
		/* assign value */
		{ "pat4", "rule-with-more-chars", EXPECT_STRING, "value-with-more-chars" },
		{ "pat4", "notassigned-rule-with-more-chars", EXPECT_UNDEFINED, NULL },
		/* empty assignments */
		{ "pat5", "empty", EXPECT_TRUE, NULL },
		{ "pat6", "negempty", EXPECT_FALSE, NULL },
		/* multiple assignment */
		{ "pat7", "multiple", EXPECT_TRUE, NULL },
		{ "pat7", "single", EXPECT_FALSE, NULL },
		{ "pat7", "values", EXPECT_STRING, "1" },
		{ "pat7", "also", EXPECT_STRING, "a-really-long-value/*" },
		{ "pat7", "happy", EXPECT_STRING, "yes!" },
		{ "pat8", "again", EXPECT_TRUE, NULL },
		{ "pat8", "another", EXPECT_STRING, "12321" },
		/* bad assignment */
		{ "patbad0", "simple", EXPECT_UNDEFINED, NULL },
		{ "patbad0", "notundef", EXPECT_TRUE, NULL },
		{ "patbad1", "simple", EXPECT_UNDEFINED, NULL },
		/* eof assignment */
		{ "pat9", "at-eof", EXPECT_FALSE, NULL },
		/* sentinel at end */
		{ NULL, NULL, 0, NULL }
	};

	cl_git_pass(git_attr_file__load_standalone(&file, cl_fixture("attr/attr2")));
	cl_assert(file->rules.length == 11);

	run_test_cases(file, cases, 0);

	git_attr_file__free(file);
}

void test_attr_lookup__check_attr_examples(void)
{
	git_attr_file *file;

	struct attr_expected cases[] = {
		{ "foo.java", "diff", EXPECT_STRING, "java" },
		{ "foo.java", "crlf", EXPECT_FALSE, NULL },
		{ "foo.java", "myAttr", EXPECT_TRUE, NULL },
		{ "foo.java", "other", EXPECT_UNDEFINED, NULL },
		{ "/prefix/dir/foo.java", "diff", EXPECT_STRING, "java" },
		{ "/prefix/dir/foo.java", "crlf", EXPECT_FALSE, NULL },
		{ "/prefix/dir/foo.java", "myAttr", EXPECT_TRUE, NULL },
		{ "/prefix/dir/foo.java", "other", EXPECT_UNDEFINED, NULL },
		{ "NoMyAttr.java", "crlf", EXPECT_FALSE, NULL },
		{ "NoMyAttr.java", "myAttr", EXPECT_UNDEFINED, NULL },
		{ "NoMyAttr.java", "other", EXPECT_UNDEFINED, NULL },
		{ "/prefix/dir/NoMyAttr.java", "crlf", EXPECT_FALSE, NULL },
		{ "/prefix/dir/NoMyAttr.java", "myAttr", EXPECT_UNDEFINED, NULL },
		{ "/prefix/dir/NoMyAttr.java", "other", EXPECT_UNDEFINED, NULL },
		{ "README", "caveat", EXPECT_STRING, "unspecified" },
		{ "/specific/path/README", "caveat", EXPECT_STRING, "unspecified" },
		{ "README", "missing", EXPECT_UNDEFINED, NULL },
		{ "/specific/path/README", "missing", EXPECT_UNDEFINED, NULL },
		/* sentinel at end */
		{ NULL, NULL, 0, NULL }
	};

	cl_git_pass(git_attr_file__load_standalone(&file, cl_fixture("attr/attr3")));
	cl_assert(file->rules.length == 3);

	run_test_cases(file, cases, 0);

	git_attr_file__free(file);
}

void test_attr_lookup__from_buffer(void)
{
	git_attr_file *file;

	struct attr_expected cases[] = {
		{ "abc", "foo", EXPECT_TRUE, NULL },
		{ "abc", "bar", EXPECT_TRUE, NULL },
		{ "abc", "baz", EXPECT_TRUE, NULL },
		{ "aaa", "foo", EXPECT_TRUE, NULL },
		{ "aaa", "bar", EXPECT_UNDEFINED, NULL },
		{ "aaa", "baz", EXPECT_TRUE, NULL },
		{ "qqq", "foo", EXPECT_UNDEFINED, NULL },
		{ "qqq", "bar", EXPECT_UNDEFINED, NULL },
		{ "qqq", "baz", EXPECT_TRUE, NULL },
		{ NULL, NULL, 0, NULL }
	};

	cl_git_pass(git_attr_file__new(&file, NULL, 0));

	cl_git_pass(git_attr_file__parse_buffer(NULL, file, "a* foo\nabc bar\n* baz"));

	cl_assert(file->rules.length == 3);

	run_test_cases(file, cases, 0);

	git_attr_file__free(file);
}
