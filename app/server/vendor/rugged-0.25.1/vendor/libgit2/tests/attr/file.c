#include "clar_libgit2.h"
#include "attr_file.h"
#include "attr_expect.h"

#define get_rule(X) ((git_attr_rule *)git_vector_get(&file->rules,(X)))
#define get_assign(R,Y) ((git_attr_assignment *)git_vector_get(&(R)->assigns,(Y)))

void test_attr_file__simple_read(void)
{
	git_attr_file *file;
	git_attr_assignment *assign;
	git_attr_rule *rule;

	cl_git_pass(git_attr_file__load_standalone(&file, cl_fixture("attr/attr0")));

	cl_assert_equal_s(cl_fixture("attr/attr0"), file->entry->path);
	cl_assert(file->rules.length == 1);

	rule = get_rule(0);
	cl_assert(rule != NULL);
	cl_assert_equal_s("*", rule->match.pattern);
	cl_assert(rule->match.length == 1);
	cl_assert((rule->match.flags & GIT_ATTR_FNMATCH_HASWILD) != 0);

	cl_assert(rule->assigns.length == 1);
	assign = get_assign(rule, 0);
	cl_assert(assign != NULL);
	cl_assert_equal_s("binary", assign->name);
	cl_assert(GIT_ATTR_TRUE(assign->value));

	git_attr_file__free(file);
}

void test_attr_file__match_variants(void)
{
	git_attr_file *file;
	git_attr_rule *rule;
	git_attr_assignment *assign;

	cl_git_pass(git_attr_file__load_standalone(&file, cl_fixture("attr/attr1")));

	cl_assert_equal_s(cl_fixture("attr/attr1"), file->entry->path);
	cl_assert(file->rules.length == 10);

	/* let's do a thorough check of this rule, then just verify
	 * the things that are unique for the later rules
	 */
	rule = get_rule(0);
	cl_assert(rule);
	cl_assert_equal_s("pat0", rule->match.pattern);
	cl_assert(rule->match.length == strlen("pat0"));
	cl_assert(rule->assigns.length == 1);
	assign = get_assign(rule,0);
	cl_assert_equal_s("attr0", assign->name);
	cl_assert(assign->name_hash == git_attr_file__name_hash(assign->name));
	cl_assert(GIT_ATTR_TRUE(assign->value));

	rule = get_rule(1);
	cl_assert_equal_s("pat1", rule->match.pattern);
	cl_assert(rule->match.length == strlen("pat1"));
	cl_assert((rule->match.flags & GIT_ATTR_FNMATCH_NEGATIVE) != 0);

	rule = get_rule(2);
	cl_assert_equal_s("pat2", rule->match.pattern);
	cl_assert(rule->match.length == strlen("pat2"));
	cl_assert((rule->match.flags & GIT_ATTR_FNMATCH_DIRECTORY) != 0);

	rule = get_rule(3);
	cl_assert_equal_s("pat3dir/pat3file", rule->match.pattern);
	cl_assert((rule->match.flags & GIT_ATTR_FNMATCH_FULLPATH) != 0);

	rule = get_rule(4);
	cl_assert_equal_s("pat4.*", rule->match.pattern);
	cl_assert((rule->match.flags & GIT_ATTR_FNMATCH_HASWILD) != 0);

	rule = get_rule(5);
	cl_assert_equal_s("*.pat5", rule->match.pattern);
	cl_assert((rule->match.flags & GIT_ATTR_FNMATCH_HASWILD) != 0);

	rule = get_rule(7);
	cl_assert_equal_s("pat7[a-e]??[xyz]", rule->match.pattern);
	cl_assert(rule->assigns.length == 1);
	cl_assert((rule->match.flags & GIT_ATTR_FNMATCH_HASWILD) != 0);
	assign = get_assign(rule,0);
	cl_assert_equal_s("attr7", assign->name);
	cl_assert(GIT_ATTR_TRUE(assign->value));

	rule = get_rule(8);
	cl_assert_equal_s("pat8 with spaces", rule->match.pattern);
	cl_assert(rule->match.length == strlen("pat8 with spaces"));

	rule = get_rule(9);
	cl_assert_equal_s("pat9", rule->match.pattern);

	git_attr_file__free(file);
}

static void check_one_assign(
	git_attr_file *file,
	int rule_idx,
	int assign_idx,
	const char *pattern,
	const char *name,
	enum attr_expect_t expected,
	const char *expected_str)
{
	git_attr_rule *rule = get_rule(rule_idx);
	git_attr_assignment *assign = get_assign(rule, assign_idx);

	cl_assert_equal_s(pattern, rule->match.pattern);
	cl_assert(rule->assigns.length == 1);
	cl_assert_equal_s(name, assign->name);
	cl_assert(assign->name_hash == git_attr_file__name_hash(assign->name));

	attr_check_expected(expected, expected_str, assign->name, assign->value);
}

void test_attr_file__assign_variants(void)
{
	git_attr_file *file;
	git_attr_rule *rule;
	git_attr_assignment *assign;

	cl_git_pass(git_attr_file__load_standalone(&file, cl_fixture("attr/attr2")));

	cl_assert_equal_s(cl_fixture("attr/attr2"), file->entry->path);
	cl_assert(file->rules.length == 11);

	check_one_assign(file, 0, 0, "pat0", "simple", EXPECT_TRUE, NULL);
	check_one_assign(file, 1, 0, "pat1", "neg", EXPECT_FALSE, NULL);
	check_one_assign(file, 2, 0, "*", "notundef", EXPECT_TRUE, NULL);
	check_one_assign(file, 3, 0, "pat2", "notundef", EXPECT_UNDEFINED, NULL);
	check_one_assign(file, 4, 0, "pat3", "assigned", EXPECT_STRING, "test-value");
	check_one_assign(file, 5, 0, "pat4", "rule-with-more-chars", EXPECT_STRING, "value-with-more-chars");
	check_one_assign(file, 6, 0, "pat5", "empty", EXPECT_TRUE, NULL);
	check_one_assign(file, 7, 0, "pat6", "negempty", EXPECT_FALSE, NULL);

	rule = get_rule(8);
	cl_assert_equal_s("pat7", rule->match.pattern);
	cl_assert(rule->assigns.length == 5);
	/* assignments will be sorted by hash value, so we have to do
	 * lookups by search instead of by position
	 */
	assign = git_attr_rule__lookup_assignment(rule, "multiple");
	cl_assert(assign);
	cl_assert_equal_s("multiple", assign->name);
	cl_assert(GIT_ATTR_TRUE(assign->value));
	assign = git_attr_rule__lookup_assignment(rule, "single");
	cl_assert(assign);
	cl_assert_equal_s("single", assign->name);
	cl_assert(GIT_ATTR_FALSE(assign->value));
	assign = git_attr_rule__lookup_assignment(rule, "values");
	cl_assert(assign);
	cl_assert_equal_s("values", assign->name);
	cl_assert_equal_s("1", assign->value);
	assign = git_attr_rule__lookup_assignment(rule, "also");
	cl_assert(assign);
	cl_assert_equal_s("also", assign->name);
	cl_assert_equal_s("a-really-long-value/*", assign->value);
	assign = git_attr_rule__lookup_assignment(rule, "happy");
	cl_assert(assign);
	cl_assert_equal_s("happy", assign->name);
	cl_assert_equal_s("yes!", assign->value);
	assign = git_attr_rule__lookup_assignment(rule, "other");
	cl_assert(!assign);

	rule = get_rule(9);
	cl_assert_equal_s("pat8", rule->match.pattern);
	cl_assert(rule->assigns.length == 2);
	assign = git_attr_rule__lookup_assignment(rule, "again");
	cl_assert(assign);
	cl_assert_equal_s("again", assign->name);
	cl_assert(GIT_ATTR_TRUE(assign->value));
	assign = git_attr_rule__lookup_assignment(rule, "another");
	cl_assert(assign);
	cl_assert_equal_s("another", assign->name);
	cl_assert_equal_s("12321", assign->value);

	check_one_assign(file, 10, 0, "pat9", "at-eof", EXPECT_FALSE, NULL);

	git_attr_file__free(file);
}

void test_attr_file__check_attr_examples(void)
{
	git_attr_file *file;
	git_attr_rule *rule;
	git_attr_assignment *assign;

	cl_git_pass(git_attr_file__load_standalone(&file, cl_fixture("attr/attr3")));
	cl_assert_equal_s(cl_fixture("attr/attr3"), file->entry->path);
	cl_assert(file->rules.length == 3);

	rule = get_rule(0);
	cl_assert_equal_s("*.java", rule->match.pattern);
	cl_assert(rule->assigns.length == 3);
	assign = git_attr_rule__lookup_assignment(rule, "diff");
	cl_assert_equal_s("diff", assign->name);
	cl_assert_equal_s("java", assign->value);
	assign = git_attr_rule__lookup_assignment(rule, "crlf");
	cl_assert_equal_s("crlf", assign->name);
	cl_assert(GIT_ATTR_FALSE(assign->value));
	assign = git_attr_rule__lookup_assignment(rule, "myAttr");
	cl_assert_equal_s("myAttr", assign->name);
	cl_assert(GIT_ATTR_TRUE(assign->value));
	assign = git_attr_rule__lookup_assignment(rule, "missing");
	cl_assert(assign == NULL);

	rule = get_rule(1);
	cl_assert_equal_s("NoMyAttr.java", rule->match.pattern);
	cl_assert(rule->assigns.length == 1);
	assign = get_assign(rule, 0);
	cl_assert_equal_s("myAttr", assign->name);
	cl_assert(GIT_ATTR_UNSPECIFIED(assign->value));

	rule = get_rule(2);
	cl_assert_equal_s("README", rule->match.pattern);
	cl_assert(rule->assigns.length == 1);
	assign = get_assign(rule, 0);
	cl_assert_equal_s("caveat", assign->name);
	cl_assert_equal_s("unspecified", assign->value);

	git_attr_file__free(file);
}
