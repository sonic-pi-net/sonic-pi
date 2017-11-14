#include "clar_libgit2.h"
#include "buffer.h"
#include "path.h"

static git_buf buf = GIT_BUF_INIT;

void test_config_read__cleanup(void)
{
	git_buf_free(&buf);
}

void test_config_read__simple_read(void)
{
	git_config *cfg;
	int32_t i;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config0")));

	cl_git_pass(git_config_get_int32(&i, cfg, "core.repositoryformatversion"));
	cl_assert(i == 0);
	cl_git_pass(git_config_get_bool(&i, cfg, "core.filemode"));
	cl_assert(i == 1);
	cl_git_pass(git_config_get_bool(&i, cfg, "core.bare"));
	cl_assert(i == 0);
	cl_git_pass(git_config_get_bool(&i, cfg, "core.logallrefupdates"));
	cl_assert(i == 1);

	git_config_free(cfg);
}

void test_config_read__case_sensitive(void)
{
	git_config *cfg;
	int i;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config1")));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "this.that.other"));
	cl_assert_equal_s("true", git_buf_cstr(&buf));
	git_buf_clear(&buf);

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "this.That.other"));
	cl_assert_equal_s("yes", git_buf_cstr(&buf));

	cl_git_pass(git_config_get_bool(&i, cfg, "this.that.other"));
	cl_assert(i == 1);
	cl_git_pass(git_config_get_bool(&i, cfg, "this.That.other"));
	cl_assert(i == 1);

	/* This one doesn't exist */
	cl_must_fail(git_config_get_bool(&i, cfg, "this.thaT.other"));

	git_config_free(cfg);
}

/*
 * If \ is the last non-space character on the line, we read the next
 * one, separating each line with SP.
 */
void test_config_read__multiline_value(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config2")));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "this.That.and"));
	cl_assert_equal_s("one one one two two three three", git_buf_cstr(&buf));

	git_config_free(cfg);
}

static void clean_test_config(void *unused)
{
	GIT_UNUSED(unused);
	cl_fixture_cleanup("./testconfig");
}

void test_config_read__multiline_value_and_eof(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[header]\n  key1 = foo\\\n");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "header.key1"));
	cl_assert_equal_s("foo", git_buf_cstr(&buf));

	git_config_free(cfg);
}

void test_config_read__multiline_eof(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[header]\n  key1 = \\\n");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "header.key1"));
	cl_assert_equal_s("", git_buf_cstr(&buf));

	git_config_free(cfg);
}

/*
 * This kind of subsection declaration is case-insensitive
 */
void test_config_read__subsection_header(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config3")));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "section.subsection.var"));
	cl_assert_equal_s("hello", git_buf_cstr(&buf));

	/* The subsection is transformed to lower-case */
	cl_must_fail(git_config_get_string_buf(&buf, cfg, "section.subSectIon.var"));

	git_config_free(cfg);
}

void test_config_read__lone_variable(void)
{
	git_config *cfg;
	int i;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config4")));

	cl_git_fail(git_config_get_int32(&i, cfg, "some.section.variable"));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.section.variable"));
	cl_assert_equal_s("", git_buf_cstr(&buf));
	git_buf_clear(&buf);

	cl_git_pass(git_config_get_bool(&i, cfg, "some.section.variable"));
	cl_assert(i == 1);

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.section.variableeq"));
	cl_assert_equal_s("", git_buf_cstr(&buf));

	cl_git_pass(git_config_get_bool(&i, cfg, "some.section.variableeq"));
	cl_assert(i == 0);

	git_config_free(cfg);
}

void test_config_read__number_suffixes(void)
{
	git_config *cfg;
	int64_t i;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config5")));

	cl_git_pass(git_config_get_int64(&i, cfg, "number.simple"));
	cl_assert(i == 1);

	cl_git_pass(git_config_get_int64(&i, cfg, "number.k"));
	cl_assert(i == 1 * 1024);

	cl_git_pass(git_config_get_int64(&i, cfg, "number.kk"));
	cl_assert(i == 1 * 1024);

	cl_git_pass(git_config_get_int64(&i, cfg, "number.m"));
	cl_assert(i == 1 * 1024 * 1024);

	cl_git_pass(git_config_get_int64(&i, cfg, "number.mm"));
	cl_assert(i == 1 * 1024 * 1024);

	cl_git_pass(git_config_get_int64(&i, cfg, "number.g"));
	cl_assert(i == 1 * 1024 * 1024 * 1024);

	cl_git_pass(git_config_get_int64(&i, cfg, "number.gg"));
	cl_assert(i == 1 * 1024 * 1024 * 1024);

	git_config_free(cfg);
}

void test_config_read__blank_lines(void)
{
	git_config *cfg;
	int i;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config6")));

	cl_git_pass(git_config_get_bool(&i, cfg, "valid.subsection.something"));
	cl_assert(i == 1);

	cl_git_pass(git_config_get_bool(&i, cfg, "something.else.something"));
	cl_assert(i == 0);

	git_config_free(cfg);
}

void test_config_read__invalid_ext_headers(void)
{
	git_config *cfg;
	cl_must_fail(git_config_open_ondisk(&cfg, cl_fixture("config/config7")));
}

void test_config_read__empty_files(void)
{
	git_config *cfg;
	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config8")));
	git_config_free(cfg);
}

void test_config_read__symbol_headers(void)
{
	git_config *cfg;
	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config20")));
	git_config_free(cfg);
}

void test_config_read__header_in_last_line(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config10")));
	git_config_free(cfg);
}

void test_config_read__prefixes(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config9")));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "remote.ab.url"));
	cl_assert_equal_s("http://example.com/git/ab", git_buf_cstr(&buf));
	git_buf_clear(&buf);

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "remote.abba.url"));
	cl_assert_equal_s("http://example.com/git/abba", git_buf_cstr(&buf));

	git_config_free(cfg);
}

void test_config_read__escaping_quotes(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config13")));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "core.editor"));
	cl_assert_equal_s("\"C:/Program Files/Nonsense/bah.exe\" \"--some option\"", git_buf_cstr(&buf));

	git_config_free(cfg);
}

void test_config_read__invalid_escape_sequence(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[header]\n  key1 = \\\\\\;\n  key2 = value2\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	git_config_free(cfg);
}

static int count_cfg_entries_and_compare_levels(
	const git_config_entry *entry, void *payload)
{
	int *count = payload;

	if (!strcmp(entry->value, "7") || !strcmp(entry->value, "17"))
		cl_assert(entry->level == GIT_CONFIG_LEVEL_GLOBAL);
	else
		cl_assert(entry->level == GIT_CONFIG_LEVEL_SYSTEM);

	(*count)++;
	return 0;
}

static int cfg_callback_countdown(const git_config_entry *entry, void *payload)
{
	int *count = payload;
	GIT_UNUSED(entry);
	(*count)--;
	if (*count == 0)
		return -100;
	return 0;
}

void test_config_read__foreach(void)
{
	git_config *cfg;
	int count, ret;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config9"),
		GIT_CONFIG_LEVEL_SYSTEM, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config15"),
		GIT_CONFIG_LEVEL_GLOBAL, 0));

	count = 0;
	cl_git_pass(git_config_foreach(cfg, count_cfg_entries_and_compare_levels, &count));
	cl_assert_equal_i(7, count);

	count = 3;
	cl_git_fail(ret = git_config_foreach(cfg, cfg_callback_countdown, &count));
	cl_assert_equal_i(-100, ret);

	git_config_free(cfg);
}

void test_config_read__iterator(void)
{
	git_config *cfg;
	git_config_iterator *iter;
	git_config_entry *entry;
	int count, ret;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config9"),
		GIT_CONFIG_LEVEL_SYSTEM, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config15"),
		GIT_CONFIG_LEVEL_GLOBAL, 0));

	count = 0;
	cl_git_pass(git_config_iterator_new(&iter, cfg));

	while ((ret = git_config_next(&entry, iter)) == 0) {
		count++;
	}

	git_config_iterator_free(iter);
	cl_assert_equal_i(GIT_ITEROVER, ret);
	cl_assert_equal_i(7, count);

	count = 3;
	cl_git_pass(git_config_iterator_new(&iter, cfg));

	git_config_iterator_free(iter);
	git_config_free(cfg);
}

static int count_cfg_entries(const git_config_entry *entry, void *payload)
{
	int *count = payload;
	GIT_UNUSED(entry);
	(*count)++;
	return 0;
}

void test_config_read__foreach_match(void)
{
	git_config *cfg;
	int count;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config9")));

	count = 0;
	cl_git_pass(
		git_config_foreach_match(cfg, "core.*", count_cfg_entries, &count));
	cl_assert_equal_i(3, count);

	count = 0;
	cl_git_pass(
		git_config_foreach_match(cfg, "remote\\.ab.*", count_cfg_entries, &count));
	cl_assert_equal_i(2, count);

	count = 0;
	cl_git_pass(
		git_config_foreach_match(cfg, ".*url$", count_cfg_entries, &count));
	cl_assert_equal_i(2, count);

	count = 0;
	cl_git_pass(
		git_config_foreach_match(cfg, ".*dummy.*", count_cfg_entries, &count));
	cl_assert_equal_i(2, count);

	count = 0;
	cl_git_pass(
		git_config_foreach_match(cfg, ".*nomatch.*", count_cfg_entries, &count));
	cl_assert_equal_i(0, count);

	git_config_free(cfg);
}

static void check_glob_iter(git_config *cfg, const char *regexp, int expected)
{
	git_config_iterator *iter;
	git_config_entry *entry;
	int count, error;

	cl_git_pass(git_config_iterator_glob_new(&iter, cfg, regexp));

	count = 0;
	while ((error = git_config_next(&entry, iter)) == 0)
		count++;

	cl_assert_equal_i(GIT_ITEROVER, error);
	cl_assert_equal_i(expected, count);
	git_config_iterator_free(iter);
}

void test_config_read__iterator_invalid_glob(void)
{
	git_config *cfg;
	git_config_iterator *iter;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config9")));

	cl_git_fail(git_config_iterator_glob_new(&iter, cfg, "*"));

	git_config_free(cfg);
}

void test_config_read__iterator_glob(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config9")));

	check_glob_iter(cfg, "core.*", 3);
	check_glob_iter(cfg, "remote\\.ab.*", 2);
	check_glob_iter(cfg, ".*url$", 2);
	check_glob_iter(cfg, ".*dummy.*", 2);
	check_glob_iter(cfg, ".*nomatch.*", 0);

	git_config_free(cfg);
}

void test_config_read__whitespace_not_required_around_assignment(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config14")));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "a.b"));
	cl_assert_equal_s("c", git_buf_cstr(&buf));
	git_buf_clear(&buf);

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "d.e"));
	cl_assert_equal_s("f", git_buf_cstr(&buf));

	git_config_free(cfg);
}

void test_config_read__read_git_config_entry(void)
{
	git_config *cfg;
	git_config_entry *entry;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config9"),
		GIT_CONFIG_LEVEL_SYSTEM, 0));

	cl_git_pass(git_config_get_entry(&entry, cfg, "core.dummy2"));
	cl_assert_equal_s("core.dummy2", entry->name);
	cl_assert_equal_s("42", entry->value);
	cl_assert_equal_i(GIT_CONFIG_LEVEL_SYSTEM, entry->level);

	git_config_entry_free(entry);
	git_config_free(cfg);
}

/*
 * At the beginning of the test:
 *  - config9 has: core.dummy2=42
 *  - config15 has: core.dummy2=7
 *  - config16 has: core.dummy2=28
 */
void test_config_read__local_config_overrides_global_config_overrides_system_config(void)
{
	git_config *cfg;
	int32_t i;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config9"),
		GIT_CONFIG_LEVEL_SYSTEM, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config15"),
		GIT_CONFIG_LEVEL_GLOBAL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config16"),
		GIT_CONFIG_LEVEL_LOCAL, 0));

	cl_git_pass(git_config_get_int32(&i, cfg, "core.dummy2"));
	cl_assert_equal_i(28, i);

	git_config_free(cfg);

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config9"),
		GIT_CONFIG_LEVEL_SYSTEM, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config15"),
		GIT_CONFIG_LEVEL_GLOBAL, 0));

	cl_git_pass(git_config_get_int32(&i, cfg, "core.dummy2"));
	cl_assert_equal_i(7, i);

	git_config_free(cfg);
}

/*
 * At the beginning of the test:
 *  - config9 has: core.global does not exist
 *  - config15 has: core.global=17
 *  - config16 has: core.global=29
 *
 * And also:
 *  - config9 has: core.system does not exist
 *  - config15 has: core.system does not exist
 *  - config16 has: core.system=11
 */
void test_config_read__fallback_from_local_to_global_and_from_global_to_system(void)
{
	git_config *cfg;
	int32_t i;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config9"),
		GIT_CONFIG_LEVEL_SYSTEM, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config15"),
		GIT_CONFIG_LEVEL_GLOBAL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config16"),
		GIT_CONFIG_LEVEL_LOCAL, 0));

	cl_git_pass(git_config_get_int32(&i, cfg, "core.global"));
	cl_assert_equal_i(17, i);
	cl_git_pass(git_config_get_int32(&i, cfg, "core.system"));
	cl_assert_equal_i(11, i);

	git_config_free(cfg);
}

/*
 * At the beginning of the test, config18 has:
 *	int32global = 28
 *	int64global = 9223372036854775803
 *	boolglobal = true
 *	stringglobal = I'm a global config value!
 *
 * And config19 has:
 *	int32global = -1
 *	int64global = -2
 *	boolglobal = false
 *	stringglobal = don't find me!
 *
 */
void test_config_read__simple_read_from_specific_level(void)
{
	git_config *cfg, *cfg_specific;
	int i;
	int64_t l, expected = +9223372036854775803;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config18"),
		GIT_CONFIG_LEVEL_GLOBAL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config19"),
		GIT_CONFIG_LEVEL_SYSTEM, 0));

	cl_git_pass(git_config_open_level(&cfg_specific, cfg, GIT_CONFIG_LEVEL_GLOBAL));

	cl_git_pass(git_config_get_int32(&i, cfg_specific, "core.int32global"));
	cl_assert_equal_i(28, i);
	cl_git_pass(git_config_get_int64(&l, cfg_specific, "core.int64global"));
	cl_assert(l == expected);
	cl_git_pass(git_config_get_bool(&i, cfg_specific, "core.boolglobal"));
	cl_assert_equal_b(true, i);
	cl_git_pass(git_config_get_string_buf(&buf, cfg_specific, "core.stringglobal"));
	cl_assert_equal_s("I'm a global config value!", git_buf_cstr(&buf));

	git_config_free(cfg_specific);
	git_config_free(cfg);
}

void test_config_read__can_load_and_parse_an_empty_config_file(void)
{
	git_config *cfg;
	int i;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));
	cl_assert_equal_i(GIT_ENOTFOUND, git_config_get_int32(&i, cfg, "nope.neither"));

	git_config_free(cfg);
}

void test_config_read__corrupt_header(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[sneaky ] \"quoted closing quote mark\\\"");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	git_config_free(cfg);
}

void test_config_read__corrupt_header2(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[unclosed \"bracket\"\n    lib = git2\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	git_config_free(cfg);
}

void test_config_read__corrupt_header3(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[unclosed \"slash\\\"]\n    lib = git2\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	git_config_free(cfg);
}

void test_config_read__invalid_key_chars(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[foo]\n    has_underscore = git2\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	cl_git_rewritefile("./testconfig", "[foo]\n  has/slash = git2\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	cl_git_rewritefile("./testconfig", "[foo]\n  has+plus = git2\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	cl_git_rewritefile("./testconfig", "[no_key]\n  = git2\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	git_config_free(cfg);
}

void test_config_read__lone_variable_with_trailing_whitespace(void)
{
	git_config *cfg;
	int b;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[foo]\n    lonevariable   \n");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));

	cl_git_pass(git_config_get_bool(&b, cfg, "foo.lonevariable"));
	cl_assert_equal_b(true, b);

	git_config_free(cfg);
}

void test_config_read__override_variable(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[some] var = one\nvar = two");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.var"));
	cl_assert_equal_s("two", git_buf_cstr(&buf));

	git_config_free(cfg);
}

void test_config_read__path(void)
{
	git_config *cfg;
	git_buf path = GIT_BUF_INIT;
	git_buf old_path = GIT_BUF_INIT;
	git_buf home_path = GIT_BUF_INIT;
	git_buf expected_path = GIT_BUF_INIT;

	cl_git_pass(p_mkdir("fakehome", 0777));
	cl_git_pass(git_path_prettify(&home_path, "fakehome", NULL));
	cl_git_pass(git_libgit2_opts(GIT_OPT_GET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL, &old_path));
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL, home_path.ptr));
	cl_git_mkfile("./testconfig", "[some]\n path = ~/somefile");
	cl_git_pass(git_path_join_unrooted(&expected_path, "somefile", home_path.ptr, NULL));

	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));
	cl_git_pass(git_config_get_path(&path, cfg, "some.path"));
	cl_assert_equal_s(expected_path.ptr, path.ptr);
	git_buf_free(&path);

	cl_git_mkfile("./testconfig", "[some]\n path = ~/");
	cl_git_pass(git_path_join_unrooted(&expected_path, "", home_path.ptr, NULL));

	cl_git_pass(git_config_get_path(&path, cfg, "some.path"));
	cl_assert_equal_s(expected_path.ptr, path.ptr);
	git_buf_free(&path);

	cl_git_mkfile("./testconfig", "[some]\n path = ~");
	cl_git_pass(git_buf_sets(&expected_path, home_path.ptr));

	cl_git_pass(git_config_get_path(&path, cfg, "some.path"));
	cl_assert_equal_s(expected_path.ptr, path.ptr);
	git_buf_free(&path);

	cl_git_mkfile("./testconfig", "[some]\n path = ~user/foo");
	cl_git_fail(git_config_get_path(&path, cfg, "some.path"));

	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL, old_path.ptr));
	git_buf_free(&old_path);
	git_buf_free(&home_path);
	git_buf_free(&expected_path);
	git_config_free(cfg);
}
