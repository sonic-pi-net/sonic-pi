#include "clar_libgit2.h"
#include "fs_path.h"

static git_buf buf = GIT_BUF_INIT;

void test_config_read__cleanup(void)
{
	git_buf_dispose(&buf);
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
	cl_assert_equal_s("true", buf.ptr);
	git_buf_dispose(&buf);

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "this.That.other"));
	cl_assert_equal_s("yes", buf.ptr);

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
	cl_assert_equal_s("one one one two two three three", buf.ptr);

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
	cl_assert_equal_s("foo", buf.ptr);

	git_config_free(cfg);
}

void test_config_read__multiline_eof(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[header]\n  key1 = \\\n");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "header.key1"));
	cl_assert_equal_s("", buf.ptr);

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
	cl_assert_equal_s("hello", buf.ptr);

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
	cl_assert_equal_s("", buf.ptr);
	git_buf_dispose(&buf);

	cl_git_pass(git_config_get_bool(&i, cfg, "some.section.variable"));
	cl_assert(i == 1);

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.section.variableeq"));
	cl_assert_equal_s("", buf.ptr);

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
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "valid.[subsection].something"));
	cl_assert_equal_s("a", buf.ptr);
	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "sec.[subsec]/child.parent"));
	cl_assert_equal_s("grand", buf.ptr);
	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "sec2.[subsec2]/child2.type"));
	cl_assert_equal_s("dvcs", buf.ptr);
	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "sec3.escape\"quote.vcs"));
	cl_assert_equal_s("git", buf.ptr);
	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "sec4.escaping\\slash.lib"));
	cl_assert_equal_s("git2", buf.ptr);
	git_buf_dispose(&buf);
	git_config_free(cfg);
}

void test_config_read__multiline_multiple_quoted_comment_chars(void)
{
	git_config *cfg;
	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config21")));
	git_config_free(cfg);
}

void test_config_read__multiline_multiple_quoted_quote_at_beginning_of_line(void)
{
	git_config* cfg;
	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config22")));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "alias.m"));
	cl_assert_equal_s("cmd ;; ;; bar", buf.ptr);
	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "alias.m2"));
	cl_assert_equal_s("'; ; something '", buf.ptr);
	git_buf_dispose(&buf);
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
	cl_assert_equal_s("http://example.com/git/ab", buf.ptr);
	git_buf_dispose(&buf);

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "remote.abba.url"));
	cl_assert_equal_s("http://example.com/git/abba", buf.ptr);

	git_config_free(cfg);
}

void test_config_read__escaping_quotes(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config13")));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "core.editor"));
	cl_assert_equal_s("\"C:/Program Files/Nonsense/bah.exe\" \"--some option\"", buf.ptr);

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
		GIT_CONFIG_LEVEL_SYSTEM, NULL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config15"),
		GIT_CONFIG_LEVEL_GLOBAL, NULL, 0));

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
	const char *keys[] = {
		"core.dummy2",
		"core.verylong",
		"core.dummy",
		"remote.ab.url",
		"remote.abba.url",
		"core.dummy2",
		"core.global"
	};
	git_config *cfg;
	git_config_iterator *iter;
	git_config_entry *entry;
	int count, ret;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config9"),
		GIT_CONFIG_LEVEL_SYSTEM, NULL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config15"),
		GIT_CONFIG_LEVEL_GLOBAL, NULL, 0));

	count = 0;
	cl_git_pass(git_config_iterator_new(&iter, cfg));

	while ((ret = git_config_next(&entry, iter)) == 0) {
		cl_assert_equal_s(entry->name, keys[count]);
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
	cl_assert_equal_s("c", buf.ptr);
	git_buf_dispose(&buf);

	cl_git_pass(git_config_get_string_buf(&buf, cfg, "d.e"));
	cl_assert_equal_s("f", buf.ptr);

	git_config_free(cfg);
}

void test_config_read__read_git_config_entry(void)
{
	git_config *cfg;
	git_config_entry *entry;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config9"),
		GIT_CONFIG_LEVEL_SYSTEM, NULL, 0));

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
		GIT_CONFIG_LEVEL_SYSTEM, NULL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config15"),
		GIT_CONFIG_LEVEL_GLOBAL, NULL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config16"),
		GIT_CONFIG_LEVEL_LOCAL, NULL, 0));

	cl_git_pass(git_config_get_int32(&i, cfg, "core.dummy2"));
	cl_assert_equal_i(28, i);

	git_config_free(cfg);

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config9"),
		GIT_CONFIG_LEVEL_SYSTEM, NULL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config15"),
		GIT_CONFIG_LEVEL_GLOBAL, NULL, 0));

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
		GIT_CONFIG_LEVEL_SYSTEM, NULL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config15"),
		GIT_CONFIG_LEVEL_GLOBAL, NULL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config16"),
		GIT_CONFIG_LEVEL_LOCAL, NULL, 0));

	cl_git_pass(git_config_get_int32(&i, cfg, "core.global"));
	cl_assert_equal_i(17, i);
	cl_git_pass(git_config_get_int32(&i, cfg, "core.system"));
	cl_assert_equal_i(11, i);

	git_config_free(cfg);
}

void test_config_read__parent_dir_is_file(void)
{
	git_config *cfg;
	int count;

	cl_git_pass(git_config_new(&cfg));
	/*
	 * Verify we can add non-existing files when the parent directory is not
	 * a directory.
	 */
	cl_git_pass(git_config_add_file_ondisk(cfg, "/dev/null/.gitconfig",
		GIT_CONFIG_LEVEL_SYSTEM, NULL, 0));

	count = 0;
	cl_git_pass(git_config_foreach(cfg, count_cfg_entries_and_compare_levels, &count));
	cl_assert_equal_i(0, count);

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
		GIT_CONFIG_LEVEL_GLOBAL, NULL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, cl_fixture("config/config19"),
		GIT_CONFIG_LEVEL_SYSTEM, NULL, 0));

	cl_git_pass(git_config_open_level(&cfg_specific, cfg, GIT_CONFIG_LEVEL_GLOBAL));

	cl_git_pass(git_config_get_int32(&i, cfg_specific, "core.int32global"));
	cl_assert_equal_i(28, i);
	cl_git_pass(git_config_get_int64(&l, cfg_specific, "core.int64global"));
	cl_assert(l == expected);
	cl_git_pass(git_config_get_bool(&i, cfg_specific, "core.boolglobal"));
	cl_assert_equal_b(true, i);
	cl_git_pass(git_config_get_string_buf(&buf, cfg_specific, "core.stringglobal"));
	cl_assert_equal_s("I'm a global config value!", buf.ptr);

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
	cl_assert_equal_s("two", buf.ptr);

	git_config_free(cfg);
}

void test_config_read__path(void)
{
	git_config *cfg;
	git_buf path = GIT_BUF_INIT;
	git_str home_path = GIT_STR_INIT;
	git_str expected_path = GIT_STR_INIT;

	cl_fake_homedir(&home_path);

	cl_git_mkfile("./testconfig", "[some]\n path = ~/somefile");
	cl_git_pass(git_fs_path_join_unrooted(&expected_path, "somefile", home_path.ptr, NULL));

	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));
	cl_git_pass(git_config_get_path(&path, cfg, "some.path"));
	cl_assert_equal_s(expected_path.ptr, path.ptr);
	git_buf_dispose(&path);

	cl_git_mkfile("./testconfig", "[some]\n path = ~/");
	cl_git_pass(git_fs_path_join_unrooted(&expected_path, "", home_path.ptr, NULL));

	cl_git_pass(git_config_get_path(&path, cfg, "some.path"));
	cl_assert_equal_s(expected_path.ptr, path.ptr);
	git_buf_dispose(&path);

	cl_git_mkfile("./testconfig", "[some]\n path = ~");
	cl_git_pass(git_str_sets(&expected_path, home_path.ptr));

	cl_git_pass(git_config_get_path(&path, cfg, "some.path"));
	cl_assert_equal_s(expected_path.ptr, path.ptr);
	git_buf_dispose(&path);

	cl_git_mkfile("./testconfig", "[some]\n path = ~user/foo");
	cl_git_fail(git_config_get_path(&path, cfg, "some.path"));

	git_str_dispose(&home_path);
	git_str_dispose(&expected_path);
	git_config_free(cfg);
}

void test_config_read__crlf_style_line_endings(void)
{
	git_buf buf = GIT_BUF_INIT;
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[some]\r\n var = value\r\n");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.var"));
	cl_assert_equal_s(buf.ptr, "value");

	git_config_free(cfg);
	git_buf_dispose(&buf);
}

void test_config_read__trailing_crlf(void)
{
	git_buf buf = GIT_BUF_INIT;
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[some]\r\n var = value\r\n\r\n");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.var"));
	cl_assert_equal_s(buf.ptr, "value");

	git_config_free(cfg);
	git_buf_dispose(&buf);
}

void test_config_read__bom(void)
{
	git_buf buf = GIT_BUF_INIT;
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "\xEF\xBB\xBF[some]\n var = value\n");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.var"));
	cl_assert_equal_s(buf.ptr, "value");

	git_config_free(cfg);
	git_buf_dispose(&buf);
}

void test_config_read__arbitrary_whitespace_before_subsection(void)
{
	git_buf buf = GIT_BUF_INIT;
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[some \t \"subsection\"]\n var = value\n");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.subsection.var"));
	cl_assert_equal_s(buf.ptr, "value");

	git_config_free(cfg);
	git_buf_dispose(&buf);
}

void test_config_read__no_whitespace_after_subsection(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[some \"subsection\" ]\n var = value\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	git_config_free(cfg);
}

void test_config_read__invalid_space_section(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "\xEF\xBB\xBF[some section]\n var = value\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	git_config_free(cfg);
}

void test_config_read__invalid_quoted_first_section(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "\xEF\xBB\xBF[\"some\"]\n var = value\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	git_config_free(cfg);
}

void test_config_read__invalid_unquoted_subsection(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "\xEF\xBB\xBF[some sub section]\n var = value\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	git_config_free(cfg);
}

void test_config_read__invalid_quoted_third_section(void)
{
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "\xEF\xBB\xBF[some sub \"section\"]\n var = value\n");
	cl_git_fail(git_config_open_ondisk(&cfg, "./testconfig"));

	git_config_free(cfg);
}

void test_config_read__unreadable_file_ignored(void)
{
	git_buf buf = GIT_BUF_INIT;
	git_config *cfg;
	int ret;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[some] var = value\n[some \"OtheR\"] var = value");
	cl_git_pass(p_chmod("./testconfig", 0));

	ret = git_config_open_ondisk(&cfg, "./test/config");
	cl_assert(ret == 0 || ret == GIT_ENOTFOUND);

	git_config_free(cfg);
	git_buf_dispose(&buf);
}

void test_config_read__single_line(void)
{
	git_buf buf = GIT_BUF_INIT;
	git_config *cfg;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[some] var = value\n[some \"OtheR\"] var = value");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.var"));
	cl_assert_equal_s(buf.ptr, "value");

	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.OtheR.var"));
	cl_assert_equal_s(buf.ptr, "value");

	git_config_free(cfg);
	cl_git_mkfile("./testconfig", "[some] var = value\n[some \"OtheR\"]var = value");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));
	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.var"));
	cl_assert_equal_s(buf.ptr, "value");

	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "some.OtheR.var"));
	cl_assert_equal_s(buf.ptr, "value");

	git_config_free(cfg);
	git_buf_dispose(&buf);
}

static int read_nosection_cb(const git_config_entry *entry, void *payload) {
	int *seen = (int*)payload;
	if (strcmp(entry->name, "key") == 0) {
		(*seen)++;
	}
	return 0;
}

/* This would ideally issue a warning, if we had a way to do so. */
void test_config_read__nosection(void)
{
	git_config *cfg;
	git_buf buf = GIT_BUF_INIT;
	int seen = 0;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config-nosection")));

	/*
	 * Given a key with no section, we do not allow reading it,
	 * but we do include it in an iteration over the config
	 * store. This appears to match how git's own APIs (and
	 * git-config(1)) behave.
	 */

	cl_git_fail_with(git_config_get_string_buf(&buf, cfg, "key"), GIT_EINVALIDSPEC);

	cl_git_pass(git_config_foreach(cfg, read_nosection_cb, &seen));
	cl_assert_equal_i(seen, 1);

	git_buf_dispose(&buf);
	git_config_free(cfg);
}

enum {
	MAP_TRUE = 0,
	MAP_FALSE = 1,
	MAP_ALWAYS = 2
};

static git_configmap _test_map1[] = {
	{GIT_CONFIGMAP_STRING, "always", MAP_ALWAYS},
	{GIT_CONFIGMAP_FALSE, NULL, MAP_FALSE},
	{GIT_CONFIGMAP_TRUE, NULL, MAP_TRUE},
};

static git_configmap _test_map2[] = {
	{GIT_CONFIGMAP_INT32, NULL, 0},
};

void test_config_read__get_mapped(void)
{
	git_config *cfg;
	int val;
	int known_good;

	cl_set_cleanup(&clean_test_config, NULL);
	cl_git_mkfile("./testconfig", "[header]\n"
								  "  key1 = 1\n"
								  "  key2 = true\n"
								  "  key3\n"
								  "  key4 = always\n"
								  "  key5 = false\n"
								  "  key6 = 0\n"
								  "  key7 = never\n"
								  "  key8 = On\n"
								  "  key9 = off\n");
	cl_git_pass(git_config_open_ondisk(&cfg, "./testconfig"));

	/* check parsing bool and string */
	cl_git_pass(git_config_get_mapped(&val, cfg, "header.key1", _test_map1, ARRAY_SIZE(_test_map1)));
	cl_assert_equal_i(val, MAP_TRUE);
	cl_git_pass(git_config_get_mapped(&val, cfg, "header.key2", _test_map1, ARRAY_SIZE(_test_map1)));
	cl_assert_equal_i(val, MAP_TRUE);
	cl_git_pass(git_config_get_mapped(&val, cfg, "header.key3", _test_map1, ARRAY_SIZE(_test_map1)));
	cl_assert_equal_i(val, MAP_TRUE);
	cl_git_pass(git_config_get_mapped(&val, cfg, "header.key8", _test_map1, ARRAY_SIZE(_test_map1)));
	cl_assert_equal_i(val, MAP_TRUE);

	cl_git_pass(git_config_get_mapped(&val, cfg, "header.key4", _test_map1, ARRAY_SIZE(_test_map1)));
	cl_assert_equal_i(val, MAP_ALWAYS);

	cl_git_pass(git_config_get_mapped(&val, cfg, "header.key5", _test_map1, ARRAY_SIZE(_test_map1)));
	cl_assert_equal_i(val, MAP_FALSE);
	cl_git_pass(git_config_get_mapped(&val, cfg, "header.key6", _test_map1, ARRAY_SIZE(_test_map1)));
	cl_assert_equal_i(val, MAP_FALSE);
	cl_git_pass(git_config_get_mapped(&val, cfg, "header.key9", _test_map1, ARRAY_SIZE(_test_map1)));
	cl_assert_equal_i(val, MAP_FALSE);

	cl_git_fail(git_config_get_mapped(&val, cfg, "header.key7", _test_map1, ARRAY_SIZE(_test_map1)));

	/* check parsing int values */
	cl_git_pass(git_config_get_mapped(&val, cfg, "header.key1", _test_map2, ARRAY_SIZE(_test_map2)));
	cl_git_pass(git_config_get_int32(&known_good, cfg, "header.key1"));
	cl_assert_equal_i(val, known_good);
	cl_git_pass(git_config_get_mapped(&val, cfg, "header.key6", _test_map2, ARRAY_SIZE(_test_map2)));
	cl_git_pass(git_config_get_int32(&known_good, cfg, "header.key6"));
	cl_assert_equal_i(val, known_good);

	cl_git_fail(git_config_get_mapped(&val, cfg, "header.key2", _test_map2, ARRAY_SIZE(_test_map2)));
	cl_git_fail(git_config_get_mapped(&val, cfg, "header.key3", _test_map2, ARRAY_SIZE(_test_map2)));
	cl_git_fail(git_config_get_mapped(&val, cfg, "header.key4", _test_map2, ARRAY_SIZE(_test_map2)));
	cl_git_fail(git_config_get_mapped(&val, cfg, "header.key5", _test_map2, ARRAY_SIZE(_test_map2)));
	cl_git_fail(git_config_get_mapped(&val, cfg, "header.key7", _test_map2, ARRAY_SIZE(_test_map2)));
	cl_git_fail(git_config_get_mapped(&val, cfg, "header.key8", _test_map2, ARRAY_SIZE(_test_map2)));
	cl_git_fail(git_config_get_mapped(&val, cfg, "header.key9", _test_map2, ARRAY_SIZE(_test_map2)));

	git_config_free(cfg);
}
