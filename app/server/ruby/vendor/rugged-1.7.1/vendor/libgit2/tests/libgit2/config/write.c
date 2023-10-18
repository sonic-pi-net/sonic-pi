#include "clar_libgit2.h"
#include "futils.h"
#include "git2/sys/config.h"
#include "config.h"

void test_config_write__initialize(void)
{
	cl_fixture_sandbox("config/config9");
	cl_fixture_sandbox("config/config15");
	cl_fixture_sandbox("config/config17");
	cl_fixture_sandbox("config/config22");
}

void test_config_write__cleanup(void)
{
	cl_fixture_cleanup("config9");
	cl_fixture_cleanup("config15");
	cl_fixture_cleanup("config17");
	cl_fixture_cleanup("config22");
}

void test_config_write__replace_value(void)
{
	git_config *cfg;
	int i;
	int64_t l, expected = +9223372036854775803;

	/* By freeing the config, we make sure we flush the values  */
	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_int32(cfg, "core.dummy", 5));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_get_int32(&i, cfg, "core.dummy"));
	cl_assert(i == 5);
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_int32(cfg, "core.dummy", 1));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_int64(cfg, "core.verylong", expected));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_get_int64(&l, cfg, "core.verylong"));
	cl_assert(l == expected);
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_must_fail(git_config_get_int32(&i, cfg, "core.verylong"));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_int64(cfg, "core.verylong", 1));
	git_config_free(cfg);
}

void test_config_write__delete_value(void)
{
	git_config *cfg;
	int32_t i;

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_int32(cfg, "core.dummy", 5));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_delete_entry(cfg, "core.dummy"));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_assert(git_config_get_int32(&i, cfg, "core.dummy") == GIT_ENOTFOUND);
	cl_git_pass(git_config_set_int32(cfg, "core.dummy", 1));
	git_config_free(cfg);
}

/*
 * At the beginning of the test:
 *  - config9 has: core.dummy2=42
 *  - config15 has: core.dummy2=7
 */
void test_config_write__delete_value_at_specific_level(void)
{
	git_config *cfg, *cfg_specific;
	int32_t i;

	cl_git_pass(git_config_open_ondisk(&cfg, "config15"));
	cl_git_pass(git_config_get_int32(&i, cfg, "core.dummy2"));
	cl_assert(i == 7);
	git_config_free(cfg);

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, "config9",
		GIT_CONFIG_LEVEL_LOCAL, NULL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, "config15",
		GIT_CONFIG_LEVEL_GLOBAL, NULL, 0));

	cl_git_pass(git_config_open_level(&cfg_specific, cfg, GIT_CONFIG_LEVEL_GLOBAL));

	cl_git_pass(git_config_delete_entry(cfg_specific, "core.dummy2"));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config15"));
	cl_assert(git_config_get_int32(&i, cfg, "core.dummy2") == GIT_ENOTFOUND);
	cl_git_pass(git_config_set_int32(cfg, "core.dummy2", 7));

	git_config_free(cfg_specific);
	git_config_free(cfg);
}

/*
 * This test exposes a bug where duplicate empty section headers could prevent
 * deletion of config entries.
 */
void test_config_write__delete_value_with_duplicate_header(void)
{
	const char *file_name  = "config-duplicate-header";
	const char *entry_name = "remote.origin.url";
	git_config *cfg;
	git_config_entry *entry;

	/* This config can occur after removing and re-adding the origin remote */
	const char *file_content =
		"[remote \"origin\"]\n"		\
		"[branch \"master\"]\n"		\
		"	remote = \"origin\"\n"	\
		"[remote \"origin\"]\n"		\
		"	url = \"foo\"\n";

	/* Write the test config and make sure the expected entry exists */
	cl_git_mkfile(file_name, file_content);
	cl_git_pass(git_config_open_ondisk(&cfg, file_name));
	cl_git_pass(git_config_get_entry(&entry, cfg, entry_name));

	/* Delete that entry */
	cl_git_pass(git_config_delete_entry(cfg, entry_name));

	/* Reopen the file and make sure the entry no longer exists */
	git_config_entry_free(entry);
	git_config_free(cfg);
	cl_git_pass(git_config_open_ondisk(&cfg, file_name));
	cl_git_fail(git_config_get_entry(&entry, cfg, entry_name));

	/* Cleanup */
	git_config_entry_free(entry);
	git_config_free(cfg);
}

/*
 * This test exposes a bug where duplicate section headers could cause
 * config_write to add a new entry when one already exists.
 */
void test_config_write__add_value_with_duplicate_header(void)
{
	const char *file_name  = "config-duplicate-insert";
	const char *entry_name = "foo.c";
	const char *old_val    = "old";
	const char *new_val    = "new";
	const char *str;
	git_config *cfg, *snapshot;

	/* c = old should be replaced by c = new.
	 * The bug causes c = new to be inserted under the first 'foo' header.
	 */
	const char *file_content =
		"[foo]\n"   \
		"  a = b\n" \
		"[other]\n" \
		"  a = b\n" \
		"[foo]\n"   \
		"  c = old\n";

	/* Write the test config */
	cl_git_mkfile(file_name, file_content);
	cl_git_pass(git_config_open_ondisk(&cfg, file_name));

	/* make sure the expected entry (foo.c) exists */
	cl_git_pass(git_config_snapshot(&snapshot, cfg));
	cl_git_pass(git_config_get_string(&str, snapshot, entry_name));
	cl_assert_equal_s(old_val, str);
	git_config_free(snapshot);

	/* Try setting foo.c to something else */
	cl_git_pass(git_config_set_string(cfg, entry_name, new_val));
	git_config_free(cfg);

	/* Reopen the file and make sure the new value was set */
	cl_git_pass(git_config_open_ondisk(&cfg, file_name));
	cl_git_pass(git_config_snapshot(&snapshot, cfg));
	cl_git_pass(git_config_get_string(&str, snapshot, entry_name));
	cl_assert_equal_s(new_val, str);

	/* Cleanup */
	git_config_free(snapshot);
	git_config_free(cfg);
}

void test_config_write__overwrite_value_with_duplicate_header(void)
{
	const char *file_name  = "config-duplicate-header";
	const char *entry_name = "remote.origin.url";
	git_config *cfg;
	git_config_entry *entry;

	/* This config can occur after removing and re-adding the origin remote */
	const char *file_content =
		"[remote \"origin\"]\n"		\
		"[branch \"master\"]\n"		\
		"	remote = \"origin\"\n"	\
		"[remote \"origin\"]\n"		\
		"	url = \"foo\"\n";

	/* Write the test config and make sure the expected entry exists */
	cl_git_mkfile(file_name, file_content);
	cl_git_pass(git_config_open_ondisk(&cfg, file_name));
	cl_git_pass(git_config_get_entry(&entry, cfg, entry_name));

	/* Update that entry */
	cl_git_pass(git_config_set_string(cfg, entry_name, "newurl"));

	/* Reopen the file and make sure the entry was updated */
	git_config_entry_free(entry);
	git_config_free(cfg);
	cl_git_pass(git_config_open_ondisk(&cfg, file_name));
	cl_git_pass(git_config_get_entry(&entry, cfg, entry_name));

	cl_assert_equal_s("newurl", entry->value);

	/* Cleanup */
	git_config_entry_free(entry);
	git_config_free(cfg);
}

static int multivar_cb(const git_config_entry *entry, void *data)
{
	int *n = (int *)data;

	cl_assert_equal_s(entry->value, "newurl");

	(*n)++;

	return 0;
}

void test_config_write__overwrite_multivar_within_duplicate_header(void)
{
	const char *file_name  = "config-duplicate-header";
	const char *entry_name = "remote.origin.url";
	git_config *cfg;
	git_config_entry *entry;
	int n = 0;

	/* This config can occur after removing and re-adding the origin remote */
	const char *file_content =
		"[remote \"origin\"]\n"		\
		"	url = \"bar\"\n"		\
		"[branch \"master\"]\n"		\
		"	remote = \"origin\"\n"	\
		"[remote \"origin\"]\n"		\
		"	url = \"foo\"\n";

	/* Write the test config and make sure the expected entry exists */
	cl_git_mkfile(file_name, file_content);
	cl_git_pass(git_config_open_ondisk(&cfg, file_name));
	cl_git_pass(git_config_get_entry(&entry, cfg, entry_name));

	/* Update that entry */
	cl_git_pass(git_config_set_multivar(cfg, entry_name, ".*", "newurl"));
	git_config_entry_free(entry);
	git_config_free(cfg);

	/* Reopen the file and make sure the entry was updated */
	cl_git_pass(git_config_open_ondisk(&cfg, file_name));
	cl_git_pass(git_config_get_multivar_foreach(cfg, entry_name, NULL, multivar_cb, &n));
	cl_assert_equal_i(2, n);

	/* Cleanup */
	git_config_free(cfg);
}

void test_config_write__write_subsection(void)
{
	git_config *cfg;
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_string(cfg, "my.own.var", "works"));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "my.own.var"));
	cl_assert_equal_s("works", buf.ptr);

	git_buf_dispose(&buf);
	git_config_free(cfg);
}

void test_config_write__delete_inexistent(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_assert(git_config_delete_entry(cfg, "core.imaginary") == GIT_ENOTFOUND);
	git_config_free(cfg);
}

void test_config_write__value_containing_quotes(void)
{
	git_config *cfg;
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_string(cfg, "core.somevar", "this \"has\" quotes"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "core.somevar"));
	cl_assert_equal_s("this \"has\" quotes", buf.ptr);
	git_buf_dispose(&buf);
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "core.somevar"));
	cl_assert_equal_s("this \"has\" quotes", buf.ptr);
	git_buf_dispose(&buf);
	git_config_free(cfg);

	/* The code path for values that already exist is different, check that one as well */
	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_string(cfg, "core.somevar", "this also \"has\" quotes"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "core.somevar"));
	cl_assert_equal_s("this also \"has\" quotes", buf.ptr);
	git_buf_dispose(&buf);
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "core.somevar"));
	cl_assert_equal_s("this also \"has\" quotes", buf.ptr);
	git_buf_dispose(&buf);
	git_config_free(cfg);
}

void test_config_write__escape_value(void)
{
	git_config *cfg;
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_string(cfg, "core.somevar", "this \"has\" quotes and \t"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "core.somevar"));
	cl_assert_equal_s("this \"has\" quotes and \t", buf.ptr);
	git_buf_dispose(&buf);
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "core.somevar"));
	cl_assert_equal_s("this \"has\" quotes and \t", buf.ptr);
	git_buf_dispose(&buf);
	git_config_free(cfg);
}

void test_config_write__add_value_at_specific_level(void)
{
	git_config *cfg, *cfg_specific;
	int i;
	int64_t l, expected = +9223372036854775803;
	git_buf buf = GIT_BUF_INIT;

	/* open config15 as global level config file */
	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, "config9",
		GIT_CONFIG_LEVEL_LOCAL, NULL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, "config15",
		GIT_CONFIG_LEVEL_GLOBAL, NULL, 0));

	cl_git_pass(git_config_open_level(&cfg_specific, cfg, GIT_CONFIG_LEVEL_GLOBAL));

	cl_git_pass(git_config_set_int32(cfg_specific, "core.int32global", 28));
	cl_git_pass(git_config_set_int64(cfg_specific, "core.int64global", expected));
	cl_git_pass(git_config_set_bool(cfg_specific, "core.boolglobal", true));
	cl_git_pass(git_config_set_string(cfg_specific, "core.stringglobal", "I'm a global config value!"));
	git_config_free(cfg_specific);
	git_config_free(cfg);

	/* open config15 as local level config file */
	cl_git_pass(git_config_open_ondisk(&cfg, "config15"));

	cl_git_pass(git_config_get_int32(&i, cfg, "core.int32global"));
	cl_assert_equal_i(28, i);
	cl_git_pass(git_config_get_int64(&l, cfg, "core.int64global"));
	cl_assert(l == expected);
	cl_git_pass(git_config_get_bool(&i, cfg, "core.boolglobal"));
	cl_assert_equal_b(true, i);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "core.stringglobal"));
	cl_assert_equal_s("I'm a global config value!", buf.ptr);

	git_buf_dispose(&buf);
	git_config_free(cfg);
}

void test_config_write__add_value_at_file_with_no_clrf_at_the_end(void)
{
	git_config *cfg;
	int i;

	cl_git_pass(git_config_open_ondisk(&cfg, "config17"));
	cl_git_pass(git_config_set_int32(cfg, "core.newline", 7));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config17"));
	cl_git_pass(git_config_get_int32(&i, cfg, "core.newline"));
	cl_assert_equal_i(7, i);

	git_config_free(cfg);
}

void test_config_write__add_section_at_file_with_no_clrf_at_the_end(void)
{
	git_config *cfg;
	int i;

	cl_git_pass(git_config_open_ondisk(&cfg, "config17"));
	cl_git_pass(git_config_set_int32(cfg, "diff.context", 10));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config17"));
	cl_git_pass(git_config_get_int32(&i, cfg, "diff.context"));
	cl_assert_equal_i(10, i);

	git_config_free(cfg);
}

void test_config_write__add_value_which_needs_quotes(void)
{
	git_config *cfg, *base;
	const char* str1;
	const char* str2;
	const char* str3;
	const char* str4;
	const char* str5;

	cl_git_pass(git_config_open_ondisk(&cfg, "config17"));
	cl_git_pass(git_config_set_string(cfg, "core.startwithspace", " Something"));
	cl_git_pass(git_config_set_string(cfg, "core.endwithspace", "Something "));
	cl_git_pass(git_config_set_string(cfg, "core.containscommentchar1", "some#thing"));
	cl_git_pass(git_config_set_string(cfg, "core.containscommentchar2", "some;thing"));
	cl_git_pass(git_config_set_string(cfg, "core.startwhithsapceandcontainsdoublequote", " some\"thing"));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&base, "config17"));
	cl_git_pass(git_config_snapshot(&cfg, base));
	cl_git_pass(git_config_get_string(&str1, cfg, "core.startwithspace"));
	cl_assert_equal_s(" Something", str1);
	cl_git_pass(git_config_get_string(&str2, cfg, "core.endwithspace"));
	cl_assert_equal_s("Something ", str2);
	cl_git_pass(git_config_get_string(&str3, cfg, "core.containscommentchar1"));
	cl_assert_equal_s("some#thing", str3);
	cl_git_pass(git_config_get_string(&str4, cfg, "core.containscommentchar2"));
	cl_assert_equal_s("some;thing", str4);
	cl_git_pass(git_config_get_string(&str5, cfg, "core.startwhithsapceandcontainsdoublequote"));
	cl_assert_equal_s(" some\"thing", str5);
	git_config_free(cfg);
	git_config_free(base);
}

void test_config_write__can_set_a_value_to_NULL(void)
{
    git_repository *repository;
    git_config *config;

    repository = cl_git_sandbox_init("testrepo.git");

    cl_git_pass(git_repository_config(&config, repository));
    cl_git_fail(git_config_set_string(config, "a.b.c", NULL));
    git_config_free(config);

    cl_git_sandbox_cleanup();
}

void test_config_write__can_set_an_empty_value(void)
{
	git_repository *repository;
	git_config *config;
	git_buf buf = {0};

	repository = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_repository_config(&config, repository));

	cl_git_pass(git_config_set_string(config, "core.somevar", ""));
	cl_git_pass(git_config_get_string_buf(&buf, config, "core.somevar"));
	cl_assert_equal_s("", buf.ptr);

	git_buf_dispose(&buf);
	git_config_free(config);
	cl_git_sandbox_cleanup();
}

void test_config_write__updating_a_locked_config_file_returns_ELOCKED(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));

	cl_git_mkfile("config9.lock", "[core]\n");

	cl_git_fail_with(git_config_set_string(cfg, "core.dump", "boom"), GIT_ELOCKED);

	git_config_free(cfg);
}

void test_config_write__outside_change(void)
{
	int32_t tmp;
	git_config *cfg;
	const char *filename = "config-ext-change";

	cl_git_mkfile(filename, "[old]\nvalue = 5\n");

	cl_git_pass(git_config_open_ondisk(&cfg, filename));

	cl_git_pass(git_config_get_int32(&tmp, cfg, "old.value"));

	/* Change the value on the file itself (simulate external process) */
	cl_git_mkfile(filename, "[old]\nvalue = 6\n");

	cl_git_pass(git_config_set_int32(cfg, "new.value", 7));

	cl_git_pass(git_config_get_int32(&tmp, cfg, "old.value"));
	cl_assert_equal_i(6, tmp);

	git_config_free(cfg);
}

#define FOO_COMMENT \
	";  another comment!\n"

#define SECTION_FOO \
	"\n"                     \
	"    \n"                 \
	" [section \"foo\"]  \n" \
	" # here's a comment\n"  \
	"\tname = \"value\"\n"   \
	"  name2 = \"value2\"\n" \

#define SECTION_FOO_WITH_COMMENT SECTION_FOO FOO_COMMENT

#define SECTION_BAR \
	"[section \"bar\"]\t\n"  \
	"\t  \n"                 \
	" barname=\"value\"\n"


void test_config_write__preserves_whitespace_and_comments(void)
{
	const char *file_name  = "config-duplicate-header";
	const char *n;
	git_config *cfg;
	git_str newfile = GIT_STR_INIT;

	/* This config can occur after removing and re-adding the origin remote */
	const char *file_content = SECTION_FOO_WITH_COMMENT SECTION_BAR;

	/* Write the test config and make sure the expected entry exists */
	cl_git_mkfile(file_name, file_content);
	cl_git_pass(git_config_open_ondisk(&cfg, file_name));
	cl_git_pass(git_config_set_string(cfg, "section.foo.other", "otherval"));
	cl_git_pass(git_config_set_string(cfg, "newsection.newname", "new_value"));

	/* Ensure that we didn't needlessly mangle the config file */
	cl_git_pass(git_futils_readbuffer(&newfile, file_name));
	n = newfile.ptr;

	cl_assert_equal_strn(SECTION_FOO, n, strlen(SECTION_FOO));
	n += strlen(SECTION_FOO);
	cl_assert_equal_strn("\tother = otherval\n", n, strlen("\tother = otherval\n"));
	n += strlen("\tother = otherval\n");
	cl_assert_equal_strn(FOO_COMMENT, n, strlen(FOO_COMMENT));
	n += strlen(FOO_COMMENT);

	cl_assert_equal_strn(SECTION_BAR, n, strlen(SECTION_BAR));
	n += strlen(SECTION_BAR);

	cl_assert_equal_s("[newsection]\n\tnewname = new_value\n", n);

	git_str_dispose(&newfile);
	git_config_free(cfg);
}

void test_config_write__preserves_entry_with_name_only(void)
{
	const char *file_name  = "config-empty-value";
	git_config *cfg;
	git_str newfile = GIT_STR_INIT;

	/* Write the test config and make sure the expected entry exists */
	cl_git_mkfile(file_name, "[section \"foo\"]\n\tname\n");
	cl_git_pass(git_config_open_ondisk(&cfg, file_name));
	cl_git_pass(git_config_set_string(cfg, "newsection.newname", "new_value"));
	cl_git_pass(git_config_set_string(cfg, "section.foo.other", "otherval"));

	cl_git_pass(git_futils_readbuffer(&newfile, file_name));
	cl_assert_equal_s("[section \"foo\"]\n\tname\n\tother = otherval\n[newsection]\n\tnewname = new_value\n", newfile.ptr);

	git_str_dispose(&newfile);
	git_config_free(cfg);
}

void test_config_write__to_empty_file(void)
{
	git_config *cfg;
	const char *filename = "config-file";
	git_str result = GIT_STR_INIT;

	cl_git_mkfile(filename, "");
	cl_git_pass(git_config_open_ondisk(&cfg, filename));
	cl_git_pass(git_config_set_string(cfg, "section.name", "value"));
	git_config_free(cfg);

	cl_git_pass(git_futils_readbuffer(&result, "config-file"));
	cl_assert_equal_s("[section]\n\tname = value\n", result.ptr);

	git_str_dispose(&result);
}

void test_config_write__to_file_with_only_comment(void)
{
	git_config *cfg;
	const char *filename = "config-file";
	git_str result = GIT_STR_INIT;

	cl_git_mkfile(filename, "\n\n");
	cl_git_pass(git_config_open_ondisk(&cfg, filename));
	cl_git_pass(git_config_set_string(cfg, "section.name", "value"));
	git_config_free(cfg);

	cl_git_pass(git_futils_readbuffer(&result, "config-file"));
	cl_assert_equal_s("\n\n[section]\n\tname = value\n", result.ptr);

	git_str_dispose(&result);
}

void test_config_write__locking(void)
{
	git_config *cfg, *cfg2;
	git_config_entry *entry;
	git_transaction *tx;
	const char *filename = "locked-file";

	/* Open the config and lock it */
	cl_git_mkfile(filename, "[section]\n\tname = value\n");
	cl_git_pass(git_config_open_ondisk(&cfg, filename));
	cl_git_pass(git_config_get_entry(&entry, cfg, "section.name"));
	cl_assert_equal_s("value", entry->value);
	git_config_entry_free(entry);
	cl_git_pass(git_config_lock(&tx, cfg));

	/* Change entries in the locked backend */
	cl_git_pass(git_config_set_string(cfg, "section.name", "other value"));
	cl_git_pass(git_config_set_string(cfg, "section2.name3", "more value"));

	/* We can see that the file we read from hasn't changed */
	cl_git_pass(git_config_open_ondisk(&cfg2, filename));
	cl_git_pass(git_config_get_entry(&entry, cfg2, "section.name"));
	cl_assert_equal_s("value", entry->value);
	git_config_entry_free(entry);
	cl_git_fail_with(GIT_ENOTFOUND, git_config_get_entry(&entry, cfg2, "section2.name3"));
	git_config_free(cfg2);

	/* And we also get the old view when we read from the locked config */
	cl_git_pass(git_config_get_entry(&entry, cfg, "section.name"));
	cl_assert_equal_s("value", entry->value);
	git_config_entry_free(entry);
	cl_git_fail_with(GIT_ENOTFOUND, git_config_get_entry(&entry, cfg, "section2.name3"));

	cl_git_pass(git_transaction_commit(tx));
	git_transaction_free(tx);

	/* Now that we've unlocked it, we should see both updates */
	cl_git_pass(git_config_get_entry(&entry, cfg, "section.name"));
	cl_assert_equal_s("other value", entry->value);
	git_config_entry_free(entry);
	cl_git_pass(git_config_get_entry(&entry, cfg, "section2.name3"));
	cl_assert_equal_s("more value", entry->value);
	git_config_entry_free(entry);

	git_config_free(cfg);

	/* We should also see the changes after reopening the config */
	cl_git_pass(git_config_open_ondisk(&cfg, filename));
	cl_git_pass(git_config_get_entry(&entry, cfg, "section.name"));
	cl_assert_equal_s("other value", entry->value);
	git_config_entry_free(entry);
	cl_git_pass(git_config_get_entry(&entry, cfg, "section2.name3"));
	cl_assert_equal_s("more value", entry->value);
	git_config_entry_free(entry);

	git_config_free(cfg);
}

void test_config_write__repeated(void)
{
	const char *filename = "config-repeated";
	git_config *cfg;
	git_str result = GIT_STR_INIT;
	const char *expected = "[sample \"prefix\"]\n\
\tsetting1 = someValue1\n\
\tsetting2 = someValue2\n\
\tsetting3 = someValue3\n\
\tsetting4 = someValue4\n\
";
	cl_git_pass(git_config_open_ondisk(&cfg, filename));
	cl_git_pass(git_config_set_string(cfg, "sample.prefix.setting1", "someValue1"));
	cl_git_pass(git_config_set_string(cfg, "sample.prefix.setting2", "someValue2"));
	cl_git_pass(git_config_set_string(cfg, "sample.prefix.setting3", "someValue3"));
	cl_git_pass(git_config_set_string(cfg, "sample.prefix.setting4", "someValue4"));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, filename));

	cl_git_pass(git_futils_readbuffer(&result, filename));
	cl_assert_equal_s(expected, result.ptr);
	git_str_dispose(&result);

	git_config_free(cfg);
}

void test_config_write__preserve_case(void)
{
	const char *filename = "config-preserve-case";
	git_config *cfg;
	git_str result = GIT_STR_INIT;
	const char *expected = "[sOMe]\n" \
		"\tThInG = foo\n" \
		"\tOtheR = thing\n";

	cl_git_pass(git_config_open_ondisk(&cfg, filename));
	cl_git_pass(git_config_set_string(cfg, "sOMe.ThInG", "foo"));
	cl_git_pass(git_config_set_string(cfg, "SomE.OtheR", "thing"));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, filename));

	cl_git_pass(git_futils_readbuffer(&result, filename));
	cl_assert_equal_s(expected, result.ptr);
	git_str_dispose(&result);

	git_config_free(cfg);
}

void test_config_write__write_config_file_with_multi_line_value(void)
{
	git_config* cfg;
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_config_open_ondisk(&cfg, "config22"));
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "alias.m"));
	cl_assert_equal_s("cmd ;; ;; bar", buf.ptr);
	cl_git_pass(git_config_set_string(cfg, "sOMe.ThInG", "foo"));
	git_buf_dispose(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, cfg, "alias.m"));
	cl_assert_equal_s("cmd ;; ;; bar", buf.ptr);
	git_buf_dispose(&buf);

	git_config_free(cfg);
}
