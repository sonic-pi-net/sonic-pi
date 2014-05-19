#include "clar_libgit2.h"

void test_config_write__initialize(void)
{
	cl_fixture_sandbox("config/config9");
	cl_fixture_sandbox("config/config15");
	cl_fixture_sandbox("config/config17");
}

void test_config_write__cleanup(void)
{
	cl_fixture_cleanup("config9");
	cl_fixture_cleanup("config15");
	cl_fixture_cleanup("config17");
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
		GIT_CONFIG_LEVEL_LOCAL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, "config15",
		GIT_CONFIG_LEVEL_GLOBAL, 0));

	cl_git_pass(git_config_open_level(&cfg_specific, cfg, GIT_CONFIG_LEVEL_GLOBAL));

	cl_git_pass(git_config_delete_entry(cfg_specific, "core.dummy2"));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config15"));
	cl_assert(git_config_get_int32(&i, cfg, "core.dummy2") == GIT_ENOTFOUND);
	cl_git_pass(git_config_set_int32(cfg, "core.dummy2", 7));

	git_config_free(cfg_specific);
	git_config_free(cfg);
}

void test_config_write__write_subsection(void)
{
	git_config *cfg;
	const char *str;

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_string(cfg, "my.own.var", "works"));
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_get_string(&str, cfg, "my.own.var"));
	cl_assert_equal_s("works", str);
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
	const char* str;

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_string(cfg, "core.somevar", "this \"has\" quotes"));
	cl_git_pass(git_config_get_string(&str, cfg, "core.somevar"));
	cl_assert_equal_s(str, "this \"has\" quotes");
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_get_string(&str, cfg, "core.somevar"));
	cl_assert_equal_s(str, "this \"has\" quotes");
	git_config_free(cfg);

	/* The code path for values that already exist is different, check that one as well */
	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_string(cfg, "core.somevar", "this also \"has\" quotes"));
	cl_git_pass(git_config_get_string(&str, cfg, "core.somevar"));
	cl_assert_equal_s(str, "this also \"has\" quotes");
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_get_string(&str, cfg, "core.somevar"));
	cl_assert_equal_s(str, "this also \"has\" quotes");
	git_config_free(cfg);
}

void test_config_write__escape_value(void)
{
	git_config *cfg;
	const char* str;

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_set_string(cfg, "core.somevar", "this \"has\" quotes and \t"));
	cl_git_pass(git_config_get_string(&str, cfg, "core.somevar"));
	cl_assert_equal_s(str, "this \"has\" quotes and \t");
	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config9"));
	cl_git_pass(git_config_get_string(&str, cfg, "core.somevar"));
	cl_assert_equal_s(str, "this \"has\" quotes and \t");
	git_config_free(cfg);
}

void test_config_write__add_value_at_specific_level(void)
{
	git_config *cfg, *cfg_specific;
	int i;
	int64_t l, expected = +9223372036854775803;
	const char *s;

	// open config15 as global level config file
	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_add_file_ondisk(cfg, "config9",
		GIT_CONFIG_LEVEL_LOCAL, 0));
	cl_git_pass(git_config_add_file_ondisk(cfg, "config15",
		GIT_CONFIG_LEVEL_GLOBAL, 0));

	cl_git_pass(git_config_open_level(&cfg_specific, cfg, GIT_CONFIG_LEVEL_GLOBAL));

	cl_git_pass(git_config_set_int32(cfg_specific, "core.int32global", 28));
	cl_git_pass(git_config_set_int64(cfg_specific, "core.int64global", expected));
	cl_git_pass(git_config_set_bool(cfg_specific, "core.boolglobal", true));
	cl_git_pass(git_config_set_string(cfg_specific, "core.stringglobal", "I'm a global config value!"));
	git_config_free(cfg_specific);
	git_config_free(cfg);

	// open config15 as local level config file
	cl_git_pass(git_config_open_ondisk(&cfg, "config15"));

	cl_git_pass(git_config_get_int32(&i, cfg, "core.int32global"));
	cl_assert_equal_i(28, i);
	cl_git_pass(git_config_get_int64(&l, cfg, "core.int64global"));
	cl_assert(l == expected);
	cl_git_pass(git_config_get_bool(&i, cfg, "core.boolglobal"));
	cl_assert_equal_b(true, i);
	cl_git_pass(git_config_get_string(&s, cfg, "core.stringglobal"));
	cl_assert_equal_s("I'm a global config value!", s);

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

void test_config_write__add_value_which_needs_quotes(void)
{
	git_config *cfg;
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

	cl_git_pass(git_config_open_ondisk(&cfg, "config17"));
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
	const char * str;

	repository = cl_git_sandbox_init("testrepo.git");
	cl_git_pass(git_repository_config(&config, repository));

	cl_git_pass(git_config_set_string(config, "core.somevar", ""));
	cl_git_pass(git_config_get_string(&str, config, "core.somevar"));
	cl_assert_equal_s(str, "");

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

	cl_git_pass(git_config_refresh(cfg));

	cl_git_pass(git_config_get_int32(&tmp, cfg, "old.value"));
	cl_assert_equal_i(6, tmp);

	git_config_free(cfg);
}
