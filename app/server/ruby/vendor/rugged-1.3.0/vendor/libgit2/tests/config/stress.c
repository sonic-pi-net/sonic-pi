#include "clar_libgit2.h"

#include "filebuf.h"
#include "futils.h"
#include "posix.h"

#define TEST_CONFIG "git-test-config"

static git_buf buf = GIT_BUF_INIT;

void test_config_stress__initialize(void)
{
	git_filebuf file = GIT_FILEBUF_INIT;

	cl_git_pass(git_filebuf_open(&file, TEST_CONFIG, 0, 0666));

	git_filebuf_printf(&file, "[color]\n\tui = auto\n");
	git_filebuf_printf(&file, "[core]\n\teditor = \n");

	cl_git_pass(git_filebuf_commit(&file));
}

void test_config_stress__cleanup(void)
{
	git_buf_dispose(&buf);
	p_unlink(TEST_CONFIG);
}

void test_config_stress__dont_break_on_invalid_input(void)
{
	git_config *config;

	cl_assert(git_path_exists(TEST_CONFIG));
	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));

	cl_git_pass(git_config_get_string_buf(&buf, config, "color.ui"));
	cl_git_pass(git_config_get_string_buf(&buf, config, "core.editor"));

	git_config_free(config);
}

void assert_config_value(git_config *config, const char *key, const char *value)
{
	git_buf_clear(&buf);
	cl_git_pass(git_config_get_string_buf(&buf, config, key));
	cl_assert_equal_s(value, git_buf_cstr(&buf));
}

void test_config_stress__comments(void)
{
	git_config *config;

	cl_git_pass(git_config_open_ondisk(&config, cl_fixture("config/config12")));

	assert_config_value(config, "some.section.test2", "hello");
	assert_config_value(config, "some.section.test3", "welcome");
	assert_config_value(config, "some.section.other", "hello! \" ; ; ; ");
	assert_config_value(config, "some.section.other2", "cool! \" # # # ");
	assert_config_value(config, "some.section.multi", "hi, this is a ; multiline comment # with ;\n special chars and other stuff !@#");
	assert_config_value(config, "some.section.multi2", "good, this is a ; multiline comment # with ;\n special chars and other stuff !@#");
	assert_config_value(config, "some.section.back", "this is \ba phrase");
	assert_config_value(config, "some.section.dollar", "some $sign");
	assert_config_value(config, "some.section.multiquotes", "!ls  x                     ls  # comment2                     $HOME");
	assert_config_value(config, "some.section.multiquotes2", "!ls  x                      ls  \"# comment2                      $HOME\"");
	assert_config_value(config, "some.section.multiquotes3", "hi # ho there are # more quotes");
	assert_config_value(config, "some.section.quotecomment", "hi # ho there are # more");

	git_config_free(config);
}

void test_config_stress__escape_subsection_names(void)
{
	git_config *config;

	cl_assert(git_path_exists("git-test-config"));
	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));

	cl_git_pass(git_config_set_string(config, "some.sec\\tion.other", "foo"));
	git_config_free(config);

	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));

	assert_config_value(config, "some.sec\\tion.other", "foo");

	git_config_free(config);
}

void test_config_stress__trailing_backslash(void)
{
	git_config *config;
	const char *path =  "C:\\iam\\some\\windows\\path\\";

	cl_assert(git_path_exists("git-test-config"));
	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));
	cl_git_pass(git_config_set_string(config, "windows.path", path));
	git_config_free(config);

	cl_git_pass(git_config_open_ondisk(&config, TEST_CONFIG));
	assert_config_value(config, "windows.path", path);

	git_config_free(config);
}

void test_config_stress__complex(void)
{
	git_config *config;
	const char *path = "./config-immediate-multiline";

	cl_git_mkfile(path, "[imm]\n multi = \"\\\nfoo\"");
	cl_git_pass(git_config_open_ondisk(&config, path));
	assert_config_value(config, "imm.multi", "foo");

	git_config_free(config);
}

void test_config_stress__quick_write(void)
{
	git_config *config_w, *config_r;
	const char *path = "./config-quick-write";
	const char *key = "quick.write";
	int32_t i;

	/* Create an external writer for one instance with the other one */
	cl_git_pass(git_config_open_ondisk(&config_w, path));
	cl_git_pass(git_config_open_ondisk(&config_r, path));

	/* Write and read in the same second (repeat to increase the chance of it happening) */
	for (i = 0; i < 10; i++) {
		int32_t val;
		cl_git_pass(git_config_set_int32(config_w, key, i));
		cl_msleep(1);
		cl_git_pass(git_config_get_int32(&val, config_r, key));
		cl_assert_equal_i(i, val);
	}

	git_config_free(config_r);
	git_config_free(config_w);
}

static int foreach_cb(const git_config_entry *entry, void *payload)
{
	if (!strcmp(entry->name, "key.value")) {
		*(char **)payload = git__strdup(entry->value);
		return 0;
	}
	return -1;
}

void test_config_stress__foreach_refreshes(void)
{
	git_config *config_w, *config_r;
	char *value = NULL;

	cl_git_pass(git_config_open_ondisk(&config_w, "./cfg"));
	cl_git_pass(git_config_open_ondisk(&config_r, "./cfg"));

	cl_git_pass(git_config_set_string(config_w, "key.value", "1"));
	cl_git_pass(git_config_foreach_match(config_r, "key.value", foreach_cb, &value));

	cl_assert_equal_s(value, "1");

	git_config_free(config_r);
	git_config_free(config_w);
	git__free(value);
}

void test_config_stress__foreach_refreshes_snapshot(void)
{
	git_config *config, *snapshot;
	char *value = NULL;

	cl_git_pass(git_config_open_ondisk(&config, "./cfg"));

	cl_git_pass(git_config_set_string(config, "key.value", "1"));
	cl_git_pass(git_config_snapshot(&snapshot, config));
	cl_git_pass(git_config_foreach_match(snapshot, "key.value", foreach_cb, &value));

	cl_assert_equal_s(value, "1");

	git_config_free(snapshot);
	git_config_free(config);
	git__free(value);
}

void test_config_stress__huge_section_with_many_values(void)
{
	git_config *config;

	if (!cl_is_env_set("GITTEST_INVASIVE_SPEED"))
		cl_skip();

	/*
	 * The config file is structured in such a way that is
	 * has a section header that is approximately 500kb of
	 * size followed by 40k entries. While the resulting
	 * configuration file itself is roughly 650kb in size and
	 * thus considered to be rather small, in the past we'd
	 * balloon to more than 20GB of memory (20000x500kb)
	 * while parsing the file. It thus was a trivial way to
	 * cause an out-of-memory situation and thus cause denial
	 * of service, e.g. via gitmodules.
	 */
	cl_git_pass(git_config_open_ondisk(&config, cl_fixture("config/config-oom")));

	git_config_free(config);
}
