#include "clar_libgit2.h"

static const char *_name = "remote.ab.url";

void test_config_multivar__initialize(void)
{
	cl_fixture_sandbox("config");
}

void test_config_multivar__cleanup(void)
{
	cl_fixture_cleanup("config");
}

static int mv_read_cb(const git_config_entry *entry, void *data)
{
	int *n = (int *) data;

	if (!strcmp(entry->name, _name))
		(*n)++;

	return 0;
}

void test_config_multivar__foreach(void)
{
	git_config *cfg;
	int n = 0;

	cl_git_pass(git_config_open_ondisk(&cfg, cl_fixture("config/config11")));

	cl_git_pass(git_config_foreach(cfg, mv_read_cb, &n));
	cl_assert(n == 2);

	git_config_free(cfg);
}

static int cb(const git_config_entry *entry, void *data)
{
	int *n = (int *) data;

	GIT_UNUSED(entry);

	(*n)++;

	return 0;
}

static void check_get_multivar_foreach(
	git_config *cfg, int expected, int expected_patterned)
{
	int n = 0;

	if (expected > 0) {
		cl_git_pass(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n));
		cl_assert_equal_i(expected, n);
	} else {
		cl_assert_equal_i(GIT_ENOTFOUND,
			git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n));
	}

	n = 0;

	if (expected_patterned > 0) {
		cl_git_pass(git_config_get_multivar_foreach(cfg, _name, "example", cb, &n));
		cl_assert_equal_i(expected_patterned, n);
	} else {
		cl_assert_equal_i(GIT_ENOTFOUND,
			git_config_get_multivar_foreach(cfg, _name, "example", cb, &n));
	}
}

static void check_get_multivar(git_config *cfg, int expected)
{
	git_config_iterator *iter;
	git_config_entry *entry;
	int n = 0;

	cl_git_pass(git_config_multivar_iterator_new(&iter, cfg, _name, NULL));

	while (git_config_next(&entry, iter) == 0)
		n++;

	cl_assert_equal_i(expected, n);
	git_config_iterator_free(iter);

}

void test_config_multivar__get(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));
	check_get_multivar_foreach(cfg, 2, 1);

	/* add another that has the _name entry */
	cl_git_pass(git_config_add_file_ondisk(cfg, "config/config9", GIT_CONFIG_LEVEL_SYSTEM, 1));
	check_get_multivar_foreach(cfg, 3, 2);

	/* add another that does not have the _name entry */
	cl_git_pass(git_config_add_file_ondisk(cfg, "config/config0", GIT_CONFIG_LEVEL_GLOBAL, 1));
	check_get_multivar_foreach(cfg, 3, 2);

	/* add another that does not have the _name entry at the end */
	cl_git_pass(git_config_add_file_ondisk(cfg, "config/config1", GIT_CONFIG_LEVEL_APP, 1));
	check_get_multivar_foreach(cfg, 3, 2);

	/* drop original file */
	cl_git_pass(git_config_add_file_ondisk(cfg, "config/config2", GIT_CONFIG_LEVEL_LOCAL, 1));
	check_get_multivar_foreach(cfg, 1, 1);

	/* drop other file with match */
	cl_git_pass(git_config_add_file_ondisk(cfg, "config/config3", GIT_CONFIG_LEVEL_SYSTEM, 1));
	check_get_multivar_foreach(cfg, 0, 0);

	/* reload original file (add different place in order) */
	cl_git_pass(git_config_add_file_ondisk(cfg, "config/config11", GIT_CONFIG_LEVEL_SYSTEM, 1));
	check_get_multivar_foreach(cfg, 2, 1);

	check_get_multivar(cfg, 2);

	git_config_free(cfg);
}

void test_config_multivar__add(void)
{
	git_config *cfg;
	int n;

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));
	cl_git_pass(git_config_set_multivar(cfg, _name, "nonexistant", "git://git.otherplace.org/libgit2"));

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n));
	cl_assert_equal_i(n, 3);

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, "otherplace", cb, &n));
	cl_assert_equal_i(n, 1);

	git_config_free(cfg);

	/* We know it works in memory, let's see if the file is written correctly */

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n));
	cl_assert_equal_i(n, 3);

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, "otherplace", cb, &n));
	cl_assert_equal_i(n, 1);

	git_config_free(cfg);
}

void test_config_multivar__add_new(void)
{
	const char *var = "a.brand.new";
	git_config *cfg;
	int n;

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));

	cl_git_pass(git_config_set_multivar(cfg, var, "", "variable"));
	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, var, NULL, cb, &n));
	cl_assert_equal_i(n, 1);

	git_config_free(cfg);
}

void test_config_multivar__replace(void)
{
	git_config *cfg;
	int n;

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n));
	cl_assert(n == 2);

	cl_git_pass(git_config_set_multivar(cfg, _name, "github", "git://git.otherplace.org/libgit2"));

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n));
	cl_assert(n == 2);

	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n));
	cl_assert(n == 2);

	git_config_free(cfg);
}

void test_config_multivar__replace_multiple(void)
{
	git_config *cfg;
	int n;

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));
	cl_git_pass(git_config_set_multivar(cfg, _name, "git://", "git://git.otherplace.org/libgit2"));

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, "otherplace", cb, &n));
	cl_assert_equal_i(n, 2);

	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, "otherplace", cb, &n));
	cl_assert_equal_i(n, 2);

	git_config_free(cfg);
}

void test_config_multivar__delete(void)
{
	git_config *cfg;
	int n;

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n));
	cl_assert_equal_i(2, n);

	cl_git_pass(git_config_delete_multivar(cfg, _name, "github"));

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n));
	cl_assert_equal_i(1, n);

	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n));
	cl_assert_equal_i(1, n);

	git_config_free(cfg);
}

void test_config_multivar__delete_multiple(void)
{
	git_config *cfg;
	int n;

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));

	n = 0;
	cl_git_pass(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n));
	cl_assert(n == 2);

	cl_git_pass(git_config_delete_multivar(cfg, _name, "git"));

	n = 0;
	cl_git_fail_with(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n), GIT_ENOTFOUND);

	git_config_free(cfg);

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));

	n = 0;
	cl_git_fail_with(git_config_get_multivar_foreach(cfg, _name, NULL, cb, &n), GIT_ENOTFOUND);

	git_config_free(cfg);
}

void test_config_multivar__delete_notfound(void)
{
	git_config *cfg;

	cl_git_pass(git_config_open_ondisk(&cfg, "config/config11"));

	cl_git_fail_with(git_config_delete_multivar(cfg, "remote.ab.noturl", "git"), GIT_ENOTFOUND);

	git_config_free(cfg);
}
