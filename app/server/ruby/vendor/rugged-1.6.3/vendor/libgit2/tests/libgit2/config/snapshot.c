#include "clar_libgit2.h"

#include "config_backend.h"

static git_config *cfg;
static git_config *snapshot;

void test_config_snapshot__cleanup(void)
{
	git_config_free(cfg);
	cfg = NULL;
	git_config_free(snapshot);
	snapshot = NULL;
}

void test_config_snapshot__create_snapshot(void)
{
	int32_t i;

	cl_git_mkfile("config", "[old]\nvalue = 5\n");
	cl_git_pass(git_config_open_ondisk(&cfg, "config"));
	cl_git_pass(git_config_get_int32(&i, cfg, "old.value"));
	cl_assert_equal_i(5, i);

	cl_git_pass(git_config_snapshot(&snapshot, cfg));

	/* Change the value on the file itself (simulate external process) */
	cl_git_mkfile("config", "[old]\nvalue = 56\n");

	cl_git_pass(git_config_get_int32(&i, cfg, "old.value"));
	cl_assert_equal_i(56, i);
	cl_git_pass(git_config_get_int32(&i, snapshot, "old.value"));
	cl_assert_equal_i(5, i);

	/* Change the value on the file itself (simulate external process) */
	cl_git_mkfile("config", "[old]\nvalue = 999\n");

	/* Old snapshot should still have the old value */
	cl_git_pass(git_config_get_int32(&i, snapshot, "old.value"));
	cl_assert_equal_i(5, i);

	/* New snapshot should see new value */
	git_config_free(snapshot);
	cl_git_pass(git_config_snapshot(&snapshot, cfg));
	cl_git_pass(git_config_get_int32(&i, snapshot, "old.value"));
	cl_assert_equal_i(999, i);

	cl_git_pass(p_unlink("config"));
}

static int count_me(const git_config_entry *entry, void *payload)
{
	int *n = (int *) payload;

	GIT_UNUSED(entry);

	(*n)++;

	return 0;
}

void test_config_snapshot__multivar(void)
{
	int count;

	count = 0;
	cl_git_mkfile("config", "[old]\nvalue = 5\nvalue = 6\n");
	cl_git_pass(git_config_open_ondisk(&cfg, "config"));
	cl_git_pass(git_config_get_multivar_foreach(cfg, "old.value", NULL, count_me, &count));
	cl_assert_equal_i(2, count);

	count = 0;
	cl_git_pass(git_config_snapshot(&snapshot, cfg));
	cl_git_pass(git_config_get_multivar_foreach(snapshot, "old.value", NULL, count_me, &count));
	cl_assert_equal_i(2, count);

	cl_git_pass(p_unlink("config"));
}

void test_config_snapshot__includes(void)
{
	int i;

	cl_git_mkfile("including", "[include]\npath = included");
	cl_git_mkfile("included", "[section]\nkey = 1\n");

	cl_git_pass(git_config_open_ondisk(&cfg, "including"));
	cl_git_pass(git_config_snapshot(&snapshot, cfg));

	cl_git_pass(git_config_get_int32(&i, snapshot, "section.key"));
	cl_assert_equal_i(i, 1);

	/* Rewrite "included" config */
	cl_git_mkfile("included", "[section]\nkey = 11\n");

	/* Assert that the live config changed, but snapshot remained the same */
	cl_git_pass(git_config_get_int32(&i, cfg, "section.key"));
	cl_assert_equal_i(i, 11);
	cl_git_pass(git_config_get_int32(&i, snapshot, "section.key"));
	cl_assert_equal_i(i, 1);

	cl_git_pass(p_unlink("including"));
	cl_git_pass(p_unlink("included"));
}

void test_config_snapshot__snapshot(void)
{
	git_config *snapshot_snapshot;
	int i;

	cl_git_mkfile("configfile", "[section]\nkey = 1\n");

	cl_git_pass(git_config_open_ondisk(&cfg, "configfile"));
	cl_git_pass(git_config_snapshot(&snapshot, cfg));

	cl_git_pass(git_config_snapshot(&snapshot_snapshot, snapshot));

	cl_git_pass(git_config_get_int32(&i, snapshot_snapshot, "section.key"));
	cl_assert_equal_i(i, 1);

	git_config_free(snapshot_snapshot);

	cl_git_pass(p_unlink("configfile"));
}

void test_config_snapshot__snapshot_from_in_memony(void)
{
	const char *configuration = "[section]\nkey = 1\n";
	git_config_backend *backend;
	int i;

	cl_git_pass(git_config_new(&cfg));
	cl_git_pass(git_config_backend_from_string(&backend, configuration, strlen(configuration)));
	cl_git_pass(git_config_add_backend(cfg, backend, 0, NULL, 0));

	cl_git_pass(git_config_snapshot(&snapshot, cfg));
	cl_git_pass(git_config_get_int32(&i, snapshot, "section.key"));
	cl_assert_equal_i(i, 1);
}
