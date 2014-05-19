#include "clar_libgit2.h"

void test_config_snapshot__create_snapshot(void)
{
	int32_t tmp;
	git_config *cfg, *snapshot;
	const char *filename = "config-ext-change";

	cl_git_mkfile(filename, "[old]\nvalue = 5\n");

	cl_git_pass(git_config_open_ondisk(&cfg, filename));

	cl_git_pass(git_config_get_int32(&tmp, cfg, "old.value"));
	cl_assert_equal_i(5, tmp);

	cl_git_pass(git_config_snapshot(&snapshot, cfg));

	/* Change the value on the file itself (simulate external process) */
	cl_git_mkfile(filename, "[old]\nvalue = 56\n");

	cl_git_pass(git_config_get_int32(&tmp, cfg, "old.value"));
	cl_assert_equal_i(56, tmp);

	cl_git_pass(git_config_get_int32(&tmp, snapshot, "old.value"));
	cl_assert_equal_i(5, tmp);
	
	git_config_free(snapshot);
	git_config_free(cfg);
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
	int count = 0;
	git_config *cfg, *snapshot;
	const char *filename = "config-file";

	cl_git_mkfile(filename, "[old]\nvalue = 5\nvalue = 6\n");

	cl_git_pass(git_config_open_ondisk(&cfg, filename));
	cl_git_pass(git_config_get_multivar_foreach(cfg, "old.value", NULL, count_me, &count));

	cl_assert_equal_i(2, count);

	cl_git_pass(git_config_snapshot(&snapshot, cfg));
	git_config_free(cfg);

	count = 0;
	cl_git_pass(git_config_get_multivar_foreach(snapshot, "old.value", NULL, count_me, &count));

	cl_assert_equal_i(2, count);

	git_config_free(snapshot);
}
