#include "clar_libgit2.h"
#include "config_helpers.h"
#include "repository.h"
#include "buffer.h"

void assert_config_entry_existence(
	git_repository *repo,
	const char *name,
	bool is_supposed_to_exist)
{
	git_config *config;
	git_config_entry *entry = NULL;
	int result;

	cl_git_pass(git_repository_config__weakptr(&config, repo));
	
	result = git_config_get_entry(&entry, config, name);
	git_config_entry_free(entry);

	if (is_supposed_to_exist)
		cl_git_pass(result);
	else
		cl_assert_equal_i(GIT_ENOTFOUND, result);
}

void assert_config_entry_value(
	git_repository *repo,
	const char *name,
	const char *expected_value)
{
	git_config *config;
	git_buf buf = GIT_BUF_INIT;

	cl_git_pass(git_repository_config__weakptr(&config, repo));

	cl_git_pass(git_config_get_string_buf(&buf, config, name));

	cl_assert_equal_s(expected_value, git_buf_cstr(&buf));
	git_buf_free(&buf);
}

static int count_config_entries_cb(
	const git_config_entry *entry,
	void *payload)
{
	int *how_many = (int *)payload;

	GIT_UNUSED(entry);

	(*how_many)++;

	return 0;
}

int count_config_entries_match(git_repository *repo, const char *pattern)
{
	git_config *config;
	int how_many = 0;

	cl_git_pass(git_repository_config(&config, repo));

	cl_assert_equal_i(0, git_config_foreach_match(
		config,	pattern, count_config_entries_cb, &how_many));

	git_config_free(config);

	return how_many;
}
