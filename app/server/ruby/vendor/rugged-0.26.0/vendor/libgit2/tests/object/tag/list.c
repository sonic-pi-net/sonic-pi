#include "clar_libgit2.h"

#include "tag.h"

static git_repository *g_repo;

#define MAX_USED_TAGS 6

struct pattern_match_t
{
	const char* pattern;
	const size_t expected_matches;
	const char* expected_results[MAX_USED_TAGS];
};

// Helpers
static void ensure_tag_pattern_match(git_repository *repo,
									 const struct pattern_match_t* data)
{
	int already_found[MAX_USED_TAGS] = { 0 };
	git_strarray tag_list;
	int error = 0;
	size_t sucessfully_found = 0;
	size_t i, j;

	cl_assert(data->expected_matches <= MAX_USED_TAGS);

	if ((error = git_tag_list_match(&tag_list, data->pattern, repo)) < 0)
		goto exit;

	if (tag_list.count != data->expected_matches)
	{
		error = GIT_ERROR;
		goto exit;
	}

	// we have to be prepared that tags come in any order.
	for (i = 0; i < tag_list.count; i++)
	{
		for (j = 0; j < data->expected_matches; j++)
		{
			if (!already_found[j] && !strcmp(data->expected_results[j], tag_list.strings[i]))
			{
				already_found[j] = 1;
				sucessfully_found++;
				break;
			}
		}
	}
	cl_assert_equal_i((int)sucessfully_found, (int)data->expected_matches);

exit:
	git_strarray_free(&tag_list);
	cl_git_pass(error);
}

// Fixture setup and teardown
void test_object_tag_list__initialize(void)
{
	g_repo = cl_git_sandbox_init("testrepo");
}

void test_object_tag_list__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_object_tag_list__list_all(void)
{
	// list all tag names from the repository
	git_strarray tag_list;

	cl_git_pass(git_tag_list(&tag_list, g_repo));

	cl_assert_equal_i((int)tag_list.count, 6);

	git_strarray_free(&tag_list);
}

static const struct pattern_match_t matches[] = {
	// All tags, including a packed one and two namespaced ones.
	{ "", 6, { "e90810b", "point_to_blob", "test", "packed-tag", "foo/bar", "foo/foo/bar" } },

	// beginning with
	{ "t*", 1, { "test" } },

	// ending with
	{ "*b", 2, { "e90810b", "point_to_blob" } },

	// exact match
	{ "e", 0 },
	{ "e90810b", 1, { "e90810b" } },

	// either or
	{ "e90810[ab]", 1, { "e90810b" } },

	// glob in the middle
	{ "foo/*/bar", 1, { "foo/foo/bar" } },

	// The matching of '*' is based on plain string matching analog to the regular expression ".*"
	// => a '/' in the tag name has no special meaning.
	// Compare to `git tag -l "*bar"`
	{ "*bar", 2, { "foo/bar", "foo/foo/bar" } },

	// End of list
	{ NULL }
};

void test_object_tag_list__list_by_pattern(void)
{
	// list all tag names from the repository matching a specified pattern
	size_t i = 0;
	while (matches[i].pattern)
		ensure_tag_pattern_match(g_repo, &matches[i++]);
}
