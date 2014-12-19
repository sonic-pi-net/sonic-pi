#include "clar_libgit2.h"
#include "strmap.h"

GIT__USE_STRMAP;

git_strmap *g_table;

void test_core_strmap__initialize(void)
{
	cl_git_pass(git_strmap_alloc(&g_table));
	cl_assert(g_table != NULL);
}

void test_core_strmap__cleanup(void)
{
	git_strmap_free(g_table);
}

void test_core_strmap__0(void)
{
	cl_assert(git_strmap_num_entries(g_table) == 0);
}

static void insert_strings(git_strmap *table, int count)
{
	int i, j, over, err;
	char *str;

	for (i = 0; i < count; ++i) {
		str = malloc(10);
		for (j = 0; j < 10; ++j)
			str[j] = 'a' + (i % 26);
		str[9] = '\0';

		/* if > 26, then encode larger value in first letters */
		for (j = 0, over = i / 26; over > 0; j++, over = over / 26)
			str[j] = 'A' + (over % 26);

		git_strmap_insert(table, str, str, err);
		cl_assert(err >= 0);
	}

	cl_assert((int)git_strmap_num_entries(table) == count);
}

void test_core_strmap__1(void)
{
	int i;
	char *str;

	insert_strings(g_table, 20);

	cl_assert(git_strmap_exists(g_table, "aaaaaaaaa"));
	cl_assert(git_strmap_exists(g_table, "ggggggggg"));
	cl_assert(!git_strmap_exists(g_table, "aaaaaaaab"));
	cl_assert(!git_strmap_exists(g_table, "abcdefghi"));

	i = 0;
	git_strmap_foreach_value(g_table, str, { i++; free(str); });
	cl_assert(i == 20);
}

void test_core_strmap__2(void)
{
	khiter_t pos;
	int i;
	char *str;

	insert_strings(g_table, 20);

	cl_assert(git_strmap_exists(g_table, "aaaaaaaaa"));
	cl_assert(git_strmap_exists(g_table, "ggggggggg"));
	cl_assert(!git_strmap_exists(g_table, "aaaaaaaab"));
	cl_assert(!git_strmap_exists(g_table, "abcdefghi"));

	cl_assert(git_strmap_exists(g_table, "bbbbbbbbb"));
	pos = git_strmap_lookup_index(g_table, "bbbbbbbbb");
	cl_assert(git_strmap_valid_index(g_table, pos));
	cl_assert_equal_s(git_strmap_value_at(g_table, pos), "bbbbbbbbb");
	free(git_strmap_value_at(g_table, pos));
	git_strmap_delete_at(g_table, pos);

	cl_assert(!git_strmap_exists(g_table, "bbbbbbbbb"));

	i = 0;
	git_strmap_foreach_value(g_table, str, { i++; free(str); });
	cl_assert(i == 19);
}

void test_core_strmap__3(void)
{
	int i;
	char *str;

	insert_strings(g_table, 10000);

	i = 0;
	git_strmap_foreach_value(g_table, str, { i++; free(str); });
	cl_assert(i == 10000);
}
