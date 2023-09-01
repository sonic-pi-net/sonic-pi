#include "clar_libgit2.h"
#include "strmap.h"

static git_strmap *g_table;

void test_strmap__initialize(void)
{
	cl_git_pass(git_strmap_new(&g_table));
	cl_assert(g_table != NULL);
}

void test_strmap__cleanup(void)
{
	git_strmap_free(g_table);
}

void test_strmap__0(void)
{
	cl_assert(git_strmap_size(g_table) == 0);
}

static void insert_strings(git_strmap *table, size_t count)
{
	size_t i, j, over;
	char *str;

	for (i = 0; i < count; ++i) {
		str = malloc(10);
		for (j = 0; j < 10; ++j)
			str[j] = 'a' + (i % 26);
		str[9] = '\0';

		/* if > 26, then encode larger value in first letters */
		for (j = 0, over = i / 26; over > 0; j++, over = over / 26)
			str[j] = 'A' + (over % 26);

		cl_git_pass(git_strmap_set(table, str, str));
	}

	cl_assert_equal_i(git_strmap_size(table), count);
}

void test_strmap__inserted_strings_can_be_retrieved(void)
{
	char *str;
	int i;

	insert_strings(g_table, 20);

	cl_assert(git_strmap_exists(g_table, "aaaaaaaaa"));
	cl_assert(git_strmap_exists(g_table, "ggggggggg"));
	cl_assert(!git_strmap_exists(g_table, "aaaaaaaab"));
	cl_assert(!git_strmap_exists(g_table, "abcdefghi"));

	i = 0;
	git_strmap_foreach_value(g_table, str, { i++; free(str); });
	cl_assert(i == 20);
}

void test_strmap__deleted_entry_cannot_be_retrieved(void)
{
	char *str;
	int i;

	insert_strings(g_table, 20);

	cl_assert(git_strmap_exists(g_table, "bbbbbbbbb"));
	str = git_strmap_get(g_table, "bbbbbbbbb");
	cl_assert_equal_s(str, "bbbbbbbbb");
	cl_git_pass(git_strmap_delete(g_table, "bbbbbbbbb"));
	free(str);

	cl_assert(!git_strmap_exists(g_table, "bbbbbbbbb"));

	i = 0;
	git_strmap_foreach_value(g_table, str, { i++; free(str); });
	cl_assert_equal_i(i, 19);
}

void test_strmap__inserting_many_keys_succeeds(void)
{
	char *str;
	int i;

	insert_strings(g_table, 10000);

	i = 0;
	git_strmap_foreach_value(g_table, str, { i++; free(str); });
	cl_assert_equal_i(i, 10000);
}

void test_strmap__get_succeeds_with_existing_entries(void)
{
	const char *keys[] = { "foo", "bar", "gobble" };
	char *values[] = { "oof", "rab", "elbbog" };
	size_t i;

	for (i = 0; i < ARRAY_SIZE(keys); i++)
		cl_git_pass(git_strmap_set(g_table, keys[i], values[i]));

	cl_assert_equal_s(git_strmap_get(g_table, "foo"), "oof");
	cl_assert_equal_s(git_strmap_get(g_table, "bar"), "rab");
	cl_assert_equal_s(git_strmap_get(g_table, "gobble"), "elbbog");
}

void test_strmap__get_returns_null_on_nonexisting_key(void)
{
	const char *keys[] = { "foo", "bar", "gobble" };
	char *values[] = { "oof", "rab", "elbbog" };
	size_t i;

	for (i = 0; i < ARRAY_SIZE(keys); i++)
		cl_git_pass(git_strmap_set(g_table, keys[i], values[i]));

	cl_assert_equal_p(git_strmap_get(g_table, "other"), NULL);
}

void test_strmap__set_persists_key(void)
{
	cl_git_pass(git_strmap_set(g_table, "foo", "oof"));
	cl_assert_equal_s(git_strmap_get(g_table, "foo"), "oof");
}

void test_strmap__set_persists_multpile_keys(void)
{
	cl_git_pass(git_strmap_set(g_table, "foo", "oof"));
	cl_git_pass(git_strmap_set(g_table, "bar", "rab"));
	cl_assert_equal_s(git_strmap_get(g_table, "foo"), "oof");
	cl_assert_equal_s(git_strmap_get(g_table, "bar"), "rab");
}

void test_strmap__set_updates_existing_key(void)
{
	cl_git_pass(git_strmap_set(g_table, "foo", "oof"));
	cl_git_pass(git_strmap_set(g_table, "bar", "rab"));
	cl_git_pass(git_strmap_set(g_table, "gobble", "elbbog"));
	cl_assert_equal_i(git_strmap_size(g_table), 3);

	cl_git_pass(git_strmap_set(g_table, "foo", "other"));
	cl_assert_equal_i(git_strmap_size(g_table), 3);

	cl_assert_equal_s(git_strmap_get(g_table, "foo"), "other");
}

void test_strmap__iteration(void)
{
	struct {
		char *key;
		char *value;
		int seen;
	} entries[] = {
		{ "foo", "oof" },
		{ "bar", "rab" },
		{ "gobble", "elbbog" },
	};
	const char *key, *value;
	size_t i, n;

	for (i = 0; i < ARRAY_SIZE(entries); i++)
		cl_git_pass(git_strmap_set(g_table, entries[i].key, entries[i].value));

	i = 0, n = 0;
	while (git_strmap_iterate((void **) &value, g_table, &i, &key) == 0) {
		size_t j;

		for (j = 0; j < ARRAY_SIZE(entries); j++) {
			if (strcmp(entries[j].key, key))
				continue;

			cl_assert_equal_i(entries[j].seen, 0);
			cl_assert_equal_s(entries[j].value, value);
			entries[j].seen++;
			break;
		}

		n++;
	}

	for (i = 0; i < ARRAY_SIZE(entries); i++)
		cl_assert_equal_i(entries[i].seen, 1);

	cl_assert_equal_i(n, ARRAY_SIZE(entries));
}

void test_strmap__iterating_empty_map_stops_immediately(void)
{
	size_t i = 0;

	cl_git_fail_with(git_strmap_iterate(NULL, g_table, &i, NULL), GIT_ITEROVER);
}
