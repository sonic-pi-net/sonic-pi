#include "clar_libgit2.h"
#include "sortedcache.h"

static int name_only_cmp(const void *a, const void *b)
{
	return strcmp(a, b);
}

void test_sortedcache__name_only(void)
{
	git_sortedcache *sc;
	void *item;
	size_t pos;

	cl_git_pass(git_sortedcache_new(
		&sc, 0, NULL, NULL, name_only_cmp, NULL));

	cl_git_pass(git_sortedcache_wlock(sc));
	cl_git_pass(git_sortedcache_upsert(&item, sc, "aaa"));
	cl_git_pass(git_sortedcache_upsert(&item, sc, "bbb"));
	cl_git_pass(git_sortedcache_upsert(&item, sc, "zzz"));
	cl_git_pass(git_sortedcache_upsert(&item, sc, "mmm"));
	cl_git_pass(git_sortedcache_upsert(&item, sc, "iii"));
	git_sortedcache_wunlock(sc);

	cl_assert_equal_sz(5, git_sortedcache_entrycount(sc));

	cl_assert((item = git_sortedcache_lookup(sc, "aaa")) != NULL);
	cl_assert_equal_s("aaa", item);
	cl_assert((item = git_sortedcache_lookup(sc, "mmm")) != NULL);
	cl_assert_equal_s("mmm", item);
	cl_assert((item = git_sortedcache_lookup(sc, "zzz")) != NULL);
	cl_assert_equal_s("zzz", item);
	cl_assert(git_sortedcache_lookup(sc, "qqq") == NULL);

	cl_assert((item = git_sortedcache_entry(sc, 0)) != NULL);
	cl_assert_equal_s("aaa", item);
	cl_assert((item = git_sortedcache_entry(sc, 1)) != NULL);
	cl_assert_equal_s("bbb", item);
	cl_assert((item = git_sortedcache_entry(sc, 2)) != NULL);
	cl_assert_equal_s("iii", item);
	cl_assert((item = git_sortedcache_entry(sc, 3)) != NULL);
	cl_assert_equal_s("mmm", item);
	cl_assert((item = git_sortedcache_entry(sc, 4)) != NULL);
	cl_assert_equal_s("zzz", item);
	cl_assert(git_sortedcache_entry(sc, 5) == NULL);

	cl_git_pass(git_sortedcache_lookup_index(&pos, sc, "aaa"));
	cl_assert_equal_sz(0, pos);
	cl_git_pass(git_sortedcache_lookup_index(&pos, sc, "iii"));
	cl_assert_equal_sz(2, pos);
	cl_git_pass(git_sortedcache_lookup_index(&pos, sc, "zzz"));
	cl_assert_equal_sz(4, pos);
	cl_assert_equal_i(
		GIT_ENOTFOUND, git_sortedcache_lookup_index(&pos, sc, "abc"));

	cl_git_pass(git_sortedcache_clear(sc, true));

	cl_assert_equal_sz(0, git_sortedcache_entrycount(sc));
	cl_assert(git_sortedcache_entry(sc, 0) == NULL);
	cl_assert(git_sortedcache_lookup(sc, "aaa") == NULL);
	cl_assert(git_sortedcache_entry(sc, 0) == NULL);

	git_sortedcache_free(sc);
}

typedef struct {
	int value;
	char smaller_value;
	char path[GIT_FLEX_ARRAY];
} sortedcache_test_struct;

static int sortedcache_test_struct_cmp(const void *a_, const void *b_)
{
	const sortedcache_test_struct *a = a_, *b = b_;
	return strcmp(a->path, b->path);
}

static void sortedcache_test_struct_free(void *payload, void *item_)
{
	sortedcache_test_struct *item = item_;
	int *count = payload;
	(*count)++;
	item->smaller_value = 0;
}

void test_sortedcache__in_memory(void)
{
	git_sortedcache *sc;
	sortedcache_test_struct *item;
	int free_count = 0;

	cl_git_pass(git_sortedcache_new(
		&sc, offsetof(sortedcache_test_struct, path),
		sortedcache_test_struct_free, &free_count,
		sortedcache_test_struct_cmp, NULL));

	cl_git_pass(git_sortedcache_wlock(sc));
	cl_git_pass(git_sortedcache_upsert((void **)&item, sc, "aaa"));
	item->value = 10;
	item->smaller_value = 1;
	cl_git_pass(git_sortedcache_upsert((void **)&item, sc, "bbb"));
	item->value = 20;
	item->smaller_value = 2;
	cl_git_pass(git_sortedcache_upsert((void **)&item, sc, "zzz"));
	item->value = 30;
	item->smaller_value = 26;
	cl_git_pass(git_sortedcache_upsert((void **)&item, sc, "mmm"));
	item->value = 40;
	item->smaller_value = 14;
	cl_git_pass(git_sortedcache_upsert((void **)&item, sc, "iii"));
	item->value = 50;
	item->smaller_value = 9;
	git_sortedcache_wunlock(sc);

	cl_assert_equal_sz(5, git_sortedcache_entrycount(sc));

	cl_git_pass(git_sortedcache_rlock(sc));

	cl_assert((item = git_sortedcache_lookup(sc, "aaa")) != NULL);
	cl_assert_equal_s("aaa", item->path);
	cl_assert_equal_i(10, item->value);
	cl_assert((item = git_sortedcache_lookup(sc, "mmm")) != NULL);
	cl_assert_equal_s("mmm", item->path);
	cl_assert_equal_i(40, item->value);
	cl_assert((item = git_sortedcache_lookup(sc, "zzz")) != NULL);
	cl_assert_equal_s("zzz", item->path);
	cl_assert_equal_i(30, item->value);
	cl_assert(git_sortedcache_lookup(sc, "abc") == NULL);

	/* not on Windows:
	 * cl_git_pass(git_sortedcache_rlock(sc)); -- grab more than one
	 */

	cl_assert((item = git_sortedcache_entry(sc, 0)) != NULL);
	cl_assert_equal_s("aaa", item->path);
	cl_assert_equal_i(10, item->value);
	cl_assert((item = git_sortedcache_entry(sc, 1)) != NULL);
	cl_assert_equal_s("bbb", item->path);
	cl_assert_equal_i(20, item->value);
	cl_assert((item = git_sortedcache_entry(sc, 2)) != NULL);
	cl_assert_equal_s("iii", item->path);
	cl_assert_equal_i(50, item->value);
	cl_assert((item = git_sortedcache_entry(sc, 3)) != NULL);
	cl_assert_equal_s("mmm", item->path);
	cl_assert_equal_i(40, item->value);
	cl_assert((item = git_sortedcache_entry(sc, 4)) != NULL);
	cl_assert_equal_s("zzz", item->path);
	cl_assert_equal_i(30, item->value);
	cl_assert(git_sortedcache_entry(sc, 5) == NULL);

	git_sortedcache_runlock(sc);
	/* git_sortedcache_runlock(sc); */

	cl_assert_equal_i(0, free_count);

	cl_git_pass(git_sortedcache_clear(sc, true));

	cl_assert_equal_i(5, free_count);

	cl_assert_equal_sz(0, git_sortedcache_entrycount(sc));
	cl_assert(git_sortedcache_entry(sc, 0) == NULL);
	cl_assert(git_sortedcache_lookup(sc, "aaa") == NULL);
	cl_assert(git_sortedcache_entry(sc, 0) == NULL);

	free_count = 0;

	cl_git_pass(git_sortedcache_wlock(sc));
	cl_git_pass(git_sortedcache_upsert((void **)&item, sc, "testing"));
	item->value = 10;
	item->smaller_value = 3;
	cl_git_pass(git_sortedcache_upsert((void **)&item, sc, "again"));
	item->value = 20;
	item->smaller_value = 1;
	cl_git_pass(git_sortedcache_upsert((void **)&item, sc, "final"));
	item->value = 30;
	item->smaller_value = 2;
	git_sortedcache_wunlock(sc);

	cl_assert_equal_sz(3, git_sortedcache_entrycount(sc));

	cl_assert((item = git_sortedcache_lookup(sc, "testing")) != NULL);
	cl_assert_equal_s("testing", item->path);
	cl_assert_equal_i(10, item->value);
	cl_assert((item = git_sortedcache_lookup(sc, "again")) != NULL);
	cl_assert_equal_s("again", item->path);
	cl_assert_equal_i(20, item->value);
	cl_assert((item = git_sortedcache_lookup(sc, "final")) != NULL);
	cl_assert_equal_s("final", item->path);
	cl_assert_equal_i(30, item->value);
	cl_assert(git_sortedcache_lookup(sc, "zzz") == NULL);

	cl_assert((item = git_sortedcache_entry(sc, 0)) != NULL);
	cl_assert_equal_s("again", item->path);
	cl_assert_equal_i(20, item->value);
	cl_assert((item = git_sortedcache_entry(sc, 1)) != NULL);
	cl_assert_equal_s("final", item->path);
	cl_assert_equal_i(30, item->value);
	cl_assert((item = git_sortedcache_entry(sc, 2)) != NULL);
	cl_assert_equal_s("testing", item->path);
	cl_assert_equal_i(10, item->value);
	cl_assert(git_sortedcache_entry(sc, 3) == NULL);

	{
		size_t pos;

		cl_git_pass(git_sortedcache_wlock(sc));

		cl_git_pass(git_sortedcache_lookup_index(&pos, sc, "again"));
		cl_assert_equal_sz(0, pos);
		cl_git_pass(git_sortedcache_remove(sc, pos));
		cl_assert_equal_i(
			GIT_ENOTFOUND, git_sortedcache_lookup_index(&pos, sc, "again"));

		cl_assert_equal_sz(2, git_sortedcache_entrycount(sc));

		cl_git_pass(git_sortedcache_lookup_index(&pos, sc, "testing"));
		cl_assert_equal_sz(1, pos);
		cl_git_pass(git_sortedcache_remove(sc, pos));
		cl_assert_equal_i(
			GIT_ENOTFOUND, git_sortedcache_lookup_index(&pos, sc, "testing"));

		cl_assert_equal_sz(1, git_sortedcache_entrycount(sc));

		cl_git_pass(git_sortedcache_lookup_index(&pos, sc, "final"));
		cl_assert_equal_sz(0, pos);
		cl_git_pass(git_sortedcache_remove(sc, pos));
		cl_assert_equal_i(
			GIT_ENOTFOUND, git_sortedcache_lookup_index(&pos, sc, "final"));

		cl_assert_equal_sz(0, git_sortedcache_entrycount(sc));

		git_sortedcache_wunlock(sc);
	}

	git_sortedcache_free(sc);

	cl_assert_equal_i(3, free_count);
}

static void sortedcache_test_reload(git_sortedcache *sc)
{
	int count = 0;
	git_str buf = GIT_STR_INIT;
	char *scan, *after;
	sortedcache_test_struct *item;

	cl_assert(git_sortedcache_lockandload(sc, &buf) > 0);

	cl_git_pass(git_sortedcache_clear(sc, false)); /* clear once we already have lock */

	for (scan = buf.ptr; *scan; scan = after + 1) {
		int val = strtol(scan, &after, 0);
		cl_assert(after > scan);
		scan = after;

		for (scan = after; git__isspace(*scan); ++scan) /* find start */;
		for (after = scan; *after && *after != '\n'; ++after) /* find eol */;
		*after = '\0';

		cl_git_pass(git_sortedcache_upsert((void **)&item, sc, scan));

		item->value = val;
		item->smaller_value = (char)(count++);
	}

	git_sortedcache_wunlock(sc);

	git_str_dispose(&buf);
}

void test_sortedcache__on_disk(void)
{
	git_sortedcache *sc;
	sortedcache_test_struct *item;
	int free_count = 0;
	size_t pos;

	cl_git_mkfile("cacheitems.txt", "10 abc\n20 bcd\n30 cde\n");

	cl_git_pass(git_sortedcache_new(
		&sc, offsetof(sortedcache_test_struct, path),
		sortedcache_test_struct_free, &free_count,
		sortedcache_test_struct_cmp, "cacheitems.txt"));

	/* should need to reload the first time */

	sortedcache_test_reload(sc);

	/* test what we loaded */

	cl_assert_equal_sz(3, git_sortedcache_entrycount(sc));

	cl_assert((item = git_sortedcache_lookup(sc, "abc")) != NULL);
	cl_assert_equal_s("abc", item->path);
	cl_assert_equal_i(10, item->value);
	cl_assert((item = git_sortedcache_lookup(sc, "cde")) != NULL);
	cl_assert_equal_s("cde", item->path);
	cl_assert_equal_i(30, item->value);
	cl_assert(git_sortedcache_lookup(sc, "aaa") == NULL);

	cl_assert((item = git_sortedcache_entry(sc, 0)) != NULL);
	cl_assert_equal_s("abc", item->path);
	cl_assert_equal_i(10, item->value);
	cl_assert((item = git_sortedcache_entry(sc, 1)) != NULL);
	cl_assert_equal_s("bcd", item->path);
	cl_assert_equal_i(20, item->value);
	cl_assert(git_sortedcache_entry(sc, 3) == NULL);

	/* should not need to reload this time */

	cl_assert_equal_i(0, git_sortedcache_lockandload(sc, NULL));

	/* rewrite ondisk file and reload */

	cl_assert_equal_i(0, free_count);

	cl_git_rewritefile(
		"cacheitems.txt", "100 abc\n200 zzz\n500 aaa\n10 final\n");
	sortedcache_test_reload(sc);

	cl_assert_equal_i(3, free_count);

	/* test what we loaded */

	cl_assert_equal_sz(4, git_sortedcache_entrycount(sc));

	cl_assert((item = git_sortedcache_lookup(sc, "abc")) != NULL);
	cl_assert_equal_s("abc", item->path);
	cl_assert_equal_i(100, item->value);
	cl_assert((item = git_sortedcache_lookup(sc, "final")) != NULL);
	cl_assert_equal_s("final", item->path);
	cl_assert_equal_i(10, item->value);
	cl_assert(git_sortedcache_lookup(sc, "cde") == NULL);

	cl_assert((item = git_sortedcache_entry(sc, 0)) != NULL);
	cl_assert_equal_s("aaa", item->path);
	cl_assert_equal_i(500, item->value);
	cl_assert((item = git_sortedcache_entry(sc, 2)) != NULL);
	cl_assert_equal_s("final", item->path);
	cl_assert_equal_i(10, item->value);
	cl_assert((item = git_sortedcache_entry(sc, 3)) != NULL);
	cl_assert_equal_s("zzz", item->path);
	cl_assert_equal_i(200, item->value);

	cl_git_pass(git_sortedcache_lookup_index(&pos, sc, "aaa"));
	cl_assert_equal_sz(0, pos);
	cl_git_pass(git_sortedcache_lookup_index(&pos, sc, "abc"));
	cl_assert_equal_sz(1, pos);
	cl_git_pass(git_sortedcache_lookup_index(&pos, sc, "final"));
	cl_assert_equal_sz(2, pos);
	cl_git_pass(git_sortedcache_lookup_index(&pos, sc, "zzz"));
	cl_assert_equal_sz(3, pos);
	cl_assert_equal_i(
		GIT_ENOTFOUND, git_sortedcache_lookup_index(&pos, sc, "missing"));
	cl_assert_equal_i(
		GIT_ENOTFOUND, git_sortedcache_lookup_index(&pos, sc, "cde"));

	git_sortedcache_free(sc);

	cl_assert_equal_i(7, free_count);
}

