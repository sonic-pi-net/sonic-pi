#include "clar_libgit2.h"
#include "oidmap.h"

static struct {
	git_oid oid;
	size_t extra;
} test_oids[0x0FFF];

static git_oidmap *g_map;

void test_core_oidmap__initialize(void)
{
	uint32_t i, j;
	for (i = 0; i < ARRAY_SIZE(test_oids); ++i) {
		uint32_t segment = i / 8;
		int modi = i - (segment * 8);

		test_oids[i].extra = i;

		for (j = 0; j < GIT_OID_SHA1_SIZE / 4; ++j) {
			test_oids[i].oid.id[j * 4    ] = (unsigned char)modi;
			test_oids[i].oid.id[j * 4 + 1] = (unsigned char)(modi >> 8);
			test_oids[i].oid.id[j * 4 + 2] = (unsigned char)(modi >> 16);
			test_oids[i].oid.id[j * 4 + 3] = (unsigned char)(modi >> 24);
		}

		test_oids[i].oid.id[ 8] = (unsigned char)i;
		test_oids[i].oid.id[ 9] = (unsigned char)(i >> 8);
		test_oids[i].oid.id[10] = (unsigned char)(i >> 16);
		test_oids[i].oid.id[11] = (unsigned char)(i >> 24);
#ifdef GIT_EXPERIMENTAL_SHA256
		test_oids[i].oid.type = GIT_OID_SHA1;
#endif
	}

	cl_git_pass(git_oidmap_new(&g_map));
}

void test_core_oidmap__cleanup(void)
{
	git_oidmap_free(g_map);
}

void test_core_oidmap__basic(void)
{
	size_t i;

	for (i = 0; i < ARRAY_SIZE(test_oids); ++i) {
		cl_assert(!git_oidmap_exists(g_map, &test_oids[i].oid));
		cl_git_pass(git_oidmap_set(g_map, &test_oids[i].oid, &test_oids[i]));
	}

	for (i = 0; i < ARRAY_SIZE(test_oids); ++i) {
		cl_assert(git_oidmap_exists(g_map, &test_oids[i].oid));
		cl_assert_equal_p(git_oidmap_get(g_map, &test_oids[i].oid), &test_oids[i]);
	}
}

void test_core_oidmap__hash_collision(void)
{
	size_t i;

	for (i = 0; i < ARRAY_SIZE(test_oids); ++i) {
		cl_assert(!git_oidmap_exists(g_map, &test_oids[i].oid));
		cl_git_pass(git_oidmap_set(g_map, &test_oids[i].oid, &test_oids[i]));
	}

	for (i = 0; i < ARRAY_SIZE(test_oids); ++i) {
		cl_assert(git_oidmap_exists(g_map, &test_oids[i].oid));
		cl_assert_equal_p(git_oidmap_get(g_map, &test_oids[i].oid), &test_oids[i]);
	}
}

void test_core_oidmap__get_succeeds_with_existing_keys(void)
{
	size_t i;

	for (i = 0; i < ARRAY_SIZE(test_oids); ++i)
		cl_git_pass(git_oidmap_set(g_map, &test_oids[i].oid, &test_oids[i]));

	for (i = 0; i < ARRAY_SIZE(test_oids); ++i)
		cl_assert_equal_p(git_oidmap_get(g_map, &test_oids[i].oid), &test_oids[i]);
}

void test_core_oidmap__get_fails_with_nonexisting_key(void)
{
	size_t i;

	/* Do _not_ add last OID to verify that we cannot look it up */
	for (i = 0; i < ARRAY_SIZE(test_oids) - 1; ++i)
		cl_git_pass(git_oidmap_set(g_map, &test_oids[i].oid, &test_oids[i]));

	cl_assert_equal_p(git_oidmap_get(g_map, &test_oids[ARRAY_SIZE(test_oids) - 1].oid), NULL);
}

void test_core_oidmap__setting_oid_persists(void)
{
	git_oid oids[] = {
		GIT_OID_INIT(GIT_OID_SHA1, { 0x01 }),
		GIT_OID_INIT(GIT_OID_SHA1, { 0x02 }),
		GIT_OID_INIT(GIT_OID_SHA1, { 0x03 })
	};

	cl_git_pass(git_oidmap_set(g_map, &oids[0], "one"));
	cl_git_pass(git_oidmap_set(g_map, &oids[1], "two"));
	cl_git_pass(git_oidmap_set(g_map, &oids[2], "three"));

	cl_assert_equal_s(git_oidmap_get(g_map, &oids[0]), "one");
	cl_assert_equal_s(git_oidmap_get(g_map, &oids[1]), "two");
	cl_assert_equal_s(git_oidmap_get(g_map, &oids[2]), "three");
}

void test_core_oidmap__setting_existing_key_updates(void)
{
	git_oid oids[] = {
		GIT_OID_INIT(GIT_OID_SHA1, { 0x01 }),
		GIT_OID_INIT(GIT_OID_SHA1, { 0x02 }),
		GIT_OID_INIT(GIT_OID_SHA1, { 0x03 })
	};

	cl_git_pass(git_oidmap_set(g_map, &oids[0], "one"));
	cl_git_pass(git_oidmap_set(g_map, &oids[1], "two"));
	cl_git_pass(git_oidmap_set(g_map, &oids[2], "three"));
	cl_assert_equal_i(git_oidmap_size(g_map), 3);

	cl_git_pass(git_oidmap_set(g_map, &oids[1], "other"));
	cl_assert_equal_i(git_oidmap_size(g_map), 3);

	cl_assert_equal_s(git_oidmap_get(g_map, &oids[1]), "other");
}
