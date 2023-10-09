
#include "clar_libgit2.h"

#include "odb.h"
#include "hash.h"

void test_object_raw_short__oid_shortener_no_duplicates(void)
{
	git_oid_shorten *os;
	int min_len;

	os = git_oid_shorten_new(0);
	cl_assert(os != NULL);

	git_oid_shorten_add(os, "22596363b3de40b06f981fb85d82312e8c0ed511");
	git_oid_shorten_add(os, "ce08fe4884650f067bd5703b6a59a8b3b3c99a09");
	git_oid_shorten_add(os, "16a0123456789abcdef4b775213c23a8bd74f5e0");
	min_len = git_oid_shorten_add(os, "ce08fe4884650f067bd5703b6a59a8b3b3c99a09");

	cl_assert(min_len == GIT_OID_SHA1_HEXSIZE + 1);

	git_oid_shorten_free(os);
}

static int insert_sequential_oids(
	char ***out, git_oid_shorten *os, int n, int fail)
{
	int i, min_len = 0;
	char numbuf[16];
	git_oid oid;
	unsigned char hashbuf[GIT_HASH_SHA1_SIZE];
	char **oids = git__calloc(n, sizeof(char *));
	cl_assert(oids != NULL);

	for (i = 0; i < n; ++i) {
		p_snprintf(numbuf, sizeof(numbuf), "%u", (unsigned int)i);
		git_hash_buf(hashbuf, numbuf, strlen(numbuf), GIT_HASH_ALGORITHM_SHA1);

		git_oid__fromraw(&oid, hashbuf, GIT_OID_SHA1);

		oids[i] = git__malloc(GIT_OID_SHA1_HEXSIZE + 1);
		cl_assert(oids[i]);
		git_oid_nfmt(oids[i], GIT_OID_SHA1_HEXSIZE + 1, &oid);

		min_len = git_oid_shorten_add(os, oids[i]);

		/* After "fail", we expect git_oid_shorten_add to fail */
		if (fail >= 0 && i >= fail)
            cl_assert(min_len < 0);
		else
            cl_assert(min_len >= 0);
	}

	*out = oids;

	return min_len;
}

static void free_oids(int n, char **oids)
{
	int i;

	for (i = 0; i < n; ++i) {
		git__free(oids[i]);
	}
	git__free(oids);
}

void test_object_raw_short__oid_shortener_stresstest_git_oid_shorten(void)
{
#define MAX_OIDS 1000

	git_oid_shorten *os;
	size_t i, j;
	int min_len = 0, found_collision;
	char **oids;

	os = git_oid_shorten_new(0);
	cl_assert(os != NULL);

	/*
	 * Insert in the shortener 1000 unique SHA1 ids
	 */
	min_len = insert_sequential_oids(&oids, os, MAX_OIDS, MAX_OIDS);
	cl_assert(min_len > 0);

	/*
	 * Compare the first `min_char - 1` characters of each
	 * SHA1 OID. If the minimizer worked, we should find at
	 * least one collision
	 */
	found_collision = 0;
	for (i = 0; i < MAX_OIDS; ++i) {
		for (j = i + 1; j < MAX_OIDS; ++j) {
			if (memcmp(oids[i], oids[j], min_len - 1) == 0)
				found_collision = 1;
		}
	}
	cl_assert_equal_b(true, found_collision);

	/*
	 * Compare the first `min_char` characters of each
	 * SHA1 OID. If the minimizer worked, every single preffix
	 * should be unique.
	 */
	found_collision = 0;
	for (i = 0; i < MAX_OIDS; ++i) {
		for (j = i + 1; j < MAX_OIDS; ++j) {
			if (memcmp(oids[i], oids[j], min_len) == 0)
				found_collision = 1;
		}
	}
	cl_assert_equal_b(false, found_collision);

	/* cleanup */
	free_oids(MAX_OIDS, oids);
	git_oid_shorten_free(os);

#undef MAX_OIDS
}

void test_object_raw_short__oid_shortener_too_much_oids(void)
{
    /* The magic number of oids at which an oid_shortener will fail.
     * This was experimentally established. */
#define MAX_OIDS 24556

	git_oid_shorten *os;
	char **oids;

	os = git_oid_shorten_new(0);
	cl_assert(os != NULL);

	cl_assert(insert_sequential_oids(&oids, os, MAX_OIDS, MAX_OIDS - 1) < 0);

	free_oids(MAX_OIDS, oids);
	git_oid_shorten_free(os);

#undef MAX_OIDS
}
