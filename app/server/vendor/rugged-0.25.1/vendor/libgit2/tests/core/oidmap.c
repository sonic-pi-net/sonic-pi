#include "clar_libgit2.h"
#include "oidmap.h"

GIT__USE_OIDMAP

typedef struct {
	git_oid oid;
	size_t extra;
} oidmap_item;

#define NITEMS 0x0fff

void test_core_oidmap__basic(void)
{
	git_oidmap *map;
	oidmap_item items[NITEMS];
	uint32_t i, j;

	for (i = 0; i < NITEMS; ++i) {
		items[i].extra = i;
		for (j = 0; j < GIT_OID_RAWSZ / 4; ++j) {
			items[i].oid.id[j * 4    ] = (unsigned char)i;
			items[i].oid.id[j * 4 + 1] = (unsigned char)(i >> 8);
			items[i].oid.id[j * 4 + 2] = (unsigned char)(i >> 16);
			items[i].oid.id[j * 4 + 3] = (unsigned char)(i >> 24);
		}
	}

	map = git_oidmap_alloc();
	cl_assert(map != NULL);

	for (i = 0; i < NITEMS; ++i) {
		khiter_t pos;
		int ret;

		pos = kh_get(oid, map, &items[i].oid);
		cl_assert(pos == kh_end(map));

		pos = kh_put(oid, map, &items[i].oid, &ret);
		cl_assert(ret != 0);

		kh_val(map, pos) = &items[i];
	}


	for (i = 0; i < NITEMS; ++i) {
		khiter_t pos;

		pos = kh_get(oid, map, &items[i].oid);
		cl_assert(pos != kh_end(map));

		cl_assert_equal_p(kh_val(map, pos), &items[i]);
	}

	git_oidmap_free(map);
}

void test_core_oidmap__hash_collision(void)
{
	git_oidmap *map;
	oidmap_item items[NITEMS];
	uint32_t i, j;

	for (i = 0; i < NITEMS; ++i) {
		uint32_t segment = i / 8;
		int modi = i - (segment * 8);

		items[i].extra = i;

		for (j = 0; j < GIT_OID_RAWSZ / 4; ++j) {
			items[i].oid.id[j * 4    ] = (unsigned char)modi;
			items[i].oid.id[j * 4 + 1] = (unsigned char)(modi >> 8);
			items[i].oid.id[j * 4 + 2] = (unsigned char)(modi >> 16);
			items[i].oid.id[j * 4 + 3] = (unsigned char)(modi >> 24);
		}

		items[i].oid.id[ 8] = (unsigned char)i;
		items[i].oid.id[ 9] = (unsigned char)(i >> 8);
		items[i].oid.id[10] = (unsigned char)(i >> 16);
		items[i].oid.id[11] = (unsigned char)(i >> 24);
	}

	map = git_oidmap_alloc();
	cl_assert(map != NULL);

	for (i = 0; i < NITEMS; ++i) {
		khiter_t pos;
		int ret;

		pos = kh_get(oid, map, &items[i].oid);
		cl_assert(pos == kh_end(map));

		pos = kh_put(oid, map, &items[i].oid, &ret);
		cl_assert(ret != 0);

		kh_val(map, pos) = &items[i];
	}


	for (i = 0; i < NITEMS; ++i) {
		khiter_t pos;

		pos = kh_get(oid, map, &items[i].oid);
		cl_assert(pos != kh_end(map));

		cl_assert_equal_p(kh_val(map, pos), &items[i]);
	}

	git_oidmap_free(map);
}
