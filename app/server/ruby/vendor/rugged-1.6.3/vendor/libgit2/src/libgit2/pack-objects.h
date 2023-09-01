/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_pack_objects_h__
#define INCLUDE_pack_objects_h__

#include "common.h"

#include "str.h"
#include "hash.h"
#include "oidmap.h"
#include "netops.h"
#include "zstream.h"
#include "pool.h"
#include "indexer.h"

#include "git2/oid.h"
#include "git2/pack.h"

#define GIT_PACK_WINDOW 10 /* number of objects to possibly delta against */
#define GIT_PACK_DEPTH 50 /* max delta depth */
#define GIT_PACK_DELTA_CACHE_SIZE (256 * 1024 * 1024)
#define GIT_PACK_DELTA_CACHE_LIMIT 1000
#define GIT_PACK_BIG_FILE_THRESHOLD (512 * 1024 * 1024)

typedef struct git_pobject {
	git_oid id;
	git_object_t type;
	off64_t offset;

	size_t size;

	unsigned int hash; /* name hint hash */

	struct git_pobject *delta; /* delta base object */
	struct git_pobject *delta_child; /* deltified objects who bases me */
	struct git_pobject *delta_sibling; /* other deltified objects
					    * who uses the same base as
					    * me */

	void *delta_data;
	size_t delta_size;
	size_t z_delta_size;

	unsigned int written:1,
	             recursing:1,
	             tagged:1,
	             filled:1;
} git_pobject;

struct git_packbuilder {
	git_repository *repo; /* associated repository */
	git_odb *odb; /* associated object database */

	git_hash_ctx ctx;
	git_zstream zstream;

	uint32_t nr_objects,
		nr_deltified,
		nr_written,
		nr_remaining;

	size_t nr_alloc;

	git_pobject *object_list;

	git_oidmap *object_ix;

	git_oidmap *walk_objects;
	git_pool object_pool;

#ifndef GIT_DEPRECATE_HARD
	git_oid pack_oid; /* hash of written pack */
#endif
	char *pack_name; /* name of written pack */

	/* synchronization objects */
	git_mutex cache_mutex;
	git_mutex progress_mutex;
	git_cond progress_cond;

	/* configs */
	size_t delta_cache_size;
	size_t max_delta_cache_size;
	size_t cache_max_small_delta_size;
	size_t big_file_threshold;
	size_t window_memory_limit;

	unsigned int nr_threads; /* nr of threads to use */

	git_packbuilder_progress progress_cb;
	void *progress_cb_payload;
	double last_progress_report_time; /* the time progress was last reported */

	bool done;
};

int git_packbuilder__write_buf(git_str *buf, git_packbuilder *pb);
int git_packbuilder__prepare(git_packbuilder *pb);


#endif
