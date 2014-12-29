/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_pack_objects_h__
#define INCLUDE_pack_objects_h__

#include "common.h"

#include "buffer.h"
#include "hash.h"
#include "oidmap.h"
#include "netops.h"
#include "zstream.h"

#include "git2/oid.h"
#include "git2/pack.h"

#define GIT_PACK_WINDOW 10 /* number of objects to possibly delta against */
#define GIT_PACK_DEPTH 50 /* max delta depth */
#define GIT_PACK_DELTA_CACHE_SIZE (256 * 1024 * 1024)
#define GIT_PACK_DELTA_CACHE_LIMIT 1000
#define GIT_PACK_BIG_FILE_THRESHOLD (512 * 1024 * 1024)

typedef struct git_pobject {
	git_oid id;
	git_otype type;
	git_off_t offset;

	size_t size;

	unsigned int hash; /* name hint hash */

	struct git_pobject *delta; /* delta base object */
	struct git_pobject *delta_child; /* deltified objects who bases me */
	struct git_pobject *delta_sibling; /* other deltified objects
					    * who uses the same base as
					    * me */

	void *delta_data;
	unsigned long delta_size;
	unsigned long z_delta_size;

	int written:1,
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
		 nr_alloc,
		 nr_written,
		 nr_remaining;

	git_pobject *object_list;

	git_oidmap *object_ix;

	git_oid pack_oid; /* hash of written pack */

	/* synchronization objects */
	git_mutex cache_mutex;
	git_mutex progress_mutex;
	git_cond progress_cond;

	/* configs */
	uint64_t delta_cache_size;
	uint64_t max_delta_cache_size;
	uint64_t cache_max_small_delta_size;
	uint64_t big_file_threshold;
	uint64_t window_memory_limit;

	int nr_threads; /* nr of threads to use */

	git_packbuilder_progress progress_cb;
	void *progress_cb_payload;
	double last_progress_report_time; /* the time progress was last reported */

	bool done;
};

int git_packbuilder_write_buf(git_buf *buf, git_packbuilder *pb);

#endif /* INCLUDE_pack_objects_h__ */
