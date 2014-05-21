/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifndef INCLUDE_pack_h__
#define INCLUDE_pack_h__

#include <zlib.h>

#include "git2/oid.h"

#include "common.h"
#include "map.h"
#include "mwindow.h"
#include "odb.h"
#include "oidmap.h"
#include "array.h"

#define GIT_PACK_FILE_MODE 0444

#define PACK_SIGNATURE 0x5041434b	/* "PACK" */
#define PACK_VERSION 2
#define pack_version_ok(v) ((v) == htonl(2) || (v) == htonl(3))
struct git_pack_header {
	uint32_t hdr_signature;
	uint32_t hdr_version;
	uint32_t hdr_entries;
};

/*
 * The first four bytes of index formats later than version 1 should
 * start with this signature, as all older git binaries would find this
 * value illegal and abort reading the file.
 *
 * This is the case because the number of objects in a packfile
 * cannot exceed 1,431,660,000 as every object would need at least
 * 3 bytes of data and the overall packfile cannot exceed 4 GiB with
 * version 1 of the index file due to the offsets limited to 32 bits.
 * Clearly the signature exceeds this maximum.
 *
 * Very old git binaries will also compare the first 4 bytes to the
 * next 4 bytes in the index and abort with a "non-monotonic index"
 * error if the second 4 byte word is smaller than the first 4
 * byte word. This would be true in the proposed future index
 * format as idx_signature would be greater than idx_version.
 */

#define PACK_IDX_SIGNATURE 0xff744f63	/* "\377tOc" */

struct git_pack_idx_header {
	uint32_t idx_signature;
	uint32_t idx_version;
};

typedef struct git_pack_cache_entry {
	size_t last_usage; /* enough? */
	git_atomic refcount;
	git_rawobj raw;
} git_pack_cache_entry;

struct pack_chain_elem {
	git_off_t base_key;
	git_off_t offset;
	size_t size;
	git_otype type;
};

typedef git_array_t(struct pack_chain_elem) git_dependency_chain;

#include "offmap.h"

GIT__USE_OFFMAP;
GIT__USE_OIDMAP;

#define GIT_PACK_CACHE_MEMORY_LIMIT 16 * 1024 * 1024
#define GIT_PACK_CACHE_SIZE_LIMIT 1024 * 1024 /* don't bother caching anything over 1MB */

typedef struct {
	size_t memory_used;
	size_t memory_limit;
	size_t use_ctr;
	git_mutex lock;
	git_offmap *entries;
} git_pack_cache;

struct git_pack_file {
	git_mwindow_file mwf;
	git_map index_map;
	git_mutex lock; /* protect updates to mwf and index_map */

	uint32_t num_objects;
	uint32_t num_bad_objects;
	git_oid *bad_object_sha1; /* array of git_oid */

	int index_version;
	git_time_t mtime;
	unsigned pack_local:1, pack_keep:1, has_cache:1;
	git_oidmap *idx_cache;
	git_oid **oids;

	git_pack_cache bases; /* delta base cache */

	/* something like ".git/objects/pack/xxxxx.pack" */
	char pack_name[GIT_FLEX_ARRAY]; /* more */
};

struct git_pack_entry {
	git_off_t offset;
	git_oid sha1;
	struct git_pack_file *p;
};

typedef struct git_packfile_stream {
	git_off_t curpos;
	int done;
	z_stream zstream;
	struct git_pack_file *p;
	git_mwindow *mw;
} git_packfile_stream;

size_t git_packfile__object_header(unsigned char *hdr, size_t size, git_otype type);

int git_packfile_unpack_header(
		size_t *size_p,
		git_otype *type_p,
		git_mwindow_file *mwf,
		git_mwindow **w_curs,
		git_off_t *curpos);

int git_packfile_resolve_header(
		size_t *size_p,
		git_otype *type_p,
		struct git_pack_file *p,
		git_off_t offset);

int git_packfile_unpack(git_rawobj *obj, struct git_pack_file *p, git_off_t *obj_offset);
int packfile_unpack_compressed(
	git_rawobj *obj,
	struct git_pack_file *p,
	git_mwindow **w_curs,
	git_off_t *curpos,
	size_t size,
	git_otype type);

int git_packfile_stream_open(git_packfile_stream *obj, struct git_pack_file *p, git_off_t curpos);
ssize_t git_packfile_stream_read(git_packfile_stream *obj, void *buffer, size_t len);
void git_packfile_stream_free(git_packfile_stream *obj);

git_off_t get_delta_base(struct git_pack_file *p, git_mwindow **w_curs,
		git_off_t *curpos, git_otype type,
		git_off_t delta_obj_offset);

void git_packfile_free(struct git_pack_file *p);
int git_packfile_alloc(struct git_pack_file **pack_out, const char *path);

int git_pack_entry_find(
		struct git_pack_entry *e,
		struct git_pack_file *p,
		const git_oid *short_oid,
		size_t len);
int git_pack_foreach_entry(
		struct git_pack_file *p,
		git_odb_foreach_cb cb,
		void *data);

#endif
