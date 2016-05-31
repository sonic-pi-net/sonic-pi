/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "git2/object.h"
#include "git2/sys/odb_backend.h"
#include "fileops.h"
#include "hash.h"
#include "odb.h"
#include "array.h"
#include "oidmap.h"

#include "git2/odb_backend.h"
#include "git2/types.h"
#include "git2/pack.h"

GIT__USE_OIDMAP

struct memobject {
	git_oid oid;
	size_t len;
	git_otype type;
	char data[];
};

struct memory_packer_db {
	git_odb_backend parent;
	git_oidmap *objects;
	git_array_t(struct memobject *) commits;
};

static int impl__write(git_odb_backend *_backend, const git_oid *oid, const void *data, size_t len, git_otype type)
{
	struct memory_packer_db *db = (struct memory_packer_db *)_backend;
	struct memobject *obj = NULL; 
	khiter_t pos;
	size_t alloc_len;
	int rval;

	pos = kh_put(oid, db->objects, oid, &rval);
	if (rval < 0)
		return -1;

	if (rval == 0)
		return 0;

	GITERR_CHECK_ALLOC_ADD(&alloc_len, sizeof(struct memobject), len);
	obj = git__malloc(alloc_len);
	GITERR_CHECK_ALLOC(obj);

	memcpy(obj->data, data, len);
	git_oid_cpy(&obj->oid, oid);
	obj->len = len;
	obj->type = type;

	kh_key(db->objects, pos) = &obj->oid;
	kh_val(db->objects, pos) = obj;

	if (type == GIT_OBJ_COMMIT) {
		struct memobject **store = git_array_alloc(db->commits);
		GITERR_CHECK_ALLOC(store);
		*store = obj;
	}

	return 0;
}

static int impl__exists(git_odb_backend *backend, const git_oid *oid)
{
	struct memory_packer_db *db = (struct memory_packer_db *)backend;
	khiter_t pos;

	pos = kh_get(oid, db->objects, oid);
	if (pos != kh_end(db->objects))
		return 1;

	return 0;
}

static int impl__read(void **buffer_p, size_t *len_p, git_otype *type_p, git_odb_backend *backend, const git_oid *oid)
{
	struct memory_packer_db *db = (struct memory_packer_db *)backend;
	struct memobject *obj = NULL;
	khiter_t pos;

	pos = kh_get(oid, db->objects, oid);
	if (pos == kh_end(db->objects))
		return GIT_ENOTFOUND;

	obj = kh_val(db->objects, pos);

	*len_p = obj->len;
	*type_p = obj->type;
	*buffer_p = git__malloc(obj->len);
	GITERR_CHECK_ALLOC(*buffer_p);

	memcpy(*buffer_p, obj->data, obj->len);
	return 0;
}

static int impl__read_header(size_t *len_p, git_otype *type_p, git_odb_backend *backend, const git_oid *oid)
{
	struct memory_packer_db *db = (struct memory_packer_db *)backend;
	struct memobject *obj = NULL;
	khiter_t pos;

	pos = kh_get(oid, db->objects, oid);
	if (pos == kh_end(db->objects))
		return GIT_ENOTFOUND;

	obj = kh_val(db->objects, pos);

	*len_p = obj->len;
	*type_p = obj->type;
	return 0;
}

int git_mempack_dump(git_buf *pack, git_repository *repo, git_odb_backend *_backend)
{
	struct memory_packer_db *db = (struct memory_packer_db *)_backend;
	git_packbuilder *packbuilder;
	uint32_t i;
	int err = -1;

	if (git_packbuilder_new(&packbuilder, repo) < 0)
		return -1;

	for (i = 0; i < db->commits.size; ++i) {
		struct memobject *commit = db->commits.ptr[i];

		err = git_packbuilder_insert_commit(packbuilder, &commit->oid);
		if (err < 0)
			goto cleanup;
	}

	err = git_packbuilder_write_buf(pack, packbuilder);

cleanup:
	git_packbuilder_free(packbuilder);
	return err;
}

void git_mempack_reset(git_odb_backend *_backend)
{
	struct memory_packer_db *db = (struct memory_packer_db *)_backend;
	struct memobject *object = NULL;

	kh_foreach_value(db->objects, object, {
		git__free(object);
	});

	git_array_clear(db->commits);

	git_oidmap_clear(db->objects);
}

static void impl__free(git_odb_backend *_backend)
{
	struct memory_packer_db *db = (struct memory_packer_db *)_backend;

	git_oidmap_free(db->objects);
	git__free(db);
}

int git_mempack_new(git_odb_backend **out)
{
	struct memory_packer_db *db;

	assert(out);

	db = git__calloc(1, sizeof(struct memory_packer_db));
	GITERR_CHECK_ALLOC(db);

	db->objects = git_oidmap_alloc();

	db->parent.read = &impl__read;
	db->parent.write = &impl__write;
	db->parent.read_header = &impl__read_header;
	db->parent.exists = &impl__exists;
	db->parent.free = &impl__free;

	*out = (git_odb_backend *)db;
	return 0;
}
