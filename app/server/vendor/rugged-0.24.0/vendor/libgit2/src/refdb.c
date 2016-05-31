/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "posix.h"

#include "git2/object.h"
#include "git2/refs.h"
#include "git2/refdb.h"
#include "git2/sys/refdb_backend.h"

#include "hash.h"
#include "refdb.h"
#include "refs.h"
#include "reflog.h"

int git_refdb_new(git_refdb **out, git_repository *repo)
{
	git_refdb *db;

	assert(out && repo);

	db = git__calloc(1, sizeof(*db));
	GITERR_CHECK_ALLOC(db);

	db->repo = repo;

	*out = db;
	GIT_REFCOUNT_INC(db);
	return 0;
}

int git_refdb_open(git_refdb **out, git_repository *repo)
{
	git_refdb *db;
	git_refdb_backend *dir;

	assert(out && repo);

	*out = NULL;

	if (git_refdb_new(&db, repo) < 0)
		return -1;

	/* Add the default (filesystem) backend */
	if (git_refdb_backend_fs(&dir, repo) < 0) {
		git_refdb_free(db);
		return -1;
	}

	db->repo = repo;
	db->backend = dir;

	*out = db;
	return 0;
}

static void refdb_free_backend(git_refdb *db)
{
	if (db->backend)
		db->backend->free(db->backend);
}

int git_refdb_set_backend(git_refdb *db, git_refdb_backend *backend)
{
	refdb_free_backend(db);
	db->backend = backend;

	return 0;
}

int git_refdb_compress(git_refdb *db)
{
	assert(db);

	if (db->backend->compress)
		return db->backend->compress(db->backend);

	return 0;
}

void git_refdb__free(git_refdb *db)
{
	refdb_free_backend(db);
	git__memzero(db, sizeof(*db));
	git__free(db);
}

void git_refdb_free(git_refdb *db)
{
	if (db == NULL)
		return;

	GIT_REFCOUNT_DEC(db, git_refdb__free);
}

int git_refdb_exists(int *exists, git_refdb *refdb, const char *ref_name)
{
	assert(exists && refdb && refdb->backend);

	return refdb->backend->exists(exists, refdb->backend, ref_name);
}

int git_refdb_lookup(git_reference **out, git_refdb *db, const char *ref_name)
{
	git_reference *ref;
	int error;

	assert(db && db->backend && out && ref_name);

	error = db->backend->lookup(&ref, db->backend, ref_name);
	if (error < 0)
		return error;

	GIT_REFCOUNT_INC(db);
	ref->db = db;

	*out = ref;
	return 0;
}

int git_refdb_iterator(git_reference_iterator **out, git_refdb *db, const char *glob)
{
	if (!db->backend || !db->backend->iterator) {
		giterr_set(GITERR_REFERENCE, "This backend doesn't support iterators");
		return -1;
	}

	if (db->backend->iterator(out, db->backend, glob) < 0)
		return -1;

	GIT_REFCOUNT_INC(db);
	(*out)->db = db;

	return 0;
}

int git_refdb_iterator_next(git_reference **out, git_reference_iterator *iter)
{
	int error;

	if ((error = iter->next(out, iter)) < 0)
		return error;

	GIT_REFCOUNT_INC(iter->db);
	(*out)->db = iter->db;

	return 0;
}

int git_refdb_iterator_next_name(const char **out, git_reference_iterator *iter)
{
	return iter->next_name(out, iter);
}

void git_refdb_iterator_free(git_reference_iterator *iter)
{
	GIT_REFCOUNT_DEC(iter->db, git_refdb__free);
	iter->free(iter);
}

int git_refdb_write(git_refdb *db, git_reference *ref, int force, const git_signature *who, const char *message, const git_oid *old_id, const char *old_target)
{
	assert(db && db->backend);

	GIT_REFCOUNT_INC(db);
	ref->db = db;

	return db->backend->write(db->backend, ref, force, who, message, old_id, old_target);
}

int git_refdb_rename(
	git_reference **out,
	git_refdb *db,
	const char *old_name,
	const char *new_name,
	int force,
	const git_signature *who,
	const char *message)
{
	int error;

	assert(db && db->backend);
	error = db->backend->rename(out, db->backend, old_name, new_name, force, who, message);
	if (error < 0)
		return error;

	if (out) {
		GIT_REFCOUNT_INC(db);
		(*out)->db = db;
	}

	return 0;
}

int git_refdb_delete(struct git_refdb *db, const char *ref_name, const git_oid *old_id, const char *old_target)
{
	assert(db && db->backend);
	return db->backend->del(db->backend, ref_name, old_id, old_target);
}

int git_refdb_reflog_read(git_reflog **out, git_refdb *db,  const char *name)
{
	int error;

	assert(db && db->backend);

	if ((error = db->backend->reflog_read(out, db->backend, name)) < 0)
		return error;

	GIT_REFCOUNT_INC(db);
	(*out)->db = db;

	return 0;
}

int git_refdb_has_log(git_refdb *db, const char *refname)
{
	assert(db && refname);

	return db->backend->has_log(db->backend, refname);
}

int git_refdb_ensure_log(git_refdb *db, const char *refname)
{
	assert(db && refname);

	return db->backend->ensure_log(db->backend, refname);
}

int git_refdb_init_backend(git_refdb_backend *backend, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		backend, version, git_refdb_backend, GIT_REFDB_BACKEND_INIT);
	return 0;
}

int git_refdb_lock(void **payload, git_refdb *db, const char *refname)
{
	assert(payload && db && refname);

	if (!db->backend->lock) {
		giterr_set(GITERR_REFERENCE, "backend does not support locking");
		return -1;
	}

	return db->backend->lock(payload, db->backend, refname);
}

int git_refdb_unlock(git_refdb *db, void *payload, int success, int update_reflog, const git_reference *ref, const git_signature *sig, const char *message)
{
	assert(db);

	return db->backend->unlock(db->backend, payload, success, update_reflog, ref, sig, message);
}
