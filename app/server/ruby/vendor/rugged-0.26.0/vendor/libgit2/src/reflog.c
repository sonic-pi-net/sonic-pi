/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "reflog.h"

#include "repository.h"
#include "filebuf.h"
#include "signature.h"
#include "refdb.h"

#include <git2/sys/refdb_backend.h>

git_reflog_entry *git_reflog_entry__alloc(void)
{
	return git__calloc(1, sizeof(git_reflog_entry));
}

void git_reflog_entry__free(git_reflog_entry *entry)
{
	git_signature_free(entry->committer);

	git__free(entry->msg);
	git__free(entry);
}

void git_reflog_free(git_reflog *reflog)
{
	size_t i;
	git_reflog_entry *entry;

	if (reflog == NULL)
		return;

	if (reflog->db)
		GIT_REFCOUNT_DEC(reflog->db, git_refdb__free);

	for (i=0; i < reflog->entries.length; i++) {
		entry = git_vector_get(&reflog->entries, i);

		git_reflog_entry__free(entry);
	}

	git_vector_free(&reflog->entries);
	git__free(reflog->ref_name);
	git__free(reflog);
}

int git_reflog_read(git_reflog **reflog, git_repository *repo,  const char *name)
{
	git_refdb *refdb;
	int error;

	assert(reflog && repo && name);

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		return error;

	return git_refdb_reflog_read(reflog, refdb, name);
}

int git_reflog_write(git_reflog *reflog)
{
	git_refdb *db;

	assert(reflog && reflog->db);

	db = reflog->db;
	return db->backend->reflog_write(db->backend, reflog);
}

int git_reflog_append(git_reflog *reflog, const git_oid *new_oid, const git_signature *committer, const char *msg)
{
	git_reflog_entry *entry;
	const git_reflog_entry *previous;
	const char *newline;

	assert(reflog && new_oid && committer);

	entry = git__calloc(1, sizeof(git_reflog_entry));
	GITERR_CHECK_ALLOC(entry);

	if ((git_signature_dup(&entry->committer, committer)) < 0)
		goto cleanup;

	if (msg != NULL) {
		if ((entry->msg = git__strdup(msg)) == NULL)
			goto cleanup;

		newline = strchr(msg, '\n');

		if (newline) {
			if (newline[1] != '\0') {
				giterr_set(GITERR_INVALID, "reflog message cannot contain newline");
				goto cleanup;
			}

			entry->msg[newline - msg] = '\0';
		}
	}

	previous = git_reflog_entry_byindex(reflog, 0);

	if (previous == NULL)
		git_oid_fromstr(&entry->oid_old, GIT_OID_HEX_ZERO);
	else
		git_oid_cpy(&entry->oid_old, &previous->oid_cur);

	git_oid_cpy(&entry->oid_cur, new_oid);

	if (git_vector_insert(&reflog->entries, entry) < 0)
		goto cleanup;

	return 0;

cleanup:
	git_reflog_entry__free(entry);
	return -1;
}

int git_reflog_rename(git_repository *repo, const char *old_name, const char *new_name)
{
	git_refdb *refdb;
	int error;

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		return -1;

	return refdb->backend->reflog_rename(refdb->backend, old_name, new_name);
}

int git_reflog_delete(git_repository *repo, const char *name)
{
	git_refdb *refdb;
	int error;

	if ((error = git_repository_refdb__weakptr(&refdb, repo)) < 0)
		return -1;

	return refdb->backend->reflog_delete(refdb->backend, name);
}

size_t git_reflog_entrycount(git_reflog *reflog)
{
	assert(reflog);
	return reflog->entries.length;
}

const git_reflog_entry * git_reflog_entry_byindex(const git_reflog *reflog, size_t idx)
{
	assert(reflog);

	if (idx >= reflog->entries.length)
		return NULL;

	return git_vector_get(
		&reflog->entries, reflog_inverse_index(idx, reflog->entries.length));
}

const git_oid * git_reflog_entry_id_old(const git_reflog_entry *entry)
{
	assert(entry);
	return &entry->oid_old;
}

const git_oid * git_reflog_entry_id_new(const git_reflog_entry *entry)
{
	assert(entry);
	return &entry->oid_cur;
}

const git_signature * git_reflog_entry_committer(const git_reflog_entry *entry)
{
	assert(entry);
	return entry->committer;
}

const char * git_reflog_entry_message(const git_reflog_entry *entry)
{
	assert(entry);
	return entry->msg;
}

int git_reflog_drop(git_reflog *reflog, size_t idx, int rewrite_previous_entry)
{
	size_t entrycount;
	git_reflog_entry *entry, *previous;

	entrycount = git_reflog_entrycount(reflog);

	entry = (git_reflog_entry *)git_reflog_entry_byindex(reflog, idx);

	if (entry == NULL) {
		giterr_set(GITERR_REFERENCE, "no reflog entry at index %"PRIuZ, idx);
		return GIT_ENOTFOUND;
	}

	git_reflog_entry__free(entry);

	if (git_vector_remove(
			&reflog->entries, reflog_inverse_index(idx, entrycount)) < 0)
		return -1;

	if (!rewrite_previous_entry)
		return 0;

	/* No need to rewrite anything when removing the most recent entry */
	if (idx == 0)
		return 0;

	/* Have the latest entry just been dropped? */
	if (entrycount == 1)
		return 0;

	entry = (git_reflog_entry *)git_reflog_entry_byindex(reflog, idx - 1);

	/* If the oldest entry has just been removed... */
	if (idx == entrycount - 1) {
		/* ...clear the oid_old member of the "new" oldest entry */
		if (git_oid_fromstr(&entry->oid_old, GIT_OID_HEX_ZERO) < 0)
			return -1;

		return 0;
	}

	previous = (git_reflog_entry *)git_reflog_entry_byindex(reflog, idx);
	git_oid_cpy(&entry->oid_old, &previous->oid_cur);

	return 0;
}
