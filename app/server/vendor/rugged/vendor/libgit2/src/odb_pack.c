/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include <zlib.h>
#include "git2/repository.h"
#include "git2/indexer.h"
#include "git2/sys/odb_backend.h"
#include "fileops.h"
#include "hash.h"
#include "odb.h"
#include "delta-apply.h"
#include "sha1_lookup.h"
#include "mwindow.h"
#include "pack.h"

#include "git2/odb_backend.h"

struct pack_backend {
	git_odb_backend parent;
	git_vector packs;
	struct git_pack_file *last_found;
	char *pack_folder;
};

struct pack_writepack {
	struct git_odb_writepack parent;
	git_indexer *indexer;
};

/**
 * The wonderful tale of a Packed Object lookup query
 * ===================================================
 *	A riveting and epic story of epicness and ASCII
 *			art, presented by yours truly,
 *				Sir Vicent of Marti
 *
 *
 *	Chapter 1: Once upon a time...
 *	Initialization of the Pack Backend
 *	--------------------------------------------------
 *
 *	# git_odb_backend_pack
 *	| Creates the pack backend structure, initializes the
 *	| callback pointers to our default read() and exist() methods,
 *	| and tries to preload all the known packfiles in the ODB.
 * |
 *	|-# packfile_load_all
 *	 | Tries to find the `pack` folder, if it exists. ODBs without
 *	 | a pack folder are ignored altogether. If there's a `pack` folder
 *	 | we run a `dirent` callback through every file in the pack folder
 *	 | to find our packfiles. The packfiles are then sorted according
 *	 | to a sorting callback.
 * 	 |
 *	 |-# packfile_load__cb
 *	 | | This callback is called from `dirent` with every single file
 *	 | | inside the pack folder. We find the packs by actually locating
 *	 | | their index (ends in ".idx"). From that index, we verify that
 *	 | | the corresponding packfile exists and is valid, and if so, we
 *	| | add it to the pack list.
 *	 | |
 *	 | |-# packfile_check
 *	 |		Make sure that there's a packfile to back this index, and store
 *	 |		some very basic information regarding the packfile itself,
 *	 |		such as the full path, the size, and the modification time.
 *	 |		We don't actually open the packfile to check for internal consistency.
 *	|
 *	|-# packfile_sort__cb
 *		Sort all the preloaded packs according to some specific criteria:
 *		we prioritize the "newer" packs because it's more likely they
 *		contain the objects we are looking for, and we prioritize local
 *		packs over remote ones.
 *
 *
 *
 *	Chapter 2: To be, or not to be...
 *	A standard packed `exist` query for an OID
 *	--------------------------------------------------
 *
 * # pack_backend__exists
 * | Check if the given SHA1 oid exists in any of the packs
 * | that have been loaded for our ODB.
 * |
 * |-# pack_entry_find
 *	| Iterate through all the packs that have been preloaded
 *	| (starting by the pack where the latest object was found)
 *	| to try to find the OID in one of them.
 *	|
 *	|-# pack_entry_find1
 *		| Check the index of an individual pack to see if the SHA1
 *		| OID can be found. If we can find the offset to that SHA1
 *		| inside of the index, that means the object is contained
 *		| inside of the packfile and we can stop searching.
 *		| Before returning, we verify that the packfile behing the
 *		| index we are searching still exists on disk.
 *		|
 *		|-# pack_entry_find_offset
 *		| | Mmap the actual index file to disk if it hasn't been opened
 *		| | yet, and run a binary search through it to find the OID.
 *		| | See <http://book.git-scm.com/7_the_packfile.html> for specifics
 *		| | on the Packfile Index format and how do we find entries in it.
 *		| |
 *		| |-# pack_index_open
 *		|	| Guess the name of the index based on the full path to the
 *		|	| packfile, open it and verify its contents. Only if the index
 *		|	| has not been opened already.
 *		|	|
 *		|	|-# pack_index_check
 *		|		Mmap the index file and do a quick run through the header
 *		|		to guess the index version (right now we support v1 and v2),
 *		|		and to verify that the size of the index makes sense.
 *		|
 *		|-# packfile_open
 *			See `packfile_open` in Chapter 3
 *
 *
 *
 *	Chapter 3: The neverending story...
 *	A standard packed `lookup` query for an OID
 *	--------------------------------------------------
 *	TODO
 *
 */


/***********************************************************
 *
 * FORWARD DECLARATIONS
 *
 ***********************************************************/

static int packfile_sort__cb(const void *a_, const void *b_);

static int packfile_load__cb(void *_data, git_buf *path);

static int pack_entry_find(struct git_pack_entry *e,
	struct pack_backend *backend, const git_oid *oid);

/* Can find the offset of an object given
 * a prefix of an identifier.
 * Sets GIT_EAMBIGUOUS if short oid is ambiguous.
 * This method assumes that len is between
 * GIT_OID_MINPREFIXLEN and GIT_OID_HEXSZ.
 */
static int pack_entry_find_prefix(
	struct git_pack_entry *e,
	struct pack_backend *backend,
	const git_oid *short_oid,
	size_t len);



/***********************************************************
 *
 * PACK WINDOW MANAGEMENT
 *
 ***********************************************************/

static int packfile_sort__cb(const void *a_, const void *b_)
{
	const struct git_pack_file *a = a_;
	const struct git_pack_file *b = b_;
	int st;

	/*
	 * Local packs tend to contain objects specific to our
	 * variant of the project than remote ones. In addition,
	 * remote ones could be on a network mounted filesystem.
	 * Favor local ones for these reasons.
	 */
	st = a->pack_local - b->pack_local;
	if (st)
		return -st;

	/*
	 * Younger packs tend to contain more recent objects,
	 * and more recent objects tend to get accessed more
	 * often.
	 */
	if (a->mtime < b->mtime)
		return 1;
	else if (a->mtime == b->mtime)
		return 0;

	return -1;
}


static int packfile_load__cb(void *data, git_buf *path)
{
	struct pack_backend *backend = data;
	struct git_pack_file *pack;
	const char *path_str = git_buf_cstr(path);
	size_t i, cmp_len = git_buf_len(path);
	int error;

	if (cmp_len <= strlen(".idx") || git__suffixcmp(path_str, ".idx") != 0)
		return 0; /* not an index */

	cmp_len -= strlen(".idx");

	for (i = 0; i < backend->packs.length; ++i) {
		struct git_pack_file *p = git_vector_get(&backend->packs, i);

		if (memcmp(p->pack_name, path_str, cmp_len) == 0)
			return 0;
	}

	error = git_mwindow_get_pack(&pack, path->ptr);

	/* ignore missing .pack file as git does */
	if (error == GIT_ENOTFOUND) {
		giterr_clear();
		return 0;
	}

	if (!error)
		error = git_vector_insert(&backend->packs, pack);

	return error;

}

static int pack_entry_find_inner(
	struct git_pack_entry *e,
	struct pack_backend *backend,
	const git_oid *oid,
	struct git_pack_file *last_found)
{
	size_t i;

	if (last_found &&
		git_pack_entry_find(e, last_found, oid, GIT_OID_HEXSZ) == 0)
		return 0;

	for (i = 0; i < backend->packs.length; ++i) {
		struct git_pack_file *p;

		p = git_vector_get(&backend->packs, i);
		if (p == last_found)
			continue;

		if (git_pack_entry_find(e, p, oid, GIT_OID_HEXSZ) == 0) {
			backend->last_found = p;
			return 0;
		}
	}

	return -1;
}

static int pack_entry_find(struct git_pack_entry *e, struct pack_backend *backend, const git_oid *oid)
{
	struct git_pack_file *last_found = backend->last_found;

	if (backend->last_found &&
		git_pack_entry_find(e, backend->last_found, oid, GIT_OID_HEXSZ) == 0)
		return 0;

	if (!pack_entry_find_inner(e, backend, oid, last_found))
		return 0;

	return git_odb__error_notfound("failed to find pack entry", oid);
}

static int pack_entry_find_prefix(
	struct git_pack_entry *e,
	struct pack_backend *backend,
	const git_oid *short_oid,
	size_t len)
{
	int error;
	size_t i;
	git_oid found_full_oid = {{0}};
	bool found = false;
	struct git_pack_file *last_found = backend->last_found;

	if (last_found) {
		error = git_pack_entry_find(e, last_found, short_oid, len);
		if (error == GIT_EAMBIGUOUS)
			return error;
		if (!error) {
			git_oid_cpy(&found_full_oid, &e->sha1);
			found = true;
		}
	}

	for (i = 0; i < backend->packs.length; ++i) {
		struct git_pack_file *p;

		p = git_vector_get(&backend->packs, i);
		if (p == last_found)
			continue;

		error = git_pack_entry_find(e, p, short_oid, len);
		if (error == GIT_EAMBIGUOUS)
			return error;
		if (!error) {
			if (found && git_oid_cmp(&e->sha1, &found_full_oid))
				return git_odb__error_ambiguous("found multiple pack entries");
			git_oid_cpy(&found_full_oid, &e->sha1);
			found = true;
			backend->last_found = p;
		}
	}

	if (!found)
		return git_odb__error_notfound("no matching pack entry for prefix", short_oid);
	else
		return 0;
}


/***********************************************************
 *
 * PACKED BACKEND PUBLIC API
 *
 * Implement the git_odb_backend API calls
 *
 ***********************************************************/
static int pack_backend__refresh(git_odb_backend *backend_)
{
	int error;
	struct stat st;
	git_buf path = GIT_BUF_INIT;
	struct pack_backend *backend = (struct pack_backend *)backend_;

	if (backend->pack_folder == NULL)
		return 0;

	if (p_stat(backend->pack_folder, &st) < 0 || !S_ISDIR(st.st_mode))
		return git_odb__error_notfound("failed to refresh packfiles", NULL);

	git_buf_sets(&path, backend->pack_folder);

	/* reload all packs */
	error = git_path_direach(&path, 0, packfile_load__cb, backend);

	git_buf_free(&path);
	git_vector_sort(&backend->packs);

	return error;
}

static int pack_backend__read_header_internal(
	size_t *len_p, git_otype *type_p,
	struct git_odb_backend *backend, const git_oid *oid)
{
	struct git_pack_entry e;
	int error;

	assert(len_p && type_p && backend && oid);

	if ((error = pack_entry_find(&e, (struct pack_backend *)backend, oid)) < 0)
		return error;

	return git_packfile_resolve_header(len_p, type_p, e.p, e.offset);
}

static int pack_backend__read_header(
	size_t *len_p, git_otype *type_p,
	struct git_odb_backend *backend, const git_oid *oid)
{
	int error;

	error = pack_backend__read_header_internal(len_p, type_p, backend, oid);

	if (error != GIT_ENOTFOUND)
		return error;

	if ((error = pack_backend__refresh(backend)) < 0)
		return error;

	return pack_backend__read_header_internal(len_p, type_p, backend, oid);
}

static int pack_backend__read_internal(
	void **buffer_p, size_t *len_p, git_otype *type_p,
	git_odb_backend *backend, const git_oid *oid)
{
	struct git_pack_entry e;
	git_rawobj raw;
	int error;

	if ((error = pack_entry_find(&e, (struct pack_backend *)backend, oid)) < 0 ||
		(error = git_packfile_unpack(&raw, e.p, &e.offset)) < 0)
		return error;

	*buffer_p = raw.data;
	*len_p = raw.len;
	*type_p = raw.type;

	return 0;
}

static int pack_backend__read(
	void **buffer_p, size_t *len_p, git_otype *type_p,
	git_odb_backend *backend, const git_oid *oid)
{
	int error;

	error = pack_backend__read_internal(buffer_p, len_p, type_p, backend, oid);

	if (error != GIT_ENOTFOUND)
		return error;

	if ((error = pack_backend__refresh(backend)) < 0)
		return error;

	return pack_backend__read_internal(buffer_p, len_p, type_p, backend, oid);
}

static int pack_backend__read_prefix_internal(
	git_oid *out_oid,
	void **buffer_p,
	size_t *len_p,
	git_otype *type_p,
	git_odb_backend *backend,
	const git_oid *short_oid,
	size_t len)
{
	int error = 0;

	if (len < GIT_OID_MINPREFIXLEN)
		error = git_odb__error_ambiguous("prefix length too short");

	else if (len >= GIT_OID_HEXSZ) {
		/* We can fall back to regular read method */
		error = pack_backend__read(buffer_p, len_p, type_p, backend, short_oid);
		if (!error)
			git_oid_cpy(out_oid, short_oid);
	} else {
		struct git_pack_entry e;
		git_rawobj raw;

		if ((error = pack_entry_find_prefix(
				&e, (struct pack_backend *)backend, short_oid, len)) == 0 &&
			(error = git_packfile_unpack(&raw, e.p, &e.offset)) == 0)
		{
			*buffer_p = raw.data;
			*len_p = raw.len;
			*type_p = raw.type;
			git_oid_cpy(out_oid, &e.sha1);
		}
	}

	return error;
}

static int pack_backend__read_prefix(
	git_oid *out_oid,
	void **buffer_p,
	size_t *len_p,
	git_otype *type_p,
	git_odb_backend *backend,
	const git_oid *short_oid,
	size_t len)
{
	int error;

	error = pack_backend__read_prefix_internal(
		out_oid, buffer_p, len_p, type_p, backend, short_oid, len);

	if (error != GIT_ENOTFOUND)
		return error;

	if ((error = pack_backend__refresh(backend)) < 0)
		return error;

	return pack_backend__read_prefix_internal(
		out_oid, buffer_p, len_p, type_p, backend, short_oid, len);
}

static int pack_backend__exists(git_odb_backend *backend, const git_oid *oid)
{
	struct git_pack_entry e;
	int error;

	error = pack_entry_find(&e, (struct pack_backend *)backend, oid);

	if (error != GIT_ENOTFOUND)
		return error == 0;

	if ((error = pack_backend__refresh(backend)) < 0) {
		giterr_clear();
		return (int)false;
	}

	return pack_entry_find(&e, (struct pack_backend *)backend, oid) == 0;
}

static int pack_backend__exists_prefix(
	git_oid *out, git_odb_backend *backend, const git_oid *short_id, size_t len)
{
	int error;
	struct pack_backend *pb = (struct pack_backend *)backend;
	struct git_pack_entry e = {0};

	error = pack_entry_find_prefix(&e, pb, short_id, len);

	if (error == GIT_ENOTFOUND && !(error = pack_backend__refresh(backend)))
		error = pack_entry_find_prefix(&e, pb, short_id, len);

	git_oid_cpy(out, &e.sha1);

	return error;
}

static int pack_backend__foreach(git_odb_backend *_backend, git_odb_foreach_cb cb, void *data)
{
	int error;
	struct git_pack_file *p;
	struct pack_backend *backend;
	unsigned int i;

	assert(_backend && cb);
	backend = (struct pack_backend *)_backend;

	/* Make sure we know about the packfiles */
	if ((error = pack_backend__refresh(_backend)) < 0)
		return error;

	git_vector_foreach(&backend->packs, i, p) {
		if ((error = git_pack_foreach_entry(p, cb, data)) < 0)
			return error;
	}

	return 0;
}

static int pack_backend__writepack_append(struct git_odb_writepack *_writepack, const void *data, size_t size, git_transfer_progress *stats)
{
	struct pack_writepack *writepack = (struct pack_writepack *)_writepack;

	assert(writepack);

	return git_indexer_append(writepack->indexer, data, size, stats);
}

static int pack_backend__writepack_commit(struct git_odb_writepack *_writepack, git_transfer_progress *stats)
{
	struct pack_writepack *writepack = (struct pack_writepack *)_writepack;

	assert(writepack);

	return git_indexer_commit(writepack->indexer, stats);
}

static void pack_backend__writepack_free(struct git_odb_writepack *_writepack)
{
	struct pack_writepack *writepack = (struct pack_writepack *)_writepack;

	assert(writepack);

	git_indexer_free(writepack->indexer);
	git__free(writepack);
}

static int pack_backend__writepack(struct git_odb_writepack **out,
	git_odb_backend *_backend,
        git_odb *odb,
	git_transfer_progress_cb progress_cb,
	void *progress_payload)
{
	struct pack_backend *backend;
	struct pack_writepack *writepack;

	assert(out && _backend);

	*out = NULL;

	backend = (struct pack_backend *)_backend;

	writepack = git__calloc(1, sizeof(struct pack_writepack));
	GITERR_CHECK_ALLOC(writepack);

	if (git_indexer_new(&writepack->indexer,
		backend->pack_folder, 0, odb, progress_cb, progress_payload) < 0) {
		git__free(writepack);
		return -1;
	}

	writepack->parent.backend = _backend;
	writepack->parent.append = pack_backend__writepack_append;
	writepack->parent.commit = pack_backend__writepack_commit;
	writepack->parent.free = pack_backend__writepack_free;

	*out = (git_odb_writepack *)writepack;

	return 0;
}

static void pack_backend__free(git_odb_backend *_backend)
{
	struct pack_backend *backend;
	size_t i;

	assert(_backend);

	backend = (struct pack_backend *)_backend;

	for (i = 0; i < backend->packs.length; ++i) {
		struct git_pack_file *p = git_vector_get(&backend->packs, i);
		git_mwindow_put_pack(p);
	}

	git_vector_free(&backend->packs);
	git__free(backend->pack_folder);
	git__free(backend);
}

static int pack_backend__alloc(struct pack_backend **out, size_t initial_size)
{
	struct pack_backend *backend = git__calloc(1, sizeof(struct pack_backend));
	GITERR_CHECK_ALLOC(backend);

	if (git_vector_init(&backend->packs, initial_size, packfile_sort__cb) < 0) {
		git__free(backend);
		return -1;
	}

	backend->parent.version = GIT_ODB_BACKEND_VERSION;

	backend->parent.read = &pack_backend__read;
	backend->parent.read_prefix = &pack_backend__read_prefix;
	backend->parent.read_header = &pack_backend__read_header;
	backend->parent.exists = &pack_backend__exists;
	backend->parent.exists_prefix = &pack_backend__exists_prefix;
	backend->parent.refresh = &pack_backend__refresh;
	backend->parent.foreach = &pack_backend__foreach;
	backend->parent.writepack = &pack_backend__writepack;
	backend->parent.free = &pack_backend__free;

	*out = backend;
	return 0;
}

int git_odb_backend_one_pack(git_odb_backend **backend_out, const char *idx)
{
	struct pack_backend *backend = NULL;
	struct git_pack_file *packfile = NULL;

	if (pack_backend__alloc(&backend, 1) < 0)
		return -1;

	if (git_mwindow_get_pack(&packfile, idx) < 0 ||
		git_vector_insert(&backend->packs, packfile) < 0)
	{
		pack_backend__free((git_odb_backend *)backend);
		return -1;
	}

	*backend_out = (git_odb_backend *)backend;
	return 0;
}

int git_odb_backend_pack(git_odb_backend **backend_out, const char *objects_dir)
{
	int error = 0;
	struct pack_backend *backend = NULL;
	git_buf path = GIT_BUF_INIT;

	if (git_mwindow_files_init() < 0)
		return -1;

	if (pack_backend__alloc(&backend, 8) < 0)
		return -1;

	if (!(error = git_buf_joinpath(&path, objects_dir, "pack")) &&
		git_path_isdir(git_buf_cstr(&path)))
	{
		backend->pack_folder = git_buf_detach(&path);

		error = pack_backend__refresh((git_odb_backend *)backend);
	}

	if (error < 0) {
		pack_backend__free((git_odb_backend *)backend);
		backend = NULL;
	}

	*backend_out = (git_odb_backend *)backend;

	git_buf_free(&path);

	return error;
}
