/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "refdb_fs.h"

#include "refs.h"
#include "hash.h"
#include "repository.h"
#include "fileops.h"
#include "filebuf.h"
#include "pack.h"
#include "reflog.h"
#include "refdb.h"
#include "iterator.h"
#include "sortedcache.h"
#include "signature.h"

#include <git2/tag.h>
#include <git2/object.h>
#include <git2/refdb.h>
#include <git2/branch.h>
#include <git2/sys/refdb_backend.h>
#include <git2/sys/refs.h>
#include <git2/sys/reflog.h>

#define DEFAULT_NESTING_LEVEL	5
#define MAX_NESTING_LEVEL		10

enum {
	PACKREF_HAS_PEEL = 1,
	PACKREF_WAS_LOOSE = 2,
	PACKREF_CANNOT_PEEL = 4,
	PACKREF_SHADOWED = 8,
};

enum {
	PEELING_NONE = 0,
	PEELING_STANDARD,
	PEELING_FULL
};

struct packref {
	git_oid oid;
	git_oid peel;
	char flags;
	char name[GIT_FLEX_ARRAY];
};

typedef struct refdb_fs_backend {
	git_refdb_backend parent;

	git_repository *repo;
	/* path to git directory */
	char *gitpath;
	/* path to common objects' directory */
	char *commonpath;

	git_sortedcache *refcache;
	int peeling_mode;
	git_iterator_flag_t iterator_flags;
	uint32_t direach_flags;
	int fsync;
} refdb_fs_backend;

static int refdb_reflog_fs__delete(git_refdb_backend *_backend, const char *name);

static int packref_cmp(const void *a_, const void *b_)
{
	const struct packref *a = a_, *b = b_;
	return strcmp(a->name, b->name);
}

static int packed_reload(refdb_fs_backend *backend)
{
	int error;
	git_buf packedrefs = GIT_BUF_INIT;
	char *scan, *eof, *eol;

	if (!backend->gitpath)
		return 0;

	error = git_sortedcache_lockandload(backend->refcache, &packedrefs);

	/*
	 * If we can't find the packed-refs, clear table and return.
	 * Any other error just gets passed through.
	 * If no error, and file wasn't changed, just return.
	 * Anything else means we need to refresh the packed refs.
	 */
	if (error <= 0) {
		if (error == GIT_ENOTFOUND) {
			git_sortedcache_clear(backend->refcache, true);
			giterr_clear();
			error = 0;
		}
		return error;
	}

	/* At this point, refresh the packed refs from the loaded buffer. */

	git_sortedcache_clear(backend->refcache, false);

	scan = (char *)packedrefs.ptr;
	eof  = scan + packedrefs.size;

	backend->peeling_mode = PEELING_NONE;

	if (*scan == '#') {
		static const char *traits_header = "# pack-refs with: ";

		if (git__prefixcmp(scan, traits_header) == 0) {
			scan += strlen(traits_header);
			eol = strchr(scan, '\n');

			if (!eol)
				goto parse_failed;
			*eol = '\0';

			if (strstr(scan, " fully-peeled ") != NULL) {
				backend->peeling_mode = PEELING_FULL;
			} else if (strstr(scan, " peeled ") != NULL) {
				backend->peeling_mode = PEELING_STANDARD;
			}

			scan = eol + 1;
		}
	}

	while (scan < eof && *scan == '#') {
		if (!(eol = strchr(scan, '\n')))
			goto parse_failed;
		scan = eol + 1;
	}

	while (scan < eof) {
		struct packref *ref;
		git_oid oid;

		/* parse "<OID> <refname>\n" */

		if (git_oid_fromstr(&oid, scan) < 0)
			goto parse_failed;
		scan += GIT_OID_HEXSZ;

		if (*scan++ != ' ')
			goto parse_failed;
		if (!(eol = strchr(scan, '\n')))
			goto parse_failed;
		*eol = '\0';
		if (eol[-1] == '\r')
			eol[-1] = '\0';

		if (git_sortedcache_upsert((void **)&ref, backend->refcache, scan) < 0)
			goto parse_failed;
		scan = eol + 1;

		git_oid_cpy(&ref->oid, &oid);

		/* look for optional "^<OID>\n" */

		if (*scan == '^') {
			if (git_oid_fromstr(&oid, scan + 1) < 0)
				goto parse_failed;
			scan += GIT_OID_HEXSZ + 1;

			if (scan < eof) {
				if (!(eol = strchr(scan, '\n')))
					goto parse_failed;
				scan = eol + 1;
			}

			git_oid_cpy(&ref->peel, &oid);
			ref->flags |= PACKREF_HAS_PEEL;
		}
		else if (backend->peeling_mode == PEELING_FULL ||
				(backend->peeling_mode == PEELING_STANDARD &&
				 git__prefixcmp(ref->name, GIT_REFS_TAGS_DIR) == 0))
			ref->flags |= PACKREF_CANNOT_PEEL;
	}

	git_sortedcache_wunlock(backend->refcache);
	git_buf_free(&packedrefs);

	return 0;

parse_failed:
	giterr_set(GITERR_REFERENCE, "corrupted packed references file");

	git_sortedcache_clear(backend->refcache, false);
	git_sortedcache_wunlock(backend->refcache);
	git_buf_free(&packedrefs);

	return -1;
}

static int loose_parse_oid(
	git_oid *oid, const char *filename, git_buf *file_content)
{
	const char *str = git_buf_cstr(file_content);

	if (git_buf_len(file_content) < GIT_OID_HEXSZ)
		goto corrupted;

	/* we need to get 40 OID characters from the file */
	if (git_oid_fromstr(oid, str) < 0)
		goto corrupted;

	/* If the file is longer than 40 chars, the 41st must be a space */
	str += GIT_OID_HEXSZ;
	if (*str == '\0' || git__isspace(*str))
		return 0;

corrupted:
	giterr_set(GITERR_REFERENCE, "corrupted loose reference file: %s", filename);
	return -1;
}

static int loose_readbuffer(git_buf *buf, const char *base, const char *path)
{
	int error;

	/* build full path to file */
	if ((error = git_buf_joinpath(buf, base, path)) < 0 ||
		(error = git_futils_readbuffer(buf, buf->ptr)) < 0)
		git_buf_free(buf);

	return error;
}

static int loose_lookup_to_packfile(refdb_fs_backend *backend, const char *name)
{
	int error = 0;
	git_buf ref_file = GIT_BUF_INIT;
	struct packref *ref = NULL;
	git_oid oid;

	/* if we fail to load the loose reference, assume someone changed
	 * the filesystem under us and skip it...
	 */
	if (loose_readbuffer(&ref_file, backend->gitpath, name) < 0) {
		giterr_clear();
		goto done;
	}

	/* skip symbolic refs */
	if (!git__prefixcmp(git_buf_cstr(&ref_file), GIT_SYMREF))
		goto done;

	/* parse OID from file */
	if ((error = loose_parse_oid(&oid, name, &ref_file)) < 0)
		goto done;

	git_sortedcache_wlock(backend->refcache);

	if (!(error = git_sortedcache_upsert(
			(void **)&ref, backend->refcache, name))) {

		git_oid_cpy(&ref->oid, &oid);
		ref->flags = PACKREF_WAS_LOOSE;
	}

	git_sortedcache_wunlock(backend->refcache);

done:
	git_buf_free(&ref_file);
	return error;
}

static int _dirent_loose_load(void *payload, git_buf *full_path)
{
	refdb_fs_backend *backend = payload;
	const char *file_path;

	if (git__suffixcmp(full_path->ptr, ".lock") == 0)
		return 0;

	if (git_path_isdir(full_path->ptr)) {
		int error = git_path_direach(
			full_path, backend->direach_flags, _dirent_loose_load, backend);
		/* Race with the filesystem, ignore it */
		if (error == GIT_ENOTFOUND) {
			giterr_clear();
			return 0;
		}

		return error;
	}

	file_path = full_path->ptr + strlen(backend->gitpath);

	return loose_lookup_to_packfile(backend, file_path);
}

/*
 * Load all the loose references from the repository
 * into the in-memory Packfile, and build a vector with
 * all the references so it can be written back to
 * disk.
 */
static int packed_loadloose(refdb_fs_backend *backend)
{
	int error;
	git_buf refs_path = GIT_BUF_INIT;

	if (git_buf_joinpath(&refs_path, backend->gitpath, GIT_REFS_DIR) < 0)
		return -1;

	/*
	 * Load all the loose files from disk into the Packfile table.
	 * This will overwrite any old packed entries with their
	 * updated loose versions
	 */
	error = git_path_direach(
		&refs_path, backend->direach_flags, _dirent_loose_load, backend);

	git_buf_free(&refs_path);

	return error;
}

static int refdb_fs_backend__exists(
	int *exists,
	git_refdb_backend *_backend,
	const char *ref_name)
{
	refdb_fs_backend *backend = (refdb_fs_backend *)_backend;
	git_buf ref_path = GIT_BUF_INIT;
	int error;

	assert(backend);

	if ((error = packed_reload(backend)) < 0 ||
		(error = git_buf_joinpath(&ref_path, backend->gitpath, ref_name)) < 0)
		return error;

	*exists = git_path_isfile(ref_path.ptr) ||
		(git_sortedcache_lookup(backend->refcache, ref_name) != NULL);

	git_buf_free(&ref_path);
	return 0;
}

static const char *loose_parse_symbolic(git_buf *file_content)
{
	const unsigned int header_len = (unsigned int)strlen(GIT_SYMREF);
	const char *refname_start;

	refname_start = (const char *)file_content->ptr;

	if (git_buf_len(file_content) < header_len + 1) {
		giterr_set(GITERR_REFERENCE, "corrupted loose reference file");
		return NULL;
	}

	/*
	 * Assume we have already checked for the header
	 * before calling this function
	 */
	refname_start += header_len;

	return refname_start;
}

/*
 * Returns whether a reference is stored per worktree or not.
 * Per-worktree references are:
 *
 * - all pseudorefs, e.g. HEAD and MERGE_HEAD
 * - all references stored inside of "refs/bisect/"
 */
static bool is_per_worktree_ref(const char *ref_name)
{
	return git__prefixcmp(ref_name, "refs/") != 0 ||
	    git__prefixcmp(ref_name, "refs/bisect/") == 0;
}

static int loose_lookup(
	git_reference **out,
	refdb_fs_backend *backend,
	const char *ref_name)
{
	git_buf ref_file = GIT_BUF_INIT;
	int error = 0;
	const char *ref_dir;

	if (out)
		*out = NULL;

	if (is_per_worktree_ref(ref_name))
		ref_dir = backend->gitpath;
	else
		ref_dir = backend->commonpath;

	if ((error = loose_readbuffer(&ref_file, ref_dir, ref_name)) < 0)
		/* cannot read loose ref file - gah */;
	else if (git__prefixcmp(git_buf_cstr(&ref_file), GIT_SYMREF) == 0) {
		const char *target;

		git_buf_rtrim(&ref_file);

		if (!(target = loose_parse_symbolic(&ref_file)))
			error = -1;
		else if (out != NULL)
			*out = git_reference__alloc_symbolic(ref_name, target);
	} else {
		git_oid oid;

		if (!(error = loose_parse_oid(&oid, ref_name, &ref_file)) &&
			out != NULL)
			*out = git_reference__alloc(ref_name, &oid, NULL);
	}

	git_buf_free(&ref_file);
	return error;
}

static int ref_error_notfound(const char *name)
{
	giterr_set(GITERR_REFERENCE, "reference '%s' not found", name);
	return GIT_ENOTFOUND;
}

static int packed_lookup(
	git_reference **out,
	refdb_fs_backend *backend,
	const char *ref_name)
{
	int error = 0;
	struct packref *entry;

	if ((error = packed_reload(backend)) < 0)
		return error;

	if (git_sortedcache_rlock(backend->refcache) < 0)
		return -1;

	entry = git_sortedcache_lookup(backend->refcache, ref_name);
	if (!entry) {
		error = ref_error_notfound(ref_name);
	} else {
		*out = git_reference__alloc(ref_name, &entry->oid, &entry->peel);
		if (!*out)
			error = -1;
	}

	git_sortedcache_runlock(backend->refcache);

	return error;
}

static int refdb_fs_backend__lookup(
	git_reference **out,
	git_refdb_backend *_backend,
	const char *ref_name)
{
	refdb_fs_backend *backend = (refdb_fs_backend *)_backend;
	int error;

	assert(backend);

	if (!(error = loose_lookup(out, backend, ref_name)))
		return 0;

	/* only try to lookup this reference on the packfile if it
	 * wasn't found on the loose refs; not if there was a critical error */
	if (error == GIT_ENOTFOUND) {
		giterr_clear();
		error = packed_lookup(out, backend, ref_name);
	}

	return error;
}

typedef struct {
	git_reference_iterator parent;

	char *glob;

	git_pool pool;
	git_vector loose;

	git_sortedcache *cache;
	size_t loose_pos;
	size_t packed_pos;
} refdb_fs_iter;

static void refdb_fs_backend__iterator_free(git_reference_iterator *_iter)
{
	refdb_fs_iter *iter = (refdb_fs_iter *) _iter;

	git_vector_free(&iter->loose);
	git_pool_clear(&iter->pool);
	git_sortedcache_free(iter->cache);
	git__free(iter);
}

static int iter_load_loose_paths(refdb_fs_backend *backend, refdb_fs_iter *iter)
{
	int error = 0;
	git_buf path = GIT_BUF_INIT;
	git_iterator *fsit = NULL;
	git_iterator_options fsit_opts = GIT_ITERATOR_OPTIONS_INIT;
	const git_index_entry *entry = NULL;

	if (!backend->commonpath) /* do nothing if no commonpath for loose refs */
		return 0;

	fsit_opts.flags = backend->iterator_flags;

	if ((error = git_buf_printf(&path, "%s/refs", backend->commonpath)) < 0 ||
		(error = git_iterator_for_filesystem(&fsit, path.ptr, &fsit_opts)) < 0) {
		git_buf_free(&path);
		return error;
	}

	error = git_buf_sets(&path, GIT_REFS_DIR);

	while (!error && !git_iterator_advance(&entry, fsit)) {
		const char *ref_name;
		struct packref *ref;
		char *ref_dup;

		git_buf_truncate(&path, strlen(GIT_REFS_DIR));
		git_buf_puts(&path, entry->path);
		ref_name = git_buf_cstr(&path);

		if (git__suffixcmp(ref_name, ".lock") == 0 ||
			(iter->glob && p_fnmatch(iter->glob, ref_name, 0) != 0))
			continue;

		git_sortedcache_rlock(backend->refcache);
		ref = git_sortedcache_lookup(backend->refcache, ref_name);
		if (ref)
			ref->flags |= PACKREF_SHADOWED;
		git_sortedcache_runlock(backend->refcache);

		ref_dup = git_pool_strdup(&iter->pool, ref_name);
		if (!ref_dup)
			error = -1;
		else
			error = git_vector_insert(&iter->loose, ref_dup);
	}

	git_iterator_free(fsit);
	git_buf_free(&path);

	return error;
}

static int refdb_fs_backend__iterator_next(
	git_reference **out, git_reference_iterator *_iter)
{
	int error = GIT_ITEROVER;
	refdb_fs_iter *iter = (refdb_fs_iter *)_iter;
	refdb_fs_backend *backend = (refdb_fs_backend *)iter->parent.db->backend;
	struct packref *ref;

	while (iter->loose_pos < iter->loose.length) {
		const char *path = git_vector_get(&iter->loose, iter->loose_pos++);

		if (loose_lookup(out, backend, path) == 0)
			return 0;

		giterr_clear();
	}

	if (!iter->cache) {
		if ((error = git_sortedcache_copy(&iter->cache, backend->refcache, 1, NULL, NULL)) < 0)
			return error;
	}

	error = GIT_ITEROVER;
	while (iter->packed_pos < git_sortedcache_entrycount(iter->cache)) {
		ref = git_sortedcache_entry(iter->cache, iter->packed_pos++);
		if (!ref) /* stop now if another thread deleted refs and we past end */
			break;

		if (ref->flags & PACKREF_SHADOWED)
			continue;
		if (iter->glob && p_fnmatch(iter->glob, ref->name, 0) != 0)
			continue;

		*out = git_reference__alloc(ref->name, &ref->oid, &ref->peel);
		error = (*out != NULL) ? 0 : -1;
		break;
	}

	return error;
}

static int refdb_fs_backend__iterator_next_name(
	const char **out, git_reference_iterator *_iter)
{
	int error = GIT_ITEROVER;
	refdb_fs_iter *iter = (refdb_fs_iter *)_iter;
	refdb_fs_backend *backend = (refdb_fs_backend *)iter->parent.db->backend;
	struct packref *ref;

	while (iter->loose_pos < iter->loose.length) {
		const char *path = git_vector_get(&iter->loose, iter->loose_pos++);

		if (loose_lookup(NULL, backend, path) == 0) {
			*out = path;
			return 0;
		}

		giterr_clear();
	}

	if (!iter->cache) {
		if ((error = git_sortedcache_copy(&iter->cache, backend->refcache, 1, NULL, NULL)) < 0)
			return error;
	}

	error = GIT_ITEROVER;
	while (iter->packed_pos < git_sortedcache_entrycount(iter->cache)) {
		ref = git_sortedcache_entry(iter->cache, iter->packed_pos++);
		if (!ref) /* stop now if another thread deleted refs and we past end */
			break;

		if (ref->flags & PACKREF_SHADOWED)
			continue;
		if (iter->glob && p_fnmatch(iter->glob, ref->name, 0) != 0)
			continue;

		*out = ref->name;
		error = 0;
		break;
	}

	return error;
}

static int refdb_fs_backend__iterator(
	git_reference_iterator **out, git_refdb_backend *_backend, const char *glob)
{
	int error;
	refdb_fs_iter *iter;
	refdb_fs_backend *backend = (refdb_fs_backend *)_backend;

	assert(backend);

	if ((error = packed_reload(backend)) < 0)
		return error;

	iter = git__calloc(1, sizeof(refdb_fs_iter));
	GITERR_CHECK_ALLOC(iter);

	git_pool_init(&iter->pool, 1);

	if (git_vector_init(&iter->loose, 8, NULL) < 0)
		goto fail;

	if (glob != NULL &&
		(iter->glob = git_pool_strdup(&iter->pool, glob)) == NULL)
		goto fail;

	iter->parent.next = refdb_fs_backend__iterator_next;
	iter->parent.next_name = refdb_fs_backend__iterator_next_name;
	iter->parent.free = refdb_fs_backend__iterator_free;

	if (iter_load_loose_paths(backend, iter) < 0)
		goto fail;

	*out = (git_reference_iterator *)iter;
	return 0;

fail:
	refdb_fs_backend__iterator_free((git_reference_iterator *)iter);
	return -1;
}

static bool ref_is_available(
	const char *old_ref, const char *new_ref, const char *this_ref)
{
	if (old_ref == NULL || strcmp(old_ref, this_ref)) {
		size_t reflen = strlen(this_ref);
		size_t newlen = strlen(new_ref);
		size_t cmplen = reflen < newlen ? reflen : newlen;
		const char *lead = reflen < newlen ? new_ref : this_ref;

		if (!strncmp(new_ref, this_ref, cmplen) && lead[cmplen] == '/') {
			return false;
		}
	}

	return true;
}

static int reference_path_available(
	refdb_fs_backend *backend,
	const char *new_ref,
	const char* old_ref,
	int force)
{
	size_t i;
	int error;

	if ((error = packed_reload(backend)) < 0)
		return error;

	if (!force) {
		int exists;

		if ((error = refdb_fs_backend__exists(
			&exists, (git_refdb_backend *)backend, new_ref)) < 0) {
			return error;
		}

		if (exists) {
			giterr_set(GITERR_REFERENCE,
				"failed to write reference '%s': a reference with "
				"that name already exists.", new_ref);
			return GIT_EEXISTS;
		}
	}

	git_sortedcache_rlock(backend->refcache);

	for (i = 0; i < git_sortedcache_entrycount(backend->refcache); ++i) {
		struct packref *ref = git_sortedcache_entry(backend->refcache, i);

		if (ref && !ref_is_available(old_ref, new_ref, ref->name)) {
			git_sortedcache_runlock(backend->refcache);
			giterr_set(GITERR_REFERENCE,
				"path to reference '%s' collides with existing one", new_ref);
			return -1;
		}
	}

	git_sortedcache_runlock(backend->refcache);
	return 0;
}

static int loose_lock(git_filebuf *file, refdb_fs_backend *backend, const char *name)
{
	int error, filebuf_flags;
	git_buf ref_path = GIT_BUF_INIT;
	const char *basedir;

	assert(file && backend && name);

	if (!git_path_isvalid(backend->repo, name, GIT_PATH_REJECT_FILESYSTEM_DEFAULTS)) {
		giterr_set(GITERR_INVALID, "invalid reference name '%s'", name);
		return GIT_EINVALIDSPEC;
	}

	if (is_per_worktree_ref(name))
		basedir = backend->gitpath;
	else
		basedir = backend->commonpath;

	/* Remove a possibly existing empty directory hierarchy
	 * which name would collide with the reference name
	 */
	if ((error = git_futils_rmdir_r(name, basedir, GIT_RMDIR_SKIP_NONEMPTY)) < 0)
		return error;

	if (git_buf_joinpath(&ref_path, basedir, name) < 0)
		return -1;

	filebuf_flags = GIT_FILEBUF_FORCE;
	if (backend->fsync)
		filebuf_flags |= GIT_FILEBUF_FSYNC;

	error = git_filebuf_open(file, ref_path.ptr, filebuf_flags, GIT_REFS_FILE_MODE);

	if (error == GIT_EDIRECTORY)
		giterr_set(GITERR_REFERENCE, "cannot lock ref '%s', there are refs beneath that folder", name);

	git_buf_free(&ref_path);
	return error;
}

static int loose_commit(git_filebuf *file, const git_reference *ref)
{
	assert(file && ref);

	if (ref->type == GIT_REF_OID) {
		char oid[GIT_OID_HEXSZ + 1];
		git_oid_nfmt(oid, sizeof(oid), &ref->target.oid);

		git_filebuf_printf(file, "%s\n", oid);
	} else if (ref->type == GIT_REF_SYMBOLIC) {
		git_filebuf_printf(file, GIT_SYMREF "%s\n", ref->target.symbolic);
	} else {
		assert(0); /* don't let this happen */
	}

	return git_filebuf_commit(file);
}

static int refdb_fs_backend__lock(void **out, git_refdb_backend *_backend, const char *refname)
{
	int error;
	git_filebuf *lock;
	refdb_fs_backend *backend = (refdb_fs_backend *) _backend;

	lock = git__calloc(1, sizeof(git_filebuf));
	GITERR_CHECK_ALLOC(lock);

	if ((error = loose_lock(lock, backend, refname)) < 0) {
		git__free(lock);
		return error;
	}

	*out = lock;
	return 0;
}

static int refdb_fs_backend__write_tail(
	git_refdb_backend *_backend,
	const git_reference *ref,
	git_filebuf *file,
	int update_reflog,
	const git_signature *who,
	const char *message,
	const git_oid *old_id,
	const char *old_target);

static int refdb_fs_backend__delete_tail(
	git_refdb_backend *_backend,
	git_filebuf *file,
	const char *ref_name,
	const git_oid *old_id, const char *old_target);

static int refdb_fs_backend__unlock(git_refdb_backend *backend, void *payload, int success, int update_reflog,
				    const git_reference *ref, const git_signature *sig, const char *message)
{
	git_filebuf *lock = (git_filebuf *) payload;
	int error = 0;

	if (success == 2)
		error = refdb_fs_backend__delete_tail(backend, lock, ref->name, NULL, NULL);
	else if (success)
		error = refdb_fs_backend__write_tail(backend, ref, lock, update_reflog, sig, message, NULL, NULL);
	else
		git_filebuf_cleanup(lock);

	git__free(lock);
	return error;
}

/*
 * Find out what object this reference resolves to.
 *
 * For references that point to a 'big' tag (e.g. an
 * actual tag object on the repository), we need to
 * cache on the packfile the OID of the object to
 * which that 'big tag' is pointing to.
 */
static int packed_find_peel(refdb_fs_backend *backend, struct packref *ref)
{
	git_object *object;

	if (ref->flags & PACKREF_HAS_PEEL || ref->flags & PACKREF_CANNOT_PEEL)
		return 0;

	/*
	 * Find the tagged object in the repository
	 */
	if (git_object_lookup(&object, backend->repo, &ref->oid, GIT_OBJ_ANY) < 0)
		return -1;

	/*
	 * If the tagged object is a Tag object, we need to resolve it;
	 * if the ref is actually a 'weak' ref, we don't need to resolve
	 * anything.
	 */
	if (git_object_type(object) == GIT_OBJ_TAG) {
		git_tag *tag = (git_tag *)object;

		/*
		 * Find the object pointed at by this tag
		 */
		git_oid_cpy(&ref->peel, git_tag_target_id(tag));
		ref->flags |= PACKREF_HAS_PEEL;

		/*
		 * The reference has now cached the resolved OID, and is
		 * marked at such. When written to the packfile, it'll be
		 * accompanied by this resolved oid
		 */
	}

	git_object_free(object);
	return 0;
}

/*
 * Write a single reference into a packfile
 */
static int packed_write_ref(struct packref *ref, git_filebuf *file)
{
	char oid[GIT_OID_HEXSZ + 1];
	git_oid_nfmt(oid, sizeof(oid), &ref->oid);

	/*
	 * For references that peel to an object in the repo, we must
	 * write the resulting peel on a separate line, e.g.
	 *
	 *	6fa8a902cc1d18527e1355773c86721945475d37 refs/tags/libgit2-0.4
	 *	^2ec0cb7959b0bf965d54f95453f5b4b34e8d3100
	 *
	 * This obviously only applies to tags.
	 * The required peels have already been loaded into `ref->peel_target`.
	 */
	if (ref->flags & PACKREF_HAS_PEEL) {
		char peel[GIT_OID_HEXSZ + 1];
		git_oid_nfmt(peel, sizeof(peel), &ref->peel);

		if (git_filebuf_printf(file, "%s %s\n^%s\n", oid, ref->name, peel) < 0)
			return -1;
	} else {
		if (git_filebuf_printf(file, "%s %s\n", oid, ref->name) < 0)
			return -1;
	}

	return 0;
}

/*
 * Remove all loose references
 *
 * Once we have successfully written a packfile,
 * all the loose references that were packed must be
 * removed from disk.
 *
 * This is a dangerous method; make sure the packfile
 * is well-written, because we are destructing references
 * here otherwise.
 */
static int packed_remove_loose(refdb_fs_backend *backend)
{
	size_t i;
	git_filebuf lock = GIT_FILEBUF_INIT;
	git_buf ref_content = GIT_BUF_INIT;
	int error = 0;

	/* backend->refcache is already locked when this is called */

	for (i = 0; i < git_sortedcache_entrycount(backend->refcache); ++i) {
		struct packref *ref = git_sortedcache_entry(backend->refcache, i);
		git_oid current_id;

		if (!ref || !(ref->flags & PACKREF_WAS_LOOSE))
			continue;

		git_filebuf_cleanup(&lock);

		/* We need to stop anybody from updating the ref while we try to do a safe delete */
		error = loose_lock(&lock, backend, ref->name);
		/* If someone else is updating it, let them do it */
		if (error == GIT_EEXISTS || error == GIT_ENOTFOUND)
			continue;

		if (error < 0) {
			git_buf_free(&ref_content);
			giterr_set(GITERR_REFERENCE, "failed to lock loose reference '%s'", ref->name);
			return error;
		}

		error = git_futils_readbuffer(&ref_content, lock.path_original);
		/* Someone else beat us to cleaning up the ref, let's simply continue */
		if (error == GIT_ENOTFOUND)
			continue;

		/* This became a symref between us packing and trying to delete it, so ignore it */
		if (!git__prefixcmp(ref_content.ptr, GIT_SYMREF))
			continue;

		/* Figure out the current id; if we find a bad ref file, skip it so we can do the rest */
		if (loose_parse_oid(&current_id, lock.path_original, &ref_content) < 0)
			continue;

		/* If the ref moved since we packed it, we must not delete it */
		if (!git_oid_equal(&current_id, &ref->oid))
			continue;

		/*
		 * if we fail to remove a single file, this is *not* good,
		 * but we should keep going and remove as many as possible.
		 * If we fail to remove, the ref is still in the old state, so
		 * we haven't lost information.
		 */
		p_unlink(lock.path_original);
	}

	git_buf_free(&ref_content);
	git_filebuf_cleanup(&lock);
	return 0;
}

/*
 * Write all the contents in the in-memory packfile to disk.
 */
static int packed_write(refdb_fs_backend *backend)
{
	git_sortedcache *refcache = backend->refcache;
	git_filebuf pack_file = GIT_FILEBUF_INIT;
	int error, open_flags = 0;
	size_t i;

	/* lock the cache to updates while we do this */
	if ((error = git_sortedcache_wlock(refcache)) < 0)
		return error;

	if (backend->fsync)
		open_flags = GIT_FILEBUF_FSYNC;

	/* Open the file! */
	if ((error = git_filebuf_open(&pack_file, git_sortedcache_path(refcache), open_flags, GIT_PACKEDREFS_FILE_MODE)) < 0)
		goto fail;

	/* Packfiles have a header... apparently
	 * This is in fact not required, but we might as well print it
	 * just for kicks */
	if ((error = git_filebuf_printf(&pack_file, "%s\n", GIT_PACKEDREFS_HEADER)) < 0)
		goto fail;

	for (i = 0; i < git_sortedcache_entrycount(refcache); ++i) {
		struct packref *ref = git_sortedcache_entry(refcache, i);
		assert(ref);

		if ((error = packed_find_peel(backend, ref)) < 0)
			goto fail;

		if ((error = packed_write_ref(ref, &pack_file)) < 0)
			goto fail;
	}

	/* if we've written all the references properly, we can commit
	 * the packfile to make the changes effective */
	if ((error = git_filebuf_commit(&pack_file)) < 0)
		goto fail;

	/* when and only when the packfile has been properly written,
	 * we can go ahead and remove the loose refs */
	if ((error = packed_remove_loose(backend)) < 0)
		goto fail;

	git_sortedcache_updated(refcache);
	git_sortedcache_wunlock(refcache);

	/* we're good now */
	return 0;

fail:
	git_filebuf_cleanup(&pack_file);
	git_sortedcache_wunlock(refcache);

	return error;
}

static int reflog_append(refdb_fs_backend *backend, const git_reference *ref, const git_oid *old, const git_oid *new, const git_signature *author, const char *message);
static int has_reflog(git_repository *repo, const char *name);

/* We only write if it's under heads/, remotes/ or notes/ or if it already has a log */
static int should_write_reflog(int *write, git_repository *repo, const char *name)
{
	int error, logall;

	error = git_repository__cvar(&logall, repo, GIT_CVAR_LOGALLREFUPDATES);
	if (error < 0)
		return error;

	/* Defaults to the opposite of the repo being bare */
	if (logall == GIT_LOGALLREFUPDATES_UNSET)
		logall = !git_repository_is_bare(repo);

	if (!logall) {
		*write = 0;
	} else if (has_reflog(repo, name)) {
		*write = 1;
	} else if (!git__prefixcmp(name, GIT_REFS_HEADS_DIR) ||
		   !git__strcmp(name, GIT_HEAD_FILE) ||
		   !git__prefixcmp(name, GIT_REFS_REMOTES_DIR) ||
		   !git__prefixcmp(name, GIT_REFS_NOTES_DIR)) {
		*write = 1;
	} else {
		*write = 0;
	}

	return 0;
}

static int cmp_old_ref(int *cmp, git_refdb_backend *backend, const char *name,
	const git_oid *old_id, const char *old_target)
{
	int error = 0;
	git_reference *old_ref = NULL;

	*cmp = 0;
	/* It "matches" if there is no old value to compare against */
	if (!old_id && !old_target)
		return 0;

	if ((error = refdb_fs_backend__lookup(&old_ref, backend, name)) < 0)
		goto out;

	/* If the types don't match, there's no way the values do */
	if (old_id && old_ref->type != GIT_REF_OID) {
		*cmp = -1;
		goto out;
	}
	if (old_target && old_ref->type != GIT_REF_SYMBOLIC) {
		*cmp = 1;
		goto out;
	}

	if (old_id && old_ref->type == GIT_REF_OID)
		*cmp = git_oid_cmp(old_id, &old_ref->target.oid);

	if (old_target && old_ref->type == GIT_REF_SYMBOLIC)
		*cmp = git__strcmp(old_target, old_ref->target.symbolic);

out:
	git_reference_free(old_ref);

	return error;
}

/*
 * The git.git comment regarding this, for your viewing pleasure:
 *
 * Special hack: If a branch is updated directly and HEAD
 * points to it (may happen on the remote side of a push
 * for example) then logically the HEAD reflog should be
 * updated too.
 * A generic solution implies reverse symref information,
 * but finding all symrefs pointing to the given branch
 * would be rather costly for this rare event (the direct
 * update of a branch) to be worth it.  So let's cheat and
 * check with HEAD only which should cover 99% of all usage
 * scenarios (even 100% of the default ones).
 */
static int maybe_append_head(refdb_fs_backend *backend, const git_reference *ref, const git_signature *who, const char *message)
{
	int error;
	git_oid old_id;
	git_reference *tmp = NULL, *head = NULL, *peeled = NULL;
	const char *name;

	if (ref->type == GIT_REF_SYMBOLIC)
		return 0;

	/* if we can't resolve, we use {0}*40 as old id */
	if (git_reference_name_to_id(&old_id, backend->repo, ref->name) < 0)
		memset(&old_id, 0, sizeof(old_id));

	if ((error = git_reference_lookup(&head, backend->repo, GIT_HEAD_FILE)) < 0)
		return error;

	if (git_reference_type(head) == GIT_REF_OID)
		goto cleanup;

	if ((error = git_reference_lookup(&tmp, backend->repo, GIT_HEAD_FILE)) < 0)
		goto cleanup;

	/* Go down the symref chain until we find the branch */
	while (git_reference_type(tmp) == GIT_REF_SYMBOLIC) {
		error = git_reference_lookup(&peeled, backend->repo, git_reference_symbolic_target(tmp));
		if (error < 0)
			break;

		git_reference_free(tmp);
		tmp = peeled;
	}

	if (error == GIT_ENOTFOUND) {
		error = 0;
		name = git_reference_symbolic_target(tmp);
	} else if (error < 0) {
		goto cleanup;
	} else {
		name = git_reference_name(tmp);
	}

	if (strcmp(name, ref->name))
		goto cleanup;

	error = reflog_append(backend, head, &old_id, git_reference_target(ref), who, message);

cleanup:
	git_reference_free(tmp);
	git_reference_free(head);
	return error;
}

static int refdb_fs_backend__write(
	git_refdb_backend *_backend,
	const git_reference *ref,
	int force,
	const git_signature *who,
	const char *message,
	const git_oid *old_id,
	const char *old_target)
{
	refdb_fs_backend *backend = (refdb_fs_backend *)_backend;
	git_filebuf file = GIT_FILEBUF_INIT;
	int error = 0;

	assert(backend);

	if ((error = reference_path_available(backend, ref->name, NULL, force)) < 0)
		return error;

	/* We need to perform the reflog append and old value check under the ref's lock */
	if ((error = loose_lock(&file, backend, ref->name)) < 0)
		return error;

	return refdb_fs_backend__write_tail(_backend, ref, &file, true, who, message, old_id, old_target);
}

static int refdb_fs_backend__write_tail(
	git_refdb_backend *_backend,
	const git_reference *ref,
	git_filebuf *file,
	int update_reflog,
	const git_signature *who,
	const char *message,
	const git_oid *old_id,
	const char *old_target)
{
	refdb_fs_backend *backend = (refdb_fs_backend *)_backend;
	int error = 0, cmp = 0, should_write;
	const char *new_target = NULL;
	const git_oid *new_id = NULL;

	if ((error = cmp_old_ref(&cmp, _backend, ref->name, old_id, old_target)) < 0)
		goto on_error;

	if (cmp) {
		giterr_set(GITERR_REFERENCE, "old reference value does not match");
		error = GIT_EMODIFIED;
		goto on_error;
	}

	if (ref->type == GIT_REF_SYMBOLIC)
		new_target = ref->target.symbolic;
	else
		new_id = &ref->target.oid;

	error = cmp_old_ref(&cmp, _backend, ref->name, new_id, new_target);
	if (error < 0 && error != GIT_ENOTFOUND)
		goto on_error;

	/* Don't update if we have the same value */
	if (!error && !cmp) {
		error = 0;
		goto on_error; /* not really error */
	}

	if (update_reflog) {
		if ((error = should_write_reflog(&should_write, backend->repo, ref->name)) < 0)
			goto on_error;

		if (should_write) {
			if ((error = reflog_append(backend, ref, NULL, NULL, who, message)) < 0)
				goto on_error;
			if ((error = maybe_append_head(backend, ref, who, message)) < 0)
				goto on_error;
		}
	}

	return loose_commit(file, ref);

on_error:
        git_filebuf_cleanup(file);
        return error;
}

static int refdb_fs_backend__delete(
	git_refdb_backend *_backend,
	const char *ref_name,
	const git_oid *old_id, const char *old_target)
{
	refdb_fs_backend *backend = (refdb_fs_backend *)_backend;
	git_filebuf file = GIT_FILEBUF_INIT;
	int error = 0;

	assert(backend && ref_name);

	if ((error = loose_lock(&file, backend, ref_name)) < 0)
		return error;

	if ((error = refdb_reflog_fs__delete(_backend, ref_name)) < 0) {
		git_filebuf_cleanup(&file);
		return error;
	}

	return refdb_fs_backend__delete_tail(_backend, &file, ref_name, old_id, old_target);
}

static int refdb_fs_backend__delete_tail(
	git_refdb_backend *_backend,
	git_filebuf *file,
	const char *ref_name,
	const git_oid *old_id, const char *old_target)
{
	refdb_fs_backend *backend = (refdb_fs_backend *)_backend;
	git_buf loose_path = GIT_BUF_INIT;
	size_t pack_pos;
	int error = 0, cmp = 0;
	bool loose_deleted = 0;

	error = cmp_old_ref(&cmp, _backend, ref_name, old_id, old_target);
	if (error < 0)
		goto cleanup;

	if (cmp) {
		giterr_set(GITERR_REFERENCE, "old reference value does not match");
		error = GIT_EMODIFIED;
		goto cleanup;
	}

	/* If a loose reference exists, remove it from the filesystem */
	if (git_buf_joinpath(&loose_path, backend->gitpath, ref_name) < 0)
		return -1;


	error = p_unlink(loose_path.ptr);
	if (error < 0 && errno == ENOENT)
		error = 0;
	else if (error < 0)
		goto cleanup;
	else if (error == 0)
		loose_deleted = 1;

	if ((error = packed_reload(backend)) < 0)
		goto cleanup;

	/* If a packed reference exists, remove it from the packfile and repack */
	if ((error = git_sortedcache_wlock(backend->refcache)) < 0)
		goto cleanup;

	if (!(error = git_sortedcache_lookup_index(
			&pack_pos, backend->refcache, ref_name)))
		error = git_sortedcache_remove(backend->refcache, pack_pos);

	git_sortedcache_wunlock(backend->refcache);

	if (error == GIT_ENOTFOUND) {
		error = loose_deleted ? 0 : ref_error_notfound(ref_name);
		goto cleanup;
	}

	error = packed_write(backend);

cleanup:
	git_buf_free(&loose_path);
	git_filebuf_cleanup(file);

	return error;
}

static int refdb_reflog_fs__rename(git_refdb_backend *_backend, const char *old_name, const char *new_name);

static int refdb_fs_backend__rename(
	git_reference **out,
	git_refdb_backend *_backend,
	const char *old_name,
	const char *new_name,
	int force,
	const git_signature *who,
	const char *message)
{
	refdb_fs_backend *backend = (refdb_fs_backend *)_backend;
	git_reference *old, *new;
	git_filebuf file = GIT_FILEBUF_INIT;
	int error;

	assert(backend);

	if ((error = reference_path_available(
			backend, new_name, old_name, force)) < 0 ||
		(error = refdb_fs_backend__lookup(&old, _backend, old_name)) < 0)
		return error;

	if ((error = refdb_fs_backend__delete(_backend, old_name, NULL, NULL)) < 0) {
		git_reference_free(old);
		return error;
	}

	new = git_reference__set_name(old, new_name);
	if (!new) {
		git_reference_free(old);
		return -1;
	}

	if ((error = loose_lock(&file, backend, new->name)) < 0) {
		git_reference_free(new);
		return error;
	}

	/* Try to rename the refog; it's ok if the old doesn't exist */
	error = refdb_reflog_fs__rename(_backend, old_name, new_name);
	if (((error == 0) || (error == GIT_ENOTFOUND)) &&
	    ((error = reflog_append(backend, new, git_reference_target(new), NULL, who, message)) < 0)) {
		git_reference_free(new);
		git_filebuf_cleanup(&file);
		return error;
	}

	if (error < 0) {
		git_reference_free(new);
		git_filebuf_cleanup(&file);
		return error;
	}


	if ((error = loose_commit(&file, new)) < 0 || out == NULL) {
		git_reference_free(new);
		return error;
	}

	*out = new;
	return 0;
}

static int refdb_fs_backend__compress(git_refdb_backend *_backend)
{
	int error;
	refdb_fs_backend *backend = (refdb_fs_backend *)_backend;

	assert(backend);

	if ((error = packed_reload(backend)) < 0 || /* load the existing packfile */
	    (error = packed_loadloose(backend)) < 0 || /* add all the loose refs */
	    (error = packed_write(backend)) < 0) /* write back to disk */
		return error;

	return 0;
}

static void refdb_fs_backend__free(git_refdb_backend *_backend)
{
	refdb_fs_backend *backend = (refdb_fs_backend *)_backend;

	assert(backend);

	git_sortedcache_free(backend->refcache);
	git__free(backend->gitpath);
	git__free(backend->commonpath);
	git__free(backend);
}

static char *setup_namespace(git_repository *repo, const char *in)
{
	git_buf path = GIT_BUF_INIT;
	char *parts, *start, *end, *out = NULL;

	if (!in)
		goto done;

	git_buf_puts(&path, in);

	/* if the repo is not namespaced, nothing else to do */
	if (repo->namespace == NULL) {
		out = git_buf_detach(&path);
		goto done;
	}

	parts = end = git__strdup(repo->namespace);
	if (parts == NULL)
		goto done;

	/*
	 * From `man gitnamespaces`:
	 *  namespaces which include a / will expand to a hierarchy
	 *  of namespaces; for example, GIT_NAMESPACE=foo/bar will store
	 *  refs under refs/namespaces/foo/refs/namespaces/bar/
	 */
	while ((start = git__strsep(&end, "/")) != NULL)
		git_buf_printf(&path, "refs/namespaces/%s/", start);

	git_buf_printf(&path, "refs/namespaces/%s/refs", end);
	git__free(parts);

	/* Make sure that the folder with the namespace exists */
	if (git_futils_mkdir_relative(git_buf_cstr(&path), in, 0777,
			GIT_MKDIR_PATH, NULL) < 0)
		goto done;

	/* Return root of the namespaced gitpath, i.e. without the trailing '/refs' */
	git_buf_rtruncate_at_char(&path, '/');
	out = git_buf_detach(&path);

done:
	git_buf_free(&path);
	return out;
}

static int reflog_alloc(git_reflog **reflog, const char *name)
{
	git_reflog *log;

	*reflog = NULL;

	log = git__calloc(1, sizeof(git_reflog));
	GITERR_CHECK_ALLOC(log);

	log->ref_name = git__strdup(name);
	GITERR_CHECK_ALLOC(log->ref_name);

	if (git_vector_init(&log->entries, 0, NULL) < 0) {
		git__free(log->ref_name);
		git__free(log);
		return -1;
	}

	*reflog = log;

	return 0;
}

static int reflog_parse(git_reflog *log, const char *buf, size_t buf_size)
{
	const char *ptr;
	git_reflog_entry *entry;

#define seek_forward(_increase) do { \
	if (_increase >= buf_size) { \
		giterr_set(GITERR_INVALID, "ran out of data while parsing reflog"); \
		goto fail; \
	} \
	buf += _increase; \
	buf_size -= _increase; \
	} while (0)

	while (buf_size > GIT_REFLOG_SIZE_MIN) {
		entry = git__calloc(1, sizeof(git_reflog_entry));
		GITERR_CHECK_ALLOC(entry);

		entry->committer = git__calloc(1, sizeof(git_signature));
		GITERR_CHECK_ALLOC(entry->committer);

		if (git_oid_fromstrn(&entry->oid_old, buf, GIT_OID_HEXSZ) < 0)
			goto fail;
		seek_forward(GIT_OID_HEXSZ + 1);

		if (git_oid_fromstrn(&entry->oid_cur, buf, GIT_OID_HEXSZ) < 0)
			goto fail;
		seek_forward(GIT_OID_HEXSZ + 1);

		ptr = buf;

		/* Seek forward to the end of the signature. */
		while (*buf && *buf != '\t' && *buf != '\n')
			seek_forward(1);

		if (git_signature__parse(entry->committer, &ptr, buf + 1, NULL, *buf) < 0)
			goto fail;

		if (*buf == '\t') {
			/* We got a message. Read everything till we reach LF. */
			seek_forward(1);
			ptr = buf;

			while (*buf && *buf != '\n')
				seek_forward(1);

			entry->msg = git__strndup(ptr, buf - ptr);
			GITERR_CHECK_ALLOC(entry->msg);
		} else
			entry->msg = NULL;

		while (*buf && *buf == '\n' && buf_size > 1)
			seek_forward(1);

		if (git_vector_insert(&log->entries, entry) < 0)
			goto fail;
	}

	return 0;

#undef seek_forward

fail:
	git_reflog_entry__free(entry);

	return -1;
}

static int create_new_reflog_file(const char *filepath)
{
	int fd, error;

	if ((error = git_futils_mkpath2file(filepath, GIT_REFLOG_DIR_MODE)) < 0)
		return error;

	if ((fd = p_open(filepath,
			O_WRONLY | O_CREAT,
			GIT_REFLOG_FILE_MODE)) < 0)
		return -1;

	return p_close(fd);
}

GIT_INLINE(int) retrieve_reflog_path(git_buf *path, git_repository *repo, const char *name)
{
	return git_buf_join3(path, '/', repo->commondir, GIT_REFLOG_DIR, name);
}

static int refdb_reflog_fs__ensure_log(git_refdb_backend *_backend, const char *name)
{
	refdb_fs_backend *backend;
	git_repository *repo;
	git_buf path = GIT_BUF_INIT;
	int error;

	assert(_backend && name);

	backend = (refdb_fs_backend *) _backend;
	repo = backend->repo;

	if ((error = retrieve_reflog_path(&path, repo, name)) < 0)
		return error;

	error = create_new_reflog_file(git_buf_cstr(&path));
	git_buf_free(&path);

	return error;
}

static int has_reflog(git_repository *repo, const char *name)
{
	int ret = 0;
	git_buf path = GIT_BUF_INIT;

	if (retrieve_reflog_path(&path, repo, name) < 0)
		goto cleanup;

	ret = git_path_isfile(git_buf_cstr(&path));

cleanup:
	git_buf_free(&path);
	return ret;
}

static int refdb_reflog_fs__has_log(git_refdb_backend *_backend, const char *name)
{
	refdb_fs_backend *backend;

	assert(_backend && name);

	backend = (refdb_fs_backend *) _backend;

	return has_reflog(backend->repo, name);
}

static int refdb_reflog_fs__read(git_reflog **out, git_refdb_backend *_backend, const char *name)
{
	int error = -1;
	git_buf log_path = GIT_BUF_INIT;
	git_buf log_file = GIT_BUF_INIT;
	git_reflog *log = NULL;
	git_repository *repo;
	refdb_fs_backend *backend;

	assert(out && _backend && name);

	backend = (refdb_fs_backend *) _backend;
	repo = backend->repo;

	if (reflog_alloc(&log, name) < 0)
		return -1;

	if (retrieve_reflog_path(&log_path, repo, name) < 0)
		goto cleanup;

	error = git_futils_readbuffer(&log_file, git_buf_cstr(&log_path));
	if (error < 0 && error != GIT_ENOTFOUND)
		goto cleanup;

	if ((error == GIT_ENOTFOUND) &&
		((error = create_new_reflog_file(git_buf_cstr(&log_path))) < 0))
		goto cleanup;
 
	if ((error = reflog_parse(log,
		git_buf_cstr(&log_file), git_buf_len(&log_file))) < 0)
		goto cleanup;

	*out = log;
	goto success;

cleanup:
	git_reflog_free(log);

success:
	git_buf_free(&log_file);
	git_buf_free(&log_path);

	return error;
}

static int serialize_reflog_entry(
	git_buf *buf,
	const git_oid *oid_old,
	const git_oid *oid_new,
	const git_signature *committer,
	const char *msg)
{
	char raw_old[GIT_OID_HEXSZ+1];
	char raw_new[GIT_OID_HEXSZ+1];

	git_oid_tostr(raw_old, GIT_OID_HEXSZ+1, oid_old);
	git_oid_tostr(raw_new, GIT_OID_HEXSZ+1, oid_new);

	git_buf_clear(buf);

	git_buf_puts(buf, raw_old);
	git_buf_putc(buf, ' ');
	git_buf_puts(buf, raw_new);

	git_signature__writebuf(buf, " ", committer);

	/* drop trailing LF */
	git_buf_rtrim(buf);

	if (msg) {
		git_buf_putc(buf, '\t');
		git_buf_puts(buf, msg);
	}

	git_buf_putc(buf, '\n');

	return git_buf_oom(buf);
}

static int lock_reflog(git_filebuf *file, refdb_fs_backend *backend, const char *refname)
{
	git_repository *repo;
	git_buf log_path = GIT_BUF_INIT;
	int error;

	repo = backend->repo;

	if (!git_path_isvalid(backend->repo, refname, GIT_PATH_REJECT_FILESYSTEM_DEFAULTS)) {
		giterr_set(GITERR_INVALID, "invalid reference name '%s'", refname);
		return GIT_EINVALIDSPEC;
	}

	if (retrieve_reflog_path(&log_path, repo, refname) < 0)
		return -1;

	if (!git_path_isfile(git_buf_cstr(&log_path))) {
		giterr_set(GITERR_INVALID,
			"log file for reference '%s' doesn't exist", refname);
		error = -1;
		goto cleanup;
	}

	error = git_filebuf_open(file, git_buf_cstr(&log_path), 0, GIT_REFLOG_FILE_MODE);

cleanup:
	git_buf_free(&log_path);

	return error;
}

static int refdb_reflog_fs__write(git_refdb_backend *_backend, git_reflog *reflog)
{
	int error = -1;
	unsigned int i;
	git_reflog_entry *entry;
	refdb_fs_backend *backend;
	git_buf log = GIT_BUF_INIT;
	git_filebuf fbuf = GIT_FILEBUF_INIT;

	assert(_backend && reflog);

	backend = (refdb_fs_backend *) _backend;

	if ((error = lock_reflog(&fbuf, backend, reflog->ref_name)) < 0)
		return -1;

	git_vector_foreach(&reflog->entries, i, entry) {
		if (serialize_reflog_entry(&log, &(entry->oid_old), &(entry->oid_cur), entry->committer, entry->msg) < 0)
			goto cleanup;

		if ((error = git_filebuf_write(&fbuf, log.ptr, log.size)) < 0)
			goto cleanup;
	}

	error = git_filebuf_commit(&fbuf);
	goto success;

cleanup:
	git_filebuf_cleanup(&fbuf);

success:
	git_buf_free(&log);

	return error;
}

/* Append to the reflog, must be called under reference lock */
static int reflog_append(refdb_fs_backend *backend, const git_reference *ref, const git_oid *old, const git_oid *new, const git_signature *who, const char *message)
{
	int error, is_symbolic, open_flags;
	git_oid old_id = {{0}}, new_id = {{0}};
	git_buf buf = GIT_BUF_INIT, path = GIT_BUF_INIT;
	git_repository *repo = backend->repo;

	is_symbolic = ref->type == GIT_REF_SYMBOLIC;

	/* "normal" symbolic updates do not write */
	if (is_symbolic &&
	    strcmp(ref->name, GIT_HEAD_FILE) &&
	    !(old && new))
		return 0;

	/* From here on is_symoblic also means that it's HEAD */

	if (old) {
		git_oid_cpy(&old_id, old);
	} else {
		error = git_reference_name_to_id(&old_id, repo, ref->name);
		if (error < 0 && error != GIT_ENOTFOUND)
			return error;
	}

	if (new) {
		git_oid_cpy(&new_id, new);
	} else {
		if (!is_symbolic) {
			git_oid_cpy(&new_id, git_reference_target(ref));
		} else {
			error = git_reference_name_to_id(&new_id, repo, git_reference_symbolic_target(ref));
			if (error < 0 && error != GIT_ENOTFOUND)
				return error;
			/* detaching HEAD does not create an entry */
			if (error == GIT_ENOTFOUND)
				return 0;

			giterr_clear();
		}
	}

	if ((error = serialize_reflog_entry(&buf, &old_id, &new_id, who, message)) < 0)
		goto cleanup;

	if ((error = retrieve_reflog_path(&path, repo, ref->name)) < 0)
		goto cleanup;

	if (((error = git_futils_mkpath2file(git_buf_cstr(&path), 0777)) < 0) &&
	    (error != GIT_EEXISTS)) {
		goto cleanup;
	}

	/* If the new branch matches part of the namespace of a previously deleted branch,
	 * there maybe an obsolete/unused directory (or directory hierarchy) in the way.
	 */
	if (git_path_isdir(git_buf_cstr(&path))) {
		if ((error = git_futils_rmdir_r(git_buf_cstr(&path), NULL, GIT_RMDIR_SKIP_NONEMPTY)) < 0) {
			if (error == GIT_ENOTFOUND)
				error = 0;
		} else if (git_path_isdir(git_buf_cstr(&path))) {
			giterr_set(GITERR_REFERENCE, "cannot create reflog at '%s', there are reflogs beneath that folder",
				ref->name);
			error = GIT_EDIRECTORY;
		}

		if (error != 0)
			goto cleanup;
	}

	open_flags = O_WRONLY | O_CREAT | O_APPEND;

	if (backend->fsync)
		open_flags |= O_FSYNC;

	error = git_futils_writebuffer(&buf, git_buf_cstr(&path), open_flags, GIT_REFLOG_FILE_MODE);

cleanup:
	git_buf_free(&buf);
	git_buf_free(&path);

	return error;
}

static int refdb_reflog_fs__rename(git_refdb_backend *_backend, const char *old_name, const char *new_name)
{
	int error = 0, fd;
	git_buf old_path = GIT_BUF_INIT;
	git_buf new_path = GIT_BUF_INIT;
	git_buf temp_path = GIT_BUF_INIT;
	git_buf normalized = GIT_BUF_INIT;
	git_repository *repo;
	refdb_fs_backend *backend;

	assert(_backend && old_name && new_name);

	backend = (refdb_fs_backend *) _backend;
	repo = backend->repo;

	if ((error = git_reference__normalize_name(
		&normalized, new_name, GIT_REF_FORMAT_ALLOW_ONELEVEL)) < 0)
			return error;

	if (git_buf_joinpath(&temp_path, repo->gitdir, GIT_REFLOG_DIR) < 0)
		return -1;

	if (git_buf_joinpath(&old_path, git_buf_cstr(&temp_path), old_name) < 0)
		return -1;

	if (git_buf_joinpath(&new_path, git_buf_cstr(&temp_path), git_buf_cstr(&normalized)) < 0)
		return -1;

	if (!git_path_exists(git_buf_cstr(&old_path))) {
		error = GIT_ENOTFOUND;
		goto cleanup;
	}

	/*
	 * Move the reflog to a temporary place. This two-phase renaming is required
	 * in order to cope with funny renaming use cases when one tries to move a reference
	 * to a partially colliding namespace:
	 *  - a/b -> a/b/c
	 *  - a/b/c/d -> a/b/c
	 */
	if (git_buf_joinpath(&temp_path, git_buf_cstr(&temp_path), "temp_reflog") < 0)
		return -1;

	if ((fd = git_futils_mktmp(&temp_path, git_buf_cstr(&temp_path), GIT_REFLOG_FILE_MODE)) < 0) {
		error = -1;
		goto cleanup;
	}

	p_close(fd);

	if (p_rename(git_buf_cstr(&old_path), git_buf_cstr(&temp_path)) < 0) {
		giterr_set(GITERR_OS, "failed to rename reflog for %s", new_name);
		error = -1;
		goto cleanup;
	}

	if (git_path_isdir(git_buf_cstr(&new_path)) && 
		(git_futils_rmdir_r(git_buf_cstr(&new_path), NULL, GIT_RMDIR_SKIP_NONEMPTY) < 0)) {
		error = -1;
		goto cleanup;
	}

	if (git_futils_mkpath2file(git_buf_cstr(&new_path), GIT_REFLOG_DIR_MODE) < 0) {
		error = -1;
		goto cleanup;
	}

	if (p_rename(git_buf_cstr(&temp_path), git_buf_cstr(&new_path)) < 0) {
		giterr_set(GITERR_OS, "failed to rename reflog for %s", new_name);
		error = -1;
	}

cleanup:
	git_buf_free(&temp_path);
	git_buf_free(&old_path);
	git_buf_free(&new_path);
	git_buf_free(&normalized);

	return error;
}

static int refdb_reflog_fs__delete(git_refdb_backend *_backend, const char *name)
{
	int error;
	git_buf path = GIT_BUF_INIT;

	git_repository *repo;
	refdb_fs_backend *backend;

	assert(_backend && name);

	backend = (refdb_fs_backend *) _backend;
	repo = backend->repo;

	error = retrieve_reflog_path(&path, repo, name);

	if (!error && git_path_exists(path.ptr))
		error = p_unlink(path.ptr);

	git_buf_free(&path);

	return error;

}

int git_refdb_backend_fs(
	git_refdb_backend **backend_out,
	git_repository *repository)
{
	int t = 0;
	git_buf gitpath = GIT_BUF_INIT;
	refdb_fs_backend *backend;

	backend = git__calloc(1, sizeof(refdb_fs_backend));
	GITERR_CHECK_ALLOC(backend);

	backend->repo = repository;

	if (repository->gitdir) {
		backend->gitpath = setup_namespace(repository, repository->gitdir);

		if (backend->gitpath == NULL)
			goto fail;
	}

	if (repository->commondir) {
		backend->commonpath = setup_namespace(repository, repository->commondir);

		if (backend->commonpath == NULL)
			goto fail;
	}

	if (git_buf_joinpath(&gitpath, backend->commonpath, GIT_PACKEDREFS_FILE) < 0 ||
		git_sortedcache_new(
			&backend->refcache, offsetof(struct packref, name),
			NULL, NULL, packref_cmp, git_buf_cstr(&gitpath)) < 0)
		goto fail;

	git_buf_free(&gitpath);

	if (!git_repository__cvar(&t, backend->repo, GIT_CVAR_IGNORECASE) && t) {
		backend->iterator_flags |= GIT_ITERATOR_IGNORE_CASE;
		backend->direach_flags  |= GIT_PATH_DIR_IGNORE_CASE;
	}
	if (!git_repository__cvar(&t, backend->repo, GIT_CVAR_PRECOMPOSE) && t) {
		backend->iterator_flags |= GIT_ITERATOR_PRECOMPOSE_UNICODE;
		backend->direach_flags  |= GIT_PATH_DIR_PRECOMPOSE_UNICODE;
	}
	if ((!git_repository__cvar(&t, backend->repo, GIT_CVAR_FSYNCOBJECTFILES) && t) ||
		git_repository__fsync_gitdir)
		backend->fsync = 1;

	backend->parent.exists = &refdb_fs_backend__exists;
	backend->parent.lookup = &refdb_fs_backend__lookup;
	backend->parent.iterator = &refdb_fs_backend__iterator;
	backend->parent.write = &refdb_fs_backend__write;
	backend->parent.del = &refdb_fs_backend__delete;
	backend->parent.rename = &refdb_fs_backend__rename;
	backend->parent.compress = &refdb_fs_backend__compress;
	backend->parent.lock = &refdb_fs_backend__lock;
	backend->parent.unlock = &refdb_fs_backend__unlock;
	backend->parent.has_log = &refdb_reflog_fs__has_log;
	backend->parent.ensure_log = &refdb_reflog_fs__ensure_log;
	backend->parent.free = &refdb_fs_backend__free;
	backend->parent.reflog_read = &refdb_reflog_fs__read;
	backend->parent.reflog_write = &refdb_reflog_fs__write;
	backend->parent.reflog_rename = &refdb_reflog_fs__rename;
	backend->parent.reflog_delete = &refdb_reflog_fs__delete;

	*backend_out = (git_refdb_backend *)backend;
	return 0;

fail:
	git_buf_free(&gitpath);
	git__free(backend->gitpath);
	git__free(backend->commonpath);
	git__free(backend);
	return -1;
}
