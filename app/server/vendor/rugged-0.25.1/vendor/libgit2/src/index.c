/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <stddef.h>

#include "common.h"
#include "repository.h"
#include "index.h"
#include "tree.h"
#include "tree-cache.h"
#include "hash.h"
#include "iterator.h"
#include "pathspec.h"
#include "ignore.h"
#include "blob.h"
#include "idxmap.h"
#include "diff.h"
#include "varint.h"

#include "git2/odb.h"
#include "git2/oid.h"
#include "git2/blob.h"
#include "git2/config.h"
#include "git2/sys/index.h"

GIT__USE_IDXMAP
GIT__USE_IDXMAP_ICASE

#define INSERT_IN_MAP_EX(idx, map, e, err) do {				\
		if ((idx)->ignore_case)					\
			git_idxmap_icase_insert((khash_t(idxicase) *) (map), (e), (e), (err)); \
		else							\
			git_idxmap_insert((map), (e), (e), (err));	\
	} while (0)

#define INSERT_IN_MAP(idx, e, err) INSERT_IN_MAP_EX(idx, (idx)->entries_map, e, err)

#define LOOKUP_IN_MAP(p, idx, k) do {					\
		if ((idx)->ignore_case)					\
			(p) = git_idxmap_icase_lookup_index((khash_t(idxicase) *) index->entries_map, (k)); \
		else							\
			(p) = git_idxmap_lookup_index(index->entries_map, (k)); \
	} while (0)

#define DELETE_IN_MAP(idx, e) do {					\
		if ((idx)->ignore_case)					\
			git_idxmap_icase_delete((khash_t(idxicase) *) (idx)->entries_map, (e)); \
		else							\
			git_idxmap_delete((idx)->entries_map, (e));	\
	} while (0)

static int index_apply_to_wd_diff(git_index *index, int action, const git_strarray *paths,
				  unsigned int flags,
				  git_index_matched_path_cb cb, void *payload);

#define entry_size(type,len) ((offsetof(type, path) + (len) + 8) & ~7)
#define short_entry_size(len) entry_size(struct entry_short, len)
#define long_entry_size(len) entry_size(struct entry_long, len)

#define minimal_entry_size (offsetof(struct entry_short, path))

static const size_t INDEX_FOOTER_SIZE = GIT_OID_RAWSZ;
static const size_t INDEX_HEADER_SIZE = 12;

static const unsigned int INDEX_VERSION_NUMBER_DEFAULT = 2;
static const unsigned int INDEX_VERSION_NUMBER_LB = 2;
static const unsigned int INDEX_VERSION_NUMBER_EXT = 3;
static const unsigned int INDEX_VERSION_NUMBER_COMP = 4;
static const unsigned int INDEX_VERSION_NUMBER_UB = 4;

static const unsigned int INDEX_HEADER_SIG = 0x44495243;
static const char INDEX_EXT_TREECACHE_SIG[] = {'T', 'R', 'E', 'E'};
static const char INDEX_EXT_UNMERGED_SIG[] = {'R', 'E', 'U', 'C'};
static const char INDEX_EXT_CONFLICT_NAME_SIG[] = {'N', 'A', 'M', 'E'};

#define INDEX_OWNER(idx) ((git_repository *)(GIT_REFCOUNT_OWNER(idx)))

struct index_header {
	uint32_t signature;
	uint32_t version;
	uint32_t entry_count;
};

struct index_extension {
	char signature[4];
	uint32_t extension_size;
};

struct entry_time {
	uint32_t seconds;
	uint32_t nanoseconds;
};

struct entry_short {
	struct entry_time ctime;
	struct entry_time mtime;
	uint32_t dev;
	uint32_t ino;
	uint32_t mode;
	uint32_t uid;
	uint32_t gid;
	uint32_t file_size;
	git_oid oid;
	uint16_t flags;
	char path[1]; /* arbitrary length */
};

struct entry_long {
	struct entry_time ctime;
	struct entry_time mtime;
	uint32_t dev;
	uint32_t ino;
	uint32_t mode;
	uint32_t uid;
	uint32_t gid;
	uint32_t file_size;
	git_oid oid;
	uint16_t flags;
	uint16_t flags_extended;
	char path[1]; /* arbitrary length */
};

struct entry_srch_key {
	const char *path;
	size_t pathlen;
	int stage;
};

struct entry_internal {
	git_index_entry entry;
	size_t pathlen;
	char path[GIT_FLEX_ARRAY];
};

struct reuc_entry_internal {
	git_index_reuc_entry entry;
	size_t pathlen;
	char path[GIT_FLEX_ARRAY];
};

/* local declarations */
static size_t read_extension(git_index *index, const char *buffer, size_t buffer_size);
static int read_header(struct index_header *dest, const void *buffer);

static int parse_index(git_index *index, const char *buffer, size_t buffer_size);
static bool is_index_extended(git_index *index);
static int write_index(git_oid *checksum, git_index *index, git_filebuf *file);

static void index_entry_free(git_index_entry *entry);
static void index_entry_reuc_free(git_index_reuc_entry *reuc);

int git_index_entry_srch(const void *key, const void *array_member)
{
	const struct entry_srch_key *srch_key = key;
	const struct entry_internal *entry = array_member;
	int cmp;
	size_t len1, len2, len;

	len1 = srch_key->pathlen;
	len2 = entry->pathlen;
	len = len1 < len2 ? len1 : len2;

	cmp = memcmp(srch_key->path, entry->path, len);
	if (cmp)
		return cmp;
	if (len1 < len2)
		return -1;
	if (len1 > len2)
		return 1;

	if (srch_key->stage != GIT_INDEX_STAGE_ANY)
		return srch_key->stage - GIT_IDXENTRY_STAGE(&entry->entry);

	return 0;
}

int git_index_entry_isrch(const void *key, const void *array_member)
{
	const struct entry_srch_key *srch_key = key;
	const struct entry_internal *entry = array_member;
	int cmp;
	size_t len1, len2, len;

	len1 = srch_key->pathlen;
	len2 = entry->pathlen;
	len = len1 < len2 ? len1 : len2;

	cmp = strncasecmp(srch_key->path, entry->path, len);

	if (cmp)
		return cmp;
	if (len1 < len2)
		return -1;
	if (len1 > len2)
		return 1;

	if (srch_key->stage != GIT_INDEX_STAGE_ANY)
		return srch_key->stage - GIT_IDXENTRY_STAGE(&entry->entry);

	return 0;
}

static int index_entry_srch_path(const void *path, const void *array_member)
{
	const git_index_entry *entry = array_member;

	return strcmp((const char *)path, entry->path);
}

static int index_entry_isrch_path(const void *path, const void *array_member)
{
	const git_index_entry *entry = array_member;

	return strcasecmp((const char *)path, entry->path);
}

int git_index_entry_cmp(const void *a, const void *b)
{
	int diff;
	const git_index_entry *entry_a = a;
	const git_index_entry *entry_b = b;

	diff = strcmp(entry_a->path, entry_b->path);

	if (diff == 0)
		diff = (GIT_IDXENTRY_STAGE(entry_a) - GIT_IDXENTRY_STAGE(entry_b));

	return diff;
}

int git_index_entry_icmp(const void *a, const void *b)
{
	int diff;
	const git_index_entry *entry_a = a;
	const git_index_entry *entry_b = b;

	diff = strcasecmp(entry_a->path, entry_b->path);

	if (diff == 0)
		diff = (GIT_IDXENTRY_STAGE(entry_a) - GIT_IDXENTRY_STAGE(entry_b));

	return diff;
}

static int conflict_name_cmp(const void *a, const void *b)
{
	const git_index_name_entry *name_a = a;
	const git_index_name_entry *name_b = b;

	if (name_a->ancestor && !name_b->ancestor)
		return 1;

	if (!name_a->ancestor && name_b->ancestor)
		return -1;

	if (name_a->ancestor)
		return strcmp(name_a->ancestor, name_b->ancestor);

	if (!name_a->ours || !name_b->ours)
		return 0;

	return strcmp(name_a->ours, name_b->ours);
}

/**
 * TODO: enable this when resolving case insensitive conflicts
 */
#if 0
static int conflict_name_icmp(const void *a, const void *b)
{
	const git_index_name_entry *name_a = a;
	const git_index_name_entry *name_b = b;

	if (name_a->ancestor && !name_b->ancestor)
		return 1;

	if (!name_a->ancestor && name_b->ancestor)
		return -1;

	if (name_a->ancestor)
		return strcasecmp(name_a->ancestor, name_b->ancestor);

	if (!name_a->ours || !name_b->ours)
		return 0;

	return strcasecmp(name_a->ours, name_b->ours);
}
#endif

static int reuc_srch(const void *key, const void *array_member)
{
	const git_index_reuc_entry *reuc = array_member;

	return strcmp(key, reuc->path);
}

static int reuc_isrch(const void *key, const void *array_member)
{
	const git_index_reuc_entry *reuc = array_member;

	return strcasecmp(key, reuc->path);
}

static int reuc_cmp(const void *a, const void *b)
{
	const git_index_reuc_entry *info_a = a;
	const git_index_reuc_entry *info_b = b;

	return strcmp(info_a->path, info_b->path);
}

static int reuc_icmp(const void *a, const void *b)
{
	const git_index_reuc_entry *info_a = a;
	const git_index_reuc_entry *info_b = b;

	return strcasecmp(info_a->path, info_b->path);
}

static void index_entry_reuc_free(git_index_reuc_entry *reuc)
{
	git__free(reuc);
}

static void index_entry_free(git_index_entry *entry)
{
	if (!entry)
		return;

	memset(&entry->id, 0, sizeof(entry->id));
	git__free(entry);
}

unsigned int git_index__create_mode(unsigned int mode)
{
	if (S_ISLNK(mode))
		return S_IFLNK;

	if (S_ISDIR(mode) || (mode & S_IFMT) == (S_IFLNK | S_IFDIR))
		return (S_IFLNK | S_IFDIR);

	return S_IFREG | GIT_PERMS_CANONICAL(mode);
}

static unsigned int index_merge_mode(
	git_index *index, git_index_entry *existing, unsigned int mode)
{
	if (index->no_symlinks && S_ISREG(mode) &&
		existing && S_ISLNK(existing->mode))
		return existing->mode;

	if (index->distrust_filemode && S_ISREG(mode))
		return (existing && S_ISREG(existing->mode)) ?
			existing->mode : git_index__create_mode(0666);

	return git_index__create_mode(mode);
}

GIT_INLINE(int) index_find_in_entries(
	size_t *out, git_vector *entries, git_vector_cmp entry_srch,
	const char *path, size_t path_len, int stage)
{
	struct entry_srch_key srch_key;
	srch_key.path = path;
	srch_key.pathlen = !path_len ? strlen(path) : path_len;
	srch_key.stage = stage;
	return git_vector_bsearch2(out, entries, entry_srch, &srch_key);
}

GIT_INLINE(int) index_find(
	size_t *out, git_index *index,
	const char *path, size_t path_len, int stage)
{
	git_vector_sort(&index->entries);

	return index_find_in_entries(
		out, &index->entries, index->entries_search, path, path_len, stage);
}

void git_index__set_ignore_case(git_index *index, bool ignore_case)
{
	index->ignore_case = ignore_case;

	if (ignore_case) {
		index->entries_cmp_path    = git__strcasecmp_cb;
		index->entries_search      = git_index_entry_isrch;
		index->entries_search_path = index_entry_isrch_path;
		index->reuc_search         = reuc_isrch;
	} else {
		index->entries_cmp_path    = git__strcmp_cb;
		index->entries_search      = git_index_entry_srch;
		index->entries_search_path = index_entry_srch_path;
		index->reuc_search         = reuc_srch;
	}

	git_vector_set_cmp(&index->entries,
		ignore_case ? git_index_entry_icmp : git_index_entry_cmp);
	git_vector_sort(&index->entries);

	git_vector_set_cmp(&index->reuc, ignore_case ? reuc_icmp : reuc_cmp);
	git_vector_sort(&index->reuc);
}

int git_index_open(git_index **index_out, const char *index_path)
{
	git_index *index;
	int error = -1;

	assert(index_out);

	index = git__calloc(1, sizeof(git_index));
	GITERR_CHECK_ALLOC(index);

	git_pool_init(&index->tree_pool, 1);

	if (index_path != NULL) {
		index->index_file_path = git__strdup(index_path);
		if (!index->index_file_path)
			goto fail;

		/* Check if index file is stored on disk already */
		if (git_path_exists(index->index_file_path) == true)
			index->on_disk = 1;
	}

	if (git_vector_init(&index->entries, 32, git_index_entry_cmp) < 0 ||
		git_idxmap_alloc(&index->entries_map) < 0 ||
		git_vector_init(&index->names, 8, conflict_name_cmp) < 0 ||
		git_vector_init(&index->reuc, 8, reuc_cmp) < 0 ||
		git_vector_init(&index->deleted, 8, git_index_entry_cmp) < 0)
		goto fail;

	index->entries_cmp_path = git__strcmp_cb;
	index->entries_search = git_index_entry_srch;
	index->entries_search_path = index_entry_srch_path;
	index->reuc_search = reuc_srch;
	index->version = INDEX_VERSION_NUMBER_DEFAULT;

	if (index_path != NULL && (error = git_index_read(index, true)) < 0)
		goto fail;

	*index_out = index;
	GIT_REFCOUNT_INC(index);

	return 0;

fail:
	git_pool_clear(&index->tree_pool);
	git_index_free(index);
	return error;
}

int git_index_new(git_index **out)
{
	return git_index_open(out, NULL);
}

static void index_free(git_index *index)
{
	/* index iterators increment the refcount of the index, so if we
	 * get here then there should be no outstanding iterators.
	 */
	assert(!git_atomic_get(&index->readers));

	git_index_clear(index);
	git_idxmap_free(index->entries_map);
	git_vector_free(&index->entries);
	git_vector_free(&index->names);
	git_vector_free(&index->reuc);
	git_vector_free(&index->deleted);

	git__free(index->index_file_path);

	git__memzero(index, sizeof(*index));
	git__free(index);
}

void git_index_free(git_index *index)
{
	if (index == NULL)
		return;

	GIT_REFCOUNT_DEC(index, index_free);
}

/* call with locked index */
static void index_free_deleted(git_index *index)
{
	int readers = (int)git_atomic_get(&index->readers);
	size_t i;

	if (readers > 0 || !index->deleted.length)
		return;

	for (i = 0; i < index->deleted.length; ++i) {
		git_index_entry *ie = git__swap(index->deleted.contents[i], NULL);
		index_entry_free(ie);
	}

	git_vector_clear(&index->deleted);
}

/* call with locked index */
static int index_remove_entry(git_index *index, size_t pos)
{
	int error = 0;
	git_index_entry *entry = git_vector_get(&index->entries, pos);

	if (entry != NULL) {
		git_tree_cache_invalidate_path(index->tree, entry->path);
		DELETE_IN_MAP(index, entry);
	}

	error = git_vector_remove(&index->entries, pos);

	if (!error) {
		if (git_atomic_get(&index->readers) > 0) {
			error = git_vector_insert(&index->deleted, entry);
		} else {
			index_entry_free(entry);
		}
	}

	return error;
}

int git_index_clear(git_index *index)
{
	int error = 0;

	assert(index);

	index->tree = NULL;
	git_pool_clear(&index->tree_pool);

	git_idxmap_clear(index->entries_map);
	while (!error && index->entries.length > 0)
		error = index_remove_entry(index, index->entries.length - 1);
	index_free_deleted(index);

	git_index_reuc_clear(index);
	git_index_name_clear(index);

	git_futils_filestamp_set(&index->stamp, NULL);

	return error;
}

static int create_index_error(int error, const char *msg)
{
	giterr_set_str(GITERR_INDEX, msg);
	return error;
}

int git_index_set_caps(git_index *index, int caps)
{
	unsigned int old_ignore_case;

	assert(index);

	old_ignore_case = index->ignore_case;

	if (caps == GIT_INDEXCAP_FROM_OWNER) {
		git_repository *repo = INDEX_OWNER(index);
		int val;

		if (!repo)
			return create_index_error(
				-1, "Cannot access repository to set index caps");

		if (!git_repository__cvar(&val, repo, GIT_CVAR_IGNORECASE))
			index->ignore_case = (val != 0);
		if (!git_repository__cvar(&val, repo, GIT_CVAR_FILEMODE))
			index->distrust_filemode = (val == 0);
		if (!git_repository__cvar(&val, repo, GIT_CVAR_SYMLINKS))
			index->no_symlinks = (val == 0);
	}
	else {
		index->ignore_case = ((caps & GIT_INDEXCAP_IGNORE_CASE) != 0);
		index->distrust_filemode = ((caps & GIT_INDEXCAP_NO_FILEMODE) != 0);
		index->no_symlinks = ((caps & GIT_INDEXCAP_NO_SYMLINKS) != 0);
	}

	if (old_ignore_case != index->ignore_case) {
		git_index__set_ignore_case(index, (bool)index->ignore_case);
	}

	return 0;
}

int git_index_caps(const git_index *index)
{
	return ((index->ignore_case ? GIT_INDEXCAP_IGNORE_CASE : 0) |
			(index->distrust_filemode ? GIT_INDEXCAP_NO_FILEMODE : 0) |
			(index->no_symlinks ? GIT_INDEXCAP_NO_SYMLINKS : 0));
}

const git_oid *git_index_checksum(git_index *index)
{
	return &index->checksum;
}

/**
 * Returns 1 for changed, 0 for not changed and <0 for errors
 */
static int compare_checksum(git_index *index)
{
	int fd;
	ssize_t bytes_read;
	git_oid checksum = {{ 0 }};

	if ((fd = p_open(index->index_file_path, O_RDONLY)) < 0)
		return fd;

	if (p_lseek(fd, -20, SEEK_END) < 0) {
		p_close(fd);
		giterr_set(GITERR_OS, "failed to seek to end of file");
		return -1;
	}

	bytes_read = p_read(fd, &checksum, GIT_OID_RAWSZ);
	p_close(fd);

	if (bytes_read < 0)
		return -1;

	return !!git_oid_cmp(&checksum, &index->checksum);
}

int git_index_read(git_index *index, int force)
{
	int error = 0, updated;
	git_buf buffer = GIT_BUF_INIT;
	git_futils_filestamp stamp = index->stamp;

	if (!index->index_file_path)
		return create_index_error(-1,
			"Failed to read index: The index is in-memory only");

	index->on_disk = git_path_exists(index->index_file_path);

	if (!index->on_disk) {
		if (force)
			return git_index_clear(index);
		return 0;
	}

	if ((updated = git_futils_filestamp_check(&stamp, index->index_file_path) < 0) ||
	    ((updated = compare_checksum(index)) < 0)) {
		giterr_set(
			GITERR_INDEX,
			"Failed to read index: '%s' no longer exists",
			index->index_file_path);
		return updated;
	}
	if (!updated && !force)
		return 0;

	error = git_futils_readbuffer(&buffer, index->index_file_path);
	if (error < 0)
		return error;

	index->tree = NULL;
	git_pool_clear(&index->tree_pool);

	error = git_index_clear(index);

	if (!error)
		error = parse_index(index, buffer.ptr, buffer.size);

	if (!error)
		git_futils_filestamp_set(&index->stamp, &stamp);

	git_buf_free(&buffer);
	return error;
}

int git_index__changed_relative_to(
	git_index *index, const git_oid *checksum)
{
	/* attempt to update index (ignoring errors) */
	if (git_index_read(index, false) < 0)
		giterr_clear();

	return !!git_oid_cmp(&index->checksum, checksum);
}

static bool is_racy_entry(git_index *index, const git_index_entry *entry)
{
	/* Git special-cases submodules in the check */
	if (S_ISGITLINK(entry->mode))
		return false;

	return git_index_entry_newer_than_index(entry, index);
}

/*
 * Force the next diff to take a look at those entries which have the
 * same timestamp as the current index.
 */
static int truncate_racily_clean(git_index *index)
{
	size_t i;
	int error;
	git_index_entry *entry;
	git_diff_options diff_opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	git_vector paths = GIT_VECTOR_INIT;
	git_diff_delta *delta;

	/* Nothing to do if there's no repo to talk about */
	if (!INDEX_OWNER(index))
		return 0;

	/* If there's no workdir, we can't know where to even check */
	if (!git_repository_workdir(INDEX_OWNER(index)))
		return 0;

	diff_opts.flags |= GIT_DIFF_INCLUDE_TYPECHANGE | GIT_DIFF_IGNORE_SUBMODULES | GIT_DIFF_DISABLE_PATHSPEC_MATCH;
	git_vector_foreach(&index->entries, i, entry) {
		if ((entry->flags_extended & GIT_IDXENTRY_UPTODATE) == 0 &&
			is_racy_entry(index, entry))
			git_vector_insert(&paths, (char *)entry->path);
	}

	if (paths.length == 0)
		goto done;

	diff_opts.pathspec.count = paths.length;
	diff_opts.pathspec.strings = (char **)paths.contents;

	if ((error = git_diff_index_to_workdir(&diff, INDEX_OWNER(index), index, &diff_opts)) < 0)
		return error;

	git_vector_foreach(&diff->deltas, i, delta) {
		entry = (git_index_entry *)git_index_get_bypath(index, delta->old_file.path, 0);

		/* Ensure that we have a stage 0 for this file (ie, it's not a
		 * conflict), otherwise smudging it is quite pointless.
		 */
		if (entry)
			entry->file_size = 0;
	}

done:
	git_diff_free(diff);
	git_vector_free(&paths);
	return 0;
}

unsigned git_index_version(git_index *index)
{
	assert(index);

	return index->version;
}

int git_index_set_version(git_index *index, unsigned int version)
{
	assert(index);

	if (version < INDEX_VERSION_NUMBER_LB ||
	    version > INDEX_VERSION_NUMBER_UB) {
		giterr_set(GITERR_INDEX, "Invalid version number");
		return -1;
	}

	index->version = version;

	return 0;
}

int git_index_write(git_index *index)
{
	git_indexwriter writer = GIT_INDEXWRITER_INIT;
	int error;

	truncate_racily_clean(index);

	if ((error = git_indexwriter_init(&writer, index)) == 0)
		error = git_indexwriter_commit(&writer);

	git_indexwriter_cleanup(&writer);

	return error;
}

const char * git_index_path(const git_index *index)
{
	assert(index);
	return index->index_file_path;
}

int git_index_write_tree(git_oid *oid, git_index *index)
{
	git_repository *repo;

	assert(oid && index);

	repo = INDEX_OWNER(index);

	if (repo == NULL)
		return create_index_error(-1, "Failed to write tree. "
		  "The index file is not backed up by an existing repository");

	return git_tree__write_index(oid, index, repo);
}

int git_index_write_tree_to(
	git_oid *oid, git_index *index, git_repository *repo)
{
	assert(oid && index && repo);
	return git_tree__write_index(oid, index, repo);
}

size_t git_index_entrycount(const git_index *index)
{
	assert(index);
	return index->entries.length;
}

const git_index_entry *git_index_get_byindex(
	git_index *index, size_t n)
{
	assert(index);
	git_vector_sort(&index->entries);
	return git_vector_get(&index->entries, n);
}

const git_index_entry *git_index_get_bypath(
	git_index *index, const char *path, int stage)
{
	khiter_t pos;
	git_index_entry key = {{ 0 }};

	assert(index);

	key.path = path;
	GIT_IDXENTRY_STAGE_SET(&key, stage);

	LOOKUP_IN_MAP(pos, index, &key);

	if (git_idxmap_valid_index(index->entries_map, pos))
		return git_idxmap_value_at(index->entries_map, pos);

	giterr_set(GITERR_INDEX, "Index does not contain %s", path);
	return NULL;
}

void git_index_entry__init_from_stat(
	git_index_entry *entry, struct stat *st, bool trust_mode)
{
	entry->ctime.seconds = (int32_t)st->st_ctime;
	entry->mtime.seconds = (int32_t)st->st_mtime;
#if defined(GIT_USE_NSEC)
	entry->mtime.nanoseconds = st->st_mtime_nsec;
	entry->ctime.nanoseconds = st->st_ctime_nsec;
#endif
	entry->dev  = st->st_rdev;
	entry->ino  = st->st_ino;
	entry->mode = (!trust_mode && S_ISREG(st->st_mode)) ?
		git_index__create_mode(0666) : git_index__create_mode(st->st_mode);
	entry->uid  = st->st_uid;
	entry->gid  = st->st_gid;
	entry->file_size = (uint32_t)st->st_size;
}

static void index_entry_adjust_namemask(
		git_index_entry *entry,
		size_t path_length)
{
	entry->flags &= ~GIT_IDXENTRY_NAMEMASK;

	if (path_length < GIT_IDXENTRY_NAMEMASK)
		entry->flags |= path_length & GIT_IDXENTRY_NAMEMASK;
	else
		entry->flags |= GIT_IDXENTRY_NAMEMASK;
}

/* When `from_workdir` is true, we will validate the paths to avoid placing
 * paths that are invalid for the working directory on the current filesystem
 * (eg, on Windows, we will disallow `GIT~1`, `AUX`, `COM1`, etc).  This
 * function will *always* prevent `.git` and directory traversal `../` from
 * being added to the index.
 */
static int index_entry_create(
	git_index_entry **out,
	git_repository *repo,
	const char *path,
	bool from_workdir)
{
	size_t pathlen = strlen(path), alloclen;
	struct entry_internal *entry;
	unsigned int path_valid_flags = GIT_PATH_REJECT_INDEX_DEFAULTS;

	/* always reject placing `.git` in the index and directory traversal.
	 * when requested, disallow platform-specific filenames and upgrade to
	 * the platform-specific `.git` tests (eg, `git~1`, etc).
	 */
	if (from_workdir)
		path_valid_flags |= GIT_PATH_REJECT_WORKDIR_DEFAULTS;

	if (!git_path_isvalid(repo, path, path_valid_flags)) {
		giterr_set(GITERR_INDEX, "invalid path: '%s'", path);
		return -1;
	}

	GITERR_CHECK_ALLOC_ADD(&alloclen, sizeof(struct entry_internal), pathlen);
	GITERR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);
	entry = git__calloc(1, alloclen);
	GITERR_CHECK_ALLOC(entry);

	entry->pathlen = pathlen;
	memcpy(entry->path, path, pathlen);
	entry->entry.path = entry->path;

	*out = (git_index_entry *)entry;
	return 0;
}

static int index_entry_init(
	git_index_entry **entry_out,
	git_index *index,
	const char *rel_path)
{
	int error = 0;
	git_index_entry *entry = NULL;
	struct stat st;
	git_oid oid;

	if (INDEX_OWNER(index) == NULL)
		return create_index_error(-1,
			"Could not initialize index entry. "
			"Index is not backed up by an existing repository.");

	if (index_entry_create(&entry, INDEX_OWNER(index), rel_path, true) < 0)
		return -1;

	/* write the blob to disk and get the oid and stat info */
	error = git_blob__create_from_paths(
		&oid, &st, INDEX_OWNER(index), NULL, rel_path, 0, true);

	if (error < 0) {
		index_entry_free(entry);
		return error;
	}

	entry->id = oid;
	git_index_entry__init_from_stat(entry, &st, !index->distrust_filemode);

	*entry_out = (git_index_entry *)entry;
	return 0;
}

static git_index_reuc_entry *reuc_entry_alloc(const char *path)
{
	size_t pathlen = strlen(path),
		structlen = sizeof(struct reuc_entry_internal),
		alloclen;
	struct reuc_entry_internal *entry;

	if (GIT_ADD_SIZET_OVERFLOW(&alloclen, structlen, pathlen) ||
		GIT_ADD_SIZET_OVERFLOW(&alloclen, alloclen, 1))
		return NULL;

	entry = git__calloc(1, alloclen);
	if (!entry)
		return NULL;

	entry->pathlen = pathlen;
	memcpy(entry->path, path, pathlen);
	entry->entry.path = entry->path;

	return (git_index_reuc_entry *)entry;
}

static int index_entry_reuc_init(git_index_reuc_entry **reuc_out,
	const char *path,
	int ancestor_mode, const git_oid *ancestor_oid,
	int our_mode, const git_oid *our_oid,
	int their_mode, const git_oid *their_oid)
{
	git_index_reuc_entry *reuc = NULL;

	assert(reuc_out && path);

	*reuc_out = reuc = reuc_entry_alloc(path);
	GITERR_CHECK_ALLOC(reuc);

	if ((reuc->mode[0] = ancestor_mode) > 0) {
		assert(ancestor_oid);
		git_oid_cpy(&reuc->oid[0], ancestor_oid);
	}

	if ((reuc->mode[1] = our_mode) > 0) {
		assert(our_oid);
		git_oid_cpy(&reuc->oid[1], our_oid);
	}

	if ((reuc->mode[2] = their_mode) > 0) {
		assert(their_oid);
		git_oid_cpy(&reuc->oid[2], their_oid);
	}

	return 0;
}

static void index_entry_cpy(
	git_index_entry *tgt,
	const git_index_entry *src)
{
	const char *tgt_path = tgt->path;
	memcpy(tgt, src, sizeof(*tgt));
	tgt->path = tgt_path;
}

static int index_entry_dup(
	git_index_entry **out,
	git_index *index,
	const git_index_entry *src)
{
	if (index_entry_create(out, INDEX_OWNER(index), src->path, false) < 0)
		return -1;

	index_entry_cpy(*out, src);
	return 0;
}

static void index_entry_cpy_nocache(
	git_index_entry *tgt,
	const git_index_entry *src)
{
	git_oid_cpy(&tgt->id, &src->id);
	tgt->mode = src->mode;
	tgt->flags = src->flags;
	tgt->flags_extended = (src->flags_extended & GIT_IDXENTRY_EXTENDED_FLAGS);
}

static int index_entry_dup_nocache(
	git_index_entry **out,
	git_index *index,
	const git_index_entry *src)
{
	if (index_entry_create(out, INDEX_OWNER(index), src->path, false) < 0)
		return -1;

	index_entry_cpy_nocache(*out, src);
	return 0;
}

static int has_file_name(git_index *index,
	 const git_index_entry *entry, size_t pos, int ok_to_replace)
{
	int retval = 0;
	size_t len = strlen(entry->path);
	int stage = GIT_IDXENTRY_STAGE(entry);
	const char *name = entry->path;

	while (pos < index->entries.length) {
		struct entry_internal *p = index->entries.contents[pos++];

		if (len >= p->pathlen)
			break;
		if (memcmp(name, p->path, len))
			break;
		if (GIT_IDXENTRY_STAGE(&p->entry) != stage)
			continue;
		if (p->path[len] != '/')
			continue;
		retval = -1;
		if (!ok_to_replace)
			break;

		if (index_remove_entry(index, --pos) < 0)
			break;
	}
	return retval;
}

/*
 * Do we have another file with a pathname that is a proper
 * subset of the name we're trying to add?
 */
static int has_dir_name(git_index *index,
		const git_index_entry *entry, int ok_to_replace)
{
	int retval = 0;
	int stage = GIT_IDXENTRY_STAGE(entry);
	const char *name = entry->path;
	const char *slash = name + strlen(name);

	for (;;) {
		size_t len, pos;

		for (;;) {
			if (*--slash == '/')
				break;
			if (slash <= entry->path)
				return retval;
		}
		len = slash - name;

		if (!index_find(&pos, index, name, len, stage)) {
			retval = -1;
			if (!ok_to_replace)
				break;

			if (index_remove_entry(index, pos) < 0)
				break;
			continue;
		}

		/*
		 * Trivial optimization: if we find an entry that
		 * already matches the sub-directory, then we know
		 * we're ok, and we can exit.
		 */
		for (; pos < index->entries.length; ++pos) {
			struct entry_internal *p = index->entries.contents[pos];

			if (p->pathlen <= len ||
			    p->path[len] != '/' ||
			    memcmp(p->path, name, len))
				break; /* not our subdirectory */

			if (GIT_IDXENTRY_STAGE(&p->entry) == stage)
				return retval;
		}
	}

	return retval;
}

static int check_file_directory_collision(git_index *index,
		git_index_entry *entry, size_t pos, int ok_to_replace)
{
	int retval = has_file_name(index, entry, pos, ok_to_replace);
	retval = retval + has_dir_name(index, entry, ok_to_replace);

	if (retval) {
		giterr_set(GITERR_INDEX,
			"'%s' appears as both a file and a directory", entry->path);
		return -1;
	}

	return 0;
}

static int canonicalize_directory_path(
	git_index *index,
	git_index_entry *entry,
	git_index_entry *existing)
{
	const git_index_entry *match, *best = NULL;
	char *search, *sep;
	size_t pos, search_len, best_len;

	if (!index->ignore_case)
		return 0;

	/* item already exists in the index, simply re-use the existing case */
	if (existing) {
		memcpy((char *)entry->path, existing->path, strlen(existing->path));
		return 0;
	}

	/* nothing to do */
	if (strchr(entry->path, '/') == NULL)
		return 0;

	if ((search = git__strdup(entry->path)) == NULL)
		return -1;

	/* starting at the parent directory and descending to the root, find the
	 * common parent directory.
	 */
	while (!best && (sep = strrchr(search, '/'))) {
		sep[1] = '\0';

		search_len = strlen(search);

		git_vector_bsearch2(
			&pos, &index->entries, index->entries_search_path, search);

		while ((match = git_vector_get(&index->entries, pos))) {
			if (GIT_IDXENTRY_STAGE(match) != 0) {
				/* conflicts do not contribute to canonical paths */
			} else if (strncmp(search, match->path, search_len) == 0) {
				/* prefer an exact match to the input filename */
				best = match;
				best_len = search_len;
				break;
			} else if (strncasecmp(search, match->path, search_len) == 0) {
				/* continue walking, there may be a path with an exact
				 * (case sensitive) match later in the index, but use this
				 * as the best match until that happens.
				 */
				if (!best) {
					best = match;
					best_len = search_len;
				}
			} else {
				break;
			}

			pos++;
		}

		sep[0] = '\0';
	}

	if (best)
		memcpy((char *)entry->path, best->path, best_len);

	git__free(search);
	return 0;
}

static int index_no_dups(void **old, void *new)
{
	const git_index_entry *entry = new;
	GIT_UNUSED(old);
	giterr_set(GITERR_INDEX, "'%s' appears multiple times at stage %d",
		entry->path, GIT_IDXENTRY_STAGE(entry));
	return GIT_EEXISTS;
}

static void index_existing_and_best(
	git_index_entry **existing,
	size_t *existing_position,
	git_index_entry **best,
	git_index *index,
	const git_index_entry *entry)
{
	git_index_entry *e;
	size_t pos;
	int error;

	error = index_find(&pos,
		index, entry->path, 0, GIT_IDXENTRY_STAGE(entry));

	if (error == 0) {
		*existing = index->entries.contents[pos];
		*existing_position = pos;
		*best = index->entries.contents[pos];
		return;
	}

	*existing = NULL;
	*existing_position = 0;
	*best = NULL;

	if (GIT_IDXENTRY_STAGE(entry) == 0) {
		for (; pos < index->entries.length; pos++) {
			int (*strcomp)(const char *a, const char *b) =
				index->ignore_case ? git__strcasecmp : git__strcmp;

			e = index->entries.contents[pos];

			if (strcomp(entry->path, e->path) != 0)
				break;

			if (GIT_IDXENTRY_STAGE(e) == GIT_INDEX_STAGE_ANCESTOR) {
				*best = e;
				continue;
			} else {
				*best = e;
				break;
			}
		}
	}
}

/* index_insert takes ownership of the new entry - if it can't insert
 * it, then it will return an error **and also free the entry**.  When
 * it replaces an existing entry, it will update the entry_ptr with the
 * actual entry in the index (and free the passed in one).
 *
 * trust_path is whether we use the given path, or whether (on case
 * insensitive systems only) we try to canonicalize the given path to
 * be within an existing directory.
 *
 * trust_mode is whether we trust the mode in entry_ptr.
 *
 * trust_id is whether we trust the id or it should be validated.
 */
static int index_insert(
	git_index *index,
	git_index_entry **entry_ptr,
	int replace,
	bool trust_path,
	bool trust_mode,
	bool trust_id)
{
	int error = 0;
	size_t path_length, position;
	git_index_entry *existing, *best, *entry;

	assert(index && entry_ptr);

	entry = *entry_ptr;

	/* make sure that the path length flag is correct */
	path_length = ((struct entry_internal *)entry)->pathlen;
	index_entry_adjust_namemask(entry, path_length);

	/* this entry is now up-to-date and should not be checked for raciness */
	entry->flags_extended |= GIT_IDXENTRY_UPTODATE;

	git_vector_sort(&index->entries);

	/* look if an entry with this path already exists, either staged, or (if
	 * this entry is a regular staged item) as the "ours" side of a conflict.
	 */
	index_existing_and_best(&existing, &position, &best, index, entry);

	/* update the file mode */
	entry->mode = trust_mode ?
		git_index__create_mode(entry->mode) :
		index_merge_mode(index, best, entry->mode);

	/* canonicalize the directory name */
	if (!trust_path)
		error = canonicalize_directory_path(index, entry, best);

	/* ensure that the given id exists (unless it's a submodule) */
	if (!error && !trust_id && INDEX_OWNER(index) &&
		(entry->mode & GIT_FILEMODE_COMMIT) != GIT_FILEMODE_COMMIT) {

		if (!git_object__is_valid(INDEX_OWNER(index), &entry->id,
			git_object__type_from_filemode(entry->mode)))
			error = -1;
	}

	/* look for tree / blob name collisions, removing conflicts if requested */
	if (!error)
		error = check_file_directory_collision(index, entry, position, replace);

	if (error < 0)
		/* skip changes */;

	/* if we are replacing an existing item, overwrite the existing entry
	 * and return it in place of the passed in one.
	 */
	else if (existing) {
		if (replace) {
			index_entry_cpy(existing, entry);

			if (trust_path)
				memcpy((char *)existing->path, entry->path, strlen(entry->path));
		}

		index_entry_free(entry);
		*entry_ptr = entry = existing;
	}
	else {
		/* if replace is not requested or no existing entry exists, insert
		 * at the sorted position.  (Since we re-sort after each insert to
		 * check for dups, this is actually cheaper in the long run.)
		 */
		error = git_vector_insert_sorted(&index->entries, entry, index_no_dups);

		if (error == 0) {
			INSERT_IN_MAP(index, entry, error);
		}
	}

	if (error < 0) {
		index_entry_free(*entry_ptr);
		*entry_ptr = NULL;
	}

	return error;
}

static int index_conflict_to_reuc(git_index *index, const char *path)
{
	const git_index_entry *conflict_entries[3];
	int ancestor_mode, our_mode, their_mode;
	git_oid const *ancestor_oid, *our_oid, *their_oid;
	int ret;

	if ((ret = git_index_conflict_get(&conflict_entries[0],
		&conflict_entries[1], &conflict_entries[2], index, path)) < 0)
		return ret;

	ancestor_mode = conflict_entries[0] == NULL ? 0 : conflict_entries[0]->mode;
	our_mode = conflict_entries[1] == NULL ? 0 : conflict_entries[1]->mode;
	their_mode = conflict_entries[2] == NULL ? 0 : conflict_entries[2]->mode;

	ancestor_oid = conflict_entries[0] == NULL ? NULL : &conflict_entries[0]->id;
	our_oid = conflict_entries[1] == NULL ? NULL : &conflict_entries[1]->id;
	their_oid = conflict_entries[2] == NULL ? NULL : &conflict_entries[2]->id;

	if ((ret = git_index_reuc_add(index, path, ancestor_mode, ancestor_oid,
		our_mode, our_oid, their_mode, their_oid)) >= 0)
		ret = git_index_conflict_remove(index, path);

	return ret;
}

static bool valid_filemode(const int filemode)
{
	return (filemode == GIT_FILEMODE_BLOB ||
		filemode == GIT_FILEMODE_BLOB_EXECUTABLE ||
		filemode == GIT_FILEMODE_LINK ||
		filemode == GIT_FILEMODE_COMMIT);
}

int git_index_add_frombuffer(
    git_index *index, const git_index_entry *source_entry,
    const void *buffer, size_t len)
{
	git_index_entry *entry = NULL;
	int error = 0;
	git_oid id;

	assert(index && source_entry->path);

	if (INDEX_OWNER(index) == NULL)
		return create_index_error(-1,
			"Could not initialize index entry. "
			"Index is not backed up by an existing repository.");

	if (!valid_filemode(source_entry->mode)) {
		giterr_set(GITERR_INDEX, "invalid filemode");
		return -1;
	}

	if (index_entry_dup(&entry, index, source_entry) < 0)
		return -1;

	error = git_blob_create_frombuffer(&id, INDEX_OWNER(index), buffer, len);
	if (error < 0) {
		index_entry_free(entry);
		return error;
	}

	git_oid_cpy(&entry->id, &id);
	entry->file_size = len;

	if ((error = index_insert(index, &entry, 1, true, true, true)) < 0)
		return error;

	/* Adding implies conflict was resolved, move conflict entries to REUC */
	if ((error = index_conflict_to_reuc(index, entry->path)) < 0 && error != GIT_ENOTFOUND)
		return error;

	git_tree_cache_invalidate_path(index->tree, entry->path);
	return 0;
}

static int add_repo_as_submodule(git_index_entry **out, git_index *index, const char *path)
{
	git_repository *sub;
	git_buf abspath = GIT_BUF_INIT;
	git_repository *repo = INDEX_OWNER(index);
	git_reference *head;
	git_index_entry *entry;
	struct stat st;
	int error;

	if (index_entry_create(&entry, INDEX_OWNER(index), path, true) < 0)
		return -1;

	if ((error = git_buf_joinpath(&abspath, git_repository_workdir(repo), path)) < 0)
		return error;

	if ((error = p_stat(abspath.ptr, &st)) < 0) {
		giterr_set(GITERR_OS, "failed to stat repository dir");
		return -1;
	}

	git_index_entry__init_from_stat(entry, &st, !index->distrust_filemode);

	if ((error = git_repository_open(&sub, abspath.ptr)) < 0)
		return error;

	if ((error = git_repository_head(&head, sub)) < 0)
		return error;

	git_oid_cpy(&entry->id, git_reference_target(head));
	entry->mode = GIT_FILEMODE_COMMIT;

	git_reference_free(head);
	git_repository_free(sub);
	git_buf_free(&abspath);

	*out = entry;
	return 0;
}

int git_index_add_bypath(git_index *index, const char *path)
{
	git_index_entry *entry = NULL;
	int ret;

	assert(index && path);

	if ((ret = index_entry_init(&entry, index, path)) == 0)
		ret = index_insert(index, &entry, 1, false, false, true);

	/* If we were given a directory, let's see if it's a submodule */
	if (ret < 0 && ret != GIT_EDIRECTORY)
		return ret;

	if (ret == GIT_EDIRECTORY) {
		git_submodule *sm;
		git_error_state err;

		giterr_state_capture(&err, ret);

		ret = git_submodule_lookup(&sm, INDEX_OWNER(index), path);
		if (ret == GIT_ENOTFOUND)
			return giterr_state_restore(&err);

		giterr_state_free(&err);

		/*
		 * EEXISTS means that there is a repository at that path, but it's not known
		 * as a submodule. We add its HEAD as an entry and don't register it.
		 */
		if (ret == GIT_EEXISTS) {
			if ((ret = add_repo_as_submodule(&entry, index, path)) < 0)
				return ret;

			if ((ret = index_insert(index, &entry, 1, false, false, true)) < 0)
				return ret;
		} else if (ret < 0) {
			return ret;
		} else {
			ret = git_submodule_add_to_index(sm, false);
			git_submodule_free(sm);
			return ret;
		}
	}

	/* Adding implies conflict was resolved, move conflict entries to REUC */
	if ((ret = index_conflict_to_reuc(index, path)) < 0 && ret != GIT_ENOTFOUND)
		return ret;

	git_tree_cache_invalidate_path(index->tree, entry->path);
	return 0;
}

int git_index_remove_bypath(git_index *index, const char *path)
{
	int ret;

	assert(index && path);

	if (((ret = git_index_remove(index, path, 0)) < 0 &&
		ret != GIT_ENOTFOUND) ||
		((ret = index_conflict_to_reuc(index, path)) < 0 &&
		ret != GIT_ENOTFOUND))
		return ret;

	if (ret == GIT_ENOTFOUND)
		giterr_clear();

	return 0;
}

int git_index__fill(git_index *index, const git_vector *source_entries)
{
	const git_index_entry *source_entry = NULL;
	size_t i;
	int ret = 0;

	assert(index);

	if (!source_entries->length)
		return 0;

	git_vector_size_hint(&index->entries, source_entries->length);
	git_idxmap_resize(index->entries_map, (khint_t)(source_entries->length * 1.3));

	git_vector_foreach(source_entries, i, source_entry) {
		git_index_entry *entry = NULL;

		if ((ret = index_entry_dup(&entry, index, source_entry)) < 0)
			break;

		index_entry_adjust_namemask(entry, ((struct entry_internal *)entry)->pathlen);
		entry->flags_extended |= GIT_IDXENTRY_UPTODATE;
		entry->mode = git_index__create_mode(entry->mode);

		if ((ret = git_vector_insert(&index->entries, entry)) < 0)
			break;

		INSERT_IN_MAP(index, entry, ret);
		if (ret < 0)
			break;
	}

	if (!ret)
		git_vector_sort(&index->entries);

	return ret;
}


int git_index_add(git_index *index, const git_index_entry *source_entry)
{
	git_index_entry *entry = NULL;
	int ret;

	assert(index && source_entry && source_entry->path);

	if (!valid_filemode(source_entry->mode)) {
		giterr_set(GITERR_INDEX, "invalid filemode");
		return -1;
	}

	if ((ret = index_entry_dup(&entry, index, source_entry)) < 0 ||
		(ret = index_insert(index, &entry, 1, true, true, false)) < 0)
		return ret;

	git_tree_cache_invalidate_path(index->tree, entry->path);
	return 0;
}

int git_index_remove(git_index *index, const char *path, int stage)
{
	int error;
	size_t position;
	git_index_entry remove_key = {{ 0 }};

	remove_key.path = path;
	GIT_IDXENTRY_STAGE_SET(&remove_key, stage);

	DELETE_IN_MAP(index, &remove_key);

	if (index_find(&position, index, path, 0, stage) < 0) {
		giterr_set(
			GITERR_INDEX, "Index does not contain %s at stage %d", path, stage);
		error = GIT_ENOTFOUND;
	} else {
		error = index_remove_entry(index, position);
	}

	return error;
}

int git_index_remove_directory(git_index *index, const char *dir, int stage)
{
	git_buf pfx = GIT_BUF_INIT;
	int error = 0;
	size_t pos;
	git_index_entry *entry;

	if (!(error = git_buf_sets(&pfx, dir)) &&
		!(error = git_path_to_dir(&pfx)))
		index_find(&pos, index, pfx.ptr, pfx.size, GIT_INDEX_STAGE_ANY);

	while (!error) {
		entry = git_vector_get(&index->entries, pos);
		if (!entry || git__prefixcmp(entry->path, pfx.ptr) != 0)
			break;

		if (GIT_IDXENTRY_STAGE(entry) != stage) {
			++pos;
			continue;
		}

		error = index_remove_entry(index, pos);

		/* removed entry at 'pos' so we don't need to increment */
	}

	git_buf_free(&pfx);

	return error;
}

int git_index_find_prefix(size_t *at_pos, git_index *index, const char *prefix)
{
	int error = 0;
	size_t pos;
	const git_index_entry *entry;

	index_find(&pos, index, prefix, strlen(prefix), GIT_INDEX_STAGE_ANY);
	entry = git_vector_get(&index->entries, pos);
	if (!entry || git__prefixcmp(entry->path, prefix) != 0)
		error = GIT_ENOTFOUND;

	if (!error && at_pos)
		*at_pos = pos;

	return error;
}

int git_index__find_pos(
	size_t *out, git_index *index, const char *path, size_t path_len, int stage)
{
	assert(index && path);
	return index_find(out, index, path, path_len, stage);
}

int git_index_find(size_t *at_pos, git_index *index, const char *path)
{
	size_t pos;

	assert(index && path);

	if (git_vector_bsearch2(
			&pos, &index->entries, index->entries_search_path, path) < 0) {
		giterr_set(GITERR_INDEX, "Index does not contain %s", path);
		return GIT_ENOTFOUND;
	}

	/* Since our binary search only looked at path, we may be in the
	 * middle of a list of stages.
	 */
	for (; pos > 0; --pos) {
		const git_index_entry *prev = git_vector_get(&index->entries, pos - 1);

		if (index->entries_cmp_path(prev->path, path) != 0)
			break;
	}

	if (at_pos)
		*at_pos = pos;

	return 0;
}

int git_index_conflict_add(git_index *index,
	const git_index_entry *ancestor_entry,
	const git_index_entry *our_entry,
	const git_index_entry *their_entry)
{
	git_index_entry *entries[3] = { 0 };
	unsigned short i;
	int ret = 0;

	assert (index);

	if ((ancestor_entry &&
			(ret = index_entry_dup(&entries[0], index, ancestor_entry)) < 0) ||
		(our_entry &&
			(ret = index_entry_dup(&entries[1], index, our_entry)) < 0) ||
		(their_entry &&
			(ret = index_entry_dup(&entries[2], index, their_entry)) < 0))
		goto on_error;

	/* Validate entries */
	for (i = 0; i < 3; i++) {
		if (entries[i] && !valid_filemode(entries[i]->mode)) {
			giterr_set(GITERR_INDEX, "invalid filemode for stage %d entry",
				i + 1);
			return -1;
		}
	}

	/* Remove existing index entries for each path */
	for (i = 0; i < 3; i++) {
		if (entries[i] == NULL)
			continue;

		if ((ret = git_index_remove(index, entries[i]->path, 0)) != 0) {
			if (ret != GIT_ENOTFOUND)
				goto on_error;

			giterr_clear();
			ret = 0;
		}
	}

	/* Add the conflict entries */
	for (i = 0; i < 3; i++) {
		if (entries[i] == NULL)
			continue;

		/* Make sure stage is correct */
		GIT_IDXENTRY_STAGE_SET(entries[i], i + 1);

		if ((ret = index_insert(index, &entries[i], 1, true, true, false)) < 0)
			goto on_error;

		entries[i] = NULL; /* don't free if later entry fails */
	}

	return 0;

on_error:
	for (i = 0; i < 3; i++) {
		if (entries[i] != NULL)
			index_entry_free(entries[i]);
	}

	return ret;
}

static int index_conflict__get_byindex(
	const git_index_entry **ancestor_out,
	const git_index_entry **our_out,
	const git_index_entry **their_out,
	git_index *index,
	size_t n)
{
	const git_index_entry *conflict_entry;
	const char *path = NULL;
	size_t count;
	int stage, len = 0;

	assert(ancestor_out && our_out && their_out && index);

	*ancestor_out = NULL;
	*our_out = NULL;
	*their_out = NULL;

	for (count = git_index_entrycount(index); n < count; ++n) {
		conflict_entry = git_vector_get(&index->entries, n);

		if (path && index->entries_cmp_path(conflict_entry->path, path) != 0)
			break;

		stage = GIT_IDXENTRY_STAGE(conflict_entry);
		path = conflict_entry->path;

		switch (stage) {
		case 3:
			*their_out = conflict_entry;
			len++;
			break;
		case 2:
			*our_out = conflict_entry;
			len++;
			break;
		case 1:
			*ancestor_out = conflict_entry;
			len++;
			break;
		default:
			break;
		};
	}

	return len;
}

int git_index_conflict_get(
	const git_index_entry **ancestor_out,
	const git_index_entry **our_out,
	const git_index_entry **their_out,
	git_index *index,
	const char *path)
{
	size_t pos;
	int len = 0;

	assert(ancestor_out && our_out && their_out && index && path);

	*ancestor_out = NULL;
	*our_out = NULL;
	*their_out = NULL;

	if (git_index_find(&pos, index, path) < 0)
		return GIT_ENOTFOUND;

	if ((len = index_conflict__get_byindex(
		ancestor_out, our_out, their_out, index, pos)) < 0)
		return len;
	else if (len == 0)
		return GIT_ENOTFOUND;

	return 0;
}

static int index_conflict_remove(git_index *index, const char *path)
{
	size_t pos = 0;
	git_index_entry *conflict_entry;
	int error = 0;

	if (path != NULL && git_index_find(&pos, index, path) < 0)
		return GIT_ENOTFOUND;

	while ((conflict_entry = git_vector_get(&index->entries, pos)) != NULL) {

		if (path != NULL &&
			index->entries_cmp_path(conflict_entry->path, path) != 0)
			break;

		if (GIT_IDXENTRY_STAGE(conflict_entry) == 0) {
			pos++;
			continue;
		}

		if ((error = index_remove_entry(index, pos)) < 0)
			break;
	}

	return error;
}

int git_index_conflict_remove(git_index *index, const char *path)
{
	assert(index && path);
	return index_conflict_remove(index, path);
}

int git_index_conflict_cleanup(git_index *index)
{
	assert(index);
	return index_conflict_remove(index, NULL);
}

int git_index_has_conflicts(const git_index *index)
{
	size_t i;
	git_index_entry *entry;

	assert(index);

	git_vector_foreach(&index->entries, i, entry) {
		if (GIT_IDXENTRY_STAGE(entry) > 0)
			return 1;
	}

	return 0;
}

int git_index_conflict_iterator_new(
	git_index_conflict_iterator **iterator_out,
	git_index *index)
{
	git_index_conflict_iterator *it = NULL;

	assert(iterator_out && index);

	it = git__calloc(1, sizeof(git_index_conflict_iterator));
	GITERR_CHECK_ALLOC(it);

	it->index = index;

	*iterator_out = it;
	return 0;
}

int git_index_conflict_next(
	const git_index_entry **ancestor_out,
	const git_index_entry **our_out,
	const git_index_entry **their_out,
	git_index_conflict_iterator *iterator)
{
	const git_index_entry *entry;
	int len;

	assert(ancestor_out && our_out && their_out && iterator);

	*ancestor_out = NULL;
	*our_out = NULL;
	*their_out = NULL;

	while (iterator->cur < iterator->index->entries.length) {
		entry = git_index_get_byindex(iterator->index, iterator->cur);

		if (git_index_entry_is_conflict(entry)) {
			if ((len = index_conflict__get_byindex(
				ancestor_out,
				our_out,
				their_out,
				iterator->index,
				iterator->cur)) < 0)
				return len;

			iterator->cur += len;
			return 0;
		}

		iterator->cur++;
	}

	return GIT_ITEROVER;
}

void git_index_conflict_iterator_free(git_index_conflict_iterator *iterator)
{
	if (iterator == NULL)
		return;

	git__free(iterator);
}

size_t git_index_name_entrycount(git_index *index)
{
	assert(index);
	return index->names.length;
}

const git_index_name_entry *git_index_name_get_byindex(
	git_index *index, size_t n)
{
	assert(index);

	git_vector_sort(&index->names);
	return git_vector_get(&index->names, n);
}

static void index_name_entry_free(git_index_name_entry *ne)
{
	if (!ne)
		return;
	git__free(ne->ancestor);
	git__free(ne->ours);
	git__free(ne->theirs);
	git__free(ne);
}

int git_index_name_add(git_index *index,
	const char *ancestor, const char *ours, const char *theirs)
{
	git_index_name_entry *conflict_name;

	assert((ancestor && ours) || (ancestor && theirs) || (ours && theirs));

	conflict_name = git__calloc(1, sizeof(git_index_name_entry));
	GITERR_CHECK_ALLOC(conflict_name);

	if ((ancestor && !(conflict_name->ancestor = git__strdup(ancestor))) ||
		(ours     && !(conflict_name->ours     = git__strdup(ours))) ||
		(theirs   && !(conflict_name->theirs   = git__strdup(theirs))) ||
		git_vector_insert(&index->names, conflict_name) < 0)
	{
		index_name_entry_free(conflict_name);
		return -1;
	}

	return 0;
}

void git_index_name_clear(git_index *index)
{
	size_t i;
	git_index_name_entry *conflict_name;

	assert(index);

	git_vector_foreach(&index->names, i, conflict_name)
		index_name_entry_free(conflict_name);

	git_vector_clear(&index->names);
}

size_t git_index_reuc_entrycount(git_index *index)
{
	assert(index);
	return index->reuc.length;
}

static int index_reuc_on_dup(void **old, void *new)
{
	index_entry_reuc_free(*old);
	*old = new;
	return GIT_EEXISTS;
}

static int index_reuc_insert(
	git_index *index,
	git_index_reuc_entry *reuc)
{
	int res;

	assert(index && reuc && reuc->path != NULL);
	assert(git_vector_is_sorted(&index->reuc));

	res = git_vector_insert_sorted(&index->reuc, reuc, &index_reuc_on_dup);
	return res == GIT_EEXISTS ? 0 : res;
}

int git_index_reuc_add(git_index *index, const char *path,
	int ancestor_mode, const git_oid *ancestor_oid,
	int our_mode, const git_oid *our_oid,
	int their_mode, const git_oid *their_oid)
{
	git_index_reuc_entry *reuc = NULL;
	int error = 0;

	assert(index && path);

	if ((error = index_entry_reuc_init(&reuc, path, ancestor_mode,
			ancestor_oid, our_mode, our_oid, their_mode, their_oid)) < 0 ||
		(error = index_reuc_insert(index, reuc)) < 0)
		index_entry_reuc_free(reuc);

	return error;
}

int git_index_reuc_find(size_t *at_pos, git_index *index, const char *path)
{
	return git_vector_bsearch2(at_pos, &index->reuc, index->reuc_search, path);
}

const git_index_reuc_entry *git_index_reuc_get_bypath(
	git_index *index, const char *path)
{
	size_t pos;
	assert(index && path);

	if (!index->reuc.length)
		return NULL;

	assert(git_vector_is_sorted(&index->reuc));

	if (git_index_reuc_find(&pos, index, path) < 0)
		return NULL;

	return git_vector_get(&index->reuc, pos);
}

const git_index_reuc_entry *git_index_reuc_get_byindex(
	git_index *index, size_t n)
{
	assert(index);
	assert(git_vector_is_sorted(&index->reuc));

	return git_vector_get(&index->reuc, n);
}

int git_index_reuc_remove(git_index *index, size_t position)
{
	int error;
	git_index_reuc_entry *reuc;

	assert(git_vector_is_sorted(&index->reuc));

	reuc = git_vector_get(&index->reuc, position);
	error = git_vector_remove(&index->reuc, position);

	if (!error)
		index_entry_reuc_free(reuc);

	return error;
}

void git_index_reuc_clear(git_index *index)
{
	size_t i;

	assert(index);

	for (i = 0; i < index->reuc.length; ++i)
		index_entry_reuc_free(git__swap(index->reuc.contents[i], NULL));

	git_vector_clear(&index->reuc);
}

static int index_error_invalid(const char *message)
{
	giterr_set(GITERR_INDEX, "Invalid data in index - %s", message);
	return -1;
}

static int read_reuc(git_index *index, const char *buffer, size_t size)
{
	const char *endptr;
	size_t len;
	int i;

	/* If called multiple times, the vector might already be initialized */
	if (index->reuc._alloc_size == 0 &&
		git_vector_init(&index->reuc, 16, reuc_cmp) < 0)
		return -1;

	while (size) {
		git_index_reuc_entry *lost;

		len = p_strnlen(buffer, size) + 1;
		if (size <= len)
			return index_error_invalid("reading reuc entries");

		lost = reuc_entry_alloc(buffer);
		GITERR_CHECK_ALLOC(lost);

		size -= len;
		buffer += len;

		/* read 3 ASCII octal numbers for stage entries */
		for (i = 0; i < 3; i++) {
			int64_t tmp;

			if (git__strtol64(&tmp, buffer, &endptr, 8) < 0 ||
				!endptr || endptr == buffer || *endptr ||
				tmp < 0 || tmp > UINT32_MAX) {
				index_entry_reuc_free(lost);
				return index_error_invalid("reading reuc entry stage");
			}

			lost->mode[i] = (uint32_t)tmp;

			len = (endptr + 1) - buffer;
			if (size <= len) {
				index_entry_reuc_free(lost);
				return index_error_invalid("reading reuc entry stage");
			}

			size -= len;
			buffer += len;
		}

		/* read up to 3 OIDs for stage entries */
		for (i = 0; i < 3; i++) {
			if (!lost->mode[i])
				continue;
			if (size < 20) {
				index_entry_reuc_free(lost);
				return index_error_invalid("reading reuc entry oid");
			}

			git_oid_fromraw(&lost->oid[i], (const unsigned char *) buffer);
			size -= 20;
			buffer += 20;
		}

		/* entry was read successfully - insert into reuc vector */
		if (git_vector_insert(&index->reuc, lost) < 0)
			return -1;
	}

	/* entries are guaranteed to be sorted on-disk */
	git_vector_set_sorted(&index->reuc, true);

	return 0;
}


static int read_conflict_names(git_index *index, const char *buffer, size_t size)
{
	size_t len;

	/* This gets called multiple times, the vector might already be initialized */
	if (index->names._alloc_size == 0 &&
		git_vector_init(&index->names, 16, conflict_name_cmp) < 0)
		return -1;

#define read_conflict_name(ptr) \
	len = p_strnlen(buffer, size) + 1; \
	if (size < len) { \
		index_error_invalid("reading conflict name entries"); \
		goto out_err; \
	} \
	if (len == 1) \
		ptr = NULL; \
	else { \
		ptr = git__malloc(len); \
		GITERR_CHECK_ALLOC(ptr); \
		memcpy(ptr, buffer, len); \
	} \
	\
	buffer += len; \
	size -= len;

	while (size) {
		git_index_name_entry *conflict_name = git__calloc(1, sizeof(git_index_name_entry));
		GITERR_CHECK_ALLOC(conflict_name);

		read_conflict_name(conflict_name->ancestor);
		read_conflict_name(conflict_name->ours);
		read_conflict_name(conflict_name->theirs);

		if (git_vector_insert(&index->names, conflict_name) < 0)
			goto out_err;

		continue;

out_err:
		git__free(conflict_name->ancestor);
		git__free(conflict_name->ours);
		git__free(conflict_name->theirs);
		git__free(conflict_name);
		return -1;
	}

#undef read_conflict_name

	/* entries are guaranteed to be sorted on-disk */
	git_vector_set_sorted(&index->names, true);

	return 0;
}

static size_t read_entry(
	git_index_entry **out,
	git_index *index,
	const void *buffer,
	size_t buffer_size,
	const char **last)
{
	size_t path_length, entry_size;
	const char *path_ptr;
	struct entry_short source;
	git_index_entry entry = {{0}};
	bool compressed = index->version >= INDEX_VERSION_NUMBER_COMP;
	char *tmp_path = NULL;

	if (INDEX_FOOTER_SIZE + minimal_entry_size > buffer_size)
		return 0;

	/* buffer is not guaranteed to be aligned */
	memcpy(&source, buffer, sizeof(struct entry_short));

	entry.ctime.seconds = (git_time_t)ntohl(source.ctime.seconds);
	entry.ctime.nanoseconds = ntohl(source.ctime.nanoseconds);
	entry.mtime.seconds = (git_time_t)ntohl(source.mtime.seconds);
	entry.mtime.nanoseconds = ntohl(source.mtime.nanoseconds);
	entry.dev = ntohl(source.dev);
	entry.ino = ntohl(source.ino);
	entry.mode = ntohl(source.mode);
	entry.uid = ntohl(source.uid);
	entry.gid = ntohl(source.gid);
	entry.file_size = ntohl(source.file_size);
	git_oid_cpy(&entry.id, &source.oid);
	entry.flags = ntohs(source.flags);

	if (entry.flags & GIT_IDXENTRY_EXTENDED) {
		uint16_t flags_raw;
		size_t flags_offset;

		flags_offset = offsetof(struct entry_long, flags_extended);
		memcpy(&flags_raw, (const char *) buffer + flags_offset,
			sizeof(flags_raw));
		flags_raw = ntohs(flags_raw);

		memcpy(&entry.flags_extended, &flags_raw, sizeof(flags_raw));
		path_ptr = (const char *) buffer + offsetof(struct entry_long, path);
	} else
		path_ptr = (const char *) buffer + offsetof(struct entry_short, path);

	if (!compressed) {
		path_length = entry.flags & GIT_IDXENTRY_NAMEMASK;

		/* if this is a very long string, we must find its
		 * real length without overflowing */
		if (path_length == 0xFFF) {
			const char *path_end;

			path_end = memchr(path_ptr, '\0', buffer_size);
			if (path_end == NULL)
				return 0;

			path_length = path_end - path_ptr;
		}

		if (entry.flags & GIT_IDXENTRY_EXTENDED)
			entry_size = long_entry_size(path_length);
		else
			entry_size = short_entry_size(path_length);

		if (INDEX_FOOTER_SIZE + entry_size > buffer_size)
			return 0;

		entry.path = (char *)path_ptr;
	} else {
		size_t varint_len;
		size_t shared = git_decode_varint((const unsigned char *)path_ptr, 
						  &varint_len);
		size_t len = strlen(path_ptr + varint_len);
		size_t last_len = strlen(*last);
		size_t tmp_path_len;

		if (varint_len == 0)
			return index_error_invalid("incorrect prefix length");

		GITERR_CHECK_ALLOC_ADD(&tmp_path_len, shared, len + 1);
		tmp_path = git__malloc(tmp_path_len);
		GITERR_CHECK_ALLOC(tmp_path);
		memcpy(tmp_path, last, last_len);
		memcpy(tmp_path + last_len, path_ptr + varint_len, len);
		entry_size = long_entry_size(shared + len);
		entry.path = tmp_path;
	}

	if (index_entry_dup(out, index, &entry) < 0) {
		git__free(tmp_path);
		return 0;
	}

	git__free(tmp_path);
	return entry_size;
}

static int read_header(struct index_header *dest, const void *buffer)
{
	const struct index_header *source = buffer;

	dest->signature = ntohl(source->signature);
	if (dest->signature != INDEX_HEADER_SIG)
		return index_error_invalid("incorrect header signature");

	dest->version = ntohl(source->version);
	if (dest->version < INDEX_VERSION_NUMBER_LB ||
		dest->version > INDEX_VERSION_NUMBER_UB)
		return index_error_invalid("incorrect header version");

	dest->entry_count = ntohl(source->entry_count);
	return 0;
}

static size_t read_extension(git_index *index, const char *buffer, size_t buffer_size)
{
	struct index_extension dest;
	size_t total_size;

	/* buffer is not guaranteed to be aligned */
	memcpy(&dest, buffer, sizeof(struct index_extension));
	dest.extension_size = ntohl(dest.extension_size);

	total_size = dest.extension_size + sizeof(struct index_extension);

	if (dest.extension_size > total_size ||
		buffer_size < total_size ||
		buffer_size - total_size < INDEX_FOOTER_SIZE)
		return 0;

	/* optional extension */
	if (dest.signature[0] >= 'A' && dest.signature[0] <= 'Z') {
		/* tree cache */
		if (memcmp(dest.signature, INDEX_EXT_TREECACHE_SIG, 4) == 0) {
			if (git_tree_cache_read(&index->tree, buffer + 8, dest.extension_size, &index->tree_pool) < 0)
				return 0;
		} else if (memcmp(dest.signature, INDEX_EXT_UNMERGED_SIG, 4) == 0) {
			if (read_reuc(index, buffer + 8, dest.extension_size) < 0)
				return 0;
		} else if (memcmp(dest.signature, INDEX_EXT_CONFLICT_NAME_SIG, 4) == 0) {
			if (read_conflict_names(index, buffer + 8, dest.extension_size) < 0)
				return 0;
		}
		/* else, unsupported extension. We cannot parse this, but we can skip
		 * it by returning `total_size */
	} else {
		/* we cannot handle non-ignorable extensions;
		 * in fact they aren't even defined in the standard */
		return 0;
	}

	return total_size;
}

static int parse_index(git_index *index, const char *buffer, size_t buffer_size)
{
	int error = 0;
	unsigned int i;
	struct index_header header = { 0 };
	git_oid checksum_calculated, checksum_expected;
	const char **last = NULL;
	const char *empty = "";

#define seek_forward(_increase) { \
	if (_increase >= buffer_size) { \
		error = index_error_invalid("ran out of data while parsing"); \
		goto done; } \
	buffer += _increase; \
	buffer_size -= _increase;\
}

	if (buffer_size < INDEX_HEADER_SIZE + INDEX_FOOTER_SIZE)
		return index_error_invalid("insufficient buffer space");

	/* Precalculate the SHA1 of the files's contents -- we'll match it to
	 * the provided SHA1 in the footer */
	git_hash_buf(&checksum_calculated, buffer, buffer_size - INDEX_FOOTER_SIZE);

	/* Parse header */
	if ((error = read_header(&header, buffer)) < 0)
		return error;

	index->version = header.version;
	if (index->version >= INDEX_VERSION_NUMBER_COMP)
		last = &empty;

	seek_forward(INDEX_HEADER_SIZE);

	assert(!index->entries.length);

	if (index->ignore_case)
		kh_resize(idxicase, (khash_t(idxicase) *) index->entries_map, header.entry_count);
	else
		kh_resize(idx, index->entries_map, header.entry_count);

	/* Parse all the entries */
	for (i = 0; i < header.entry_count && buffer_size > INDEX_FOOTER_SIZE; ++i) {
		git_index_entry *entry;
		size_t entry_size = read_entry(&entry, index, buffer, buffer_size, last);

		/* 0 bytes read means an object corruption */
		if (entry_size == 0) {
			error = index_error_invalid("invalid entry");
			goto done;
		}

		if ((error = git_vector_insert(&index->entries, entry)) < 0) {
			index_entry_free(entry);
			goto done;
		}

		INSERT_IN_MAP(index, entry, error);

		if (error < 0) {
			index_entry_free(entry);
			goto done;
		}
		error = 0;

		seek_forward(entry_size);
	}

	if (i != header.entry_count) {
		error = index_error_invalid("header entries changed while parsing");
		goto done;
	}

	/* There's still space for some extensions! */
	while (buffer_size > INDEX_FOOTER_SIZE) {
		size_t extension_size;

		extension_size = read_extension(index, buffer, buffer_size);

		/* see if we have read any bytes from the extension */
		if (extension_size == 0) {
			error = index_error_invalid("extension is truncated");
			goto done;
		}

		seek_forward(extension_size);
	}

	if (buffer_size != INDEX_FOOTER_SIZE) {
		error = index_error_invalid(
			"buffer size does not match index footer size");
		goto done;
	}

	/* 160-bit SHA-1 over the content of the index file before this checksum. */
	git_oid_fromraw(&checksum_expected, (const unsigned char *)buffer);

	if (git_oid__cmp(&checksum_calculated, &checksum_expected) != 0) {
		error = index_error_invalid(
			"calculated checksum does not match expected");
		goto done;
	}

	git_oid_cpy(&index->checksum, &checksum_calculated);

#undef seek_forward

	/* Entries are stored case-sensitively on disk, so re-sort now if
	 * in-memory index is supposed to be case-insensitive
	 */
	git_vector_set_sorted(&index->entries, !index->ignore_case);
	git_vector_sort(&index->entries);

done:
	return error;
}

static bool is_index_extended(git_index *index)
{
	size_t i, extended;
	git_index_entry *entry;

	extended = 0;

	git_vector_foreach(&index->entries, i, entry) {
		entry->flags &= ~GIT_IDXENTRY_EXTENDED;
		if (entry->flags_extended & GIT_IDXENTRY_EXTENDED_FLAGS) {
			extended++;
			entry->flags |= GIT_IDXENTRY_EXTENDED;
		}
	}

	return (extended > 0);
}

static int write_disk_entry(git_filebuf *file, git_index_entry *entry, const char **last)
{
	void *mem = NULL;
	struct entry_short *ondisk;
	size_t path_len, disk_size;
	char *path;
	const char *path_start = entry->path;
	size_t same_len = 0;

	path_len = ((struct entry_internal *)entry)->pathlen;

	if (last) {
		const char *last_c = *last;

		while (*path_start == *last_c) {
			if (!*path_start || !*last_c)
				break;
			++path_start;
			++last_c;
			++same_len;
		}
		path_len -= same_len;
		*last = entry->path;
	}

	if (entry->flags & GIT_IDXENTRY_EXTENDED)
		disk_size = long_entry_size(path_len);
	else
		disk_size = short_entry_size(path_len);

	if (git_filebuf_reserve(file, &mem, disk_size) < 0)
		return -1;

	ondisk = (struct entry_short *)mem;

	memset(ondisk, 0x0, disk_size);

	/**
	 * Yes, we have to truncate.
	 *
	 * The on-disk format for Index entries clearly defines
	 * the time and size fields to be 4 bytes each -- so even if
	 * we store these values with 8 bytes on-memory, they must
	 * be truncated to 4 bytes before writing to disk.
	 *
	 * In 2038 I will be either too dead or too rich to care about this
	 */
	ondisk->ctime.seconds = htonl((uint32_t)entry->ctime.seconds);
	ondisk->mtime.seconds = htonl((uint32_t)entry->mtime.seconds);
	ondisk->ctime.nanoseconds = htonl(entry->ctime.nanoseconds);
	ondisk->mtime.nanoseconds = htonl(entry->mtime.nanoseconds);
	ondisk->dev = htonl(entry->dev);
	ondisk->ino = htonl(entry->ino);
	ondisk->mode = htonl(entry->mode);
	ondisk->uid = htonl(entry->uid);
	ondisk->gid = htonl(entry->gid);
	ondisk->file_size = htonl((uint32_t)entry->file_size);

	git_oid_cpy(&ondisk->oid, &entry->id);

	ondisk->flags = htons(entry->flags);

	if (entry->flags & GIT_IDXENTRY_EXTENDED) {
		struct entry_long *ondisk_ext;
		ondisk_ext = (struct entry_long *)ondisk;
		ondisk_ext->flags_extended = htons(entry->flags_extended &
			GIT_IDXENTRY_EXTENDED_FLAGS);
		path = ondisk_ext->path;
	}
	else
		path = ondisk->path;

	if (last) {
		path += git_encode_varint((unsigned char *) path,
					  disk_size,
					  path_len - same_len);
	}
	memcpy(path, path_start, path_len);

	return 0;
}

static int write_entries(git_index *index, git_filebuf *file)
{
	int error = 0;
	size_t i;
	git_vector case_sorted, *entries;
	git_index_entry *entry;
	const char **last = NULL;
	const char *empty = "";

	/* If index->entries is sorted case-insensitively, then we need
	 * to re-sort it case-sensitively before writing */
	if (index->ignore_case) {
		git_vector_dup(&case_sorted, &index->entries, git_index_entry_cmp);
		git_vector_sort(&case_sorted);
		entries = &case_sorted;
	} else {
		entries = &index->entries;
	}

	if (index->version >= INDEX_VERSION_NUMBER_COMP)
		last = &empty;

	git_vector_foreach(entries, i, entry)
		if ((error = write_disk_entry(file, entry, last)) < 0)
			break;

	if (index->ignore_case)
		git_vector_free(&case_sorted);

	return error;
}

static int write_extension(git_filebuf *file, struct index_extension *header, git_buf *data)
{
	struct index_extension ondisk;

	memset(&ondisk, 0x0, sizeof(struct index_extension));
	memcpy(&ondisk, header, 4);
	ondisk.extension_size = htonl(header->extension_size);

	git_filebuf_write(file, &ondisk, sizeof(struct index_extension));
	return git_filebuf_write(file, data->ptr, data->size);
}

static int create_name_extension_data(git_buf *name_buf, git_index_name_entry *conflict_name)
{
	int error = 0;

	if (conflict_name->ancestor == NULL)
		error = git_buf_put(name_buf, "\0", 1);
	else
		error = git_buf_put(name_buf, conflict_name->ancestor, strlen(conflict_name->ancestor) + 1);

	if (error != 0)
		goto on_error;

	if (conflict_name->ours == NULL)
		error = git_buf_put(name_buf, "\0", 1);
	else
		error = git_buf_put(name_buf, conflict_name->ours, strlen(conflict_name->ours) + 1);

	if (error != 0)
		goto on_error;

	if (conflict_name->theirs == NULL)
		error = git_buf_put(name_buf, "\0", 1);
	else
		error = git_buf_put(name_buf, conflict_name->theirs, strlen(conflict_name->theirs) + 1);

on_error:
	return error;
}

static int write_name_extension(git_index *index, git_filebuf *file)
{
	git_buf name_buf = GIT_BUF_INIT;
	git_vector *out = &index->names;
	git_index_name_entry *conflict_name;
	struct index_extension extension;
	size_t i;
	int error = 0;

	git_vector_foreach(out, i, conflict_name) {
		if ((error = create_name_extension_data(&name_buf, conflict_name)) < 0)
			goto done;
	}

	memset(&extension, 0x0, sizeof(struct index_extension));
	memcpy(&extension.signature, INDEX_EXT_CONFLICT_NAME_SIG, 4);
	extension.extension_size = (uint32_t)name_buf.size;

	error = write_extension(file, &extension, &name_buf);

	git_buf_free(&name_buf);

done:
	return error;
}

static int create_reuc_extension_data(git_buf *reuc_buf, git_index_reuc_entry *reuc)
{
	int i;
	int error = 0;

	if ((error = git_buf_put(reuc_buf, reuc->path, strlen(reuc->path) + 1)) < 0)
		return error;

	for (i = 0; i < 3; i++) {
		if ((error = git_buf_printf(reuc_buf, "%o", reuc->mode[i])) < 0 ||
			(error = git_buf_put(reuc_buf, "\0", 1)) < 0)
			return error;
	}

	for (i = 0; i < 3; i++) {
		if (reuc->mode[i] && (error = git_buf_put(reuc_buf, (char *)&reuc->oid[i].id, GIT_OID_RAWSZ)) < 0)
			return error;
	}

	return 0;
}

static int write_reuc_extension(git_index *index, git_filebuf *file)
{
	git_buf reuc_buf = GIT_BUF_INIT;
	git_vector *out = &index->reuc;
	git_index_reuc_entry *reuc;
	struct index_extension extension;
	size_t i;
	int error = 0;

	git_vector_foreach(out, i, reuc) {
		if ((error = create_reuc_extension_data(&reuc_buf, reuc)) < 0)
			goto done;
	}

	memset(&extension, 0x0, sizeof(struct index_extension));
	memcpy(&extension.signature, INDEX_EXT_UNMERGED_SIG, 4);
	extension.extension_size = (uint32_t)reuc_buf.size;

	error = write_extension(file, &extension, &reuc_buf);

	git_buf_free(&reuc_buf);

done:
	return error;
}

static int write_tree_extension(git_index *index, git_filebuf *file)
{
	struct index_extension extension;
	git_buf buf = GIT_BUF_INIT;
	int error;

	if (index->tree == NULL)
		return 0;

	if ((error = git_tree_cache_write(&buf, index->tree)) < 0)
		return error;

	memset(&extension, 0x0, sizeof(struct index_extension));
	memcpy(&extension.signature, INDEX_EXT_TREECACHE_SIG, 4);
	extension.extension_size = (uint32_t)buf.size;

	error = write_extension(file, &extension, &buf);

	git_buf_free(&buf);

	return error;
}

static void clear_uptodate(git_index *index)
{
	git_index_entry *entry;
	size_t i;

	git_vector_foreach(&index->entries, i, entry)
		entry->flags_extended &= ~GIT_IDXENTRY_UPTODATE;
}

static int write_index(git_oid *checksum, git_index *index, git_filebuf *file)
{
	git_oid hash_final;
	struct index_header header;
	bool is_extended;
	uint32_t index_version_number;

	assert(index && file);

	if (index->version <= INDEX_VERSION_NUMBER_EXT)  {
		is_extended = is_index_extended(index);
		index_version_number = is_extended ? INDEX_VERSION_NUMBER_EXT : INDEX_VERSION_NUMBER_LB;
	} else {
		index_version_number = index->version;
	}

	header.signature = htonl(INDEX_HEADER_SIG);
	header.version = htonl(index_version_number);
	header.entry_count = htonl((uint32_t)index->entries.length);

	if (git_filebuf_write(file, &header, sizeof(struct index_header)) < 0)
		return -1;

	if (write_entries(index, file) < 0)
		return -1;

	/* write the tree cache extension */
	if (index->tree != NULL && write_tree_extension(index, file) < 0)
		return -1;

	/* write the rename conflict extension */
	if (index->names.length > 0 && write_name_extension(index, file) < 0)
		return -1;

	/* write the reuc extension */
	if (index->reuc.length > 0 && write_reuc_extension(index, file) < 0)
		return -1;

	/* get out the hash for all the contents we've appended to the file */
	git_filebuf_hash(&hash_final, file);
	git_oid_cpy(checksum, &hash_final);

	/* write it at the end of the file */
	if (git_filebuf_write(file, hash_final.id, GIT_OID_RAWSZ) < 0)
		return -1;

	/* file entries are no longer up to date */
	clear_uptodate(index);

	return 0;
}

int git_index_entry_stage(const git_index_entry *entry)
{
	return GIT_IDXENTRY_STAGE(entry);
}

int git_index_entry_is_conflict(const git_index_entry *entry)
{
	return (GIT_IDXENTRY_STAGE(entry) > 0);
}

typedef struct read_tree_data {
	git_index *index;
	git_vector *old_entries;
	git_vector *new_entries;
	git_vector_cmp entry_cmp;
	git_tree_cache *tree;
} read_tree_data;

static int read_tree_cb(
	const char *root, const git_tree_entry *tentry, void *payload)
{
	read_tree_data *data = payload;
	git_index_entry *entry = NULL, *old_entry;
	git_buf path = GIT_BUF_INIT;
	size_t pos;

	if (git_tree_entry__is_tree(tentry))
		return 0;

	if (git_buf_joinpath(&path, root, tentry->filename) < 0)
		return -1;

	if (index_entry_create(&entry, INDEX_OWNER(data->index), path.ptr, false) < 0)
		return -1;

	entry->mode = tentry->attr;
	git_oid_cpy(&entry->id, git_tree_entry_id(tentry));

	/* look for corresponding old entry and copy data to new entry */
	if (data->old_entries != NULL &&
		!index_find_in_entries(
			&pos, data->old_entries, data->entry_cmp, path.ptr, 0, 0) &&
		(old_entry = git_vector_get(data->old_entries, pos)) != NULL &&
		entry->mode == old_entry->mode &&
		git_oid_equal(&entry->id, &old_entry->id))
	{
		index_entry_cpy(entry, old_entry);
		entry->flags_extended = 0;
	}

	index_entry_adjust_namemask(entry, path.size);
	git_buf_free(&path);

	if (git_vector_insert(data->new_entries, entry) < 0) {
		index_entry_free(entry);
		return -1;
	}

	return 0;
}

int git_index_read_tree(git_index *index, const git_tree *tree)
{
	int error = 0;
	git_vector entries = GIT_VECTOR_INIT;
	git_idxmap *entries_map;
	read_tree_data data;
	size_t i;
	git_index_entry *e;

	if (git_idxmap_alloc(&entries_map) < 0)
		return -1;

	git_vector_set_cmp(&entries, index->entries._cmp); /* match sort */

	data.index = index;
	data.old_entries = &index->entries;
	data.new_entries = &entries;
	data.entry_cmp   = index->entries_search;

	index->tree = NULL;
	git_pool_clear(&index->tree_pool);

	git_vector_sort(&index->entries);

	if ((error = git_tree_walk(tree, GIT_TREEWALK_POST, read_tree_cb, &data)) < 0)
		goto cleanup;

	if (index->ignore_case)
		kh_resize(idxicase, (khash_t(idxicase) *) entries_map, entries.length);
	else
		kh_resize(idx, entries_map, entries.length);

	git_vector_foreach(&entries, i, e) {
		INSERT_IN_MAP_EX(index, entries_map, e, error);

		if (error < 0) {
			giterr_set(GITERR_INDEX, "failed to insert entry into map");
			return error;
		}
	}

	error = 0;

	git_vector_sort(&entries);

	if ((error = git_index_clear(index)) < 0) {
		/* well, this isn't good */;
	} else {
		git_vector_swap(&entries, &index->entries);
		entries_map = git__swap(index->entries_map, entries_map);
	}

cleanup:
	git_vector_free(&entries);
	git_idxmap_free(entries_map);
	if (error < 0)
		return error;

	error = git_tree_cache_read_tree(&index->tree, tree, &index->tree_pool);

	return error;
}

static int git_index_read_iterator(
	git_index *index,
	git_iterator *new_iterator,
	size_t new_length_hint)
{
	git_vector new_entries = GIT_VECTOR_INIT,
		remove_entries = GIT_VECTOR_INIT;
	git_idxmap *new_entries_map = NULL;
	git_iterator *index_iterator = NULL;
	git_iterator_options opts = GIT_ITERATOR_OPTIONS_INIT;
	const git_index_entry *old_entry, *new_entry;
	git_index_entry *entry;
	size_t i;
	int error;

	assert((new_iterator->flags & GIT_ITERATOR_DONT_IGNORE_CASE));

	if ((error = git_vector_init(&new_entries, new_length_hint, index->entries._cmp)) < 0 ||
		(error = git_vector_init(&remove_entries, index->entries.length, NULL)) < 0 ||
		(error = git_idxmap_alloc(&new_entries_map)) < 0)
		goto done;

	if (index->ignore_case && new_length_hint)
		kh_resize(idxicase, (khash_t(idxicase) *) new_entries_map, new_length_hint);
	else if (new_length_hint)
		kh_resize(idx, new_entries_map, new_length_hint);

	opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE |
		GIT_ITERATOR_INCLUDE_CONFLICTS;

	if ((error = git_iterator_for_index(&index_iterator,
			git_index_owner(index), index, &opts)) < 0 ||
		((error = git_iterator_current(&old_entry, index_iterator)) < 0 &&
			error != GIT_ITEROVER) ||
		((error = git_iterator_current(&new_entry, new_iterator)) < 0 &&
			error != GIT_ITEROVER))
		goto done;

	while (true) {
		git_index_entry
			*dup_entry = NULL,
			*add_entry = NULL,
			*remove_entry = NULL;
		int diff;

		error = 0;

		if (old_entry && new_entry)
			diff = git_index_entry_cmp(old_entry, new_entry);
		else if (!old_entry && new_entry)
			diff = 1;
		else if (old_entry && !new_entry)
			diff = -1;
		else
			break;

		if (diff < 0) {
			remove_entry = (git_index_entry *)old_entry;
		} else if (diff > 0) {
			dup_entry = (git_index_entry *)new_entry;
		} else {
			/* Path and stage are equal, if the OID is equal, keep it to
			 * keep the stat cache data.
			 */
			if (git_oid_equal(&old_entry->id, &new_entry->id) &&
				old_entry->mode == new_entry->mode) {
				add_entry = (git_index_entry *)old_entry;
			} else {
				dup_entry = (git_index_entry *)new_entry;
				remove_entry = (git_index_entry *)old_entry;
			}
		}

		if (dup_entry) {
			if ((error = index_entry_dup_nocache(&add_entry, index, dup_entry)) < 0)
				goto done;

			index_entry_adjust_namemask(add_entry,
				((struct entry_internal *)add_entry)->pathlen);
		}

		/* invalidate this path in the tree cache if this is new (to
		 * invalidate the parent trees)
		 */
		if (dup_entry && !remove_entry && index->tree)
			git_tree_cache_invalidate_path(index->tree, dup_entry->path);

		if (add_entry) {
			if ((error = git_vector_insert(&new_entries, add_entry)) == 0)
				INSERT_IN_MAP_EX(index, new_entries_map, add_entry, error);
		}

		if (remove_entry && error >= 0)
			error = git_vector_insert(&remove_entries, remove_entry);

		if (error < 0) {
			giterr_set(GITERR_INDEX, "failed to insert entry");
			goto done;
		}

		if (diff <= 0) {
			if ((error = git_iterator_advance(&old_entry, index_iterator)) < 0 &&
				error != GIT_ITEROVER)
				goto done;
		}

		if (diff >= 0) {
			if ((error = git_iterator_advance(&new_entry, new_iterator)) < 0 &&
				error != GIT_ITEROVER)
				goto done;
		}
	}

	git_index_name_clear(index);
	git_index_reuc_clear(index);

	git_vector_swap(&new_entries, &index->entries);
	new_entries_map = git__swap(index->entries_map, new_entries_map);

	git_vector_foreach(&remove_entries, i, entry) {
		if (index->tree)
			git_tree_cache_invalidate_path(index->tree, entry->path);

		index_entry_free(entry);
	}

	clear_uptodate(index);

	error = 0;

done:
	git_idxmap_free(new_entries_map);
	git_vector_free(&new_entries);
	git_vector_free(&remove_entries);
	git_iterator_free(index_iterator);
	return error;
}

int git_index_read_index(
	git_index *index,
	const git_index *new_index)
{
	git_iterator *new_iterator = NULL;
	git_iterator_options opts = GIT_ITERATOR_OPTIONS_INIT;
	int error;

	opts.flags = GIT_ITERATOR_DONT_IGNORE_CASE |
		GIT_ITERATOR_INCLUDE_CONFLICTS;

	if ((error = git_iterator_for_index(&new_iterator,
		git_index_owner(new_index), (git_index *)new_index, &opts)) < 0 ||
		(error = git_index_read_iterator(index, new_iterator,
		new_index->entries.length)) < 0)
		goto done;

done:
	git_iterator_free(new_iterator);
	return error;
}

git_repository *git_index_owner(const git_index *index)
{
	return INDEX_OWNER(index);
}

enum {
	INDEX_ACTION_NONE = 0,
	INDEX_ACTION_UPDATE = 1,
	INDEX_ACTION_REMOVE = 2,
	INDEX_ACTION_ADDALL = 3,
};

int git_index_add_all(
	git_index *index,
	const git_strarray *paths,
	unsigned int flags,
	git_index_matched_path_cb cb,
	void *payload)
{
	int error;
	git_repository *repo;
	git_iterator *wditer = NULL;
	git_pathspec ps;
	bool no_fnmatch = (flags & GIT_INDEX_ADD_DISABLE_PATHSPEC_MATCH) != 0;

	assert(index);

	repo = INDEX_OWNER(index);
	if ((error = git_repository__ensure_not_bare(repo, "index add all")) < 0)
		return error;

	if ((error = git_pathspec__init(&ps, paths)) < 0)
		return error;

	/* optionally check that pathspec doesn't mention any ignored files */
	if ((flags & GIT_INDEX_ADD_CHECK_PATHSPEC) != 0 &&
		(flags & GIT_INDEX_ADD_FORCE) == 0 &&
		(error = git_ignore__check_pathspec_for_exact_ignores(
			repo, &ps.pathspec, no_fnmatch)) < 0)
		goto cleanup;

	error = index_apply_to_wd_diff(index, INDEX_ACTION_ADDALL, paths, flags, cb, payload);

	if (error)
		giterr_set_after_callback(error);

cleanup:
	git_iterator_free(wditer);
	git_pathspec__clear(&ps);

	return error;
}

struct foreach_diff_data {
	git_index *index;
	const git_pathspec *pathspec;
	unsigned int flags;
	git_index_matched_path_cb cb;
	void *payload;
};

static int apply_each_file(const git_diff_delta *delta, float progress, void *payload)
{
	struct foreach_diff_data *data = payload;
	const char *match, *path;
	int error = 0;

	GIT_UNUSED(progress);

	path = delta->old_file.path;

	/* We only want those which match the pathspecs */
	if (!git_pathspec__match(
		    &data->pathspec->pathspec, path, false, (bool)data->index->ignore_case,
		    &match, NULL))
		return 0;

	if (data->cb)
		error = data->cb(path, match, data->payload);

	if (error > 0) /* skip this entry */
		return 0;
	if (error < 0) /* actual error */
		return error;

	/* If the workdir item does not exist, remove it from the index. */
	if ((delta->new_file.flags & GIT_DIFF_FLAG_EXISTS) == 0)
		error = git_index_remove_bypath(data->index, path);
	else
		error = git_index_add_bypath(data->index, delta->new_file.path);

	return error;
}

static int index_apply_to_wd_diff(git_index *index, int action, const git_strarray *paths,
				  unsigned int flags,
				  git_index_matched_path_cb cb, void *payload)
{
	int error;
	git_diff *diff;
	git_pathspec ps;
	git_repository *repo;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	struct foreach_diff_data data = {
		index,
		NULL,
		flags,
		cb,
		payload,
	};

	assert(index);
	assert(action == INDEX_ACTION_UPDATE || action == INDEX_ACTION_ADDALL);

	repo = INDEX_OWNER(index);

	if (!repo) {
		return create_index_error(-1,
			"cannot run update; the index is not backed up by a repository.");
	}

	/*
	 * We do the matching ourselves intead of passing the list to
	 * diff because we want to tell the callback which one
	 * matched, which we do not know if we ask diff to filter for us.
	 */
	if ((error = git_pathspec__init(&ps, paths)) < 0)
		return error;

	opts.flags = GIT_DIFF_INCLUDE_TYPECHANGE;
	if (action == INDEX_ACTION_ADDALL) {
		opts.flags |= GIT_DIFF_INCLUDE_UNTRACKED |
			GIT_DIFF_RECURSE_UNTRACKED_DIRS;

		if (flags == GIT_INDEX_ADD_FORCE)
			opts.flags |= GIT_DIFF_INCLUDE_IGNORED;
	}

	if ((error = git_diff_index_to_workdir(&diff, repo, index, &opts)) < 0)
		goto cleanup;

	data.pathspec = &ps;
	error = git_diff_foreach(diff, apply_each_file, NULL, NULL, NULL, &data);
	git_diff_free(diff);

	if (error) /* make sure error is set if callback stopped iteration */
		giterr_set_after_callback(error);

cleanup:
	git_pathspec__clear(&ps);
	return error;
}

static int index_apply_to_all(
	git_index *index,
	int action,
	const git_strarray *paths,
	git_index_matched_path_cb cb,
	void *payload)
{
	int error = 0;
	size_t i;
	git_pathspec ps;
	const char *match;
	git_buf path = GIT_BUF_INIT;

	assert(index);

	if ((error = git_pathspec__init(&ps, paths)) < 0)
		return error;

	git_vector_sort(&index->entries);

	for (i = 0; !error && i < index->entries.length; ++i) {
		git_index_entry *entry = git_vector_get(&index->entries, i);

		/* check if path actually matches */
		if (!git_pathspec__match(
				&ps.pathspec, entry->path, false, (bool)index->ignore_case,
				&match, NULL))
			continue;

		/* issue notification callback if requested */
		if (cb && (error = cb(entry->path, match, payload)) != 0) {
			if (error > 0) { /* return > 0 means skip this one */
				error = 0;
				continue;
			}
			if (error < 0)   /* return < 0 means abort */
				break;
		}

		/* index manipulation may alter entry, so don't depend on it */
		if ((error = git_buf_sets(&path, entry->path)) < 0)
			break;

		switch (action) {
		case INDEX_ACTION_NONE:
			break;
		case INDEX_ACTION_UPDATE:
			error = git_index_add_bypath(index, path.ptr);

			if (error == GIT_ENOTFOUND) {
				giterr_clear();

				error = git_index_remove_bypath(index, path.ptr);

				if (!error) /* back up foreach if we removed this */
					i--;
			}
			break;
		case INDEX_ACTION_REMOVE:
			if (!(error = git_index_remove_bypath(index, path.ptr)))
				i--; /* back up foreach if we removed this */
			break;
		default:
			giterr_set(GITERR_INVALID, "Unknown index action %d", action);
			error = -1;
			break;
		}
	}

	git_buf_free(&path);
	git_pathspec__clear(&ps);

	return error;
}

int git_index_remove_all(
	git_index *index,
	const git_strarray *pathspec,
	git_index_matched_path_cb cb,
	void *payload)
{
	int error = index_apply_to_all(
		index, INDEX_ACTION_REMOVE, pathspec, cb, payload);

	if (error) /* make sure error is set if callback stopped iteration */
		giterr_set_after_callback(error);

	return error;
}

int git_index_update_all(
	git_index *index,
	const git_strarray *pathspec,
	git_index_matched_path_cb cb,
	void *payload)
{
	int error = index_apply_to_wd_diff(index, INDEX_ACTION_UPDATE, pathspec, 0, cb, payload);
	if (error) /* make sure error is set if callback stopped iteration */
		giterr_set_after_callback(error);

	return error;
}

int git_index_snapshot_new(git_vector *snap, git_index *index)
{
	int error;

	GIT_REFCOUNT_INC(index);

	git_atomic_inc(&index->readers);
	git_vector_sort(&index->entries);

	error = git_vector_dup(snap, &index->entries, index->entries._cmp);

	if (error < 0)
		git_index_free(index);

	return error;
}

void git_index_snapshot_release(git_vector *snap, git_index *index)
{
	git_vector_free(snap);

	git_atomic_dec(&index->readers);

	git_index_free(index);
}

int git_index_snapshot_find(
	size_t *out, git_vector *entries, git_vector_cmp entry_srch,
	const char *path, size_t path_len, int stage)
{
	return index_find_in_entries(out, entries, entry_srch, path, path_len, stage);
}

int git_indexwriter_init(
	git_indexwriter *writer,
	git_index *index)
{
	int error;

	GIT_REFCOUNT_INC(index);

	writer->index = index;

	if (!index->index_file_path)
		return create_index_error(-1,
			"Failed to write index: The index is in-memory only");

	if ((error = git_filebuf_open(
		&writer->file, index->index_file_path, GIT_FILEBUF_HASH_CONTENTS, GIT_INDEX_FILE_MODE)) < 0) {

		if (error == GIT_ELOCKED)
			giterr_set(GITERR_INDEX, "The index is locked. This might be due to a concurrent or crashed process");

		return error;
	}

	writer->should_write = 1;

	return 0;
}

int git_indexwriter_init_for_operation(
	git_indexwriter *writer,
	git_repository *repo,
	unsigned int *checkout_strategy)
{
	git_index *index;
	int error;

	if ((error = git_repository_index__weakptr(&index, repo)) < 0 ||
		(error = git_indexwriter_init(writer, index)) < 0)
		return error;

	writer->should_write = (*checkout_strategy & GIT_CHECKOUT_DONT_WRITE_INDEX) == 0;
	*checkout_strategy |= GIT_CHECKOUT_DONT_WRITE_INDEX;

	return 0;
}

int git_indexwriter_commit(git_indexwriter *writer)
{
	int error;
	git_oid checksum = {{ 0 }};

	if (!writer->should_write)
		return 0;

	git_vector_sort(&writer->index->entries);
	git_vector_sort(&writer->index->reuc);

	if ((error = write_index(&checksum, writer->index, &writer->file)) < 0) {
		git_indexwriter_cleanup(writer);
		return error;
	}

	if ((error = git_filebuf_commit(&writer->file)) < 0)
		return error;

	if ((error = git_futils_filestamp_check(
		&writer->index->stamp, writer->index->index_file_path)) < 0) {
		giterr_set(GITERR_OS, "Could not read index timestamp");
		return -1;
	}

	writer->index->on_disk = 1;
	git_oid_cpy(&writer->index->checksum, &checksum);

	git_index_free(writer->index);
	writer->index = NULL;

	return 0;
}

void git_indexwriter_cleanup(git_indexwriter *writer)
{
	git_filebuf_cleanup(&writer->file);

	git_index_free(writer->index);
	writer->index = NULL;
}
