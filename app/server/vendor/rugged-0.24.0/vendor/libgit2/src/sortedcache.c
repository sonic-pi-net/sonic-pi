#include "sortedcache.h"

GIT__USE_STRMAP

int git_sortedcache_new(
	git_sortedcache **out,
	size_t item_path_offset,
	git_sortedcache_free_item_fn free_item,
	void *free_item_payload,
	git_vector_cmp item_cmp,
	const char *path)
{
	git_sortedcache *sc;
	size_t pathlen, alloclen;

	pathlen = path ? strlen(path) : 0;

	GITERR_CHECK_ALLOC_ADD(&alloclen, sizeof(git_sortedcache), pathlen);
	GITERR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);
	sc = git__calloc(1, alloclen);
	GITERR_CHECK_ALLOC(sc);

	git_pool_init(&sc->pool, 1);

	if (git_vector_init(&sc->items, 4, item_cmp) < 0 ||
		git_strmap_alloc(&sc->map) < 0)
		goto fail;

	if (git_rwlock_init(&sc->lock)) {
		giterr_set(GITERR_OS, "Failed to initialize lock");
		goto fail;
	}

	sc->item_path_offset  = item_path_offset;
	sc->free_item         = free_item;
	sc->free_item_payload = free_item_payload;
	GIT_REFCOUNT_INC(sc);
	if (pathlen)
		memcpy(sc->path, path, pathlen);

	*out = sc;
	return 0;

fail:
	git_strmap_free(sc->map);
	git_vector_free(&sc->items);
	git_pool_clear(&sc->pool);
	git__free(sc);
	return -1;
}

void git_sortedcache_incref(git_sortedcache *sc)
{
	GIT_REFCOUNT_INC(sc);
}

const char *git_sortedcache_path(git_sortedcache *sc)
{
	return sc->path;
}

static void sortedcache_clear(git_sortedcache *sc)
{
	git_strmap_clear(sc->map);

	if (sc->free_item) {
		size_t i;
		void *item;

		git_vector_foreach(&sc->items, i, item) {
			sc->free_item(sc->free_item_payload, item);
		}
	}

	git_vector_clear(&sc->items);

	git_pool_clear(&sc->pool);
}

static void sortedcache_free(git_sortedcache *sc)
{
	/* acquire write lock to make sure everyone else is done */
	if (git_sortedcache_wlock(sc) < 0)
		return;

	sortedcache_clear(sc);
	git_vector_free(&sc->items);
	git_strmap_free(sc->map);

	git_sortedcache_wunlock(sc);

	git_rwlock_free(&sc->lock);
	git__free(sc);
}

void git_sortedcache_free(git_sortedcache *sc)
{
	if (!sc)
		return;
	GIT_REFCOUNT_DEC(sc, sortedcache_free);
}

static int sortedcache_copy_item(void *payload, void *tgt_item, void *src_item)
{
	git_sortedcache *sc = payload;
	/* path will already have been copied by upsert */
	memcpy(tgt_item, src_item, sc->item_path_offset);
	return 0;
}

/* copy a sorted cache */
int git_sortedcache_copy(
	git_sortedcache **out,
	git_sortedcache *src,
	bool lock,
	int (*copy_item)(void *payload, void *tgt_item, void *src_item),
	void *payload)
{
	int error = 0;
	git_sortedcache *tgt;
	size_t i;
	void *src_item, *tgt_item;

	/* just use memcpy if no special copy fn is passed in */
	if (!copy_item) {
		copy_item = sortedcache_copy_item;
		payload   = src;
	}

	if ((error = git_sortedcache_new(
			&tgt, src->item_path_offset,
			src->free_item, src->free_item_payload,
			src->items._cmp, src->path)) < 0)
		return error;

	if (lock && git_sortedcache_rlock(src) < 0) {
		git_sortedcache_free(tgt);
		return -1;
	}

	git_vector_foreach(&src->items, i, src_item) {
		char *path = ((char *)src_item) + src->item_path_offset;

		if ((error = git_sortedcache_upsert(&tgt_item, tgt, path)) < 0 ||
			(error = copy_item(payload, tgt_item, src_item)) < 0)
			break;
	}

	if (lock)
		git_sortedcache_runlock(src);
	if (error)
		git_sortedcache_free(tgt);

	*out = !error ? tgt : NULL;

	return error;
}

/* lock sortedcache while making modifications */
int git_sortedcache_wlock(git_sortedcache *sc)
{
	GIT_UNUSED(sc); /* prevent warning when compiled w/o threads */

	if (git_rwlock_wrlock(&sc->lock) < 0) {
		giterr_set(GITERR_OS, "Unable to acquire write lock on cache");
		return -1;
	}
	return 0;
}

/* unlock sorted cache when done with modifications */
void git_sortedcache_wunlock(git_sortedcache *sc)
{
	git_vector_sort(&sc->items);
	git_rwlock_wrunlock(&sc->lock);
}

/* lock sortedcache for read */
int git_sortedcache_rlock(git_sortedcache *sc)
{
	GIT_UNUSED(sc); /* prevent warning when compiled w/o threads */

	if (git_rwlock_rdlock(&sc->lock) < 0) {
		giterr_set(GITERR_OS, "Unable to acquire read lock on cache");
		return -1;
	}
	return 0;
}

/* unlock sorted cache when done reading */
void git_sortedcache_runlock(git_sortedcache *sc)
{
	GIT_UNUSED(sc); /* prevent warning when compiled w/o threads */
	git_rwlock_rdunlock(&sc->lock);
}

/* if the file has changed, lock cache and load file contents into buf;
 * returns <0 on error, >0 if file has not changed
 */
int git_sortedcache_lockandload(git_sortedcache *sc, git_buf *buf)
{
	int error, fd;

	if ((error = git_sortedcache_wlock(sc)) < 0)
		return error;

	if ((error = git_futils_filestamp_check(&sc->stamp, sc->path)) <= 0)
		goto unlock;

	if (!git__is_sizet(sc->stamp.size)) {
		giterr_set(GITERR_INVALID, "Unable to load file larger than size_t");
		error = -1;
		goto unlock;
	}

	if ((fd = git_futils_open_ro(sc->path)) < 0) {
		error = fd;
		goto unlock;
	}

	if (buf)
		error = git_futils_readbuffer_fd(buf, fd, (size_t)sc->stamp.size);

	(void)p_close(fd);

	if (error < 0)
		goto unlock;

	return 1; /* return 1 -> file needs reload and was successfully loaded */

unlock:
	git_sortedcache_wunlock(sc);
	return error;
}

void git_sortedcache_updated(git_sortedcache *sc)
{
	/* update filestamp to latest value */
	git_futils_filestamp_check(&sc->stamp, sc->path);
}

/* release all items in sorted cache */
int git_sortedcache_clear(git_sortedcache *sc, bool wlock)
{
	if (wlock && git_sortedcache_wlock(sc) < 0)
		return -1;

	sortedcache_clear(sc);

	if (wlock)
		git_sortedcache_wunlock(sc);

	return 0;
}

/* find and/or insert item, returning pointer to item data */
int git_sortedcache_upsert(void **out, git_sortedcache *sc, const char *key)
{
	int error = 0;
	khiter_t pos;
	void *item;
	size_t keylen, itemlen;
	char *item_key;

	pos = git_strmap_lookup_index(sc->map, key);
	if (git_strmap_valid_index(sc->map, pos)) {
		item = git_strmap_value_at(sc->map, pos);
		goto done;
	}

	keylen  = strlen(key);
	itemlen = sc->item_path_offset + keylen + 1;
	itemlen = (itemlen + 7) & ~7;

	if ((item = git_pool_mallocz(&sc->pool, (uint32_t)itemlen)) == NULL) {
		/* don't use GITERR_CHECK_ALLOC b/c of lock */
		error = -1;
		goto done;
	}

	/* one strange thing is that even if the vector or hash table insert
	 * fail, there is no way to free the pool item so we just abandon it
	 */

	item_key = ((char *)item) + sc->item_path_offset;
	memcpy(item_key, key, keylen);

	pos = kh_put(str, sc->map, item_key, &error);
	if (error < 0)
		goto done;

	if (!error)
		kh_key(sc->map, pos) = item_key;
	kh_val(sc->map, pos) = item;

	error = git_vector_insert(&sc->items, item);
	if (error < 0)
		git_strmap_delete_at(sc->map, pos);

done:
	if (out)
		*out = !error ? item : NULL;
	return error;
}

/* lookup item by key */
void *git_sortedcache_lookup(const git_sortedcache *sc, const char *key)
{
	khiter_t pos = git_strmap_lookup_index(sc->map, key);
	if (git_strmap_valid_index(sc->map, pos))
		return git_strmap_value_at(sc->map, pos);
	return NULL;
}

/* find out how many items are in the cache */
size_t git_sortedcache_entrycount(const git_sortedcache *sc)
{
	return git_vector_length(&sc->items);
}

/* lookup item by index */
void *git_sortedcache_entry(git_sortedcache *sc, size_t pos)
{
	/* make sure the items are sorted so this gets the correct item */
	if (!git_vector_is_sorted(&sc->items))
		git_vector_sort(&sc->items);

	return git_vector_get(&sc->items, pos);
}

/* helper struct so bsearch callback can know offset + key value for cmp */
struct sortedcache_magic_key {
	size_t offset;
	const char *key;
};

static int sortedcache_magic_cmp(const void *key, const void *value)
{
	const struct sortedcache_magic_key *magic = key;
	const char *value_key = ((const char *)value) + magic->offset;
	return strcmp(magic->key, value_key);
}

/* lookup index of item by key */
int git_sortedcache_lookup_index(
	size_t *out, git_sortedcache *sc, const char *key)
{
	struct sortedcache_magic_key magic;

	magic.offset = sc->item_path_offset;
	magic.key    = key;

	return git_vector_bsearch2(out, &sc->items, sortedcache_magic_cmp, &magic);
}

/* remove entry from cache */
int git_sortedcache_remove(git_sortedcache *sc, size_t pos)
{
	char *item;
	khiter_t mappos;

	/* because of pool allocation, this can't actually remove the item,
	 * but we can remove it from the items vector and the hash table.
	 */

	if ((item = git_vector_get(&sc->items, pos)) == NULL) {
		giterr_set(GITERR_INVALID, "Removing item out of range");
		return GIT_ENOTFOUND;
	}

	(void)git_vector_remove(&sc->items, pos);

	mappos = git_strmap_lookup_index(sc->map, item + sc->item_path_offset);
	git_strmap_delete_at(sc->map, mappos);

	if (sc->free_item)
		sc->free_item(sc->free_item_payload, item);

	return 0;
}

