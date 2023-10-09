/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "cache.h"

#include "repository.h"
#include "commit.h"
#include "thread.h"
#include "util.h"
#include "odb.h"
#include "object.h"
#include "git2/oid.h"

bool git_cache__enabled = true;
ssize_t git_cache__max_storage = (256 * 1024 * 1024);
git_atomic_ssize git_cache__current_storage = {0};

static size_t git_cache__max_object_size[8] = {
	0,     /* GIT_OBJECT__EXT1 */
	4096,  /* GIT_OBJECT_COMMIT */
	4096,  /* GIT_OBJECT_TREE */
	0,     /* GIT_OBJECT_BLOB */
	4096,  /* GIT_OBJECT_TAG */
	0,     /* GIT_OBJECT__EXT2 */
	0,     /* GIT_OBJECT_OFS_DELTA */
	0      /* GIT_OBJECT_REF_DELTA */
};

int git_cache_set_max_object_size(git_object_t type, size_t size)
{
	if (type < 0 || (size_t)type >= ARRAY_SIZE(git_cache__max_object_size)) {
		git_error_set(GIT_ERROR_INVALID, "type out of range");
		return -1;
	}

	git_cache__max_object_size[type] = size;
	return 0;
}

int git_cache_init(git_cache *cache)
{
	memset(cache, 0, sizeof(*cache));

	if ((git_oidmap_new(&cache->map)) < 0)
		return -1;

	if (git_rwlock_init(&cache->lock)) {
		git_error_set(GIT_ERROR_OS, "failed to initialize cache rwlock");
		return -1;
	}

	return 0;
}

/* called with lock */
static void clear_cache(git_cache *cache)
{
	git_cached_obj *evict = NULL;

	if (git_cache_size(cache) == 0)
		return;

	git_oidmap_foreach_value(cache->map, evict, {
		git_cached_obj_decref(evict);
	});

	git_oidmap_clear(cache->map);
	git_atomic_ssize_add(&git_cache__current_storage, -cache->used_memory);
	cache->used_memory = 0;
}

void git_cache_clear(git_cache *cache)
{
	if (git_rwlock_wrlock(&cache->lock) < 0)
		return;

	clear_cache(cache);

	git_rwlock_wrunlock(&cache->lock);
}

void git_cache_dispose(git_cache *cache)
{
	git_cache_clear(cache);
	git_oidmap_free(cache->map);
	git_rwlock_free(&cache->lock);
	git__memzero(cache, sizeof(*cache));
}

/* Called with lock */
static void cache_evict_entries(git_cache *cache)
{
	size_t evict_count = git_cache_size(cache) / 2048, i;
	ssize_t evicted_memory = 0;

	if (evict_count < 8)
		evict_count = 8;

	/* do not infinite loop if there's not enough entries to evict  */
	if (evict_count > git_cache_size(cache)) {
		clear_cache(cache);
		return;
	}

	i = 0;
	while (evict_count > 0) {
		git_cached_obj *evict;
		const git_oid *key;

		if (git_oidmap_iterate((void **) &evict, cache->map, &i, &key) == GIT_ITEROVER)
			break;

		evict_count--;
		evicted_memory += evict->size;
		git_oidmap_delete(cache->map, key);
		git_cached_obj_decref(evict);
	}

	cache->used_memory -= evicted_memory;
	git_atomic_ssize_add(&git_cache__current_storage, -evicted_memory);
}

static bool cache_should_store(git_object_t object_type, size_t object_size)
{
	size_t max_size = git_cache__max_object_size[object_type];
	return git_cache__enabled && object_size < max_size;
}

static void *cache_get(git_cache *cache, const git_oid *oid, unsigned int flags)
{
	git_cached_obj *entry;

	if (!git_cache__enabled || git_rwlock_rdlock(&cache->lock) < 0)
		return NULL;

	if ((entry = git_oidmap_get(cache->map, oid)) != NULL) {
		if (flags && entry->flags != flags) {
			entry = NULL;
		} else {
			git_cached_obj_incref(entry);
		}
	}

	git_rwlock_rdunlock(&cache->lock);

	return entry;
}

static void *cache_store(git_cache *cache, git_cached_obj *entry)
{
	git_cached_obj *stored_entry;

	git_cached_obj_incref(entry);

	if (!git_cache__enabled && cache->used_memory > 0) {
		git_cache_clear(cache);
		return entry;
	}

	if (!cache_should_store(entry->type, entry->size))
		return entry;

	if (git_rwlock_wrlock(&cache->lock) < 0)
		return entry;

	/* soften the load on the cache */
	if (git_atomic_ssize_get(&git_cache__current_storage) > git_cache__max_storage)
		cache_evict_entries(cache);

	/* not found */
	if ((stored_entry = git_oidmap_get(cache->map, &entry->oid)) == NULL) {
		if (git_oidmap_set(cache->map, &entry->oid, entry) == 0) {
			git_cached_obj_incref(entry);
			cache->used_memory += entry->size;
			git_atomic_ssize_add(&git_cache__current_storage, (ssize_t)entry->size);
		}
	}
	/* found */
	else {
		if (stored_entry->flags == entry->flags) {
			git_cached_obj_decref(entry);
			git_cached_obj_incref(stored_entry);
			entry = stored_entry;
		} else if (stored_entry->flags == GIT_CACHE_STORE_RAW &&
			   entry->flags == GIT_CACHE_STORE_PARSED) {
			if (git_oidmap_set(cache->map, &entry->oid, entry) == 0) {
				git_cached_obj_decref(stored_entry);
				git_cached_obj_incref(entry);
			} else {
				git_cached_obj_decref(entry);
				git_cached_obj_incref(stored_entry);
				entry = stored_entry;
			}
		} else {
			/* NO OP */
		}
	}

	git_rwlock_wrunlock(&cache->lock);
	return entry;
}

void *git_cache_store_raw(git_cache *cache, git_odb_object *entry)
{
	entry->cached.flags = GIT_CACHE_STORE_RAW;
	return cache_store(cache, (git_cached_obj *)entry);
}

void *git_cache_store_parsed(git_cache *cache, git_object *entry)
{
	entry->cached.flags = GIT_CACHE_STORE_PARSED;
	return cache_store(cache, (git_cached_obj *)entry);
}

git_odb_object *git_cache_get_raw(git_cache *cache, const git_oid *oid)
{
	return cache_get(cache, oid, GIT_CACHE_STORE_RAW);
}

git_object *git_cache_get_parsed(git_cache *cache, const git_oid *oid)
{
	return cache_get(cache, oid, GIT_CACHE_STORE_PARSED);
}

void *git_cache_get_any(git_cache *cache, const git_oid *oid)
{
	return cache_get(cache, oid, GIT_CACHE_STORE_ANY);
}

void git_cached_obj_decref(void *_obj)
{
	git_cached_obj *obj = _obj;

	if (git_atomic32_dec(&obj->refcount) == 0) {
		switch (obj->flags) {
		case GIT_CACHE_STORE_RAW:
			git_odb_object__free(_obj);
			break;

		case GIT_CACHE_STORE_PARSED:
			git_object__free(_obj);
			break;

		default:
			git__free(_obj);
			break;
		}
	}
}
