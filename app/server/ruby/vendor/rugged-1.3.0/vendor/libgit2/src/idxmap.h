/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_idxmap_h__
#define INCLUDE_idxmap_h__

#include "common.h"

#include "git2/index.h"

/** A map with `git_index_entry`s as key. */
typedef struct kh_idx_s git_idxmap;
/** A map with case-insensitive `git_index_entry`s as key */
typedef struct kh_idxicase_s git_idxmap_icase;

/**
 * Allocate a new index entry map.
 *
 * @param out Pointer to the map that shall be allocated.
 * @return 0 on success, an error code if allocation has failed.
 */
int git_idxmap_new(git_idxmap **out);

/**
 * Allocate a new case-insensitive index entry map.
 *
 * @param out Pointer to the map that shall be allocated.
 * @return 0 on success, an error code if allocation has failed.
 */
int git_idxmap_icase_new(git_idxmap_icase **out);

/**
 * Free memory associated with the map.
 *
 * Note that this function will _not_ free values added to this
 * map.
 *
 * @param map Pointer to the map that is to be free'd. May be
 *            `NULL`.
 */
void git_idxmap_free(git_idxmap *map);

/**
 * Free memory associated with the map.
 *
 * Note that this function will _not_ free values added to this
 * map.
 *
 * @param map Pointer to the map that is to be free'd. May be
 *            `NULL`.
 */
void git_idxmap_icase_free(git_idxmap_icase *map);

/**
 * Clear all entries from the map.
 *
 * This function will remove all entries from the associated map.
 * Memory associated with it will not be released, though.
 *
 * @param map Pointer to the map that shall be cleared. May be
 *            `NULL`.
 */
void git_idxmap_clear(git_idxmap *map);

/**
 * Clear all entries from the map.
 *
 * This function will remove all entries from the associated map.
 * Memory associated with it will not be released, though.
 *
 * @param map Pointer to the map that shall be cleared. May be
 *            `NULL`.
 */
void git_idxmap_icase_clear(git_idxmap_icase *map);

/**
 * Resize the map by allocating more memory.
 *
 * @param map map that shall be resized
 * @param size count of entries that the map shall hold
 * @return `0` if the map was successfully resized, a negative
 *         error code otherwise
 */
int git_idxmap_resize(git_idxmap *map, size_t size);

/**
 * Resize the map by allocating more memory.
 *
 * @param map map that shall be resized
 * @param size count of entries that the map shall hold
 * @return `0` if the map was successfully resized, a negative
 *         error code otherwise
 */
int git_idxmap_icase_resize(git_idxmap_icase *map, size_t size);

/**
 * Return value associated with the given key.
 *
 * @param map map to search key in
 * @param key key to search for; the index entry will be searched
 *            for by its case-sensitive path
 * @return value associated with the given key or NULL if the key was not found
 */
void *git_idxmap_get(git_idxmap *map, const git_index_entry *key);

/**
 * Return value associated with the given key.
 *
 * @param map map to search key in
 * @param key key to search for; the index entry will be searched
 *            for by its case-insensitive path
 * @return value associated with the given key or NULL if the key was not found
 */
void *git_idxmap_icase_get(git_idxmap_icase *map, const git_index_entry *key);

/**
 * Set the entry for key to value.
 *
 * If the map has no corresponding entry for the given key, a new
 * entry will be created with the given value. If an entry exists
 * already, its value will be updated to match the given value.
 *
 * @param map map to create new entry in
 * @param key key to set
 * @param value value to associate the key with; may be NULL
 * @return zero if the key was successfully set, a negative error
 *         code otherwise
 */
int git_idxmap_set(git_idxmap *map, const git_index_entry *key, void *value);

/**
 * Set the entry for key to value.
 *
 * If the map has no corresponding entry for the given key, a new
 * entry will be created with the given value. If an entry exists
 * already, its value will be updated to match the given value.
 *
 * @param map map to create new entry in
 * @param key key to set
 * @param value value to associate the key with; may be NULL
 * @return zero if the key was successfully set, a negative error
 *         code otherwise
 */
int git_idxmap_icase_set(git_idxmap_icase *map, const git_index_entry *key, void *value);

/**
 * Delete an entry from the map.
 *
 * Delete the given key and its value from the map. If no such
 * key exists, this will do nothing.
 *
 * @param map map to delete key in
 * @param key key to delete
 * @return `0` if the key has been deleted, GIT_ENOTFOUND if no
 *         such key was found, a negative code in case of an
 *         error
 */
int git_idxmap_delete(git_idxmap *map, const git_index_entry *key);

/**
 * Delete an entry from the map.
 *
 * Delete the given key and its value from the map. If no such
 * key exists, this will do nothing.
 *
 * @param map map to delete key in
 * @param key key to delete
 * @return `0` if the key has been deleted, GIT_ENOTFOUND if no
 *         such key was found, a negative code in case of an
 *         error
 */
int git_idxmap_icase_delete(git_idxmap_icase *map, const git_index_entry *key);

#endif
