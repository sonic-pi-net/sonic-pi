/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_strmap_h__
#define INCLUDE_strmap_h__

#include "git2_util.h"

/** A map with C strings as key. */
typedef struct kh_str_s git_strmap;

/**
 * Allocate a new string map.
 *
 * @param out Pointer to the map that shall be allocated.
 * @return 0 on success, an error code if allocation has failed.
 */
int git_strmap_new(git_strmap **out);

/**
 * Free memory associated with the map.
 *
 * Note that this function will _not_ free keys or values added
 * to this map.
 *
 * @param map Pointer to the map that is to be free'd. May be
 *            `NULL`.
 */
void git_strmap_free(git_strmap *map);

/**
 * Clear all entries from the map.
 *
 * This function will remove all entries from the associated map.
 * Memory associated with it will not be released, though.
 *
 * @param map Pointer to the map that shall be cleared. May be
 *            `NULL`.
 */
void git_strmap_clear(git_strmap *map);

/**
 * Return the number of elements in the map.
 *
 * @parameter map map containing the elements
 * @return number of elements in the map
 */
size_t git_strmap_size(git_strmap *map);

/**
 * Return value associated with the given key.
 *
 * @param map map to search key in
 * @param key key to search for
 * @return value associated with the given key or NULL if the key was not found
 */
void *git_strmap_get(git_strmap *map, const char *key);

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
int git_strmap_set(git_strmap *map, const char *key, void *value);

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
int git_strmap_delete(git_strmap *map, const char *key);

/**
 * Check whether a key exists in the given map.
 *
 * @param map map to query for the key
 * @param key key to search for
 * @return 0 if the key has not been found, 1 otherwise
 */
int git_strmap_exists(git_strmap *map, const char *key);

/**
 * Iterate over entries of the map.
 *
 * This functions allows to iterate over all key-value entries of
 * the map. The current position is stored in the `iter` variable
 * and should be initialized to `0` before the first call to this
 * function.
 *
 * @param map map to iterate over
 * @param value pointer to the variable where to store the current
 *            value. May be NULL.
 * @param iter iterator storing the current position. Initialize
 *             with zero previous to the first call.
 * @param key pointer to the variable where to store the current
 *            key. May be NULL.
 * @return `0` if the next entry was correctly retrieved.
 *        GIT_ITEROVER if no entries are left. A negative error
 *        code otherwise.
 */
int git_strmap_iterate(void **value, git_strmap *map, size_t *iter, const char **key);

#define git_strmap_foreach(h, kvar, vvar, code) { size_t __i = 0;		\
	while (git_strmap_iterate((void **) &(vvar), h, &__i, &(kvar)) == 0) {	\
		code;								\
	} }

#define git_strmap_foreach_value(h, vvar, code) { size_t __i = 0;		\
	while (git_strmap_iterate((void **) &(vvar), h, &__i, NULL) == 0) {	\
		code;								\
	} }

#endif
