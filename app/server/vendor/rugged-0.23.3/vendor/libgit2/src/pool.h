/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_pool_h__
#define INCLUDE_pool_h__

#include "common.h"

typedef struct git_pool_page git_pool_page;

/**
 * Chunked allocator.
 *
 * A `git_pool` can be used when you want to cheaply allocate
 * multiple items of the same type and are willing to free them
 * all together with a single call.  The two most common cases
 * are a set of fixed size items (such as lots of OIDs) or a
 * bunch of strings.
 *
 * Internally, a `git_pool` allocates pages of memory and then
 * deals out blocks from the trailing unused portion of each page.
 * The pages guarantee that the number of actual allocations done
 * will be much smaller than the number of items needed.
 *
 * For examples of how to set up a `git_pool` see `git_pool_init`.
 */
typedef struct {
	git_pool_page *open; /* pages with space left */
	git_pool_page *full; /* pages with no space left */
	void *free_list;     /* optional: list of freed blocks */
	uint32_t item_size;  /* size of single alloc unit in bytes */
	uint32_t page_size;  /* size of page in bytes */
	uint32_t items;
	unsigned has_string_alloc : 1; /* was the strdup function used */
	unsigned has_multi_item_alloc : 1; /* was items ever > 1 in malloc */
	unsigned has_large_page_alloc : 1; /* are any pages > page_size */
} git_pool;

#define GIT_POOL_INIT_STRINGPOOL { 0, 0, 0, 1, 4000, 0, 0, 0, 0 }

/**
 * Initialize a pool.
 *
 * To allocation strings, use like this:
 *
 *     git_pool_init(&string_pool, 1, 0);
 *     my_string = git_pool_strdup(&string_pool, your_string);
 *
 * To allocate items of fixed size, use like this:
 *
 *     git_pool_init(&pool, sizeof(item), 0);
 *     my_item = git_pool_malloc(&pool, 1);
 *
 * Of course, you can use this in other ways, but those are the
 * two most common patterns.
 */
extern int git_pool_init(
	git_pool *pool, uint32_t item_size, uint32_t items_per_page);

/**
 * Free all items in pool
 */
extern void git_pool_clear(git_pool *pool);

/**
 * Swap two pools with one another
 */
extern void git_pool_swap(git_pool *a, git_pool *b);

/**
 * Allocate space for one or more items from a pool.
 */
extern void *git_pool_malloc(git_pool *pool, uint32_t items);

/**
 * Allocate space and zero it out.
 */
GIT_INLINE(void *) git_pool_mallocz(git_pool *pool, uint32_t items)
{
	void *ptr = git_pool_malloc(pool, items);
	if (ptr)
		memset(ptr, 0, (size_t)items * (size_t)pool->item_size);
	return ptr;
}

/**
 * Allocate space and duplicate string data into it.
 *
 * This is allowed only for pools with item_size == sizeof(char)
 */
extern char *git_pool_strndup(git_pool *pool, const char *str, size_t n);

/**
 * Allocate space and duplicate a string into it.
 *
 * This is allowed only for pools with item_size == sizeof(char)
 */
extern char *git_pool_strdup(git_pool *pool, const char *str);

/**
 * Allocate space and duplicate a string into it, NULL is no error.
 *
 * This is allowed only for pools with item_size == sizeof(char)
 */
extern char *git_pool_strdup_safe(git_pool *pool, const char *str);

/**
 * Allocate space for the concatenation of two strings.
 *
 * This is allowed only for pools with item_size == sizeof(char)
 */
extern char *git_pool_strcat(git_pool *pool, const char *a, const char *b);

/**
 * Push a block back onto the free list for the pool.
 *
 * This is allowed only if the item_size is >= sizeof(void*).
 *
 * In some cases, it is helpful to "release" an allocated block
 * for reuse.  Pools don't support a general purpose free, but
 * they will keep a simple free blocks linked list provided the
 * native block size is large enough to hold a void pointer
 */
extern void git_pool_free(git_pool *pool, void *ptr);

/**
 * Push an array of pool allocated blocks efficiently onto the free list.
 *
 * This has the same constraints as `git_pool_free()` above.
 */
extern void git_pool_free_array(git_pool *pool, size_t count, void **ptrs);

/*
 * Misc utilities
 */

extern uint32_t git_pool__open_pages(git_pool *pool);

extern uint32_t git_pool__full_pages(git_pool *pool);

extern bool git_pool__ptr_in_pool(git_pool *pool, void *ptr);

extern uint32_t git_pool__suggest_items_per_page(uint32_t item_size);

#endif
