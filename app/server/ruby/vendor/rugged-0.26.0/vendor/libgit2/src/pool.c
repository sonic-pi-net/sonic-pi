/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "pool.h"

#include "posix.h"
#ifndef GIT_WIN32
#include <unistd.h>
#endif

struct git_pool_page {
	git_pool_page *next;
	uint32_t size;
	uint32_t avail;
	GIT_ALIGN(char data[GIT_FLEX_ARRAY], 8);
};

static void *pool_alloc_page(git_pool *pool, uint32_t size);

uint32_t git_pool__system_page_size(void)
{
	static uint32_t size = 0;

	if (!size) {
		size_t page_size;
		if (git__page_size(&page_size) < 0)
			page_size = 4096;
		/* allow space for malloc overhead */
		size = page_size - (2 * sizeof(void *)) - sizeof(git_pool_page);
	}

	return size;
}

#ifndef GIT_DEBUG_POOL
void git_pool_init(git_pool *pool, uint32_t item_size)
{
	assert(pool);
	assert(item_size >= 1);

	memset(pool, 0, sizeof(git_pool));
	pool->item_size = item_size;
	pool->page_size = git_pool__system_page_size();
}

void git_pool_clear(git_pool *pool)
{
	git_pool_page *scan, *next;

	for (scan = pool->pages; scan != NULL; scan = next) {
		next = scan->next;
		git__free(scan);
	}

	pool->pages = NULL;
}

static void *pool_alloc_page(git_pool *pool, uint32_t size)
{
	git_pool_page *page;
	const uint32_t new_page_size = (size <= pool->page_size) ? pool->page_size : size;
	size_t alloc_size;

	if (GIT_ADD_SIZET_OVERFLOW(&alloc_size, new_page_size, sizeof(git_pool_page)) ||
		!(page = git__malloc(alloc_size)))
		return NULL;

	page->size = new_page_size;
	page->avail = new_page_size - size;
	page->next = pool->pages;

	pool->pages = page;

	return page->data;
}

static void *pool_alloc(git_pool *pool, uint32_t size)
{
	git_pool_page *page = pool->pages;
	void *ptr = NULL;

	if (!page || page->avail < size)
		return pool_alloc_page(pool, size);

	ptr = &page->data[page->size - page->avail];
	page->avail -= size;

	return ptr;
}

uint32_t git_pool__open_pages(git_pool *pool)
{
	uint32_t ct = 0;
	git_pool_page *scan;
	for (scan = pool->pages; scan != NULL; scan = scan->next) ct++;
	return ct;
}

bool git_pool__ptr_in_pool(git_pool *pool, void *ptr)
{
	git_pool_page *scan;
	for (scan = pool->pages; scan != NULL; scan = scan->next)
		if ((void *)scan->data <= ptr &&
			(void *)(((char *)scan->data) + scan->size) > ptr)
			return true;
	return false;
}

#else

static int git_pool__ptr_cmp(const void * a, const void * b)
{
	if(a > b) {
		return 1;
	}
	if(a < b) {
		return -1;
	}
	else {
		return 0;
	}
}

void git_pool_init(git_pool *pool, uint32_t item_size)
{
	assert(pool);
	assert(item_size >= 1);

	memset(pool, 0, sizeof(git_pool));
	pool->item_size = item_size;
	pool->page_size = git_pool__system_page_size();
	git_vector_init(&pool->allocations, 100, git_pool__ptr_cmp);
}

void git_pool_clear(git_pool *pool)
{
	git_vector_free_deep(&pool->allocations);
}

static void *pool_alloc(git_pool *pool, uint32_t size) {
	void *ptr = NULL;
	if((ptr = git__malloc(size)) == NULL) {
		return NULL;
	}
	git_vector_insert_sorted(&pool->allocations, ptr, NULL);
	return ptr;
}

bool git_pool__ptr_in_pool(git_pool *pool, void *ptr)
{
	size_t pos;
	return git_vector_bsearch(&pos, &pool->allocations, ptr) != GIT_ENOTFOUND;
}
#endif

void git_pool_swap(git_pool *a, git_pool *b)
{
	git_pool temp;

	if (a == b)
		return;

	memcpy(&temp, a, sizeof(temp));
	memcpy(a, b, sizeof(temp));
	memcpy(b, &temp, sizeof(temp));
}

static uint32_t alloc_size(git_pool *pool, uint32_t count)
{
	const uint32_t align = sizeof(void *) - 1;

	if (pool->item_size > 1) {
		const uint32_t item_size = (pool->item_size + align) & ~align;
		return item_size * count;
	}

	return (count + align) & ~align;
}

void *git_pool_malloc(git_pool *pool, uint32_t items)
{
	return pool_alloc(pool, alloc_size(pool, items));
}

void *git_pool_mallocz(git_pool *pool, uint32_t items)
{
	const uint32_t size = alloc_size(pool, items);
	void *ptr = pool_alloc(pool, size);
	if (ptr)
		memset(ptr, 0x0, size);
	return ptr;
}

char *git_pool_strndup(git_pool *pool, const char *str, size_t n)
{
	char *ptr = NULL;

	assert(pool && str && pool->item_size == sizeof(char));

	if ((uint32_t)(n + 1) < n)
		return NULL;

	if ((ptr = git_pool_malloc(pool, (uint32_t)(n + 1))) != NULL) {
		memcpy(ptr, str, n);
		ptr[n] = '\0';
	}

	return ptr;
}

char *git_pool_strdup(git_pool *pool, const char *str)
{
	assert(pool && str && pool->item_size == sizeof(char));
	return git_pool_strndup(pool, str, strlen(str));
}

char *git_pool_strdup_safe(git_pool *pool, const char *str)
{
	return str ? git_pool_strdup(pool, str) : NULL;
}

char *git_pool_strcat(git_pool *pool, const char *a, const char *b)
{
	void *ptr;
	size_t len_a, len_b;

	assert(pool && pool->item_size == sizeof(char));

	len_a = a ? strlen(a) : 0;
	len_b = b ? strlen(b) : 0;

	if ((ptr = git_pool_malloc(pool, (uint32_t)(len_a + len_b + 1))) != NULL) {
		if (len_a)
			memcpy(ptr, a, len_a);
		if (len_b)
			memcpy(((char *)ptr) + len_a, b, len_b);
		*(((char *)ptr) + len_a + len_b) = '\0';
	}
	return ptr;
}
