#include "pool.h"
#ifndef GIT_WIN32
#include <unistd.h>
#endif

struct git_pool_page {
	git_pool_page *next;
	uint32_t size;
	uint32_t avail;
	GIT_ALIGN(char data[GIT_FLEX_ARRAY], 8);
};

struct pool_freelist {
	struct pool_freelist *next;
};

#define GIT_POOL_MIN_USABLE	4
#define GIT_POOL_MIN_PAGESZ	2 * sizeof(void*)

static void *pool_alloc_page(git_pool *pool, uint32_t size);
static void pool_insert_page(git_pool *pool, git_pool_page *page);

int git_pool_init(
	git_pool *pool, uint32_t item_size, uint32_t items_per_page)
{
	assert(pool);

	if (!item_size)
		item_size = 1;
	/* round up item_size for decent object alignment */
	if (item_size > 4)
		item_size = (item_size + 7) & ~7;
	else if (item_size == 3)
		item_size = 4;

	if (!items_per_page)
		items_per_page = git_pool__suggest_items_per_page(item_size);
	if (item_size * items_per_page < GIT_POOL_MIN_PAGESZ)
		items_per_page = (GIT_POOL_MIN_PAGESZ + item_size - 1) / item_size;

	memset(pool, 0, sizeof(git_pool));
	pool->item_size = item_size;
	pool->page_size = item_size * items_per_page;

	return 0;
}

void git_pool_clear(git_pool *pool)
{
	git_pool_page *scan, *next;

	for (scan = pool->open; scan != NULL; scan = next) {
		next = scan->next;
		git__free(scan);
	}
	pool->open = NULL;

	for (scan = pool->full; scan != NULL; scan = next) {
		next = scan->next;
		git__free(scan);
	}
	pool->full = NULL;

	pool->free_list = NULL;

	pool->items = 0;

	pool->has_string_alloc     = 0;
	pool->has_multi_item_alloc = 0;
	pool->has_large_page_alloc = 0;
}

void git_pool_swap(git_pool *a, git_pool *b)
{
	git_pool temp;

	if (a == b)
		return;

	memcpy(&temp, a, sizeof(temp));
	memcpy(a, b, sizeof(temp));
	memcpy(b, &temp, sizeof(temp));
}

static void pool_insert_page(git_pool *pool, git_pool_page *page)
{
	git_pool_page *scan;

	/* If there are no open pages or this page has the most open space,
	 * insert it at the beginning of the list.  This is the common case.
	 */
	if (pool->open == NULL || pool->open->avail < page->avail) {
		page->next = pool->open;
		pool->open = page;
		return;
	}

	/* Otherwise insert into sorted position. */
	for (scan = pool->open;
		 scan->next && scan->next->avail > page->avail;
		 scan = scan->next);
	page->next = scan->next;
	scan->next = page;
}

static void *pool_alloc_page(git_pool *pool, uint32_t size)
{
	git_pool_page *page;
	uint32_t alloc_size;

	if (size <= pool->page_size)
		alloc_size = pool->page_size;
	else {
		alloc_size = size;
		pool->has_large_page_alloc = 1;
	}

	page = git__calloc(1, alloc_size + sizeof(git_pool_page));
	if (!page)
		return NULL;

	page->size  = alloc_size;
	page->avail = alloc_size - size;

	if (page->avail > 0)
		pool_insert_page(pool, page);
	else {
		page->next = pool->full;
		pool->full = page;
	}

	pool->items++;

	return page->data;
}

GIT_INLINE(void) pool_remove_page(
	git_pool *pool, git_pool_page *page, git_pool_page *prev)
{
	if (prev == NULL)
		pool->open = page->next;
	else
		prev->next = page->next;
}

void *git_pool_malloc(git_pool *pool, uint32_t items)
{
	git_pool_page *scan = pool->open, *prev;
	uint32_t size = ((items * pool->item_size) + 7) & ~7;
	void *ptr = NULL;

	pool->has_string_alloc = 0;
	if (items > 1)
		pool->has_multi_item_alloc = 1;
	else if (pool->free_list != NULL) {
		ptr = pool->free_list;
		pool->free_list = ((struct pool_freelist *)pool->free_list)->next;
		return ptr;
	}

	/* just add a block if there is no open one to accomodate this */
	if (size >= pool->page_size || !scan || scan->avail < size)
		return pool_alloc_page(pool, size);

	pool->items++;

	/* find smallest block in free list with space */
	for (scan = pool->open, prev = NULL;
		 scan->next && scan->next->avail >= size;
		 prev = scan, scan = scan->next);

	/* allocate space from the block */
	ptr = &scan->data[scan->size - scan->avail];
	scan->avail -= size;

	/* move to full list if there is almost no space left */
	if (scan->avail < pool->item_size || scan->avail < GIT_POOL_MIN_USABLE) {
		pool_remove_page(pool, scan, prev);
		scan->next = pool->full;
		pool->full = scan;
	}
	/* reorder list if block is now smaller than the one after it */
	else if (scan->next != NULL && scan->next->avail > scan->avail) {
		pool_remove_page(pool, scan, prev);
		pool_insert_page(pool, scan);
	}

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

	pool->has_string_alloc = 1;

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

	assert(pool && a && b && pool->item_size == sizeof(char));

	len_a = a ? strlen(a) : 0;
	len_b = b ? strlen(b) : 0;

	if ((ptr = git_pool_malloc(pool, (uint32_t)(len_a + len_b + 1))) != NULL) {
		if (len_a)
			memcpy(ptr, a, len_a);
		if (len_b)
			memcpy(((char *)ptr) + len_a, b, len_b);
		*(((char *)ptr) + len_a + len_b) = '\0';
	}
	pool->has_string_alloc = 1;

	return ptr;
}

void git_pool_free(git_pool *pool, void *ptr)
{
	struct pool_freelist *item = ptr;

	assert(pool && pool->item_size >= sizeof(void*));

	if (item) {
		item->next = pool->free_list;
		pool->free_list = item;
	}
}

void git_pool_free_array(git_pool *pool, size_t count, void **ptrs)
{
	struct pool_freelist **items = (struct pool_freelist **)ptrs;
	size_t i;

	assert(pool && ptrs && pool->item_size >= sizeof(void*));

	if (!count)
		return;

	for (i = count - 1; i > 0; --i)
		items[i]->next = items[i - 1];

	items[i]->next  = pool->free_list;
	pool->free_list = items[count - 1];
}

uint32_t git_pool__open_pages(git_pool *pool)
{
	uint32_t ct = 0;
	git_pool_page *scan;
	for (scan = pool->open; scan != NULL; scan = scan->next) ct++;
	return ct;
}

uint32_t git_pool__full_pages(git_pool *pool)
{
	uint32_t ct = 0;
	git_pool_page *scan;
	for (scan = pool->full; scan != NULL; scan = scan->next) ct++;
	return ct;
}

bool git_pool__ptr_in_pool(git_pool *pool, void *ptr)
{
	git_pool_page *scan;
	for (scan = pool->open; scan != NULL; scan = scan->next)
		if ((void *)scan->data <= ptr &&
			(void *)(((char *)scan->data) + scan->size) > ptr)
			return true;
	for (scan = pool->full; scan != NULL; scan = scan->next)
		if ((void *)scan->data <= ptr &&
			(void *)(((char *)scan->data) + scan->size) > ptr)
			return true;
	return false;
}

uint32_t git_pool__system_page_size(void)
{
	static uint32_t size = 0;

	if (!size) {
#ifdef GIT_WIN32
		SYSTEM_INFO info;
		GetSystemInfo(&info);
		size = (uint32_t)info.dwPageSize;
#elif defined(__amigaos4__)
		size = (uint32_t)4096; /* 4K as there is no global value we can query */
#else
		size = (uint32_t)sysconf(_SC_PAGE_SIZE);
#endif

		size -= 2 * sizeof(void *); /* allow space for malloc overhead */
	}

	return size;
}

uint32_t git_pool__suggest_items_per_page(uint32_t item_size)
{
	uint32_t page_bytes =
		git_pool__system_page_size() - sizeof(git_pool_page);
	return page_bytes / item_size;
}

