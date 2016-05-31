/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"
#include "vector.h"

/* In elements, not bytes */
#define MIN_ALLOCSIZE	8

GIT_INLINE(size_t) compute_new_size(git_vector *v)
{
	size_t new_size = v->_alloc_size;

	/* Use a resize factor of 1.5, which is quick to compute using integer
	 * instructions and less than the golden ratio (1.618...) */
	if (new_size < MIN_ALLOCSIZE)
		new_size = MIN_ALLOCSIZE;
	else if (new_size <= (SIZE_MAX / 3) * 2)
		new_size += new_size / 2;
	else
		new_size = SIZE_MAX;

	return new_size;
}

GIT_INLINE(int) resize_vector(git_vector *v, size_t new_size)
{
	void *new_contents;

	new_contents = git__reallocarray(v->contents, new_size, sizeof(void *));
	GITERR_CHECK_ALLOC(new_contents);

	v->_alloc_size = new_size;
	v->contents = new_contents;

	return 0;
}

int git_vector_size_hint(git_vector *v, size_t size_hint)
{
	if (v->_alloc_size >= size_hint)
		return 0;
	return resize_vector(v, size_hint);
}

int git_vector_dup(git_vector *v, const git_vector *src, git_vector_cmp cmp)
{
	size_t bytes;

	assert(v && src);

	GITERR_CHECK_ALLOC_MULTIPLY(&bytes, src->length, sizeof(void *));

	v->_alloc_size = src->length;
	v->_cmp = cmp ? cmp : src->_cmp;
	v->length = src->length;
	v->flags  = src->flags;
	if (cmp != src->_cmp)
		git_vector_set_sorted(v, 0);
	v->contents = git__malloc(bytes);
	GITERR_CHECK_ALLOC(v->contents);

	memcpy(v->contents, src->contents, bytes);

	return 0;
}

void git_vector_free(git_vector *v)
{
	assert(v);

	git__free(v->contents);
	v->contents = NULL;

	v->length = 0;
	v->_alloc_size = 0;
}

void git_vector_free_deep(git_vector *v)
{
	size_t i;

	assert(v);

	for (i = 0; i < v->length; ++i) {
		git__free(v->contents[i]);
		v->contents[i] = NULL;
	}

	git_vector_free(v);
}

int git_vector_init(git_vector *v, size_t initial_size, git_vector_cmp cmp)
{
	assert(v);

	v->_alloc_size = 0;
	v->_cmp = cmp;
	v->length = 0;
	v->flags = GIT_VECTOR_SORTED;
	v->contents = NULL;

	return resize_vector(v, max(initial_size, MIN_ALLOCSIZE));
}

void **git_vector_detach(size_t *size, size_t *asize, git_vector *v)
{
	void **data = v->contents;

	if (size)
		*size = v->length;
	if (asize)
		*asize = v->_alloc_size;

	v->_alloc_size = 0;
	v->length   = 0;
	v->contents = NULL;

	return data;
}

int git_vector_insert(git_vector *v, void *element)
{
	assert(v);

	if (v->length >= v->_alloc_size &&
		resize_vector(v, compute_new_size(v)) < 0)
		return -1;

	v->contents[v->length++] = element;

	git_vector_set_sorted(v, v->length <= 1);

	return 0;
}

int git_vector_insert_sorted(
	git_vector *v, void *element, int (*on_dup)(void **old, void *new))
{
	int result;
	size_t pos;

	assert(v && v->_cmp);

	if (!git_vector_is_sorted(v))
		git_vector_sort(v);

	if (v->length >= v->_alloc_size &&
		resize_vector(v, compute_new_size(v)) < 0)
		return -1;

	/* If we find the element and have a duplicate handler callback,
	 * invoke it.  If it returns non-zero, then cancel insert, otherwise
	 * proceed with normal insert.
	 */
	if (!git__bsearch(v->contents, v->length, element, v->_cmp, &pos) &&
		on_dup && (result = on_dup(&v->contents[pos], element)) < 0)
		return result;

	/* shift elements to the right */
	if (pos < v->length)
		memmove(v->contents + pos + 1, v->contents + pos,
		        (v->length - pos) * sizeof(void *));

	v->contents[pos] = element;
	v->length++;

	return 0;
}

void git_vector_sort(git_vector *v)
{
	assert(v);

	if (git_vector_is_sorted(v) || !v->_cmp)
		return;

	if (v->length > 1)
		git__tsort(v->contents, v->length, v->_cmp);
	git_vector_set_sorted(v, 1);
}

int git_vector_bsearch2(
	size_t *at_pos,
	git_vector *v,
	git_vector_cmp key_lookup,
	const void *key)
{
	assert(v && key && key_lookup);

	/* need comparison function to sort the vector */
	if (!v->_cmp)
		return -1;

	git_vector_sort(v);

	return git__bsearch(v->contents, v->length, key, key_lookup, at_pos);
}

int git_vector_search2(
	size_t *at_pos, const git_vector *v, git_vector_cmp key_lookup, const void *key)
{
	size_t i;

	assert(v && key && key_lookup);

	for (i = 0; i < v->length; ++i) {
		if (key_lookup(key, v->contents[i]) == 0) {
			if (at_pos)
				*at_pos = i;

			return 0;
		}
	}

	return GIT_ENOTFOUND;
}

static int strict_comparison(const void *a, const void *b)
{
	return (a == b) ? 0 : -1;
}

int git_vector_search(size_t *at_pos, const git_vector *v, const void *entry)
{
	return git_vector_search2(at_pos, v, v->_cmp ? v->_cmp : strict_comparison, entry);
}

int git_vector_remove(git_vector *v, size_t idx)
{
	size_t shift_count;

	assert(v);

	if (idx >= v->length)
		return GIT_ENOTFOUND;

	shift_count = v->length - idx - 1;

	if (shift_count)
		memmove(&v->contents[idx], &v->contents[idx + 1],
			shift_count * sizeof(void *));

	v->length--;
	return 0;
}

void git_vector_pop(git_vector *v)
{
	if (v->length > 0)
		v->length--;
}

void git_vector_uniq(git_vector *v, void  (*git_free_cb)(void *))
{
	git_vector_cmp cmp;
	size_t i, j;

	if (v->length <= 1)
		return;

	git_vector_sort(v);
	cmp = v->_cmp ? v->_cmp : strict_comparison;

	for (i = 0, j = 1 ; j < v->length; ++j)
		if (!cmp(v->contents[i], v->contents[j])) {
			if (git_free_cb)
				git_free_cb(v->contents[i]);

			v->contents[i] = v->contents[j];
		} else
			v->contents[++i] = v->contents[j];

	v->length -= j - i - 1;
}

void git_vector_remove_matching(
	git_vector *v,
	int (*match)(const git_vector *v, size_t idx, void *payload),
	void *payload)
{
	size_t i, j;

	for (i = 0, j = 0; j < v->length; ++j) {
		v->contents[i] = v->contents[j];

		if (!match(v, i, payload))
			i++;
	}

	v->length = i;
}

void git_vector_clear(git_vector *v)
{
	assert(v);
	v->length = 0;
	git_vector_set_sorted(v, 1);
}

void git_vector_swap(git_vector *a, git_vector *b)
{
	git_vector t;

	assert(a && b);

	if (a != b) {
		memcpy(&t, a, sizeof(t));
		memcpy(a, b, sizeof(t));
		memcpy(b, &t, sizeof(t));
	}
}

int git_vector_resize_to(git_vector *v, size_t new_length)
{
	if (new_length > v->_alloc_size &&
		resize_vector(v, new_length) < 0)
		return -1;

	if (new_length > v->length)
		memset(&v->contents[v->length], 0,
			sizeof(void *) * (new_length - v->length));

	v->length = new_length;

	return 0;
}

int git_vector_set(void **old, git_vector *v, size_t position, void *value)
{
	if (position + 1 > v->length) {
		if (git_vector_resize_to(v, position + 1) < 0)
			return -1;
	}

	if (old != NULL)
		*old = v->contents[position];

	v->contents[position] = value;

	return 0;
}

int git_vector_verify_sorted(const git_vector *v)
{
	size_t i;

	if (!git_vector_is_sorted(v))
		return -1;

	for (i = 1; i < v->length; ++i) {
		if (v->_cmp(v->contents[i - 1], v->contents[i]) > 0)
			return -1;
	}

	return 0;
}
