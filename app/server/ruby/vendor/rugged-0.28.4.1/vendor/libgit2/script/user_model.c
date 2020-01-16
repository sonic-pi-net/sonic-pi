/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

void *realloc(void *ptr, size_t size);
void *memmove(void *dest, const void *src, size_t n);
size_t strlen(const char *s);

typedef struct va_list_str *va_list;

typedef struct git_vector {
	void **contents;
	size_t length;
} git_vector;

typedef struct git_buf {
	char *ptr;
	size_t asize, size;
} git_buf;

int git_vector_insert(git_vector *v, void *element)
{
	if (!v)
		__coverity_panic__();

	v->contents = realloc(v->contents, ++v->length);
	if (!v->contents)
		__coverity_panic__();
	v->contents[v->length] = element;

	return 0;
}

int git_buf_len(const struct git_buf *buf)
{
	return strlen(buf->ptr);
}

int git_buf_vprintf(git_buf *buf, const char *format, va_list ap)
{
    char ch, *s;
    size_t len;

    __coverity_string_null_sink__(format);
    __coverity_string_size_sink__(format);

    ch = *format;
    ch = *(char *)ap;

    buf->ptr = __coverity_alloc__(len);
    __coverity_writeall__(buf->ptr);
    buf->size = len;

    return 0;
}

int git_buf_put(git_buf *buf, const char *data, size_t len)
{
    buf->ptr = __coverity_alloc__(buf->size + len + 1);
    memmove(buf->ptr + buf->size, data, len);
    buf->size += len;
    buf->ptr[buf->size + len] = 0;
    return 0;
}

int git_buf_set(git_buf *buf, const void *data, size_t len)
{
    buf->ptr = __coverity_alloc__(len + 1);
    memmove(buf->ptr, data, len);
    buf->size = len + 1;
    return 0;
}

void clar__fail(
	const char *file,
	int line,
	const char *error,
	const char *description,
	int should_abort)
{
	if (should_abort)
		__coverity_panic__();
}

void clar__assert(
	int condition,
	const char *file,
	int line,
	const char *error,
	const char *description,
	int should_abort)
{
	if (!condition && should_abort)
		__coverity_panic__();
}
