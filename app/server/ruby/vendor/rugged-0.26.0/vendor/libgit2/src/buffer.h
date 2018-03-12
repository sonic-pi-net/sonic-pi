/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_buffer_h__
#define INCLUDE_buffer_h__

#include "common.h"
#include "git2/strarray.h"
#include "git2/buffer.h"

/* typedef struct {
 *  	char   *ptr;
 *  	size_t asize, size;
 * } git_buf;
 */

extern char git_buf__initbuf[];
extern char git_buf__oom[];

/* Use to initialize buffer structure when git_buf is on stack */
#define GIT_BUF_INIT { git_buf__initbuf, 0, 0 }

GIT_INLINE(bool) git_buf_is_allocated(const git_buf *buf)
{
	return (buf->ptr != NULL && buf->asize > 0);
}

/**
 * Initialize a git_buf structure.
 *
 * For the cases where GIT_BUF_INIT cannot be used to do static
 * initialization.
 */
extern int git_buf_init(git_buf *buf, size_t initial_size);

/**
 * Resize the buffer allocation to make more space.
 *
 * This will attempt to grow the buffer to accommodate the additional size.
 * It is similar to `git_buf_grow`, but performs the new size calculation,
 * checking for overflow.
 *
 * Like `git_buf_grow`, if this is a user-supplied buffer, this will allocate
 * a new buffer.
 */
extern int git_buf_grow_by(git_buf *buffer, size_t additional_size);

/**
 * Attempt to grow the buffer to hold at least `target_size` bytes.
 *
 * If the allocation fails, this will return an error.  If `mark_oom` is true,
 * this will mark the buffer as invalid for future operations; if false,
 * existing buffer content will be preserved, but calling code must handle
 * that buffer was not expanded.  If `preserve_external` is true, then any
 * existing data pointed to be `ptr` even if `asize` is zero will be copied
 * into the newly allocated buffer.
 */
extern int git_buf_try_grow(
	git_buf *buf, size_t target_size, bool mark_oom);

/**
 * Sanitizes git_buf structures provided from user input.  Users of the
 * library, when providing git_buf's, may wish to provide a NULL ptr for
 * ease of handling.  The buffer routines, however, expect a non-NULL ptr
 * always.  This helper method simply handles NULL input, converting to a
 * git_buf__initbuf. If a buffer with a non-NULL ptr is passed in, this method
 * assures that the buffer is '\0'-terminated.
 */
extern void git_buf_sanitize(git_buf *buf);

extern void git_buf_swap(git_buf *buf_a, git_buf *buf_b);
extern char *git_buf_detach(git_buf *buf);
extern int git_buf_attach(git_buf *buf, char *ptr, size_t asize);

/* Populates a `git_buf` where the contents are not "owned" by the
 * buffer, and calls to `git_buf_free` will not free the given buf.
 */
extern void git_buf_attach_notowned(
	git_buf *buf, const char *ptr, size_t size);

/**
 * Test if there have been any reallocation failures with this git_buf.
 *
 * Any function that writes to a git_buf can fail due to memory allocation
 * issues.  If one fails, the git_buf will be marked with an OOM error and
 * further calls to modify the buffer will fail.  Check git_buf_oom() at the
 * end of your sequence and it will be true if you ran out of memory at any
 * point with that buffer.
 *
 * @return false if no error, true if allocation error
 */
GIT_INLINE(bool) git_buf_oom(const git_buf *buf)
{
	return (buf->ptr == git_buf__oom);
}

/*
 * Functions below that return int value error codes will return 0 on
 * success or -1 on failure (which generally means an allocation failed).
 * Using a git_buf where the allocation has failed with result in -1 from
 * all further calls using that buffer.  As a result, you can ignore the
 * return code of these functions and call them in a series then just call
 * git_buf_oom at the end.
 */
int git_buf_sets(git_buf *buf, const char *string);
int git_buf_putc(git_buf *buf, char c);
int git_buf_putcn(git_buf *buf, char c, size_t len);
int git_buf_put(git_buf *buf, const char *data, size_t len);
int git_buf_puts(git_buf *buf, const char *string);
int git_buf_printf(git_buf *buf, const char *format, ...) GIT_FORMAT_PRINTF(2, 3);
int git_buf_vprintf(git_buf *buf, const char *format, va_list ap);
void git_buf_clear(git_buf *buf);
void git_buf_consume(git_buf *buf, const char *end);
void git_buf_truncate(git_buf *buf, size_t len);
void git_buf_shorten(git_buf *buf, size_t amount);
void git_buf_rtruncate_at_char(git_buf *path, char separator);

/** General join with separator */
int git_buf_join_n(git_buf *buf, char separator, int nbuf, ...);
/** Fast join of two strings - first may legally point into `buf` data */
int git_buf_join(git_buf *buf, char separator, const char *str_a, const char *str_b);
/** Fast join of three strings - cannot reference `buf` data */
int git_buf_join3(git_buf *buf, char separator, const char *str_a, const char *str_b, const char *str_c);

/**
 * Join two strings as paths, inserting a slash between as needed.
 * @return 0 on success, -1 on failure
 */
GIT_INLINE(int) git_buf_joinpath(git_buf *buf, const char *a, const char *b)
{
	return git_buf_join(buf, '/', a, b);
}

GIT_INLINE(const char *) git_buf_cstr(const git_buf *buf)
{
	return buf->ptr;
}

GIT_INLINE(size_t) git_buf_len(const git_buf *buf)
{
	return buf->size;
}

void git_buf_copy_cstr(char *data, size_t datasize, const git_buf *buf);

#define git_buf_PUTS(buf, str) git_buf_put(buf, str, sizeof(str) - 1)

GIT_INLINE(ssize_t) git_buf_rfind_next(const git_buf *buf, char ch)
{
	ssize_t idx = (ssize_t)buf->size - 1;
	while (idx >= 0 && buf->ptr[idx] == ch) idx--;
	while (idx >= 0 && buf->ptr[idx] != ch) idx--;
	return idx;
}

GIT_INLINE(ssize_t) git_buf_rfind(const git_buf *buf, char ch)
{
	ssize_t idx = (ssize_t)buf->size - 1;
	while (idx >= 0 && buf->ptr[idx] != ch) idx--;
	return idx;
}

GIT_INLINE(ssize_t) git_buf_find(const git_buf *buf, char ch)
{
	void *found = memchr(buf->ptr, ch, buf->size);
	return found ? (ssize_t)((const char *)found - buf->ptr) : -1;
}

/* Remove whitespace from the end of the buffer */
void git_buf_rtrim(git_buf *buf);

int git_buf_cmp(const git_buf *a, const git_buf *b);

/* Quote and unquote a buffer as specified in
 * http://marc.info/?l=git&m=112927316408690&w=2
 */
int git_buf_quote(git_buf *buf);
int git_buf_unquote(git_buf *buf);

/* Write data as base64 encoded in buffer */
int git_buf_encode_base64(git_buf *buf, const char *data, size_t len);
/* Decode the given bas64 and write the result to the buffer */
int git_buf_decode_base64(git_buf *buf, const char *base64, size_t len);

/* Write data as "base85" encoded in buffer */
int git_buf_encode_base85(git_buf *buf, const char *data, size_t len);
/* Decode the given "base85" and write the result to the buffer */
int git_buf_decode_base85(git_buf *buf, const char *base64, size_t len, size_t output_len);

/*
 * Insert, remove or replace a portion of the buffer.
 *
 * @param buf The buffer to work with
 *
 * @param where The location in the buffer where the transformation
 * should be applied.
 *
 * @param nb_to_remove The number of chars to be removed. 0 to not
 * remove any character in the buffer.
 *
 * @param data A pointer to the data which should be inserted.
 *
 * @param nb_to_insert The number of chars to be inserted. 0 to not
 * insert any character from the buffer.
 *
 * @return 0 or an error code.
 */
int git_buf_splice(
	git_buf *buf,
	size_t where,
	size_t nb_to_remove,
	const char *data,
	size_t nb_to_insert);

#endif
