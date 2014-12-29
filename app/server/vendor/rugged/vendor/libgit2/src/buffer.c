/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "buffer.h"
#include "posix.h"
#include "git2/buffer.h"
#include <ctype.h>

/* Used as default value for git_buf->ptr so that people can always
 * assume ptr is non-NULL and zero terminated even for new git_bufs.
 */
char git_buf__initbuf[1];

char git_buf__oom[1];

#define ENSURE_SIZE(b, d) \
	if ((d) > buf->asize && git_buf_grow(b, (d)) < 0)\
		return -1;


void git_buf_init(git_buf *buf, size_t initial_size)
{
	buf->asize = 0;
	buf->size = 0;
	buf->ptr = git_buf__initbuf;

	if (initial_size)
		git_buf_grow(buf, initial_size);
}

int git_buf_try_grow(
	git_buf *buf, size_t target_size, bool mark_oom, bool preserve_external)
{
	char *new_ptr;
	size_t new_size;

	if (buf->ptr == git_buf__oom)
		return -1;

	if (!target_size)
		target_size = buf->size;

	if (target_size <= buf->asize)
		return 0;

	if (buf->asize == 0) {
		new_size = target_size;
		new_ptr = NULL;
	} else {
		new_size = buf->asize;
		new_ptr = buf->ptr;
	}

	/* grow the buffer size by 1.5, until it's big enough
	 * to fit our target size */
	while (new_size < target_size)
		new_size = (new_size << 1) - (new_size >> 1);

	/* round allocation up to multiple of 8 */
	new_size = (new_size + 7) & ~7;

	new_ptr = git__realloc(new_ptr, new_size);

	if (!new_ptr) {
		if (mark_oom) {
			if (buf->ptr) git__free(buf->ptr);
			buf->ptr = git_buf__oom;
		}
		return -1;
	}

	if (preserve_external && !buf->asize && buf->ptr != NULL && buf->size > 0)
		memcpy(new_ptr, buf->ptr, min(buf->size, new_size));

	buf->asize = new_size;
	buf->ptr   = new_ptr;

	/* truncate the existing buffer size if necessary */
	if (buf->size >= buf->asize)
		buf->size = buf->asize - 1;
	buf->ptr[buf->size] = '\0';

	return 0;
}

int git_buf_grow(git_buf *buffer, size_t target_size)
{
	return git_buf_try_grow(buffer, target_size, true, true);
}

void git_buf_free(git_buf *buf)
{
	if (!buf) return;

	if (buf->asize > 0 && buf->ptr != NULL && buf->ptr != git_buf__oom)
		git__free(buf->ptr);

	git_buf_init(buf, 0);
}

void git_buf_sanitize(git_buf *buf)
{
	if (buf->ptr == NULL) {
		assert(buf->size == 0 && buf->asize == 0);
		buf->ptr = git_buf__initbuf;
	} else if (buf->asize > buf->size)
		buf->ptr[buf->size] = '\0';
}

void git_buf_clear(git_buf *buf)
{
	buf->size = 0;

	if (!buf->ptr) {
		buf->ptr = git_buf__initbuf;
		buf->asize = 0;
	}

	if (buf->asize > 0)
		buf->ptr[0] = '\0';
}

int git_buf_set(git_buf *buf, const void *data, size_t len)
{
	if (len == 0 || data == NULL) {
		git_buf_clear(buf);
	} else {
		if (data != buf->ptr) {
			ENSURE_SIZE(buf, len + 1);
			memmove(buf->ptr, data, len);
		}

		buf->size = len;
		if (buf->asize > buf->size)
			buf->ptr[buf->size] = '\0';

	}
	return 0;
}

int git_buf_sets(git_buf *buf, const char *string)
{
	return git_buf_set(buf, string, string ? strlen(string) : 0);
}

int git_buf_putc(git_buf *buf, char c)
{
	ENSURE_SIZE(buf, buf->size + 2);
	buf->ptr[buf->size++] = c;
	buf->ptr[buf->size] = '\0';
	return 0;
}

int git_buf_putcn(git_buf *buf, char c, size_t len)
{
	ENSURE_SIZE(buf, buf->size + len + 1);
	memset(buf->ptr + buf->size, c, len);
	buf->size += len;
	buf->ptr[buf->size] = '\0';
	return 0;
}

int git_buf_put(git_buf *buf, const char *data, size_t len)
{
	ENSURE_SIZE(buf, buf->size + len + 1);
	memmove(buf->ptr + buf->size, data, len);
	buf->size += len;
	buf->ptr[buf->size] = '\0';
	return 0;
}

int git_buf_puts(git_buf *buf, const char *string)
{
	assert(string);
	return git_buf_put(buf, string, strlen(string));
}

static const char b64str[] =
	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

int git_buf_put_base64(git_buf *buf, const char *data, size_t len)
{
	size_t extra = len % 3;
	uint8_t *write, a, b, c;
	const uint8_t *read = (const uint8_t *)data;

	ENSURE_SIZE(buf, buf->size + 4 * ((len / 3) + !!extra) + 1);
	write = (uint8_t *)&buf->ptr[buf->size];

	/* convert each run of 3 bytes into 4 output bytes */
	for (len -= extra; len > 0; len -= 3) {
		a = *read++;
		b = *read++;
		c = *read++;

		*write++ = b64str[a >> 2];
		*write++ = b64str[(a & 0x03) << 4 | b >> 4];
		*write++ = b64str[(b & 0x0f) << 2 | c >> 6];
		*write++ = b64str[c & 0x3f];
	}

	if (extra > 0) {
		a = *read++;
		b = (extra > 1) ? *read++ : 0;

		*write++ = b64str[a >> 2];
		*write++ = b64str[(a & 0x03) << 4 | b >> 4];
		*write++ = (extra > 1) ? b64str[(b & 0x0f) << 2] : '=';
		*write++ = '=';
	}

	buf->size = ((char *)write) - buf->ptr;
	buf->ptr[buf->size] = '\0';

	return 0;
}

static const char b85str[] =
	"0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz!#$%&()*+-;<=>?@^_`{|}~";

int git_buf_put_base85(git_buf *buf, const char *data, size_t len)
{
	ENSURE_SIZE(buf, buf->size + (5 * ((len / 4) + !!(len % 4))) + 1);

	while (len) {
		uint32_t acc = 0;
		char b85[5];
		int i;

		for (i = 24; i >= 0; i -= 8) {
			uint8_t ch = *data++;
			acc |= ch << i;

			if (--len == 0)
				break;
		}

		for (i = 4; i >= 0; i--) {
			int val = acc % 85;
			acc /= 85;

			b85[i] = b85str[val];
		}

		for (i = 0; i < 5; i++)
			buf->ptr[buf->size++] = b85[i];
	}

	buf->ptr[buf->size] = '\0';

	return 0;
}

int git_buf_vprintf(git_buf *buf, const char *format, va_list ap)
{
	int len;
	const size_t expected_size = buf->size + (strlen(format) * 2);

	ENSURE_SIZE(buf, expected_size);

	while (1) {
		va_list args;
		va_copy(args, ap);

		len = p_vsnprintf(
			buf->ptr + buf->size,
			buf->asize - buf->size,
			format, args
		);

		va_end(args);

		if (len < 0) {
			git__free(buf->ptr);
			buf->ptr = git_buf__oom;
			return -1;
		}

		if ((size_t)len + 1 <= buf->asize - buf->size) {
			buf->size += len;
			break;
		}

		ENSURE_SIZE(buf, buf->size + len + 1);
	}

	return 0;
}

int git_buf_printf(git_buf *buf, const char *format, ...)
{
	int r;
	va_list ap;

	va_start(ap, format);
	r = git_buf_vprintf(buf, format, ap);
	va_end(ap);

	return r;
}

void git_buf_copy_cstr(char *data, size_t datasize, const git_buf *buf)
{
	size_t copylen;

	assert(data && datasize && buf);

	data[0] = '\0';

	if (buf->size == 0 || buf->asize <= 0)
		return;

	copylen = buf->size;
	if (copylen > datasize - 1)
		copylen = datasize - 1;
	memmove(data, buf->ptr, copylen);
	data[copylen] = '\0';
}

void git_buf_consume(git_buf *buf, const char *end)
{
	if (end > buf->ptr && end <= buf->ptr + buf->size) {
		size_t consumed = end - buf->ptr;
		memmove(buf->ptr, end, buf->size - consumed);
		buf->size -= consumed;
		buf->ptr[buf->size] = '\0';
	}
}

void git_buf_truncate(git_buf *buf, size_t len)
{
	if (len >= buf->size)
		return;

	buf->size = len;
	if (buf->size < buf->asize)
		buf->ptr[buf->size] = '\0';
}

void git_buf_shorten(git_buf *buf, size_t amount)
{
	if (buf->size > amount)
		git_buf_truncate(buf, buf->size - amount);
	else
		git_buf_clear(buf);
}

void git_buf_rtruncate_at_char(git_buf *buf, char separator)
{
	ssize_t idx = git_buf_rfind_next(buf, separator);
	git_buf_truncate(buf, idx < 0 ? 0 : (size_t)idx);
}

void git_buf_swap(git_buf *buf_a, git_buf *buf_b)
{
	git_buf t = *buf_a;
	*buf_a = *buf_b;
	*buf_b = t;
}

char *git_buf_detach(git_buf *buf)
{
	char *data = buf->ptr;

	if (buf->asize == 0 || buf->ptr == git_buf__oom)
		return NULL;

	git_buf_init(buf, 0);

	return data;
}

void git_buf_attach(git_buf *buf, char *ptr, size_t asize)
{
	git_buf_free(buf);

	if (ptr) {
		buf->ptr = ptr;
		buf->size = strlen(ptr);
		if (asize)
			buf->asize = (asize < buf->size) ? buf->size + 1 : asize;
		else /* pass 0 to fall back on strlen + 1 */
			buf->asize = buf->size + 1;
	} else {
		git_buf_grow(buf, asize);
	}
}

int git_buf_join_n(git_buf *buf, char separator, int nbuf, ...)
{
	va_list ap;
	int i;
	size_t total_size = 0, original_size = buf->size;
	char *out, *original = buf->ptr;

	if (buf->size > 0 && buf->ptr[buf->size - 1] != separator)
		++total_size; /* space for initial separator */

	/* Make two passes to avoid multiple reallocation */

	va_start(ap, nbuf);
	for (i = 0; i < nbuf; ++i) {
		const char* segment;
		size_t segment_len;

		segment = va_arg(ap, const char *);
		if (!segment)
			continue;

		segment_len = strlen(segment);
		total_size += segment_len;
		if (segment_len == 0 || segment[segment_len - 1] != separator)
			++total_size; /* space for separator */
	}
	va_end(ap);

	/* expand buffer if needed */
	if (total_size == 0)
		return 0;
	if (git_buf_grow(buf, buf->size + total_size + 1) < 0)
		return -1;

	out = buf->ptr + buf->size;

	/* append separator to existing buf if needed */
	if (buf->size > 0 && out[-1] != separator)
		*out++ = separator;

	va_start(ap, nbuf);
	for (i = 0; i < nbuf; ++i) {
		const char* segment;
		size_t segment_len;

		segment = va_arg(ap, const char *);
		if (!segment)
			continue;

		/* deal with join that references buffer's original content */
		if (segment >= original && segment < original + original_size) {
			size_t offset = (segment - original);
			segment = buf->ptr + offset;
			segment_len = original_size - offset;
		} else {
			segment_len = strlen(segment);
		}

		/* skip leading separators */
		if (out > buf->ptr && out[-1] == separator)
			while (segment_len > 0 && *segment == separator) {
				segment++;
				segment_len--;
			}

		/* copy over next buffer */
		if (segment_len > 0) {
			memmove(out, segment, segment_len);
			out += segment_len;
		}

		/* append trailing separator (except for last item) */
		if (i < nbuf - 1 && out > buf->ptr && out[-1] != separator)
			*out++ = separator;
	}
	va_end(ap);

	/* set size based on num characters actually written */
	buf->size = out - buf->ptr;
	buf->ptr[buf->size] = '\0';

	return 0;
}

int git_buf_join(
	git_buf *buf,
	char separator,
	const char *str_a,
	const char *str_b)
{
	size_t strlen_a = str_a ? strlen(str_a) : 0;
	size_t strlen_b = strlen(str_b);
	int need_sep = 0;
	ssize_t offset_a = -1;

	/* not safe to have str_b point internally to the buffer */
	assert(str_b < buf->ptr || str_b >= buf->ptr + buf->size);

	/* figure out if we need to insert a separator */
	if (separator && strlen_a) {
		while (*str_b == separator) { str_b++; strlen_b--; }
		if (str_a[strlen_a - 1] != separator)
			need_sep = 1;
	}

	/* str_a could be part of the buffer */
	if (str_a >= buf->ptr && str_a < buf->ptr + buf->size)
		offset_a = str_a - buf->ptr;

	if (git_buf_grow(buf, strlen_a + strlen_b + need_sep + 1) < 0)
		return -1;
	assert(buf->ptr);

	/* fix up internal pointers */
	if (offset_a >= 0)
		str_a = buf->ptr + offset_a;

	/* do the actual copying */
	if (offset_a != 0 && str_a)
		memmove(buf->ptr, str_a, strlen_a);
	if (need_sep)
		buf->ptr[strlen_a] = separator;
	memcpy(buf->ptr + strlen_a + need_sep, str_b, strlen_b);

	buf->size = strlen_a + strlen_b + need_sep;
	buf->ptr[buf->size] = '\0';

	return 0;
}

int git_buf_join3(
	git_buf *buf,
	char separator,
	const char *str_a,
	const char *str_b,
	const char *str_c)
{
	size_t len_a = strlen(str_a), len_b = strlen(str_b), len_c = strlen(str_c);
	int sep_a = 0, sep_b = 0;
	char *tgt;

	/* for this function, disallow pointers into the existing buffer */
	assert(str_a < buf->ptr || str_a >= buf->ptr + buf->size);
	assert(str_b < buf->ptr || str_b >= buf->ptr + buf->size);
	assert(str_c < buf->ptr || str_c >= buf->ptr + buf->size);

	if (separator) {
		if (len_a > 0) {
			while (*str_b == separator) { str_b++; len_b--; }
			sep_a = (str_a[len_a - 1] != separator);
		}
		if (len_a > 0 || len_b > 0)
			while (*str_c == separator) { str_c++; len_c--; }
		if (len_b > 0)
			sep_b = (str_b[len_b - 1] != separator);
	}

	if (git_buf_grow(buf, len_a + sep_a + len_b + sep_b + len_c + 1) < 0)
		return -1;

	tgt = buf->ptr;

	if (len_a) {
		memcpy(tgt, str_a, len_a);
		tgt += len_a;
	}
	if (sep_a)
		*tgt++ = separator;
	if (len_b) {
		memcpy(tgt, str_b, len_b);
		tgt += len_b;
	}
	if (sep_b)
		*tgt++ = separator;
	if (len_c)
		memcpy(tgt, str_c, len_c);

	buf->size = len_a + sep_a + len_b + sep_b + len_c;
	buf->ptr[buf->size] = '\0';

	return 0;
}

void git_buf_rtrim(git_buf *buf)
{
	while (buf->size > 0) {
		if (!git__isspace(buf->ptr[buf->size - 1]))
			break;

		buf->size--;
	}

	if (buf->asize > buf->size)
		buf->ptr[buf->size] = '\0';
}

int git_buf_cmp(const git_buf *a, const git_buf *b)
{
	int result = memcmp(a->ptr, b->ptr, min(a->size, b->size));
	return (result != 0) ? result :
		(a->size < b->size) ? -1 : (a->size > b->size) ? 1 : 0;
}

int git_buf_splice(
	git_buf *buf,
	size_t where,
	size_t nb_to_remove,
	const char *data,
	size_t nb_to_insert)
{
	assert(buf &&
		where <= git_buf_len(buf) &&
		where + nb_to_remove <= git_buf_len(buf));

	/* Ported from git.git
	 * https://github.com/git/git/blob/16eed7c/strbuf.c#L159-176
	 */
	ENSURE_SIZE(buf, buf->size + nb_to_insert - nb_to_insert + 1);

	memmove(buf->ptr + where + nb_to_insert,
			buf->ptr + where + nb_to_remove,
			buf->size - where - nb_to_remove);

	memcpy(buf->ptr + where, data, nb_to_insert);

	buf->size = buf->size + nb_to_insert - nb_to_remove;
	buf->ptr[buf->size] = '\0';
	return 0;
}
