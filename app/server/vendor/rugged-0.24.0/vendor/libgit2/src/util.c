/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include <git2.h>
#include "common.h"
#include <stdio.h>
#include <ctype.h>
#include "posix.h"

#ifdef GIT_WIN32
# include "win32/w32_buffer.h"
#endif

#ifdef _MSC_VER
# include <Shlwapi.h>
#endif

void git_strarray_free(git_strarray *array)
{
	size_t i;

	if (array == NULL)
		return;

	for (i = 0; i < array->count; ++i)
		git__free(array->strings[i]);

	git__free(array->strings);

	memset(array, 0, sizeof(*array));
}

int git_strarray_copy(git_strarray *tgt, const git_strarray *src)
{
	size_t i;

	assert(tgt && src);

	memset(tgt, 0, sizeof(*tgt));

	if (!src->count)
		return 0;

	tgt->strings = git__calloc(src->count, sizeof(char *));
	GITERR_CHECK_ALLOC(tgt->strings);

	for (i = 0; i < src->count; ++i) {
		if (!src->strings[i])
			continue;

		tgt->strings[tgt->count] = git__strdup(src->strings[i]);
		if (!tgt->strings[tgt->count]) {
			git_strarray_free(tgt);
			memset(tgt, 0, sizeof(*tgt));
			return -1;
		}

		tgt->count++;
	}

	return 0;
}

int git__strtol64(int64_t *result, const char *nptr, const char **endptr, int base)
{
	const char *p;
	int64_t n, nn;
	int c, ovfl, v, neg, ndig;

	p = nptr;
	neg = 0;
	n = 0;
	ndig = 0;
	ovfl = 0;

	/*
	 * White space
	 */
	while (git__isspace(*p))
		p++;

	/*
	 * Sign
	 */
	if (*p == '-' || *p == '+')
		if (*p++ == '-')
			neg = 1;

	/*
	 * Base
	 */
	if (base == 0) {
		if (*p != '0')
			base = 10;
		else {
			base = 8;
			if (p[1] == 'x' || p[1] == 'X') {
				p += 2;
				base = 16;
			}
		}
	} else if (base == 16 && *p == '0') {
		if (p[1] == 'x' || p[1] == 'X')
			p += 2;
	} else if (base < 0 || 36 < base)
		goto Return;

	/*
	 * Non-empty sequence of digits
	 */
	for (;; p++,ndig++) {
		c = *p;
		v = base;
		if ('0'<=c && c<='9')
			v = c - '0';
		else if ('a'<=c && c<='z')
			v = c - 'a' + 10;
		else if ('A'<=c && c<='Z')
			v = c - 'A' + 10;
		if (v >= base)
			break;
		nn = n*base + v;
		if (nn < n)
			ovfl = 1;
		n = nn;
	}

Return:
	if (ndig == 0) {
		giterr_set(GITERR_INVALID, "Failed to convert string to long. Not a number");
		return -1;
	}

	if (endptr)
		*endptr = p;

	if (ovfl) {
		giterr_set(GITERR_INVALID, "Failed to convert string to long. Overflow error");
		return -1;
	}

	*result = neg ? -n : n;
	return 0;
}

int git__strtol32(int32_t *result, const char *nptr, const char **endptr, int base)
{
	int error;
	int32_t tmp_int;
	int64_t tmp_long;

	if ((error = git__strtol64(&tmp_long, nptr, endptr, base)) < 0)
		return error;

	tmp_int = tmp_long & 0xFFFFFFFF;
	if (tmp_int != tmp_long) {
		giterr_set(GITERR_INVALID, "Failed to convert. '%s' is too large", nptr);
		return -1;
	}

	*result = tmp_int;

	return error;
}

int git__strcmp(const char *a, const char *b)
{
	while (*a && *b && *a == *b)
		++a, ++b;
	return (int)(*(const unsigned char *)a) - (int)(*(const unsigned char *)b);
}

int git__strcasecmp(const char *a, const char *b)
{
	while (*a && *b && git__tolower(*a) == git__tolower(*b))
		++a, ++b;
	return ((unsigned char)git__tolower(*a) - (unsigned char)git__tolower(*b));
}

int git__strcasesort_cmp(const char *a, const char *b)
{
	int cmp = 0;

	while (*a && *b) {
		if (*a != *b) {
			if (git__tolower(*a) != git__tolower(*b))
				break;
			/* use case in sort order even if not in equivalence */
			if (!cmp)
				cmp = (int)(*(const uint8_t *)a) - (int)(*(const uint8_t *)b);
		}

		++a, ++b;
	}

	if (*a || *b)
		return (unsigned char)git__tolower(*a) - (unsigned char)git__tolower(*b);

	return cmp;
}

int git__strncmp(const char *a, const char *b, size_t sz)
{
	while (sz && *a && *b && *a == *b)
		--sz, ++a, ++b;
	if (!sz)
		return 0;
	return (int)(*(const unsigned char *)a) - (int)(*(const unsigned char *)b);
}

int git__strncasecmp(const char *a, const char *b, size_t sz)
{
	int al, bl;

	do {
		al = (unsigned char)git__tolower(*a);
		bl = (unsigned char)git__tolower(*b);
		++a, ++b;
	} while (--sz && al && al == bl);

	return al - bl;
}

void git__strntolower(char *str, size_t len)
{
	size_t i;

	for (i = 0; i < len; ++i) {
		str[i] = (char)git__tolower(str[i]);
	}
}

void git__strtolower(char *str)
{
	git__strntolower(str, strlen(str));
}

int git__prefixcmp(const char *str, const char *prefix)
{
	for (;;) {
		unsigned char p = *(prefix++), s;
		if (!p)
			return 0;
		if ((s = *(str++)) != p)
			return s - p;
	}
}

int git__prefixcmp_icase(const char *str, const char *prefix)
{
	return strncasecmp(str, prefix, strlen(prefix));
}

int git__prefixncmp_icase(const char *str, size_t str_n, const char *prefix)
{
	int s, p;

	while(str_n--) {
		s = (unsigned char)git__tolower(*str++);
		p = (unsigned char)git__tolower(*prefix++);

		if (s != p)
			return s - p;
	}

	return (0 - *prefix);
}

int git__suffixcmp(const char *str, const char *suffix)
{
	size_t a = strlen(str);
	size_t b = strlen(suffix);
	if (a < b)
		return -1;
	return strcmp(str + (a - b), suffix);
}

char *git__strtok(char **end, const char *sep)
{
	char *ptr = *end;

	while (*ptr && strchr(sep, *ptr))
		++ptr;

	if (*ptr) {
		char *start = ptr;
		*end = start + 1;

		while (**end && !strchr(sep, **end))
			++*end;

		if (**end) {
			**end = '\0';
			++*end;
		}

		return start;
	}

	return NULL;
}

/* Similar to strtok, but does not collapse repeated tokens. */
char *git__strsep(char **end, const char *sep)
{
	char *start = *end, *ptr = *end;

	while (*ptr && !strchr(sep, *ptr))
		++ptr;

	if (*ptr) {
		*end = ptr + 1;
		*ptr = '\0';

		return start;
	}

	return NULL;
}

void git__hexdump(const char *buffer, size_t len)
{
	static const size_t LINE_WIDTH = 16;

	size_t line_count, last_line, i, j;
	const char *line;

	line_count = (len / LINE_WIDTH);
	last_line = (len % LINE_WIDTH);

	for (i = 0; i < line_count; ++i) {
		line = buffer + (i * LINE_WIDTH);
		for (j = 0; j < LINE_WIDTH; ++j, ++line)
			printf("%02X ", (unsigned char)*line & 0xFF);

		printf("| ");

		line = buffer + (i * LINE_WIDTH);
		for (j = 0; j < LINE_WIDTH; ++j, ++line)
			printf("%c", (*line >= 32 && *line <= 126) ? *line : '.');

		printf("\n");
	}

	if (last_line > 0) {

		line = buffer + (line_count * LINE_WIDTH);
		for (j = 0; j < last_line; ++j, ++line)
			printf("%02X ", (unsigned char)*line & 0xFF);

		for (j = 0; j < (LINE_WIDTH - last_line); ++j)
			printf("	");

		printf("| ");

		line = buffer + (line_count * LINE_WIDTH);
		for (j = 0; j < last_line; ++j, ++line)
			printf("%c", (*line >= 32 && *line <= 126) ? *line : '.');

		printf("\n");
	}

	printf("\n");
}

#ifdef GIT_LEGACY_HASH
uint32_t git__hash(const void *key, int len, unsigned int seed)
{
	const uint32_t m = 0x5bd1e995;
	const int r = 24;
	uint32_t h = seed ^ len;

	const unsigned char *data = (const unsigned char *)key;

	while(len >= 4) {
		uint32_t k = *(uint32_t *)data;

		k *= m;
		k ^= k >> r;
		k *= m;

		h *= m;
		h ^= k;

		data += 4;
		len -= 4;
	}

	switch(len) {
	case 3: h ^= data[2] << 16;
	case 2: h ^= data[1] << 8;
	case 1: h ^= data[0];
			h *= m;
	};

	h ^= h >> 13;
	h *= m;
	h ^= h >> 15;

	return h;
}
#else
/*
	Cross-platform version of Murmurhash3
	http://code.google.com/p/smhasher/wiki/MurmurHash3
	by Austin Appleby (aappleby@gmail.com)

	This code is on the public domain.
*/
uint32_t git__hash(const void *key, int len, uint32_t seed)
{

#define MURMUR_BLOCK() {\
	k1 *= c1; \
	k1 = git__rotl(k1,11);\
	k1 *= c2;\
	h1 ^= k1;\
	h1 = h1*3 + 0x52dce729;\
	c1 = c1*5 + 0x7b7d159c;\
	c2 = c2*5 + 0x6bce6396;\
}

	const uint8_t *data = (const uint8_t*)key;
	const int nblocks = len / 4;

	const uint32_t *blocks = (const uint32_t *)(data + nblocks * 4);
	const uint8_t *tail = (const uint8_t *)(data + nblocks * 4);

	uint32_t h1 = 0x971e137b ^ seed;
	uint32_t k1;

	uint32_t c1 = 0x95543787;
	uint32_t c2 = 0x2ad7eb25;

	int i;

	for (i = -nblocks; i; i++) {
		k1 = blocks[i];
		MURMUR_BLOCK();
	}

	k1 = 0;

	switch(len & 3) {
	case 3: k1 ^= tail[2] << 16;
	case 2: k1 ^= tail[1] << 8;
	case 1: k1 ^= tail[0];
			MURMUR_BLOCK();
	}

	h1 ^= len;
	h1 ^= h1 >> 16;
	h1 *= 0x85ebca6b;
	h1 ^= h1 >> 13;
	h1 *= 0xc2b2ae35;
	h1 ^= h1 >> 16;

	return h1;
}
#endif

/**
 * A modified `bsearch` from the BSD glibc.
 *
 * Copyright (c) 1990 Regents of the University of California.
 * All rights reserved.
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 3. [rescinded 22 July 1999]
 * 4. Neither the name of the University nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
int git__bsearch(
	void **array,
	size_t array_len,
	const void *key,
	int (*compare)(const void *, const void *),
	size_t *position)
{
	size_t lim;
	int cmp = -1;
	void **part, **base = array;

	for (lim = array_len; lim != 0; lim >>= 1) {
		part = base + (lim >> 1);
		cmp = (*compare)(key, *part);
		if (cmp == 0) {
			base = part;
			break;
		}
		if (cmp > 0) { /* key > p; take right partition */
			base = part + 1;
			lim--;
		} /* else take left partition */
	}

	if (position)
		*position = (base - array);

	return (cmp == 0) ? 0 : GIT_ENOTFOUND;
}

int git__bsearch_r(
	void **array,
	size_t array_len,
	const void *key,
	int (*compare_r)(const void *, const void *, void *),
	void *payload,
	size_t *position)
{
	size_t lim;
	int cmp = -1;
	void **part, **base = array;

	for (lim = array_len; lim != 0; lim >>= 1) {
		part = base + (lim >> 1);
		cmp = (*compare_r)(key, *part, payload);
		if (cmp == 0) {
			base = part;
			break;
		}
		if (cmp > 0) { /* key > p; take right partition */
			base = part + 1;
			lim--;
		} /* else take left partition */
	}

	if (position)
		*position = (base - array);

	return (cmp == 0) ? 0 : GIT_ENOTFOUND;
}

/**
 * A strcmp wrapper
 *
 * We don't want direct pointers to the CRT on Windows, we may
 * get stdcall conflicts.
 */
int git__strcmp_cb(const void *a, const void *b)
{
	return strcmp((const char *)a, (const char *)b);
}

int git__strcasecmp_cb(const void *a, const void *b)
{
	return strcasecmp((const char *)a, (const char *)b);
}

int git__parse_bool(int *out, const char *value)
{
	/* A missing value means true */
	if (value == NULL ||
		!strcasecmp(value, "true") ||
		!strcasecmp(value, "yes") ||
		!strcasecmp(value, "on")) {
		*out = 1;
		return 0;
	}
	if (!strcasecmp(value, "false") ||
		!strcasecmp(value, "no") ||
		!strcasecmp(value, "off") ||
		value[0] == '\0') {
		*out = 0;
		return 0;
	}

	return -1;
}

size_t git__unescape(char *str)
{
	char *scan, *pos = str;

	if (!str)
		return 0;

	for (scan = str; *scan; pos++, scan++) {
		if (*scan == '\\' && *(scan + 1) != '\0')
			scan++; /* skip '\' but include next char */
		if (pos != scan)
			*pos = *scan;
	}

	if (pos != scan) {
		*pos = '\0';
	}

	return (pos - str);
}

#if defined(HAVE_QSORT_S) || (defined(HAVE_QSORT_R) && defined(BSD))
typedef struct {
	git__sort_r_cmp cmp;
	void *payload;
} git__qsort_r_glue;

static int GIT_STDLIB_CALL git__qsort_r_glue_cmp(
	void *payload, const void *a, const void *b)
{
	git__qsort_r_glue *glue = payload;
	return glue->cmp(a, b, glue->payload);
}
#endif

void git__qsort_r(
	void *els, size_t nel, size_t elsize, git__sort_r_cmp cmp, void *payload)
{
#if defined(HAVE_QSORT_R) && defined(BSD)
	git__qsort_r_glue glue = { cmp, payload };
	qsort_r(els, nel, elsize, &glue, git__qsort_r_glue_cmp);
#elif defined(HAVE_QSORT_R) && defined(__GLIBC__)
	qsort_r(els, nel, elsize, cmp, payload);
#elif defined(HAVE_QSORT_S)
	git__qsort_r_glue glue = { cmp, payload };
	qsort_s(els, nel, elsize, git__qsort_r_glue_cmp, &glue);
#else
	git__insertsort_r(els, nel, elsize, NULL, cmp, payload);
#endif
}

void git__insertsort_r(
	void *els, size_t nel, size_t elsize, void *swapel,
	git__sort_r_cmp cmp, void *payload)
{
	uint8_t *base = els;
	uint8_t *end = base + nel * elsize;
	uint8_t *i, *j;
	bool freeswap = !swapel;

	if (freeswap)
		swapel = git__malloc(elsize);

	for (i = base + elsize; i < end; i += elsize)
		for (j = i; j > base && cmp(j, j - elsize, payload) < 0; j -= elsize) {
			memcpy(swapel, j, elsize);
			memcpy(j, j - elsize, elsize);
			memcpy(j - elsize, swapel, elsize);
		}

	if (freeswap)
		git__free(swapel);
}

/*
 * git__utf8_iterate is taken from the utf8proc project,
 * http://www.public-software-group.org/utf8proc
 *
 * Copyright (c) 2009 Public Software Group e. V., Berlin, Germany
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the ""Software""),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */

static const int8_t utf8proc_utf8class[256] = {
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
	3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
	4, 4, 4, 4, 4, 4, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0
};

int git__utf8_charlen(const uint8_t *str, int str_len)
{
	int length, i;

	length = utf8proc_utf8class[str[0]];
	if (!length)
		return -1;

	if (str_len >= 0 && length > str_len)
		return -str_len;

	for (i = 1; i < length; i++) {
		if ((str[i] & 0xC0) != 0x80)
			return -i;
	}

	return length;
}

int git__utf8_iterate(const uint8_t *str, int str_len, int32_t *dst)
{
	int length;
	int32_t uc = -1;

	*dst = -1;
	length = git__utf8_charlen(str, str_len);
	if (length < 0)
		return -1;

	switch (length) {
		case 1:
			uc = str[0];
			break;
		case 2:
			uc = ((str[0] & 0x1F) <<  6) + (str[1] & 0x3F);
			if (uc < 0x80) uc = -1;
			break;
		case 3:
			uc = ((str[0] & 0x0F) << 12) + ((str[1] & 0x3F) <<  6)
				+ (str[2] & 0x3F);
			if (uc < 0x800 || (uc >= 0xD800 && uc < 0xE000) ||
					(uc >= 0xFDD0 && uc < 0xFDF0)) uc = -1;
			break;
		case 4:
			uc = ((str[0] & 0x07) << 18) + ((str[1] & 0x3F) << 12)
				+ ((str[2] & 0x3F) <<  6) + (str[3] & 0x3F);
			if (uc < 0x10000 || uc >= 0x110000) uc = -1;
			break;
	}

	if (uc < 0 || ((uc & 0xFFFF) >= 0xFFFE))
		return -1;

	*dst = uc;
	return length;
}

#ifdef GIT_WIN32
int git__getenv(git_buf *out, const char *name)
{
	wchar_t *wide_name = NULL, *wide_value = NULL;
	DWORD value_len;
	int error = -1;

	git_buf_clear(out);

	if (git__utf8_to_16_alloc(&wide_name, name) < 0)
		return -1;

	if ((value_len = GetEnvironmentVariableW(wide_name, NULL, 0)) > 0) {
		wide_value = git__malloc(value_len * sizeof(wchar_t));
		GITERR_CHECK_ALLOC(wide_value);

		value_len = GetEnvironmentVariableW(wide_name, wide_value, value_len);
	}

	if (value_len)
		error = git_buf_put_w(out, wide_value, value_len);
	else if (GetLastError() == ERROR_ENVVAR_NOT_FOUND)
		error = GIT_ENOTFOUND;
	else
		giterr_set(GITERR_OS, "could not read environment variable '%s'", name);

	git__free(wide_name);
	git__free(wide_value);
	return error;
}
#else
int git__getenv(git_buf *out, const char *name)
{
	const char *val = getenv(name);

	git_buf_clear(out);

	if (!val)
		return GIT_ENOTFOUND;

	return git_buf_puts(out, val);
}
#endif
