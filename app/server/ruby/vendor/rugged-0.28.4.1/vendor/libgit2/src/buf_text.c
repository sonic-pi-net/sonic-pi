/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "buf_text.h"

int git_buf_text_puts_escaped(
	git_buf *buf,
	const char *string,
	const char *esc_chars,
	const char *esc_with)
{
	const char *scan;
	size_t total = 0, esc_len = strlen(esc_with), count, alloclen;

	if (!string)
		return 0;

	for (scan = string; *scan; ) {
		/* count run of non-escaped characters */
		count = strcspn(scan, esc_chars);
		total += count;
		scan += count;
		/* count run of escaped characters */
		count = strspn(scan, esc_chars);
		total += count * (esc_len + 1);
		scan += count;
	}

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, total, 1);
	if (git_buf_grow_by(buf, alloclen) < 0)
		return -1;

	for (scan = string; *scan; ) {
		count = strcspn(scan, esc_chars);

		memmove(buf->ptr + buf->size, scan, count);
		scan += count;
		buf->size += count;

		for (count = strspn(scan, esc_chars); count > 0; --count) {
			/* copy escape sequence */
			memmove(buf->ptr + buf->size, esc_with, esc_len);
			buf->size += esc_len;
			/* copy character to be escaped */
			buf->ptr[buf->size] = *scan;
			buf->size++;
			scan++;
		}
	}

	buf->ptr[buf->size] = '\0';

	return 0;
}

void git_buf_text_unescape(git_buf *buf)
{
	buf->size = git__unescape(buf->ptr);
}

int git_buf_text_crlf_to_lf(git_buf *tgt, const git_buf *src)
{
	const char *scan = src->ptr;
	const char *scan_end = src->ptr + src->size;
	const char *next = memchr(scan, '\r', src->size);
	size_t new_size;
	char *out;

	assert(tgt != src);

	if (!next)
		return git_buf_set(tgt, src->ptr, src->size);

	/* reduce reallocs while in the loop */
	GIT_ERROR_CHECK_ALLOC_ADD(&new_size, src->size, 1);
	if (git_buf_grow(tgt, new_size) < 0)
		return -1;

	out = tgt->ptr;
	tgt->size = 0;

	/* Find the next \r and copy whole chunk up to there to tgt */
	for (; next; scan = next + 1, next = memchr(scan, '\r', scan_end - scan)) {
		if (next > scan) {
			size_t copylen = (size_t)(next - scan);
			memcpy(out, scan, copylen);
			out += copylen;
		}

		/* Do not drop \r unless it is followed by \n */
		if (next + 1 == scan_end || next[1] != '\n')
			*out++ = '\r';
	}

	/* Copy remaining input into dest */
	if (scan < scan_end) {
		size_t remaining = (size_t)(scan_end - scan);
		memcpy(out, scan, remaining);
		out += remaining;
	}

	tgt->size = (size_t)(out - tgt->ptr);
	tgt->ptr[tgt->size] = '\0';

	return 0;
}

int git_buf_text_lf_to_crlf(git_buf *tgt, const git_buf *src)
{
	const char *start = src->ptr;
	const char *end = start + src->size;
	const char *scan = start;
	const char *next = memchr(scan, '\n', src->size);
	size_t alloclen;

	assert(tgt != src);

	if (!next)
		return git_buf_set(tgt, src->ptr, src->size);

	/* attempt to reduce reallocs while in the loop */
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, src->size, src->size >> 4);
	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, alloclen, 1);
	if (git_buf_grow(tgt, alloclen) < 0)
		return -1;
	tgt->size = 0;

	for (; next; scan = next + 1, next = memchr(scan, '\n', end - scan)) {
		size_t copylen = next - scan;

		/* if we find mixed line endings, carry on */
		if (copylen && next[-1] == '\r')
			copylen--;

		GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, copylen, 3);
		if (git_buf_grow_by(tgt, alloclen) < 0)
			return -1;

		if (copylen) {
			memcpy(tgt->ptr + tgt->size, scan, copylen);
			tgt->size += copylen;
		}

		tgt->ptr[tgt->size++] = '\r';
		tgt->ptr[tgt->size++] = '\n';
	}

	tgt->ptr[tgt->size] = '\0';
	return git_buf_put(tgt, scan, end - scan);
}

int git_buf_text_common_prefix(git_buf *buf, const git_strarray *strings)
{
	size_t i;
	const char *str, *pfx;

	git_buf_clear(buf);

	if (!strings || !strings->count)
		return 0;

	/* initialize common prefix to first string */
	if (git_buf_sets(buf, strings->strings[0]) < 0)
		return -1;

	/* go through the rest of the strings, truncating to shared prefix */
	for (i = 1; i < strings->count; ++i) {

		for (str = strings->strings[i], pfx = buf->ptr;
			 *str && *str == *pfx; str++, pfx++)
			/* scanning */;

		git_buf_truncate(buf, pfx - buf->ptr);

		if (!buf->size)
			break;
	}

	return 0;
}

bool git_buf_text_is_binary(const git_buf *buf)
{
	const char *scan = buf->ptr, *end = buf->ptr + buf->size;
	git_bom_t bom;
	int printable = 0, nonprintable = 0;

	scan += git_buf_text_detect_bom(&bom, buf);

	if (bom > GIT_BOM_UTF8)
		return 1;

	while (scan < end) {
		unsigned char c = *scan++;

		/* Printable characters are those above SPACE (0x1F) excluding DEL,
		 * and including BS, ESC and FF.
		 */
		if ((c > 0x1F && c != 127) || c == '\b' || c == '\033' || c == '\014')
			printable++;
		else if (c == '\0')
			return true;
		else if (!git__isspace(c))
			nonprintable++;
	}

	return ((printable >> 7) < nonprintable);
}

bool git_buf_text_contains_nul(const git_buf *buf)
{
	return (memchr(buf->ptr, '\0', buf->size) != NULL);
}

int git_buf_text_detect_bom(git_bom_t *bom, const git_buf *buf)
{
	const char *ptr;
	size_t len;

	*bom = GIT_BOM_NONE;
	/* need at least 2 bytes to look for any BOM */
	if (buf->size < 2)
		return 0;

	ptr = buf->ptr;
	len = buf->size;

	switch (*ptr++) {
	case 0:
		if (len >= 4 && ptr[0] == 0 && ptr[1] == '\xFE' && ptr[2] == '\xFF') {
			*bom = GIT_BOM_UTF32_BE;
			return 4;
		}
		break;
	case '\xEF':
		if (len >= 3 && ptr[0] == '\xBB' && ptr[1] == '\xBF') {
			*bom = GIT_BOM_UTF8;
			return 3;
		}
		break;
	case '\xFE':
		if (*ptr == '\xFF') {
			*bom = GIT_BOM_UTF16_BE;
			return 2;
		}
		break;
	case '\xFF':
		if (*ptr != '\xFE')
			break;
		if (len >= 4 && ptr[1] == 0 && ptr[2] == 0) {
			*bom = GIT_BOM_UTF32_LE;
			return 4;
		} else {
			*bom = GIT_BOM_UTF16_LE;
			return 2;
		}
		break;
	default:
		break;
	}

	return 0;
}

bool git_buf_text_gather_stats(
	git_buf_text_stats *stats, const git_buf *buf, bool skip_bom)
{
	const char *scan = buf->ptr, *end = buf->ptr + buf->size;
	int skip;

	memset(stats, 0, sizeof(*stats));

	/* BOM detection */
	skip = git_buf_text_detect_bom(&stats->bom, buf);
	if (skip_bom)
		scan += skip;

	/* Ignore EOF character */
	if (buf->size > 0 && end[-1] == '\032')
		end--;

	/* Counting loop */
	while (scan < end) {
		unsigned char c = *scan++;

		if (c > 0x1F && c != 0x7F)
			stats->printable++;
		else switch (c) {
			case '\0':
				stats->nul++;
				stats->nonprintable++;
				break;
			case '\n':
				stats->lf++;
				break;
			case '\r':
				stats->cr++;
				if (scan < end && *scan == '\n')
					stats->crlf++;
				break;
			case '\t': case '\f': case '\v': case '\b': case 0x1b: /*ESC*/
				stats->printable++;
				break;
			default:
				stats->nonprintable++;
				break;
			}
	}

	/* Treat files with a bare CR as binary */
	return (stats->cr != stats->crlf || stats->nul > 0 ||
		((stats->printable >> 7) < stats->nonprintable));
}
