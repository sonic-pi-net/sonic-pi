/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"
#include "git2/odb.h"
#include "delta-apply.h"

/*
 * This file was heavily cribbed from BinaryDelta.java in JGit, which
 * itself was heavily cribbed from <code>patch-delta.c</code> in the
 * GIT project.	The original delta patching code was written by
 * Nicolas Pitre <nico@cam.org>.
 */

static int hdr_sz(
	size_t *size,
	const unsigned char **delta,
	const unsigned char *end)
{
	const unsigned char *d = *delta;
	size_t r = 0;
	unsigned int c, shift = 0;

	do {
		if (d == end)
			return -1;
		c = *d++;
		r |= (c & 0x7f) << shift;
		shift += 7;
	} while (c & 0x80);
	*delta = d;
	*size = r;
	return 0;
}

int git__delta_read_header(
	const unsigned char *delta,
	size_t delta_len,
	size_t *base_sz,
	size_t *res_sz)
{
	const unsigned char *delta_end = delta + delta_len;
	if ((hdr_sz(base_sz, &delta, delta_end) < 0) ||
	    (hdr_sz(res_sz, &delta, delta_end) < 0))
		return -1;
	return 0;
}

int git__delta_apply(
	git_rawobj *out,
	const unsigned char *base,
	size_t base_len,
	const unsigned char *delta,
	size_t delta_len)
{
	const unsigned char *delta_end = delta + delta_len;
	size_t base_sz, res_sz, alloc_sz;
	unsigned char *res_dp;

	/* Check that the base size matches the data we were given;
	 * if not we would underflow while accessing data from the
	 * base object, resulting in data corruption or segfault.
	 */
	if ((hdr_sz(&base_sz, &delta, delta_end) < 0) || (base_sz != base_len)) {
		giterr_set(GITERR_INVALID, "Failed to apply delta. Base size does not match given data");
		return -1;
	}

	if (hdr_sz(&res_sz, &delta, delta_end) < 0) {
		giterr_set(GITERR_INVALID, "Failed to apply delta. Base size does not match given data");
		return -1;
	}

	GITERR_CHECK_ALLOC_ADD(&alloc_sz, res_sz, 1);
	res_dp = git__malloc(alloc_sz);
	GITERR_CHECK_ALLOC(res_dp);

	res_dp[res_sz] = '\0';
	out->data = res_dp;
	out->len = res_sz;

	while (delta < delta_end) {
		unsigned char cmd = *delta++;
		if (cmd & 0x80) {
			/* cmd is a copy instruction; copy from the base.
			 */
			size_t off = 0, len = 0;

			if (cmd & 0x01) off = *delta++;
			if (cmd & 0x02) off |= *delta++ << 8;
			if (cmd & 0x04) off |= *delta++ << 16;
			if (cmd & 0x08) off |= *delta++ << 24;

			if (cmd & 0x10) len = *delta++;
			if (cmd & 0x20) len |= *delta++ << 8;
			if (cmd & 0x40) len |= *delta++ << 16;
			if (!len)		len = 0x10000;

			if (base_len < off + len || res_sz < len)
				goto fail;
			memcpy(res_dp, base + off, len);
			res_dp += len;
			res_sz -= len;

		} else if (cmd) {
			/* cmd is a literal insert instruction; copy from
			 * the delta stream itself.
			 */
			if (delta_end - delta < cmd || res_sz < cmd)
				goto fail;
			memcpy(res_dp, delta, cmd);
			delta += cmd;
			res_dp += cmd;
			res_sz -= cmd;

		} else {
			/* cmd == 0 is reserved for future encodings.
			 */
			goto fail;
		}
	}

	if (delta != delta_end || res_sz)
		goto fail;
	return 0;

fail:
	git__free(out->data);
	out->data = NULL;
	giterr_set(GITERR_INVALID, "Failed to apply delta");
	return -1;
}
