/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_delta_apply_h__
#define INCLUDE_delta_apply_h__

#include "odb.h"

/**
 * Apply a git binary delta to recover the original content.
 *
 * @param out the output buffer to receive the original data.
 *		Only out->data and out->len are populated, as this is
 *		the only information available in the delta.
 * @param base the base to copy from during copy instructions.
 * @param base_len number of bytes available at base.
 * @param delta the delta to execute copy/insert instructions from.
 * @param delta_len total number of bytes in the delta.
 * @return
 * - 0 on a successful delta unpack.
 * - GIT_ERROR if the delta is corrupt or doesn't match the base.
 */
extern int git__delta_apply(
	git_rawobj *out,
	const unsigned char *base,
	size_t base_len,
	const unsigned char *delta,
	size_t delta_len);

/**
 * Read the header of a git binary delta.
 *
 * @param delta the delta to execute copy/insert instructions from.
 * @param delta_len total number of bytes in the delta.
 * @param base_sz pointer to store the base size field.
 * @param res_sz pointer to store the result size field.
 * @return
 * - 0 on a successful decoding the header.
 * - GIT_ERROR if the delta is corrupt.
 */
extern int git__delta_read_header(
	const unsigned char *delta,
	size_t delta_len,
	size_t *base_sz,
	size_t *res_sz);

#endif
