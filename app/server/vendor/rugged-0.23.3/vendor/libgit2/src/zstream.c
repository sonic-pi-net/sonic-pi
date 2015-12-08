/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include <zlib.h>

#include "zstream.h"
#include "buffer.h"

#define ZSTREAM_BUFFER_SIZE (1024 * 1024)
#define ZSTREAM_BUFFER_MIN_EXTRA 8

static int zstream_seterr(git_zstream *zs)
{
	if (zs->zerr == Z_OK || zs->zerr == Z_STREAM_END)
		return 0;

	if (zs->zerr == Z_MEM_ERROR)
		giterr_set_oom();
	else if (zs->z.msg)
		giterr_set(GITERR_ZLIB, zs->z.msg);
	else
		giterr_set(GITERR_ZLIB, "Unknown compression error");

	return -1;
}

int git_zstream_init(git_zstream *zstream)
{
	zstream->zerr = deflateInit(&zstream->z, Z_DEFAULT_COMPRESSION);
	return zstream_seterr(zstream);
}

void git_zstream_free(git_zstream *zstream)
{
	deflateEnd(&zstream->z);
}

void git_zstream_reset(git_zstream *zstream)
{
	deflateReset(&zstream->z);
	zstream->in = NULL;
	zstream->in_len = 0;
	zstream->zerr = Z_STREAM_END;
}

int git_zstream_set_input(git_zstream *zstream, const void *in, size_t in_len)
{
	zstream->in = in;
	zstream->in_len = in_len;
	zstream->zerr = Z_OK;
	return 0;
}

bool git_zstream_done(git_zstream *zstream)
{
	return (!zstream->in_len && zstream->zerr == Z_STREAM_END);
}

size_t git_zstream_suggest_output_len(git_zstream *zstream)
{
	if (zstream->in_len > ZSTREAM_BUFFER_SIZE)
		return ZSTREAM_BUFFER_SIZE;
	else if (zstream->in_len > ZSTREAM_BUFFER_MIN_EXTRA)
		return zstream->in_len;
	else
		return ZSTREAM_BUFFER_MIN_EXTRA;
}

int git_zstream_get_output(void *out, size_t *out_len, git_zstream *zstream)
{
	int zflush = Z_FINISH;
	size_t out_remain = *out_len;

	while (out_remain > 0 && zstream->zerr != Z_STREAM_END) {
		size_t out_queued, in_queued, out_used, in_used;

		/* set up in data */
		zstream->z.next_in  = (Bytef *)zstream->in;
		zstream->z.avail_in = (uInt)zstream->in_len;
		if ((size_t)zstream->z.avail_in != zstream->in_len) {
			zstream->z.avail_in = INT_MAX;
			zflush = Z_NO_FLUSH;
		} else {
			zflush = Z_FINISH;
		}
		in_queued = (size_t)zstream->z.avail_in;

		/* set up out data */
		zstream->z.next_out = out;
		zstream->z.avail_out = (uInt)out_remain;
		if ((size_t)zstream->z.avail_out != out_remain)
			zstream->z.avail_out = INT_MAX;
		out_queued = (size_t)zstream->z.avail_out;

		/* compress next chunk */
		zstream->zerr = deflate(&zstream->z, zflush);

		if (zstream->zerr == Z_STREAM_ERROR)
			return zstream_seterr(zstream);

		out_used = (out_queued - zstream->z.avail_out);
		out_remain -= out_used;
		out = ((char *)out) + out_used;

		in_used = (in_queued - zstream->z.avail_in);
		zstream->in_len -= in_used;
		zstream->in += in_used;
	}

	/* either we finished the input or we did not flush the data */
	assert(zstream->in_len > 0 || zflush == Z_FINISH);

	/* set out_size to number of bytes actually written to output */
	*out_len = *out_len - out_remain;

	return 0;
}

int git_zstream_deflatebuf(git_buf *out, const void *in, size_t in_len)
{
	git_zstream zs = GIT_ZSTREAM_INIT;
	int error = 0;

	if ((error = git_zstream_init(&zs)) < 0)
		return error;

	if ((error = git_zstream_set_input(&zs, in, in_len)) < 0)
		goto done;

	while (!git_zstream_done(&zs)) {
		size_t step = git_zstream_suggest_output_len(&zs), written;

		if ((error = git_buf_grow_by(out, step)) < 0)
			goto done;

		written = out->asize - out->size;

		if ((error = git_zstream_get_output(
				out->ptr + out->size, &written, &zs)) < 0)
			goto done;

		out->size += written;
	}

	/* NULL terminate for consistency if possible */
	if (out->size < out->asize)
		out->ptr[out->size] = '\0';

done:
	git_zstream_free(&zs);
	return error;
}
