/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_stream_h__
#define INCLUDE_stream_h__

#include "common.h"
#include "git2/sys/stream.h"

GIT_INLINE(int) git_stream_connect(git_stream *st)
{
	return st->connect(st);
}

GIT_INLINE(int) git_stream_is_encrypted(git_stream *st)
{
	return st->encrypted;
}

GIT_INLINE(int) git_stream_certificate(git_cert **out, git_stream *st)
{
	if (!st->encrypted) {
		giterr_set(GITERR_INVALID, "an unencrypted stream does not have a certificate");
		return -1;
	}

	return st->certificate(out, st);
}

GIT_INLINE(int) git_stream_supports_proxy(git_stream *st)
{
	return st->proxy_support;
}

GIT_INLINE(int) git_stream_set_proxy(git_stream *st, const char *proxy_url)
{
	if (!st->proxy_support) {
		giterr_set(GITERR_INVALID, "proxy not supported on this stream");
		return -1;
	}

	return st->set_proxy(st, proxy_url);
}

GIT_INLINE(ssize_t) git_stream_read(git_stream *st, void *data, size_t len)
{
	return st->read(st, data, len);
}

GIT_INLINE(ssize_t) git_stream_write(git_stream *st, const char *data, size_t len, int flags)
{
	return st->write(st, data, len, flags);
}

GIT_INLINE(int) git_stream_close(git_stream *st)
{
	return st->close(st);
}

GIT_INLINE(void) git_stream_free(git_stream *st)
{
	st->free(st);
}

#endif
