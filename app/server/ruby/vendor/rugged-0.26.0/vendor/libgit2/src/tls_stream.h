/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_tls_stream_h__
#define INCLUDE_tls_stream_h__

#include "common.h"

#include "git2/sys/stream.h"

/**
 * Create a TLS stream with the most appropriate backend available for
 * the current platform.
 *
 * This allows us to ask for a SecureTransport or OpenSSL stream
 * according to being on general Unix vs OS X.
 */
extern int git_tls_stream_new(git_stream **out, const char *host, const char *port);

#endif
