/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_sys_git_stream_h__
#define INCLUDE_sys_git_stream_h__

#include "git2/common.h"
#include "git2/types.h"

GIT_BEGIN_DECL

#define GIT_STREAM_VERSION 1

/**
 * Every stream must have this struct as its first element, so the
 * API can talk to it. You'd define your stream as
 *
 *     struct my_stream {
 *             git_stream parent;
 *             ...
 *     }
 *
 * and fill the functions
 */
typedef struct git_stream {
	int version;

	int encrypted;
	int proxy_support;
	int (*connect)(struct git_stream *);
	int (*certificate)(git_cert **, struct git_stream *);
	int (*set_proxy)(struct git_stream *, const char *proxy_url);
	ssize_t (*read)(struct git_stream *, void *, size_t);
	ssize_t (*write)(struct git_stream *, const char *, size_t, int);
	int (*close)(struct git_stream *);
	void (*free)(struct git_stream *);
} git_stream;

typedef int (*git_stream_cb)(git_stream **out, const char *host, const char *port);

/**
 * Register a TLS stream constructor for the library to use
 *
 * If a constructor is already set, it will be overwritten. Pass
 * `NULL` in order to deregister the current constructor.
 *
 * @param ctor the constructor to use
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_stream_register_tls(git_stream_cb ctor);

GIT_END_DECL

#endif
