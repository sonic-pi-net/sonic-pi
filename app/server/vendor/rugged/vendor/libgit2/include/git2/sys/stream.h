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
	int (*connect)(struct git_stream *);
	int (*certificate)(git_cert **, struct git_stream *);
	ssize_t (*read)(struct git_stream *, void *, size_t);
	ssize_t (*write)(struct git_stream *, const char *, size_t, int);
	int (*close)(struct git_stream *);
	void (*free)(struct git_stream *);
} git_stream;

#endif
