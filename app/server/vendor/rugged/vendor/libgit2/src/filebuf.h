/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_filebuf_h__
#define INCLUDE_filebuf_h__

#include "fileops.h"
#include "hash.h"
#include <zlib.h>

#ifdef GIT_THREADS
#	define GIT_FILEBUF_THREADS
#endif

#define GIT_FILEBUF_HASH_CONTENTS		(1 << 0)
#define GIT_FILEBUF_APPEND				(1 << 2)
#define GIT_FILEBUF_FORCE				(1 << 3)
#define GIT_FILEBUF_TEMPORARY			(1 << 4)
#define GIT_FILEBUF_DO_NOT_BUFFER		(1 << 5)
#define GIT_FILEBUF_DEFLATE_SHIFT		(6)

#define GIT_FILELOCK_EXTENSION ".lock\0"
#define GIT_FILELOCK_EXTLENGTH 6

struct git_filebuf {
	char *path_original;
	char *path_lock;

	int (*write)(struct git_filebuf *file, void *source, size_t len);

	bool compute_digest;
	git_hash_ctx digest;

	unsigned char *buffer;
	unsigned char *z_buf;

	z_stream zs;
	int flush_mode;

	size_t buf_size, buf_pos;
	git_file fd;
	bool fd_is_open;
	bool do_not_buffer;
	int last_error;
};

typedef struct git_filebuf git_filebuf;

#define GIT_FILEBUF_INIT {0}

/*
 * The git_filebuf object lifecycle is:
 * - Allocate git_filebuf, preferably using GIT_FILEBUF_INIT.
 *
 * - Call git_filebuf_open() to initialize the filebuf for use.
 *
 * - Make as many calls to git_filebuf_write(), git_filebuf_printf(),
 *   git_filebuf_reserve() as you like. The error codes for these
 *   functions don't need to be checked. They are stored internally
 *   by the file buffer.
 *
 * - While you are writing, you may call git_filebuf_hash() to get
 *   the hash of all you have written so far. This function will
 *   fail if any of the previous writes to the buffer failed.
 *
 * - To close the git_filebuf, you may call git_filebuf_commit() or
 *   git_filebuf_commit_at() to save the file, or
 *   git_filebuf_cleanup() to abandon the file.  All of these will
 *   free the git_filebuf object. Likewise, all of these will fail
 *   if any of the previous writes to the buffer failed, and set
 *   an error code accordingly.
 */
int git_filebuf_write(git_filebuf *lock, const void *buff, size_t len);
int git_filebuf_reserve(git_filebuf *file, void **buff, size_t len);
int git_filebuf_printf(git_filebuf *file, const char *format, ...) GIT_FORMAT_PRINTF(2, 3);

int git_filebuf_open(git_filebuf *lock, const char *path, int flags, mode_t mode);
int git_filebuf_commit(git_filebuf *lock);
int git_filebuf_commit_at(git_filebuf *lock, const char *path);
void git_filebuf_cleanup(git_filebuf *lock);
int git_filebuf_hash(git_oid *oid, git_filebuf *file);
int git_filebuf_flush(git_filebuf *file);
int git_filebuf_stats(time_t *mtime, size_t *size, git_filebuf *file);

#endif
