/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_hashsig_h__
#define INCLUDE_hashsig_h__

#include "common.h"

/**
 * Similarity signature of line hashes for a buffer
 */
typedef struct git_hashsig git_hashsig;

typedef enum {
	GIT_HASHSIG_NORMAL = 0, /* use all data */
	GIT_HASHSIG_IGNORE_WHITESPACE = 1, /* ignore whitespace */
	GIT_HASHSIG_SMART_WHITESPACE = 2, /* ignore \r and all space after \n */
} git_hashsig_option_t;

/**
 * Build a similarity signature for a buffer
 *
 * If you have passed a whitespace-ignoring buffer, then the whitespace
 * will be removed from the buffer while it is being processed, modifying
 * the buffer in place.  Sorry about that!
 *
 * This will return an error if the buffer doesn't contain enough data to
 * compute a valid signature.
 *
 * @param out The array of hashed runs representing the file content
 * @param buf The contents of the file to hash
 * @param buflen The length of the data at `buf`
 * @param generate_pairwise_hashes Should pairwise runs be hashed
 */
extern int git_hashsig_create(
	git_hashsig **out,
	const char *buf,
	size_t buflen,
	git_hashsig_option_t opts);

/**
 * Build a similarity signature from a file
 *
 * This walks through the file, only loading a maximum of 4K of file data at
 * a time.  Otherwise, it acts just like `git_hashsig_create`.
 *
 * This will return an error if the file doesn't contain enough data to
 * compute a valid signature.
 */
extern int git_hashsig_create_fromfile(
	git_hashsig **out,
	const char *path,
	git_hashsig_option_t opts);

/**
 * Release memory for a content similarity signature
 */
extern void git_hashsig_free(git_hashsig *sig);

/**
 * Measure similarity between two files
 *
 * @return <0 for error, [0 to 100] as similarity score
 */
extern int git_hashsig_compare(
	const git_hashsig *a,
	const git_hashsig *b);

#endif
