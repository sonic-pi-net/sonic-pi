/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_oid_h__
#define INCLUDE_oid_h__

#include "git2/oid.h"

/**
 * Format a git_oid into a newly allocated c-string.
 *
 * The c-string is owned by the caller and needs to be manually freed.
 *
 * @param id the oid structure to format
 * @return the c-string; NULL if memory is exhausted. Caller must
 *			deallocate the string with git__free().
 */
char *git_oid_allocfmt(const git_oid *id);

GIT_INLINE(int) git_oid__hashcmp(const unsigned char *sha1, const unsigned char *sha2)
{
	int i;

	for (i = 0; i < GIT_OID_RAWSZ; i++, sha1++, sha2++) {
		if (*sha1 != *sha2)
			return *sha1 - *sha2;
	}

	return 0;
}

/*
 * Compare two oid structures.
 *
 * @param a first oid structure.
 * @param b second oid structure.
 * @return <0, 0, >0 if a < b, a == b, a > b.
 */
GIT_INLINE(int) git_oid__cmp(const git_oid *a, const git_oid *b)
{
	return git_oid__hashcmp(a->id, b->id);
}

#endif
