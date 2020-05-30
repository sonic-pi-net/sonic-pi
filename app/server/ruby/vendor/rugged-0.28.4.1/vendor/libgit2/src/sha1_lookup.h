/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_sha1_lookup_h__
#define INCLUDE_sha1_lookup_h__

#include "common.h"

#include <stdlib.h>

int sha1_position(const void *table,
			size_t stride,
			unsigned lo, unsigned hi,
			const unsigned char *key);

#endif
