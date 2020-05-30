/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#include "git2/common.h"

#ifndef GIT_WIN32

#include <limits.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>

char *p_realpath(const char *pathname, char *resolved)
{
	char *ret;
	if ((ret = realpath(pathname, resolved)) == NULL)
		return NULL;

#ifdef __OpenBSD__
	/* The OpenBSD realpath function behaves differently,
	 * figure out if the file exists */
	if (access(ret, F_OK) < 0)
		ret = NULL;
#endif
	return ret;
}

#endif
