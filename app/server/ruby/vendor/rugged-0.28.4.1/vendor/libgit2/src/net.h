/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_net_h__
#define INCLUDE_net_h__

#include "common.h"

typedef struct git_net_url {
	char *scheme;
	char *host;
	char *port;
	char *path;
	char *query;
	char *username;
	char *password;
} git_net_url;

#define GIT_NET_URL_INIT { NULL }

/** Parses a string containing a URL into a structure.  */
int git_net_url_parse(git_net_url *url, const char *str);

/** Returns nonzero if the URL is on the default port. */
int git_net_url_is_default_port(git_net_url *url);

/** Swaps the contents of one URL for another.  */
void git_net_url_swap(git_net_url *a, git_net_url *b);

/** Disposes the contents of the structure. */
void git_net_url_dispose(git_net_url *url);

#endif
