/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"
#include "git2/types.h"
#include "git2/remote.h"
#include "git2/net.h"
#include "git2/transport.h"
#include "path.h"

typedef struct transport_definition {
	char *prefix;
	unsigned priority;
	git_transport_cb fn;
	void *param;
} transport_definition;

static git_smart_subtransport_definition http_subtransport_definition = { git_smart_subtransport_http, 1 };
static git_smart_subtransport_definition git_subtransport_definition = { git_smart_subtransport_git, 0 };
#ifdef GIT_SSH
static git_smart_subtransport_definition ssh_subtransport_definition = { git_smart_subtransport_ssh, 0 };
#endif

static transport_definition local_transport_definition = { "file://", 1, git_transport_local, NULL };
#ifdef GIT_SSH
static transport_definition ssh_transport_definition = { "ssh://", 1, git_transport_smart, &ssh_subtransport_definition };
#else
static transport_definition dummy_transport_definition = { NULL, 1, git_transport_dummy, NULL };
#endif

static transport_definition transports[] = {
	{"git://", 1, git_transport_smart, &git_subtransport_definition},
	{"http://", 1, git_transport_smart, &http_subtransport_definition},
	{"https://", 1, git_transport_smart, &http_subtransport_definition},
	{"file://", 1, git_transport_local, NULL},
#ifdef GIT_SSH
	{"ssh://", 1, git_transport_smart, &ssh_subtransport_definition},
#endif
	{NULL, 0, 0}
};

static git_vector additional_transports = GIT_VECTOR_INIT;

#define GIT_TRANSPORT_COUNT (sizeof(transports)/sizeof(transports[0])) - 1

static int transport_find_fn(const char *url, git_transport_cb *callback, void **param)
{
	size_t i = 0;
	unsigned priority = 0;
	transport_definition *definition = NULL, *definition_iter;

	// First, check to see if it's an obvious URL, which a URL scheme
	for (i = 0; i < GIT_TRANSPORT_COUNT; ++i) {
		definition_iter = &transports[i];

		if (strncasecmp(url, definition_iter->prefix, strlen(definition_iter->prefix)))
			continue;

		if (definition_iter->priority > priority)
			definition = definition_iter;
	}

	git_vector_foreach(&additional_transports, i, definition_iter) {
		if (strncasecmp(url, definition_iter->prefix, strlen(definition_iter->prefix)))
			continue;

		if (definition_iter->priority > priority)
			definition = definition_iter;
	}

#ifdef GIT_WIN32
	/* On Windows, it might not be possible to discern between absolute local
	 * and ssh paths - first check if this is a valid local path that points
	 * to a directory and if so assume local path, else assume SSH */

	/* Check to see if the path points to a file on the local file system */
	if (!definition && git_path_exists(url) && git_path_isdir(url))
		definition = &local_transport_definition;
#endif

	/* For other systems, perform the SSH check first, to avoid going to the
	 * filesystem if it is not necessary */

	/* It could be a SSH remote path. Check to see if there's a :
	 * SSH is an unsupported transport mechanism in this version of libgit2 */
	if (!definition && strrchr(url, ':'))
#ifdef GIT_SSH
		definition = &ssh_transport_definition;
#else
        definition = &dummy_transport_definition;
#endif

#ifndef GIT_WIN32
	/* Check to see if the path points to a file on the local file system */
	if (!definition && git_path_exists(url) && git_path_isdir(url))
		definition = &local_transport_definition;
#endif

	if (!definition)
		return -1;

	*callback = definition->fn;
	*param = definition->param;

	return 0;
}

/**************
 * Public API *
 **************/

int git_transport_dummy(git_transport **transport, git_remote *owner, void *param)
{
	GIT_UNUSED(transport);
	GIT_UNUSED(owner);
	GIT_UNUSED(param);
	giterr_set(GITERR_NET, "This transport isn't implemented. Sorry");
	return -1;
}

int git_transport_new(git_transport **out, git_remote *owner, const char *url)
{
	git_transport_cb fn;
	git_transport *transport;
	void *param;
	int error;

	if (transport_find_fn(url, &fn, &param) < 0) {
		giterr_set(GITERR_NET, "Unsupported URL protocol");
		return -1;
	}

	error = fn(&transport, owner, param);
	if (error < 0)
		return error;

	*out = transport;

	return 0;
}

int git_transport_register(
	const char *prefix,
	unsigned priority,
	git_transport_cb cb,
	void *param)
{
	transport_definition *d;

	d = git__calloc(sizeof(transport_definition), 1);
	GITERR_CHECK_ALLOC(d);

	d->prefix = git__strdup(prefix);

	if (!d->prefix)
		goto on_error;

	d->priority = priority;
	d->fn = cb;
	d->param = param;

	if (git_vector_insert(&additional_transports, d) < 0)
		goto on_error;

	return 0;

on_error:
	git__free(d->prefix);
	git__free(d);
	return -1;
}

int git_transport_unregister(
	const char *prefix,
	unsigned priority)
{
	transport_definition *d;
	unsigned i;

	git_vector_foreach(&additional_transports, i, d) {
		if (d->priority == priority && !strcasecmp(d->prefix, prefix)) {
			if (git_vector_remove(&additional_transports, i) < 0)
				return -1;

			git__free(d->prefix);
			git__free(d);

			if (!additional_transports.length)
				git_vector_free(&additional_transports);

			return 0;
		}
	}

	return GIT_ENOTFOUND;
}

/* from remote.h */
int git_remote_valid_url(const char *url)
{
	git_transport_cb fn;
	void *param;

	return !transport_find_fn(url, &fn, &param);
}

int git_remote_supported_url(const char* url)
{
	git_transport_cb fn;
	void *param;

	if (transport_find_fn(url, &fn, &param) < 0)
		return 0;

	return fn != &git_transport_dummy;
}

int git_transport_init(git_transport *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_transport, GIT_TRANSPORT_INIT);
	return 0;
}
