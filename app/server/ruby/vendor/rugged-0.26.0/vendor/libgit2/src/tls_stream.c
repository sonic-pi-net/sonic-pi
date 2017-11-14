/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "tls_stream.h"

#include "git2/errors.h"

#include "openssl_stream.h"
#include "stransport_stream.h"

static git_stream_cb tls_ctor;

int git_stream_register_tls(git_stream_cb ctor)
{
	tls_ctor = ctor;

	return 0;
}

int git_tls_stream_new(git_stream **out, const char *host, const char *port)
{

	if (tls_ctor)
		return tls_ctor(out, host, port);

#ifdef GIT_SECURE_TRANSPORT
	return git_stransport_stream_new(out, host, port);
#elif defined(GIT_OPENSSL)
	return git_openssl_stream_new(out, host, port);
#else
	GIT_UNUSED(out);
	GIT_UNUSED(host);
	GIT_UNUSED(port);

	giterr_set(GITERR_SSL, "there is no TLS stream available");
	return -1;
#endif
}
