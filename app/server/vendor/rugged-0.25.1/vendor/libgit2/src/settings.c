/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifdef GIT_OPENSSL
# include <openssl/err.h>
#endif

#include <git2.h>
#include "common.h"
#include "sysdir.h"
#include "cache.h"
#include "global.h"
#include "object.h"

void git_libgit2_version(int *major, int *minor, int *rev)
{
	*major = LIBGIT2_VER_MAJOR;
	*minor = LIBGIT2_VER_MINOR;
	*rev = LIBGIT2_VER_REVISION;
}

int git_libgit2_features(void)
{
	return 0
#ifdef GIT_THREADS
		| GIT_FEATURE_THREADS
#endif
#if defined(GIT_OPENSSL) || defined(GIT_WINHTTP) || defined(GIT_SECURE_TRANSPORT)
		| GIT_FEATURE_HTTPS
#endif
#if defined(GIT_SSH)
		| GIT_FEATURE_SSH
#endif
#if defined(GIT_USE_NSEC)
		| GIT_FEATURE_NSEC
#endif
	;
}

/* Declarations for tuneable settings */
extern size_t git_mwindow__window_size;
extern size_t git_mwindow__mapped_limit;

static int config_level_to_sysdir(int config_level)
{
	int val = -1;

	switch (config_level) {
	case GIT_CONFIG_LEVEL_SYSTEM:
		val = GIT_SYSDIR_SYSTEM;
		break;
	case GIT_CONFIG_LEVEL_XDG:
		val = GIT_SYSDIR_XDG;
		break;
	case GIT_CONFIG_LEVEL_GLOBAL:
		val = GIT_SYSDIR_GLOBAL;
		break;
	case GIT_CONFIG_LEVEL_PROGRAMDATA:
		val = GIT_SYSDIR_PROGRAMDATA;
		break;
	default:
		giterr_set(
			GITERR_INVALID, "Invalid config path selector %d", config_level);
	}

	return val;
}

extern char *git__user_agent;
extern char *git__ssl_ciphers;

const char *git_libgit2__user_agent(void)
{
	return git__user_agent;
}

const char *git_libgit2__ssl_ciphers(void)
{
	return git__ssl_ciphers;
}

int git_libgit2_opts(int key, ...)
{
	int error = 0;
	va_list ap;

	va_start(ap, key);

	switch (key) {
	case GIT_OPT_SET_MWINDOW_SIZE:
		git_mwindow__window_size = va_arg(ap, size_t);
		break;

	case GIT_OPT_GET_MWINDOW_SIZE:
		*(va_arg(ap, size_t *)) = git_mwindow__window_size;
		break;

	case GIT_OPT_SET_MWINDOW_MAPPED_LIMIT:
		git_mwindow__mapped_limit = va_arg(ap, size_t);
		break;

	case GIT_OPT_GET_MWINDOW_MAPPED_LIMIT:
		*(va_arg(ap, size_t *)) = git_mwindow__mapped_limit;
		break;

	case GIT_OPT_GET_SEARCH_PATH:
		if ((error = config_level_to_sysdir(va_arg(ap, int))) >= 0) {
			git_buf *out = va_arg(ap, git_buf *);
			const git_buf *tmp;

			git_buf_sanitize(out);
			if ((error = git_sysdir_get(&tmp, error)) < 0)
				break;

			error = git_buf_sets(out, tmp->ptr);
		}
		break;

	case GIT_OPT_SET_SEARCH_PATH:
		if ((error = config_level_to_sysdir(va_arg(ap, int))) >= 0)
			error = git_sysdir_set(error, va_arg(ap, const char *));
		break;

	case GIT_OPT_SET_CACHE_OBJECT_LIMIT:
		{
			git_otype type = (git_otype)va_arg(ap, int);
			size_t size = va_arg(ap, size_t);
			error = git_cache_set_max_object_size(type, size);
			break;
		}

	case GIT_OPT_SET_CACHE_MAX_SIZE:
		git_cache__max_storage = va_arg(ap, ssize_t);
		break;

	case GIT_OPT_ENABLE_CACHING:
		git_cache__enabled = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_GET_CACHED_MEMORY:
		*(va_arg(ap, ssize_t *)) = git_cache__current_storage.val;
		*(va_arg(ap, ssize_t *)) = git_cache__max_storage;
		break;

	case GIT_OPT_GET_TEMPLATE_PATH:
		{
			git_buf *out = va_arg(ap, git_buf *);
			const git_buf *tmp;

			git_buf_sanitize(out);
			if ((error = git_sysdir_get(&tmp, GIT_SYSDIR_TEMPLATE)) < 0)
				break;

			error = git_buf_sets(out, tmp->ptr);
		}
		break;

	case GIT_OPT_SET_TEMPLATE_PATH:
		error = git_sysdir_set(GIT_SYSDIR_TEMPLATE, va_arg(ap, const char *));
		break;

	case GIT_OPT_SET_SSL_CERT_LOCATIONS:
#ifdef GIT_OPENSSL
		{
			const char *file = va_arg(ap, const char *);
			const char *path = va_arg(ap, const char *);
			if (!SSL_CTX_load_verify_locations(git__ssl_ctx, file, path)) {
				giterr_set(GITERR_NET, "SSL error: %s",
					ERR_error_string(ERR_get_error(), NULL));
				error = -1;
			}
		}
#else
		giterr_set(GITERR_NET, "cannot set certificate locations: OpenSSL is not enabled");
		error = -1;
#endif
		break;
	case GIT_OPT_SET_USER_AGENT:
		git__free(git__user_agent);
		git__user_agent = git__strdup(va_arg(ap, const char *));
		if (!git__user_agent) {
			giterr_set_oom();
			error = -1;
		}

		break;

	case GIT_OPT_ENABLE_STRICT_OBJECT_CREATION:
		git_object__strict_input_validation = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_SET_SSL_CIPHERS:
#ifdef GIT_OPENSSL
		{
			git__free(git__ssl_ciphers);
			git__ssl_ciphers = git__strdup(va_arg(ap, const char *));
			if (!git__ssl_ciphers) {
				giterr_set_oom();
				error = -1;
			}
		}
#else
		giterr_set(GITERR_NET, "cannot set custom ciphers: OpenSSL is not enabled");
		error = -1;
#endif
		break;

	case GIT_OPT_GET_USER_AGENT:
		{
			git_buf *out = va_arg(ap, git_buf *);
			git_buf_sanitize(out);
			error = git_buf_sets(out, git__user_agent);
		}
		break;

	default:
		giterr_set(GITERR_INVALID, "invalid option key");
		error = -1;
	}

	va_end(ap);

	return error;
}

