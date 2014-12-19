/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#ifdef GIT_SSL
# include <openssl/err.h>
#endif

#include <git2.h>
#include "common.h"
#include "sysdir.h"
#include "cache.h"
#include "global.h"

void git_libgit2_version(int *major, int *minor, int *rev)
{
	*major = LIBGIT2_VER_MAJOR;
	*minor = LIBGIT2_VER_MINOR;
	*rev = LIBGIT2_VER_REVISION;
}

int git_libgit2_features()
{
	return 0
#ifdef GIT_THREADS
		| GIT_FEATURE_THREADS
#endif
#if defined(GIT_SSL) || defined(GIT_WINHTTP)
		| GIT_FEATURE_HTTPS
#endif
#if defined(GIT_SSH)
		| GIT_FEATURE_SSH
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
	case GIT_CONFIG_LEVEL_SYSTEM: val = GIT_SYSDIR_SYSTEM; break;
	case GIT_CONFIG_LEVEL_XDG:    val = GIT_SYSDIR_XDG; break;
	case GIT_CONFIG_LEVEL_GLOBAL: val = GIT_SYSDIR_GLOBAL; break;
	default:
		giterr_set(
			GITERR_INVALID, "Invalid config path selector %d", config_level);
	}

	return val;
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
#ifdef GIT_SSL
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
		giterr_set(GITERR_NET, "Cannot set certificate locations: OpenSSL is not enabled");
		error = -1;
#endif
		break;
	}

	va_end(ap);

	return error;
}

