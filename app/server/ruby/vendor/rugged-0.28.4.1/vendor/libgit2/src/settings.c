/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "common.h"

#ifdef GIT_OPENSSL
# include <openssl/err.h>
#endif

#ifdef GIT_MBEDTLS
# include <mbedtls/error.h>
#endif

#include <git2.h>
#include "alloc.h"
#include "sysdir.h"
#include "cache.h"
#include "global.h"
#include "object.h"
#include "odb.h"
#include "refs.h"
#include "index.h"
#include "transports/smart.h"
#include "streams/openssl.h"
#include "streams/mbedtls.h"

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
#ifdef GIT_HTTPS
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
extern size_t git_indexer__max_objects;
extern bool git_disable_pack_keep_file_checks;

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
		git_error_set(
			GIT_ERROR_INVALID, "invalid config path selector %d", config_level);
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
			git_object_t type = (git_object_t)va_arg(ap, int);
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
			error = git_openssl__set_cert_location(file, path);
		}
#elif defined(GIT_MBEDTLS)
		{
			const char *file = va_arg(ap, const char *);
			const char *path = va_arg(ap, const char *);
			if (file)
				error = git_mbedtls__set_cert_location(file, 0);
			if (error && path)
				error = git_mbedtls__set_cert_location(path, 1);
		}
#else
		git_error_set(GIT_ERROR_SSL, "TLS backend doesn't support certificate locations");
		error = -1;
#endif
		break;
	case GIT_OPT_SET_USER_AGENT:
		git__free(git__user_agent);
		git__user_agent = git__strdup(va_arg(ap, const char *));
		if (!git__user_agent) {
			git_error_set_oom();
			error = -1;
		}

		break;

	case GIT_OPT_ENABLE_STRICT_OBJECT_CREATION:
		git_object__strict_input_validation = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_ENABLE_STRICT_SYMBOLIC_REF_CREATION:
		git_reference__enable_symbolic_ref_target_validation = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_SET_SSL_CIPHERS:
#if (GIT_OPENSSL || GIT_MBEDTLS)
		{
			git__free(git__ssl_ciphers);
			git__ssl_ciphers = git__strdup(va_arg(ap, const char *));
			if (!git__ssl_ciphers) {
				git_error_set_oom();
				error = -1;
			}
		}
#else
		git_error_set(GIT_ERROR_SSL, "TLS backend doesn't support custom ciphers");
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

	case GIT_OPT_ENABLE_OFS_DELTA:
		git_smart__ofs_delta_enabled = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_ENABLE_FSYNC_GITDIR:
		git_repository__fsync_gitdir = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_GET_WINDOWS_SHAREMODE:
#ifdef GIT_WIN32
		*(va_arg(ap, unsigned long *)) = git_win32__createfile_sharemode;
#endif
		break;

	case GIT_OPT_SET_WINDOWS_SHAREMODE:
#ifdef GIT_WIN32
		git_win32__createfile_sharemode = va_arg(ap, unsigned long);
#endif
		break;

	case GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION:
		git_odb__strict_hash_verification = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_SET_ALLOCATOR:
		error = git_allocator_setup(va_arg(ap, git_allocator *));
		break;

	case GIT_OPT_ENABLE_UNSAVED_INDEX_SAFETY:
		git_index__enforce_unsaved_safety = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_SET_PACK_MAX_OBJECTS:
		git_indexer__max_objects = va_arg(ap, size_t);
		break;

	case GIT_OPT_GET_PACK_MAX_OBJECTS:
		*(va_arg(ap, size_t *)) = git_indexer__max_objects;
		break;

	case GIT_OPT_DISABLE_PACK_KEEP_FILE_CHECKS:
		git_disable_pack_keep_file_checks = (va_arg(ap, int) != 0);
		break;

	default:
		git_error_set(GIT_ERROR_INVALID, "invalid option key");
		error = -1;
	}

	va_end(ap);

	return error;
}
