/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "libgit2.h"

#include <git2.h>
#include "alloc.h"
#include "buf.h"
#include "cache.h"
#include "common.h"
#include "filter.h"
#include "grafts.h"
#include "hash.h"
#include "index.h"
#include "merge_driver.h"
#include "pool.h"
#include "mwindow.h"
#include "object.h"
#include "odb.h"
#include "rand.h"
#include "refs.h"
#include "runtime.h"
#include "sysdir.h"
#include "thread.h"
#include "threadstate.h"
#include "git2/global.h"
#include "streams/registry.h"
#include "streams/mbedtls.h"
#include "streams/openssl.h"
#include "streams/socket.h"
#include "transports/smart.h"
#include "transports/http.h"
#include "transports/ssh.h"

#ifdef GIT_WIN32
# include "win32/w32_leakcheck.h"
#endif

/* Declarations for tuneable settings */
extern size_t git_mwindow__window_size;
extern size_t git_mwindow__mapped_limit;
extern size_t git_mwindow__file_limit;
extern size_t git_indexer__max_objects;
extern bool git_disable_pack_keep_file_checks;
extern int git_odb__packed_priority;
extern int git_odb__loose_priority;
extern int git_socket_stream__connect_timeout;
extern int git_socket_stream__timeout;

char *git__user_agent;
char *git__ssl_ciphers;

static void libgit2_settings_global_shutdown(void)
{
	git__free(git__user_agent);
	git__free(git__ssl_ciphers);
	git_repository__free_extensions();
}

static int git_libgit2_settings_global_init(void)
{
	return git_runtime_shutdown_register(libgit2_settings_global_shutdown);
}

int git_libgit2_init(void)
{
	static git_runtime_init_fn init_fns[] = {
#ifdef GIT_WIN32
		git_win32_leakcheck_global_init,
#endif
		git_allocator_global_init,
		git_threadstate_global_init,
		git_threads_global_init,
		git_rand_global_init,
		git_hash_global_init,
		git_sysdir_global_init,
		git_filter_global_init,
		git_merge_driver_global_init,
		git_transport_ssh_global_init,
		git_stream_registry_global_init,
		git_socket_stream_global_init,
		git_openssl_stream_global_init,
		git_mbedtls_stream_global_init,
		git_mwindow_global_init,
		git_pool_global_init,
		git_libgit2_settings_global_init
	};

	return git_runtime_init(init_fns, ARRAY_SIZE(init_fns));
}

int git_libgit2_init_count(void)
{
	return git_runtime_init_count();
}

int git_libgit2_shutdown(void)
{
	return git_runtime_shutdown();
}

int git_libgit2_version(int *major, int *minor, int *rev)
{
	*major = LIBGIT2_VER_MAJOR;
	*minor = LIBGIT2_VER_MINOR;
	*rev = LIBGIT2_VER_REVISION;

	return 0;
}

const char *git_libgit2_prerelease(void)
{
	return LIBGIT2_VER_PRERELEASE;
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

static int config_level_to_sysdir(int *out, int config_level)
{
	switch (config_level) {
	case GIT_CONFIG_LEVEL_SYSTEM:
		*out = GIT_SYSDIR_SYSTEM;
		return 0;
	case GIT_CONFIG_LEVEL_XDG:
		*out = GIT_SYSDIR_XDG;
		return 0;
	case GIT_CONFIG_LEVEL_GLOBAL:
		*out = GIT_SYSDIR_GLOBAL;
		return 0;
	case GIT_CONFIG_LEVEL_PROGRAMDATA:
		*out = GIT_SYSDIR_PROGRAMDATA;
		return 0;
	default:
		break;
	}

	git_error_set(
		GIT_ERROR_INVALID, "invalid config path selector %d", config_level);
	return -1;
}

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

	case GIT_OPT_SET_MWINDOW_FILE_LIMIT:
		git_mwindow__file_limit = va_arg(ap, size_t);
		break;

	case GIT_OPT_GET_MWINDOW_FILE_LIMIT:
		*(va_arg(ap, size_t *)) = git_mwindow__file_limit;
		break;

	case GIT_OPT_GET_SEARCH_PATH:
		{
			int sysdir = va_arg(ap, int);
			git_buf *out = va_arg(ap, git_buf *);
			git_str str = GIT_STR_INIT;
			const git_str *tmp;
			int level;

			if ((error = git_buf_tostr(&str, out)) < 0 ||
			    (error = config_level_to_sysdir(&level, sysdir)) < 0 ||
			    (error = git_sysdir_get(&tmp, level)) < 0 ||
			    (error = git_str_put(&str, tmp->ptr, tmp->size)) < 0)
				break;

			error = git_buf_fromstr(out, &str);
		}
		break;

	case GIT_OPT_SET_SEARCH_PATH:
		{
			int level;

			if ((error = config_level_to_sysdir(&level, va_arg(ap, int))) >= 0)
				error = git_sysdir_set(level, va_arg(ap, const char *));
		}
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
			git_str str = GIT_STR_INIT;
			const git_str *tmp;

			if ((error = git_buf_tostr(&str, out)) < 0 ||
			    (error = git_sysdir_get(&tmp, GIT_SYSDIR_TEMPLATE)) < 0 ||
			    (error = git_str_put(&str, tmp->ptr, tmp->size)) < 0)
				break;

			error = git_buf_fromstr(out, &str);
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
			error = git_mbedtls__set_cert_location(file, path);
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
			git_str str = GIT_STR_INIT;

			if ((error = git_buf_tostr(&str, out)) < 0 ||
			    (error = git_str_puts(&str, git__user_agent)) < 0)
				break;

			error = git_buf_fromstr(out, &str);
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

	case GIT_OPT_ENABLE_HTTP_EXPECT_CONTINUE:
		git_http__expect_continue = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_SET_ODB_PACKED_PRIORITY:
		git_odb__packed_priority = va_arg(ap, int);
		break;

	case GIT_OPT_SET_ODB_LOOSE_PRIORITY:
		git_odb__loose_priority = va_arg(ap, int);
		break;

	case GIT_OPT_SET_EXTENSIONS:
		{
			const char **extensions = va_arg(ap, const char **);
			size_t len = va_arg(ap, size_t);
			error = git_repository__set_extensions(extensions, len);
		}
		break;

	case GIT_OPT_GET_EXTENSIONS:
		{
			git_strarray *out = va_arg(ap, git_strarray *);
			char **extensions;
			size_t len;

			if ((error = git_repository__extensions(&extensions, &len)) < 0)
				break;

			out->strings = extensions;
			out->count = len;
		}
		break;

	case GIT_OPT_GET_OWNER_VALIDATION:
		*(va_arg(ap, int *)) = git_repository__validate_ownership;
		break;

	case GIT_OPT_SET_OWNER_VALIDATION:
		git_repository__validate_ownership = (va_arg(ap, int) != 0);
		break;

	case GIT_OPT_GET_HOMEDIR:
		{
			git_buf *out = va_arg(ap, git_buf *);
			git_str str = GIT_STR_INIT;
			const git_str *tmp;

			if ((error = git_buf_tostr(&str, out)) < 0 ||
			    (error = git_sysdir_get(&tmp, GIT_SYSDIR_HOME)) < 0 ||
			    (error = git_str_put(&str, tmp->ptr, tmp->size)) < 0)
				break;

			error = git_buf_fromstr(out, &str);
		}
		break;

	case GIT_OPT_SET_HOMEDIR:
		error = git_sysdir_set(GIT_SYSDIR_HOME, va_arg(ap, const char *));
		break;

	case GIT_OPT_GET_SERVER_CONNECT_TIMEOUT:
		*(va_arg(ap, int *)) = git_socket_stream__connect_timeout;
		break;

	case GIT_OPT_SET_SERVER_CONNECT_TIMEOUT:
		{
			int timeout = va_arg(ap, int);

			if (timeout < 0) {
				git_error_set(GIT_ERROR_INVALID, "invalid connect timeout");
				error = -1;
			} else {
				git_socket_stream__connect_timeout = timeout;
			}
		}
		break;

	case GIT_OPT_GET_SERVER_TIMEOUT:
		*(va_arg(ap, int *)) = git_socket_stream__timeout;
		break;

	case GIT_OPT_SET_SERVER_TIMEOUT:
		{
			int timeout = va_arg(ap, int);

			if (timeout < 0) {
				git_error_set(GIT_ERROR_INVALID, "invalid timeout");
				error = -1;
			} else {
				git_socket_stream__timeout = timeout;
			}
		}
		break;

	default:
		git_error_set(GIT_ERROR_INVALID, "invalid option key");
		error = -1;
	}

	va_end(ap);

	return error;
}
