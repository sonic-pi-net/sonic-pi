/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_config_h__
#define INCLUDE_config_h__

#include "git2.h"
#include "git2/config.h"
#include "vector.h"
#include "repository.h"

#define GIT_CONFIG_FILENAME_PROGRAMDATA "config"
#define GIT_CONFIG_FILENAME_SYSTEM "gitconfig"
#define GIT_CONFIG_FILENAME_GLOBAL ".gitconfig"
#define GIT_CONFIG_FILENAME_XDG    "config"

#define GIT_CONFIG_FILENAME_INREPO "config"
#define GIT_CONFIG_FILE_MODE 0666

struct git_config {
	git_refcount rc;
	git_vector files;
};

extern int git_config__global_location(git_buf *buf);

extern int git_config_rename_section(
	git_repository *repo,
	const char *old_section_name,	/* eg "branch.dummy" */
	const char *new_section_name);	/* NULL to drop the old section */

/**
 * Create a configuration file backend for ondisk files
 *
 * These are the normal `.gitconfig` files that Core Git
 * processes. Note that you first have to add this file to a
 * configuration object before you can query it for configuration
 * variables.
 *
 * @param out the new backend
 * @param path where the config file is located
 */
extern int git_config_file__ondisk(git_config_backend **out, const char *path);

extern int git_config__normalize_name(const char *in, char **out);

/* internal only: does not normalize key and sets out to NULL if not found */
extern int git_config__lookup_entry(
	git_config_entry **out,
	const git_config *cfg,
	const char *key,
	bool no_errors);

/* internal only: update and/or delete entry string with constraints */
extern int git_config__update_entry(
	git_config *cfg,
	const char *key,
	const char *value,
	bool overwrite_existing,
	bool only_if_existing);

/*
 * Lookup functions that cannot fail.  These functions look up a config
 * value and return a fallback value if the value is missing or if any
 * failures occur while trying to access the value.
 */

extern char *git_config__get_string_force(
	const git_config *cfg, const char *key, const char *fallback_value);

extern int git_config__get_bool_force(
	const git_config *cfg, const char *key, int fallback_value);

extern int git_config__get_int_force(
	const git_config *cfg, const char *key, int fallback_value);

/* API for repository cvar-style lookups from config - not cached, but
 * uses cvar value maps and fallbacks
 */
extern int git_config__cvar(
	int *out, git_config *config, git_cvar_cached cvar);

/**
 * The opposite of git_config_lookup_map_value, we take an enum value
 * and map it to the string or bool value on the config.
 */
int git_config_lookup_map_enum(git_cvar_t *type_out, const char **str_out,
			       const git_cvar_map *maps, size_t map_n, int enum_val);

/**
 * Unlock the backend with the highest priority
 *
 * Unlocking will allow other writers to updat the configuration
 * file. Optionally, any changes performed since the lock will be
 * applied to the configuration.
 *
 * @param cfg the configuration
 * @param commit boolean which indicates whether to commit any changes
 * done since locking
 * @return 0 or an error code
 */
GIT_EXTERN(int) git_config_unlock(git_config *cfg, int commit);

#endif
