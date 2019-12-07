/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_attrcache_h__
#define INCLUDE_attrcache_h__

#include "attr_file.h"
#include "strmap.h"

#define GIT_ATTR_CONFIG       "core.attributesfile"
#define GIT_IGNORE_CONFIG     "core.excludesfile"

typedef struct {
	char *cfg_attr_file; /* cached value of core.attributesfile */
	char *cfg_excl_file; /* cached value of core.excludesfile */
	git_strmap *files;	 /* hash path to git_attr_cache_entry records */
	git_strmap *macros;	 /* hash name to vector<git_attr_assignment> */
	git_mutex lock;
	git_pool  pool;
} git_attr_cache;

extern int git_attr_cache__do_init(git_repository *repo);

#define git_attr_cache__init(REPO) \
	(git_repository_attr_cache(REPO) ? 0 : git_attr_cache__do_init(REPO))

/* get file - loading and reload as needed */
extern int git_attr_cache__get(
	git_attr_file **file,
	git_repository *repo,
	git_attr_session *attr_session,
	git_attr_file_source source,
	const char *base,
	const char *filename,
	git_attr_file_parser parser);

extern bool git_attr_cache__is_cached(
	git_repository *repo,
	git_attr_file_source source,
	const char *path);

extern int git_attr_cache__alloc_file_entry(
	git_attr_file_entry **out,
	const char *base,
	const char *path,
	git_pool *pool);

extern int git_attr_cache__insert_macro(
	git_repository *repo, git_attr_rule *macro);

extern git_attr_rule *git_attr_cache__lookup_macro(
	git_repository *repo, const char *name);

#endif
