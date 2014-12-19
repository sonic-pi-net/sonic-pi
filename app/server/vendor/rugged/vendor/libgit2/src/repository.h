/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_repository_h__
#define INCLUDE_repository_h__

#include "git2/common.h"
#include "git2/oid.h"
#include "git2/odb.h"
#include "git2/repository.h"
#include "git2/object.h"
#include "git2/config.h"

#include "cache.h"
#include "refs.h"
#include "buffer.h"
#include "object.h"
#include "attrcache.h"
#include "submodule.h"
#include "diff_driver.h"

#define DOT_GIT ".git"
#define GIT_DIR DOT_GIT "/"
#define GIT_DIR_MODE 0755
#define GIT_BARE_DIR_MODE 0777

/** Cvar cache identifiers */
typedef enum {
	GIT_CVAR_AUTO_CRLF = 0, /* core.autocrlf */
	GIT_CVAR_EOL,           /* core.eol */
	GIT_CVAR_SYMLINKS,      /* core.symlinks */
	GIT_CVAR_IGNORECASE,    /* core.ignorecase */
	GIT_CVAR_FILEMODE,      /* core.filemode */
	GIT_CVAR_IGNORESTAT,    /* core.ignorestat */
	GIT_CVAR_TRUSTCTIME,    /* core.trustctime */
	GIT_CVAR_ABBREV,        /* core.abbrev */
	GIT_CVAR_PRECOMPOSE,    /* core.precomposeunicode */
	GIT_CVAR_SAFE_CRLF,		/* core.safecrlf */
	GIT_CVAR_LOGALLREFUPDATES, /* core.logallrefupdates */
	GIT_CVAR_PROTECTHFS,    /* core.protectHFS */
	GIT_CVAR_PROTECTNTFS,   /* core.protectNTFS */
	GIT_CVAR_CACHE_MAX
} git_cvar_cached;

/**
 * CVAR value enumerations
 *
 * These are the values that are actually stored in the cvar cache, instead
 * of their string equivalents. These values are internal and symbolic;
 * make sure that none of them is set to `-1`, since that is the unique
 * identifier for "not cached"
 */
typedef enum {
	/* The value hasn't been loaded from the cache yet */
	GIT_CVAR_NOT_CACHED = -1,

	/* core.safecrlf: false, 'fail', 'warn' */
	GIT_SAFE_CRLF_FALSE = 0,
	GIT_SAFE_CRLF_FAIL = 1,
	GIT_SAFE_CRLF_WARN = 2,

	/* core.autocrlf: false, true, 'input; */
	GIT_AUTO_CRLF_FALSE = 0,
	GIT_AUTO_CRLF_TRUE = 1,
	GIT_AUTO_CRLF_INPUT = 2,
	GIT_AUTO_CRLF_DEFAULT = GIT_AUTO_CRLF_FALSE,

	/* core.eol: unset, 'crlf', 'lf', 'native' */
	GIT_EOL_UNSET = 0,
	GIT_EOL_CRLF = 1,
	GIT_EOL_LF = 2,
#ifdef GIT_WIN32
	GIT_EOL_NATIVE = GIT_EOL_CRLF,
#else
	GIT_EOL_NATIVE = GIT_EOL_LF,
#endif
	GIT_EOL_DEFAULT = GIT_EOL_NATIVE,

	/* core.symlinks: bool */
	GIT_SYMLINKS_DEFAULT = GIT_CVAR_TRUE,
	/* core.ignorecase */
	GIT_IGNORECASE_DEFAULT = GIT_CVAR_FALSE,
	/* core.filemode */
	GIT_FILEMODE_DEFAULT = GIT_CVAR_TRUE,
	/* core.ignorestat */
	GIT_IGNORESTAT_DEFAULT = GIT_CVAR_FALSE,
	/* core.trustctime */
	GIT_TRUSTCTIME_DEFAULT = GIT_CVAR_TRUE,
	/* core.abbrev */
	GIT_ABBREV_DEFAULT = 7,
	/* core.precomposeunicode */
	GIT_PRECOMPOSE_DEFAULT = GIT_CVAR_FALSE,
	/* core.safecrlf */
	GIT_SAFE_CRLF_DEFAULT = GIT_CVAR_FALSE,
	/* core.logallrefupdates */
	GIT_LOGALLREFUPDATES_UNSET = 2,
	GIT_LOGALLREFUPDATES_DEFAULT = GIT_LOGALLREFUPDATES_UNSET,
	/* core.protectHFS */
	GIT_PROTECTHFS_DEFAULT = GIT_CVAR_FALSE,
	/* core.protectNTFS */
	GIT_PROTECTNTFS_DEFAULT = GIT_CVAR_FALSE,
} git_cvar_value;

/* internal repository init flags */
enum {
	GIT_REPOSITORY_INIT__HAS_DOTGIT = (1u << 16),
	GIT_REPOSITORY_INIT__NATURAL_WD = (1u << 17),
	GIT_REPOSITORY_INIT__IS_REINIT  = (1u << 18),
};

/** Internal structure for repository object */
struct git_repository {
	git_odb *_odb;
	git_refdb *_refdb;
	git_config *_config;
	git_index *_index;
	git_submodule_cache *_submodules;

	git_cache objects;
	git_attr_cache *attrcache;
	git_diff_driver_registry *diff_drivers;

	char *path_repository;
	char *workdir;
	char *namespace;
	char *name_8dot3;

	unsigned is_bare:1,
		has_8dot3:1,
		has_8dot3_default:1;
	unsigned int lru_counter;

	git_cvar_value cvar_cache[GIT_CVAR_CACHE_MAX];
};

GIT_INLINE(git_attr_cache *) git_repository_attr_cache(git_repository *repo)
{
	return repo->attrcache;
}

int git_repository_head_tree(git_tree **tree, git_repository *repo);

/*
 * Weak pointers to repository internals.
 *
 * The returned pointers do not need to be freed. Do not keep
 * permanent references to these (i.e. between API calls), since they may
 * become invalidated if the user replaces a repository internal.
 */
int git_repository_config__weakptr(git_config **out, git_repository *repo);
int git_repository_odb__weakptr(git_odb **out, git_repository *repo);
int git_repository_refdb__weakptr(git_refdb **out, git_repository *repo);
int git_repository_index__weakptr(git_index **out, git_repository *repo);

/*
 * CVAR cache
 *
 * Efficient access to the most used config variables of a repository.
 * The cache is cleared every time the config backend is replaced.
 */
int git_repository__cvar(int *out, git_repository *repo, git_cvar_cached cvar);
void git_repository__cvar_cache_clear(git_repository *repo);

GIT_INLINE(int) git_repository__ensure_not_bare(
	git_repository *repo,
	const char *operation_name)
{
	if (!git_repository_is_bare(repo))
		return 0;

	giterr_set(
		GITERR_REPOSITORY,
		"Cannot %s. This operation is not allowed against bare repositories.",
		operation_name);

	return GIT_EBAREREPO;
}

int git_repository__set_orig_head(git_repository *repo, const git_oid *orig_head);

int git_repository__cleanup_files(git_repository *repo, const char *files[], size_t files_len);

/*
 * Gets the DOS-compatible 8.3 "short name".  This will return only the
 * short name for the repository directory (ie, "git~1" for ".git").  This
 * will always return a pointer to `git_repository__8dot3_default` when
 * "GIT~1" is the short name.  This will return NULL for bare repositories,
 * and systems that do not have a short name.
 */
const char *git_repository__8dot3_name(git_repository *repo);

/* The default DOS-compatible 8.3 "short name" for a git repository,
 * "GIT~1".
 */
extern const char *git_repository__8dot3_default;
extern size_t git_repository__8dot3_default_len;

#endif
