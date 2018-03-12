/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_repository_h__
#define INCLUDE_repository_h__

#include "common.h"

#include "git2/common.h"
#include "git2/oid.h"
#include "git2/odb.h"
#include "git2/repository.h"
#include "git2/object.h"
#include "git2/config.h"

#include "array.h"
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

/* Default DOS-compatible 8.3 "short name" for a git repository, "GIT~1" */
#define GIT_DIR_SHORTNAME "GIT~1"

extern bool git_repository__fsync_gitdir;

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
	GIT_CVAR_FSYNCOBJECTFILES, /* core.fsyncObjectFiles */
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
	/* core.fsyncObjectFiles */
	GIT_FSYNCOBJECTFILES_DEFAULT = GIT_CVAR_FALSE,
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

	git_cache objects;
	git_attr_cache *attrcache;
	git_diff_driver_registry *diff_drivers;

	char *gitlink;
	char *gitdir;
	char *commondir;
	char *workdir;
	char *namespace;

	char *ident_name;
	char *ident_email;

	git_array_t(git_buf) reserved_names;

	unsigned is_bare:1;
	unsigned is_worktree:1;

	unsigned int lru_counter;

	git_atomic attr_session_key;

	git_cvar_value cvar_cache[GIT_CVAR_CACHE_MAX];
	git_strmap *submodule_cache;
};

GIT_INLINE(git_attr_cache *) git_repository_attr_cache(git_repository *repo)
{
	return repo->attrcache;
}

int git_repository_head_tree(git_tree **tree, git_repository *repo);
int git_repository_create_head(const char *git_dir, const char *ref_name);

/*
 * Called for each HEAD.
 *
 * Can return either 0, causing the iteration over HEADs to
 * continue, or a non-0 value causing the iteration to abort. The
 * return value is passed back to the caller of
 * `git_repository_foreach_head`
 */
typedef int (*git_repository_foreach_head_cb)(git_repository *repo, const char *path, void *payload);

/*
 * Iterate over repository and all worktree HEADs.
 *
 * This function will be called for the repository HEAD and for
 * all HEADS of linked worktrees. For each HEAD, the callback is
 * executed with the given payload. The return value equals the
 * return value of the last executed callback function.
 */
int git_repository_foreach_head(git_repository *repo, git_repository_foreach_head_cb cb, void *payload);

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
		"cannot %s. This operation is not allowed against bare repositories.",
		operation_name);

	return GIT_EBAREREPO;
}

int git_repository__set_orig_head(git_repository *repo, const git_oid *orig_head);

int git_repository__cleanup_files(git_repository *repo, const char *files[], size_t files_len);

/* The default "reserved names" for a repository */
extern git_buf git_repository__reserved_names_win32[];
extern size_t git_repository__reserved_names_win32_len;

extern git_buf git_repository__reserved_names_posix[];
extern size_t git_repository__reserved_names_posix_len;

/*
 * Gets any "reserved names" in the repository.  This will return paths
 * that should not be allowed in the repository (like ".git") to avoid
 * conflicting with the repository path, or with alternate mechanisms to
 * the repository path (eg, "GIT~1").  Every attempt will be made to look
 * up all possible reserved names - if there was a conflict for the shortname
 * GIT~1, for example, this function will try to look up the alternate
 * shortname.  If that fails, this function returns false, but out and outlen
 * will still be populated with good defaults.
 */
bool git_repository__reserved_names(
	git_buf **out, size_t *outlen, git_repository *repo, bool include_ntfs);

#endif
