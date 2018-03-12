/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "repository.h"

#include <ctype.h>

#include "git2/object.h"
#include "git2/refdb.h"
#include "git2/sys/repository.h"

#include "common.h"
#include "commit.h"
#include "tag.h"
#include "blob.h"
#include "fileops.h"
#include "sysdir.h"
#include "filebuf.h"
#include "index.h"
#include "config.h"
#include "refs.h"
#include "filter.h"
#include "odb.h"
#include "remote.h"
#include "merge.h"
#include "diff_driver.h"
#include "annotated_commit.h"
#include "submodule.h"
#include "worktree.h"

#include "strmap.h"

#ifdef GIT_WIN32
# include "win32/w32_util.h"
#endif

bool git_repository__fsync_gitdir = false;

static const struct {
    git_repository_item_t parent;
    const char *name;
    bool directory;
} items[] = {
	{ GIT_REPOSITORY_ITEM_GITDIR, NULL, true },
	{ GIT_REPOSITORY_ITEM_WORKDIR, NULL, true },
	{ GIT_REPOSITORY_ITEM_COMMONDIR, NULL, true },
	{ GIT_REPOSITORY_ITEM_GITDIR, "index", false },
	{ GIT_REPOSITORY_ITEM_COMMONDIR, "objects", true },
	{ GIT_REPOSITORY_ITEM_COMMONDIR, "refs", true },
	{ GIT_REPOSITORY_ITEM_COMMONDIR, "packed-refs", false },
	{ GIT_REPOSITORY_ITEM_COMMONDIR, "remotes", true },
	{ GIT_REPOSITORY_ITEM_COMMONDIR, "config", false },
	{ GIT_REPOSITORY_ITEM_COMMONDIR, "info", true },
	{ GIT_REPOSITORY_ITEM_COMMONDIR, "hooks", true },
	{ GIT_REPOSITORY_ITEM_COMMONDIR, "logs", true },
	{ GIT_REPOSITORY_ITEM_GITDIR, "modules", true },
	{ GIT_REPOSITORY_ITEM_COMMONDIR, "worktrees", true }
};

static int check_repositoryformatversion(git_config *config);

#define GIT_COMMONDIR_FILE "commondir"
#define GIT_GITDIR_FILE "gitdir"

#define GIT_FILE_CONTENT_PREFIX "gitdir:"

#define GIT_BRANCH_MASTER "master"

#define GIT_REPO_VERSION 0

git_buf git_repository__reserved_names_win32[] = {
	{ DOT_GIT, 0, CONST_STRLEN(DOT_GIT) },
	{ GIT_DIR_SHORTNAME, 0, CONST_STRLEN(GIT_DIR_SHORTNAME) }
};
size_t git_repository__reserved_names_win32_len = 2;

git_buf git_repository__reserved_names_posix[] = {
	{ DOT_GIT, 0, CONST_STRLEN(DOT_GIT) },
};
size_t git_repository__reserved_names_posix_len = 1;

static void set_odb(git_repository *repo, git_odb *odb)
{
	if (odb) {
		GIT_REFCOUNT_OWN(odb, repo);
		GIT_REFCOUNT_INC(odb);
	}

	if ((odb = git__swap(repo->_odb, odb)) != NULL) {
		GIT_REFCOUNT_OWN(odb, NULL);
		git_odb_free(odb);
	}
}

static void set_refdb(git_repository *repo, git_refdb *refdb)
{
	if (refdb) {
		GIT_REFCOUNT_OWN(refdb, repo);
		GIT_REFCOUNT_INC(refdb);
	}

	if ((refdb = git__swap(repo->_refdb, refdb)) != NULL) {
		GIT_REFCOUNT_OWN(refdb, NULL);
		git_refdb_free(refdb);
	}
}

static void set_config(git_repository *repo, git_config *config)
{
	if (config) {
		GIT_REFCOUNT_OWN(config, repo);
		GIT_REFCOUNT_INC(config);
	}

	if ((config = git__swap(repo->_config, config)) != NULL) {
		GIT_REFCOUNT_OWN(config, NULL);
		git_config_free(config);
	}

	git_repository__cvar_cache_clear(repo);
}

static void set_index(git_repository *repo, git_index *index)
{
	if (index) {
		GIT_REFCOUNT_OWN(index, repo);
		GIT_REFCOUNT_INC(index);
	}

	if ((index = git__swap(repo->_index, index)) != NULL) {
		GIT_REFCOUNT_OWN(index, NULL);
		git_index_free(index);
	}
}

void git_repository__cleanup(git_repository *repo)
{
	assert(repo);

	git_repository_submodule_cache_clear(repo);
	git_cache_clear(&repo->objects);
	git_attr_cache_flush(repo);

	set_config(repo, NULL);
	set_index(repo, NULL);
	set_odb(repo, NULL);
	set_refdb(repo, NULL);
}

void git_repository_free(git_repository *repo)
{
	size_t i;

	if (repo == NULL)
		return;

	git_repository__cleanup(repo);

	git_cache_free(&repo->objects);

	git_diff_driver_registry_free(repo->diff_drivers);
	repo->diff_drivers = NULL;

	for (i = 0; i < repo->reserved_names.size; i++)
		git_buf_free(git_array_get(repo->reserved_names, i));
	git_array_clear(repo->reserved_names);

	git__free(repo->gitlink);
	git__free(repo->gitdir);
	git__free(repo->commondir);
	git__free(repo->workdir);
	git__free(repo->namespace);
	git__free(repo->ident_name);
	git__free(repo->ident_email);

	git__memzero(repo, sizeof(*repo));
	git__free(repo);
}

/*
 * Git repository open methods
 *
 * Open a repository object from its path
 */
static bool valid_repository_path(git_buf *repository_path, git_buf *common_path)
{
	/* Check if we have a separate commondir (e.g. we have a
	 * worktree) */
	if (git_path_contains_file(repository_path, GIT_COMMONDIR_FILE)) {
		git_buf common_link  = GIT_BUF_INIT;
		git_buf_joinpath(&common_link, repository_path->ptr, GIT_COMMONDIR_FILE);

		git_futils_readbuffer(&common_link, common_link.ptr);
		git_buf_rtrim(&common_link);

		if (git_path_is_relative(common_link.ptr)) {
			git_buf_joinpath(common_path, repository_path->ptr, common_link.ptr);
		} else {
			git_buf_swap(common_path, &common_link);
		}

		git_buf_free(&common_link);
	}
	else {
		git_buf_set(common_path, repository_path->ptr, repository_path->size);
	}

	/* Make sure the commondir path always has a trailing * slash */
	if (git_buf_rfind(common_path, '/') != (ssize_t)common_path->size - 1)
		git_buf_putc(common_path, '/');

	/* Ensure HEAD file exists */
	if (git_path_contains_file(repository_path, GIT_HEAD_FILE) == false)
		return false;

	/* Check files in common dir */
	if (git_path_contains_dir(common_path, GIT_OBJECTS_DIR) == false)
		return false;
	if (git_path_contains_dir(common_path, GIT_REFS_DIR) == false)
		return false;

	return true;
}

static git_repository *repository_alloc(void)
{
	git_repository *repo = git__calloc(1, sizeof(git_repository));

	if (repo == NULL ||
		git_cache_init(&repo->objects) < 0)
		goto on_error;

	git_array_init_to_size(repo->reserved_names, 4);
	if (!repo->reserved_names.ptr)
		goto on_error;

	/* set all the entries in the cvar cache to `unset` */
	git_repository__cvar_cache_clear(repo);

	return repo;

on_error:
	if (repo)
		git_cache_free(&repo->objects);

	git__free(repo);
	return NULL;
}

int git_repository_new(git_repository **out)
{
	git_repository *repo;

	*out = repo = repository_alloc();
	GITERR_CHECK_ALLOC(repo);

	repo->is_bare = 1;
	repo->is_worktree = 0;

	return 0;
}

static int load_config_data(git_repository *repo, const git_config *config)
{
	int is_bare;

	/* Try to figure out if it's bare, default to non-bare if it's not set */
	if (git_config_get_bool(&is_bare, config, "core.bare") < 0)
		repo->is_bare = 0;
	else
		repo->is_bare = is_bare;

	return 0;
}

static int load_workdir(git_repository *repo, git_config *config, git_buf *parent_path)
{
	int error;
	git_config_entry *ce;
	git_buf worktree = GIT_BUF_INIT;
	git_buf path = GIT_BUF_INIT;

	if (repo->is_bare)
		return 0;

	if ((error = git_config__lookup_entry(
			&ce, config, "core.worktree", false)) < 0)
		return error;

	if (repo->is_worktree) {
		char *gitlink = git_worktree__read_link(repo->gitdir, GIT_GITDIR_FILE);
		if (!gitlink) {
			error = -1;
			goto cleanup;
		}

		git_buf_attach(&worktree, gitlink, 0);

		if ((git_path_dirname_r(&worktree, worktree.ptr)) < 0 ||
		    git_path_to_dir(&worktree) < 0) {
			error = -1;
			goto cleanup;
		}

		repo->workdir = git_buf_detach(&worktree);
	}
	else if (ce && ce->value) {
		if ((error = git_path_prettify_dir(
				&worktree, ce->value, repo->gitdir)) < 0)
			goto cleanup;

		repo->workdir = git_buf_detach(&worktree);
	}
	else if (parent_path && git_path_isdir(parent_path->ptr))
		repo->workdir = git_buf_detach(parent_path);
	else {
		if (git_path_dirname_r(&worktree, repo->gitdir) < 0 ||
		    git_path_to_dir(&worktree) < 0) {
			error = -1;
			goto cleanup;
		}

		repo->workdir = git_buf_detach(&worktree);
	}

	GITERR_CHECK_ALLOC(repo->workdir);
cleanup:
	git_buf_free(&path);
	git_config_entry_free(ce);
	return error;
}

/*
 * This function returns furthest offset into path where a ceiling dir
 * is found, so we can stop processing the path at that point.
 *
 * Note: converting this to use git_bufs instead of GIT_PATH_MAX buffers on
 * the stack could remove directories name limits, but at the cost of doing
 * repeated malloc/frees inside the loop below, so let's not do it now.
 */
static size_t find_ceiling_dir_offset(
	const char *path,
	const char *ceiling_directories)
{
	char buf[GIT_PATH_MAX + 1];
	char buf2[GIT_PATH_MAX + 1];
	const char *ceil, *sep;
	size_t len, max_len = 0, min_len;

	assert(path);

	min_len = (size_t)(git_path_root(path) + 1);

	if (ceiling_directories == NULL || min_len == 0)
		return min_len;

	for (sep = ceil = ceiling_directories; *sep; ceil = sep + 1) {
		for (sep = ceil; *sep && *sep != GIT_PATH_LIST_SEPARATOR; sep++);
		len = sep - ceil;

		if (len == 0 || len >= sizeof(buf) || git_path_root(ceil) == -1)
			continue;

		strncpy(buf, ceil, len);
		buf[len] = '\0';

		if (p_realpath(buf, buf2) == NULL)
			continue;

		len = strlen(buf2);
		if (len > 0 && buf2[len-1] == '/')
			buf[--len] = '\0';

		if (!strncmp(path, buf2, len) &&
			(path[len] == '/' || !path[len]) &&
			len > max_len)
		{
			max_len = len;
		}
	}

	return (max_len <= min_len ? min_len : max_len);
}

/*
 * Read the contents of `file_path` and set `path_out` to the repo dir that
 * it points to.  Before calling, set `path_out` to the base directory that
 * should be used if the contents of `file_path` are a relative path.
 */
static int read_gitfile(git_buf *path_out, const char *file_path)
{
	int     error = 0;
	git_buf file = GIT_BUF_INIT;
	size_t  prefix_len = strlen(GIT_FILE_CONTENT_PREFIX);

	assert(path_out && file_path);

	if (git_futils_readbuffer(&file, file_path) < 0)
		return -1;

	git_buf_rtrim(&file);
	/* apparently on Windows, some people use backslashes in paths */
	git_path_mkposix(file.ptr);

	if (git_buf_len(&file) <= prefix_len ||
		memcmp(git_buf_cstr(&file), GIT_FILE_CONTENT_PREFIX, prefix_len) != 0)
	{
		giterr_set(GITERR_REPOSITORY,
			"the `.git` file at '%s' is malformed", file_path);
		error = -1;
	}
	else if ((error = git_path_dirname_r(path_out, file_path)) >= 0) {
		const char *gitlink = git_buf_cstr(&file) + prefix_len;
		while (*gitlink && git__isspace(*gitlink)) gitlink++;

		error = git_path_prettify_dir(
			path_out, gitlink, git_buf_cstr(path_out));
	}

	git_buf_free(&file);
	return error;
}

static int find_repo(
	git_buf *gitdir_path,
	git_buf *workdir_path,
	git_buf *gitlink_path,
	git_buf *commondir_path,
	const char *start_path,
	uint32_t flags,
	const char *ceiling_dirs)
{
	int error;
	git_buf path = GIT_BUF_INIT;
	git_buf repo_link = GIT_BUF_INIT;
	git_buf common_link = GIT_BUF_INIT;
	struct stat st;
	dev_t initial_device = 0;
	int min_iterations;
	bool in_dot_git;
	size_t ceiling_offset = 0;

	git_buf_clear(gitdir_path);

	error = git_path_prettify(&path, start_path, NULL);
	if (error < 0)
		return error;

	/* in_dot_git toggles each loop:
	 * /a/b/c/.git, /a/b/c, /a/b/.git, /a/b, /a/.git, /a
	 * With GIT_REPOSITORY_OPEN_BARE or GIT_REPOSITORY_OPEN_NO_DOTGIT, we
	 * assume we started with /a/b/c.git and don't append .git the first
	 * time through.
	 * min_iterations indicates the number of iterations left before going
	 * further counts as a search. */
	if (flags & (GIT_REPOSITORY_OPEN_BARE | GIT_REPOSITORY_OPEN_NO_DOTGIT)) {
		in_dot_git = true;
		min_iterations = 1;
	} else {
		in_dot_git = false;
		min_iterations = 2;
	}

	for (;;) {
		if (!(flags & GIT_REPOSITORY_OPEN_NO_DOTGIT)) {
			if (!in_dot_git) {
				error = git_buf_joinpath(&path, path.ptr, DOT_GIT);
				if (error < 0)
					break;
			}
			in_dot_git = !in_dot_git;
		}

		if (p_stat(path.ptr, &st) == 0) {
			/* check that we have not crossed device boundaries */
			if (initial_device == 0)
				initial_device = st.st_dev;
			else if (st.st_dev != initial_device &&
				 !(flags & GIT_REPOSITORY_OPEN_CROSS_FS))
				break;

			if (S_ISDIR(st.st_mode)) {
				if (valid_repository_path(&path, &common_link)) {
					git_path_to_dir(&path);
					git_buf_set(gitdir_path, path.ptr, path.size);

					if (gitlink_path)
						git_buf_attach(gitlink_path,
							git_worktree__read_link(path.ptr, GIT_GITDIR_FILE), 0);
					if (commondir_path)
						git_buf_swap(&common_link, commondir_path);

					break;
				}
			}
			else if (S_ISREG(st.st_mode) && git__suffixcmp(path.ptr, "/" DOT_GIT) == 0) {
				error = read_gitfile(&repo_link, path.ptr);
				if (error < 0)
					break;
				if (valid_repository_path(&repo_link, &common_link)) {
					git_buf_swap(gitdir_path, &repo_link);

					if (gitlink_path)
						error = git_buf_put(gitlink_path, path.ptr, path.size);
					if (commondir_path)
						git_buf_swap(&common_link, commondir_path);
				}
				break;
			}
		}

		/* Move up one directory. If we're in_dot_git, we'll search the
		 * parent itself next. If we're !in_dot_git, we'll search .git
		 * in the parent directory next (added at the top of the loop). */
		if (git_path_dirname_r(&path, path.ptr) < 0) {
			error = -1;
			break;
		}

		/* Once we've checked the directory (and .git if applicable),
		 * find the ceiling for a search. */
		if (min_iterations && (--min_iterations == 0))
			ceiling_offset = find_ceiling_dir_offset(path.ptr, ceiling_dirs);

		/* Check if we should stop searching here. */
		if (min_iterations == 0
		    && (path.ptr[ceiling_offset] == 0
			|| (flags & GIT_REPOSITORY_OPEN_NO_SEARCH)))
			break;
	}

	if (!error && workdir_path && !(flags & GIT_REPOSITORY_OPEN_BARE)) {
		if (!git_buf_len(gitdir_path))
			git_buf_clear(workdir_path);
		else {
			git_path_dirname_r(workdir_path, path.ptr);
			git_path_to_dir(workdir_path);
		}
		if (git_buf_oom(workdir_path))
			return -1;
	}

	/* If we didn't find the repository, and we don't have any other error
	 * to report, report that. */
	if (!git_buf_len(gitdir_path) && !error) {
		giterr_set(GITERR_REPOSITORY,
			"could not find repository from '%s'", start_path);
		error = GIT_ENOTFOUND;
	}

	git_buf_free(&path);
	git_buf_free(&repo_link);
	git_buf_free(&common_link);
	return error;
}

int git_repository_open_bare(
	git_repository **repo_ptr,
	const char *bare_path)
{
	int error;
	git_buf path = GIT_BUF_INIT, common_path = GIT_BUF_INIT;
	git_repository *repo = NULL;

	if ((error = git_path_prettify_dir(&path, bare_path, NULL)) < 0)
		return error;

	if (!valid_repository_path(&path, &common_path)) {
		git_buf_free(&path);
		git_buf_free(&common_path);
		giterr_set(GITERR_REPOSITORY, "path is not a repository: %s", bare_path);
		return GIT_ENOTFOUND;
	}

	repo = repository_alloc();
	GITERR_CHECK_ALLOC(repo);

	repo->gitdir = git_buf_detach(&path);
	GITERR_CHECK_ALLOC(repo->gitdir);
	repo->commondir = git_buf_detach(&common_path);
	GITERR_CHECK_ALLOC(repo->commondir);

	/* of course we're bare! */
	repo->is_bare = 1;
	repo->is_worktree = 0;
	repo->workdir = NULL;

	*repo_ptr = repo;
	return 0;
}

static int _git_repository_open_ext_from_env(
	git_repository **out,
	const char *start_path)
{
	git_repository *repo = NULL;
	git_index *index = NULL;
	git_odb *odb = NULL;
	git_buf dir_buf = GIT_BUF_INIT;
	git_buf ceiling_dirs_buf = GIT_BUF_INIT;
	git_buf across_fs_buf = GIT_BUF_INIT;
	git_buf index_file_buf = GIT_BUF_INIT;
	git_buf namespace_buf = GIT_BUF_INIT;
	git_buf object_dir_buf = GIT_BUF_INIT;
	git_buf alts_buf = GIT_BUF_INIT;
	git_buf work_tree_buf = GIT_BUF_INIT;
	git_buf common_dir_buf = GIT_BUF_INIT;
	const char *ceiling_dirs = NULL;
	unsigned flags = 0;
	int error;

	if (!start_path) {
		error = git__getenv(&dir_buf, "GIT_DIR");
		if (error == GIT_ENOTFOUND) {
			giterr_clear();
			start_path = ".";
		} else if (error < 0)
			goto error;
		else {
			start_path = git_buf_cstr(&dir_buf);
			flags |= GIT_REPOSITORY_OPEN_NO_SEARCH;
			flags |= GIT_REPOSITORY_OPEN_NO_DOTGIT;
		}
	}

	error = git__getenv(&ceiling_dirs_buf, "GIT_CEILING_DIRECTORIES");
	if (error == GIT_ENOTFOUND)
		giterr_clear();
	else if (error < 0)
		goto error;
	else
		ceiling_dirs = git_buf_cstr(&ceiling_dirs_buf);

	error = git__getenv(&across_fs_buf, "GIT_DISCOVERY_ACROSS_FILESYSTEM");
	if (error == GIT_ENOTFOUND)
		giterr_clear();
	else if (error < 0)
		goto error;
	else {
		int across_fs = 0;
		error = git_config_parse_bool(&across_fs, git_buf_cstr(&across_fs_buf));
		if (error < 0)
			goto error;
		if (across_fs)
			flags |= GIT_REPOSITORY_OPEN_CROSS_FS;
	}

	error = git__getenv(&index_file_buf, "GIT_INDEX_FILE");
	if (error == GIT_ENOTFOUND)
		giterr_clear();
	else if (error < 0)
		goto error;
	else {
		error = git_index_open(&index, git_buf_cstr(&index_file_buf));
		if (error < 0)
			goto error;
	}

	error = git__getenv(&namespace_buf, "GIT_NAMESPACE");
	if (error == GIT_ENOTFOUND)
		giterr_clear();
	else if (error < 0)
		goto error;

	error = git__getenv(&object_dir_buf, "GIT_OBJECT_DIRECTORY");
	if (error == GIT_ENOTFOUND)
		giterr_clear();
	else if (error < 0)
		goto error;
	else {
		error = git_odb_open(&odb, git_buf_cstr(&object_dir_buf));
		if (error < 0)
			goto error;
	}

	error = git__getenv(&work_tree_buf, "GIT_WORK_TREE");
	if (error == GIT_ENOTFOUND)
		giterr_clear();
	else if (error < 0)
		goto error;
	else {
		giterr_set(GITERR_INVALID, "GIT_WORK_TREE unimplemented");
		error = GIT_ERROR;
		goto error;
	}

	error = git__getenv(&work_tree_buf, "GIT_COMMON_DIR");
	if (error == GIT_ENOTFOUND)
		giterr_clear();
	else if (error < 0)
		goto error;
	else {
		giterr_set(GITERR_INVALID, "GIT_COMMON_DIR unimplemented");
		error = GIT_ERROR;
		goto error;
	}

	error = git_repository_open_ext(&repo, start_path, flags, ceiling_dirs);
	if (error < 0)
		goto error;

	if (odb)
		git_repository_set_odb(repo, odb);

	error = git__getenv(&alts_buf, "GIT_ALTERNATE_OBJECT_DIRECTORIES");
	if (error == GIT_ENOTFOUND) {
		giterr_clear();
		error = 0;
	} else if (error < 0)
		goto error;
        else {
		const char *end;
		char *alt, *sep;
		if (!odb) {
			error = git_repository_odb(&odb, repo);
			if (error < 0)
				goto error;
		}

		end = git_buf_cstr(&alts_buf) + git_buf_len(&alts_buf);
		for (sep = alt = alts_buf.ptr; sep != end; alt = sep+1) {
			for (sep = alt; *sep && *sep != GIT_PATH_LIST_SEPARATOR; sep++)
				;
			if (*sep)
				*sep = '\0';
			error = git_odb_add_disk_alternate(odb, alt);
			if (error < 0)
				goto error;
		}
	}

	if (git_buf_len(&namespace_buf)) {
		error = git_repository_set_namespace(repo, git_buf_cstr(&namespace_buf));
		if (error < 0)
			goto error;
	}

	git_repository_set_index(repo, index);

	if (out) {
		*out = repo;
		goto success;
	}
error:
	git_repository_free(repo);
success:
	git_odb_free(odb);
	git_index_free(index);
	git_buf_free(&common_dir_buf);
	git_buf_free(&work_tree_buf);
	git_buf_free(&alts_buf);
	git_buf_free(&object_dir_buf);
	git_buf_free(&namespace_buf);
	git_buf_free(&index_file_buf);
	git_buf_free(&across_fs_buf);
	git_buf_free(&ceiling_dirs_buf);
	git_buf_free(&dir_buf);
	return error;
}

static int repo_is_worktree(unsigned *out, const git_repository *repo)
{
	git_buf gitdir_link = GIT_BUF_INIT;
	int error;

	/* Worktrees cannot have the same commondir and gitdir */
	if (repo->commondir && repo->gitdir
	    && !strcmp(repo->commondir, repo->gitdir)) {
		*out = 0;
		return 0;
	}

	if ((error = git_buf_joinpath(&gitdir_link, repo->gitdir, "gitdir")) < 0)
		return -1;

	/* A 'gitdir' file inside a git directory is currently
	 * only used when the repository is a working tree. */
	*out = !!git_path_exists(gitdir_link.ptr);

	git_buf_free(&gitdir_link);
	return error;
}

int git_repository_open_ext(
	git_repository **repo_ptr,
	const char *start_path,
	unsigned int flags,
	const char *ceiling_dirs)
{
	int error;
	unsigned is_worktree;
	git_buf gitdir = GIT_BUF_INIT, workdir = GIT_BUF_INIT,
		gitlink = GIT_BUF_INIT, commondir = GIT_BUF_INIT;
	git_repository *repo;
	git_config *config = NULL;

	if (flags & GIT_REPOSITORY_OPEN_FROM_ENV)
		return _git_repository_open_ext_from_env(repo_ptr, start_path);

	if (repo_ptr)
		*repo_ptr = NULL;

	error = find_repo(
		&gitdir, &workdir, &gitlink, &commondir, start_path, flags, ceiling_dirs);

	if (error < 0 || !repo_ptr)
		return error;

	repo = repository_alloc();
	GITERR_CHECK_ALLOC(repo);

	repo->gitdir = git_buf_detach(&gitdir);
	GITERR_CHECK_ALLOC(repo->gitdir);

	if (gitlink.size) {
		repo->gitlink = git_buf_detach(&gitlink);
		GITERR_CHECK_ALLOC(repo->gitlink);
	}
	if (commondir.size) {
		repo->commondir = git_buf_detach(&commondir);
		GITERR_CHECK_ALLOC(repo->commondir);
	}

	if ((error = repo_is_worktree(&is_worktree, repo)) < 0)
		goto cleanup;
	repo->is_worktree = is_worktree;

	/*
	 * We'd like to have the config, but git doesn't particularly
	 * care if it's not there, so we need to deal with that.
	 */

	error = git_repository_config_snapshot(&config, repo);
	if (error < 0 && error != GIT_ENOTFOUND)
		goto cleanup;

	if (config && (error = check_repositoryformatversion(config)) < 0)
		goto cleanup;

	if ((flags & GIT_REPOSITORY_OPEN_BARE) != 0)
		repo->is_bare = 1;
	else {

		if (config &&
		    ((error = load_config_data(repo, config)) < 0 ||
		     (error = load_workdir(repo, config, &workdir)) < 0))
			goto cleanup;
	}

cleanup:
	git_buf_free(&gitdir);
	git_buf_free(&workdir);
	git_config_free(config);

	if (error < 0)
		git_repository_free(repo);
	else
		*repo_ptr = repo;

	return error;
}

int git_repository_open(git_repository **repo_out, const char *path)
{
	return git_repository_open_ext(
		repo_out, path, GIT_REPOSITORY_OPEN_NO_SEARCH, NULL);
}

int git_repository_open_from_worktree(git_repository **repo_out, git_worktree *wt)
{
	git_buf path = GIT_BUF_INIT;
	git_repository *repo = NULL;
	int len, err;

	assert(repo_out && wt);

	*repo_out = NULL;
	len = strlen(wt->gitlink_path);

	if (len <= 4 || strcasecmp(wt->gitlink_path + len - 4, ".git")) {
		err = -1;
		goto out;
	}

	if ((err = git_buf_set(&path, wt->gitlink_path, len - 4)) < 0)
		goto out;

	if ((err = git_repository_open(&repo, path.ptr)) < 0)
		goto out;

	*repo_out = repo;

out:
	git_buf_free(&path);

	return err;
}

int git_repository_wrap_odb(git_repository **repo_out, git_odb *odb)
{
	git_repository *repo;

	repo = repository_alloc();
	GITERR_CHECK_ALLOC(repo);

	git_repository_set_odb(repo, odb);
	*repo_out = repo;

	return 0;
}

int git_repository_discover(
	git_buf *out,
	const char *start_path,
	int across_fs,
	const char *ceiling_dirs)
{
	uint32_t flags = across_fs ? GIT_REPOSITORY_OPEN_CROSS_FS : 0;

	assert(start_path);

	git_buf_sanitize(out);

	return find_repo(out, NULL, NULL, NULL, start_path, flags, ceiling_dirs);
}

static int load_config(
	git_config **out,
	git_repository *repo,
	const char *global_config_path,
	const char *xdg_config_path,
	const char *system_config_path,
	const char *programdata_path)
{
	int error;
	git_buf config_path = GIT_BUF_INIT;
	git_config *cfg = NULL;

	assert(repo && out);

	if ((error = git_config_new(&cfg)) < 0)
		return error;

	if ((error = git_repository_item_path(&config_path, repo, GIT_REPOSITORY_ITEM_CONFIG)) == 0)
		error = git_config_add_file_ondisk(cfg, config_path.ptr, GIT_CONFIG_LEVEL_LOCAL, 0);

	if (error && error != GIT_ENOTFOUND)
		goto on_error;

	git_buf_free(&config_path);

	if (global_config_path != NULL &&
		(error = git_config_add_file_ondisk(
			cfg, global_config_path, GIT_CONFIG_LEVEL_GLOBAL, 0)) < 0 &&
		error != GIT_ENOTFOUND)
		goto on_error;

	if (xdg_config_path != NULL &&
		(error = git_config_add_file_ondisk(
			cfg, xdg_config_path, GIT_CONFIG_LEVEL_XDG, 0)) < 0 &&
		error != GIT_ENOTFOUND)
		goto on_error;

	if (system_config_path != NULL &&
		(error = git_config_add_file_ondisk(
			cfg, system_config_path, GIT_CONFIG_LEVEL_SYSTEM, 0)) < 0 &&
		error != GIT_ENOTFOUND)
		goto on_error;

	if (programdata_path != NULL &&
		(error = git_config_add_file_ondisk(
			cfg, programdata_path, GIT_CONFIG_LEVEL_PROGRAMDATA, 0)) < 0 &&
		error != GIT_ENOTFOUND)
		goto on_error;

	giterr_clear(); /* clear any lingering ENOTFOUND errors */

	*out = cfg;
	return 0;

on_error:
	git_buf_free(&config_path);
	git_config_free(cfg);
	*out = NULL;
	return error;
}

static const char *path_unless_empty(git_buf *buf)
{
	return git_buf_len(buf) > 0 ? git_buf_cstr(buf) : NULL;
}

int git_repository_config__weakptr(git_config **out, git_repository *repo)
{
	int error = 0;

	if (repo->_config == NULL) {
		git_buf global_buf = GIT_BUF_INIT;
		git_buf xdg_buf = GIT_BUF_INIT;
		git_buf system_buf = GIT_BUF_INIT;
		git_buf programdata_buf = GIT_BUF_INIT;
		git_config *config;

		git_config_find_global(&global_buf);
		git_config_find_xdg(&xdg_buf);
		git_config_find_system(&system_buf);
		git_config_find_programdata(&programdata_buf);

		/* If there is no global file, open a backend for it anyway */
		if (git_buf_len(&global_buf) == 0)
			git_config__global_location(&global_buf);

		error = load_config(
			&config, repo,
			path_unless_empty(&global_buf),
			path_unless_empty(&xdg_buf),
			path_unless_empty(&system_buf),
			path_unless_empty(&programdata_buf));
		if (!error) {
			GIT_REFCOUNT_OWN(config, repo);

			config = git__compare_and_swap(&repo->_config, NULL, config);
			if (config != NULL) {
				GIT_REFCOUNT_OWN(config, NULL);
				git_config_free(config);
			}
		}

		git_buf_free(&global_buf);
		git_buf_free(&xdg_buf);
		git_buf_free(&system_buf);
		git_buf_free(&programdata_buf);
	}

	*out = repo->_config;
	return error;
}

int git_repository_config(git_config **out, git_repository *repo)
{
	if (git_repository_config__weakptr(out, repo) < 0)
		return -1;

	GIT_REFCOUNT_INC(*out);
	return 0;
}

int git_repository_config_snapshot(git_config **out, git_repository *repo)
{
	int error;
	git_config *weak;

	if ((error = git_repository_config__weakptr(&weak, repo)) < 0)
		return error;

	return git_config_snapshot(out, weak);
}

void git_repository_set_config(git_repository *repo, git_config *config)
{
	assert(repo && config);
	set_config(repo, config);
}

int git_repository_odb__weakptr(git_odb **out, git_repository *repo)
{
	int error = 0;

	assert(repo && out);

	if (repo->_odb == NULL) {
		git_buf odb_path = GIT_BUF_INIT;
		git_odb *odb;

		if ((error = git_repository_item_path(&odb_path, repo,
				GIT_REPOSITORY_ITEM_OBJECTS)) < 0 ||
			(error = git_odb_new(&odb)) < 0)
			return error;

		GIT_REFCOUNT_OWN(odb, repo);

		if ((error = git_odb__set_caps(odb, GIT_ODB_CAP_FROM_OWNER)) < 0 ||
			(error = git_odb__add_default_backends(odb, odb_path.ptr, 0, 0)) < 0) {
			git_odb_free(odb);
			return error;
		}

		odb = git__compare_and_swap(&repo->_odb, NULL, odb);
		if (odb != NULL) {
			GIT_REFCOUNT_OWN(odb, NULL);
			git_odb_free(odb);
		}

		git_buf_free(&odb_path);
	}

	*out = repo->_odb;
	return error;
}

int git_repository_odb(git_odb **out, git_repository *repo)
{
	if (git_repository_odb__weakptr(out, repo) < 0)
		return -1;

	GIT_REFCOUNT_INC(*out);
	return 0;
}

void git_repository_set_odb(git_repository *repo, git_odb *odb)
{
	assert(repo && odb);
	set_odb(repo, odb);
}

int git_repository_refdb__weakptr(git_refdb **out, git_repository *repo)
{
	int error = 0;

	assert(out && repo);

	if (repo->_refdb == NULL) {
		git_refdb *refdb;

		error = git_refdb_open(&refdb, repo);
		if (!error) {
			GIT_REFCOUNT_OWN(refdb, repo);

			refdb = git__compare_and_swap(&repo->_refdb, NULL, refdb);
			if (refdb != NULL) {
				GIT_REFCOUNT_OWN(refdb, NULL);
				git_refdb_free(refdb);
			}
		}
	}

	*out = repo->_refdb;
	return error;
}

int git_repository_refdb(git_refdb **out, git_repository *repo)
{
	if (git_repository_refdb__weakptr(out, repo) < 0)
		return -1;

	GIT_REFCOUNT_INC(*out);
	return 0;
}

void git_repository_set_refdb(git_repository *repo, git_refdb *refdb)
{
	assert(repo && refdb);
	set_refdb(repo, refdb);
}

int git_repository_index__weakptr(git_index **out, git_repository *repo)
{
	int error = 0;

	assert(out && repo);

	if (repo->_index == NULL) {
		git_buf index_path = GIT_BUF_INIT;
		git_index *index;

		if ((error = git_buf_joinpath(&index_path, repo->gitdir, GIT_INDEX_FILE)) < 0)
			return error;

		error = git_index_open(&index, index_path.ptr);
		if (!error) {
			GIT_REFCOUNT_OWN(index, repo);

			index = git__compare_and_swap(&repo->_index, NULL, index);
			if (index != NULL) {
				GIT_REFCOUNT_OWN(index, NULL);
				git_index_free(index);
			}

			error = git_index_set_caps(repo->_index, GIT_INDEXCAP_FROM_OWNER);
		}

		git_buf_free(&index_path);
	}

	*out = repo->_index;
	return error;
}

int git_repository_index(git_index **out, git_repository *repo)
{
	if (git_repository_index__weakptr(out, repo) < 0)
		return -1;

	GIT_REFCOUNT_INC(*out);
	return 0;
}

void git_repository_set_index(git_repository *repo, git_index *index)
{
	assert(repo);
	set_index(repo, index);
}

int git_repository_set_namespace(git_repository *repo, const char *namespace)
{
	git__free(repo->namespace);

	if (namespace == NULL) {
		repo->namespace = NULL;
		return 0;
	}

	return (repo->namespace = git__strdup(namespace)) ? 0 : -1;
}

const char *git_repository_get_namespace(git_repository *repo)
{
	return repo->namespace;
}

#ifdef GIT_WIN32
static int reserved_names_add8dot3(git_repository *repo, const char *path)
{
	char *name = git_win32_path_8dot3_name(path);
	const char *def = GIT_DIR_SHORTNAME;
	const char *def_dot_git = DOT_GIT;
	size_t name_len, def_len = CONST_STRLEN(GIT_DIR_SHORTNAME);
	size_t def_dot_git_len = CONST_STRLEN(DOT_GIT);
	git_buf *buf;

	if (!name)
		return 0;

	name_len = strlen(name);

	if ((name_len == def_len && memcmp(name, def, def_len) == 0) ||
		(name_len == def_dot_git_len && memcmp(name, def_dot_git, def_dot_git_len) == 0)) {
		git__free(name);
		return 0;
	}

	if ((buf = git_array_alloc(repo->reserved_names)) == NULL)
		return -1;

	git_buf_attach(buf, name, name_len);
	return true;
}

bool git_repository__reserved_names(
	git_buf **out, size_t *outlen, git_repository *repo, bool include_ntfs)
{
	GIT_UNUSED(include_ntfs);

	if (repo->reserved_names.size == 0) {
		git_buf *buf;
		size_t i;

		/* Add the static defaults */
		for (i = 0; i < git_repository__reserved_names_win32_len; i++) {
			if ((buf = git_array_alloc(repo->reserved_names)) == NULL)
				goto on_error;

			buf->ptr = git_repository__reserved_names_win32[i].ptr;
			buf->size = git_repository__reserved_names_win32[i].size;
		}

		/* Try to add any repo-specific reserved names - the gitlink file
		 * within a submodule or the repository (if the repository directory
		 * is beneath the workdir).  These are typically `.git`, but should
		 * be protected in case they are not.  Note, repo and workdir paths
		 * are always prettified to end in `/`, so a prefixcmp is safe.
		 */
		if (!repo->is_bare) {
			int (*prefixcmp)(const char *, const char *);
			int error, ignorecase;

			error = git_repository__cvar(
				&ignorecase, repo, GIT_CVAR_IGNORECASE);
			prefixcmp = (error || ignorecase) ? git__prefixcmp_icase :
				git__prefixcmp;

			if (repo->gitlink &&
				reserved_names_add8dot3(repo, repo->gitlink) < 0)
				goto on_error;

			if (repo->gitdir &&
				prefixcmp(repo->gitdir, repo->workdir) == 0 &&
				reserved_names_add8dot3(repo, repo->gitdir) < 0)
				goto on_error;
		}
	}

	*out = repo->reserved_names.ptr;
	*outlen = repo->reserved_names.size;

	return true;

	/* Always give good defaults, even on OOM */
on_error:
	*out = git_repository__reserved_names_win32;
	*outlen = git_repository__reserved_names_win32_len;

	return false;
}
#else
bool git_repository__reserved_names(
	git_buf **out, size_t *outlen, git_repository *repo, bool include_ntfs)
{
	GIT_UNUSED(repo);

	if (include_ntfs) {
		*out = git_repository__reserved_names_win32;
		*outlen = git_repository__reserved_names_win32_len;
	} else {
		*out = git_repository__reserved_names_posix;
		*outlen = git_repository__reserved_names_posix_len;
	}

	return true;
}
#endif

static int check_repositoryformatversion(git_config *config)
{
	int version, error;

	error = git_config_get_int32(&version, config, "core.repositoryformatversion");
	/* git ignores this if the config variable isn't there */
	if (error == GIT_ENOTFOUND)
		return 0;

	if (error < 0)
		return -1;

	if (GIT_REPO_VERSION < version) {
		giterr_set(GITERR_REPOSITORY,
			"unsupported repository version %d. Only versions up to %d are supported.",
			version, GIT_REPO_VERSION);
		return -1;
	}

	return 0;
}

int git_repository_create_head(const char *git_dir, const char *ref_name)
{
	git_buf ref_path = GIT_BUF_INIT;
	git_filebuf ref = GIT_FILEBUF_INIT;
	const char *fmt;

	if (git_buf_joinpath(&ref_path, git_dir, GIT_HEAD_FILE) < 0 ||
		git_filebuf_open(&ref, ref_path.ptr, 0, GIT_REFS_FILE_MODE) < 0)
		goto fail;

	if (!ref_name)
		ref_name = GIT_BRANCH_MASTER;

	if (git__prefixcmp(ref_name, GIT_REFS_DIR) == 0)
		fmt = "ref: %s\n";
	else
		fmt = "ref: " GIT_REFS_HEADS_DIR "%s\n";

	if (git_filebuf_printf(&ref, fmt, ref_name) < 0 ||
		git_filebuf_commit(&ref) < 0)
		goto fail;

	git_buf_free(&ref_path);
	return 0;

fail:
	git_buf_free(&ref_path);
	git_filebuf_cleanup(&ref);
	return -1;
}

static bool is_chmod_supported(const char *file_path)
{
	struct stat st1, st2;

	if (p_stat(file_path, &st1) < 0)
		return false;

	if (p_chmod(file_path, st1.st_mode ^ S_IXUSR) < 0)
		return false;

	if (p_stat(file_path, &st2) < 0)
		return false;

	return (st1.st_mode != st2.st_mode);
}

static bool is_filesystem_case_insensitive(const char *gitdir_path)
{
	git_buf path = GIT_BUF_INIT;
	int is_insensitive = -1;

	if (!git_buf_joinpath(&path, gitdir_path, "CoNfIg"))
		is_insensitive = git_path_exists(git_buf_cstr(&path));

	git_buf_free(&path);
	return is_insensitive;
}

static bool are_symlinks_supported(const char *wd_path)
{
	git_buf path = GIT_BUF_INIT;
	int fd;
	struct stat st;
	int symlinks_supported = -1;

	if ((fd = git_futils_mktmp(&path, wd_path, 0666)) < 0 ||
		p_close(fd) < 0 ||
		p_unlink(path.ptr) < 0 ||
		p_symlink("testing", path.ptr) < 0 ||
		p_lstat(path.ptr, &st) < 0)
		symlinks_supported = false;
	else
		symlinks_supported = (S_ISLNK(st.st_mode) != 0);

	(void)p_unlink(path.ptr);
	git_buf_free(&path);

	return symlinks_supported;
}

static int create_empty_file(const char *path, mode_t mode)
{
	int fd;

	if ((fd = p_creat(path, mode)) < 0) {
		giterr_set(GITERR_OS, "error while creating '%s'", path);
		return -1;
	}

	if (p_close(fd) < 0) {
		giterr_set(GITERR_OS, "error while closing '%s'", path);
		return -1;
	}

	return 0;
}

static int repo_local_config(
	git_config **out,
	git_buf *config_dir,
	git_repository *repo,
	const char *repo_dir)
{
	int error = 0;
	git_config *parent;
	const char *cfg_path;

	if (git_buf_joinpath(config_dir, repo_dir, GIT_CONFIG_FILENAME_INREPO) < 0)
		return -1;
	cfg_path = git_buf_cstr(config_dir);

	/* make LOCAL config if missing */
	if (!git_path_isfile(cfg_path) &&
		(error = create_empty_file(cfg_path, GIT_CONFIG_FILE_MODE)) < 0)
		return error;

	/* if no repo, just open that file directly */
	if (!repo)
		return git_config_open_ondisk(out, cfg_path);

	/* otherwise, open parent config and get that level */
	if ((error = git_repository_config__weakptr(&parent, repo)) < 0)
		return error;

	if (git_config_open_level(out, parent, GIT_CONFIG_LEVEL_LOCAL) < 0) {
		giterr_clear();

		if (!(error = git_config_add_file_ondisk(
				parent, cfg_path, GIT_CONFIG_LEVEL_LOCAL, false)))
			error = git_config_open_level(out, parent, GIT_CONFIG_LEVEL_LOCAL);
	}

	git_config_free(parent);

	return error;
}

static int repo_init_fs_configs(
	git_config *cfg,
	const char *cfg_path,
	const char *repo_dir,
	const char *work_dir,
	bool update_ignorecase)
{
	int error = 0;

	if (!work_dir)
		work_dir = repo_dir;

	if ((error = git_config_set_bool(
			cfg, "core.filemode", is_chmod_supported(cfg_path))) < 0)
		return error;

	if (!are_symlinks_supported(work_dir)) {
		if ((error = git_config_set_bool(cfg, "core.symlinks", false)) < 0)
			return error;
	} else if (git_config_delete_entry(cfg, "core.symlinks") < 0)
		giterr_clear();

	if (update_ignorecase) {
		if (is_filesystem_case_insensitive(repo_dir)) {
			if ((error = git_config_set_bool(cfg, "core.ignorecase", true)) < 0)
				return error;
		} else if (git_config_delete_entry(cfg, "core.ignorecase") < 0)
			giterr_clear();
	}

#ifdef GIT_USE_ICONV
	if ((error = git_config_set_bool(
			cfg, "core.precomposeunicode",
			git_path_does_fs_decompose_unicode(work_dir))) < 0)
		return error;
	/* on non-iconv platforms, don't even set core.precomposeunicode */
#endif

	return 0;
}

static int repo_init_config(
	const char *repo_dir,
	const char *work_dir,
	uint32_t flags,
	uint32_t mode)
{
	int error = 0;
	git_buf cfg_path = GIT_BUF_INIT, worktree_path = GIT_BUF_INIT;
	git_config *config = NULL;
	bool is_bare = ((flags & GIT_REPOSITORY_INIT_BARE) != 0);
	bool is_reinit = ((flags & GIT_REPOSITORY_INIT__IS_REINIT) != 0);

	if ((error = repo_local_config(&config, &cfg_path, NULL, repo_dir)) < 0)
		goto cleanup;

	if (is_reinit && (error = check_repositoryformatversion(config)) < 0)
		goto cleanup;

#define SET_REPO_CONFIG(TYPE, NAME, VAL) do { \
	if ((error = git_config_set_##TYPE(config, NAME, VAL)) < 0) \
		goto cleanup; } while (0)

	SET_REPO_CONFIG(bool, "core.bare", is_bare);
	SET_REPO_CONFIG(int32, "core.repositoryformatversion", GIT_REPO_VERSION);

	if ((error = repo_init_fs_configs(
			config, cfg_path.ptr, repo_dir, work_dir, !is_reinit)) < 0)
		goto cleanup;

	if (!is_bare) {
		SET_REPO_CONFIG(bool, "core.logallrefupdates", true);

		if (!(flags & GIT_REPOSITORY_INIT__NATURAL_WD)) {
			if ((error = git_buf_sets(&worktree_path, work_dir)) < 0)
				goto cleanup;

			if ((flags & GIT_REPOSITORY_INIT_RELATIVE_GITLINK))
				if ((error = git_path_make_relative(&worktree_path, repo_dir)) < 0)
					goto cleanup;

			SET_REPO_CONFIG(string, "core.worktree", worktree_path.ptr);
		} else if (is_reinit) {
			if (git_config_delete_entry(config, "core.worktree") < 0)
				giterr_clear();
		}
	}

	if (mode == GIT_REPOSITORY_INIT_SHARED_GROUP) {
		SET_REPO_CONFIG(int32, "core.sharedrepository", 1);
		SET_REPO_CONFIG(bool, "receive.denyNonFastforwards", true);
	}
	else if (mode == GIT_REPOSITORY_INIT_SHARED_ALL) {
		SET_REPO_CONFIG(int32, "core.sharedrepository", 2);
		SET_REPO_CONFIG(bool, "receive.denyNonFastforwards", true);
	}

cleanup:
	git_buf_free(&cfg_path);
	git_buf_free(&worktree_path);
	git_config_free(config);

	return error;
}

static int repo_reinit_submodule_fs(git_submodule *sm, const char *n, void *p)
{
	git_repository *smrepo = NULL;
	GIT_UNUSED(n); GIT_UNUSED(p);

	if (git_submodule_open(&smrepo, sm) < 0 ||
		git_repository_reinit_filesystem(smrepo, true) < 0)
		giterr_clear();
	git_repository_free(smrepo);

	return 0;
}

int git_repository_reinit_filesystem(git_repository *repo, int recurse)
{
	int error = 0;
	git_buf path = GIT_BUF_INIT;
	git_config *config = NULL;
	const char *repo_dir = git_repository_path(repo);

	if (!(error = repo_local_config(&config, &path, repo, repo_dir)))
		error = repo_init_fs_configs(
			config, path.ptr, repo_dir, git_repository_workdir(repo), true);

	git_config_free(config);
	git_buf_free(&path);

	git_repository__cvar_cache_clear(repo);

	if (!repo->is_bare && recurse)
		(void)git_submodule_foreach(repo, repo_reinit_submodule_fs, NULL);

	return error;
}

static int repo_write_template(
	const char *git_dir,
	bool allow_overwrite,
	const char *file,
	mode_t mode,
	bool hidden,
	const char *content)
{
	git_buf path = GIT_BUF_INIT;
	int fd, error = 0, flags;

	if (git_buf_joinpath(&path, git_dir, file) < 0)
		return -1;

	if (allow_overwrite)
		flags = O_WRONLY | O_CREAT | O_TRUNC;
	else
		flags = O_WRONLY | O_CREAT | O_EXCL;

	fd = p_open(git_buf_cstr(&path), flags, mode);

	if (fd >= 0) {
		error = p_write(fd, content, strlen(content));

		p_close(fd);
	}
	else if (errno != EEXIST)
		error = fd;

#ifdef GIT_WIN32
	if (!error && hidden) {
		if (git_win32__set_hidden(path.ptr, true) < 0)
			error = -1;
	}
#else
	GIT_UNUSED(hidden);
#endif

	git_buf_free(&path);

	if (error)
		giterr_set(GITERR_OS,
			"failed to initialize repository with template '%s'", file);

	return error;
}

static int repo_write_gitlink(
	const char *in_dir, const char *to_repo, bool use_relative_path)
{
	int error;
	git_buf buf = GIT_BUF_INIT;
	git_buf path_to_repo = GIT_BUF_INIT;
	struct stat st;

	git_path_dirname_r(&buf, to_repo);
	git_path_to_dir(&buf);
	if (git_buf_oom(&buf))
		return -1;

	/* don't write gitlink to natural workdir */
	if (git__suffixcmp(to_repo, "/" DOT_GIT "/") == 0 &&
		strcmp(in_dir, buf.ptr) == 0)
	{
		error = GIT_PASSTHROUGH;
		goto cleanup;
	}

	if ((error = git_buf_joinpath(&buf, in_dir, DOT_GIT)) < 0)
		goto cleanup;

	if (!p_stat(buf.ptr, &st) && !S_ISREG(st.st_mode)) {
		giterr_set(GITERR_REPOSITORY,
			"cannot overwrite gitlink file into path '%s'", in_dir);
		error = GIT_EEXISTS;
		goto cleanup;
	}

	git_buf_clear(&buf);

	error = git_buf_sets(&path_to_repo, to_repo);

	if (!error && use_relative_path)
		error = git_path_make_relative(&path_to_repo, in_dir);

	if (!error)
		error = git_buf_join(&buf, ' ', GIT_FILE_CONTENT_PREFIX, path_to_repo.ptr);

	if (!error)
		error = repo_write_template(in_dir, true, DOT_GIT, 0666, true, buf.ptr);

cleanup:
	git_buf_free(&buf);
	git_buf_free(&path_to_repo);
	return error;
}

static mode_t pick_dir_mode(git_repository_init_options *opts)
{
	if (opts->mode == GIT_REPOSITORY_INIT_SHARED_UMASK)
		return 0777;
	if (opts->mode == GIT_REPOSITORY_INIT_SHARED_GROUP)
		return (0775 | S_ISGID);
	if (opts->mode == GIT_REPOSITORY_INIT_SHARED_ALL)
		return (0777 | S_ISGID);
	return opts->mode;
}

#include "repo_template.h"

static int repo_init_structure(
	const char *repo_dir,
	const char *work_dir,
	git_repository_init_options *opts)
{
	int error = 0;
	repo_template_item *tpl;
	bool external_tpl =
		((opts->flags & GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE) != 0);
	mode_t dmode = pick_dir_mode(opts);
	bool chmod = opts->mode != GIT_REPOSITORY_INIT_SHARED_UMASK;

	/* Hide the ".git" directory */
#ifdef GIT_WIN32
	if ((opts->flags & GIT_REPOSITORY_INIT__HAS_DOTGIT) != 0) {
		if (git_win32__set_hidden(repo_dir, true) < 0) {
			giterr_set(GITERR_OS,
				"failed to mark Git repository folder as hidden");
			return -1;
		}
	}
#endif

	/* Create the .git gitlink if appropriate */
	if ((opts->flags & GIT_REPOSITORY_INIT_BARE) == 0 &&
		(opts->flags & GIT_REPOSITORY_INIT__NATURAL_WD) == 0)
	{
		if (repo_write_gitlink(work_dir, repo_dir, opts->flags & GIT_REPOSITORY_INIT_RELATIVE_GITLINK) < 0)
			return -1;
	}

	/* Copy external template if requested */
	if (external_tpl) {
		git_config *cfg = NULL;
		const char *tdir = NULL;
		bool default_template = false;
		git_buf template_buf = GIT_BUF_INIT;

		if (opts->template_path)
			tdir = opts->template_path;
		else if ((error = git_config_open_default(&cfg)) >= 0) {
			if (!git_config_get_path(&template_buf, cfg, "init.templatedir"))
				tdir = template_buf.ptr;
			giterr_clear();
		}

		if (!tdir) {
			if (!(error = git_sysdir_find_template_dir(&template_buf)))
				tdir = template_buf.ptr;
			default_template = true;
		}

		/*
		 * If tdir was the empty string, treat it like tdir was a path to an
		 * empty directory (so, don't do any copying). This is the behavior
		 * that git(1) exhibits, although it doesn't seem to be officially
		 * documented.
		 */
		if (tdir && git__strcmp(tdir, "") != 0) {
			uint32_t cpflags = GIT_CPDIR_COPY_SYMLINKS |
				GIT_CPDIR_SIMPLE_TO_MODE |
				GIT_CPDIR_COPY_DOTFILES;
			if (opts->mode != GIT_REPOSITORY_INIT_SHARED_UMASK)
					cpflags |= GIT_CPDIR_CHMOD_DIRS;
			error = git_futils_cp_r(tdir, repo_dir, cpflags, dmode);
		}

		git_buf_free(&template_buf);
		git_config_free(cfg);

		if (error < 0) {
			if (!default_template)
				return error;

			/* if template was default, ignore error and use internal */
			giterr_clear();
			external_tpl = false;
			error = 0;
		}
	}

	/* Copy internal template
	 * - always ensure existence of dirs
	 * - only create files if no external template was specified
	 */
	for (tpl = repo_template; !error && tpl->path; ++tpl) {
		if (!tpl->content) {
			uint32_t mkdir_flags = GIT_MKDIR_PATH;
			if (chmod)
				mkdir_flags |= GIT_MKDIR_CHMOD;

			error = git_futils_mkdir_relative(
				tpl->path, repo_dir, dmode, mkdir_flags, NULL);
		}
		else if (!external_tpl) {
			const char *content = tpl->content;

			if (opts->description && strcmp(tpl->path, GIT_DESC_FILE) == 0)
				content = opts->description;

			error = repo_write_template(
				repo_dir, false, tpl->path, tpl->mode, false, content);
		}
	}

	return error;
}

static int mkdir_parent(git_buf *buf, uint32_t mode, bool skip2)
{
	/* When making parent directories during repository initialization
	 * don't try to set gid or grant world write access
	 */
	return git_futils_mkdir(
		buf->ptr, mode & ~(S_ISGID | 0002),
		GIT_MKDIR_PATH | GIT_MKDIR_VERIFY_DIR |
		(skip2 ? GIT_MKDIR_SKIP_LAST2 : GIT_MKDIR_SKIP_LAST));
}

static int repo_init_directories(
	git_buf *repo_path,
	git_buf *wd_path,
	const char *given_repo,
	git_repository_init_options *opts)
{
	int error = 0;
	bool is_bare, add_dotgit, has_dotgit, natural_wd;
	mode_t dirmode;

	/* There are three possible rules for what we are allowed to create:
	 * - MKPATH means anything we need
	 * - MKDIR means just the .git directory and its parent and the workdir
	 * - Neither means only the .git directory can be created
	 *
	 * There are 5 "segments" of path that we might need to deal with:
	 * 1. The .git directory
	 * 2. The parent of the .git directory
	 * 3. Everything above the parent of the .git directory
	 * 4. The working directory (often the same as #2)
	 * 5. Everything above the working directory (often the same as #3)
	 *
	 * For all directories created, we start with the init_mode value for
	 * permissions and then strip off bits in some cases:
	 *
	 * For MKPATH, we create #3 (and #5) paths without S_ISGID or S_IWOTH
	 * For MKPATH and MKDIR, we create #2 (and #4) without S_ISGID
	 * For all rules, we create #1 using the untouched init_mode
	 */

	/* set up repo path */

	is_bare = ((opts->flags & GIT_REPOSITORY_INIT_BARE) != 0);

	add_dotgit =
		(opts->flags & GIT_REPOSITORY_INIT_NO_DOTGIT_DIR) == 0 &&
		!is_bare &&
		git__suffixcmp(given_repo, "/" DOT_GIT) != 0 &&
		git__suffixcmp(given_repo, "/" GIT_DIR) != 0;

	if (git_buf_joinpath(repo_path, given_repo, add_dotgit ? GIT_DIR : "") < 0)
		return -1;

	has_dotgit = (git__suffixcmp(repo_path->ptr, "/" GIT_DIR) == 0);
	if (has_dotgit)
		opts->flags |= GIT_REPOSITORY_INIT__HAS_DOTGIT;

	/* set up workdir path */

	if (!is_bare) {
		if (opts->workdir_path) {
			if (git_path_join_unrooted(
					wd_path, opts->workdir_path, repo_path->ptr, NULL) < 0)
				return -1;
		} else if (has_dotgit) {
			if (git_path_dirname_r(wd_path, repo_path->ptr) < 0)
				return -1;
		} else {
			giterr_set(GITERR_REPOSITORY, "cannot pick working directory"
				" for non-bare repository that isn't a '.git' directory");
			return -1;
		}

		if (git_path_to_dir(wd_path) < 0)
			return -1;
	} else {
		git_buf_clear(wd_path);
	}

	natural_wd =
		has_dotgit &&
		wd_path->size > 0 &&
		wd_path->size + strlen(GIT_DIR) == repo_path->size &&
		memcmp(repo_path->ptr, wd_path->ptr, wd_path->size) == 0;
	if (natural_wd)
		opts->flags |= GIT_REPOSITORY_INIT__NATURAL_WD;

	/* create directories as needed / requested */

	dirmode = pick_dir_mode(opts);

	if ((opts->flags & GIT_REPOSITORY_INIT_MKPATH) != 0) {
		/* create path #5 */
		if (wd_path->size > 0 &&
			(error = mkdir_parent(wd_path, dirmode, false)) < 0)
			return error;

		/* create path #3 (if not the same as #5) */
		if (!natural_wd &&
			(error = mkdir_parent(repo_path, dirmode, has_dotgit)) < 0)
			return error;
	}

	if ((opts->flags & GIT_REPOSITORY_INIT_MKDIR) != 0 ||
		(opts->flags & GIT_REPOSITORY_INIT_MKPATH) != 0)
	{
		/* create path #4 */
		if (wd_path->size > 0 &&
			(error = git_futils_mkdir(
				wd_path->ptr, dirmode & ~S_ISGID,
				GIT_MKDIR_VERIFY_DIR)) < 0)
			return error;

		/* create path #2 (if not the same as #4) */
		if (!natural_wd &&
			(error = git_futils_mkdir(
				repo_path->ptr, dirmode & ~S_ISGID,
				GIT_MKDIR_VERIFY_DIR | GIT_MKDIR_SKIP_LAST)) < 0)
			return error;
	}

	if ((opts->flags & GIT_REPOSITORY_INIT_MKDIR) != 0 ||
		(opts->flags & GIT_REPOSITORY_INIT_MKPATH) != 0 ||
		has_dotgit)
	{
		/* create path #1 */
		error = git_futils_mkdir(repo_path->ptr, dirmode,
			GIT_MKDIR_VERIFY_DIR | ((dirmode & S_ISGID) ? GIT_MKDIR_CHMOD : 0));
	}

	/* prettify both directories now that they are created */

	if (!error) {
		error = git_path_prettify_dir(repo_path, repo_path->ptr, NULL);

		if (!error && wd_path->size > 0)
			error = git_path_prettify_dir(wd_path, wd_path->ptr, NULL);
	}

	return error;
}

static int repo_init_create_origin(git_repository *repo, const char *url)
{
	int error;
	git_remote *remote;

	if (!(error = git_remote_create(&remote, repo, GIT_REMOTE_ORIGIN, url))) {
		git_remote_free(remote);
	}

	return error;
}

int git_repository_init(
	git_repository **repo_out, const char *path, unsigned is_bare)
{
	git_repository_init_options opts = GIT_REPOSITORY_INIT_OPTIONS_INIT;

	opts.flags = GIT_REPOSITORY_INIT_MKPATH; /* don't love this default */
	if (is_bare)
		opts.flags |= GIT_REPOSITORY_INIT_BARE;

	return git_repository_init_ext(repo_out, path, &opts);
}

int git_repository_init_ext(
	git_repository **out,
	const char *given_repo,
	git_repository_init_options *opts)
{
	int error;
	git_buf repo_path = GIT_BUF_INIT, wd_path = GIT_BUF_INIT,
		common_path = GIT_BUF_INIT;
	const char *wd;

	assert(out && given_repo && opts);

	GITERR_CHECK_VERSION(opts, GIT_REPOSITORY_INIT_OPTIONS_VERSION, "git_repository_init_options");

	error = repo_init_directories(&repo_path, &wd_path, given_repo, opts);
	if (error < 0)
		goto cleanup;

	wd = (opts->flags & GIT_REPOSITORY_INIT_BARE) ? NULL : git_buf_cstr(&wd_path);
	if (valid_repository_path(&repo_path, &common_path)) {

		if ((opts->flags & GIT_REPOSITORY_INIT_NO_REINIT) != 0) {
			giterr_set(GITERR_REPOSITORY,
				"attempt to reinitialize '%s'", given_repo);
			error = GIT_EEXISTS;
			goto cleanup;
		}

		opts->flags |= GIT_REPOSITORY_INIT__IS_REINIT;

		error = repo_init_config(
			repo_path.ptr, wd, opts->flags, opts->mode);

		/* TODO: reinitialize the templates */
	}
	else {
		if (!(error = repo_init_structure(
				repo_path.ptr, wd, opts)) &&
			!(error = repo_init_config(
				repo_path.ptr, wd, opts->flags, opts->mode)))
			error = git_repository_create_head(
				repo_path.ptr, opts->initial_head);
	}
	if (error < 0)
		goto cleanup;

	error = git_repository_open(out, repo_path.ptr);

	if (!error && opts->origin_url)
		error = repo_init_create_origin(*out, opts->origin_url);

cleanup:
	git_buf_free(&common_path);
	git_buf_free(&repo_path);
	git_buf_free(&wd_path);

	return error;
}

int git_repository_head_detached(git_repository *repo)
{
	git_reference *ref;
	git_odb *odb = NULL;
	int exists;

	if (git_repository_odb__weakptr(&odb, repo) < 0)
		return -1;

	if (git_reference_lookup(&ref, repo, GIT_HEAD_FILE) < 0)
		return -1;

	if (git_reference_type(ref) == GIT_REF_SYMBOLIC) {
		git_reference_free(ref);
		return 0;
	}

	exists = git_odb_exists(odb, git_reference_target(ref));

	git_reference_free(ref);
	return exists;
}

static int get_worktree_file_path(git_buf *out, git_repository *repo, const char *worktree, const char *file)
{
	git_buf_clear(out);
	return git_buf_printf(out, "%s/worktrees/%s/%s", repo->commondir, worktree, file);
}

int git_repository_head_detached_for_worktree(git_repository *repo, const char *name)
{
	git_reference *ref = NULL;
	int error;

	assert(repo && name);

	if ((error = git_repository_head_for_worktree(&ref, repo, name)) < 0)
		goto out;

	error = (git_reference_type(ref) != GIT_REF_SYMBOLIC);
out:
	git_reference_free(ref);

	return error;
}

int git_repository_head(git_reference **head_out, git_repository *repo)
{
	git_reference *head;
	int error;

	if ((error = git_reference_lookup(&head, repo, GIT_HEAD_FILE)) < 0)
		return error;

	if (git_reference_type(head) == GIT_REF_OID) {
		*head_out = head;
		return 0;
	}

	error = git_reference_lookup_resolved(head_out, repo, git_reference_symbolic_target(head), -1);
	git_reference_free(head);

	return error == GIT_ENOTFOUND ? GIT_EUNBORNBRANCH : error;
}

int git_repository_head_for_worktree(git_reference **out, git_repository *repo, const char *name)
{
	git_buf path = GIT_BUF_INIT;
	git_reference *head = NULL;
	int error;

	assert(out && repo && name);

	*out = NULL;

	if ((error = get_worktree_file_path(&path, repo, name, GIT_HEAD_FILE)) < 0 ||
	    (error = git_reference__read_head(&head, repo, path.ptr)) < 0)
		goto out;

	if (git_reference_type(head) != GIT_REF_OID) {
		git_reference *resolved;

		error = git_reference_lookup_resolved(&resolved, repo, git_reference_symbolic_target(head), -1);
		git_reference_free(head);
		head = resolved;
	}

	*out = head;

out:
	if (error)
		git_reference_free(head);

	git_buf_free(&path);

	return error;
}

int git_repository_foreach_head(git_repository *repo, git_repository_foreach_head_cb cb, void *payload)
{
	git_strarray worktrees = GIT_VECTOR_INIT;
	git_buf path = GIT_BUF_INIT;
	int error;
	size_t i;

	/* Execute callback for HEAD of commondir */
	if ((error = git_buf_joinpath(&path, repo->commondir, GIT_HEAD_FILE)) < 0 ||
	    (error = cb(repo, path.ptr, payload) != 0))
		goto out;

	if ((error = git_worktree_list(&worktrees, repo)) < 0) {
		error = 0;
		goto out;
	}

	/* Execute callback for all worktree HEADs */
	for (i = 0; i < worktrees.count; i++) {
		if (get_worktree_file_path(&path, repo, worktrees.strings[i], GIT_HEAD_FILE) < 0)
			continue;

		if ((error = cb(repo, path.ptr, payload)) != 0)
			goto out;
	}

out:
	git_buf_free(&path);
	git_strarray_free(&worktrees);
	return error;
}

int git_repository_head_unborn(git_repository *repo)
{
	git_reference *ref = NULL;
	int error;

	error = git_repository_head(&ref, repo);
	git_reference_free(ref);

	if (error == GIT_EUNBORNBRANCH) {
		giterr_clear();
		return 1;
	}

	if (error < 0)
		return -1;

	return 0;
}

static int at_least_one_cb(const char *refname, void *payload)
{
	GIT_UNUSED(refname);
	GIT_UNUSED(payload);
	return GIT_PASSTHROUGH;
}

static int repo_contains_no_reference(git_repository *repo)
{
	int error = git_reference_foreach_name(repo, &at_least_one_cb, NULL);

	if (error == GIT_PASSTHROUGH)
		return 0;

	if (!error)
		return 1;

	return error;
}

int git_repository_is_empty(git_repository *repo)
{
	git_reference *head = NULL;
	int is_empty = 0;

	if (git_reference_lookup(&head, repo, GIT_HEAD_FILE) < 0)
		return -1;

	if (git_reference_type(head) == GIT_REF_SYMBOLIC)
		is_empty =
			(strcmp(git_reference_symbolic_target(head),
					GIT_REFS_HEADS_DIR "master") == 0) &&
			repo_contains_no_reference(repo);

	git_reference_free(head);

	return is_empty;
}

int git_repository_item_path(git_buf *out, git_repository *repo, git_repository_item_t item)
{
	const char *parent;

	switch (items[item].parent) {
		case GIT_REPOSITORY_ITEM_GITDIR:
			parent = git_repository_path(repo);
			break;
		case GIT_REPOSITORY_ITEM_WORKDIR:
			parent = git_repository_workdir(repo);
			break;
		case GIT_REPOSITORY_ITEM_COMMONDIR:
			parent = git_repository_commondir(repo);
			break;
		default:
			giterr_set(GITERR_INVALID, "invalid item directory");
			return -1;
	}

	if (parent == NULL) {
		giterr_set(GITERR_INVALID, "path cannot exist in repository");
		return GIT_ENOTFOUND;
	}

	if (git_buf_sets(out, parent) < 0)
		return -1;

	if (items[item].name) {
		if (git_buf_joinpath(out, parent, items[item].name) < 0)
			return -1;
	}

	if (items[item].directory) {
		if (git_path_to_dir(out) < 0)
			return -1;
	}

	return 0;
}

const char *git_repository_path(git_repository *repo)
{
	assert(repo);
	return repo->gitdir;
}

const char *git_repository_workdir(git_repository *repo)
{
	assert(repo);

	if (repo->is_bare)
		return NULL;

	return repo->workdir;
}

const char *git_repository_commondir(git_repository *repo)
{
	assert(repo);
	return repo->commondir;
}

int git_repository_set_workdir(
	git_repository *repo, const char *workdir, int update_gitlink)
{
	int error = 0;
	git_buf path = GIT_BUF_INIT;

	assert(repo && workdir);

	if (git_path_prettify_dir(&path, workdir, NULL) < 0)
		return -1;

	if (repo->workdir && strcmp(repo->workdir, path.ptr) == 0)
		return 0;

	if (update_gitlink) {
		git_config *config;

		if (git_repository_config__weakptr(&config, repo) < 0)
			return -1;

		error = repo_write_gitlink(path.ptr, git_repository_path(repo), false);

		/* passthrough error means gitlink is unnecessary */
		if (error == GIT_PASSTHROUGH)
			error = git_config_delete_entry(config, "core.worktree");
		else if (!error)
			error = git_config_set_string(config, "core.worktree", path.ptr);

		if (!error)
			error = git_config_set_bool(config, "core.bare", false);
	}

	if (!error) {
		char *old_workdir = repo->workdir;

		repo->workdir = git_buf_detach(&path);
		repo->is_bare = 0;

		git__free(old_workdir);
	}

	return error;
}

int git_repository_is_bare(git_repository *repo)
{
	assert(repo);
	return repo->is_bare;
}

int git_repository_is_worktree(git_repository *repo)
{
	assert(repo);
	return repo->is_worktree;
}

int git_repository_set_bare(git_repository *repo)
{
	int error;
	git_config *config;

	assert(repo);

	if (repo->is_bare)
		return 0;

	if ((error = git_repository_config__weakptr(&config, repo)) < 0)
		return error;

	if ((error = git_config_set_bool(config, "core.bare", true)) < 0)
		return error;

	if ((error = git_config__update_entry(config, "core.worktree", NULL, true, true)) < 0)
		return error;

	git__free(repo->workdir);
	repo->workdir = NULL;
	repo->is_bare = 1;

	return 0;
}

int git_repository_head_tree(git_tree **tree, git_repository *repo)
{
	git_reference *head;
	git_object *obj;
	int error;

	if ((error = git_repository_head(&head, repo)) < 0)
		return error;

	if ((error = git_reference_peel(&obj, head, GIT_OBJ_TREE)) < 0)
		goto cleanup;

	*tree = (git_tree *)obj;

cleanup:
	git_reference_free(head);
	return error;
}

int git_repository__set_orig_head(git_repository *repo, const git_oid *orig_head)
{
	git_filebuf file = GIT_FILEBUF_INIT;
	git_buf file_path = GIT_BUF_INIT;
	char orig_head_str[GIT_OID_HEXSZ];
	int error = 0;

	git_oid_fmt(orig_head_str, orig_head);

	if ((error = git_buf_joinpath(&file_path, repo->gitdir, GIT_ORIG_HEAD_FILE)) == 0 &&
		(error = git_filebuf_open(&file, file_path.ptr, GIT_FILEBUF_FORCE, GIT_MERGE_FILE_MODE)) == 0 &&
		(error = git_filebuf_printf(&file, "%.*s\n", GIT_OID_HEXSZ, orig_head_str)) == 0)
		error = git_filebuf_commit(&file);

	if (error < 0)
		git_filebuf_cleanup(&file);

	git_buf_free(&file_path);

	return error;
}

int git_repository_message(git_buf *out, git_repository *repo)
{
	git_buf path = GIT_BUF_INIT;
	struct stat st;
	int error;

	git_buf_sanitize(out);

	if (git_buf_joinpath(&path, repo->gitdir, GIT_MERGE_MSG_FILE) < 0)
		return -1;

	if ((error = p_stat(git_buf_cstr(&path), &st)) < 0) {
		if (errno == ENOENT)
			error = GIT_ENOTFOUND;
		giterr_set(GITERR_OS, "could not access message file");
	} else {
		error = git_futils_readbuffer(out, git_buf_cstr(&path));
	}

	git_buf_free(&path);

	return error;
}

int git_repository_message_remove(git_repository *repo)
{
	git_buf path = GIT_BUF_INIT;
	int error;

	if (git_buf_joinpath(&path, repo->gitdir, GIT_MERGE_MSG_FILE) < 0)
		return -1;

	error = p_unlink(git_buf_cstr(&path));
	git_buf_free(&path);

	return error;
}

int git_repository_hashfile(
	git_oid *out,
	git_repository *repo,
	const char *path,
	git_otype type,
	const char *as_path)
{
	int error;
	git_filter_list *fl = NULL;
	git_file fd = -1;
	git_off_t len;
	git_buf full_path = GIT_BUF_INIT;

	assert(out && path && repo); /* as_path can be NULL */

	/* At some point, it would be nice if repo could be NULL to just
	 * apply filter rules defined in system and global files, but for
	 * now that is not possible because git_filters_load() needs it.
	 */

	error = git_path_join_unrooted(
		&full_path, path, git_repository_workdir(repo), NULL);
	if (error < 0)
		return error;

	if (!as_path)
		as_path = path;

	/* passing empty string for "as_path" indicated --no-filters */
	if (strlen(as_path) > 0) {
		error = git_filter_list_load(
			&fl, repo, NULL, as_path,
			GIT_FILTER_TO_ODB, GIT_FILTER_DEFAULT);
		if (error < 0)
			return error;
	} else {
		error = 0;
	}

	/* at this point, error is a count of the number of loaded filters */

	fd = git_futils_open_ro(full_path.ptr);
	if (fd < 0) {
		error = fd;
		goto cleanup;
	}

	len = git_futils_filesize(fd);
	if (len < 0) {
		error = (int)len;
		goto cleanup;
	}

	if (!git__is_sizet(len)) {
		giterr_set(GITERR_OS, "file size overflow for 32-bit systems");
		error = -1;
		goto cleanup;
	}

	error = git_odb__hashfd_filtered(out, fd, (size_t)len, type, fl);

cleanup:
	if (fd >= 0)
		p_close(fd);
	git_filter_list_free(fl);
	git_buf_free(&full_path);

	return error;
}

static int checkout_message(git_buf *out, git_reference *old, const char *new)
{
	git_buf_puts(out, "checkout: moving from ");

	if (git_reference_type(old) == GIT_REF_SYMBOLIC)
		git_buf_puts(out, git_reference__shorthand(git_reference_symbolic_target(old)));
	else
		git_buf_puts(out, git_oid_tostr_s(git_reference_target(old)));

	git_buf_puts(out, " to ");

	if (git_reference__is_branch(new) ||
		git_reference__is_tag(new) ||
		git_reference__is_remote(new))
		git_buf_puts(out, git_reference__shorthand(new));
	else
		git_buf_puts(out, new);

	if (git_buf_oom(out))
		return -1;

	return 0;
}

static int detach(git_repository *repo, const git_oid *id, const char *new)
{
	int error;
	git_buf log_message = GIT_BUF_INIT;
	git_object *object = NULL, *peeled = NULL;
	git_reference *new_head = NULL, *current = NULL;

	assert(repo && id);

	if ((error = git_reference_lookup(&current, repo, GIT_HEAD_FILE)) < 0)
		return error;

	if ((error = git_object_lookup(&object, repo, id, GIT_OBJ_ANY)) < 0)
		goto cleanup;

	if ((error = git_object_peel(&peeled, object, GIT_OBJ_COMMIT)) < 0)
		goto cleanup;

	if (new == NULL)
		new = git_oid_tostr_s(git_object_id(peeled));

	if ((error = checkout_message(&log_message, current, new)) < 0)
		goto cleanup;

	error = git_reference_create(&new_head, repo, GIT_HEAD_FILE, git_object_id(peeled), true, git_buf_cstr(&log_message));

cleanup:
	git_buf_free(&log_message);
	git_object_free(object);
	git_object_free(peeled);
	git_reference_free(current);
	git_reference_free(new_head);
	return error;
}

int git_repository_set_head(
	git_repository* repo,
	const char* refname)
{
	git_reference *ref = NULL, *current = NULL, *new_head = NULL;
	git_buf log_message = GIT_BUF_INIT;
	int error;

	assert(repo && refname);

	if ((error = git_reference_lookup(&current, repo, GIT_HEAD_FILE)) < 0)
		return error;

	if ((error = checkout_message(&log_message, current, refname)) < 0)
		goto cleanup;

	error = git_reference_lookup(&ref, repo, refname);
	if (error < 0 && error != GIT_ENOTFOUND)
		goto cleanup;

	if (ref && current->type == GIT_REF_SYMBOLIC && git__strcmp(current->target.symbolic, ref->name) &&
	    git_reference_is_branch(ref) && git_branch_is_checked_out(ref)) {
		giterr_set(GITERR_REPOSITORY, "cannot set HEAD to reference '%s' as it is the current HEAD "
			"of a linked repository.", git_reference_name(ref));
		error = -1;
		goto cleanup;
	}

	if (!error) {
		if (git_reference_is_branch(ref)) {
			error = git_reference_symbolic_create(&new_head, repo, GIT_HEAD_FILE,
					git_reference_name(ref), true, git_buf_cstr(&log_message));
		} else {
			error = detach(repo, git_reference_target(ref),
				git_reference_is_tag(ref) || git_reference_is_remote(ref) ? refname : NULL);
		}
	} else if (git_reference__is_branch(refname)) {
		error = git_reference_symbolic_create(&new_head, repo, GIT_HEAD_FILE, refname,
				true, git_buf_cstr(&log_message));
	}

cleanup:
	git_buf_free(&log_message);
	git_reference_free(current);
	git_reference_free(ref);
	git_reference_free(new_head);
	return error;
}

int git_repository_set_head_detached(
	git_repository* repo,
	const git_oid* commitish)
{
	return detach(repo, commitish, NULL);
}

int git_repository_set_head_detached_from_annotated(
	git_repository *repo,
	const git_annotated_commit *commitish)
{
	assert(repo && commitish);

	return detach(repo, git_annotated_commit_id(commitish), commitish->description);
}

int git_repository_detach_head(git_repository* repo)
{
	git_reference *old_head = NULL,	*new_head = NULL, *current = NULL;
	git_object *object = NULL;
	git_buf log_message = GIT_BUF_INIT;
	int error;

	assert(repo);

	if ((error = git_reference_lookup(&current, repo, GIT_HEAD_FILE)) < 0)
		return error;

	if ((error = git_repository_head(&old_head, repo)) < 0)
		goto cleanup;

	if ((error = git_object_lookup(&object, repo, git_reference_target(old_head), GIT_OBJ_COMMIT)) < 0)
		goto cleanup;

	if ((error = checkout_message(&log_message, current, git_oid_tostr_s(git_object_id(object)))) < 0)
		goto cleanup;

	error = git_reference_create(&new_head, repo, GIT_HEAD_FILE, git_reference_target(old_head),
			1, git_buf_cstr(&log_message));

cleanup:
	git_buf_free(&log_message);
	git_object_free(object);
	git_reference_free(old_head);
	git_reference_free(new_head);
	git_reference_free(current);
	return error;
}

/**
 * Loosely ported from git.git
 * https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh#L198-289
 */
int git_repository_state(git_repository *repo)
{
	git_buf repo_path = GIT_BUF_INIT;
	int state = GIT_REPOSITORY_STATE_NONE;

	assert(repo);

	if (git_buf_puts(&repo_path, repo->gitdir) < 0)
		return -1;

	if (git_path_contains_file(&repo_path, GIT_REBASE_MERGE_INTERACTIVE_FILE))
		state = GIT_REPOSITORY_STATE_REBASE_INTERACTIVE;
	else if (git_path_contains_dir(&repo_path, GIT_REBASE_MERGE_DIR))
		state = GIT_REPOSITORY_STATE_REBASE_MERGE;
	else if (git_path_contains_file(&repo_path, GIT_REBASE_APPLY_REBASING_FILE))
		state = GIT_REPOSITORY_STATE_REBASE;
	else if (git_path_contains_file(&repo_path, GIT_REBASE_APPLY_APPLYING_FILE))
		state = GIT_REPOSITORY_STATE_APPLY_MAILBOX;
	else if (git_path_contains_dir(&repo_path, GIT_REBASE_APPLY_DIR))
		state = GIT_REPOSITORY_STATE_APPLY_MAILBOX_OR_REBASE;
	else if (git_path_contains_file(&repo_path, GIT_MERGE_HEAD_FILE))
		state = GIT_REPOSITORY_STATE_MERGE;
	else if (git_path_contains_file(&repo_path, GIT_REVERT_HEAD_FILE)) {
		state = GIT_REPOSITORY_STATE_REVERT;
		if (git_path_contains_file(&repo_path, GIT_SEQUENCER_TODO_FILE)) {
			state = GIT_REPOSITORY_STATE_REVERT_SEQUENCE;
		}
	} else if (git_path_contains_file(&repo_path, GIT_CHERRYPICK_HEAD_FILE)) {
		state = GIT_REPOSITORY_STATE_CHERRYPICK;
		if (git_path_contains_file(&repo_path, GIT_SEQUENCER_TODO_FILE)) {
			state = GIT_REPOSITORY_STATE_CHERRYPICK_SEQUENCE;
		}
	} else if (git_path_contains_file(&repo_path, GIT_BISECT_LOG_FILE))
		state = GIT_REPOSITORY_STATE_BISECT;

	git_buf_free(&repo_path);
	return state;
}

int git_repository__cleanup_files(
	git_repository *repo, const char *files[], size_t files_len)
{
	git_buf buf = GIT_BUF_INIT;
	size_t i;
	int error;

	for (error = 0, i = 0; !error && i < files_len; ++i) {
		const char *path;

		if (git_buf_joinpath(&buf, repo->gitdir, files[i]) < 0)
			return -1;

		path = git_buf_cstr(&buf);

		if (git_path_isfile(path)) {
			error = p_unlink(path);
		} else if (git_path_isdir(path)) {
			error = git_futils_rmdir_r(path, NULL,
				GIT_RMDIR_REMOVE_FILES | GIT_RMDIR_REMOVE_BLOCKERS);
		}

		git_buf_clear(&buf);
	}

	git_buf_free(&buf);
	return error;
}

static const char *state_files[] = {
	GIT_MERGE_HEAD_FILE,
	GIT_MERGE_MODE_FILE,
	GIT_MERGE_MSG_FILE,
	GIT_REVERT_HEAD_FILE,
	GIT_CHERRYPICK_HEAD_FILE,
	GIT_BISECT_LOG_FILE,
	GIT_REBASE_MERGE_DIR,
	GIT_REBASE_APPLY_DIR,
	GIT_SEQUENCER_DIR,
};

int git_repository_state_cleanup(git_repository *repo)
{
	assert(repo);

	return git_repository__cleanup_files(repo, state_files, ARRAY_SIZE(state_files));
}

int git_repository_is_shallow(git_repository *repo)
{
	git_buf path = GIT_BUF_INIT;
	struct stat st;
	int error;

	if ((error = git_buf_joinpath(&path, repo->gitdir, "shallow")) < 0)
		return error;

	error = git_path_lstat(path.ptr, &st);
	git_buf_free(&path);

	if (error == GIT_ENOTFOUND) {
		giterr_clear();
		return 0;
	}

	if (error < 0)
		return error;
	return st.st_size == 0 ? 0 : 1;
}

int git_repository_init_init_options(
	git_repository_init_options *opts, unsigned int version)
{
	GIT_INIT_STRUCTURE_FROM_TEMPLATE(
		opts, version, git_repository_init_options,
		GIT_REPOSITORY_INIT_OPTIONS_INIT);
	return 0;
}

int git_repository_ident(const char **name, const char **email, const git_repository *repo)
{
	*name = repo->ident_name;
	*email = repo->ident_email;

	return 0;
}

int git_repository_set_ident(git_repository *repo, const char *name, const char *email)
{
	char *tmp_name = NULL, *tmp_email = NULL;

	if (name) {
		tmp_name = git__strdup(name);
		GITERR_CHECK_ALLOC(tmp_name);
	}

	if (email) {
		tmp_email = git__strdup(email);
		GITERR_CHECK_ALLOC(tmp_email);
	}

	tmp_name = git__swap(repo->ident_name, tmp_name);
	tmp_email = git__swap(repo->ident_email, tmp_email);

	git__free(tmp_name);
	git__free(tmp_email);

	return 0;
}

int git_repository_submodule_cache_all(git_repository *repo)
{
	int error;

	assert(repo);

	if ((error = git_strmap_alloc(&repo->submodule_cache)))
		return error;

	error = git_submodule__map(repo, repo->submodule_cache);
	return error;
}

int git_repository_submodule_cache_clear(git_repository *repo)
{
	git_submodule *sm;
	assert(repo);
	if (repo->submodule_cache == NULL) {
		return 0;
	}
	git_strmap_foreach_value(repo->submodule_cache, sm, {
		git_submodule_free(sm);
	});
	git_strmap_free(repo->submodule_cache);
	repo->submodule_cache = 0;
	return 0;
}
