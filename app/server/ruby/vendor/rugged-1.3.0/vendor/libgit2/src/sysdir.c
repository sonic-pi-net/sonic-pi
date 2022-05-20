/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "sysdir.h"

#include "runtime.h"
#include "buffer.h"
#include "path.h"
#include <ctype.h>
#if GIT_WIN32
#include "win32/findfile.h"
#else
#include <unistd.h>
#include <pwd.h>
#endif

static int git_sysdir_guess_programdata_dirs(git_buf *out)
{
#ifdef GIT_WIN32
	return git_win32__find_programdata_dirs(out);
#else
	git_buf_clear(out);
	return 0;
#endif
}

static int git_sysdir_guess_system_dirs(git_buf *out)
{
#ifdef GIT_WIN32
	return git_win32__find_system_dirs(out, L"etc\\");
#else
	return git_buf_sets(out, "/etc");
#endif
}

#ifndef GIT_WIN32
static int get_passwd_home(git_buf *out, uid_t uid)
{
	struct passwd pwd, *pwdptr;
	char *buf = NULL;
	long buflen;
	int error;

	GIT_ASSERT_ARG(out);

	if ((buflen = sysconf(_SC_GETPW_R_SIZE_MAX)) == -1)
		buflen = 1024;

	do {
		buf = git__realloc(buf, buflen);
		error = getpwuid_r(uid, &pwd, buf, buflen, &pwdptr);
		buflen *= 2;
	} while (error == ERANGE && buflen <= 8192);

	if (error) {
		git_error_set(GIT_ERROR_OS, "failed to get passwd entry");
		goto out;
	}

	if (!pwdptr) {
		git_error_set(GIT_ERROR_OS, "no passwd entry found for user");
		goto out;
	}

	if ((error = git_buf_puts(out, pwdptr->pw_dir)) < 0)
		goto out;

out:
	git__free(buf);
	return error;
}
#endif

static int git_sysdir_guess_global_dirs(git_buf *out)
{
#ifdef GIT_WIN32
	return git_win32__find_global_dirs(out);
#else
	int error;
	uid_t uid, euid;
	const char *sandbox_id;

	uid = getuid();
	euid = geteuid();

	/**
	 * If APP_SANDBOX_CONTAINER_ID is set, we are running in a
	 * sandboxed environment on macOS.
	 */
	sandbox_id = getenv("APP_SANDBOX_CONTAINER_ID");

	/*
	 * In case we are running setuid, use the configuration
	 * of the effective user.
	 *
	 * If we are running in a sandboxed environment on macOS,
	 * we have to get the HOME dir from the password entry file.
	 */
	if (!sandbox_id && uid == euid)
	    error = git__getenv(out, "HOME");
	else
	    error = get_passwd_home(out, euid);

	if (error == GIT_ENOTFOUND) {
		git_error_clear();
		error = 0;
	}

	return error;
#endif
}

static int git_sysdir_guess_xdg_dirs(git_buf *out)
{
#ifdef GIT_WIN32
	return git_win32__find_xdg_dirs(out);
#else
	git_buf env = GIT_BUF_INIT;
	int error;
	uid_t uid, euid;

	uid = getuid();
	euid = geteuid();

	/*
	 * In case we are running setuid, only look up passwd
	 * directory of the effective user.
	 */
	if (uid == euid) {
		if ((error = git__getenv(&env, "XDG_CONFIG_HOME")) == 0)
			error = git_buf_joinpath(out, env.ptr, "git");

		if (error == GIT_ENOTFOUND && (error = git__getenv(&env, "HOME")) == 0)
			error = git_buf_joinpath(out, env.ptr, ".config/git");
	} else {
		if ((error = get_passwd_home(&env, euid)) == 0)
			error = git_buf_joinpath(out, env.ptr, ".config/git");
	}

	if (error == GIT_ENOTFOUND) {
		git_error_clear();
		error = 0;
	}

	git_buf_dispose(&env);
	return error;
#endif
}

static int git_sysdir_guess_template_dirs(git_buf *out)
{
#ifdef GIT_WIN32
	return git_win32__find_system_dirs(out, L"share\\git-core\\templates");
#else
	return git_buf_sets(out, "/usr/share/git-core/templates");
#endif
}

struct git_sysdir__dir {
	git_buf buf;
	int (*guess)(git_buf *out);
};

static struct git_sysdir__dir git_sysdir__dirs[] = {
	{ GIT_BUF_INIT, git_sysdir_guess_system_dirs },
	{ GIT_BUF_INIT, git_sysdir_guess_global_dirs },
	{ GIT_BUF_INIT, git_sysdir_guess_xdg_dirs },
	{ GIT_BUF_INIT, git_sysdir_guess_programdata_dirs },
	{ GIT_BUF_INIT, git_sysdir_guess_template_dirs },
};

static void git_sysdir_global_shutdown(void)
{
	size_t i;

	for (i = 0; i < ARRAY_SIZE(git_sysdir__dirs); ++i)
		git_buf_dispose(&git_sysdir__dirs[i].buf);
}

int git_sysdir_global_init(void)
{
	size_t i;
	int error = 0;

	for (i = 0; !error && i < ARRAY_SIZE(git_sysdir__dirs); i++)
		error = git_sysdir__dirs[i].guess(&git_sysdir__dirs[i].buf);

	return git_runtime_shutdown_register(git_sysdir_global_shutdown);
}

static int git_sysdir_check_selector(git_sysdir_t which)
{
	if (which < ARRAY_SIZE(git_sysdir__dirs))
		return 0;

	git_error_set(GIT_ERROR_INVALID, "config directory selector out of range");
	return -1;
}


int git_sysdir_get(const git_buf **out, git_sysdir_t which)
{
	GIT_ASSERT_ARG(out);

	*out = NULL;

	GIT_ERROR_CHECK_ERROR(git_sysdir_check_selector(which));

	*out = &git_sysdir__dirs[which].buf;
	return 0;
}

#define PATH_MAGIC "$PATH"

int git_sysdir_set(git_sysdir_t which, const char *search_path)
{
	const char *expand_path = NULL;
	git_buf merge = GIT_BUF_INIT;

	GIT_ERROR_CHECK_ERROR(git_sysdir_check_selector(which));

	if (search_path != NULL)
		expand_path = strstr(search_path, PATH_MAGIC);

	/* reset the default if this path has been cleared */
	if (!search_path)
		git_sysdir__dirs[which].guess(&git_sysdir__dirs[which].buf);

	/* if $PATH is not referenced, then just set the path */
	if (!expand_path) {
		if (search_path)
			git_buf_sets(&git_sysdir__dirs[which].buf, search_path);

		goto done;
	}

	/* otherwise set to join(before $PATH, old value, after $PATH) */
	if (expand_path > search_path)
		git_buf_set(&merge, search_path, expand_path - search_path);

	if (git_buf_len(&git_sysdir__dirs[which].buf))
		git_buf_join(&merge, GIT_PATH_LIST_SEPARATOR,
			merge.ptr, git_sysdir__dirs[which].buf.ptr);

	expand_path += strlen(PATH_MAGIC);
	if (*expand_path)
		git_buf_join(&merge, GIT_PATH_LIST_SEPARATOR, merge.ptr, expand_path);

	git_buf_swap(&git_sysdir__dirs[which].buf, &merge);
	git_buf_dispose(&merge);

done:
	if (git_buf_oom(&git_sysdir__dirs[which].buf))
		return -1;

	return 0;
}

static int git_sysdir_find_in_dirlist(
	git_buf *path,
	const char *name,
	git_sysdir_t which,
	const char *label)
{
	size_t len;
	const char *scan, *next = NULL;
	const git_buf *syspath;

	GIT_ERROR_CHECK_ERROR(git_sysdir_get(&syspath, which));
	if (!syspath || !git_buf_len(syspath))
		goto done;

	for (scan = git_buf_cstr(syspath); scan; scan = next) {
		/* find unescaped separator or end of string */
		for (next = scan; *next; ++next) {
			if (*next == GIT_PATH_LIST_SEPARATOR &&
				(next <= scan || next[-1] != '\\'))
				break;
		}

		len = (size_t)(next - scan);
		next = (*next ? next + 1 : NULL);
		if (!len)
			continue;

		GIT_ERROR_CHECK_ERROR(git_buf_set(path, scan, len));
		if (name)
			GIT_ERROR_CHECK_ERROR(git_buf_joinpath(path, path->ptr, name));

		if (git_path_exists(path->ptr))
			return 0;
	}

done:
	if (name)
		git_error_set(GIT_ERROR_OS, "the %s file '%s' doesn't exist", label, name);
	else
		git_error_set(GIT_ERROR_OS, "the %s directory doesn't exist", label);
	git_buf_dispose(path);
	return GIT_ENOTFOUND;
}

int git_sysdir_find_system_file(git_buf *path, const char *filename)
{
	return git_sysdir_find_in_dirlist(
		path, filename, GIT_SYSDIR_SYSTEM, "system");
}

int git_sysdir_find_global_file(git_buf *path, const char *filename)
{
	return git_sysdir_find_in_dirlist(
		path, filename, GIT_SYSDIR_GLOBAL, "global");
}

int git_sysdir_find_xdg_file(git_buf *path, const char *filename)
{
	return git_sysdir_find_in_dirlist(
		path, filename, GIT_SYSDIR_XDG, "global/xdg");
}

int git_sysdir_find_programdata_file(git_buf *path, const char *filename)
{
	return git_sysdir_find_in_dirlist(
		path, filename, GIT_SYSDIR_PROGRAMDATA, "ProgramData");
}

int git_sysdir_find_template_dir(git_buf *path)
{
	return git_sysdir_find_in_dirlist(
		path, NULL, GIT_SYSDIR_TEMPLATE, "template");
}

int git_sysdir_expand_global_file(git_buf *path, const char *filename)
{
	int error;

	if ((error = git_sysdir_find_global_file(path, NULL)) == 0) {
		if (filename)
			error = git_buf_joinpath(path, path->ptr, filename);
	}

	return error;
}
