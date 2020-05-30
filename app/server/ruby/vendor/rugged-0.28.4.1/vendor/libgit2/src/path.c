/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */

#include "path.h"

#include "posix.h"
#include "repository.h"
#ifdef GIT_WIN32
#include "win32/posix.h"
#include "win32/w32_buffer.h"
#include "win32/w32_util.h"
#include "win32/version.h"
#include <aclapi.h>
#else
#include <dirent.h>
#endif
#include <stdio.h>
#include <ctype.h>

static int dos_drive_prefix_length(const char *path)
{
	int i;

	/*
	 * Does it start with an ASCII letter (i.e. highest bit not set),
	 * followed by a colon?
	 */
	if (!(0x80 & (unsigned char)*path))
		return *path && path[1] == ':' ? 2 : 0;

	/*
	 * While drive letters must be letters of the English alphabet, it is
	 * possible to assign virtually _any_ Unicode character via `subst` as
	 * a drive letter to "virtual drives". Even `1`, or `ä`. Or fun stuff
	 * like this:
	 *
	 *	subst ֍: %USERPROFILE%\Desktop
	 */
	for (i = 1; i < 4 && (0x80 & (unsigned char)path[i]); i++)
		; /* skip first UTF-8 character */
	return path[i] == ':' ? i + 1 : 0;
}

#ifdef GIT_WIN32
static bool looks_like_network_computer_name(const char *path, int pos)
{
	if (pos < 3)
		return false;

	if (path[0] != '/' || path[1] != '/')
		return false;

	while (pos-- > 2) {
		if (path[pos] == '/')
			return false;
	}

	return true;
}
#endif

/*
 * Based on the Android implementation, BSD licensed.
 * http://android.git.kernel.org/
 *
 * Copyright (C) 2008 The Android Open Source Project
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in
 *   the documentation and/or other materials provided with the
 *   distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * AS IS AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */
int git_path_basename_r(git_buf *buffer, const char *path)
{
	const char *endp, *startp;
	int len, result;

	/* Empty or NULL string gets treated as "." */
	if (path == NULL || *path == '\0') {
		startp = ".";
		len		= 1;
		goto Exit;
	}

	/* Strip trailing slashes */
	endp = path + strlen(path) - 1;
	while (endp > path && *endp == '/')
		endp--;

	/* All slashes becomes "/" */
	if (endp == path && *endp == '/') {
		startp = "/";
		len	= 1;
		goto Exit;
	}

	/* Find the start of the base */
	startp = endp;
	while (startp > path && *(startp - 1) != '/')
		startp--;

	/* Cast is safe because max path < max int */
	len = (int)(endp - startp + 1);

Exit:
	result = len;

	if (buffer != NULL && git_buf_set(buffer, startp, len) < 0)
		return -1;

	return result;
}

/*
 * Determine if the path is a Windows prefix and, if so, returns
 * its actual lentgh. If it is not a prefix, returns -1.
 */
static int win32_prefix_length(const char *path, int len)
{
#ifndef GIT_WIN32
	GIT_UNUSED(path);
	GIT_UNUSED(len);
#else
	/*
	 * Mimic unix behavior where '/.git' returns '/': 'C:/.git'
	 * will return 'C:/' here
	 */
	if (dos_drive_prefix_length(path) == len)
		return len;

	/*
	 * Similarly checks if we're dealing with a network computer name
	 * '//computername/.git' will return '//computername/'
	 */
	if (looks_like_network_computer_name(path, len))
		return len;
#endif

	return -1;
}

/*
 * Based on the Android implementation, BSD licensed.
 * Check http://android.git.kernel.org/
 */
int git_path_dirname_r(git_buf *buffer, const char *path)
{
	const char *endp;
	int is_prefix = 0, len;

	/* Empty or NULL string gets treated as "." */
	if (path == NULL || *path == '\0') {
		path = ".";
		len = 1;
		goto Exit;
	}

	/* Strip trailing slashes */
	endp = path + strlen(path) - 1;
	while (endp > path && *endp == '/')
		endp--;

	if (endp - path + 1 > INT_MAX) {
		git_error_set(GIT_ERROR_INVALID, "path too long");
		len = -1;
		goto Exit;
	}

	if ((len = win32_prefix_length(path, (int)(endp - path + 1))) > 0) {
		is_prefix = 1;
		goto Exit;
	}

	/* Find the start of the dir */
	while (endp > path && *endp != '/')
		endp--;

	/* Either the dir is "/" or there are no slashes */
	if (endp == path) {
		path = (*endp == '/') ? "/" : ".";
		len = 1;
		goto Exit;
	}

	do {
		endp--;
	} while (endp > path && *endp == '/');

	if (endp - path + 1 > INT_MAX) {
		git_error_set(GIT_ERROR_INVALID, "path too long");
		len = -1;
		goto Exit;
	}

	if ((len = win32_prefix_length(path, (int)(endp - path + 1))) > 0) {
		is_prefix = 1;
		goto Exit;
	}

	/* Cast is safe because max path < max int */
	len = (int)(endp - path + 1);

Exit:
	if (buffer) {
		if (git_buf_set(buffer, path, len) < 0)
			return -1;
		if (is_prefix && git_buf_putc(buffer, '/') < 0)
			return -1;
	}

	return len;
}


char *git_path_dirname(const char *path)
{
	git_buf buf = GIT_BUF_INIT;
	char *dirname;

	git_path_dirname_r(&buf, path);
	dirname = git_buf_detach(&buf);
	git_buf_dispose(&buf); /* avoid memleak if error occurs */

	return dirname;
}

char *git_path_basename(const char *path)
{
	git_buf buf = GIT_BUF_INIT;
	char *basename;

	git_path_basename_r(&buf, path);
	basename = git_buf_detach(&buf);
	git_buf_dispose(&buf); /* avoid memleak if error occurs */

	return basename;
}

size_t git_path_basename_offset(git_buf *buffer)
{
	ssize_t slash;

	if (!buffer || buffer->size <= 0)
		return 0;

	slash = git_buf_rfind_next(buffer, '/');

	if (slash >= 0 && buffer->ptr[slash] == '/')
		return (size_t)(slash + 1);

	return 0;
}

const char *git_path_topdir(const char *path)
{
	size_t len;
	ssize_t i;

	assert(path);
	len = strlen(path);

	if (!len || path[len - 1] != '/')
		return NULL;

	for (i = (ssize_t)len - 2; i >= 0; --i)
		if (path[i] == '/')
			break;

	return &path[i + 1];
}

int git_path_root(const char *path)
{
	int offset = 0, prefix_len;

	/* Does the root of the path look like a windows drive ? */
	if ((prefix_len = dos_drive_prefix_length(path)))
		offset += prefix_len;

#ifdef GIT_WIN32
	/* Are we dealing with a windows network path? */
	else if ((path[0] == '/' && path[1] == '/' && path[2] != '/') ||
		(path[0] == '\\' && path[1] == '\\' && path[2] != '\\'))
	{
		offset += 2;

		/* Skip the computer name segment */
		while (path[offset] && path[offset] != '/' && path[offset] != '\\')
			offset++;
	}

	if (path[offset] == '\\')
		return offset;
#endif

	if (path[offset] == '/')
		return offset;

	return -1;	/* Not a real error - signals that path is not rooted */
}

void git_path_trim_slashes(git_buf *path)
{
	int ceiling = git_path_root(path->ptr) + 1;
	assert(ceiling >= 0);

	while (path->size > (size_t)ceiling) {
		if (path->ptr[path->size-1] != '/')
			break;

		path->ptr[path->size-1] = '\0';
		path->size--;
	}
}

int git_path_join_unrooted(
	git_buf *path_out, const char *path, const char *base, ssize_t *root_at)
{
	ssize_t root;

	assert(path && path_out);

	root = (ssize_t)git_path_root(path);

	if (base != NULL && root < 0) {
		if (git_buf_joinpath(path_out, base, path) < 0)
			return -1;

		root = (ssize_t)strlen(base);
	} else {
		if (git_buf_sets(path_out, path) < 0)
			return -1;

		if (root < 0)
			root = 0;
		else if (base)
			git_path_equal_or_prefixed(base, path, &root);
	}

	if (root_at)
		*root_at = root;

	return 0;
}

void git_path_squash_slashes(git_buf *path)
{
	char *p, *q;

	if (path->size == 0)
		return;

	for (p = path->ptr, q = path->ptr; *q; p++, q++) {
		*p = *q;

		while (*q == '/' && *(q+1) == '/') {
			path->size--;
			q++;
		}
	}

	*p = '\0';
}

int git_path_prettify(git_buf *path_out, const char *path, const char *base)
{
	char buf[GIT_PATH_MAX];

	assert(path && path_out);

	/* construct path if needed */
	if (base != NULL && git_path_root(path) < 0) {
		if (git_buf_joinpath(path_out, base, path) < 0)
			return -1;
		path = path_out->ptr;
	}

	if (p_realpath(path, buf) == NULL) {
		/* git_error_set resets the errno when dealing with a GIT_ERROR_OS kind of error */
		int error = (errno == ENOENT || errno == ENOTDIR) ? GIT_ENOTFOUND : -1;
		git_error_set(GIT_ERROR_OS, "failed to resolve path '%s'", path);

		git_buf_clear(path_out);

		return error;
	}

	return git_buf_sets(path_out, buf);
}

int git_path_prettify_dir(git_buf *path_out, const char *path, const char *base)
{
	int error = git_path_prettify(path_out, path, base);
	return (error < 0) ? error : git_path_to_dir(path_out);
}

int git_path_to_dir(git_buf *path)
{
	if (path->asize > 0 &&
		git_buf_len(path) > 0 &&
		path->ptr[git_buf_len(path) - 1] != '/')
		git_buf_putc(path, '/');

	return git_buf_oom(path) ? -1 : 0;
}

void git_path_string_to_dir(char* path, size_t size)
{
	size_t end = strlen(path);

	if (end && path[end - 1] != '/' && end < size) {
		path[end] = '/';
		path[end + 1] = '\0';
	}
}

int git__percent_decode(git_buf *decoded_out, const char *input)
{
	int len, hi, lo, i;
	assert(decoded_out && input);

	len = (int)strlen(input);
	git_buf_clear(decoded_out);

	for(i = 0; i < len; i++)
	{
		char c = input[i];

		if (c != '%')
			goto append;

		if (i >= len - 2)
			goto append;

		hi = git__fromhex(input[i + 1]);
		lo = git__fromhex(input[i + 2]);

		if (hi < 0 || lo < 0)
			goto append;

		c = (char)(hi << 4 | lo);
		i += 2;

append:
		if (git_buf_putc(decoded_out, c) < 0)
			return -1;
	}

	return 0;
}

static int error_invalid_local_file_uri(const char *uri)
{
	git_error_set(GIT_ERROR_CONFIG, "'%s' is not a valid local file URI", uri);
	return -1;
}

static int local_file_url_prefixlen(const char *file_url)
{
	int len = -1;

	if (git__prefixcmp(file_url, "file://") == 0) {
		if (file_url[7] == '/')
			len = 8;
		else if (git__prefixcmp(file_url + 7, "localhost/") == 0)
			len = 17;
	}

	return len;
}

bool git_path_is_local_file_url(const char *file_url)
{
	return (local_file_url_prefixlen(file_url) > 0);
}

int git_path_fromurl(git_buf *local_path_out, const char *file_url)
{
	int offset;

	assert(local_path_out && file_url);

	if ((offset = local_file_url_prefixlen(file_url)) < 0 ||
		file_url[offset] == '\0' || file_url[offset] == '/')
		return error_invalid_local_file_uri(file_url);

#ifndef GIT_WIN32
	offset--;	/* A *nix absolute path starts with a forward slash */
#endif

	git_buf_clear(local_path_out);
	return git__percent_decode(local_path_out, file_url + offset);
}

int git_path_walk_up(
	git_buf *path,
	const char *ceiling,
	int (*cb)(void *data, const char *),
	void *data)
{
	int error = 0;
	git_buf iter;
	ssize_t stop = 0, scan;
	char oldc = '\0';

	assert(path && cb);

	if (ceiling != NULL) {
		if (git__prefixcmp(path->ptr, ceiling) == 0)
			stop = (ssize_t)strlen(ceiling);
		else
			stop = git_buf_len(path);
	}
	scan = git_buf_len(path);

	/* empty path: yield only once */
	if (!scan) {
		error = cb(data, "");
		if (error)
			git_error_set_after_callback(error);
		return error;
	}

	iter.ptr = path->ptr;
	iter.size = git_buf_len(path);
	iter.asize = path->asize;

	while (scan >= stop) {
		error = cb(data, iter.ptr);
		iter.ptr[scan] = oldc;

		if (error) {
			git_error_set_after_callback(error);
			break;
		}

		scan = git_buf_rfind_next(&iter, '/');
		if (scan >= 0) {
			scan++;
			oldc = iter.ptr[scan];
			iter.size = scan;
			iter.ptr[scan] = '\0';
		}
	}

	if (scan >= 0)
		iter.ptr[scan] = oldc;

	/* relative path: yield for the last component */
	if (!error && stop == 0 && iter.ptr[0] != '/') {
		error = cb(data, "");
		if (error)
			git_error_set_after_callback(error);
	}

	return error;
}

bool git_path_exists(const char *path)
{
	assert(path);
	return p_access(path, F_OK) == 0;
}

bool git_path_isdir(const char *path)
{
	struct stat st;
	if (p_stat(path, &st) < 0)
		return false;

	return S_ISDIR(st.st_mode) != 0;
}

bool git_path_isfile(const char *path)
{
	struct stat st;

	assert(path);
	if (p_stat(path, &st) < 0)
		return false;

	return S_ISREG(st.st_mode) != 0;
}

bool git_path_islink(const char *path)
{
	struct stat st;

	assert(path);
	if (p_lstat(path, &st) < 0)
		return false;

	return S_ISLNK(st.st_mode) != 0;
}

#ifdef GIT_WIN32

bool git_path_is_empty_dir(const char *path)
{
	git_win32_path filter_w;
	bool empty = false;

	if (git_win32__findfirstfile_filter(filter_w, path)) {
		WIN32_FIND_DATAW findData;
		HANDLE hFind = FindFirstFileW(filter_w, &findData);

		/* FindFirstFile will fail if there are no children to the given
		 * path, which can happen if the given path is a file (and obviously
		 * has no children) or if the given path is an empty mount point.
		 * (Most directories have at least directory entries '.' and '..',
		 * but ridiculously another volume mounted in another drive letter's
		 * path space do not, and thus have nothing to enumerate.)  If
		 * FindFirstFile fails, check if this is a directory-like thing
		 * (a mount point).
		 */
		if (hFind == INVALID_HANDLE_VALUE)
			return git_path_isdir(path);

		/* If the find handle was created successfully, then it's a directory */
		empty = true;

		do {
			/* Allow the enumeration to return . and .. and still be considered
			 * empty. In the special case of drive roots (i.e. C:\) where . and
			 * .. do not occur, we can still consider the path to be an empty
			 * directory if there's nothing there. */
			if (!git_path_is_dot_or_dotdotW(findData.cFileName)) {
				empty = false;
				break;
			}
		} while (FindNextFileW(hFind, &findData));

		FindClose(hFind);
	}

	return empty;
}

#else

static int path_found_entry(void *payload, git_buf *path)
{
	GIT_UNUSED(payload);
	return !git_path_is_dot_or_dotdot(path->ptr);
}

bool git_path_is_empty_dir(const char *path)
{
	int error;
	git_buf dir = GIT_BUF_INIT;

	if (!git_path_isdir(path))
		return false;

	if ((error = git_buf_sets(&dir, path)) != 0)
		git_error_clear();
	else
		error = git_path_direach(&dir, 0, path_found_entry, NULL);

	git_buf_dispose(&dir);

	return !error;
}

#endif

int git_path_set_error(int errno_value, const char *path, const char *action)
{
	switch (errno_value) {
	case ENOENT:
	case ENOTDIR:
		git_error_set(GIT_ERROR_OS, "could not find '%s' to %s", path, action);
		return GIT_ENOTFOUND;

	case EINVAL:
	case ENAMETOOLONG:
		git_error_set(GIT_ERROR_OS, "invalid path for filesystem '%s'", path);
		return GIT_EINVALIDSPEC;

	case EEXIST:
		git_error_set(GIT_ERROR_OS, "failed %s - '%s' already exists", action, path);
		return GIT_EEXISTS;

	case EACCES:
		git_error_set(GIT_ERROR_OS, "failed %s - '%s' is locked", action, path);
		return GIT_ELOCKED;

	default:
		git_error_set(GIT_ERROR_OS, "could not %s '%s'", action, path);
		return -1;
	}
}

int git_path_lstat(const char *path, struct stat *st)
{
	if (p_lstat(path, st) == 0)
		return 0;

	return git_path_set_error(errno, path, "stat");
}

static bool _check_dir_contents(
	git_buf *dir,
	const char *sub,
	bool (*predicate)(const char *))
{
	bool result;
	size_t dir_size = git_buf_len(dir);
	size_t sub_size = strlen(sub);
	size_t alloc_size;

	/* leave base valid even if we could not make space for subdir */
	if (GIT_ADD_SIZET_OVERFLOW(&alloc_size, dir_size, sub_size) ||
		GIT_ADD_SIZET_OVERFLOW(&alloc_size, alloc_size, 2) ||
		git_buf_try_grow(dir, alloc_size, false) < 0)
		return false;

	/* save excursion */
	if (git_buf_joinpath(dir, dir->ptr, sub) < 0)
		return false;

	result = predicate(dir->ptr);

	/* restore path */
	git_buf_truncate(dir, dir_size);
	return result;
}

bool git_path_contains(git_buf *dir, const char *item)
{
	return _check_dir_contents(dir, item, &git_path_exists);
}

bool git_path_contains_dir(git_buf *base, const char *subdir)
{
	return _check_dir_contents(base, subdir, &git_path_isdir);
}

bool git_path_contains_file(git_buf *base, const char *file)
{
	return _check_dir_contents(base, file, &git_path_isfile);
}

int git_path_find_dir(git_buf *dir, const char *path, const char *base)
{
	int error = git_path_join_unrooted(dir, path, base, NULL);

	if (!error) {
		char buf[GIT_PATH_MAX];
		if (p_realpath(dir->ptr, buf) != NULL)
			error = git_buf_sets(dir, buf);
	}

	/* call dirname if this is not a directory */
	if (!error) /* && git_path_isdir(dir->ptr) == false) */
		error = (git_path_dirname_r(dir, dir->ptr) < 0) ? -1 : 0;

	if (!error)
		error = git_path_to_dir(dir);

	return error;
}

int git_path_resolve_relative(git_buf *path, size_t ceiling)
{
	char *base, *to, *from, *next;
	size_t len;

	GIT_ERROR_CHECK_ALLOC_BUF(path);

	if (ceiling > path->size)
		ceiling = path->size;

	/* recognize drive prefixes, etc. that should not be backed over */
	if (ceiling == 0)
		ceiling = git_path_root(path->ptr) + 1;

	/* recognize URL prefixes that should not be backed over */
	if (ceiling == 0) {
		for (next = path->ptr; *next && git__isalpha(*next); ++next);
		if (next[0] == ':' && next[1] == '/' && next[2] == '/')
			ceiling = (next + 3) - path->ptr;
	}

	base = to = from = path->ptr + ceiling;

	while (*from) {
		for (next = from; *next && *next != '/'; ++next);

		len = next - from;

		if (len == 1 && from[0] == '.')
			/* do nothing with singleton dot */;

		else if (len == 2 && from[0] == '.' && from[1] == '.') {
			/* error out if trying to up one from a hard base */
			if (to == base && ceiling != 0) {
				git_error_set(GIT_ERROR_INVALID,
					"cannot strip root component off url");
				return -1;
			}

			/* no more path segments to strip,
			 * use '../' as a new base path */
			if (to == base) {
				if (*next == '/')
					len++;

				if (to != from)
					memmove(to, from, len);

				to += len;
				/* this is now the base, can't back up from a
				 * relative prefix */
				base = to;
			} else {
				/* back up a path segment */
				while (to > base && to[-1] == '/') to--;
				while (to > base && to[-1] != '/') to--;
			}
		} else {
			if (*next == '/' && *from != '/')
				len++;

			if (to != from)
				memmove(to, from, len);

			to += len;
		}

		from += len;

		while (*from == '/') from++;
	}

	*to = '\0';

	path->size = to - path->ptr;

	return 0;
}

int git_path_apply_relative(git_buf *target, const char *relpath)
{
	return git_buf_joinpath(target, git_buf_cstr(target), relpath) ||
	    git_path_resolve_relative(target, 0);
}

int git_path_cmp(
	const char *name1, size_t len1, int isdir1,
	const char *name2, size_t len2, int isdir2,
	int (*compare)(const char *, const char *, size_t))
{
	unsigned char c1, c2;
	size_t len = len1 < len2 ? len1 : len2;
	int cmp;

	cmp = compare(name1, name2, len);
	if (cmp)
		return cmp;

	c1 = name1[len];
	c2 = name2[len];

	if (c1 == '\0' && isdir1)
		c1 = '/';

	if (c2 == '\0' && isdir2)
		c2 = '/';

	return (c1 < c2) ? -1 : (c1 > c2) ? 1 : 0;
}

size_t git_path_common_dirlen(const char *one, const char *two)
{
	const char *p, *q, *dirsep = NULL;

	for (p = one, q = two; *p && *q; p++, q++) {
		if (*p == '/' && *q == '/')
			dirsep = p;
		else if (*p != *q)
			break;
	}

	return dirsep ? (dirsep - one) + 1 : 0;
}

int git_path_make_relative(git_buf *path, const char *parent)
{
	const char *p, *q, *p_dirsep, *q_dirsep;
	size_t plen = path->size, newlen, alloclen, depth = 1, i, offset;

	for (p_dirsep = p = path->ptr, q_dirsep = q = parent; *p && *q; p++, q++) {
		if (*p == '/' && *q == '/') {
			p_dirsep = p;
			q_dirsep = q;
		}
		else if (*p != *q)
			break;
	}

	/* need at least 1 common path segment */
	if ((p_dirsep == path->ptr || q_dirsep == parent) &&
		(*p_dirsep != '/' || *q_dirsep != '/')) {
		git_error_set(GIT_ERROR_INVALID,
			"%s is not a parent of %s", parent, path->ptr);
		return GIT_ENOTFOUND;
	}

	if (*p == '/' && !*q)
		p++;
	else if (!*p && *q == '/')
		q++;
	else if (!*p && !*q)
		return git_buf_clear(path), 0;
	else {
		p = p_dirsep + 1;
		q = q_dirsep + 1;
	}

	plen -= (p - path->ptr);

	if (!*q)
		return git_buf_set(path, p, plen);

	for (; (q = strchr(q, '/')) && *(q + 1); q++)
		depth++;

	GIT_ERROR_CHECK_ALLOC_MULTIPLY(&newlen, depth, 3);
	GIT_ERROR_CHECK_ALLOC_ADD(&newlen, newlen, plen);

	GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, newlen, 1);

	/* save the offset as we might realllocate the pointer */
	offset = p - path->ptr;
	if (git_buf_try_grow(path, alloclen, 1) < 0)
		return -1;
	p = path->ptr + offset;

	memmove(path->ptr + (depth * 3), p, plen + 1);

	for (i = 0; i < depth; i++)
		memcpy(path->ptr + (i * 3), "../", 3);

	path->size = newlen;
	return 0;
}

bool git_path_has_non_ascii(const char *path, size_t pathlen)
{
	const uint8_t *scan = (const uint8_t *)path, *end;

	for (end = scan + pathlen; scan < end; ++scan)
		if (*scan & 0x80)
			return true;

	return false;
}

#ifdef GIT_USE_ICONV

int git_path_iconv_init_precompose(git_path_iconv_t *ic)
{
	git_buf_init(&ic->buf, 0);
	ic->map = iconv_open(GIT_PATH_REPO_ENCODING, GIT_PATH_NATIVE_ENCODING);
	return 0;
}

void git_path_iconv_clear(git_path_iconv_t *ic)
{
	if (ic) {
		if (ic->map != (iconv_t)-1)
			iconv_close(ic->map);
		git_buf_dispose(&ic->buf);
	}
}

int git_path_iconv(git_path_iconv_t *ic, const char **in, size_t *inlen)
{
	char *nfd = (char*)*in, *nfc;
	size_t nfdlen = *inlen, nfclen, wantlen = nfdlen, alloclen, rv;
	int retry = 1;

	if (!ic || ic->map == (iconv_t)-1 ||
		!git_path_has_non_ascii(*in, *inlen))
		return 0;

	git_buf_clear(&ic->buf);

	while (1) {
		GIT_ERROR_CHECK_ALLOC_ADD(&alloclen, wantlen, 1);
		if (git_buf_grow(&ic->buf, alloclen) < 0)
			return -1;

		nfc    = ic->buf.ptr   + ic->buf.size;
		nfclen = ic->buf.asize - ic->buf.size;

		rv = iconv(ic->map, &nfd, &nfdlen, &nfc, &nfclen);

		ic->buf.size = (nfc - ic->buf.ptr);

		if (rv != (size_t)-1)
			break;

		/* if we cannot convert the data (probably because iconv thinks
		 * it is not valid UTF-8 source data), then use original data
		 */
		if (errno != E2BIG)
			return 0;

		/* make space for 2x the remaining data to be converted
		 * (with per retry overhead to avoid infinite loops)
		 */
		wantlen = ic->buf.size + max(nfclen, nfdlen) * 2 + (size_t)(retry * 4);

		if (retry++ > 4)
			goto fail;
	}

	ic->buf.ptr[ic->buf.size] = '\0';

	*in    = ic->buf.ptr;
	*inlen = ic->buf.size;

	return 0;

fail:
	git_error_set(GIT_ERROR_OS, "unable to convert unicode path data");
	return -1;
}

static const char *nfc_file = "\xC3\x85\x73\x74\x72\xC3\xB6\x6D.XXXXXX";
static const char *nfd_file = "\x41\xCC\x8A\x73\x74\x72\x6F\xCC\x88\x6D.XXXXXX";

/* Check if the platform is decomposing unicode data for us.  We will
 * emulate core Git and prefer to use precomposed unicode data internally
 * on these platforms, composing the decomposed unicode on the fly.
 *
 * This mainly happens on the Mac where HDFS stores filenames as
 * decomposed unicode.  Even on VFAT and SAMBA file systems, the Mac will
 * return decomposed unicode from readdir() even when the actual
 * filesystem is storing precomposed unicode.
 */
bool git_path_does_fs_decompose_unicode(const char *root)
{
	git_buf path = GIT_BUF_INIT;
	int fd;
	bool found_decomposed = false;
	char tmp[6];

	/* Create a file using a precomposed path and then try to find it
	 * using the decomposed name.  If the lookup fails, then we will mark
	 * that we should precompose unicode for this repository.
	 */
	if (git_buf_joinpath(&path, root, nfc_file) < 0 ||
		(fd = p_mkstemp(path.ptr)) < 0)
		goto done;
	p_close(fd);

	/* record trailing digits generated by mkstemp */
	memcpy(tmp, path.ptr + path.size - sizeof(tmp), sizeof(tmp));

	/* try to look up as NFD path */
	if (git_buf_joinpath(&path, root, nfd_file) < 0)
		goto done;
	memcpy(path.ptr + path.size - sizeof(tmp), tmp, sizeof(tmp));

	found_decomposed = git_path_exists(path.ptr);

	/* remove temporary file (using original precomposed path) */
	if (git_buf_joinpath(&path, root, nfc_file) < 0)
		goto done;
	memcpy(path.ptr + path.size - sizeof(tmp), tmp, sizeof(tmp));

	(void)p_unlink(path.ptr);

done:
	git_buf_dispose(&path);
	return found_decomposed;
}

#else

bool git_path_does_fs_decompose_unicode(const char *root)
{
	GIT_UNUSED(root);
	return false;
}

#endif

#if defined(__sun) || defined(__GNU__)
typedef char path_dirent_data[sizeof(struct dirent) + FILENAME_MAX + 1];
#else
typedef struct dirent path_dirent_data;
#endif

int git_path_direach(
	git_buf *path,
	uint32_t flags,
	int (*fn)(void *, git_buf *),
	void *arg)
{
	int error = 0;
	ssize_t wd_len;
	DIR *dir;
	struct dirent *de;

#ifdef GIT_USE_ICONV
	git_path_iconv_t ic = GIT_PATH_ICONV_INIT;
#endif

	GIT_UNUSED(flags);

	if (git_path_to_dir(path) < 0)
		return -1;

	wd_len = git_buf_len(path);

	if ((dir = opendir(path->ptr)) == NULL) {
		git_error_set(GIT_ERROR_OS, "failed to open directory '%s'", path->ptr);
		if (errno == ENOENT)
			return GIT_ENOTFOUND;

		return -1;
	}

#ifdef GIT_USE_ICONV
	if ((flags & GIT_PATH_DIR_PRECOMPOSE_UNICODE) != 0)
		(void)git_path_iconv_init_precompose(&ic);
#endif

	while ((de = readdir(dir)) != NULL) {
		const char *de_path = de->d_name;
		size_t de_len = strlen(de_path);

		if (git_path_is_dot_or_dotdot(de_path))
			continue;

#ifdef GIT_USE_ICONV
		if ((error = git_path_iconv(&ic, &de_path, &de_len)) < 0)
			break;
#endif

		if ((error = git_buf_put(path, de_path, de_len)) < 0)
			break;

		git_error_clear();
		error = fn(arg, path);

		git_buf_truncate(path, wd_len); /* restore path */

		/* Only set our own error if the callback did not set one already */
		if (error != 0) {
			if (!git_error_last())
				git_error_set_after_callback(error);

			break;
		}
	}

	closedir(dir);

#ifdef GIT_USE_ICONV
	git_path_iconv_clear(&ic);
#endif

	return error;
}

#if defined(GIT_WIN32) && !defined(__MINGW32__)

/* Using _FIND_FIRST_EX_LARGE_FETCH may increase performance in Windows 7
 * and better.
 */
#ifndef FIND_FIRST_EX_LARGE_FETCH
# define FIND_FIRST_EX_LARGE_FETCH 2
#endif

int git_path_diriter_init(
	git_path_diriter *diriter,
	const char *path,
	unsigned int flags)
{
	git_win32_path path_filter;

	static int is_win7_or_later = -1;
	if (is_win7_or_later < 0)
		is_win7_or_later = git_has_win32_version(6, 1, 0);

	assert(diriter && path);

	memset(diriter, 0, sizeof(git_path_diriter));
	diriter->handle = INVALID_HANDLE_VALUE;

	if (git_buf_puts(&diriter->path_utf8, path) < 0)
		return -1;

	git_path_trim_slashes(&diriter->path_utf8);

	if (diriter->path_utf8.size == 0) {
		git_error_set(GIT_ERROR_FILESYSTEM, "could not open directory '%s'", path);
		return -1;
	}

	if ((diriter->parent_len = git_win32_path_from_utf8(diriter->path, diriter->path_utf8.ptr)) < 0 ||
			!git_win32__findfirstfile_filter(path_filter, diriter->path_utf8.ptr)) {
		git_error_set(GIT_ERROR_OS, "could not parse the directory path '%s'", path);
		return -1;
	}

	diriter->handle = FindFirstFileExW(
		path_filter,
		is_win7_or_later ? FindExInfoBasic : FindExInfoStandard,
		&diriter->current,
		FindExSearchNameMatch,
		NULL,
		is_win7_or_later ? FIND_FIRST_EX_LARGE_FETCH : 0);

	if (diriter->handle == INVALID_HANDLE_VALUE) {
		git_error_set(GIT_ERROR_OS, "could not open directory '%s'", path);
		return -1;
	}

	diriter->parent_utf8_len = diriter->path_utf8.size;
	diriter->flags = flags;
	return 0;
}

static int diriter_update_paths(git_path_diriter *diriter)
{
	size_t filename_len, path_len;

	filename_len = wcslen(diriter->current.cFileName);

	if (GIT_ADD_SIZET_OVERFLOW(&path_len, diriter->parent_len, filename_len) ||
		GIT_ADD_SIZET_OVERFLOW(&path_len, path_len, 2))
		return -1;

	if (path_len > GIT_WIN_PATH_UTF16) {
		git_error_set(GIT_ERROR_FILESYSTEM,
			"invalid path '%.*ls\\%ls' (path too long)",
			diriter->parent_len, diriter->path, diriter->current.cFileName);
		return -1;
	}

	diriter->path[diriter->parent_len] = L'\\';
	memcpy(&diriter->path[diriter->parent_len+1],
		diriter->current.cFileName, filename_len * sizeof(wchar_t));
	diriter->path[path_len-1] = L'\0';

	git_buf_truncate(&diriter->path_utf8, diriter->parent_utf8_len);

	if (diriter->parent_utf8_len > 0 &&
		diriter->path_utf8.ptr[diriter->parent_utf8_len-1] != '/')
		git_buf_putc(&diriter->path_utf8, '/');

	git_buf_put_w(&diriter->path_utf8, diriter->current.cFileName, filename_len);

	if (git_buf_oom(&diriter->path_utf8))
		return -1;

	return 0;
}

int git_path_diriter_next(git_path_diriter *diriter)
{
	bool skip_dot = !(diriter->flags & GIT_PATH_DIR_INCLUDE_DOT_AND_DOTDOT);

	do {
		/* Our first time through, we already have the data from
		 * FindFirstFileW.  Use it, otherwise get the next file.
		 */
		if (!diriter->needs_next)
			diriter->needs_next = 1;
		else if (!FindNextFileW(diriter->handle, &diriter->current))
			return GIT_ITEROVER;
	} while (skip_dot && git_path_is_dot_or_dotdotW(diriter->current.cFileName));

	if (diriter_update_paths(diriter) < 0)
		return -1;

	return 0;
}

int git_path_diriter_filename(
	const char **out,
	size_t *out_len,
	git_path_diriter *diriter)
{
	assert(out && out_len && diriter);

	assert(diriter->path_utf8.size > diriter->parent_utf8_len);

	*out = &diriter->path_utf8.ptr[diriter->parent_utf8_len+1];
	*out_len = diriter->path_utf8.size - diriter->parent_utf8_len - 1;
	return 0;
}

int git_path_diriter_fullpath(
	const char **out,
	size_t *out_len,
	git_path_diriter *diriter)
{
	assert(out && out_len && diriter);

	*out = diriter->path_utf8.ptr;
	*out_len = diriter->path_utf8.size;
	return 0;
}

int git_path_diriter_stat(struct stat *out, git_path_diriter *diriter)
{
	assert(out && diriter);

	return git_win32__file_attribute_to_stat(out,
		(WIN32_FILE_ATTRIBUTE_DATA *)&diriter->current,
		diriter->path);
}

void git_path_diriter_free(git_path_diriter *diriter)
{
	if (diriter == NULL)
		return;

	git_buf_dispose(&diriter->path_utf8);

	if (diriter->handle != INVALID_HANDLE_VALUE) {
		FindClose(diriter->handle);
		diriter->handle = INVALID_HANDLE_VALUE;
	}
}

#else

int git_path_diriter_init(
	git_path_diriter *diriter,
	const char *path,
	unsigned int flags)
{
	assert(diriter && path);

	memset(diriter, 0, sizeof(git_path_diriter));

	if (git_buf_puts(&diriter->path, path) < 0)
		return -1;

	git_path_trim_slashes(&diriter->path);

	if (diriter->path.size == 0) {
		git_error_set(GIT_ERROR_FILESYSTEM, "could not open directory '%s'", path);
		return -1;
	}

	if ((diriter->dir = opendir(diriter->path.ptr)) == NULL) {
		git_buf_dispose(&diriter->path);

		git_error_set(GIT_ERROR_OS, "failed to open directory '%s'", path);
		return -1;
	}

#ifdef GIT_USE_ICONV
	if ((flags & GIT_PATH_DIR_PRECOMPOSE_UNICODE) != 0)
		(void)git_path_iconv_init_precompose(&diriter->ic);
#endif

	diriter->parent_len = diriter->path.size;
	diriter->flags = flags;

	return 0;
}

int git_path_diriter_next(git_path_diriter *diriter)
{
	struct dirent *de;
	const char *filename;
	size_t filename_len;
	bool skip_dot = !(diriter->flags & GIT_PATH_DIR_INCLUDE_DOT_AND_DOTDOT);
	int error = 0;

	assert(diriter);

	errno = 0;

	do {
		if ((de = readdir(diriter->dir)) == NULL) {
			if (!errno)
				return GIT_ITEROVER;

			git_error_set(GIT_ERROR_OS,
				"could not read directory '%s'", diriter->path.ptr);
			return -1;
		}
	} while (skip_dot && git_path_is_dot_or_dotdot(de->d_name));

	filename = de->d_name;
	filename_len = strlen(filename);

#ifdef GIT_USE_ICONV
	if ((diriter->flags & GIT_PATH_DIR_PRECOMPOSE_UNICODE) != 0 &&
		(error = git_path_iconv(&diriter->ic, &filename, &filename_len)) < 0)
		return error;
#endif

	git_buf_truncate(&diriter->path, diriter->parent_len);

	if (diriter->parent_len > 0 &&
		diriter->path.ptr[diriter->parent_len-1] != '/')
		git_buf_putc(&diriter->path, '/');

	git_buf_put(&diriter->path, filename, filename_len);

	if (git_buf_oom(&diriter->path))
		return -1;

	return error;
}

int git_path_diriter_filename(
	const char **out,
	size_t *out_len,
	git_path_diriter *diriter)
{
	assert(out && out_len && diriter);

	assert(diriter->path.size > diriter->parent_len);

	*out = &diriter->path.ptr[diriter->parent_len+1];
	*out_len = diriter->path.size - diriter->parent_len - 1;
	return 0;
}

int git_path_diriter_fullpath(
	const char **out,
	size_t *out_len,
	git_path_diriter *diriter)
{
	assert(out && out_len && diriter);

	*out = diriter->path.ptr;
	*out_len = diriter->path.size;
	return 0;
}

int git_path_diriter_stat(struct stat *out, git_path_diriter *diriter)
{
	assert(out && diriter);

	return git_path_lstat(diriter->path.ptr, out);
}

void git_path_diriter_free(git_path_diriter *diriter)
{
	if (diriter == NULL)
		return;

	if (diriter->dir) {
		closedir(diriter->dir);
		diriter->dir = NULL;
	}

#ifdef GIT_USE_ICONV
	git_path_iconv_clear(&diriter->ic);
#endif

	git_buf_dispose(&diriter->path);
}

#endif

int git_path_dirload(
	git_vector *contents,
	const char *path,
	size_t prefix_len,
	uint32_t flags)
{
	git_path_diriter iter = GIT_PATH_DIRITER_INIT;
	const char *name;
	size_t name_len;
	char *dup;
	int error;

	assert(contents && path);

	if ((error = git_path_diriter_init(&iter, path, flags)) < 0)
		return error;

	while ((error = git_path_diriter_next(&iter)) == 0) {
		if ((error = git_path_diriter_fullpath(&name, &name_len, &iter)) < 0)
			break;

		assert(name_len > prefix_len);

		dup = git__strndup(name + prefix_len, name_len - prefix_len);
		GIT_ERROR_CHECK_ALLOC(dup);

		if ((error = git_vector_insert(contents, dup)) < 0)
			break;
	}

	if (error == GIT_ITEROVER)
		error = 0;

	git_path_diriter_free(&iter);
	return error;
}

int git_path_from_url_or_path(git_buf *local_path_out, const char *url_or_path)
{
	if (git_path_is_local_file_url(url_or_path))
		return git_path_fromurl(local_path_out, url_or_path);
	else
		return git_buf_sets(local_path_out, url_or_path);
}

/* Reject paths like AUX or COM1, or those versions that end in a dot or
 * colon.  ("AUX." or "AUX:")
 */
GIT_INLINE(bool) verify_dospath(
	const char *component,
	size_t len,
	const char dospath[3],
	bool trailing_num)
{
	size_t last = trailing_num ? 4 : 3;

	if (len < last || git__strncasecmp(component, dospath, 3) != 0)
		return true;

	if (trailing_num && (component[3] < '1' || component[3] > '9'))
		return true;

	return (len > last &&
		component[last] != '.' &&
		component[last] != ':');
}

static int32_t next_hfs_char(const char **in, size_t *len)
{
	while (*len) {
		int32_t codepoint;
		int cp_len = git__utf8_iterate((const uint8_t *)(*in), (int)(*len), &codepoint);
		if (cp_len < 0)
			return -1;

		(*in) += cp_len;
		(*len) -= cp_len;

		/* these code points are ignored completely */
		switch (codepoint) {
		case 0x200c: /* ZERO WIDTH NON-JOINER */
		case 0x200d: /* ZERO WIDTH JOINER */
		case 0x200e: /* LEFT-TO-RIGHT MARK */
		case 0x200f: /* RIGHT-TO-LEFT MARK */
		case 0x202a: /* LEFT-TO-RIGHT EMBEDDING */
		case 0x202b: /* RIGHT-TO-LEFT EMBEDDING */
		case 0x202c: /* POP DIRECTIONAL FORMATTING */
		case 0x202d: /* LEFT-TO-RIGHT OVERRIDE */
		case 0x202e: /* RIGHT-TO-LEFT OVERRIDE */
		case 0x206a: /* INHIBIT SYMMETRIC SWAPPING */
		case 0x206b: /* ACTIVATE SYMMETRIC SWAPPING */
		case 0x206c: /* INHIBIT ARABIC FORM SHAPING */
		case 0x206d: /* ACTIVATE ARABIC FORM SHAPING */
		case 0x206e: /* NATIONAL DIGIT SHAPES */
		case 0x206f: /* NOMINAL DIGIT SHAPES */
		case 0xfeff: /* ZERO WIDTH NO-BREAK SPACE */
			continue;
		}

		/* fold into lowercase -- this will only fold characters in
		 * the ASCII range, which is perfectly fine, because the
		 * git folder name can only be composed of ascii characters
		 */
		return git__tolower(codepoint);
	}
	return 0; /* NULL byte -- end of string */
}

static bool verify_dotgit_hfs_generic(const char *path, size_t len, const char *needle, size_t needle_len)
{
	size_t i;
	char c;

	if (next_hfs_char(&path, &len) != '.')
		return true;

	for (i = 0; i < needle_len; i++) {
		c = next_hfs_char(&path, &len);
		if (c != needle[i])
			return true;
	}

	if (next_hfs_char(&path, &len) != '\0')
		return true;

	return false;
}

static bool verify_dotgit_hfs(const char *path, size_t len)
{
	return verify_dotgit_hfs_generic(path, len, "git", CONST_STRLEN("git"));
}

GIT_INLINE(bool) verify_dotgit_ntfs(git_repository *repo, const char *path, size_t len)
{
	git_buf *reserved = git_repository__reserved_names_win32;
	size_t reserved_len = git_repository__reserved_names_win32_len;
	size_t start = 0, i;

	if (repo)
		git_repository__reserved_names(&reserved, &reserved_len, repo, true);

	for (i = 0; i < reserved_len; i++) {
		git_buf *r = &reserved[i];

		if (len >= r->size &&
			strncasecmp(path, r->ptr, r->size) == 0) {
			start = r->size;
			break;
		}
	}

	if (!start)
		return true;

	/*
	 * Reject paths that start with Windows-style directory separators
	 * (".git\") or NTFS alternate streams (".git:") and could be used
	 * to write to the ".git" directory on Windows platforms.
	 */
	if (path[start] == '\\' || path[start] == ':')
		return false;

	/* Reject paths like '.git ' or '.git.' */
	for (i = start; i < len; i++) {
		if (path[i] != ' ' && path[i] != '.')
			return true;
	}

	return false;
}

/*
 * Windows paths that end with spaces and/or dots are elided to the
 * path without them for backward compatibility.  That is to say
 * that opening file "foo ", "foo." or even "foo . . ." will all
 * map to a filename of "foo".  This function identifies spaces and
 * dots at the end of a filename, whether the proper end of the
 * filename (end of string) or a colon (which would indicate a
 * Windows alternate data stream.)
 */
GIT_INLINE(bool) ntfs_end_of_filename(const char *path)
{
	const char *c = path;

	for (;; c++) {
		if (*c == '\0' || *c == ':')
			return true;
		if (*c != ' ' && *c != '.')
			return false;
	}

	return true;
}

GIT_INLINE(bool) verify_dotgit_ntfs_generic(const char *name, size_t len, const char *dotgit_name, size_t dotgit_len, const char *shortname_pfix)
{
	int i, saw_tilde;

	if (name[0] == '.' && len >= dotgit_len &&
	    !strncasecmp(name + 1, dotgit_name, dotgit_len)) {
		return !ntfs_end_of_filename(name + dotgit_len + 1);
	}

	/* Detect the basic NTFS shortname with the first six chars */
	if (!strncasecmp(name, dotgit_name, 6) && name[6] == '~' &&
	    name[7] >= '1' && name[7] <= '4')
		return !ntfs_end_of_filename(name + 8);

	/* Catch fallback names */
	for (i = 0, saw_tilde = 0; i < 8; i++) {
		if (name[i] == '\0') {
			return true;
		} else if (saw_tilde) {
			if (name[i] < '0' || name[i] > '9')
				return true;
		} else if (name[i] == '~') {
			if (name[i+1] < '1' || name[i+1]  > '9')
				return true;
			saw_tilde = 1;
		} else if (i >= 6) {
			return true;
		} else if ((unsigned char)name[i] > 127) {
			return true;
		} else if (git__tolower(name[i]) != shortname_pfix[i]) {
			return true;
		}
	}

	return !ntfs_end_of_filename(name + i);
}

GIT_INLINE(bool) verify_char(unsigned char c, unsigned int flags)
{
	if ((flags & GIT_PATH_REJECT_BACKSLASH) && c == '\\')
		return false;

	if ((flags & GIT_PATH_REJECT_SLASH) && c == '/')
		return false;

	if (flags & GIT_PATH_REJECT_NT_CHARS) {
		if (c < 32)
			return false;

		switch (c) {
		case '<':
		case '>':
		case ':':
		case '"':
		case '|':
		case '?':
		case '*':
			return false;
		}
	}

	return true;
}

/*
 * Return the length of the common prefix between str and prefix, comparing them
 * case-insensitively (must be ASCII to match).
 */
GIT_INLINE(size_t) common_prefix_icase(const char *str, size_t len, const char *prefix)
{
	size_t count = 0;

	while (len >0 && tolower(*str) == tolower(*prefix)) {
		count++;
		str++;
		prefix++;
		len--;
	}

	return count;
}

/*
 * We fundamentally don't like some paths when dealing with user-inputted
 * strings (in checkout or ref names): we don't want dot or dot-dot
 * anywhere, we want to avoid writing weird paths on Windows that can't
 * be handled by tools that use the non-\\?\ APIs, we don't want slashes
 * or double slashes at the end of paths that can make them ambiguous.
 *
 * For checkout, we don't want to recurse into ".git" either.
 */
static bool verify_component(
	git_repository *repo,
	const char *component,
	size_t len,
	uint16_t mode,
	unsigned int flags)
{
	if (len == 0)
		return false;

	if ((flags & GIT_PATH_REJECT_TRAVERSAL) &&
		len == 1 && component[0] == '.')
		return false;

	if ((flags & GIT_PATH_REJECT_TRAVERSAL) &&
		len == 2 && component[0] == '.' && component[1] == '.')
		return false;

	if ((flags & GIT_PATH_REJECT_TRAILING_DOT) && component[len-1] == '.')
		return false;

	if ((flags & GIT_PATH_REJECT_TRAILING_SPACE) && component[len-1] == ' ')
		return false;

	if ((flags & GIT_PATH_REJECT_TRAILING_COLON) && component[len-1] == ':')
		return false;

	if (flags & GIT_PATH_REJECT_DOS_PATHS) {
		if (!verify_dospath(component, len, "CON", false) ||
			!verify_dospath(component, len, "PRN", false) ||
			!verify_dospath(component, len, "AUX", false) ||
			!verify_dospath(component, len, "NUL", false) ||
			!verify_dospath(component, len, "COM", true)  ||
			!verify_dospath(component, len, "LPT", true))
			return false;
	}

	if (flags & GIT_PATH_REJECT_DOT_GIT_HFS) {
		if (!verify_dotgit_hfs(component, len))
			return false;
		if (S_ISLNK(mode) && git_path_is_gitfile(component, len, GIT_PATH_GITFILE_GITMODULES, GIT_PATH_FS_HFS))
			return false;
	}

	if (flags & GIT_PATH_REJECT_DOT_GIT_NTFS) {
		if (!verify_dotgit_ntfs(repo, component, len))
			return false;
		if (S_ISLNK(mode) && git_path_is_gitfile(component, len, GIT_PATH_GITFILE_GITMODULES, GIT_PATH_FS_NTFS))
			return false;
	}

	/* don't bother rerunning the `.git` test if we ran the HFS or NTFS
	 * specific tests, they would have already rejected `.git`.
	 */
	if ((flags & GIT_PATH_REJECT_DOT_GIT_HFS) == 0 &&
	    (flags & GIT_PATH_REJECT_DOT_GIT_NTFS) == 0 &&
	    (flags & GIT_PATH_REJECT_DOT_GIT_LITERAL)) {
		if (len >= 4 &&
		    component[0] == '.' &&
		    (component[1] == 'g' || component[1] == 'G') &&
		    (component[2] == 'i' || component[2] == 'I') &&
		    (component[3] == 't' || component[3] == 'T')) {
			if (len == 4)
				return false;

			if (S_ISLNK(mode) && common_prefix_icase(component, len, ".gitmodules") == len)
				return false;
		}
	    }

	return true;
}

GIT_INLINE(unsigned int) dotgit_flags(
	git_repository *repo,
	unsigned int flags)
{
	int protectHFS = 0, protectNTFS = 1;
	int error = 0;

	flags |= GIT_PATH_REJECT_DOT_GIT_LITERAL;

#ifdef __APPLE__
	protectHFS = 1;
#endif

	if (repo && !protectHFS)
		error = git_repository__configmap_lookup(&protectHFS, repo, GIT_CONFIGMAP_PROTECTHFS);
	if (!error && protectHFS)
		flags |= GIT_PATH_REJECT_DOT_GIT_HFS;

	if (repo)
		error = git_repository__configmap_lookup(&protectNTFS, repo, GIT_CONFIGMAP_PROTECTNTFS);
	if (!error && protectNTFS)
		flags |= GIT_PATH_REJECT_DOT_GIT_NTFS;

	return flags;
}

bool git_path_isvalid(
	git_repository *repo,
	const char *path,
	uint16_t mode,
	unsigned int flags)
{
	const char *start, *c;

	/* Upgrade the ".git" checks based on platform */
	if ((flags & GIT_PATH_REJECT_DOT_GIT))
		flags = dotgit_flags(repo, flags);

	for (start = c = path; *c; c++) {
		if (!verify_char(*c, flags))
			return false;

		if (*c == '/') {
			if (!verify_component(repo, start, (c - start), mode, flags))
				return false;

			start = c+1;
		}
	}

	return verify_component(repo, start, (c - start), mode, flags);
}

int git_path_normalize_slashes(git_buf *out, const char *path)
{
	int error;
	char *p;

	if ((error = git_buf_puts(out, path)) < 0)
		return error;

	for (p = out->ptr; *p; p++) {
		if (*p == '\\')
			*p = '/';
	}

	return 0;
}

static const struct {
	const char *file;
	const char *hash;
	size_t filelen;
} gitfiles[] = {
	{ "gitignore", "gi250a", CONST_STRLEN("gitignore") },
	{ "gitmodules", "gi7eba", CONST_STRLEN("gitmodules") },
	{ "gitattributes", "gi7d29", CONST_STRLEN("gitattributes") }
};

extern int git_path_is_gitfile(const char *path, size_t pathlen, git_path_gitfile gitfile, git_path_fs fs)
{
	const char *file, *hash;
	size_t filelen;

	if (!(gitfile >= GIT_PATH_GITFILE_GITIGNORE && gitfile < ARRAY_SIZE(gitfiles))) {
		git_error_set(GIT_ERROR_OS, "invalid gitfile for path validation");
		return -1;
	}

	file = gitfiles[gitfile].file;
	filelen = gitfiles[gitfile].filelen;
	hash = gitfiles[gitfile].hash;

	switch (fs) {
	case GIT_PATH_FS_GENERIC:
		return !verify_dotgit_ntfs_generic(path, pathlen, file, filelen, hash) ||
		       !verify_dotgit_hfs_generic(path, pathlen, file, filelen);
	case GIT_PATH_FS_NTFS:
		return !verify_dotgit_ntfs_generic(path, pathlen, file, filelen, hash);
	case GIT_PATH_FS_HFS:
		return !verify_dotgit_hfs_generic(path, pathlen, file, filelen);
	default:
		git_error_set(GIT_ERROR_OS, "invalid filesystem for path validation");
		return -1;
	}
}

bool git_path_supports_symlinks(const char *dir)
{
	git_buf path = GIT_BUF_INIT;
	bool supported = false;
	struct stat st;
	int fd;

	if ((fd = git_futils_mktmp(&path, dir, 0666)) < 0 ||
	    p_close(fd) < 0 ||
	    p_unlink(path.ptr) < 0 ||
	    p_symlink("testing", path.ptr) < 0 ||
	    p_lstat(path.ptr, &st) < 0)
		goto done;

	supported = (S_ISLNK(st.st_mode) != 0);
done:
	if (path.size)
		(void)p_unlink(path.ptr);
	git_buf_dispose(&path);
	return supported;
}

int git_path_validate_system_file_ownership(const char *path)
{
#ifndef GIT_WIN32
	GIT_UNUSED(path);
	return GIT_OK;
#else
	git_win32_path buf;
	PSID owner_sid;
	PSECURITY_DESCRIPTOR descriptor = NULL;
	HANDLE token;
	TOKEN_USER *info = NULL;
	DWORD err, len;
	int ret;

	if (git_win32_path_from_utf8(buf, path) < 0)
		return -1;

	err = GetNamedSecurityInfoW(buf, SE_FILE_OBJECT,
				    OWNER_SECURITY_INFORMATION |
					    DACL_SECURITY_INFORMATION,
				    &owner_sid, NULL, NULL, NULL, &descriptor);

	if (err == ERROR_FILE_NOT_FOUND || err == ERROR_PATH_NOT_FOUND) {
		ret = GIT_ENOTFOUND;
		goto cleanup;
	}

	if (err != ERROR_SUCCESS) {
		git_error_set(GIT_ERROR_OS, "failed to get security information");
		ret = GIT_ERROR;
		goto cleanup;
	}

	if (!IsValidSid(owner_sid)) {
		git_error_set(GIT_ERROR_INVALID, "programdata configuration file owner is unknown");
		ret = GIT_ERROR;
		goto cleanup;
	}

	if (IsWellKnownSid(owner_sid, WinBuiltinAdministratorsSid) ||
	    IsWellKnownSid(owner_sid, WinLocalSystemSid)) {
		ret = GIT_OK;
		goto cleanup;
	}

	/* Obtain current user's SID */
	if (OpenProcessToken(GetCurrentProcess(), TOKEN_QUERY, &token) &&
	    !GetTokenInformation(token, TokenUser, NULL, 0, &len)) {
		info = git__malloc(len);
		GIT_ERROR_CHECK_ALLOC(info);
		if (!GetTokenInformation(token, TokenUser, info, len, &len)) {
			git__free(info);
			info = NULL;
		}
	}

	/*
	 * If the file is owned by the same account that is running the current
	 * process, it's okay to read from that file.
	 */
	if (info && EqualSid(owner_sid, info->User.Sid))
		ret = GIT_OK;
	else {
		git_error_set(GIT_ERROR_INVALID, "programdata configuration file owner is not valid");
		ret = GIT_ERROR;
	}
	free(info);

cleanup:
	if (descriptor)
		LocalFree(descriptor);

	return ret;
#endif
}
