/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"
#include "path.h"
#include "posix.h"
#ifdef GIT_WIN32
#include "win32/posix.h"
#include "win32/w32_util.h"
#else
#include <dirent.h>
#endif
#include <stdio.h>
#include <ctype.h>

#define LOOKS_LIKE_DRIVE_PREFIX(S) (git__isalpha((S)[0]) && (S)[1] == ':')

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
 * Based on the Android implementation, BSD licensed.
 * Check http://android.git.kernel.org/
 */
int git_path_dirname_r(git_buf *buffer, const char *path)
{
	const char *endp;
	int result, len;

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

	/* Cast is safe because max path < max int */
	len = (int)(endp - path + 1);

#ifdef GIT_WIN32
	/* Mimic unix behavior where '/.git' returns '/': 'C:/.git' will return
		'C:/' here */

	if (len == 2 && LOOKS_LIKE_DRIVE_PREFIX(path)) {
		len = 3;
		goto Exit;
	}

	/* Similarly checks if we're dealing with a network computer name
		'//computername/.git' will return '//computername/' */

	if (looks_like_network_computer_name(path, len)) {
		len++;
		goto Exit;
	}

#endif

Exit:
	result = len;

	if (buffer != NULL && git_buf_set(buffer, path, len) < 0)
		return -1;

	return result;
}


char *git_path_dirname(const char *path)
{
	git_buf buf = GIT_BUF_INIT;
	char *dirname;

	git_path_dirname_r(&buf, path);
	dirname = git_buf_detach(&buf);
	git_buf_free(&buf); /* avoid memleak if error occurs */

	return dirname;
}

char *git_path_basename(const char *path)
{
	git_buf buf = GIT_BUF_INIT;
	char *basename;

	git_path_basename_r(&buf, path);
	basename = git_buf_detach(&buf);
	git_buf_free(&buf); /* avoid memleak if error occurs */

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
	int offset = 0;

	/* Does the root of the path look like a windows drive ? */
	if (LOOKS_LIKE_DRIVE_PREFIX(path))
		offset += 2;

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
#endif

	if (path[offset] == '/' || path[offset] == '\\')
		return offset;

	return -1;	/* Not a real error - signals that path is not rooted */
}

int git_path_join_unrooted(
	git_buf *path_out, const char *path, const char *base, ssize_t *root_at)
{
	int error, root;

	assert(path && path_out);

	root = git_path_root(path);

	if (base != NULL && root < 0) {
		error = git_buf_joinpath(path_out, base, path);

		if (root_at)
			*root_at = (ssize_t)strlen(base);
	}
	else {
		error = git_buf_sets(path_out, path);

		if (root_at)
			*root_at = (root < 0) ? 0 : (ssize_t)root;
	}

	return error;
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
		/* giterr_set resets the errno when dealing with a GITERR_OS kind of error */
		int error = (errno == ENOENT || errno == ENOTDIR) ? GIT_ENOTFOUND : -1;
		giterr_set(GITERR_OS, "Failed to resolve path '%s'", path);

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
	giterr_set(GITERR_CONFIG, "'%s' is not a valid local file URI", uri);
	return -1;
}

int git_path_fromurl(git_buf *local_path_out, const char *file_url)
{
	int offset = 0, len;

	assert(local_path_out && file_url);

	if (git__prefixcmp(file_url, "file://") != 0)
		return error_invalid_local_file_uri(file_url);

	offset += 7;
	len = (int)strlen(file_url);

	if (offset < len && file_url[offset] == '/')
		offset++;
	else if (offset < len && git__prefixcmp(file_url + offset, "localhost/") == 0)
		offset += 10;
	else
		return error_invalid_local_file_uri(file_url);

	if (offset >= len || file_url[offset] == '/')
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
	int (*cb)(void *data, git_buf *),
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

	iter.ptr = path->ptr;
	iter.size = git_buf_len(path);
	iter.asize = path->asize;

	while (scan >= stop) {
		error = cb(data, &iter);
		iter.ptr[scan] = oldc;

		if (error) {
			giterr_set_after_callback(error);
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

#ifdef GIT_WIN32

bool git_path_is_empty_dir(const char *path)
{
	git_win32_path filter_w;
	bool empty = false;

	if (git_win32__findfirstfile_filter(filter_w, path)) {
		WIN32_FIND_DATAW findData;
		HANDLE hFind = FindFirstFileW(filter_w, &findData);

		/* If the find handle was created successfully, then it's a directory */
		if (hFind != INVALID_HANDLE_VALUE) {
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
		giterr_clear();
	else
		error = git_path_direach(&dir, 0, path_found_entry, NULL);

	git_buf_free(&dir);

	return !error;
}

#endif

int git_path_set_error(int errno_value, const char *path, const char *action)
{
	switch (errno_value) {
	case ENOENT:
	case ENOTDIR:
		giterr_set(GITERR_OS, "Could not find '%s' to %s", path, action);
		return GIT_ENOTFOUND;

	case EINVAL:
	case ENAMETOOLONG:
		giterr_set(GITERR_OS, "Invalid path for filesystem '%s'", path);
		return GIT_EINVALIDSPEC;

	case EEXIST:
		giterr_set(GITERR_OS, "Failed %s - '%s' already exists", action, path);
		return GIT_EEXISTS;

	default:
		giterr_set(GITERR_OS, "Could not %s '%s'", action, path);
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

	/* leave base valid even if we could not make space for subdir */
	if (git_buf_try_grow(dir, dir_size + sub_size + 2, false, false) < 0)
		return false;

	/* save excursion */
	git_buf_joinpath(dir, dir->ptr, sub);

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

	if (!path || git_buf_oom(path))
		return -1;

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
				giterr_set(GITERR_INVALID,
					"Cannot strip root component off url");
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
	git_buf_joinpath(target, git_buf_cstr(target), relpath);
	return git_path_resolve_relative(target, 0);
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
		git_buf_free(&ic->buf);
	}
}

int git_path_iconv(git_path_iconv_t *ic, char **in, size_t *inlen)
{
	char *nfd = *in, *nfc;
	size_t nfdlen = *inlen, nfclen, wantlen = nfdlen, rv;
	int retry = 1;

	if (!ic || ic->map == (iconv_t)-1 ||
		!git_path_has_non_ascii(*in, *inlen))
		return 0;

	git_buf_clear(&ic->buf);

	while (1) {
		if (git_buf_grow(&ic->buf, wantlen + 1) < 0)
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
	giterr_set(GITERR_OS, "Unable to convert unicode path data");
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
	git_buf_free(&path);
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
	path_dirent_data de_data;
	struct dirent *de, *de_buf = (struct dirent *)&de_data;

	(void)flags;

#ifdef GIT_USE_ICONV
	git_path_iconv_t ic = GIT_PATH_ICONV_INIT;
#endif

	if (git_path_to_dir(path) < 0)
		return -1;

	wd_len = git_buf_len(path);

	if ((dir = opendir(path->ptr)) == NULL) {
		giterr_set(GITERR_OS, "Failed to open directory '%s'", path->ptr);
		if (errno == ENOENT)
			return GIT_ENOTFOUND;

		return -1;
	}

#ifdef GIT_USE_ICONV
	if ((flags & GIT_PATH_DIR_PRECOMPOSE_UNICODE) != 0)
		(void)git_path_iconv_init_precompose(&ic);
#endif

	while (p_readdir_r(dir, de_buf, &de) == 0 && de != NULL) {
		char *de_path = de->d_name;
		size_t de_len = strlen(de_path);

		if (git_path_is_dot_or_dotdot(de_path))
			continue;

#ifdef GIT_USE_ICONV
		if ((error = git_path_iconv(&ic, &de_path, &de_len)) < 0)
			break;
#endif

		if ((error = git_buf_put(path, de_path, de_len)) < 0)
			break;

		error = fn(arg, path);

		git_buf_truncate(path, wd_len); /* restore path */

		if (error != 0) {
			giterr_set_after_callback(error);
			break;
		}
	}

	closedir(dir);

#ifdef GIT_USE_ICONV
	git_path_iconv_clear(&ic);
#endif

	return error;
}

int git_path_dirload(
	const char *path,
	size_t prefix_len,
	size_t alloc_extra,
	unsigned int flags,
	git_vector *contents)
{
	int error, need_slash;
	DIR *dir;
	size_t path_len;
	path_dirent_data de_data;
	struct dirent *de, *de_buf = (struct dirent *)&de_data;

	(void)flags;

#ifdef GIT_USE_ICONV
	git_path_iconv_t ic = GIT_PATH_ICONV_INIT;
#endif

	assert(path && contents);

	path_len = strlen(path);

	if (!path_len || path_len < prefix_len) {
		giterr_set(GITERR_INVALID, "Invalid directory path '%s'", path);
		return -1;
	}
	if ((dir = opendir(path)) == NULL) {
		giterr_set(GITERR_OS, "Failed to open directory '%s'", path);
		return -1;
	}

#ifdef GIT_USE_ICONV
	if ((flags & GIT_PATH_DIR_PRECOMPOSE_UNICODE) != 0)
		(void)git_path_iconv_init_precompose(&ic);
#endif

	path += prefix_len;
	path_len -= prefix_len;
	need_slash = (path_len > 0 && path[path_len-1] != '/') ? 1 : 0;

	while ((error = p_readdir_r(dir, de_buf, &de)) == 0 && de != NULL) {
		char *entry_path, *de_path = de->d_name;
		size_t alloc_size, de_len = strlen(de_path);

		if (git_path_is_dot_or_dotdot(de_path))
			continue;

#ifdef GIT_USE_ICONV
		if ((error = git_path_iconv(&ic, &de_path, &de_len)) < 0)
			break;
#endif

		alloc_size = path_len + need_slash + de_len + 1 + alloc_extra;
		if ((entry_path = git__calloc(alloc_size, 1)) == NULL) {
			error = -1;
			break;
		}

		if (path_len)
			memcpy(entry_path, path, path_len);
		if (need_slash)
			entry_path[path_len] = '/';
		memcpy(&entry_path[path_len + need_slash], de_path, de_len);

		if ((error = git_vector_insert(contents, entry_path)) < 0)
			break;
	}

	closedir(dir);

#ifdef GIT_USE_ICONV
	git_path_iconv_clear(&ic);
#endif

	if (error != 0)
		giterr_set(GITERR_OS, "Failed to process directory entry in '%s'", path);

	return error;
}

int git_path_with_stat_cmp(const void *a, const void *b)
{
	const git_path_with_stat *psa = a, *psb = b;
	return strcmp(psa->path, psb->path);
}

int git_path_with_stat_cmp_icase(const void *a, const void *b)
{
	const git_path_with_stat *psa = a, *psb = b;
	return strcasecmp(psa->path, psb->path);
}

int git_path_dirload_with_stat(
	const char *path,
	size_t prefix_len,
	unsigned int flags,
	const char *start_stat,
	const char *end_stat,
	git_vector *contents)
{
	int error;
	unsigned int i;
	git_path_with_stat *ps;
	git_buf full = GIT_BUF_INIT;
	int (*strncomp)(const char *a, const char *b, size_t sz);
	size_t start_len = start_stat ? strlen(start_stat) : 0;
	size_t end_len = end_stat ? strlen(end_stat) : 0, cmp_len;

	if (git_buf_set(&full, path, prefix_len) < 0)
		return -1;

	error = git_path_dirload(
		path, prefix_len, sizeof(git_path_with_stat) + 1, flags, contents);
	if (error < 0) {
		git_buf_free(&full);
		return error;
	}

	strncomp = (flags & GIT_PATH_DIR_IGNORE_CASE) != 0 ?
		git__strncasecmp : git__strncmp;

	/* stat struct at start of git_path_with_stat, so shift path text */
	git_vector_foreach(contents, i, ps) {
		size_t path_len = strlen((char *)ps);
		memmove(ps->path, ps, path_len + 1);
		ps->path_len = path_len;
	}

	git_vector_foreach(contents, i, ps) {
		/* skip if before start_stat or after end_stat */
		cmp_len = min(start_len, ps->path_len);
		if (cmp_len && strncomp(ps->path, start_stat, cmp_len) < 0)
			continue;
		cmp_len = min(end_len, ps->path_len);
		if (cmp_len && strncomp(ps->path, end_stat, cmp_len) > 0)
			continue;

		git_buf_truncate(&full, prefix_len);

		if ((error = git_buf_joinpath(&full, full.ptr, ps->path)) < 0 ||
			(error = git_path_lstat(full.ptr, &ps->st)) < 0) {
			if (error == GIT_ENOTFOUND) {
				giterr_clear();
				error = 0;
				git_vector_remove(contents, i--);
				continue;
			}

			break;
		}

		if (S_ISDIR(ps->st.st_mode)) {
			ps->path[ps->path_len++] = '/';
			ps->path[ps->path_len] = '\0';
		}
	}

	/* sort now that directory suffix is added */
	git_vector_sort(contents);

	git_buf_free(&full);

	return error;
}
