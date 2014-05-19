/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "common.h"
#include "fileops.h"
#include "global.h"
#include <ctype.h>
#if GIT_WIN32
#include "win32/findfile.h"
#endif

int git_futils_mkpath2file(const char *file_path, const mode_t mode)
{
	return git_futils_mkdir(
		file_path, NULL, mode,
		GIT_MKDIR_PATH | GIT_MKDIR_SKIP_LAST | GIT_MKDIR_VERIFY_DIR);
}

int git_futils_mktmp(git_buf *path_out, const char *filename, mode_t mode)
{
	int fd;
	mode_t mask;

	p_umask(mask = p_umask(0));

	git_buf_sets(path_out, filename);
	git_buf_puts(path_out, "_git2_XXXXXX");

	if (git_buf_oom(path_out))
		return -1;

	if ((fd = p_mkstemp(path_out->ptr)) < 0) {
		giterr_set(GITERR_OS,
			"Failed to create temporary file '%s'", path_out->ptr);
		return -1;
	}

	if (p_chmod(path_out->ptr, (mode & ~mask))) {
		giterr_set(GITERR_OS,
			"Failed to set permissions on file '%s'", path_out->ptr);
		return -1;
	}

	return fd;
}

int git_futils_creat_withpath(const char *path, const mode_t dirmode, const mode_t mode)
{
	int fd;

	if (git_futils_mkpath2file(path, dirmode) < 0)
		return -1;

	fd = p_creat(path, mode);
	if (fd < 0) {
		giterr_set(GITERR_OS, "Failed to create file '%s'", path);
		return -1;
	}

	return fd;
}

int git_futils_creat_locked(const char *path, const mode_t mode)
{
	int fd = p_open(path, O_WRONLY | O_CREAT | O_TRUNC |
		O_EXCL | O_BINARY | O_CLOEXEC, mode);

	if (fd < 0) {
		giterr_set(GITERR_OS, "Failed to create locked file '%s'", path);
		return errno == EEXIST ? GIT_ELOCKED : -1;
	}

	return fd;
}

int git_futils_creat_locked_withpath(const char *path, const mode_t dirmode, const mode_t mode)
{
	if (git_futils_mkpath2file(path, dirmode) < 0)
		return -1;

	return git_futils_creat_locked(path, mode);
}

int git_futils_open_ro(const char *path)
{
	int fd = p_open(path, O_RDONLY);
	if (fd < 0)
		return git_path_set_error(errno, path, "open");
	return fd;
}

git_off_t git_futils_filesize(git_file fd)
{
	struct stat sb;

	if (p_fstat(fd, &sb)) {
		giterr_set(GITERR_OS, "Failed to stat file descriptor");
		return -1;
	}

	return sb.st_size;
}

mode_t git_futils_canonical_mode(mode_t raw_mode)
{
	if (S_ISREG(raw_mode))
		return S_IFREG | GIT_PERMS_CANONICAL(raw_mode);
	else if (S_ISLNK(raw_mode))
		return S_IFLNK;
	else if (S_ISGITLINK(raw_mode))
		return S_IFGITLINK;
	else if (S_ISDIR(raw_mode))
		return S_IFDIR;
	else
		return 0;
}

int git_futils_readbuffer_fd(git_buf *buf, git_file fd, size_t len)
{
	ssize_t read_size = 0;

	git_buf_clear(buf);

	if (git_buf_grow(buf, len + 1) < 0)
		return -1;

	/* p_read loops internally to read len bytes */
	read_size = p_read(fd, buf->ptr, len);

	if (read_size != (ssize_t)len) {
		giterr_set(GITERR_OS, "Failed to read descriptor");
		git_buf_free(buf);
		return -1;
	}

	buf->ptr[read_size] = '\0';
	buf->size = read_size;

	return 0;
}

int git_futils_readbuffer_updated(
	git_buf *buf, const char *path, time_t *mtime, size_t *size, int *updated)
{
	git_file fd;
	struct stat st;
	bool changed = false;

	assert(buf && path && *path);

	if (updated != NULL)
		*updated = 0;

	if (p_stat(path, &st) < 0)
		return git_path_set_error(errno, path, "stat");


	if (S_ISDIR(st.st_mode)) {
		giterr_set(GITERR_INVALID, "requested file is a directory");
		return GIT_ENOTFOUND;
	}

	if (!git__is_sizet(st.st_size+1)) {
		giterr_set(GITERR_OS, "Invalid regular file stat for '%s'", path);
		return -1;
	}

	/*
	 * If we were given a time and/or a size, we only want to read the file
	 * if it has been modified.
	 */
	if (size && *size != (size_t)st.st_size)
		changed = true;
	if (mtime && *mtime != st.st_mtime)
		changed = true;
	if (!size && !mtime)
		changed = true;

	if (!changed) {
		return 0;
	}

	if (mtime != NULL)
		*mtime = st.st_mtime;
	if (size != NULL)
		*size = (size_t)st.st_size;

	if ((fd = git_futils_open_ro(path)) < 0)
		return fd;

	if (git_futils_readbuffer_fd(buf, fd, (size_t)st.st_size) < 0) {
		p_close(fd);
		return -1;
	}

	p_close(fd);

	if (updated != NULL)
		*updated = 1;

	return 0;
}

int git_futils_readbuffer(git_buf *buf, const char *path)
{
	return git_futils_readbuffer_updated(buf, path, NULL, NULL, NULL);
}

int git_futils_writebuffer(
	const git_buf *buf,	const char *path, int flags, mode_t mode)
{
	int fd, error = 0;

	if (flags <= 0)
		flags = O_CREAT | O_TRUNC | O_WRONLY;
	if (!mode)
		mode = GIT_FILEMODE_BLOB;

	if ((fd = p_open(path, flags, mode)) < 0) {
		giterr_set(GITERR_OS, "Could not open '%s' for writing", path);
		return fd;
	}

	if ((error = p_write(fd, git_buf_cstr(buf), git_buf_len(buf))) < 0) {
		giterr_set(GITERR_OS, "Could not write to '%s'", path);
		(void)p_close(fd);
		return error;
	}

	if ((error = p_close(fd)) < 0)
		giterr_set(GITERR_OS, "Error while closing '%s'", path);

	return error;
}

int git_futils_mv_withpath(const char *from, const char *to, const mode_t dirmode)
{
	if (git_futils_mkpath2file(to, dirmode) < 0)
		return -1;

	if (p_rename(from, to) < 0) {
		giterr_set(GITERR_OS, "Failed to rename '%s' to '%s'", from, to);
		return -1;
	}

	return 0;
}

int git_futils_mmap_ro(git_map *out, git_file fd, git_off_t begin, size_t len)
{
	return p_mmap(out, len, GIT_PROT_READ, GIT_MAP_SHARED, fd, begin);
}

int git_futils_mmap_ro_file(git_map *out, const char *path)
{
	git_file fd = git_futils_open_ro(path);
	git_off_t len;
	int result;

	if (fd < 0)
		return fd;

	len = git_futils_filesize(fd);
	if (!git__is_sizet(len)) {
		giterr_set(GITERR_OS, "File `%s` too large to mmap", path);
		return -1;
	}

	result = git_futils_mmap_ro(out, fd, 0, (size_t)len);
	p_close(fd);
	return result;
}

void git_futils_mmap_free(git_map *out)
{
	p_munmap(out);
}

int git_futils_mkdir(
	const char *path,
	const char *base,
	mode_t mode,
	uint32_t flags)
{
	int error = -1;
	git_buf make_path = GIT_BUF_INIT;
	ssize_t root = 0, min_root_len;
	char lastch = '/', *tail;
	struct stat st;

	/* build path and find "root" where we should start calling mkdir */
	if (git_path_join_unrooted(&make_path, path, base, &root) < 0)
		return -1;

	if (make_path.size == 0) {
		giterr_set(GITERR_OS, "Attempt to create empty path");
		goto done;
	}

	/* remove trailing slashes on path */
	while (make_path.ptr[make_path.size - 1] == '/') {
		make_path.size--;
		make_path.ptr[make_path.size] = '\0';
	}

	/* if we are not supposed to made the last element, truncate it */
	if ((flags & GIT_MKDIR_SKIP_LAST2) != 0) {
		git_buf_rtruncate_at_char(&make_path, '/');
		flags |= GIT_MKDIR_SKIP_LAST;
	}
	if ((flags & GIT_MKDIR_SKIP_LAST) != 0)
		git_buf_rtruncate_at_char(&make_path, '/');

	/* if nothing left after truncation, then we're done! */
	if (!make_path.size) {
		error = 0;
		goto done;
	}

	/* if we are not supposed to make the whole path, reset root */
	if ((flags & GIT_MKDIR_PATH) == 0)
		root = git_buf_rfind(&make_path, '/');

	/* advance root past drive name or network mount prefix */
	min_root_len = git_path_root(make_path.ptr);
	if (root < min_root_len)
		root = min_root_len;
	while (root >= 0 && make_path.ptr[root] == '/')
		++root;

	/* clip root to make_path length */
	if (root > (ssize_t)make_path.size)
		root = (ssize_t)make_path.size; /* i.e. NUL byte of string */
	if (root < 0)
		root = 0;

	/* walk down tail of path making each directory */
	for (tail = &make_path.ptr[root]; *tail; *tail = lastch) {

		/* advance tail to include next path component */
		while (*tail == '/')
			tail++;
		while (*tail && *tail != '/')
			tail++;

		/* truncate path at next component */
		lastch = *tail;
		*tail = '\0';
		st.st_mode = 0;

		/* make directory */
		if (p_mkdir(make_path.ptr, mode) < 0) {
			int tmp_errno = giterr_system_last();

			/* ignore error if directory already exists */
			if (p_stat(make_path.ptr, &st) < 0 || !S_ISDIR(st.st_mode)) {
				giterr_system_set(tmp_errno);
				giterr_set(GITERR_OS, "Failed to make directory '%s'", make_path.ptr);
				goto done;
			}

			/* with exclusive create, existing dir is an error */
			if ((flags & GIT_MKDIR_EXCL) != 0) {
				giterr_set(GITERR_OS, "Directory already exists '%s'", make_path.ptr);
				error = GIT_EEXISTS;
				goto done;
			}
		}

		/* chmod if requested and necessary */
		if (((flags & GIT_MKDIR_CHMOD_PATH) != 0 ||
			 (lastch == '\0' && (flags & GIT_MKDIR_CHMOD) != 0)) &&
			st.st_mode != mode &&
			(error = p_chmod(make_path.ptr, mode)) < 0) {
			giterr_set(GITERR_OS, "Failed to set permissions on '%s'", make_path.ptr);
			goto done;
		}
	}

	error = 0;

	/* check that full path really is a directory if requested & needed */
	if ((flags & GIT_MKDIR_VERIFY_DIR) != 0 &&
		lastch != '\0' &&
		(p_stat(make_path.ptr, &st) < 0 || !S_ISDIR(st.st_mode))) {
		giterr_set(GITERR_OS, "Path is not a directory '%s'", make_path.ptr);
		error = GIT_ENOTFOUND;
	}

done:
	git_buf_free(&make_path);
	return error;
}

int git_futils_mkdir_r(const char *path, const char *base, const mode_t mode)
{
	return git_futils_mkdir(path, base, mode, GIT_MKDIR_PATH);
}

typedef struct {
	const char *base;
	size_t baselen;
	uint32_t flags;
	int depth;
} futils__rmdir_data;

#define FUTILS_MAX_DEPTH 100

static int futils__error_cannot_rmdir(const char *path, const char *filemsg)
{
	if (filemsg)
		giterr_set(GITERR_OS, "Could not remove directory. File '%s' %s",
				   path, filemsg);
	else
		giterr_set(GITERR_OS, "Could not remove directory '%s'", path);

	return -1;
}

static int futils__rm_first_parent(git_buf *path, const char *ceiling)
{
	int error = GIT_ENOTFOUND;
	struct stat st;

	while (error == GIT_ENOTFOUND) {
		git_buf_rtruncate_at_char(path, '/');

		if (!path->size || git__prefixcmp(path->ptr, ceiling) != 0)
			error = 0;
		else if (p_lstat_posixly(path->ptr, &st) == 0) {
			if (S_ISREG(st.st_mode) || S_ISLNK(st.st_mode))
				error = p_unlink(path->ptr);
			else if (!S_ISDIR(st.st_mode))
				error = -1; /* fail to remove non-regular file */
		} else if (errno != ENOTDIR)
			error = -1;
	}

	if (error)
		futils__error_cannot_rmdir(path->ptr, "cannot remove parent");

	return error;
}

static int futils__rmdir_recurs_foreach(void *opaque, git_buf *path)
{
	int error = 0;
	futils__rmdir_data *data = opaque;
	struct stat st;

	if (data->depth > FUTILS_MAX_DEPTH)
		error = futils__error_cannot_rmdir(
			path->ptr, "directory nesting too deep");

	else if ((error = p_lstat_posixly(path->ptr, &st)) < 0) {
		if (errno == ENOENT)
			error = 0;
		else if (errno == ENOTDIR) {
			/* asked to remove a/b/c/d/e and a/b is a normal file */
			if ((data->flags & GIT_RMDIR_REMOVE_BLOCKERS) != 0)
				error = futils__rm_first_parent(path, data->base);
			else
				futils__error_cannot_rmdir(
					path->ptr, "parent is not directory");
		}
		else
			error = git_path_set_error(errno, path->ptr, "rmdir");
	}

	else if (S_ISDIR(st.st_mode)) {
		data->depth++;

		error = git_path_direach(path, 0, futils__rmdir_recurs_foreach, data);

		data->depth--;

		if (error < 0)
			return error;

		if (data->depth == 0 && (data->flags & GIT_RMDIR_SKIP_ROOT) != 0)
			return error;

		if ((error = p_rmdir(path->ptr)) < 0) {
			if ((data->flags & GIT_RMDIR_SKIP_NONEMPTY) != 0 &&
				(errno == ENOTEMPTY || errno == EEXIST || errno == EBUSY))
				error = 0;
			else
				error = git_path_set_error(errno, path->ptr, "rmdir");
		}
	}

	else if ((data->flags & GIT_RMDIR_REMOVE_FILES) != 0) {
		if (p_unlink(path->ptr) < 0)
			error = git_path_set_error(errno, path->ptr, "remove");
	}

	else if ((data->flags & GIT_RMDIR_SKIP_NONEMPTY) == 0)
		error = futils__error_cannot_rmdir(path->ptr, "still present");

	return error;
}

static int futils__rmdir_empty_parent(void *opaque, git_buf *path)
{
	futils__rmdir_data *data = opaque;
	int error = 0;

	if (git_buf_len(path) <= data->baselen)
		error = GIT_ITEROVER;

	else if (p_rmdir(git_buf_cstr(path)) < 0) {
		int en = errno;

		if (en == ENOENT || en == ENOTDIR) {
			/* do nothing */
		} else if (en == ENOTEMPTY || en == EEXIST || en == EBUSY) {
			error = GIT_ITEROVER;
		} else {
			error = git_path_set_error(errno, git_buf_cstr(path), "rmdir");
		}
	}

	return error;
}

int git_futils_rmdir_r(
	const char *path, const char *base, uint32_t flags)
{
	int error;
	git_buf fullpath = GIT_BUF_INIT;
	futils__rmdir_data data;

	/* build path and find "root" where we should start calling mkdir */
	if (git_path_join_unrooted(&fullpath, path, base, NULL) < 0)
		return -1;

	memset(&data, 0, sizeof(data));
	data.base    = base ? base : "";
	data.baselen = base ? strlen(base) : 0;
	data.flags   = flags;

	error = futils__rmdir_recurs_foreach(&data, &fullpath);

	/* remove now-empty parents if requested */
	if (!error && (flags & GIT_RMDIR_EMPTY_PARENTS) != 0)
		error = git_path_walk_up(
			&fullpath, base, futils__rmdir_empty_parent, &data);

	if (error == GIT_ITEROVER) {
		giterr_clear();
		error = 0;
	}

	git_buf_free(&fullpath);

	return error;
}

int git_futils_fake_symlink(const char *old, const char *new)
{
	int retcode = GIT_ERROR;
	int fd = git_futils_creat_withpath(new, 0755, 0644);
	if (fd >= 0) {
		retcode = p_write(fd, old, strlen(old));
		p_close(fd);
	}
	return retcode;
}

static int cp_by_fd(int ifd, int ofd, bool close_fd_when_done)
{
	int error = 0;
	char buffer[4096];
	ssize_t len = 0;

	while (!error && (len = p_read(ifd, buffer, sizeof(buffer))) > 0)
		/* p_write() does not have the same semantics as write().  It loops
		 * internally and will return 0 when it has completed writing.
		 */
		error = p_write(ofd, buffer, len);

	if (len < 0) {
		giterr_set(GITERR_OS, "Read error while copying file");
		error = (int)len;
	}

	if (close_fd_when_done) {
		p_close(ifd);
		p_close(ofd);
	}

	return error;
}

int git_futils_cp(const char *from, const char *to, mode_t filemode)
{
	int ifd, ofd;

	if ((ifd = git_futils_open_ro(from)) < 0)
		return ifd;

	if ((ofd = p_open(to, O_WRONLY | O_CREAT | O_EXCL, filemode)) < 0) {
		p_close(ifd);
		return git_path_set_error(errno, to, "open for writing");
	}

	return cp_by_fd(ifd, ofd, true);
}

static int cp_link(const char *from, const char *to, size_t link_size)
{
	int error = 0;
	ssize_t read_len;
	char *link_data = git__malloc(link_size + 1);
	GITERR_CHECK_ALLOC(link_data);

	read_len = p_readlink(from, link_data, link_size);
	if (read_len != (ssize_t)link_size) {
		giterr_set(GITERR_OS, "Failed to read symlink data for '%s'", from);
		error = -1;
	}
	else {
		link_data[read_len] = '\0';

		if (p_symlink(link_data, to) < 0) {
			giterr_set(GITERR_OS, "Could not symlink '%s' as '%s'",
				link_data, to);
			error = -1;
		}
	}

	git__free(link_data);
	return error;
}

typedef struct {
	const char *to_root;
	git_buf to;
	ssize_t from_prefix;
	uint32_t flags;
	uint32_t mkdir_flags;
	mode_t dirmode;
} cp_r_info;

#define GIT_CPDIR__MKDIR_DONE_FOR_TO_ROOT (1u << 10)

static int _cp_r_mkdir(cp_r_info *info, git_buf *from)
{
	int error = 0;

	/* create root directory the first time we need to create a directory */
	if ((info->flags & GIT_CPDIR__MKDIR_DONE_FOR_TO_ROOT) == 0) {
		error = git_futils_mkdir(
			info->to_root, NULL, info->dirmode,
			(info->flags & GIT_CPDIR_CHMOD_DIRS) ? GIT_MKDIR_CHMOD : 0);

		info->flags |= GIT_CPDIR__MKDIR_DONE_FOR_TO_ROOT;
	}

	/* create directory with root as base to prevent excess chmods */
	if (!error)
		error = git_futils_mkdir(
			from->ptr + info->from_prefix, info->to_root,
			info->dirmode, info->mkdir_flags);

	return error;
}

static int _cp_r_callback(void *ref, git_buf *from)
{
	int error = 0;
	cp_r_info *info = ref;
	struct stat from_st, to_st;
	bool exists = false;

	if ((info->flags & GIT_CPDIR_COPY_DOTFILES) == 0 &&
		from->ptr[git_path_basename_offset(from)] == '.')
		return 0;

	if ((error = git_buf_joinpath(
			&info->to, info->to_root, from->ptr + info->from_prefix)) < 0)
		return error;

	if (!(error = git_path_lstat(info->to.ptr, &to_st)))
		exists = true;
	else if (error != GIT_ENOTFOUND)
		return error;
	else {
		giterr_clear();
		error = 0;
	}

	if ((error = git_path_lstat(from->ptr, &from_st)) < 0)
		return error;

	if (S_ISDIR(from_st.st_mode)) {
		mode_t oldmode = info->dirmode;

		/* if we are not chmod'ing, then overwrite dirmode */
		if ((info->flags & GIT_CPDIR_CHMOD_DIRS) == 0)
			info->dirmode = from_st.st_mode;

		/* make directory now if CREATE_EMPTY_DIRS is requested and needed */
		if (!exists && (info->flags & GIT_CPDIR_CREATE_EMPTY_DIRS) != 0)
			error = _cp_r_mkdir(info, from);

		/* recurse onto target directory */
		if (!error && (!exists || S_ISDIR(to_st.st_mode)))
			error = git_path_direach(from, 0, _cp_r_callback, info);

		if (oldmode != 0)
			info->dirmode = oldmode;

		return error;
	}

	if (exists) {
		if ((info->flags & GIT_CPDIR_OVERWRITE) == 0)
			return 0;

		if (p_unlink(info->to.ptr) < 0) {
			giterr_set(GITERR_OS, "Cannot overwrite existing file '%s'",
				info->to.ptr);
			return GIT_EEXISTS;
		}
	}

	/* Done if this isn't a regular file or a symlink */
	if (!S_ISREG(from_st.st_mode) &&
		(!S_ISLNK(from_st.st_mode) ||
		 (info->flags & GIT_CPDIR_COPY_SYMLINKS) == 0))
		return 0;

	/* Make container directory on demand if needed */
	if ((info->flags & GIT_CPDIR_CREATE_EMPTY_DIRS) == 0 &&
		(error = _cp_r_mkdir(info, from)) < 0)
		return error;

	/* make symlink or regular file */
	if (S_ISLNK(from_st.st_mode))
		error = cp_link(from->ptr, info->to.ptr, (size_t)from_st.st_size);
	else {
		mode_t usemode = from_st.st_mode;

		if ((info->flags & GIT_CPDIR_SIMPLE_TO_MODE) != 0)
			usemode = GIT_PERMS_FOR_WRITE(usemode);

		error = git_futils_cp(from->ptr, info->to.ptr, usemode);
	}

	return error;
}

int git_futils_cp_r(
	const char *from,
	const char *to,
	uint32_t flags,
	mode_t dirmode)
{
	int error;
	git_buf path = GIT_BUF_INIT;
	cp_r_info info;

	if (git_buf_joinpath(&path, from, "") < 0) /* ensure trailing slash */
		return -1;

	memset(&info, 0, sizeof(info));
	info.to_root = to;
	info.flags   = flags;
	info.dirmode = dirmode;
	info.from_prefix = path.size;
	git_buf_init(&info.to, 0);

	/* precalculate mkdir flags */
	if ((flags & GIT_CPDIR_CREATE_EMPTY_DIRS) == 0) {
		/* if not creating empty dirs, then use mkdir to create the path on
		 * demand right before files are copied.
		 */
		info.mkdir_flags = GIT_MKDIR_PATH | GIT_MKDIR_SKIP_LAST;
		if ((flags & GIT_CPDIR_CHMOD_DIRS) != 0)
			info.mkdir_flags |= GIT_MKDIR_CHMOD_PATH;
	} else {
		/* otherwise, we will do simple mkdir as directories are encountered */
		info.mkdir_flags =
			((flags & GIT_CPDIR_CHMOD_DIRS) != 0) ? GIT_MKDIR_CHMOD : 0;
	}

	error = _cp_r_callback(&info, &path);

	git_buf_free(&path);
	git_buf_free(&info.to);

	return error;
}

int git_futils_filestamp_check(
	git_futils_filestamp *stamp, const char *path)
{
	struct stat st;

	/* if the stamp is NULL, then always reload */
	if (stamp == NULL)
		return 1;

	if (p_stat(path, &st) < 0)
		return GIT_ENOTFOUND;

	if (stamp->mtime == (git_time_t)st.st_mtime &&
		stamp->size  == (git_off_t)st.st_size   &&
		stamp->ino   == (unsigned int)st.st_ino)
		return 0;

	stamp->mtime = (git_time_t)st.st_mtime;
	stamp->size  = (git_off_t)st.st_size;
	stamp->ino   = (unsigned int)st.st_ino;

	return 1;
}

void git_futils_filestamp_set(
	git_futils_filestamp *target, const git_futils_filestamp *source)
{
	assert(target);

	if (source)
		memcpy(target, source, sizeof(*target));
	else
		memset(target, 0, sizeof(*target));
}


void git_futils_filestamp_set_from_stat(
	git_futils_filestamp *stamp, struct stat *st)
{
	if (st) {
		stamp->mtime = (git_time_t)st->st_mtime;
		stamp->size  = (git_off_t)st->st_size;
		stamp->ino   = (unsigned int)st->st_ino;
	} else {
		memset(stamp, 0, sizeof(*stamp));
	}
}
