/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#include "../posix.h"
#include "../fileops.h"
#include "path.h"
#include "utf-conv.h"
#include "repository.h"
#include "reparse.h"
#include <errno.h>
#include <io.h>
#include <fcntl.h>
#include <ws2tcpip.h>

#ifndef FILE_NAME_NORMALIZED
# define FILE_NAME_NORMALIZED 0
#endif

/* Options which we always provide to _wopen.
 *
 * _O_BINARY - Raw access; no translation of CR or LF characters
 * _O_NOINHERIT - Do not mark the created handle as inheritable by child processes.
 *    The Windows default is 'not inheritable', but the CRT's default (following
 *    POSIX convention) is 'inheritable'. We have no desire for our handles to be
 *    inheritable on Windows, so specify the flag to get default behavior back. */
#define STANDARD_OPEN_FLAGS (_O_BINARY | _O_NOINHERIT)

/* GetFinalPathNameByHandleW signature */
typedef DWORD(WINAPI *PFGetFinalPathNameByHandleW)(HANDLE, LPWSTR, DWORD, DWORD);

/* Helper function which converts UTF-8 paths to UTF-16.
 * On failure, errno is set. */
static int utf8_to_16_with_errno(git_win32_path dest, const char *src)
{
	int len = git_win32_path_from_utf8(dest, src);

	if (len < 0) {
		if (GetLastError() == ERROR_INSUFFICIENT_BUFFER)
			errno = ENAMETOOLONG;
		else
			errno = EINVAL; /* Bad code point, presumably */
	}

	return len;
}

int p_mkdir(const char *path, mode_t mode)
{
	git_win32_path buf;

	GIT_UNUSED(mode);

	if (utf8_to_16_with_errno(buf, path) < 0)
		return -1;

	return _wmkdir(buf);
}

int p_unlink(const char *path)
{
	git_win32_path buf;
	int error;

	if (utf8_to_16_with_errno(buf, path) < 0)
		return -1;

	error = _wunlink(buf);

	/* If the file could not be deleted because it was
	 * read-only, clear the bit and try again */
	if (error == -1 && errno == EACCES) {
		_wchmod(buf, 0666);
		error = _wunlink(buf);
	}

	return error;
}

int p_fsync(int fd)
{
	HANDLE fh = (HANDLE)_get_osfhandle(fd);

	if (fh == INVALID_HANDLE_VALUE) {
		errno = EBADF;
		return -1;
	}

	if (!FlushFileBuffers(fh)) {
		DWORD code = GetLastError();

		if (code == ERROR_INVALID_HANDLE)
			errno = EINVAL;
		else
			errno = EIO;

		return -1;
	}

	return 0;
}

GIT_INLINE(time_t) filetime_to_time_t(const FILETIME *ft)
{
	long long winTime = ((long long)ft->dwHighDateTime << 32) + ft->dwLowDateTime;
	winTime -= 116444736000000000LL; /* Windows to Unix Epoch conversion */
	winTime /= 10000000;		 /* Nano to seconds resolution */
	return (time_t)winTime;
}

/* On success, returns the length, in characters, of the path stored in dest.
 * On failure, returns a negative value. */
static int readlink_w(
	git_win32_path dest,
	const git_win32_path path)
{
	BYTE buf[MAXIMUM_REPARSE_DATA_BUFFER_SIZE];
	GIT_REPARSE_DATA_BUFFER *reparse_buf = (GIT_REPARSE_DATA_BUFFER *)buf;
	HANDLE handle = NULL;
	DWORD ioctl_ret;
	wchar_t *target;
	size_t target_len;

	int error = -1;

	handle = CreateFileW(path, GENERIC_READ,
		FILE_SHARE_READ | FILE_SHARE_DELETE, NULL, OPEN_EXISTING,
		FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, NULL);

	if (handle == INVALID_HANDLE_VALUE) {
		errno = ENOENT;
		return -1;
	}

	if (!DeviceIoControl(handle, FSCTL_GET_REPARSE_POINT, NULL, 0,
		reparse_buf, sizeof(buf), &ioctl_ret, NULL)) {
		errno = EINVAL;
		goto on_error;
	}

	switch (reparse_buf->ReparseTag) {
	case IO_REPARSE_TAG_SYMLINK:
		target = reparse_buf->SymbolicLinkReparseBuffer.PathBuffer +
			(reparse_buf->SymbolicLinkReparseBuffer.SubstituteNameOffset / sizeof(WCHAR));
		target_len = reparse_buf->SymbolicLinkReparseBuffer.SubstituteNameLength / sizeof(WCHAR);
		break;
	case IO_REPARSE_TAG_MOUNT_POINT:
		target = reparse_buf->MountPointReparseBuffer.PathBuffer +
			(reparse_buf->MountPointReparseBuffer.SubstituteNameOffset / sizeof(WCHAR));
		target_len = reparse_buf->MountPointReparseBuffer.SubstituteNameLength / sizeof(WCHAR);
		break;
	default:
		errno = EINVAL;
		goto on_error;
	}

	if (target_len) {
		/* The path may need to have a prefix removed. */
		target_len = git_win32__canonicalize_path(target, target_len);

		/* Need one additional character in the target buffer
		 * for the terminating NULL. */
		if (GIT_WIN_PATH_UTF16 > target_len) {
			wcscpy(dest, target);
			error = (int)target_len;
		}
	}

on_error:
	CloseHandle(handle);
	return error;
}

#define WIN32_IS_WSEP(CH) ((CH) == L'/' || (CH) == L'\\')

static int lstat_w(
	wchar_t *path,
	struct stat *buf,
	bool posix_enotdir)
{
	WIN32_FILE_ATTRIBUTE_DATA fdata;

	if (GetFileAttributesExW(path, GetFileExInfoStandard, &fdata)) {
		int fMode = S_IREAD;

		if (!buf)
			return 0;

		if (fdata.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
			fMode |= S_IFDIR;
		else
			fMode |= S_IFREG;

		if (!(fdata.dwFileAttributes & FILE_ATTRIBUTE_READONLY))
			fMode |= S_IWRITE;

		buf->st_ino = 0;
		buf->st_gid = 0;
		buf->st_uid = 0;
		buf->st_nlink = 1;
		buf->st_mode = (mode_t)fMode;
		buf->st_size = ((git_off_t)fdata.nFileSizeHigh << 32) + fdata.nFileSizeLow;
		buf->st_dev = buf->st_rdev = (_getdrive() - 1);
		buf->st_atime = filetime_to_time_t(&(fdata.ftLastAccessTime));
		buf->st_mtime = filetime_to_time_t(&(fdata.ftLastWriteTime));
		buf->st_ctime = filetime_to_time_t(&(fdata.ftCreationTime));

		if (fdata.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT) {
			git_win32_path target;

			if (readlink_w(target, path) >= 0) {
				buf->st_mode = (buf->st_mode & ~S_IFMT) | S_IFLNK;

				/* st_size gets the UTF-8 length of the target name, in bytes,
				 * not counting the NULL terminator */
				if ((buf->st_size = git__utf16_to_8(NULL, 0, target)) < 0)
					return -1;
			}
		}

		return 0;
	}

	errno = ENOENT;

	/* To match POSIX behavior, set ENOTDIR when any of the folders in the
	 * file path is a regular file, otherwise set ENOENT.
	 */
	if (posix_enotdir) {
		size_t path_len = wcslen(path);

		/* scan up path until we find an existing item */
		while (1) {
			DWORD attrs;

			/* remove last directory component */
			for (path_len--; path_len > 0 && !WIN32_IS_WSEP(path[path_len]); path_len--);

			if (path_len <= 0)
				break;

			path[path_len] = L'\0';
			attrs = GetFileAttributesW(path);

			if (attrs != INVALID_FILE_ATTRIBUTES) {
				if (!(attrs & FILE_ATTRIBUTE_DIRECTORY))
					errno = ENOTDIR;
				break;
			}
		}
	}

	return -1;
}

static int do_lstat(const char *path, struct stat *buf, bool posixly_correct)
{
	git_win32_path path_w;
	int len;

	if ((len = utf8_to_16_with_errno(path_w, path)) < 0)
		return -1;

	git_win32__path_trim_end(path_w, len);

	return lstat_w(path_w, buf, posixly_correct);
}

int p_lstat(const char *filename, struct stat *buf)
{
	return do_lstat(filename, buf, false);
}

int p_lstat_posixly(const char *filename, struct stat *buf)
{
	return do_lstat(filename, buf, true);
}

int p_readlink(const char *path, char *buf, size_t bufsiz)
{
	git_win32_path path_w, target_w;
	git_win32_utf8_path target;
	int len;

	/* readlink(2) does not NULL-terminate the string written
	 * to the target buffer. Furthermore, the target buffer need
	 * not be large enough to hold the entire result. A truncated
	 * result should be written in this case. Since this truncation
	 * could occur in the middle of the encoding of a code point,
	 * we need to buffer the result on the stack. */

	if (utf8_to_16_with_errno(path_w, path) < 0 ||
		readlink_w(target_w, path_w) < 0 ||
		(len = git_win32_path_to_utf8(target, target_w)) < 0)
		return -1;

	bufsiz = min((size_t)len, bufsiz);
	memcpy(buf, target, bufsiz);

	return (int)bufsiz;
}

int p_symlink(const char *old, const char *new)
{
	/* Real symlinks on NTFS require admin privileges. Until this changes,
	 * libgit2 just creates a text file with the link target in the contents.
	 */
	return git_futils_fake_symlink(old, new);
}

int p_open(const char *path, int flags, ...)
{
	git_win32_path buf;
	mode_t mode = 0;

	if (utf8_to_16_with_errno(buf, path) < 0)
		return -1;

	if (flags & O_CREAT) {
		va_list arg_list;

		va_start(arg_list, flags);
		mode = (mode_t)va_arg(arg_list, int);
		va_end(arg_list);
	}

	return _wopen(buf, flags | STANDARD_OPEN_FLAGS, mode);
}

int p_creat(const char *path, mode_t mode)
{
	git_win32_path buf;

	if (utf8_to_16_with_errno(buf, path) < 0)
		return -1;

	return _wopen(buf, _O_WRONLY | _O_CREAT | _O_TRUNC | STANDARD_OPEN_FLAGS, mode);
}

int p_getcwd(char *buffer_out, size_t size)
{
	git_win32_path buf;
	wchar_t *cwd = _wgetcwd(buf, GIT_WIN_PATH_UTF16);

	if (!cwd)
		return -1;

	/* Convert the working directory back to UTF-8 */
	if (git__utf16_to_8(buffer_out, size, cwd) < 0) {
		DWORD code = GetLastError();

		if (code == ERROR_INSUFFICIENT_BUFFER)
			errno = ERANGE;
		else
			errno = EINVAL;

		return -1;
	}

	return 0;
}

/*
 * Returns the address of the GetFinalPathNameByHandleW function.
 * This function is available on Windows Vista and higher.
 */
static PFGetFinalPathNameByHandleW get_fpnbyhandle(void)
{
	static PFGetFinalPathNameByHandleW pFunc = NULL;
	PFGetFinalPathNameByHandleW toReturn = pFunc;

	if (!toReturn) {
		HMODULE hModule = GetModuleHandleW(L"kernel32");

		if (hModule)
			toReturn = (PFGetFinalPathNameByHandleW)GetProcAddress(hModule, "GetFinalPathNameByHandleW");

		pFunc = toReturn;
	}

	assert(toReturn);

	return toReturn;
}

static int getfinalpath_w(
	git_win32_path dest,
	const wchar_t *path)
{
	PFGetFinalPathNameByHandleW pgfp = get_fpnbyhandle();
	HANDLE hFile;
	DWORD dwChars;

	if (!pgfp)
		return -1;

	/* Use FILE_FLAG_BACKUP_SEMANTICS so we can open a directory. Do not
	* specify FILE_FLAG_OPEN_REPARSE_POINT; we want to open a handle to the
	* target of the link. */
	hFile = CreateFileW(path, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_DELETE,
		NULL, OPEN_EXISTING, FILE_FLAG_BACKUP_SEMANTICS, NULL);

	if (INVALID_HANDLE_VALUE == hFile)
		return -1;

	/* Call GetFinalPathNameByHandle */
	dwChars = pgfp(hFile, dest, GIT_WIN_PATH_UTF16, FILE_NAME_NORMALIZED);
	CloseHandle(hFile);

	if (!dwChars || dwChars >= GIT_WIN_PATH_UTF16)
		return -1;

	/* The path may be delivered to us with a prefix; canonicalize */
	return (int)git_win32__canonicalize_path(dest, dwChars);
}

static int follow_and_lstat_link(git_win32_path path, struct stat* buf)
{
	git_win32_path target_w;

	if (getfinalpath_w(target_w, path) < 0)
		return -1;

	return lstat_w(target_w, buf, false);
}

int p_stat(const char* path, struct stat* buf)
{
	git_win32_path path_w;
	int len;

	if ((len = utf8_to_16_with_errno(path_w, path)) < 0)
		return -1;

	git_win32__path_trim_end(path_w, len);

	if (lstat_w(path_w, buf, false) < 0)
		return -1;

	/* The item is a symbolic link or mount point. No need to iterate
	 * to follow multiple links; use GetFinalPathNameFromHandle. */
	if (S_ISLNK(buf->st_mode))
		return follow_and_lstat_link(path_w, buf);

	return 0;
}

int p_chdir(const char* path)
{
	git_win32_path buf;

	if (utf8_to_16_with_errno(buf, path) < 0)
		return -1;

	return _wchdir(buf);
}

int p_chmod(const char* path, mode_t mode)
{
	git_win32_path buf;

	if (utf8_to_16_with_errno(buf, path) < 0)
		return -1;

	return _wchmod(buf, mode);
}

int p_rmdir(const char* path)
{
	git_win32_path buf;
	int error;

	if (utf8_to_16_with_errno(buf, path) < 0)
		return -1;

	error = _wrmdir(buf);

	if (error == -1) {
		switch (GetLastError()) {
			/* _wrmdir() is documented to return EACCES if "A program has an open
			 * handle to the directory."  This sounds like what everybody else calls
			 * EBUSY.  Let's convert appropriate error codes.
			 */
			case ERROR_SHARING_VIOLATION:
				errno = EBUSY;
				break;

			/* This error can be returned when trying to rmdir an extant file. */
			case ERROR_DIRECTORY:
				errno = ENOTDIR;
				break;
		}
	}

	return error;
}

char *p_realpath(const char *orig_path, char *buffer)
{
	git_win32_path orig_path_w, buffer_w;

	if (utf8_to_16_with_errno(orig_path_w, orig_path) < 0)
		return NULL;

	/* Note that if the path provided is a relative path, then the current directory
	 * is used to resolve the path -- which is a concurrency issue because the current
	 * directory is a process-wide variable. */
	if (!GetFullPathNameW(orig_path_w, GIT_WIN_PATH_UTF16, buffer_w, NULL)) {
		if (GetLastError() == ERROR_INSUFFICIENT_BUFFER)
			errno = ENAMETOOLONG;
		else
			errno = EINVAL;

		return NULL;
	}

	/* The path must exist. */
	if (GetFileAttributesW(buffer_w) == INVALID_FILE_ATTRIBUTES) {
		errno = ENOENT;
		return NULL;
	}

	/* Convert the path to UTF-8. */
	if (buffer) {
		/* If the caller provided a buffer, then it is assumed to be GIT_WIN_PATH_UTF8
		 * characters in size. If it isn't, then we may overflow. */
		if (git__utf16_to_8(buffer, GIT_WIN_PATH_UTF8, buffer_w) < 0)
			return NULL;
	} else {
		/* If the caller did not provide a buffer, then we allocate one for the caller
		 * from the heap. */
		if (git__utf16_to_8_alloc(&buffer, buffer_w) < 0)
			return NULL;
	}

	/* Convert backslashes to forward slashes */
	git_path_mkposix(buffer);

	return buffer;
}

int p_vsnprintf(char *buffer, size_t count, const char *format, va_list argptr)
{
#ifdef _MSC_VER
	int len;

	if (count == 0 ||
		(len = _vsnprintf_s(buffer, count, _TRUNCATE, format, argptr)) < 0)
		return _vscprintf(format, argptr);

	return len;
#else /* MinGW */
	return vsnprintf(buffer, count, format, argptr);
#endif
}

int p_snprintf(char *buffer, size_t count, const char *format, ...)
{
	va_list va;
	int r;

	va_start(va, format);
	r = p_vsnprintf(buffer, count, format, va);
	va_end(va);

	return r;
}

int p_mkstemp(char *tmp_path)
{
#if defined(_MSC_VER)
	if (_mktemp_s(tmp_path, strlen(tmp_path) + 1) != 0)
		return -1;
#else
	if (_mktemp(tmp_path) == NULL)
		return -1;
#endif

	return p_open(tmp_path, O_RDWR | O_CREAT | O_EXCL, 0744); //-V536
}

int p_access(const char* path, mode_t mode)
{
	git_win32_path buf;

	if (utf8_to_16_with_errno(buf, path) < 0)
		return -1;

	return _waccess(buf, mode);
}

int p_rename(const char *from, const char *to)
{
	git_win32_path wfrom;
	git_win32_path wto;
	int rename_tries;
	int rename_succeeded;
	int error;

	if (utf8_to_16_with_errno(wfrom, from) < 0 ||
		utf8_to_16_with_errno(wto, to) < 0)
		return -1;
	
	/* wait up to 50ms if file is locked by another thread or process */
	rename_tries = 0;
	rename_succeeded = 0;
	while (rename_tries < 10) {
		if (MoveFileExW(wfrom, wto, MOVEFILE_REPLACE_EXISTING | MOVEFILE_COPY_ALLOWED) != 0) {
			rename_succeeded = 1;
			break;
		}
		
		error = GetLastError();
		if (error == ERROR_SHARING_VIOLATION || error == ERROR_ACCESS_DENIED) {
			Sleep(5);
			rename_tries++;
		} else
			break;
	}
	
	return rename_succeeded ? 0 : -1;
}

int p_recv(GIT_SOCKET socket, void *buffer, size_t length, int flags)
{
	if ((size_t)((int)length) != length)
		return -1; /* giterr_set will be done by caller */

	return recv(socket, buffer, (int)length, flags);
}

int p_send(GIT_SOCKET socket, const void *buffer, size_t length, int flags)
{
	if ((size_t)((int)length) != length)
		return -1; /* giterr_set will be done by caller */

	return send(socket, buffer, (int)length, flags);
}

/**
 * Borrowed from http://old.nabble.com/Porting-localtime_r-and-gmtime_r-td15282276.html
 * On Win32, `gmtime_r` doesn't exist but `gmtime` is threadsafe, so we can use that
 */
struct tm *
p_localtime_r (const time_t *timer, struct tm *result)
{
	struct tm *local_result;
	local_result = localtime (timer);

	if (local_result == NULL || result == NULL)
		return NULL;

	memcpy (result, local_result, sizeof (struct tm));
	return result;
}
struct tm *
p_gmtime_r (const time_t *timer, struct tm *result)
{
	struct tm *local_result;
	local_result = gmtime (timer);

	if (local_result == NULL || result == NULL)
		return NULL;

	memcpy (result, local_result, sizeof (struct tm));
	return result;
}

int p_inet_pton(int af, const char *src, void *dst)
{
	struct sockaddr_storage sin;
	void *addr;
	int sin_len = sizeof(struct sockaddr_storage), addr_len;
	int error = 0;

	if (af == AF_INET) {
		addr = &((struct sockaddr_in *)&sin)->sin_addr;
		addr_len = sizeof(struct in_addr);
	} else if (af == AF_INET6) {
		addr = &((struct sockaddr_in6 *)&sin)->sin6_addr;
		addr_len = sizeof(struct in6_addr);
	} else {
		errno = EAFNOSUPPORT;
		return -1;
	}

	if ((error = WSAStringToAddressA((LPSTR)src, af, NULL, (LPSOCKADDR)&sin, &sin_len)) == 0) {
		memcpy(dst, addr, addr_len);
		return 1;
	}

	switch(WSAGetLastError()) {
	case WSAEINVAL:
		return 0;
	case WSAEFAULT:
		errno = ENOSPC;
		return -1;
	case WSA_NOT_ENOUGH_MEMORY:
		errno = ENOMEM;
		return -1;
	}

	errno = EINVAL;
	return -1;
}
