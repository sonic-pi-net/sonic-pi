#ifdef _WIN32

#define RM_RETRY_COUNT	5
#define RM_RETRY_DELAY	10

#ifdef __MINGW32__

/* These security-enhanced functions are not available
 * in MinGW, so just use the vanilla ones */
#define wcscpy_s(a, b, c) wcscpy((a), (c))
#define wcscat_s(a, b, c) wcscat((a), (c))

#endif /* __MINGW32__ */

static int
fs__dotordotdot(WCHAR *_tocheck)
{
	return _tocheck[0] == '.' &&
		(_tocheck[1] == '\0' ||
		 (_tocheck[1] == '.' && _tocheck[2] == '\0'));
}

static int
fs_rmdir_rmdir(WCHAR *_wpath)
{
	unsigned retries = 1;

	while (!RemoveDirectoryW(_wpath)) {
		/* Only retry when we have retries remaining, and the
		 * error was ERROR_DIR_NOT_EMPTY. */
		if (retries++ > RM_RETRY_COUNT ||
			ERROR_DIR_NOT_EMPTY != GetLastError())
			return -1;

		/* Give whatever has a handle to a child item some time
		 * to release it before trying again */
		Sleep(RM_RETRY_DELAY * retries * retries);
	}

	return 0;
}

static void
fs_rmdir_helper(WCHAR *_wsource)
{
	WCHAR buffer[MAX_PATH];
	HANDLE find_handle;
	WIN32_FIND_DATAW find_data;
	size_t buffer_prefix_len;

	/* Set up the buffer and capture the length */
	wcscpy_s(buffer, MAX_PATH, _wsource);
	wcscat_s(buffer, MAX_PATH, L"\\");
	buffer_prefix_len = wcslen(buffer);

	/* FindFirstFile needs a wildcard to match multiple items */
	wcscat_s(buffer, MAX_PATH, L"*");
	find_handle = FindFirstFileW(buffer, &find_data);
	cl_assert(INVALID_HANDLE_VALUE != find_handle);

	do {
		/* FindFirstFile/FindNextFile gives back . and ..
		 * entries at the beginning */
		if (fs__dotordotdot(find_data.cFileName))
			continue;

		wcscpy_s(buffer + buffer_prefix_len, MAX_PATH - buffer_prefix_len, find_data.cFileName);

		if (FILE_ATTRIBUTE_DIRECTORY & find_data.dwFileAttributes)
			fs_rmdir_helper(buffer);
		else {
			/* If set, the +R bit must be cleared before deleting */
			if (FILE_ATTRIBUTE_READONLY & find_data.dwFileAttributes)
				cl_assert(SetFileAttributesW(buffer, find_data.dwFileAttributes & ~FILE_ATTRIBUTE_READONLY));

			cl_assert(DeleteFileW(buffer));
		}
	}
	while (FindNextFileW(find_handle, &find_data));

	/* Ensure that we successfully completed the enumeration */
	cl_assert(ERROR_NO_MORE_FILES == GetLastError());

	/* Close the find handle */
	FindClose(find_handle);

	/* Now that the directory is empty, remove it */
	cl_assert(0 == fs_rmdir_rmdir(_wsource));
}

static int
fs_rm_wait(WCHAR *_wpath)
{
	unsigned retries = 1;
	DWORD last_error;

	do {
		if (INVALID_FILE_ATTRIBUTES == GetFileAttributesW(_wpath))
			last_error = GetLastError();
		else
			last_error = ERROR_SUCCESS;

		/* Is the item gone? */
		if (ERROR_FILE_NOT_FOUND == last_error ||
			ERROR_PATH_NOT_FOUND == last_error)
			return 0;

		Sleep(RM_RETRY_DELAY * retries * retries);	
	}
	while (retries++ <= RM_RETRY_COUNT);

	return -1;
}

static void
fs_rm(const char *_source)
{
	WCHAR wsource[MAX_PATH];
	DWORD attrs;

	/* The input path is UTF-8. Convert it to wide characters
	 * for use with the Windows API */
	cl_assert(MultiByteToWideChar(CP_UTF8,
				MB_ERR_INVALID_CHARS,
				_source,
				-1, /* Indicates NULL termination */
				wsource,
				MAX_PATH));

	/* Does the item exist? If not, we have no work to do */
	attrs = GetFileAttributesW(wsource);

	if (INVALID_FILE_ATTRIBUTES == attrs)
		return;

	if (FILE_ATTRIBUTE_DIRECTORY & attrs)
		fs_rmdir_helper(wsource);
	else {
		/* The item is a file. Strip the +R bit */
		if (FILE_ATTRIBUTE_READONLY & attrs)
			cl_assert(SetFileAttributesW(wsource, attrs & ~FILE_ATTRIBUTE_READONLY));

		cl_assert(DeleteFileW(wsource));
	}

	/* Wait for the DeleteFile or RemoveDirectory call to complete */
	cl_assert(0 == fs_rm_wait(wsource));
}

static void
fs_copydir_helper(WCHAR *_wsource, WCHAR *_wdest)
{
	WCHAR buf_source[MAX_PATH], buf_dest[MAX_PATH];
	HANDLE find_handle;
	WIN32_FIND_DATAW find_data;
	size_t buf_source_prefix_len, buf_dest_prefix_len;

	wcscpy_s(buf_source, MAX_PATH, _wsource);
	wcscat_s(buf_source, MAX_PATH, L"\\");
	buf_source_prefix_len = wcslen(buf_source);

	wcscpy_s(buf_dest, MAX_PATH, _wdest);
	wcscat_s(buf_dest, MAX_PATH, L"\\");
	buf_dest_prefix_len = wcslen(buf_dest);

	/* Get an enumerator for the items in the source. */
	wcscat_s(buf_source, MAX_PATH, L"*");
	find_handle = FindFirstFileW(buf_source, &find_data);
	cl_assert(INVALID_HANDLE_VALUE != find_handle);

	/* Create the target directory. */
	cl_assert(CreateDirectoryW(_wdest, NULL));

	do {
		/* FindFirstFile/FindNextFile gives back . and ..
		 * entries at the beginning */
		if (fs__dotordotdot(find_data.cFileName))
			continue;

		wcscpy_s(buf_source + buf_source_prefix_len, MAX_PATH - buf_source_prefix_len, find_data.cFileName);
		wcscpy_s(buf_dest + buf_dest_prefix_len, MAX_PATH - buf_dest_prefix_len, find_data.cFileName);

		if (FILE_ATTRIBUTE_DIRECTORY & find_data.dwFileAttributes)
			fs_copydir_helper(buf_source, buf_dest);
		else
			cl_assert(CopyFileW(buf_source, buf_dest, TRUE));
	}
	while (FindNextFileW(find_handle, &find_data));

	/* Ensure that we successfully completed the enumeration */
	cl_assert(ERROR_NO_MORE_FILES == GetLastError());

	/* Close the find handle */
	FindClose(find_handle);
}

static void
fs_copy(const char *_source, const char *_dest)
{
	WCHAR wsource[MAX_PATH], wdest[MAX_PATH];
	DWORD source_attrs, dest_attrs;
	HANDLE find_handle;
	WIN32_FIND_DATAW find_data;

	/* The input paths are UTF-8. Convert them to wide characters
	 * for use with the Windows API. */
	cl_assert(MultiByteToWideChar(CP_UTF8,
				MB_ERR_INVALID_CHARS,
				_source,
				-1,
				wsource,
				MAX_PATH));

	cl_assert(MultiByteToWideChar(CP_UTF8,
				MB_ERR_INVALID_CHARS,
				_dest,
				-1,
				wdest,
				MAX_PATH));

	/* Check the source for existence */
	source_attrs = GetFileAttributesW(wsource);
	cl_assert(INVALID_FILE_ATTRIBUTES != source_attrs);

	/* Check the target for existence */
	dest_attrs = GetFileAttributesW(wdest);

	if (INVALID_FILE_ATTRIBUTES != dest_attrs) {
		/* Target exists; append last path part of source to target.
		 * Use FindFirstFile to parse the path */
		find_handle = FindFirstFileW(wsource, &find_data);
		cl_assert(INVALID_HANDLE_VALUE != find_handle);
		wcscat_s(wdest, MAX_PATH, L"\\");
		wcscat_s(wdest, MAX_PATH, find_data.cFileName);
		FindClose(find_handle);

		/* Check the new target for existence */
		cl_assert(INVALID_FILE_ATTRIBUTES == GetFileAttributesW(wdest));
	}

	if (FILE_ATTRIBUTE_DIRECTORY & source_attrs)
		fs_copydir_helper(wsource, wdest);
	else
		cl_assert(CopyFileW(wsource, wdest, TRUE));
}

void
cl_fs_cleanup(void)
{
	fs_rm(fixture_path(_clar_path, "*"));
}

#else

#include <errno.h>
#include <string.h>

static int
shell_out(char * const argv[])
{
	int status, piderr;
	pid_t pid;

	pid = fork();

	if (pid < 0) {
		fprintf(stderr,
			"System error: `fork()` call failed (%d) - %s\n",
			errno, strerror(errno));
		exit(-1);
	}

	if (pid == 0) {
		execv(argv[0], argv);
	}

	do {
		piderr = waitpid(pid, &status, WUNTRACED);
	} while (piderr < 0 && (errno == EAGAIN || errno == EINTR));

	return WEXITSTATUS(status);
}

static void
fs_copy(const char *_source, const char *dest)
{
	char *argv[5];
	char *source;
	size_t source_len;

	source = strdup(_source);
	source_len = strlen(source);

	if (source[source_len - 1] == '/')
		source[source_len - 1] = 0;

	argv[0] = "/bin/cp";
	argv[1] = "-R";
	argv[2] = source;
	argv[3] = (char *)dest;
	argv[4] = NULL;

	cl_must_pass_(
		shell_out(argv),
		"Failed to copy test fixtures to sandbox"
	);

	free(source);
}

static void
fs_rm(const char *source)
{
	char *argv[4];

	argv[0] = "/bin/rm";
	argv[1] = "-Rf";
	argv[2] = (char *)source;
	argv[3] = NULL;

	cl_must_pass_(
		shell_out(argv),
		"Failed to cleanup the sandbox"
	);
}

void
cl_fs_cleanup(void)
{
	clar_unsandbox();
	clar_sandbox();
}
#endif
