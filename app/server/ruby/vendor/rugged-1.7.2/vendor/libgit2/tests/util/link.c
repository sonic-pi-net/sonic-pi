#include "clar_libgit2.h"
#include "posix.h"

#ifdef GIT_WIN32
# include "win32/reparse.h"
#endif

void test_link__cleanup(void)
{
#ifdef GIT_WIN32
	RemoveDirectory("lstat_junction");
	RemoveDirectory("lstat_dangling");
	RemoveDirectory("lstat_dangling_dir");
	RemoveDirectory("lstat_dangling_junction");

	RemoveDirectory("stat_junction");
	RemoveDirectory("stat_dangling");
	RemoveDirectory("stat_dangling_dir");
	RemoveDirectory("stat_dangling_junction");
#endif
}

#ifdef GIT_WIN32
static bool should_run(void)
{
	static SID_IDENTIFIER_AUTHORITY authority = { SECURITY_NT_AUTHORITY };
	PSID admin_sid;
	BOOL is_admin;

	cl_win32_pass(AllocateAndInitializeSid(&authority, 2, SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS, 0, 0, 0, 0, 0, 0, &admin_sid));
	cl_win32_pass(CheckTokenMembership(NULL, admin_sid, &is_admin));
	FreeSid(admin_sid);

	return is_admin ? true : false;
}
#else
static bool should_run(void)
{
	return true;
}
#endif

static void do_symlink(const char *old, const char *new, int is_dir)
{
#ifndef GIT_WIN32
	GIT_UNUSED(is_dir);

	cl_must_pass(symlink(old, new));
#else
	typedef DWORD (WINAPI *create_symlink_func)(LPCTSTR, LPCTSTR, DWORD);
	HMODULE module;
	create_symlink_func pCreateSymbolicLink;

	cl_assert(module = GetModuleHandle("kernel32"));
	cl_assert(pCreateSymbolicLink = (create_symlink_func)(void *)GetProcAddress(module, "CreateSymbolicLinkA"));

	cl_win32_pass(pCreateSymbolicLink(new, old, is_dir));
#endif
}

static void do_hardlink(const char *old, const char *new)
{
#ifndef GIT_WIN32
	cl_must_pass(link(old, new));
#else
	typedef DWORD (WINAPI *create_hardlink_func)(LPCTSTR, LPCTSTR, LPSECURITY_ATTRIBUTES);
	HMODULE module;
	create_hardlink_func pCreateHardLink;

	cl_assert(module = GetModuleHandle("kernel32"));
	cl_assert(pCreateHardLink = (create_hardlink_func)(void *)GetProcAddress(module, "CreateHardLinkA"));

	cl_win32_pass(pCreateHardLink(new, old, 0));
#endif
}

#ifdef GIT_WIN32

static void do_junction(const char *old, const char *new)
{
	GIT_REPARSE_DATA_BUFFER *reparse_buf;
	HANDLE handle;
	git_str unparsed_buf = GIT_STR_INIT;
	wchar_t *subst_utf16, *print_utf16;
	DWORD ioctl_ret;
	int subst_utf16_len, subst_byte_len, print_utf16_len, print_byte_len, ret;
	USHORT reparse_buflen;
	size_t i;

	/* Junction targets must be the unparsed name, starting with \??\, using
	 * backslashes instead of forward, and end in a trailing backslash.
	 * eg: \??\C:\Foo\
	 */
	git_str_puts(&unparsed_buf, "\\??\\");

	for (i = 0; i < strlen(old); i++)
		git_str_putc(&unparsed_buf, old[i] == '/' ? '\\' : old[i]);

	git_str_putc(&unparsed_buf, '\\');

	subst_utf16_len = git_utf8_to_16(NULL, 0, git_str_cstr(&unparsed_buf));
	subst_byte_len = subst_utf16_len * sizeof(WCHAR);

	print_utf16_len = subst_utf16_len - 4;
	print_byte_len = subst_byte_len - (4 * sizeof(WCHAR));

	/* The junction must be an empty directory before the junction attribute
	 * can be added.
	 */
	cl_win32_pass(CreateDirectoryA(new, NULL));

	handle = CreateFileA(new, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
		FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, NULL);
	cl_win32_pass(handle != INVALID_HANDLE_VALUE);

	reparse_buflen = (USHORT)(REPARSE_DATA_HEADER_SIZE +
		REPARSE_DATA_MOUNTPOINT_HEADER_SIZE +
		subst_byte_len + sizeof(WCHAR) +
		print_byte_len + sizeof(WCHAR));

	reparse_buf = LocalAlloc(LMEM_FIXED|LMEM_ZEROINIT, reparse_buflen);
	cl_assert(reparse_buf);

	subst_utf16 = reparse_buf->ReparseBuffer.MountPoint.PathBuffer;
	print_utf16 = subst_utf16 + subst_utf16_len + 1;

	ret = git_utf8_to_16(subst_utf16, subst_utf16_len + 1,
		git_str_cstr(&unparsed_buf));
	cl_assert_equal_i(subst_utf16_len, ret);

	ret = git_utf8_to_16(print_utf16,
		print_utf16_len + 1, git_str_cstr(&unparsed_buf) + 4);
	cl_assert_equal_i(print_utf16_len, ret);

	reparse_buf->ReparseTag = IO_REPARSE_TAG_MOUNT_POINT;
	reparse_buf->ReparseBuffer.MountPoint.SubstituteNameOffset = 0;
	reparse_buf->ReparseBuffer.MountPoint.SubstituteNameLength = subst_byte_len;
	reparse_buf->ReparseBuffer.MountPoint.PrintNameOffset = (USHORT)(subst_byte_len + sizeof(WCHAR));
	reparse_buf->ReparseBuffer.MountPoint.PrintNameLength = print_byte_len;
	reparse_buf->ReparseDataLength = reparse_buflen - REPARSE_DATA_HEADER_SIZE;

	cl_win32_pass(DeviceIoControl(handle, FSCTL_SET_REPARSE_POINT,
		reparse_buf, reparse_buflen, NULL, 0, &ioctl_ret, NULL));

	CloseHandle(handle);
	LocalFree(reparse_buf);

	git_str_dispose(&unparsed_buf);
}

static void do_custom_reparse(const char *path)
{
	REPARSE_GUID_DATA_BUFFER *reparse_buf;
	HANDLE handle;
	DWORD ioctl_ret;

	const char *reparse_data = "Reparse points are silly.";
	size_t reparse_buflen = REPARSE_GUID_DATA_BUFFER_HEADER_SIZE +
		strlen(reparse_data) + 1;

	reparse_buf = LocalAlloc(LMEM_FIXED|LMEM_ZEROINIT, reparse_buflen);
	cl_assert(reparse_buf);

	reparse_buf->ReparseTag = 42;
	reparse_buf->ReparseDataLength = (WORD)(strlen(reparse_data) + 1);

	reparse_buf->ReparseGuid.Data1 = 0xdeadbeef;
	reparse_buf->ReparseGuid.Data2 = 0xdead;
	reparse_buf->ReparseGuid.Data3 = 0xbeef;
	reparse_buf->ReparseGuid.Data4[0] = 42;
	reparse_buf->ReparseGuid.Data4[1] = 42;
	reparse_buf->ReparseGuid.Data4[2] = 42;
	reparse_buf->ReparseGuid.Data4[3] = 42;
	reparse_buf->ReparseGuid.Data4[4] = 42;
	reparse_buf->ReparseGuid.Data4[5] = 42;
	reparse_buf->ReparseGuid.Data4[6] = 42;
	reparse_buf->ReparseGuid.Data4[7] = 42;
	reparse_buf->ReparseGuid.Data4[8] = 42;

	memcpy(reparse_buf->GenericReparseBuffer.DataBuffer,
		reparse_data, strlen(reparse_data) + 1);

	handle = CreateFileA(path, GENERIC_WRITE, 0, NULL, OPEN_EXISTING,
		FILE_FLAG_OPEN_REPARSE_POINT | FILE_FLAG_BACKUP_SEMANTICS, NULL);
	cl_win32_pass(handle != INVALID_HANDLE_VALUE);

	cl_win32_pass(DeviceIoControl(handle, FSCTL_SET_REPARSE_POINT,
		reparse_buf,
		reparse_buf->ReparseDataLength + REPARSE_GUID_DATA_BUFFER_HEADER_SIZE,
		NULL, 0, &ioctl_ret, NULL));

	CloseHandle(handle);
	LocalFree(reparse_buf);
}

#endif

void test_link__stat_regular_file(void)
{
	struct stat st;

	cl_git_rewritefile("stat_regfile", "This is a regular file!\n");

	cl_must_pass(p_stat("stat_regfile", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(24, st.st_size);
}

void test_link__lstat_regular_file(void)
{
	struct stat st;

	cl_git_rewritefile("lstat_regfile", "This is a regular file!\n");

	cl_must_pass(p_stat("lstat_regfile", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(24, st.st_size);
}

void test_link__stat_symlink(void)
{
	struct stat st;

	if (!should_run())
		clar__skip();

	cl_git_rewritefile("stat_target", "This is the target of a symbolic link.\n");
	do_symlink("stat_target", "stat_symlink", 0);

	cl_must_pass(p_stat("stat_target", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(39, st.st_size);

	cl_must_pass(p_stat("stat_symlink", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(39, st.st_size);
}

void test_link__stat_symlink_directory(void)
{
	struct stat st;

	if (!should_run())
		clar__skip();

	p_mkdir("stat_dirtarget", 0777);
	do_symlink("stat_dirtarget", "stat_dirlink", 1);

	cl_must_pass(p_stat("stat_dirtarget", &st));
	cl_assert(S_ISDIR(st.st_mode));

	cl_must_pass(p_stat("stat_dirlink", &st));
	cl_assert(S_ISDIR(st.st_mode));
}

void test_link__stat_symlink_chain(void)
{
	struct stat st;

	if (!should_run())
		clar__skip();

	cl_git_rewritefile("stat_final_target", "Final target of some symbolic links...\n");
	do_symlink("stat_final_target", "stat_chain_3", 0);
	do_symlink("stat_chain_3", "stat_chain_2", 0);
	do_symlink("stat_chain_2", "stat_chain_1", 0);

	cl_must_pass(p_stat("stat_chain_1", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(39, st.st_size);
}

void test_link__stat_dangling_symlink(void)
{
	struct stat st;

	if (!should_run())
		clar__skip();

	do_symlink("stat_nonexistent", "stat_dangling", 0);

	cl_must_fail(p_stat("stat_nonexistent", &st));
	cl_must_fail(p_stat("stat_dangling", &st));
}

void test_link__stat_dangling_symlink_directory(void)
{
	struct stat st;

	if (!should_run())
		clar__skip();

	do_symlink("stat_nonexistent", "stat_dangling_dir", 1);

	cl_must_fail(p_stat("stat_nonexistent_dir", &st));
	cl_must_fail(p_stat("stat_dangling", &st));
}

void test_link__lstat_symlink(void)
{
	git_str target_path = GIT_STR_INIT;
	struct stat st;

	if (!should_run())
		clar__skip();

	/* Windows always writes the canonical path as the link target, so
	 * write the full path on all platforms.
	 */
	git_str_join(&target_path, '/', clar_sandbox_path(), "lstat_target");

	cl_git_rewritefile("lstat_target", "This is the target of a symbolic link.\n");
	do_symlink(git_str_cstr(&target_path), "lstat_symlink", 0);

	cl_must_pass(p_lstat("lstat_target", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(39, st.st_size);

	cl_must_pass(p_lstat("lstat_symlink", &st));
	cl_assert(S_ISLNK(st.st_mode));
	cl_assert_equal_i(git_str_len(&target_path), st.st_size);

	git_str_dispose(&target_path);
}

void test_link__lstat_symlink_directory(void)
{
	git_str target_path = GIT_STR_INIT;
	struct stat st;

	if (!should_run())
		clar__skip();

	git_str_join(&target_path, '/', clar_sandbox_path(), "lstat_dirtarget");

	p_mkdir("lstat_dirtarget", 0777);
	do_symlink(git_str_cstr(&target_path), "lstat_dirlink", 1);

	cl_must_pass(p_lstat("lstat_dirtarget", &st));
	cl_assert(S_ISDIR(st.st_mode));

	cl_must_pass(p_lstat("lstat_dirlink", &st));
	cl_assert(S_ISLNK(st.st_mode));
	cl_assert_equal_i(git_str_len(&target_path), st.st_size);

	git_str_dispose(&target_path);
}

void test_link__lstat_dangling_symlink(void)
{
	struct stat st;

	if (!should_run())
		clar__skip();

	do_symlink("lstat_nonexistent", "lstat_dangling", 0);

	cl_must_fail(p_lstat("lstat_nonexistent", &st));

	cl_must_pass(p_lstat("lstat_dangling", &st));
	cl_assert(S_ISLNK(st.st_mode));
	cl_assert_equal_i(strlen("lstat_nonexistent"), st.st_size);
}

void test_link__lstat_dangling_symlink_directory(void)
{
	struct stat st;

	if (!should_run())
		clar__skip();

	do_symlink("lstat_nonexistent", "lstat_dangling_dir", 1);

	cl_must_fail(p_lstat("lstat_nonexistent", &st));

	cl_must_pass(p_lstat("lstat_dangling_dir", &st));
	cl_assert(S_ISLNK(st.st_mode));
	cl_assert_equal_i(strlen("lstat_nonexistent"), st.st_size);
}

void test_link__stat_junction(void)
{
#ifdef GIT_WIN32
	git_str target_path = GIT_STR_INIT;
	struct stat st;

	git_str_join(&target_path, '/', clar_sandbox_path(), "stat_junctarget");

	p_mkdir("stat_junctarget", 0777);
	do_junction(git_str_cstr(&target_path), "stat_junction");

	cl_must_pass(p_stat("stat_junctarget", &st));
	cl_assert(S_ISDIR(st.st_mode));

	cl_must_pass(p_stat("stat_junction", &st));
	cl_assert(S_ISDIR(st.st_mode));

	git_str_dispose(&target_path);
#endif
}

void test_link__stat_dangling_junction(void)
{
#ifdef GIT_WIN32
	git_str target_path = GIT_STR_INIT;
	struct stat st;

	git_str_join(&target_path, '/', clar_sandbox_path(), "stat_nonexistent_junctarget");

	p_mkdir("stat_nonexistent_junctarget", 0777);
	do_junction(git_str_cstr(&target_path), "stat_dangling_junction");

	RemoveDirectory("stat_nonexistent_junctarget");

	cl_must_fail(p_stat("stat_nonexistent_junctarget", &st));
	cl_must_fail(p_stat("stat_dangling_junction", &st));

	git_str_dispose(&target_path);
#endif
}

void test_link__lstat_junction(void)
{
#ifdef GIT_WIN32
	git_str target_path = GIT_STR_INIT;
	struct stat st;

	git_str_join(&target_path, '/', clar_sandbox_path(), "lstat_junctarget");

	p_mkdir("lstat_junctarget", 0777);
	do_junction(git_str_cstr(&target_path), "lstat_junction");

	cl_must_pass(p_lstat("lstat_junctarget", &st));
	cl_assert(S_ISDIR(st.st_mode));

	cl_must_pass(p_lstat("lstat_junction", &st));
	cl_assert(S_ISLNK(st.st_mode));

	git_str_dispose(&target_path);
#endif
}

void test_link__lstat_dangling_junction(void)
{
#ifdef GIT_WIN32
	git_str target_path = GIT_STR_INIT;
	struct stat st;

	git_str_join(&target_path, '/', clar_sandbox_path(), "lstat_nonexistent_junctarget");

	p_mkdir("lstat_nonexistent_junctarget", 0777);
	do_junction(git_str_cstr(&target_path), "lstat_dangling_junction");

	RemoveDirectory("lstat_nonexistent_junctarget");

	cl_must_fail(p_lstat("lstat_nonexistent_junctarget", &st));

	cl_must_pass(p_lstat("lstat_dangling_junction", &st));
	cl_assert(S_ISLNK(st.st_mode));
	cl_assert_equal_i(git_str_len(&target_path), st.st_size);

	git_str_dispose(&target_path);
#endif
}

void test_link__stat_hardlink(void)
{
	struct stat st;

	if (!should_run())
		clar__skip();

	cl_git_rewritefile("stat_hardlink1", "This file has many names!\n");
	do_hardlink("stat_hardlink1", "stat_hardlink2");

	cl_must_pass(p_stat("stat_hardlink1", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(26, st.st_size);

	cl_must_pass(p_stat("stat_hardlink2", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(26, st.st_size);
}

void test_link__lstat_hardlink(void)
{
	struct stat st;

	if (!should_run())
		clar__skip();

	cl_git_rewritefile("lstat_hardlink1", "This file has many names!\n");
	do_hardlink("lstat_hardlink1", "lstat_hardlink2");

	cl_must_pass(p_lstat("lstat_hardlink1", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(26, st.st_size);

	cl_must_pass(p_lstat("lstat_hardlink2", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(26, st.st_size);
}

void test_link__stat_reparse_point(void)
{
#ifdef GIT_WIN32
	struct stat st;

	/* Generic reparse points should be treated as regular files, only
	 * symlinks and junctions should be treated as links.
	 */

	cl_git_rewritefile("stat_reparse", "This is a reparse point!\n");
	do_custom_reparse("stat_reparse");

	cl_must_pass(p_lstat("stat_reparse", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(25, st.st_size);
#endif
}

void test_link__lstat_reparse_point(void)
{
#ifdef GIT_WIN32
	struct stat st;

	cl_git_rewritefile("lstat_reparse", "This is a reparse point!\n");
	do_custom_reparse("lstat_reparse");

	cl_must_pass(p_lstat("lstat_reparse", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert_equal_i(25, st.st_size);
#endif
}

void test_link__readlink_nonexistent_file(void)
{
	char buf[2048];

	cl_must_fail(p_readlink("readlink_nonexistent", buf, 2048));
	cl_assert_equal_i(ENOENT, errno);
}

void test_link__readlink_normal_file(void)
{
	char buf[2048];

	cl_git_rewritefile("readlink_regfile", "This is a regular file!\n");
	cl_must_fail(p_readlink("readlink_regfile", buf, 2048));
	cl_assert_equal_i(EINVAL, errno);
}

void test_link__readlink_symlink(void)
{
	git_str target_path = GIT_STR_INIT;
	int len;
	char buf[2048];

	if (!should_run())
		clar__skip();

	git_str_join(&target_path, '/', clar_sandbox_path(), "readlink_target");

	cl_git_rewritefile("readlink_target", "This is the target of a symlink\n");
	do_symlink(git_str_cstr(&target_path), "readlink_link", 0);

	len = p_readlink("readlink_link", buf, 2048);
	cl_must_pass(len);

	buf[len] = 0;

	cl_assert_equal_s(git_str_cstr(&target_path), buf);

	git_str_dispose(&target_path);
}

void test_link__readlink_dangling(void)
{
	git_str target_path = GIT_STR_INIT;
	int len;
	char buf[2048];

	if (!should_run())
		clar__skip();

	git_str_join(&target_path, '/', clar_sandbox_path(), "readlink_nonexistent");

	do_symlink(git_str_cstr(&target_path), "readlink_dangling", 0);

	len = p_readlink("readlink_dangling", buf, 2048);
	cl_must_pass(len);

	buf[len] = 0;

	cl_assert_equal_s(git_str_cstr(&target_path), buf);

	git_str_dispose(&target_path);
}

void test_link__readlink_multiple(void)
{
	git_str target_path = GIT_STR_INIT,
		path3 = GIT_STR_INIT, path2 = GIT_STR_INIT, path1 = GIT_STR_INIT;
	int len;
	char buf[2048];

	if (!should_run())
		clar__skip();

	git_str_join(&target_path, '/', clar_sandbox_path(), "readlink_final");
	git_str_join(&path3, '/', clar_sandbox_path(), "readlink_3");
	git_str_join(&path2, '/', clar_sandbox_path(), "readlink_2");
	git_str_join(&path1, '/', clar_sandbox_path(), "readlink_1");

	do_symlink(git_str_cstr(&target_path), git_str_cstr(&path3), 0);
	do_symlink(git_str_cstr(&path3), git_str_cstr(&path2), 0);
	do_symlink(git_str_cstr(&path2), git_str_cstr(&path1), 0);

	len = p_readlink("readlink_1", buf, 2048);
	cl_must_pass(len);

	buf[len] = 0;

	cl_assert_equal_s(git_str_cstr(&path2), buf);

	git_str_dispose(&path1);
	git_str_dispose(&path2);
	git_str_dispose(&path3);
	git_str_dispose(&target_path);
}
