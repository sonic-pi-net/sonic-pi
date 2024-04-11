#ifndef _WIN32
# include <arpa/inet.h>
# include <sys/socket.h>
# include <netinet/in.h>
#else
# include <ws2tcpip.h>
# ifdef _MSC_VER
#  pragma comment(lib, "ws2_32")
# endif
#endif

#include "clar_libgit2.h"
#include "futils.h"
#include "posix.h"

void test_posix__initialize(void)
{
#ifdef GIT_WIN32
	/* on win32, the WSA context needs to be initialized
	 * before any socket calls can be performed */
	WSADATA wsd;

	cl_git_pass(WSAStartup(MAKEWORD(2,2), &wsd));
	cl_assert(LOBYTE(wsd.wVersion) == 2 && HIBYTE(wsd.wVersion) == 2);
#endif
}

static bool supports_ipv6(void)
{
#ifdef GIT_WIN32
	/* IPv6 is supported on Vista and newer */
	return git_has_win32_version(6, 0, 0);
#else
	return 1;
#endif
}

void test_posix__inet_pton(void)
{
	struct in_addr addr;
	struct in6_addr addr6;
	size_t i;

	struct in_addr_data {
		const char *p;
		const uint8_t n[4];
	};

	struct in6_addr_data {
		const char *p;
		const uint8_t n[16];
	};

	static struct in_addr_data in_addr_data[] = {
		{ "0.0.0.0", { 0, 0, 0, 0 } },
		{ "10.42.101.8", { 10, 42, 101, 8 } },
		{ "127.0.0.1", { 127, 0, 0, 1 } },
		{ "140.177.10.12", { 140, 177, 10, 12 } },
		{ "204.232.175.90", { 204, 232, 175, 90 } },
		{ "255.255.255.255", { 255, 255, 255, 255 } },
	};

	static struct in6_addr_data in6_addr_data[] = {
		{ "::", { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 } },
		{ "::1", { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01 } },
		{ "0:0:0:0:0:0:0:1", { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01 } },
		{ "2001:db8:8714:3a90::12", { 0x20, 0x01, 0x0d, 0xb8, 0x87, 0x14, 0x3a, 0x90, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x12 } },
		{ "fe80::f8ba:c2d6:86be:3645", { 0xfe, 0x80, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0xba, 0xc2, 0xd6, 0x86, 0xbe, 0x36, 0x45 } },
		{ "::ffff:204.152.189.116", { 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xff, 0xff, 0xcc, 0x98, 0xbd, 0x74 } },
	};

	/* Test some ipv4 addresses */
	for (i = 0; i < 6; i++) {
		cl_assert(p_inet_pton(AF_INET, in_addr_data[i].p, &addr) == 1);
		cl_assert(memcmp(&addr, in_addr_data[i].n, sizeof(struct in_addr)) == 0);
	}

	/* Test some ipv6 addresses */
	if (supports_ipv6())
	{
		for (i = 0; i < 6; i++) {
			cl_assert(p_inet_pton(AF_INET6, in6_addr_data[i].p, &addr6) == 1);
			cl_assert(memcmp(&addr6, in6_addr_data[i].n, sizeof(struct in6_addr)) == 0);
		}
	}

	/* Test some invalid strings */
	cl_assert(p_inet_pton(AF_INET, "", &addr) == 0);
	cl_assert(p_inet_pton(AF_INET, "foo", &addr) == 0);
	cl_assert(p_inet_pton(AF_INET, " 127.0.0.1", &addr) == 0);
	cl_assert(p_inet_pton(AF_INET, "bar", &addr) == 0);
	cl_assert(p_inet_pton(AF_INET, "10.foo.bar.1", &addr) == 0);

	/* Test unsupported address families */
	cl_git_fail(p_inet_pton(INT_MAX-1, "52.472", &addr));
	cl_assert_equal_i(EAFNOSUPPORT, errno);
}

void test_posix__utimes(void)
{
	struct p_timeval times[2];
	struct stat st;
	time_t curtime;
	int fd;

	/* test p_utimes */
	times[0].tv_sec = 1234567890;
	times[0].tv_usec = 0;
	times[1].tv_sec = 1234567890;
	times[1].tv_usec = 0;

	cl_git_mkfile("foo", "Dummy file.");
	cl_must_pass(p_utimes("foo", times));

	cl_must_pass(p_stat("foo", &st));
	cl_assert_equal_i(1234567890, st.st_atime);
	cl_assert_equal_i(1234567890, st.st_mtime);


	/* test p_futimes */
	times[0].tv_sec = 1414141414;
	times[0].tv_usec = 0;
	times[1].tv_sec = 1414141414;
	times[1].tv_usec = 0;

	cl_must_pass(fd = p_open("foo", O_RDWR));
	cl_must_pass(p_futimes(fd, times));
	cl_must_pass(p_close(fd));

	cl_must_pass(p_stat("foo", &st));
	cl_assert_equal_i(1414141414, st.st_atime);
	cl_assert_equal_i(1414141414, st.st_mtime);


	/* test p_utimes with current time, assume that
	 * it takes < 5 seconds to get the time...!
	 */
	cl_must_pass(p_utimes("foo", NULL));

	curtime = time(NULL);
	cl_must_pass(p_stat("foo", &st));
	cl_assert((st.st_atime - curtime) < 5);
	cl_assert((st.st_mtime - curtime) < 5);

	cl_must_pass(p_unlink("foo"));
}

void test_posix__unlink_removes_symlink(void)
{
	if (!git_fs_path_supports_symlinks(clar_sandbox_path()))
		clar__skip();

	cl_git_mkfile("file", "Dummy file.");
	cl_git_pass(git_futils_mkdir("dir", 0777, 0));

	cl_must_pass(p_symlink("file", "file-symlink"));
	cl_must_pass(p_symlink("dir", "dir-symlink"));

	cl_must_pass(p_unlink("file-symlink"));
	cl_must_pass(p_unlink("dir-symlink"));

	cl_assert(git_fs_path_exists("file"));
	cl_assert(git_fs_path_exists("dir"));

	cl_must_pass(p_unlink("file"));
	cl_must_pass(p_rmdir("dir"));
}

void test_posix__symlink_resolves_to_correct_type(void)
{
	git_str contents = GIT_STR_INIT;

	if (!git_fs_path_supports_symlinks(clar_sandbox_path()))
		clar__skip();

	cl_must_pass(git_futils_mkdir("dir", 0777, 0));
	cl_must_pass(git_futils_mkdir("file", 0777, 0));
	cl_git_mkfile("dir/file", "symlink target");

	cl_git_pass(p_symlink("file", "dir/link"));

	cl_git_pass(git_futils_readbuffer(&contents, "dir/file"));
	cl_assert_equal_s(contents.ptr, "symlink target");

	cl_must_pass(p_unlink("dir/link"));
	cl_must_pass(p_unlink("dir/file"));
	cl_must_pass(p_rmdir("dir"));
	cl_must_pass(p_rmdir("file"));

	git_str_dispose(&contents);
}

void test_posix__relative_symlink(void)
{
	git_str contents = GIT_STR_INIT;

	if (!git_fs_path_supports_symlinks(clar_sandbox_path()))
		clar__skip();

	cl_must_pass(git_futils_mkdir("dir", 0777, 0));
	cl_git_mkfile("file", "contents");
	cl_git_pass(p_symlink("../file", "dir/link"));
	cl_git_pass(git_futils_readbuffer(&contents, "dir/link"));
	cl_assert_equal_s(contents.ptr, "contents");

	cl_must_pass(p_unlink("file"));
	cl_must_pass(p_unlink("dir/link"));
	cl_must_pass(p_rmdir("dir"));

	git_str_dispose(&contents);
}

void test_posix__symlink_to_file_across_dirs(void)
{
	git_str contents = GIT_STR_INIT;

	if (!git_fs_path_supports_symlinks(clar_sandbox_path()))
		clar__skip();

	/*
	 * Create a relative symlink that points into another
	 * directory. This used to not work on Win32, where we
	 * forgot to convert directory separators to
	 * Windows-style ones.
	 */
	cl_must_pass(git_futils_mkdir("dir", 0777, 0));
	cl_git_mkfile("dir/target", "symlink target");
	cl_git_pass(p_symlink("dir/target", "link"));

	cl_git_pass(git_futils_readbuffer(&contents, "dir/target"));
	cl_assert_equal_s(contents.ptr, "symlink target");

	cl_must_pass(p_unlink("dir/target"));
	cl_must_pass(p_unlink("link"));
	cl_must_pass(p_rmdir("dir"));

	git_str_dispose(&contents);
}
