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

#include <locale.h>

#include "clar_libgit2.h"
#include "posix.h"
#include "userdiff.h"

void test_core_posix__initialize(void)
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

void test_core_posix__inet_pton(void)
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

void test_core_posix__utimes(void)
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

	p_stat("foo", &st);
	cl_assert_equal_i(1234567890, st.st_atime);
	cl_assert_equal_i(1234567890, st.st_mtime);


	/* test p_futimes */
	times[0].tv_sec = 1414141414;
	times[0].tv_usec = 0;
	times[1].tv_sec = 1414141414;
	times[1].tv_usec = 0;

	cl_must_pass(fd = p_open("foo", O_RDWR));
	cl_must_pass(p_futimes(fd, times));
	p_close(fd);

	p_stat("foo", &st);
	cl_assert_equal_i(1414141414, st.st_atime);
	cl_assert_equal_i(1414141414, st.st_mtime);


	/* test p_utimes with current time, assume that
	 * it takes < 5 seconds to get the time...!
	 */
	cl_must_pass(p_utimes("foo", NULL));

	curtime = time(NULL);
	p_stat("foo", &st);
	cl_assert((st.st_atime - curtime) < 5);
	cl_assert((st.st_mtime - curtime) < 5);

	p_unlink("foo");
}

void test_core_posix__p_regcomp_ignores_global_locale_ctype(void)
{
	regex_t preg;
	int error = 0;

	const char* oldlocale = setlocale(LC_CTYPE, NULL);

	if (!setlocale(LC_CTYPE, "UTF-8") &&
	    !setlocale(LC_CTYPE, "c.utf8") &&
			!setlocale(LC_CTYPE, "en_US.UTF-8"))
		cl_skip();

	if (MB_CUR_MAX == 1) {
		setlocale(LC_CTYPE, oldlocale);
		cl_fail("Expected locale to be switched to multibyte");
	}

	p_regcomp(&preg, "[\xc0-\xff][\x80-\xbf]", REG_EXTENDED);
	regfree(&preg);

	setlocale(LC_CTYPE, oldlocale);

	cl_must_pass(error);
}

void test_core_posix__p_regcomp_compile_userdiff_regexps(void)
{
	size_t idx;

	for (idx = 0; idx < ARRAY_SIZE(builtin_defs); ++idx) {
		git_diff_driver_definition ddef = builtin_defs[idx];
		int error = 0;
		regex_t preg;

		error = p_regcomp(&preg, ddef.fns, REG_EXTENDED | ddef.flags);
		regfree(&preg);
		cl_must_pass(error);

		error = p_regcomp(&preg, ddef.words, REG_EXTENDED);
		regfree(&preg);
		cl_must_pass(error);
	}
}
