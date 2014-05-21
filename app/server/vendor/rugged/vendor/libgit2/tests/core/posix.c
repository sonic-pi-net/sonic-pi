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
#include "posix.h"

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
	cl_git_fail(p_inet_pton(12, "52.472", NULL)); /* AF_DECnet */
	cl_assert_equal_i(EAFNOSUPPORT, errno);

	cl_git_fail(p_inet_pton(5, "315.124", NULL)); /* AF_CHAOS */
	cl_assert_equal_i(EAFNOSUPPORT, errno);
}
