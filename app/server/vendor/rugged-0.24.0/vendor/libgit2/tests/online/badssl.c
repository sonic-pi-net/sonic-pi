#include "clar_libgit2.h"

#include "git2/clone.h"

static git_repository *g_repo;

#if defined(GIT_OPENSSL) || defined(GIT_WINHTTP) || defined(GIT_SECURE_TRANSPORT)
static bool g_has_ssl = true;
#else
static bool g_has_ssl = false;
#endif

void test_online_badssl__expired(void)
{
	if (!g_has_ssl)
		cl_skip();

	cl_git_fail_with(GIT_ECERTIFICATE,
			 git_clone(&g_repo, "https://expired.badssl.com/fake.git", "./fake", NULL));
}

void test_online_badssl__wrong_host(void)
{
	if (!g_has_ssl)
		cl_skip();

	cl_git_fail_with(GIT_ECERTIFICATE,
			 git_clone(&g_repo, "https://wrong.host.badssl.com/fake.git", "./fake", NULL));
}

void test_online_badssl__self_signed(void)
{
	if (!g_has_ssl)
		cl_skip();

	cl_git_fail_with(GIT_ECERTIFICATE,
			 git_clone(&g_repo, "https://self-signed.badssl.com/fake.git", "./fake", NULL));
}
