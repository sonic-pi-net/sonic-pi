#include "clar_libgit2.h"

#include "git2/clone.h"

static git_repository *g_repo;

#if defined(GIT_OPENSSL) || defined(GIT_WINHTTP) || defined(GIT_SECURE_TRANSPORT)
static bool g_has_ssl = true;
#else
static bool g_has_ssl = false;
#endif

static int cert_check_assert_invalid(git_cert *cert, int valid, const char* host, void *payload)
{
	GIT_UNUSED(cert); GIT_UNUSED(host); GIT_UNUSED(payload);

	cl_assert_equal_i(0, valid);

	return GIT_ECERTIFICATE;
}

void test_online_badssl__expired(void)
{
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	opts.fetch_opts.callbacks.certificate_check = cert_check_assert_invalid;

	if (!g_has_ssl)
		cl_skip();

	cl_git_fail_with(GIT_ECERTIFICATE,
			 git_clone(&g_repo, "https://expired.badssl.com/fake.git", "./fake", NULL));

	cl_git_fail_with(GIT_ECERTIFICATE,
			 git_clone(&g_repo, "https://expired.badssl.com/fake.git", "./fake", &opts));
}

void test_online_badssl__wrong_host(void)
{
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	opts.fetch_opts.callbacks.certificate_check = cert_check_assert_invalid;

	if (!g_has_ssl)
		cl_skip();

	cl_git_fail_with(GIT_ECERTIFICATE,
			 git_clone(&g_repo, "https://wrong.host.badssl.com/fake.git", "./fake", NULL));
	cl_git_fail_with(GIT_ECERTIFICATE,
			 git_clone(&g_repo, "https://wrong.host.badssl.com/fake.git", "./fake", &opts));
}

void test_online_badssl__self_signed(void)
{
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	opts.fetch_opts.callbacks.certificate_check = cert_check_assert_invalid;

	if (!g_has_ssl)
		cl_skip();

	cl_git_fail_with(GIT_ECERTIFICATE,
			 git_clone(&g_repo, "https://self-signed.badssl.com/fake.git", "./fake", NULL));
	cl_git_fail_with(GIT_ECERTIFICATE,
			 git_clone(&g_repo, "https://self-signed.badssl.com/fake.git", "./fake", &opts));
}

void test_online_badssl__old_cipher(void)
{
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	opts.fetch_opts.callbacks.certificate_check = cert_check_assert_invalid;

	/* FIXME: we don't actually reject RC4 anywhere, figure out what to tweak */
	cl_skip();

	if (!g_has_ssl)
		cl_skip();

	cl_git_fail_with(GIT_ECERTIFICATE,
			 git_clone(&g_repo, "https://rc4.badssl.com/fake.git", "./fake", NULL));
	cl_git_fail_with(GIT_ECERTIFICATE,
			 git_clone(&g_repo, "https://rc4.badssl.com/fake.git", "./fake", &opts));
}
