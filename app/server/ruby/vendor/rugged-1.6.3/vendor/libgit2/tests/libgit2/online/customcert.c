#include "clar_libgit2.h"

#include "path.h"
#include "git2/clone.h"
#include "git2/cred_helpers.h"
#include "remote.h"
#include "futils.h"
#include "refs.h"

/*
 * Certificate one is in the `certs` folder; certificate two is in the
 * `self-signed.pem` file.
 */
#define CUSTOM_CERT_ONE_URL "https://test.libgit2.org:1443/anonymous/test.git"
#define CUSTOM_CERT_ONE_PATH "certs"

#define CUSTOM_CERT_TWO_URL "https://test.libgit2.org:2443/anonymous/test.git"
#define CUSTOM_CERT_TWO_FILE "self-signed.pem"

#if (GIT_OPENSSL || GIT_MBEDTLS)
static git_repository *g_repo;
static int initialized = false;
#endif

void test_online_customcert__initialize(void)
{
#if (GIT_OPENSSL || GIT_MBEDTLS)
	g_repo = NULL;

	if (!initialized) {
		git_str path = GIT_STR_INIT, file = GIT_STR_INIT;
		char cwd[GIT_PATH_MAX];

		cl_fixture_sandbox(CUSTOM_CERT_ONE_PATH);
		cl_fixture_sandbox(CUSTOM_CERT_TWO_FILE);

		cl_must_pass(p_getcwd(cwd, GIT_PATH_MAX));
		cl_git_pass(git_str_joinpath(&path, cwd, CUSTOM_CERT_ONE_PATH));
		cl_git_pass(git_str_joinpath(&file, cwd, CUSTOM_CERT_TWO_FILE));

		cl_git_pass(git_libgit2_opts(GIT_OPT_SET_SSL_CERT_LOCATIONS,
		                             file.ptr, path.ptr));
		initialized = true;

		git_str_dispose(&file);
		git_str_dispose(&path);
	}
#endif
}

void test_online_customcert__cleanup(void)
{
#if (GIT_OPENSSL || GIT_MBEDTLS)
	if (g_repo) {
		git_repository_free(g_repo);
		g_repo = NULL;
	}

	cl_fixture_cleanup("./cloned");
	cl_fixture_cleanup(CUSTOM_CERT_ONE_PATH);
	cl_fixture_cleanup(CUSTOM_CERT_TWO_FILE);
#endif
}

void test_online_customcert__file(void)
{
#if (GIT_OPENSSL || GIT_MBEDTLS)
	cl_git_pass(git_clone(&g_repo, CUSTOM_CERT_ONE_URL, "./cloned", NULL));
	cl_assert(git_fs_path_exists("./cloned/master.txt"));
#endif
}

void test_online_customcert__path(void)
{
#if (GIT_OPENSSL || GIT_MBEDTLS)
	cl_git_pass(git_clone(&g_repo, CUSTOM_CERT_TWO_URL, "./cloned", NULL));
	cl_assert(git_fs_path_exists("./cloned/master.txt"));
#endif
}
