#include "clar_libgit2.h"
#include "futils.h"
#include "sysdir.h"
#include "repository.h"
#include <ctype.h>

static git_repository *repo;
static git_config *config;

void test_repo_objectformat__initialize(void)
{
	repo = cl_git_sandbox_init("empty_bare.git");

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_int32(config, "core.repositoryformatversion", 1));
}

void test_repo_objectformat__cleanup(void)
{
	git_config_free(config);
	cl_git_sandbox_cleanup();
}

void test_repo_objectformat__unspecified(void)
{
	git_repository *other;

	cl_git_pass(git_repository_open(&other, "empty_bare.git"));
	cl_assert_equal_i(GIT_OID_SHA1, git_repository_oid_type(other));
	git_repository_free(other);
}

void test_repo_objectformat__sha1(void)
{
	git_repository *other;

	cl_git_pass(git_config_set_string(config, "extensions.objectformat", "sha1"));

	cl_git_pass(git_repository_open(&other, "empty_bare.git"));
	cl_assert_equal_i(GIT_OID_SHA1, git_repository_oid_type(other));
	git_repository_free(other);
}

void test_repo_objectformat__sha256(void)
{
#ifndef GIT_EXPERIMENTAL_SHA256
	cl_skip();
#else
	git_repository *other;

	cl_git_pass(git_config_set_string(config, "extensions.objectformat", "sha256"));

	cl_git_pass(git_repository_open(&other, "empty_bare.git"));
	cl_assert_equal_i(GIT_OID_SHA256, git_repository_oid_type(other));
	git_repository_free(other);
#endif
}

void test_repo_objectformat__invalid(void)
{
	git_repository *other;

	cl_git_pass(git_config_set_string(config, "extensions.objectformat", "bogus"));

	cl_git_fail_with(GIT_EINVALID, git_repository_open(&other, "empty_bare.git"));
	cl_assert_equal_s("unknown object format 'bogus'", git_error_last()->message);
	git_repository_free(other);
}

