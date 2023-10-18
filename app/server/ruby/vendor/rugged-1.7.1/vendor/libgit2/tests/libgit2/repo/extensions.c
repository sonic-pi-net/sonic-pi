#include "clar_libgit2.h"
#include "futils.h"
#include "sysdir.h"
#include <ctype.h>

static git_repository *repo;

void test_repo_extensions__initialize(void)
{
	git_config *config;

	repo = cl_git_sandbox_init("empty_bare.git");

	cl_git_pass(git_repository_config(&config, repo));
	cl_git_pass(git_config_set_int32(config, "core.repositoryformatversion", 1));
	git_config_free(config);
}

void test_repo_extensions__cleanup(void)
{
	cl_git_sandbox_cleanup();
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_EXTENSIONS, NULL, 0));
}

void test_repo_extensions__builtin(void)
{
	git_repository *extended;

	cl_repo_set_string(repo, "extensions.noop", "foobar");

	cl_git_pass(git_repository_open(&extended, "empty_bare.git"));
	cl_assert(git_repository_path(extended) != NULL);
	cl_assert(git__suffixcmp(git_repository_path(extended), "/") == 0);
	git_repository_free(extended);
}

void test_repo_extensions__negate_builtin(void)
{
	const char *in[] = { "foo", "!noop", "baz" };
	git_repository *extended;

	cl_repo_set_string(repo, "extensions.noop", "foobar");

	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_EXTENSIONS, in, ARRAY_SIZE(in)));

	cl_git_fail(git_repository_open(&extended, "empty_bare.git"));
	git_repository_free(extended);
}

void test_repo_extensions__unsupported(void)
{
	git_repository *extended = NULL;

	cl_repo_set_string(repo, "extensions.unknown", "foobar");

	cl_git_fail(git_repository_open(&extended, "empty_bare.git"));
	git_repository_free(extended);
}

void test_repo_extensions__adds_extension(void)
{
	const char *in[] = { "foo", "!noop", "newextension", "baz" };
	git_repository *extended;

	cl_repo_set_string(repo, "extensions.newextension", "foobar");
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_EXTENSIONS, in, ARRAY_SIZE(in)));

	cl_git_pass(git_repository_open(&extended, "empty_bare.git"));
	cl_assert(git_repository_path(extended) != NULL);
	cl_assert(git__suffixcmp(git_repository_path(extended), "/") == 0);
	git_repository_free(extended);
}
