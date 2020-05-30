#include "clar_libgit2.h"
#include "buffer.h"
#include "futils.h"
#include "repository.h"

#ifdef GIT_WIN32
# define ROOT_PREFIX "C:"
#else
# define ROOT_PREFIX
#endif

static git_repository *_repo;

void test_config_conditionals__initialize(void)
{
	_repo = cl_git_sandbox_init("empty_standard_repo");
}

void test_config_conditionals__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void assert_condition_includes(const char *keyword, const char *path, bool expected)
{
	git_buf buf = GIT_BUF_INIT;
	git_config *cfg;

	cl_git_pass(git_buf_printf(&buf, "[includeIf \"%s:%s\"]\n", keyword, path));
	cl_git_pass(git_buf_puts(&buf, "path = other\n"));

	cl_git_mkfile("empty_standard_repo/.git/config", buf.ptr);
	cl_git_mkfile("empty_standard_repo/.git/other", "[foo]\nbar=baz\n");
	_repo = cl_git_sandbox_reopen();

	cl_git_pass(git_repository_config(&cfg, _repo));

	if (expected) {
		git_buf_clear(&buf);
		cl_git_pass(git_config_get_string_buf(&buf, cfg, "foo.bar"));
		cl_assert_equal_s("baz", git_buf_cstr(&buf));
	} else {
		cl_git_fail_with(GIT_ENOTFOUND,
				 git_config_get_string_buf(&buf, cfg, "foo.bar"));
	}

	git_buf_dispose(&buf);
	git_config_free(cfg);
}

static char *sandbox_path(git_buf *buf, const char *suffix)
{
	char *path = p_realpath(clar_sandbox_path(), NULL);
	cl_assert(path);
	cl_git_pass(git_buf_attach(buf, path, 0));
	cl_git_pass(git_buf_joinpath(buf, buf->ptr, suffix));
	return buf->ptr;
}

void test_config_conditionals__gitdir(void)
{
	git_buf path = GIT_BUF_INIT;

	assert_condition_includes("gitdir", ROOT_PREFIX "/", true);
	assert_condition_includes("gitdir", "empty_stand", false);
	assert_condition_includes("gitdir", "empty_stand/", false);
	assert_condition_includes("gitdir", "empty_stand/.git", false);
	assert_condition_includes("gitdir", "empty_stand/.git/", false);
	assert_condition_includes("gitdir", "empty_stand*/", true);
	assert_condition_includes("gitdir", "empty_stand*/.git", true);
	assert_condition_includes("gitdir", "empty_stand*/.git/", false);
	assert_condition_includes("gitdir", "empty_standard_repo", false);
	assert_condition_includes("gitdir", "empty_standard_repo/", true);
	assert_condition_includes("gitdir", "empty_standard_repo/.git", true);
	assert_condition_includes("gitdir", "empty_standard_repo/.git/", false);

	assert_condition_includes("gitdir", "./", false);

	assert_condition_includes("gitdir", ROOT_PREFIX "/nonexistent", false);
	assert_condition_includes("gitdir", ROOT_PREFIX "/empty_standard_repo", false);
	assert_condition_includes("gitdir", "~/empty_standard_repo", false);

	assert_condition_includes("gitdir", sandbox_path(&path, "/"), true);
	assert_condition_includes("gitdir", sandbox_path(&path, "/*"), false);
	assert_condition_includes("gitdir", sandbox_path(&path, "/**"), true);

	assert_condition_includes("gitdir", sandbox_path(&path, "empty_standard_repo"), false);
	assert_condition_includes("gitdir", sandbox_path(&path, "empty_standard_repo/"), true);
	assert_condition_includes("gitdir", sandbox_path(&path, "empty_standard_repo/"), true);
	assert_condition_includes("gitdir", sandbox_path(&path, "Empty_Standard_Repo"), false);
	assert_condition_includes("gitdir", sandbox_path(&path, "Empty_Standard_Repo/"), false);

	git_buf_dispose(&path);
}

void test_config_conditionals__gitdir_i(void)
{
	git_buf path = GIT_BUF_INIT;

	assert_condition_includes("gitdir/i", sandbox_path(&path, "empty_standard_repo/"), true);
	assert_condition_includes("gitdir/i", sandbox_path(&path, "EMPTY_STANDARD_REPO/"), true);

	git_buf_dispose(&path);
}

void test_config_conditionals__invalid_conditional_fails(void)
{
	assert_condition_includes("foobar", ".git", false);
}

static void set_head(git_repository *repo, const char *name)
{
	cl_git_pass(git_repository_create_head(git_repository_path(repo), name));
}

void test_config_conditionals__onbranch(void)
{
	assert_condition_includes("onbranch", "master", true);
	assert_condition_includes("onbranch", "m*", true);
	assert_condition_includes("onbranch", "*", true);
	assert_condition_includes("onbranch", "master/", false);
	assert_condition_includes("onbranch", "foo", false);

	set_head(_repo, "foo");
	assert_condition_includes("onbranch", "master", false);
	assert_condition_includes("onbranch", "foo", true);
	assert_condition_includes("onbranch", "f*o", true);

	set_head(_repo, "dir/ref");
	assert_condition_includes("onbranch", "dir/ref", true);
	assert_condition_includes("onbranch", "dir/", true);
	assert_condition_includes("onbranch", "dir/*", true);
	assert_condition_includes("onbranch", "dir/**", true);
	assert_condition_includes("onbranch", "**", true);
	assert_condition_includes("onbranch", "dir", false);
	assert_condition_includes("onbranch", "dir*", false);

	set_head(_repo, "dir/subdir/ref");
	assert_condition_includes("onbranch", "dir/subdir/", true);
	assert_condition_includes("onbranch", "dir/subdir/*", true);
	assert_condition_includes("onbranch", "dir/subdir/ref", true);
	assert_condition_includes("onbranch", "dir/", true);
	assert_condition_includes("onbranch", "dir/**", true);
	assert_condition_includes("onbranch", "**", true);
	assert_condition_includes("onbranch", "dir", false);
	assert_condition_includes("onbranch", "dir*", false);
	assert_condition_includes("onbranch", "dir/*", false);
}
