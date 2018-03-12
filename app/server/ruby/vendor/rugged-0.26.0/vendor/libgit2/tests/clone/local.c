#include "clar_libgit2.h"

#include "git2/clone.h"
#include "clone.h"
#include "buffer.h"
#include "path.h"
#include "posix.h"
#include "fileops.h"

static int file_url(git_buf *buf, const char *host, const char *path)
{
	if (path[0] == '/')
		path++;

	git_buf_clear(buf);
	return git_buf_printf(buf, "file://%s/%s", host, path);
}

#ifdef GIT_WIN32
static int git_style_unc_path(git_buf *buf, const char *host, const char *path)
{
	git_buf_clear(buf);

	if (host)
		git_buf_printf(buf, "//%s/", host);

	if (path[0] == '/')
		path++;

	if (git__isalpha(path[0]) && path[1] == ':' && path[2] == '/') {
		git_buf_printf(buf, "%c$/", path[0]);
		path += 3;
	}

	git_buf_puts(buf, path);

	return git_buf_oom(buf) ? -1 : 0;
}

static int unc_path(git_buf *buf, const char *host, const char *path)
{
	char *c;

	if (git_style_unc_path(buf, host, path) < 0)
		return -1;

	for (c = buf->ptr; *c; c++)
		if (*c == '/')
			*c = '\\';

	return 0;
}
#endif

void test_clone_local__should_clone_local(void)
{
	git_buf buf = GIT_BUF_INIT;

	/* we use a fixture path because it needs to exist for us to want to clone */
	const char *path = cl_fixture("testrepo.git");

	cl_git_pass(file_url(&buf, "", path));
	cl_assert_equal_i(0, git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL_AUTO));
	cl_assert_equal_i(1,  git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL));
	cl_assert_equal_i(1,  git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL_NO_LINKS));
	cl_assert_equal_i(0, git_clone__should_clone_local(buf.ptr, GIT_CLONE_NO_LOCAL));

	cl_git_pass(file_url(&buf, "localhost", path));
	cl_assert_equal_i(0, git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL_AUTO));
	cl_assert_equal_i(1,  git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL));
	cl_assert_equal_i(1,  git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL_NO_LINKS));
	cl_assert_equal_i(0, git_clone__should_clone_local(buf.ptr, GIT_CLONE_NO_LOCAL));

	cl_git_pass(file_url(&buf, "other-host.mycompany.com", path));
	cl_assert_equal_i(0, git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL_AUTO));
	cl_assert_equal_i(0, git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL));
	cl_assert_equal_i(0, git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL_NO_LINKS));
	cl_assert_equal_i(0, git_clone__should_clone_local(buf.ptr, GIT_CLONE_NO_LOCAL));

	/* Ensure that file:/// urls are percent decoded: .git == %2e%67%69%74 */
	cl_git_pass(file_url(&buf, "", path));
	git_buf_shorten(&buf, 4);
	cl_git_pass(git_buf_puts(&buf, "%2e%67%69%74"));
	cl_assert_equal_i(0, git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL_AUTO));
	cl_assert_equal_i(1,  git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL));
	cl_assert_equal_i(1,  git_clone__should_clone_local(buf.ptr, GIT_CLONE_LOCAL_NO_LINKS));
	cl_assert_equal_i(0, git_clone__should_clone_local(buf.ptr, GIT_CLONE_NO_LOCAL));

	cl_assert_equal_i(1,  git_clone__should_clone_local(path, GIT_CLONE_LOCAL_AUTO));
	cl_assert_equal_i(1,  git_clone__should_clone_local(path, GIT_CLONE_LOCAL));
	cl_assert_equal_i(1,  git_clone__should_clone_local(path, GIT_CLONE_LOCAL_NO_LINKS));
	cl_assert_equal_i(0, git_clone__should_clone_local(path, GIT_CLONE_NO_LOCAL));

	git_buf_free(&buf);
}

void test_clone_local__hardlinks(void)
{
	git_repository *repo;
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	git_buf buf = GIT_BUF_INIT;
	struct stat st;

	/*
	 * In this first clone, we just copy over, since the temp dir
	 * will often be in a different filesystem, so we cannot
	 * link. It also allows us to control the number of links
	 */
	opts.bare = true;
	opts.local = GIT_CLONE_LOCAL_NO_LINKS;
	cl_git_pass(git_clone(&repo, cl_fixture("testrepo.git"), "./clone.git", &opts));
	git_repository_free(repo);

	/* This second clone is in the same filesystem, so we can hardlink */

	opts.local = GIT_CLONE_LOCAL;
	cl_git_pass(git_clone(&repo, cl_git_path_url("clone.git"), "./clone2.git", &opts));

#ifndef GIT_WIN32
	git_buf_clear(&buf);
	cl_git_pass(git_buf_join_n(&buf, '/', 4, git_repository_path(repo), "objects", "08", "b041783f40edfe12bb406c9c9a8a040177c125"));

	cl_git_pass(p_stat(buf.ptr, &st));
	cl_assert_equal_i(2, st.st_nlink);
#endif

	git_repository_free(repo);
	git_buf_clear(&buf);

	opts.local = GIT_CLONE_LOCAL_NO_LINKS;
	cl_git_pass(git_clone(&repo, cl_git_path_url("clone.git"), "./clone3.git", &opts));

	git_buf_clear(&buf);
	cl_git_pass(git_buf_join_n(&buf, '/', 4, git_repository_path(repo), "objects", "08", "b041783f40edfe12bb406c9c9a8a040177c125"));

	cl_git_pass(p_stat(buf.ptr, &st));
	cl_assert_equal_i(1, st.st_nlink);

	git_repository_free(repo);

	/* this one should automatically use links */
	cl_git_pass(git_clone(&repo, "./clone.git", "./clone4.git", NULL));

#ifndef GIT_WIN32
	git_buf_clear(&buf);
	cl_git_pass(git_buf_join_n(&buf, '/', 4, git_repository_path(repo), "objects", "08", "b041783f40edfe12bb406c9c9a8a040177c125"));

	cl_git_pass(p_stat(buf.ptr, &st));
	cl_assert_equal_i(3, st.st_nlink);
#endif

	git_buf_free(&buf);
	git_repository_free(repo);

	cl_git_pass(git_futils_rmdir_r("./clone.git", NULL, GIT_RMDIR_REMOVE_FILES));
	cl_git_pass(git_futils_rmdir_r("./clone2.git", NULL, GIT_RMDIR_REMOVE_FILES));
	cl_git_pass(git_futils_rmdir_r("./clone3.git", NULL, GIT_RMDIR_REMOVE_FILES));
	cl_git_pass(git_futils_rmdir_r("./clone4.git", NULL, GIT_RMDIR_REMOVE_FILES));
}

void test_clone_local__standard_unc_paths_are_written_git_style(void)
{
#ifdef GIT_WIN32
	git_repository *repo;
	git_remote *remote;
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	git_buf unc = GIT_BUF_INIT, git_unc = GIT_BUF_INIT;

	/* we use a fixture path because it needs to exist for us to want to clone */
	const char *path = cl_fixture("testrepo.git");

	cl_git_pass(unc_path(&unc, "localhost", path));
	cl_git_pass(git_style_unc_path(&git_unc, "localhost", path));

	cl_git_pass(git_clone(&repo, unc.ptr, "./clone.git", &opts));
	cl_git_pass(git_remote_lookup(&remote, repo, "origin"));

	cl_assert_equal_s(git_unc.ptr, git_remote_url(remote));

	git_remote_free(remote);
	git_repository_free(repo);
	git_buf_free(&unc);
	git_buf_free(&git_unc);

	cl_git_pass(git_futils_rmdir_r("./clone.git", NULL, GIT_RMDIR_REMOVE_FILES));
#endif
}

void test_clone_local__git_style_unc_paths(void)
{
#ifdef GIT_WIN32
	git_repository *repo;
	git_remote *remote;
	git_clone_options opts = GIT_CLONE_OPTIONS_INIT;
	git_buf git_unc = GIT_BUF_INIT;

	/* we use a fixture path because it needs to exist for us to want to clone */
	const char *path = cl_fixture("testrepo.git");

	cl_git_pass(git_style_unc_path(&git_unc, "localhost", path));

	cl_git_pass(git_clone(&repo, git_unc.ptr, "./clone.git", &opts));
	cl_git_pass(git_remote_lookup(&remote, repo, "origin"));

	cl_assert_equal_s(git_unc.ptr, git_remote_url(remote));

	git_remote_free(remote);
	git_repository_free(repo);
	git_buf_free(&git_unc);

	cl_git_pass(git_futils_rmdir_r("./clone.git", NULL, GIT_RMDIR_REMOVE_FILES));
#endif
}
