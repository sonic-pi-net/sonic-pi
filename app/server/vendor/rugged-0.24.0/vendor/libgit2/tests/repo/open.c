#include "clar_libgit2.h"
#include "fileops.h"
#include "sysdir.h"
#include <ctype.h>

void test_repo_open__cleanup(void)
{
	cl_git_sandbox_cleanup();

	if (git_path_isdir("alternate"))
		git_futils_rmdir_r("alternate", NULL, GIT_RMDIR_REMOVE_FILES);
}

void test_repo_open__bare_empty_repo(void)
{
	git_repository *repo = cl_git_sandbox_init("empty_bare.git");

	cl_assert(git_repository_path(repo) != NULL);
	cl_assert(git__suffixcmp(git_repository_path(repo), "/") == 0);
	cl_assert(git_repository_workdir(repo) == NULL);
}

void test_repo_open__format_version_1(void)
{
	git_repository *repo;
	git_config *config;

	repo = cl_git_sandbox_init("empty_bare.git");

	cl_git_pass(git_repository_open(&repo, "empty_bare.git"));
	cl_git_pass(git_repository_config(&config, repo));

	cl_git_pass(git_config_set_int32(config, "core.repositoryformatversion", 1));

	git_config_free(config);
	git_repository_free(repo);
	cl_git_fail(git_repository_open(&repo, "empty_bare.git"));
}

void test_repo_open__standard_empty_repo_through_gitdir(void)
{
	git_repository *repo;

	cl_git_pass(git_repository_open(&repo, cl_fixture("empty_standard_repo/.gitted")));

	cl_assert(git_repository_path(repo) != NULL);
	cl_assert(git__suffixcmp(git_repository_path(repo), "/") == 0);

	cl_assert(git_repository_workdir(repo) != NULL);
	cl_assert(git__suffixcmp(git_repository_workdir(repo), "/") == 0);

	git_repository_free(repo);
}

void test_repo_open__standard_empty_repo_through_workdir(void)
{
	git_repository *repo = cl_git_sandbox_init("empty_standard_repo");

	cl_assert(git_repository_path(repo) != NULL);
	cl_assert(git__suffixcmp(git_repository_path(repo), "/") == 0);

	cl_assert(git_repository_workdir(repo) != NULL);
	cl_assert(git__suffixcmp(git_repository_workdir(repo), "/") == 0);
}


void test_repo_open__open_with_discover(void)
{
	static const char *variants[] = {
		"attr", "attr/", "attr/.git", "attr/.git/",
		"attr/sub", "attr/sub/", "attr/sub/sub", "attr/sub/sub/",
		NULL
	};
	git_repository *repo;
	const char **scan;

	cl_fixture_sandbox("attr");
	cl_git_pass(p_rename("attr/.gitted", "attr/.git"));

	for (scan = variants; *scan != NULL; scan++) {
		cl_git_pass(git_repository_open_ext(&repo, *scan, 0, NULL));
		cl_assert(git__suffixcmp(git_repository_path(repo), "attr/.git/") == 0);
		cl_assert(git__suffixcmp(git_repository_workdir(repo), "attr/") == 0);
		git_repository_free(repo);
	}

	cl_fixture_cleanup("attr");
}

static void make_gitlink_dir(const char *dir, const char *linktext)
{
	git_buf path = GIT_BUF_INIT;

	cl_git_pass(git_futils_mkdir(dir, 0777, GIT_MKDIR_VERIFY_DIR));
	cl_git_pass(git_buf_joinpath(&path, dir, ".git"));
	cl_git_rewritefile(path.ptr, linktext);
	git_buf_free(&path);
}

void test_repo_open__gitlinked(void)
{
	/* need to have both repo dir and workdir set up correctly */
	git_repository *repo = cl_git_sandbox_init("empty_standard_repo");
	git_repository *repo2;

	make_gitlink_dir("alternate", "gitdir: ../empty_standard_repo/.git");

	cl_git_pass(git_repository_open(&repo2, "alternate"));

	cl_assert(git_repository_path(repo2) != NULL);
	cl_assert_(git__suffixcmp(git_repository_path(repo2), "empty_standard_repo/.git/") == 0, git_repository_path(repo2));
	cl_assert_equal_s(git_repository_path(repo), git_repository_path(repo2));

	cl_assert(git_repository_workdir(repo2) != NULL);
	cl_assert_(git__suffixcmp(git_repository_workdir(repo2), "alternate/") == 0, git_repository_workdir(repo2));

	git_repository_free(repo2);
}

void test_repo_open__from_git_new_workdir(void)
{
#ifndef GIT_WIN32
	/* The git-new-workdir script that ships with git sets up a bunch of
	 * symlinks to create a second workdir that shares the object db with
	 * another checkout.  Libgit2 can open a repo that has been configured
	 * this way.
	 */

	git_repository *repo2;
	git_buf link_tgt = GIT_BUF_INIT, link = GIT_BUF_INIT, body = GIT_BUF_INIT;
	const char **scan;
	int link_fd;
	static const char *links[] = {
		"config", "refs", "logs/refs", "objects", "info", "hooks",
		"packed-refs", "remotes", "rr-cache", "svn", NULL
	};
	static const char *copies[] = {
		"HEAD", NULL
	};

	cl_git_sandbox_init("empty_standard_repo");

	cl_git_pass(p_mkdir("alternate", 0777));
	cl_git_pass(p_mkdir("alternate/.git", 0777));

	for (scan = links; *scan != NULL; scan++) {
		git_buf_joinpath(&link_tgt, "empty_standard_repo/.git", *scan);
		if (git_path_exists(link_tgt.ptr)) {
			git_buf_joinpath(&link_tgt, "../../empty_standard_repo/.git", *scan);
			git_buf_joinpath(&link, "alternate/.git", *scan);
			if (strchr(*scan, '/'))
				git_futils_mkpath2file(link.ptr, 0777);
			cl_assert_(symlink(link_tgt.ptr, link.ptr) == 0, strerror(errno));
		}
	}
	for (scan = copies; *scan != NULL; scan++) {
		git_buf_joinpath(&link_tgt, "empty_standard_repo/.git", *scan);
		if (git_path_exists(link_tgt.ptr)) {
			git_buf_joinpath(&link, "alternate/.git", *scan);
			cl_git_pass(git_futils_readbuffer(&body, link_tgt.ptr));

			cl_assert((link_fd = git_futils_creat_withpath(link.ptr, 0777, 0666)) >= 0);
			cl_must_pass(p_write(link_fd, body.ptr, body.size));
			p_close(link_fd);
		}
	}

	git_buf_free(&link_tgt);
	git_buf_free(&link);
	git_buf_free(&body);


	cl_git_pass(git_repository_open(&repo2, "alternate"));

	cl_assert(git_repository_path(repo2) != NULL);
	cl_assert_(git__suffixcmp(git_repository_path(repo2), "alternate/.git/") == 0, git_repository_path(repo2));

	cl_assert(git_repository_workdir(repo2) != NULL);
	cl_assert_(git__suffixcmp(git_repository_workdir(repo2), "alternate/") == 0, git_repository_workdir(repo2));

	git_repository_free(repo2);
#endif
}

void test_repo_open__failures(void)
{
	git_repository *base, *repo;
	git_buf ceiling = GIT_BUF_INIT;

	base = cl_git_sandbox_init("attr");
	cl_git_pass(git_buf_sets(&ceiling, git_repository_workdir(base)));

	/* fail with no searching */
	cl_git_fail(git_repository_open(&repo, "attr/sub"));
	cl_git_fail(git_repository_open_ext(
		&repo, "attr/sub", GIT_REPOSITORY_OPEN_NO_SEARCH, NULL));

	/* fail with ceiling too low */
	cl_git_pass(git_buf_joinpath(&ceiling, ceiling.ptr, "sub"));
	cl_git_fail(git_repository_open_ext(&repo, "attr/sub", 0, ceiling.ptr));

	/* fail with no repo */
	cl_git_pass(p_mkdir("alternate", 0777));
	cl_git_pass(p_mkdir("alternate/.git", 0777));
	cl_git_fail(git_repository_open_ext(&repo, "alternate", 0, NULL));
	cl_git_fail(git_repository_open_ext(&repo, "alternate/.git", 0, NULL));

	git_buf_free(&ceiling);
}

void test_repo_open__bad_gitlinks(void)
{
	git_repository *repo;
	static const char *bad_links[] = {
		"garbage\n", "gitdir", "gitdir:\n", "gitdir: foobar",
		"gitdir: ../invalid", "gitdir: ../invalid2",
		"gitdir: ../attr/.git with extra stuff",
		NULL
	};
	const char **scan;

	cl_git_sandbox_init("attr");

	cl_git_pass(p_mkdir("invalid", 0777));
	cl_git_pass(git_futils_mkdir_r("invalid2/.git", 0777));

	for (scan = bad_links; *scan != NULL; scan++) {
		make_gitlink_dir("alternate", *scan);
		cl_git_fail(git_repository_open_ext(&repo, "alternate", 0, NULL));
	}

	git_futils_rmdir_r("invalid", NULL, GIT_RMDIR_REMOVE_FILES);
	git_futils_rmdir_r("invalid2", NULL, GIT_RMDIR_REMOVE_FILES);
}

#ifdef GIT_WIN32
static void unposix_path(git_buf *path)
{
	char *src, *tgt;

	src = tgt = path->ptr;

	/* convert "/d/..." to "d:\..." */
	if (src[0] == '/' && isalpha(src[1]) && src[2] == '/') {
		*tgt++ = src[1];
		*tgt++ = ':';
		*tgt++ = '\\';
		src += 3;
	}

	while (*src) {
		*tgt++ = (*src == '/') ? '\\' : *src;
		src++;
	}

	*tgt = '\0';
}
#endif

void test_repo_open__win32_path(void)
{
#ifdef GIT_WIN32
	git_repository *repo = cl_git_sandbox_init("empty_standard_repo"), *repo2;
	git_buf winpath = GIT_BUF_INIT;
	static const char *repo_path = "empty_standard_repo/.git/";
	static const char *repo_wd   = "empty_standard_repo/";

	cl_assert(git__suffixcmp(git_repository_path(repo), repo_path) == 0);
	cl_assert(git__suffixcmp(git_repository_workdir(repo), repo_wd) == 0);

	cl_git_pass(git_buf_sets(&winpath, git_repository_path(repo)));
	unposix_path(&winpath);
	cl_git_pass(git_repository_open(&repo2, winpath.ptr));
	cl_assert(git__suffixcmp(git_repository_path(repo2), repo_path) == 0);
	cl_assert(git__suffixcmp(git_repository_workdir(repo2), repo_wd) == 0);
	git_repository_free(repo2);

	cl_git_pass(git_buf_sets(&winpath, git_repository_path(repo)));
	git_buf_truncate(&winpath, winpath.size - 1); /* remove trailing '/' */
	unposix_path(&winpath);
	cl_git_pass(git_repository_open(&repo2, winpath.ptr));
	cl_assert(git__suffixcmp(git_repository_path(repo2), repo_path) == 0);
	cl_assert(git__suffixcmp(git_repository_workdir(repo2), repo_wd) == 0);
	git_repository_free(repo2);

	cl_git_pass(git_buf_sets(&winpath, git_repository_workdir(repo)));
	unposix_path(&winpath);
	cl_git_pass(git_repository_open(&repo2, winpath.ptr));
	cl_assert(git__suffixcmp(git_repository_path(repo2), repo_path) == 0);
	cl_assert(git__suffixcmp(git_repository_workdir(repo2), repo_wd) == 0);
	git_repository_free(repo2);

	cl_git_pass(git_buf_sets(&winpath, git_repository_workdir(repo)));
	git_buf_truncate(&winpath, winpath.size - 1); /* remove trailing '/' */
	unposix_path(&winpath);
	cl_git_pass(git_repository_open(&repo2, winpath.ptr));
	cl_assert(git__suffixcmp(git_repository_path(repo2), repo_path) == 0);
	cl_assert(git__suffixcmp(git_repository_workdir(repo2), repo_wd) == 0);
	git_repository_free(repo2);

	git_buf_free(&winpath);
#endif
}

void test_repo_open__opening_a_non_existing_repository_returns_ENOTFOUND(void)
{
	git_repository *repo;
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_open(&repo, "i-do-not/exist"));
}

void test_repo_open__no_config(void)
{
	git_buf path = GIT_BUF_INIT;
	git_repository *repo;
	git_config *config;

	cl_fixture_sandbox("empty_standard_repo");
	cl_git_pass(cl_rename(
		"empty_standard_repo/.gitted", "empty_standard_repo/.git"));

	/* remove local config */
	cl_git_pass(git_futils_rmdir_r(
		"empty_standard_repo/.git/config", NULL, GIT_RMDIR_REMOVE_FILES));

	/* isolate from system level configs */
	cl_must_pass(p_mkdir("alternate", 0777));
	cl_git_pass(git_path_prettify(&path, "alternate", NULL));
	cl_git_pass(git_libgit2_opts(
		GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_GLOBAL, path.ptr));
	cl_git_pass(git_libgit2_opts(
		GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_SYSTEM, path.ptr));
	cl_git_pass(git_libgit2_opts(
		GIT_OPT_SET_SEARCH_PATH, GIT_CONFIG_LEVEL_XDG, path.ptr));

	git_buf_free(&path);

	cl_git_pass(git_repository_open(&repo, "empty_standard_repo"));
	cl_git_pass(git_repository_config(&config, repo));

	cl_git_pass(git_config_set_string(config, "test.set", "42"));

	git_config_free(config);
	git_repository_free(repo);
	cl_fixture_cleanup("empty_standard_repo");

	cl_sandbox_set_search_path_defaults();
}

void test_repo_open__force_bare(void)
{
	/* need to have both repo dir and workdir set up correctly */
	git_repository *repo = cl_git_sandbox_init("empty_standard_repo");
	git_repository *barerepo;

	make_gitlink_dir("alternate", "gitdir: ../empty_standard_repo/.git");

	cl_assert(!git_repository_is_bare(repo));

	cl_git_pass(git_repository_open(&barerepo, "alternate"));
	cl_assert(!git_repository_is_bare(barerepo));
	git_repository_free(barerepo);

	cl_git_pass(git_repository_open_bare(
		&barerepo, "empty_standard_repo/.git"));
	cl_assert(git_repository_is_bare(barerepo));
	git_repository_free(barerepo);

	cl_git_fail(git_repository_open_bare(&barerepo, "alternate/.git"));

	cl_git_pass(git_repository_open_ext(
		&barerepo, "alternate/.git", GIT_REPOSITORY_OPEN_BARE, NULL));
	cl_assert(git_repository_is_bare(barerepo));
	git_repository_free(barerepo);

	cl_git_pass(p_mkdir("empty_standard_repo/subdir", 0777));
	cl_git_mkfile("empty_standard_repo/subdir/something.txt", "something");

	cl_git_fail(git_repository_open_bare(
		&barerepo, "empty_standard_repo/subdir"));

	cl_git_pass(git_repository_open_ext(
		&barerepo, "empty_standard_repo/subdir", GIT_REPOSITORY_OPEN_BARE, NULL));
	cl_assert(git_repository_is_bare(barerepo));
	git_repository_free(barerepo);

	cl_git_pass(p_mkdir("alternate/subdir", 0777));
	cl_git_pass(p_mkdir("alternate/subdir/sub2", 0777));
	cl_git_mkfile("alternate/subdir/sub2/something.txt", "something");

	cl_git_fail(git_repository_open_bare(&barerepo, "alternate/subdir/sub2"));

	cl_git_pass(git_repository_open_ext(
		&barerepo, "alternate/subdir/sub2", GIT_REPOSITORY_OPEN_BARE, NULL));
	cl_assert(git_repository_is_bare(barerepo));
	git_repository_free(barerepo);
}
