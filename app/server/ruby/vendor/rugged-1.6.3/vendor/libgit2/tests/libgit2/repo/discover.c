#include "clar_libgit2.h"

#include "odb.h"
#include "futils.h"
#include "repository.h"

#define TEMP_REPO_FOLDER "temprepo/"
#define DISCOVER_FOLDER TEMP_REPO_FOLDER "discover.git"

#define SUB_REPOSITORY_FOLDER_NAME "sub_repo"
#define SUB_REPOSITORY_FOLDER DISCOVER_FOLDER "/" SUB_REPOSITORY_FOLDER_NAME
#define SUB_REPOSITORY_GITDIR SUB_REPOSITORY_FOLDER "/.git"
#define SUB_REPOSITORY_FOLDER_SUB SUB_REPOSITORY_FOLDER "/sub"
#define SUB_REPOSITORY_FOLDER_SUB_SUB SUB_REPOSITORY_FOLDER_SUB "/subsub"
#define SUB_REPOSITORY_FOLDER_SUB_SUB_SUB SUB_REPOSITORY_FOLDER_SUB_SUB "/subsubsub"

#define REPOSITORY_ALTERNATE_FOLDER DISCOVER_FOLDER "/alternate_sub_repo"
#define REPOSITORY_ALTERNATE_FOLDER_SUB REPOSITORY_ALTERNATE_FOLDER "/sub"
#define REPOSITORY_ALTERNATE_FOLDER_SUB_SUB REPOSITORY_ALTERNATE_FOLDER_SUB "/subsub"
#define REPOSITORY_ALTERNATE_FOLDER_SUB_SUB_SUB REPOSITORY_ALTERNATE_FOLDER_SUB_SUB "/subsubsub"

#define ALTERNATE_MALFORMED_FOLDER1 DISCOVER_FOLDER "/alternate_malformed_repo1"
#define ALTERNATE_MALFORMED_FOLDER2 DISCOVER_FOLDER "/alternate_malformed_repo2"
#define ALTERNATE_MALFORMED_FOLDER3 DISCOVER_FOLDER "/alternate_malformed_repo3"
#define ALTERNATE_NOT_FOUND_FOLDER DISCOVER_FOLDER "/alternate_not_found_repo"

static void ensure_repository_discover(const char *start_path,
				       const char *ceiling_dirs,
				       const char *expected_path)
{
	git_buf found_path = GIT_BUF_INIT;
	git_str resolved = GIT_STR_INIT;

	git_str_attach(&resolved, p_realpath(expected_path, NULL), 0);
	cl_assert(resolved.size > 0);
	cl_git_pass(git_fs_path_to_dir(&resolved));
	cl_git_pass(git_repository_discover(&found_path, start_path, 1, ceiling_dirs));

	cl_assert_equal_s(found_path.ptr, resolved.ptr);

	git_str_dispose(&resolved);
	git_buf_dispose(&found_path);
}

static void write_file(const char *path, const char *content)
{
	git_file file;
	int error;

	if (git_fs_path_exists(path)) {
		cl_git_pass(p_unlink(path));
	}

	file = git_futils_creat_withpath(path, 0777, 0666);
	cl_assert(file >= 0);

	error = p_write(file, content, strlen(content) * sizeof(char));
	p_close(file);
	cl_git_pass(error);
}

/*no check is performed on ceiling_dirs length, so be sure it's long enough */
static void append_ceiling_dir(git_str *ceiling_dirs, const char *path)
{
	git_str pretty_path = GIT_STR_INIT;
	char ceiling_separator[2] = { GIT_PATH_LIST_SEPARATOR, '\0' };

	cl_git_pass(git_fs_path_prettify_dir(&pretty_path, path, NULL));

	if (ceiling_dirs->size > 0)
		git_str_puts(ceiling_dirs, ceiling_separator);

	git_str_puts(ceiling_dirs, pretty_path.ptr);

	git_str_dispose(&pretty_path);
	cl_assert(git_str_oom(ceiling_dirs) == 0);
}

static git_buf discovered;
static git_str ceiling_dirs;

void test_repo_discover__initialize(void)
{
	git_repository *repo;
	const mode_t mode = 0777;
	git_futils_mkdir_r(DISCOVER_FOLDER, mode);

	git_str_init(&ceiling_dirs, 0);
	append_ceiling_dir(&ceiling_dirs, TEMP_REPO_FOLDER);

	cl_git_pass(git_repository_init(&repo, DISCOVER_FOLDER, 1));
	git_repository_free(repo);

	cl_git_pass(git_repository_init(&repo, SUB_REPOSITORY_FOLDER, 0));
	cl_git_pass(git_futils_mkdir_r(SUB_REPOSITORY_FOLDER_SUB_SUB_SUB, mode));
	cl_git_pass(git_futils_mkdir_r(SUB_REPOSITORY_FOLDER_SUB_SUB_SUB, mode));

	cl_git_pass(git_futils_mkdir_r(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB_SUB, mode));
	write_file(REPOSITORY_ALTERNATE_FOLDER "/" DOT_GIT, "gitdir: ../" SUB_REPOSITORY_FOLDER_NAME "/" DOT_GIT);
	write_file(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB "/" DOT_GIT, "gitdir: ../../../" SUB_REPOSITORY_FOLDER_NAME "/" DOT_GIT);
	write_file(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB_SUB "/" DOT_GIT, "gitdir: ../../../../");

	cl_git_pass(git_futils_mkdir_r(ALTERNATE_MALFORMED_FOLDER1, mode));
	write_file(ALTERNATE_MALFORMED_FOLDER1 "/" DOT_GIT, "Anything but not gitdir:");
	cl_git_pass(git_futils_mkdir_r(ALTERNATE_MALFORMED_FOLDER2, mode));
	write_file(ALTERNATE_MALFORMED_FOLDER2 "/" DOT_GIT, "gitdir:");
	cl_git_pass(git_futils_mkdir_r(ALTERNATE_MALFORMED_FOLDER3, mode));
	write_file(ALTERNATE_MALFORMED_FOLDER3 "/" DOT_GIT, "gitdir: \n\n\n");
	cl_git_pass(git_futils_mkdir_r(ALTERNATE_NOT_FOUND_FOLDER, mode));
	write_file(ALTERNATE_NOT_FOUND_FOLDER "/" DOT_GIT, "gitdir: a_repository_that_surely_does_not_exist");

	git_repository_free(repo);
}

void test_repo_discover__cleanup(void)
{
	git_buf_dispose(&discovered);
	git_str_dispose(&ceiling_dirs);
	cl_git_pass(git_futils_rmdir_r(TEMP_REPO_FOLDER, NULL, GIT_RMDIR_REMOVE_FILES));
}

void test_repo_discover__discovering_repo_with_exact_path_succeeds(void)
{
	cl_git_pass(git_repository_discover(&discovered, DISCOVER_FOLDER, 0, ceiling_dirs.ptr));
	cl_git_pass(git_repository_discover(&discovered, SUB_REPOSITORY_FOLDER, 0, ceiling_dirs.ptr));
}

void test_repo_discover__discovering_nonexistent_dir_fails(void)
{
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&discovered, DISCOVER_FOLDER "-nonexistent", 0, NULL));
}

void test_repo_discover__discovering_repo_with_subdirectory_succeeds(void)
{
	ensure_repository_discover(SUB_REPOSITORY_FOLDER_SUB, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);
	ensure_repository_discover(SUB_REPOSITORY_FOLDER_SUB_SUB, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);
	ensure_repository_discover(SUB_REPOSITORY_FOLDER_SUB_SUB_SUB, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);
}

void test_repo_discover__discovering_repository_with_alternative_gitdir_succeeds(void)
{
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB_SUB, ceiling_dirs.ptr, DISCOVER_FOLDER);
}

void test_repo_discover__discovering_repository_with_malformed_alternative_gitdir_fails(void)
{
	cl_git_fail(git_repository_discover(&discovered, ALTERNATE_MALFORMED_FOLDER1, 0, ceiling_dirs.ptr));
	cl_git_fail(git_repository_discover(&discovered, ALTERNATE_MALFORMED_FOLDER2, 0, ceiling_dirs.ptr));
	cl_git_fail(git_repository_discover(&discovered, ALTERNATE_MALFORMED_FOLDER3, 0, ceiling_dirs.ptr));
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&discovered, ALTERNATE_NOT_FOUND_FOLDER, 0, ceiling_dirs.ptr));
}

void test_repo_discover__discovering_repository_with_ceiling(void)
{
	append_ceiling_dir(&ceiling_dirs, SUB_REPOSITORY_FOLDER_SUB);

	/* this must pass as ceiling_directories cannot prevent the current
	 * working directory to be checked */
	ensure_repository_discover(SUB_REPOSITORY_FOLDER, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);

	ensure_repository_discover(SUB_REPOSITORY_FOLDER_SUB, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&discovered, SUB_REPOSITORY_FOLDER_SUB_SUB, 0, ceiling_dirs.ptr));
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&discovered, SUB_REPOSITORY_FOLDER_SUB_SUB_SUB, 0, ceiling_dirs.ptr));
}

void test_repo_discover__other_ceiling(void)
{
	append_ceiling_dir(&ceiling_dirs, SUB_REPOSITORY_FOLDER);

	/* this must pass as ceiling_directories cannot predent the current
	 * working directory to be checked */
	ensure_repository_discover(SUB_REPOSITORY_FOLDER, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);

	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&discovered, SUB_REPOSITORY_FOLDER_SUB, 0, ceiling_dirs.ptr));
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&discovered, SUB_REPOSITORY_FOLDER_SUB_SUB, 0, ceiling_dirs.ptr));
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&discovered, SUB_REPOSITORY_FOLDER_SUB_SUB_SUB, 0, ceiling_dirs.ptr));
}

void test_repo_discover__ceiling_should_not_affect_gitdir_redirection(void)
{
	append_ceiling_dir(&ceiling_dirs, SUB_REPOSITORY_FOLDER);

	/* gitfile redirection should not be affected by ceiling directories */
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB, ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB_SUB, ceiling_dirs.ptr, DISCOVER_FOLDER);
}

void test_repo_discover__discovery_starting_at_file_succeeds(void)
{
	int fd;

	cl_assert((fd = p_creat(SUB_REPOSITORY_FOLDER "/file", 0600)) >= 0);
	cl_assert(p_close(fd) == 0);

	ensure_repository_discover(SUB_REPOSITORY_FOLDER "/file", ceiling_dirs.ptr, SUB_REPOSITORY_GITDIR);
}

void test_repo_discover__discovery_starting_at_system_root_causes_no_hang(void)
{
#ifdef GIT_WIN32
	git_buf out = GIT_BUF_INIT;
	cl_git_fail(git_repository_discover(&out, "C:/", 0, NULL));
	cl_git_fail(git_repository_discover(&out, "//localhost/", 0, NULL));
#endif
}
