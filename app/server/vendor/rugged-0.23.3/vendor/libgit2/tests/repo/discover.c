#include "clar_libgit2.h"

#include "odb.h"
#include "fileops.h"
#include "repository.h"

#define TEMP_REPO_FOLDER "temprepo/"
#define DISCOVER_FOLDER TEMP_REPO_FOLDER "discover.git"

#define SUB_REPOSITORY_FOLDER_NAME "sub_repo"
#define SUB_REPOSITORY_FOLDER DISCOVER_FOLDER "/" SUB_REPOSITORY_FOLDER_NAME
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
				       git_buf *expected_path)
{
	git_buf found_path = GIT_BUF_INIT;
	cl_git_pass(git_repository_discover(&found_path, start_path, 0, ceiling_dirs));
	//across_fs is always 0 as we can't automate the filesystem change tests
	cl_assert_equal_s(found_path.ptr, expected_path->ptr);
	git_buf_free(&found_path);
}

static void write_file(const char *path, const char *content)
{
	git_file file;
   int error;

	if (git_path_exists(path)) {
		cl_git_pass(p_unlink(path));
	}

	file = git_futils_creat_withpath(path, 0777, 0666);
	cl_assert(file >= 0);

	error = p_write(file, content, strlen(content) * sizeof(char));
	p_close(file);
	cl_git_pass(error);
}

//no check is performed on ceiling_dirs length, so be sure it's long enough
static void append_ceiling_dir(git_buf *ceiling_dirs, const char *path)
{
	git_buf pretty_path = GIT_BUF_INIT;
	char ceiling_separator[2] = { GIT_PATH_LIST_SEPARATOR, '\0' };

	cl_git_pass(git_path_prettify_dir(&pretty_path, path, NULL));

	if (ceiling_dirs->size > 0)
		git_buf_puts(ceiling_dirs, ceiling_separator);

	git_buf_puts(ceiling_dirs, pretty_path.ptr);

	git_buf_free(&pretty_path);
	cl_assert(git_buf_oom(ceiling_dirs) == 0);
}

void test_repo_discover__0(void)
{
	// test discover
	git_repository *repo;
	git_buf ceiling_dirs_buf = GIT_BUF_INIT, repository_path = GIT_BUF_INIT,
		sub_repository_path = GIT_BUF_INIT, found_path = GIT_BUF_INIT;
	const char *ceiling_dirs;
	const mode_t mode = 0777;

	git_futils_mkdir_r(DISCOVER_FOLDER, NULL, mode);
	append_ceiling_dir(&ceiling_dirs_buf, TEMP_REPO_FOLDER);
	ceiling_dirs = git_buf_cstr(&ceiling_dirs_buf);

	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&repository_path, DISCOVER_FOLDER, 0, ceiling_dirs));

	cl_git_pass(git_repository_init(&repo, DISCOVER_FOLDER, 1));
	cl_git_pass(git_repository_discover(&repository_path, DISCOVER_FOLDER, 0, ceiling_dirs));
	git_repository_free(repo);

	cl_git_pass(git_repository_init(&repo, SUB_REPOSITORY_FOLDER, 0));
	cl_git_pass(git_futils_mkdir_r(SUB_REPOSITORY_FOLDER_SUB_SUB_SUB, NULL, mode));
	cl_git_pass(git_repository_discover(&sub_repository_path, SUB_REPOSITORY_FOLDER, 0, ceiling_dirs));

	cl_git_pass(git_futils_mkdir_r(SUB_REPOSITORY_FOLDER_SUB_SUB_SUB, NULL, mode));
	ensure_repository_discover(SUB_REPOSITORY_FOLDER_SUB, ceiling_dirs, &sub_repository_path);
	ensure_repository_discover(SUB_REPOSITORY_FOLDER_SUB_SUB, ceiling_dirs, &sub_repository_path);
	ensure_repository_discover(SUB_REPOSITORY_FOLDER_SUB_SUB_SUB, ceiling_dirs, &sub_repository_path);

	cl_git_pass(git_futils_mkdir_r(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB_SUB, NULL, mode));
	write_file(REPOSITORY_ALTERNATE_FOLDER "/" DOT_GIT, "gitdir: ../" SUB_REPOSITORY_FOLDER_NAME "/" DOT_GIT);
	write_file(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB "/" DOT_GIT, "gitdir: ../../../" SUB_REPOSITORY_FOLDER_NAME "/" DOT_GIT);
	write_file(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB_SUB "/" DOT_GIT, "gitdir: ../../../../");
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER, ceiling_dirs, &sub_repository_path);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB, ceiling_dirs, &sub_repository_path);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB, ceiling_dirs, &sub_repository_path);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB_SUB, ceiling_dirs, &repository_path);

	cl_git_pass(git_futils_mkdir_r(ALTERNATE_MALFORMED_FOLDER1, NULL, mode));
	write_file(ALTERNATE_MALFORMED_FOLDER1 "/" DOT_GIT, "Anything but not gitdir:");
	cl_git_pass(git_futils_mkdir_r(ALTERNATE_MALFORMED_FOLDER2, NULL, mode));
	write_file(ALTERNATE_MALFORMED_FOLDER2 "/" DOT_GIT, "gitdir:");
	cl_git_pass(git_futils_mkdir_r(ALTERNATE_MALFORMED_FOLDER3, NULL, mode));
	write_file(ALTERNATE_MALFORMED_FOLDER3 "/" DOT_GIT, "gitdir: \n\n\n");
	cl_git_pass(git_futils_mkdir_r(ALTERNATE_NOT_FOUND_FOLDER, NULL, mode));
	write_file(ALTERNATE_NOT_FOUND_FOLDER "/" DOT_GIT, "gitdir: a_repository_that_surely_does_not_exist");
	cl_git_fail(git_repository_discover(&found_path, ALTERNATE_MALFORMED_FOLDER1, 0, ceiling_dirs));
	cl_git_fail(git_repository_discover(&found_path, ALTERNATE_MALFORMED_FOLDER2, 0, ceiling_dirs));
	cl_git_fail(git_repository_discover(&found_path, ALTERNATE_MALFORMED_FOLDER3, 0, ceiling_dirs));
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&found_path, ALTERNATE_NOT_FOUND_FOLDER, 0, ceiling_dirs));

	append_ceiling_dir(&ceiling_dirs_buf, SUB_REPOSITORY_FOLDER);
	ceiling_dirs = git_buf_cstr(&ceiling_dirs_buf);

	//this must pass as ceiling_directories cannot predent the current
	//working directory to be checked
	cl_git_pass(git_repository_discover(&found_path, SUB_REPOSITORY_FOLDER, 0, ceiling_dirs));
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&found_path, SUB_REPOSITORY_FOLDER_SUB, 0, ceiling_dirs));
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&found_path, SUB_REPOSITORY_FOLDER_SUB_SUB, 0, ceiling_dirs));
	cl_assert_equal_i(GIT_ENOTFOUND, git_repository_discover(&found_path, SUB_REPOSITORY_FOLDER_SUB_SUB_SUB, 0, ceiling_dirs));

	//.gitfile redirection should not be affected by ceiling directories
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER, ceiling_dirs, &sub_repository_path);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB, ceiling_dirs, &sub_repository_path);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB, ceiling_dirs, &sub_repository_path);
	ensure_repository_discover(REPOSITORY_ALTERNATE_FOLDER_SUB_SUB_SUB, ceiling_dirs, &repository_path);

	cl_git_pass(git_futils_rmdir_r(TEMP_REPO_FOLDER, NULL, GIT_RMDIR_REMOVE_FILES));
	git_repository_free(repo);
	git_buf_free(&ceiling_dirs_buf);
	git_buf_free(&repository_path);
	git_buf_free(&sub_repository_path);
}

