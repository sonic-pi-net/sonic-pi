#include "clar_libgit2.h"
#include "fileops.h"
#include "path.h"
#include "posix.h"

void test_core_copy__file(void)
{
	struct stat st;
	const char *content = "This is some stuff to copy\n";

	cl_git_mkfile("copy_me", content);

	cl_git_pass(git_futils_cp("copy_me", "copy_me_two", 0664));

	cl_git_pass(git_path_lstat("copy_me_two", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert(strlen(content) == (size_t)st.st_size);

	cl_git_pass(p_unlink("copy_me_two"));
	cl_git_pass(p_unlink("copy_me"));
}

void test_core_copy__file_in_dir(void)
{
	struct stat st;
	const char *content = "This is some other stuff to copy\n";

	cl_git_pass(git_futils_mkdir("an_dir/in_a_dir", NULL, 0775, GIT_MKDIR_PATH));
	cl_git_mkfile("an_dir/in_a_dir/copy_me", content);
	cl_assert(git_path_isdir("an_dir"));

	cl_git_pass(git_futils_mkpath2file
		("an_dir/second_dir/and_more/copy_me_two", 0775));

	cl_git_pass(git_futils_cp
		("an_dir/in_a_dir/copy_me",
		 "an_dir/second_dir/and_more/copy_me_two",
		 0664));

	cl_git_pass(git_path_lstat("an_dir/second_dir/and_more/copy_me_two", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert(strlen(content) == (size_t)st.st_size);

	cl_git_pass(git_futils_rmdir_r("an_dir", NULL, GIT_RMDIR_REMOVE_FILES));
	cl_assert(!git_path_isdir("an_dir"));
}

void assert_hard_link(const char *path)
{
	/* we assert this by checking that there's more than one link to the file */
	struct stat st;

	cl_assert(git_path_isfile(path));
	cl_git_pass(p_stat(path, &st));
	cl_assert(st.st_nlink > 1);
}

void test_core_copy__tree(void)
{
	struct stat st;
	const char *content = "File content\n";

	cl_git_pass(git_futils_mkdir("src/b", NULL, 0775, GIT_MKDIR_PATH));
	cl_git_pass(git_futils_mkdir("src/c/d", NULL, 0775, GIT_MKDIR_PATH));
	cl_git_pass(git_futils_mkdir("src/c/e", NULL, 0775, GIT_MKDIR_PATH));

	cl_git_mkfile("src/f1", content);
	cl_git_mkfile("src/b/f2", content);
	cl_git_mkfile("src/c/f3", content);
	cl_git_mkfile("src/c/d/f4", content);
	cl_git_mkfile("src/c/d/.f5", content);

#ifndef GIT_WIN32
	cl_assert(p_symlink("../../b/f2", "src/c/d/l1") == 0);
#endif

	cl_assert(git_path_isdir("src"));
	cl_assert(git_path_isdir("src/b"));
	cl_assert(git_path_isdir("src/c/d"));
	cl_assert(git_path_isfile("src/c/d/f4"));

	/* copy with no empty dirs, yes links, no dotfiles, no overwrite */

	cl_git_pass(
		git_futils_cp_r("src", "t1", GIT_CPDIR_COPY_SYMLINKS, 0) );

	cl_assert(git_path_isdir("t1"));
	cl_assert(git_path_isdir("t1/b"));
	cl_assert(git_path_isdir("t1/c"));
	cl_assert(git_path_isdir("t1/c/d"));
	cl_assert(!git_path_isdir("t1/c/e"));

	cl_assert(git_path_isfile("t1/f1"));
	cl_assert(git_path_isfile("t1/b/f2"));
	cl_assert(git_path_isfile("t1/c/f3"));
	cl_assert(git_path_isfile("t1/c/d/f4"));
	cl_assert(!git_path_isfile("t1/c/d/.f5"));

	cl_git_pass(git_path_lstat("t1/c/f3", &st));
	cl_assert(S_ISREG(st.st_mode));
	cl_assert(strlen(content) == (size_t)st.st_size);

#ifndef GIT_WIN32
	cl_git_pass(git_path_lstat("t1/c/d/l1", &st));
	cl_assert(S_ISLNK(st.st_mode));
#endif

	cl_git_pass(git_futils_rmdir_r("t1", NULL, GIT_RMDIR_REMOVE_FILES));
	cl_assert(!git_path_isdir("t1"));

	/* copy with empty dirs, no links, yes dotfiles, no overwrite */

	cl_git_pass(
		git_futils_cp_r("src", "t2", GIT_CPDIR_CREATE_EMPTY_DIRS | GIT_CPDIR_COPY_DOTFILES, 0) );

	cl_assert(git_path_isdir("t2"));
	cl_assert(git_path_isdir("t2/b"));
	cl_assert(git_path_isdir("t2/c"));
	cl_assert(git_path_isdir("t2/c/d"));
	cl_assert(git_path_isdir("t2/c/e"));

	cl_assert(git_path_isfile("t2/f1"));
	cl_assert(git_path_isfile("t2/b/f2"));
	cl_assert(git_path_isfile("t2/c/f3"));
	cl_assert(git_path_isfile("t2/c/d/f4"));
	cl_assert(git_path_isfile("t2/c/d/.f5"));

#ifndef GIT_WIN32
	cl_git_fail(git_path_lstat("t2/c/d/l1", &st));
#endif

	cl_git_pass(git_futils_rmdir_r("t2", NULL, GIT_RMDIR_REMOVE_FILES));
	cl_assert(!git_path_isdir("t2"));

#ifndef GIT_WIN32
	cl_git_pass(git_futils_cp_r("src", "t3", GIT_CPDIR_CREATE_EMPTY_DIRS | GIT_CPDIR_LINK_FILES, 0));
	cl_assert(git_path_isdir("t3"));

	cl_assert(git_path_isdir("t3"));
	cl_assert(git_path_isdir("t3/b"));
	cl_assert(git_path_isdir("t3/c"));
	cl_assert(git_path_isdir("t3/c/d"));
	cl_assert(git_path_isdir("t3/c/e"));

	assert_hard_link("t3/f1");
	assert_hard_link("t3/b/f2");
	assert_hard_link("t3/c/f3");
	assert_hard_link("t3/c/d/f4");
#endif

	cl_git_pass(git_futils_rmdir_r("src", NULL, GIT_RMDIR_REMOVE_FILES));
}
