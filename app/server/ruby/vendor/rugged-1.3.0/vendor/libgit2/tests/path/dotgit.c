#include "clar_libgit2.h"
#include "path.h"

static char *gitmodules_altnames[] = {
	".gitmodules",

	/*
	 * Equivalent to the ".git\u200cmodules" string from git but hard-coded
	 * as a UTF-8 sequence
	 */
	".git\xe2\x80\x8cmodules",

	".Gitmodules",
	".gitmoduleS",

	".gitmodules ",
	".gitmodules.",
	".gitmodules  ",
	".gitmodules. ",
	".gitmodules .",
	".gitmodules..",
	".gitmodules   ",
	".gitmodules.  ",
	".gitmodules . ",
	".gitmodules  .",

	".Gitmodules ",
	".Gitmodules.",
	".Gitmodules  ",
	".Gitmodules. ",
	".Gitmodules .",
	".Gitmodules..",
	".Gitmodules   ",
	".Gitmodules.  ",
	".Gitmodules . ",
	".Gitmodules  .",

	"GITMOD~1",
	"gitmod~1",
	"GITMOD~2",
	"gitmod~3",
	"GITMOD~4",

	"GITMOD~1 ",
	"gitmod~2.",
	"GITMOD~3  ",
	"gitmod~4. ",
	"GITMOD~1 .",
	"gitmod~2   ",
	"GITMOD~3.  ",
	"gitmod~4 . ",

	"GI7EBA~1",
	"gi7eba~9",

	"GI7EB~10",
	"GI7EB~11",
	"GI7EB~99",
	"GI7EB~10",
	"GI7E~100",
	"GI7E~101",
	"GI7E~999",
	"~1000000",
	"~9999999",
};

static char *gitmodules_not_altnames[] = {
	".gitmodules x",
	".gitmodules .x",

	" .gitmodules",

	"..gitmodules",

	"gitmodules",

	".gitmodule",

	".gitmodules x ",
	".gitmodules .x",

	"GI7EBA~",
	"GI7EBA~0",
	"GI7EBA~~1",
	"GI7EBA~X",
	"Gx7EBA~1",
	"GI7EBX~1",

	"GI7EB~1",
	"GI7EB~01",
	"GI7EB~1",
};

void test_path_dotgit__dotgit_modules(void)
{
	size_t i;

	cl_assert_equal_i(1, git_path_is_gitfile(".gitmodules", strlen(".gitmodules"), GIT_PATH_GITFILE_GITMODULES, GIT_PATH_FS_GENERIC));
	cl_assert_equal_i(1, git_path_is_gitfile(".git\xe2\x80\x8cmodules", strlen(".git\xe2\x80\x8cmodules"), GIT_PATH_GITFILE_GITMODULES, GIT_PATH_FS_GENERIC));

	for (i = 0; i < ARRAY_SIZE(gitmodules_altnames); i++) {
		const char *name = gitmodules_altnames[i];
		if (!git_path_is_gitfile(name, strlen(name), GIT_PATH_GITFILE_GITMODULES, GIT_PATH_FS_GENERIC))
			cl_fail(name);
	}

	for (i = 0; i < ARRAY_SIZE(gitmodules_not_altnames); i++) {
		const char *name = gitmodules_not_altnames[i];
		if (git_path_is_gitfile(name, strlen(name), GIT_PATH_GITFILE_GITMODULES, GIT_PATH_FS_GENERIC))
			cl_fail(name);
	}
}

void test_path_dotgit__dotgit_modules_symlink(void)
{
	cl_assert_equal_b(true, git_path_validate(NULL, ".gitmodules", 0, GIT_PATH_REJECT_DOT_GIT_HFS|GIT_PATH_REJECT_DOT_GIT_NTFS));
	cl_assert_equal_b(false, git_path_validate(NULL, ".gitmodules", S_IFLNK, GIT_PATH_REJECT_DOT_GIT_HFS));
	cl_assert_equal_b(false, git_path_validate(NULL, ".gitmodules", S_IFLNK, GIT_PATH_REJECT_DOT_GIT_NTFS));
	cl_assert_equal_b(false, git_path_validate(NULL, ".gitmodules . .::$DATA", S_IFLNK, GIT_PATH_REJECT_DOT_GIT_NTFS));
}
