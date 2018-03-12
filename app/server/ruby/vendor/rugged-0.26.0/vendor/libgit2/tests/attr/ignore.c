#include "clar_libgit2.h"
#include "posix.h"
#include "path.h"
#include "fileops.h"

static git_repository *g_repo = NULL;

void test_attr_ignore__initialize(void)
{
	g_repo = cl_git_sandbox_init("attr");
}

void test_attr_ignore__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

static void assert_is_ignored_(
	bool expected, const char *filepath, const char *file, int line)
{
	int is_ignored = 0;

	cl_git_expect(
		git_ignore_path_is_ignored(&is_ignored, g_repo, filepath), 0, file, line);

	clar__assert_equal(
		file, line, "expected != is_ignored", 1, "%d",
		(int)(expected != 0), (int)(is_ignored != 0));
}
#define assert_is_ignored(expected, filepath) \
	assert_is_ignored_(expected, filepath, __FILE__, __LINE__)

void test_attr_ignore__honor_temporary_rules(void)
{
	cl_git_rewritefile("attr/.gitignore", "/NewFolder\n/NewFolder/NewFolder");

	assert_is_ignored(false, "File.txt");
	assert_is_ignored(true, "NewFolder");
	assert_is_ignored(true, "NewFolder/NewFolder");
	assert_is_ignored(true, "NewFolder/NewFolder/File.txt");
}

void test_attr_ignore__allow_root(void)
{
	cl_git_rewritefile("attr/.gitignore", "/");

	assert_is_ignored(false, "File.txt");
	assert_is_ignored(false, "NewFolder");
	assert_is_ignored(false, "NewFolder/NewFolder");
	assert_is_ignored(false, "NewFolder/NewFolder/File.txt");
}

void test_attr_ignore__ignore_root(void)
{
	cl_git_rewritefile("attr/.gitignore", "/\n\n/NewFolder\n/NewFolder/NewFolder");

	assert_is_ignored(false, "File.txt");
	assert_is_ignored(true, "NewFolder");
	assert_is_ignored(true, "NewFolder/NewFolder");
	assert_is_ignored(true, "NewFolder/NewFolder/File.txt");
}

void test_attr_ignore__full_paths(void)
{
	cl_git_rewritefile("attr/.gitignore", "Folder/*/Contained");

	assert_is_ignored(true, "Folder/Middle/Contained");
	assert_is_ignored(false, "Folder/Middle/More/More/Contained");

	cl_git_rewritefile("attr/.gitignore", "Folder/**/Contained");

	assert_is_ignored(true, "Folder/Middle/Contained");
	assert_is_ignored(true, "Folder/Middle/More/More/Contained");

	cl_git_rewritefile("attr/.gitignore", "Folder/**/Contained/*/Child");

	assert_is_ignored(true, "Folder/Middle/Contained/Happy/Child");
	assert_is_ignored(false, "Folder/Middle/Contained/Not/Happy/Child");
	assert_is_ignored(true, "Folder/Middle/More/More/Contained/Happy/Child");
	assert_is_ignored(false, "Folder/Middle/More/More/Contained/Not/Happy/Child");
}

void test_attr_ignore__more_starstar_cases(void)
{
	cl_must_pass(p_unlink("attr/.gitignore"));
	cl_git_mkfile(
		"attr/dir/.gitignore",
		"sub/**/*.html\n");

	assert_is_ignored(false, "aaa.html");
	assert_is_ignored(false, "dir");
	assert_is_ignored(false, "dir/sub");
	assert_is_ignored(true,  "dir/sub/sub2/aaa.html");
	assert_is_ignored(true,  "dir/sub/aaa.html");
	assert_is_ignored(false, "dir/aaa.html");
	assert_is_ignored(false, "sub");
	assert_is_ignored(false, "sub/aaa.html");
	assert_is_ignored(false, "sub/sub2/aaa.html");
}

void test_attr_ignore__leading_stars(void)
{
	cl_git_rewritefile(
		"attr/.gitignore",
		"*/onestar\n"
		"**/twostars\n"
		"*/parent1/kid1/*\n"
		"**/parent2/kid2/*\n");

	assert_is_ignored(true, "dir1/onestar");
	assert_is_ignored(true, "dir1/onestar/child"); /* in ignored dir */
	assert_is_ignored(false, "dir1/dir2/onestar");

	assert_is_ignored(true, "dir1/twostars");
	assert_is_ignored(true, "dir1/twostars/child"); /* in ignored dir */
	assert_is_ignored(true, "dir1/dir2/twostars");
	assert_is_ignored(true, "dir1/dir2/twostars/child"); /* in ignored dir */
	assert_is_ignored(true, "dir1/dir2/dir3/twostars");

	assert_is_ignored(true, "dir1/parent1/kid1/file");
	assert_is_ignored(true, "dir1/parent1/kid1/file/inside/parent");
	assert_is_ignored(false, "dir1/dir2/parent1/kid1/file");
	assert_is_ignored(false, "dir1/parent1/file");
	assert_is_ignored(false, "dir1/kid1/file");

	assert_is_ignored(true, "dir1/parent2/kid2/file");
	assert_is_ignored(true, "dir1/parent2/kid2/file/inside/parent");
	assert_is_ignored(true, "dir1/dir2/parent2/kid2/file");
	assert_is_ignored(true, "dir1/dir2/dir3/parent2/kid2/file");
	assert_is_ignored(false, "dir1/parent2/file");
	assert_is_ignored(false, "dir1/kid2/file");
}

void test_attr_ignore__globs_and_path_delimiters(void)
{
	cl_git_rewritefile("attr/.gitignore", "foo/bar/**");
	assert_is_ignored(true, "foo/bar/baz");
	assert_is_ignored(true, "foo/bar/baz/quux");

	cl_git_rewritefile("attr/.gitignore", "_*/");
	assert_is_ignored(true, "sub/_test/a/file");
	assert_is_ignored(false, "test_folder/file");
	assert_is_ignored(true, "_test/file");
	assert_is_ignored(true, "_test/a/file");

	cl_git_rewritefile("attr/.gitignore", "**/_*/");
	assert_is_ignored(true, "sub/_test/a/file");
	assert_is_ignored(false, "test_folder/file");
	assert_is_ignored(true, "_test/file");
	assert_is_ignored(true, "_test/a/file");

	cl_git_rewritefile("attr/.gitignore", "**/_*/foo/bar/*ux");

	assert_is_ignored(true, "sub/_test/foo/bar/qux/file");
	assert_is_ignored(true, "_test/foo/bar/qux/file");
	assert_is_ignored(true, "_test/foo/bar/crux/file");
	assert_is_ignored(false, "_test/foo/bar/code/file");
}

void test_attr_ignore__skip_gitignore_directory(void)
{
	cl_git_rewritefile("attr/.git/info/exclude", "/NewFolder\n/NewFolder/NewFolder");
	p_unlink("attr/.gitignore");
	cl_assert(!git_path_exists("attr/.gitignore"));
	p_mkdir("attr/.gitignore", 0777);
	cl_git_mkfile("attr/.gitignore/garbage.txt", "new_file\n");

	assert_is_ignored(false, "File.txt");
	assert_is_ignored(true, "NewFolder");
	assert_is_ignored(true, "NewFolder/NewFolder");
	assert_is_ignored(true, "NewFolder/NewFolder/File.txt");
}

void test_attr_ignore__subdirectory_gitignore(void)
{
	p_unlink("attr/.gitignore");
	cl_assert(!git_path_exists("attr/.gitignore"));
	cl_git_mkfile(
		"attr/.gitignore",
		"file1\n");
	p_mkdir("attr/dir", 0777);
	cl_git_mkfile(
		"attr/dir/.gitignore",
		"file2/\n");

	assert_is_ignored(true, "file1");
	assert_is_ignored(true, "dir/file1");
	assert_is_ignored(true, "dir/file2/actual_file");  /* in ignored dir */
	assert_is_ignored(false, "dir/file3");
}

void test_attr_ignore__expand_tilde_to_homedir(void)
{
	git_config *cfg;

	assert_is_ignored(false, "example.global_with_tilde");

	cl_fake_home();

	/* construct fake home with fake global excludes */
	cl_git_mkfile("home/globalexclude", "# found me\n*.global_with_tilde\n");

	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_pass(git_config_set_string(cfg, "core.excludesfile", "~/globalexclude"));
	git_config_free(cfg);

	git_attr_cache_flush(g_repo); /* must reset to pick up change */

	assert_is_ignored(true, "example.global_with_tilde");

	cl_git_pass(git_futils_rmdir_r("home", NULL, GIT_RMDIR_REMOVE_FILES));

	cl_fake_home_cleanup(NULL);

	git_attr_cache_flush(g_repo); /* must reset to pick up change */

	assert_is_ignored(false, "example.global_with_tilde");
}

/* Ensure that the .gitignore in the subdirectory only affects
 * items in the subdirectory. */
void test_attr_ignore__gitignore_in_subdir(void)
{
	cl_git_rmfile("attr/.gitignore");

	cl_must_pass(p_mkdir("attr/dir1", 0777));
	cl_must_pass(p_mkdir("attr/dir1/dir2", 0777));
	cl_must_pass(p_mkdir("attr/dir1/dir2/dir3", 0777));

	cl_git_mkfile("attr/dir1/dir2/dir3/.gitignore", "dir1/\ndir1/subdir/");

	assert_is_ignored(false, "dir1/file");
	assert_is_ignored(false, "dir1/dir2/file");
	assert_is_ignored(false, "dir1/dir2/dir3/file");
	assert_is_ignored(true,  "dir1/dir2/dir3/dir1/file");
	assert_is_ignored(true,  "dir1/dir2/dir3/dir1/subdir/foo");

	if (cl_repo_get_bool(g_repo, "core.ignorecase")) {
		cl_git_mkfile("attr/dir1/dir2/dir3/.gitignore", "DiR1/\nDiR1/subdir/\n");

		assert_is_ignored(false, "dir1/file");
		assert_is_ignored(false, "dir1/dir2/file");
		assert_is_ignored(false, "dir1/dir2/dir3/file");
		assert_is_ignored(true,  "dir1/dir2/dir3/dir1/file");
		assert_is_ignored(true,  "dir1/dir2/dir3/dir1/subdir/foo");
	}
}

/* Ensure that files do not match folder cases */
void test_attr_ignore__dont_ignore_files_for_folder(void)
{
	cl_git_rmfile("attr/.gitignore");

	cl_git_mkfile("attr/dir/.gitignore", "test/\n");

	/* Create "test" as a file; ensure it is not ignored. */
	cl_git_mkfile("attr/dir/test", "This is a file.");

	assert_is_ignored(false, "dir/test");
	if (cl_repo_get_bool(g_repo, "core.ignorecase"))
		assert_is_ignored(false, "dir/TeSt");

	/* Create "test" as a directory; ensure it is ignored. */
	cl_git_rmfile("attr/dir/test");
	cl_must_pass(p_mkdir("attr/dir/test", 0777));

	assert_is_ignored(true, "dir/test");
	if (cl_repo_get_bool(g_repo, "core.ignorecase"))
		assert_is_ignored(true, "dir/TeSt");

	/* Remove "test" entirely; ensure it is not ignored.
	 * (As it doesn't exist, it is not a directory.)
	 */
	cl_must_pass(p_rmdir("attr/dir/test"));

	assert_is_ignored(false, "dir/test");
	if (cl_repo_get_bool(g_repo, "core.ignorecase"))
		assert_is_ignored(false, "dir/TeSt");
}

void test_attr_ignore__symlink_to_outside(void)
{
#ifdef GIT_WIN32
	cl_skip();
#endif

	cl_git_rewritefile("attr/.gitignore", "symlink\n");
	cl_git_mkfile("target", "target");
	cl_git_pass(p_symlink("../target", "attr/symlink"));
	assert_is_ignored(true, "symlink");
	assert_is_ignored(true, "lala/../symlink");
}

void test_attr_ignore__test(void)
{
	cl_git_rewritefile("attr/.gitignore",
		"/*/\n"
		"!/src\n");
	assert_is_ignored(false, "src/foo.c");
	assert_is_ignored(false, "src/foo/foo.c");
	assert_is_ignored(false, "README.md");
	assert_is_ignored(true, "dist/foo.o");
	assert_is_ignored(true, "bin/foo");
}

void test_attr_ignore__unignore_dir_succeeds(void)
{
	cl_git_rewritefile("attr/.gitignore",
		"*.c\n"
		"!src/*.c\n");
	assert_is_ignored(false, "src/foo.c");
	assert_is_ignored(true, "src/foo/foo.c");
}

void test_attr_ignore__case_insensitive_unignores_previous_rule(void)
{
	git_config *cfg;

	cl_git_rewritefile("attr/.gitignore",
		"/case\n"
		"!/Case/\n");

	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_pass(git_config_set_bool(cfg, "core.ignorecase", true));

	cl_must_pass(p_mkdir("attr/case", 0755));
	cl_git_mkfile("attr/case/file", "content");

	assert_is_ignored(false, "case/file");
}

void test_attr_ignore__case_sensitive_unignore_does_nothing(void)
{
	git_config *cfg;

	cl_git_rewritefile("attr/.gitignore",
		"/case\n"
		"!/Case/\n");

	cl_git_pass(git_repository_config(&cfg, g_repo));
	cl_git_pass(git_config_set_bool(cfg, "core.ignorecase", false));

	cl_must_pass(p_mkdir("attr/case", 0755));
	cl_git_mkfile("attr/case/file", "content");

	assert_is_ignored(true, "case/file");
}
