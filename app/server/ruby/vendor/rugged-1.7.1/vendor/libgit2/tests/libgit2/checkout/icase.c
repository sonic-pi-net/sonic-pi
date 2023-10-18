#include "clar_libgit2.h"

#include "git2/checkout.h"
#include "refs.h"
#include "path.h"
#include "repository.h"

#ifdef GIT_WIN32
# include <windows.h>
#else
# include <dirent.h>
#endif

static git_repository *repo;
static git_object *obj;
static git_checkout_options checkout_opts;

void test_checkout_icase__initialize(void)
{
	git_oid id;
	git_config *cfg;
	int icase = 0;

	repo = cl_git_sandbox_init("testrepo");

	cl_git_pass(git_repository_config_snapshot(&cfg, repo));
	git_config_get_bool(&icase, cfg, "core.ignorecase");
	git_config_free(cfg);

	if (!icase)
		cl_skip();

	cl_git_pass(git_reference_name_to_id(&id, repo, "refs/heads/dir"));
	cl_git_pass(git_object_lookup(&obj, repo, &id, GIT_OBJECT_ANY));

	git_checkout_options_init(&checkout_opts, GIT_CHECKOUT_OPTIONS_VERSION);
	checkout_opts.checkout_strategy = GIT_CHECKOUT_NONE;
}

void test_checkout_icase__cleanup(void)
{
	git_object_free(obj);
	cl_git_sandbox_cleanup();
}

static char *get_filename(const char *in)
{
	char *search_dirname, *search_filename, *filename = NULL;
	git_str out = GIT_STR_INIT;
	DIR *dir;
	struct dirent *de;

	cl_assert(search_dirname = git_fs_path_dirname(in));
	cl_assert(search_filename = git_fs_path_basename(in));

	cl_assert(dir = opendir(search_dirname));

	while ((de = readdir(dir))) {
		if (strcasecmp(de->d_name, search_filename) == 0) {
			git_str_join(&out, '/', search_dirname, de->d_name);
			filename = git_str_detach(&out);
			break;
		}
	}

	closedir(dir);

	git__free(search_dirname);
	git__free(search_filename);
	git_str_dispose(&out);

	return filename;
}

static void assert_name_is(const char *expected)
{
	char *actual;
	size_t actual_len, expected_len, start;

	cl_assert(actual = get_filename(expected));

	expected_len = strlen(expected);
	actual_len = strlen(actual);
	cl_assert(actual_len >= expected_len);

	start = actual_len - expected_len;
	cl_assert_equal_s(expected, actual + start);

	if (start)
		cl_assert_equal_strn("/", actual + (start - 1), 1);

	free(actual);
}

static int symlink_or_fake(git_repository *repo, const char *a, const char *b)
{
	int symlinks;

	cl_git_pass(git_repository__configmap_lookup(&symlinks, repo, GIT_CONFIGMAP_SYMLINKS));

	if (symlinks)
		return p_symlink(a, b);
	else
		return git_futils_fake_symlink(a, b);
}

void test_checkout_icase__refuses_to_overwrite_files_for_files(void)
{
	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE|GIT_CHECKOUT_RECREATE_MISSING;

	cl_git_write2file("testrepo/BRANCH_FILE.txt", "neue file\n", 10, \
		O_WRONLY | O_CREAT | O_TRUNC, 0644);

	cl_git_fail(git_checkout_tree(repo, obj, &checkout_opts));
	assert_name_is("testrepo/BRANCH_FILE.txt");
}

void test_checkout_icase__overwrites_files_for_files_when_forced(void)
{
	checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_write2file("testrepo/NEW.txt", "neue file\n", 10, \
		O_WRONLY | O_CREAT | O_TRUNC, 0644);

	cl_git_pass(git_checkout_tree(repo, obj, &checkout_opts));
	assert_name_is("testrepo/new.txt");
}

void test_checkout_icase__refuses_to_overwrite_links_for_files(void)
{
	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE|GIT_CHECKOUT_RECREATE_MISSING;

	cl_must_pass(symlink_or_fake(repo, "../tmp", "testrepo/BRANCH_FILE.txt"));

	cl_git_fail(git_checkout_tree(repo, obj, &checkout_opts));

	cl_assert(!git_fs_path_exists("tmp"));
	assert_name_is("testrepo/BRANCH_FILE.txt");
}

void test_checkout_icase__overwrites_links_for_files_when_forced(void)
{
	checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_must_pass(symlink_or_fake(repo, "../tmp", "testrepo/NEW.txt"));

	cl_git_pass(git_checkout_tree(repo, obj, &checkout_opts));

	cl_assert(!git_fs_path_exists("tmp"));
	assert_name_is("testrepo/new.txt");
}

void test_checkout_icase__overwrites_empty_folders_for_files(void)
{
	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE|GIT_CHECKOUT_RECREATE_MISSING;

	cl_must_pass(p_mkdir("testrepo/NEW.txt", 0777));

	cl_git_pass(git_checkout_tree(repo, obj, &checkout_opts));

	assert_name_is("testrepo/new.txt");
	cl_assert(!git_fs_path_isdir("testrepo/new.txt"));
}

void test_checkout_icase__refuses_to_overwrite_populated_folders_for_files(void)
{
	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE|GIT_CHECKOUT_RECREATE_MISSING;

	cl_must_pass(p_mkdir("testrepo/BRANCH_FILE.txt", 0777));
	cl_git_write2file("testrepo/BRANCH_FILE.txt/foobar", "neue file\n", 10, \
		O_WRONLY | O_CREAT | O_TRUNC, 0644);

	cl_git_fail(git_checkout_tree(repo, obj, &checkout_opts));

	assert_name_is("testrepo/BRANCH_FILE.txt");
	cl_assert(git_fs_path_isdir("testrepo/BRANCH_FILE.txt"));
}

void test_checkout_icase__overwrites_folders_for_files_when_forced(void)
{
	checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_must_pass(p_mkdir("testrepo/NEW.txt", 0777));
	cl_git_write2file("testrepo/NEW.txt/foobar", "neue file\n", 10, \
		O_WRONLY | O_CREAT | O_TRUNC, 0644);

	cl_git_pass(git_checkout_tree(repo, obj, &checkout_opts));

	assert_name_is("testrepo/new.txt");
	cl_assert(!git_fs_path_isdir("testrepo/new.txt"));
}

void test_checkout_icase__refuses_to_overwrite_files_for_folders(void)
{
	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE|GIT_CHECKOUT_RECREATE_MISSING;

	cl_git_write2file("testrepo/A", "neue file\n", 10, \
		O_WRONLY | O_CREAT | O_TRUNC, 0644);

	cl_git_fail(git_checkout_tree(repo, obj, &checkout_opts));
	assert_name_is("testrepo/A");
	cl_assert(!git_fs_path_isdir("testrepo/A"));
}

void test_checkout_icase__overwrites_files_for_folders_when_forced(void)
{
	checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_git_write2file("testrepo/A", "neue file\n", 10, \
		O_WRONLY | O_CREAT | O_TRUNC, 0644);

	cl_git_pass(git_checkout_tree(repo, obj, &checkout_opts));
	assert_name_is("testrepo/a");
	cl_assert(git_fs_path_isdir("testrepo/a"));
}

void test_checkout_icase__refuses_to_overwrite_links_for_folders(void)
{
	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE|GIT_CHECKOUT_RECREATE_MISSING;

	cl_must_pass(symlink_or_fake(repo, "..", "testrepo/A"));

	cl_git_fail(git_checkout_tree(repo, obj, &checkout_opts));

	cl_assert(!git_fs_path_exists("b.txt"));
	assert_name_is("testrepo/A");
}

void test_checkout_icase__overwrites_links_for_folders_when_forced(void)
{
	checkout_opts.checkout_strategy = GIT_CHECKOUT_FORCE;

	cl_must_pass(symlink_or_fake(repo, "..", "testrepo/A"));

	cl_git_pass(git_checkout_tree(repo, obj, &checkout_opts));

	cl_assert(!git_fs_path_exists("b.txt"));
	assert_name_is("testrepo/a");
}

void test_checkout_icase__ignores_unstaged_casechange(void)
{
	git_reference *orig_ref, *br2_ref;
	git_commit *orig, *br2;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup_resolved(&orig_ref, repo, "HEAD", 100));
	cl_git_pass(git_commit_lookup(&orig, repo, git_reference_target(orig_ref)));
	cl_git_pass(git_reset(repo, (git_object *)orig, GIT_RESET_HARD, NULL));

	cl_rename("testrepo/branch_file.txt", "testrepo/Branch_File.txt");

	cl_git_pass(git_reference_lookup_resolved(&br2_ref, repo, "refs/heads/br2", 100));
	cl_git_pass(git_commit_lookup(&br2, repo, git_reference_target(br2_ref)));

	cl_git_pass(git_checkout_tree(repo, (const git_object *)br2, &checkout_opts));

	git_commit_free(orig);
	git_commit_free(br2);
	git_reference_free(orig_ref);
	git_reference_free(br2_ref);
}

void test_checkout_icase__conflicts_with_casechanged_subtrees(void)
{
	git_reference *orig_ref;
	git_object *orig, *subtrees;
	git_oid oid;
	git_checkout_options checkout_opts = GIT_CHECKOUT_OPTIONS_INIT;

	checkout_opts.checkout_strategy = GIT_CHECKOUT_SAFE;

	cl_git_pass(git_reference_lookup_resolved(&orig_ref, repo, "HEAD", 100));
	cl_git_pass(git_object_lookup(&orig, repo, git_reference_target(orig_ref), GIT_OBJECT_COMMIT));
	cl_git_pass(git_reset(repo, (git_object *)orig, GIT_RESET_HARD, NULL));

	cl_must_pass(p_mkdir("testrepo/AB", 0777));
	cl_must_pass(p_mkdir("testrepo/AB/C", 0777));
	cl_git_write2file("testrepo/AB/C/3.txt", "Foobar!\n", 8, O_RDWR|O_CREAT, 0666);

	cl_git_pass(git_reference_name_to_id(&oid, repo, "refs/heads/subtrees"));
	cl_git_pass(git_object_lookup(&subtrees, repo, &oid, GIT_OBJECT_ANY));

	cl_git_fail(git_checkout_tree(repo, subtrees, &checkout_opts));

	git_object_free(orig);
	git_object_free(subtrees);
    git_reference_free(orig_ref);
}

