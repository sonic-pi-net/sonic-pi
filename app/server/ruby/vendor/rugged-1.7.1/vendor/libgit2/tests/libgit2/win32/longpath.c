#include "clar_libgit2.h"

#include "git2/clone.h"
#include "clone.h"
#include "futils.h"
#include "repository.h"

static git_str path = GIT_STR_INIT;

#define LONG_FILENAME "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.txt"

void test_win32_longpath__initialize(void)
{
#ifdef GIT_WIN32
	const char *base = clar_sandbox_path();
	size_t base_len = strlen(base);
	size_t remain = MAX_PATH - base_len;
	size_t i;

	git_str_clear(&path);
	git_str_puts(&path, base);
	git_str_putc(&path, '/');

	cl_assert(remain < (MAX_PATH - 5));

	for (i = 0; i < (remain - 5); i++)
		git_str_putc(&path, 'a');
#endif
}

void test_win32_longpath__cleanup(void)
{
	git_str_dispose(&path);
	cl_git_sandbox_cleanup();
}

void test_win32_longpath__errmsg_on_checkout(void)
{
#ifdef GIT_WIN32
	git_repository *repo;

	cl_git_fail(git_clone(&repo, cl_fixture("testrepo.git"), path.ptr, NULL));
	cl_assert(git__prefixcmp(git_error_last()->message, "path too long") == 0);
#endif
}

void test_win32_longpath__workdir_path_validated(void)
{
#ifdef GIT_WIN32
	git_repository *repo = cl_git_sandbox_init("testrepo");
	git_str out = GIT_STR_INIT;

	cl_git_pass(git_repository_workdir_path(&out, repo, "a.txt"));

	/* even if the repo path is a drive letter, this is too long */
	cl_git_fail(git_repository_workdir_path(&out, repo, LONG_FILENAME));
	cl_assert(git__prefixcmp(git_error_last()->message, "path too long") == 0);

	cl_repo_set_bool(repo, "core.longpaths", true);
	cl_git_pass(git_repository_workdir_path(&out, repo, "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa.txt"));
	cl_git_pass(git_repository_workdir_path(&out, repo, LONG_FILENAME));
	git_str_dispose(&out);
#endif
}

#ifdef GIT_WIN32
static void assert_longpath_status_and_add(git_repository *repo, const char *wddata, const char *repodata) {
	git_index *index;
	git_blob *blob;
	git_str out = GIT_STR_INIT;
	const git_index_entry *entry;
	unsigned int status_flags;

	cl_git_pass(git_repository_workdir_path(&out, repo, LONG_FILENAME));

	cl_git_rewritefile(out.ptr, wddata);

	cl_git_pass(git_status_file(&status_flags, repo, LONG_FILENAME));
	cl_assert_equal_i(GIT_STATUS_WT_NEW, status_flags);

	cl_git_pass(git_repository_index(&index, repo));
	cl_git_pass(git_index_add_bypath(index, LONG_FILENAME));

	cl_git_pass(git_status_file(&status_flags, repo, LONG_FILENAME));
	cl_assert_equal_i(GIT_STATUS_INDEX_NEW, status_flags);

	cl_assert((entry = git_index_get_bypath(index, LONG_FILENAME, 0)) != NULL);
	cl_git_pass(git_blob_lookup(&blob, repo, &entry->id));
	cl_assert_equal_s(repodata, git_blob_rawcontent(blob));

	git_blob_free(blob);
	git_index_free(index);
	git_str_dispose(&out);
}
#endif

void test_win32_longpath__status_and_add(void)
{
#ifdef GIT_WIN32
	git_repository *repo = cl_git_sandbox_init("testrepo");

	cl_repo_set_bool(repo, "core.longpaths", true);

	/*
	 * Doing no content filtering, we expect the data we add
	 * to be the data in the repository.
	 */
	assert_longpath_status_and_add(repo,
	    "This is a long path.\r\n",
	    "This is a long path.\r\n");
#endif
}

void test_win32_longpath__status_and_add_with_filter(void)
{
#ifdef GIT_WIN32
	git_repository *repo = cl_git_sandbox_init("testrepo");

	cl_repo_set_bool(repo, "core.longpaths", true);
	cl_repo_set_bool(repo, "core.autocrlf", true);

	/*
	 * With `core.autocrlf`, we expect the data we add to have
	 * newline conversion performed.
	 */
	assert_longpath_status_and_add(repo,
	    "This is a long path.\r\n",
	    "This is a long path.\n");
#endif
}
