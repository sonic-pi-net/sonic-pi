#include "clar_libgit2.h"
#include "pool.h"

#include <git2.h>
#include "git2/sys/commit.h"
#include "git2/sys/mempack.h"

static size_t original_mwindow_file_limit = 0;

void test_pack_threadsafety__initialize(void)
{
	size_t open_mwindow_files = 1;

	cl_git_pass(git_libgit2_opts(GIT_OPT_GET_MWINDOW_FILE_LIMIT, &original_mwindow_file_limit));
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_MWINDOW_FILE_LIMIT, open_mwindow_files));
}

void test_pack_threadsafety__cleanup(void)
{
	cl_git_pass(git_libgit2_opts(GIT_OPT_SET_MWINDOW_FILE_LIMIT, original_mwindow_file_limit));
}

#ifdef GIT_THREADS
static void *get_status(void *arg)
{
	const char *repo_path = (const char *)arg;
	git_repository *repo;
	git_status_list *status;

	cl_git_pass(git_repository_open(&repo, repo_path));
	cl_git_pass(git_status_list_new(&status, repo, NULL));
	git_status_list_free(status);
	git_repository_free(repo);

	return NULL;
}
#endif

void test_pack_threadsafety__open_repo_in_multiple_threads(void)
{
#ifdef GIT_THREADS
	const char *repo_path = cl_fixture("../..");
	git_repository *repo;
	git_thread threads[8];
	size_t i;

	/* If we can't open the libgit2 repo or if it isn't a full repo
	 * with proper history, just skip this test */
	if (git_repository_open(&repo, repo_path) < 0)
		cl_skip();
	if (git_repository_is_shallow(repo))
		cl_skip();
	git_repository_free(repo);

	for (i = 0; i < ARRAY_SIZE(threads); i++)
		git_thread_create(&threads[i], get_status, (void *)repo_path);
	for (i = 0; i < ARRAY_SIZE(threads); i++)
		git_thread_join(&threads[i], NULL);
#else
	cl_skip();
#endif
}
