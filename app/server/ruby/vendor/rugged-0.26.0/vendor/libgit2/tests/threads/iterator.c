#include "clar_libgit2.h"
#include "thread_helpers.h"
#include "iterator.h"

static git_repository *_repo;

void test_threads_iterator__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

static void *run_workdir_iterator(void *arg)
{
	int error = 0;
	git_iterator *iter;
	git_iterator_options iter_opts = GIT_ITERATOR_OPTIONS_INIT;
	const git_index_entry *entry = NULL;

	iter_opts.flags = GIT_ITERATOR_DONT_AUTOEXPAND;

	cl_git_pass(git_iterator_for_workdir(
		&iter, _repo, NULL, NULL, &iter_opts));

	while (!error) {
		if (entry && entry->mode == GIT_FILEMODE_TREE) {
			error = git_iterator_advance_into(&entry, iter);

			if (error == GIT_ENOTFOUND)
				error = git_iterator_advance(&entry, iter);
		} else {
			error = git_iterator_advance(&entry, iter);
		}

		if (!error)
			(void)git_iterator_current_is_ignored(iter);
	}

	cl_assert_equal_i(GIT_ITEROVER, error);

	git_iterator_free(iter);
	giterr_clear();
	return arg;
}


void test_threads_iterator__workdir(void)
{
	_repo = cl_git_sandbox_init("status");

	run_in_parallel(
		1, 20, run_workdir_iterator, NULL, NULL);
}
