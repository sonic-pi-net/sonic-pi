#include "clar_libgit2.h"
#include "thread_helpers.h"

#ifdef GIT_THREADS

# if defined(GIT_WIN32)
#  define git_thread_yield() Sleep(0)
# elif defined(__FreeBSD__) || defined(__MidnightBSD__) || defined(__DragonFly__)
#  define git_thread_yield() pthread_yield()
# else
#  define git_thread_yield() sched_yield()
# endif

#else
# define git_thread_yield() (void)0
#endif

static git_repository *_repo;
static git_tree *_a, *_b;
static git_atomic _counts[4];
static int _check_counts;
#ifdef GIT_WIN32
static int _retries;
#endif

#define THREADS 20

void test_threads_diff__initialize(void)
{
#ifdef GIT_WIN32
	_retries = git_win32__retries;
	git_win32__retries = 1;
#endif
}

void test_threads_diff__cleanup(void)
{
	cl_git_sandbox_cleanup();

#ifdef GIT_WIN32
	git_win32__retries = _retries;
#endif
}

static void setup_trees(void)
{
	git_index *idx;

	_repo = cl_git_sandbox_reopen(); /* reopen sandbox to flush caches */

	/* avoid competing to load initial index */
	cl_git_pass(git_repository_index(&idx, _repo));
	git_index_free(idx);

	cl_git_pass(git_revparse_single(
		(git_object **)&_a, _repo, "0017bd4ab1^{tree}"));
	cl_git_pass(git_revparse_single(
		(git_object **)&_b, _repo, "26a125ee1b^{tree}"));

	memset(_counts, 0, sizeof(_counts));
}

static void free_trees(void)
{
	git_tree_free(_a); _a = NULL;
	git_tree_free(_b); _b = NULL;

	if (_check_counts) {
		cl_assert_equal_i(288, git_atomic_get(&_counts[0]));
		cl_assert_equal_i(112, git_atomic_get(&_counts[1]));
		cl_assert_equal_i( 80, git_atomic_get(&_counts[2]));
		cl_assert_equal_i( 96, git_atomic_get(&_counts[3]));
	}
}

static void *run_index_diffs(void *arg)
{
	int thread = *(int *)arg;
	git_repository *repo;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	size_t i;
	int exp[4] = { 0, 0, 0, 0 };

	cl_git_pass(git_repository_open(&repo, git_repository_path(_repo)));

	switch (thread & 0x03) {
	case 0: /* diff index to workdir */;
		cl_git_pass(git_diff_index_to_workdir(&diff, repo, NULL, &opts));
		break;
	case 1: /* diff tree 'a' to index */;
		cl_git_pass(git_diff_tree_to_index(&diff, repo, _a, NULL, &opts));
		break;
	case 2: /* diff tree 'b' to index */;
		cl_git_pass(git_diff_tree_to_index(&diff, repo, _b, NULL, &opts));
		break;
	case 3: /* diff index to workdir (explicit index) */;
		{
			git_index *idx;
			cl_git_pass(git_repository_index(&idx, repo));
			cl_git_pass(git_diff_index_to_workdir(&diff, repo, idx, &opts));
			git_index_free(idx);
			break;
		}
	}

	/* keep some diff stats to make sure results are as expected */

	i = git_diff_num_deltas(diff);
	git_atomic_add(&_counts[0], (int32_t)i);
	exp[0] = (int)i;

	while (i > 0) {
		switch (git_diff_get_delta(diff, --i)->status) {
		case GIT_DELTA_MODIFIED: exp[1]++; git_atomic_inc(&_counts[1]); break;
		case GIT_DELTA_ADDED:    exp[2]++; git_atomic_inc(&_counts[2]); break;
		case GIT_DELTA_DELETED:  exp[3]++; git_atomic_inc(&_counts[3]); break;
		default: break;
		}
	}

	switch (thread & 0x03) {
	case 0: case 3:
		cl_assert_equal_i(8, exp[0]); cl_assert_equal_i(4, exp[1]);
		cl_assert_equal_i(0, exp[2]); cl_assert_equal_i(4, exp[3]);
		break;
	case 1:
		cl_assert_equal_i(12, exp[0]); cl_assert_equal_i(3, exp[1]);
		cl_assert_equal_i(7, exp[2]); cl_assert_equal_i(2, exp[3]);
		break;
	case 2:
		cl_assert_equal_i(8, exp[0]); cl_assert_equal_i(3, exp[1]);
		cl_assert_equal_i(3, exp[2]); cl_assert_equal_i(2, exp[3]);
		break;
	}

	git_diff_free(diff);
	git_repository_free(repo);
	git_error_clear();

	return arg;
}

void test_threads_diff__concurrent_diffs(void)
{
	_repo = cl_git_sandbox_init("status");
	_check_counts = 1;

	run_in_parallel(
		5, 32, run_index_diffs, setup_trees, free_trees);
}

static void *run_index_diffs_with_modifier(void *arg)
{
	int thread = *(int *)arg;
	git_diff_options opts = GIT_DIFF_OPTIONS_INIT;
	git_diff *diff = NULL;
	git_index *idx = NULL;
	git_repository *repo;

	cl_git_pass(git_repository_open(&repo, git_repository_path(_repo)));
	cl_git_pass(git_repository_index(&idx, repo));

	/* have first thread altering the index as we go */
	if (thread == 0) {
		int i;

		for (i = 0; i < 300; ++i) {
			switch (i & 0x03) {
			case 0: (void)git_index_add_bypath(idx, "new_file"); break;
			case 1: (void)git_index_remove_bypath(idx, "modified_file"); break;
			case 2: (void)git_index_remove_bypath(idx, "new_file"); break;
			case 3: (void)git_index_add_bypath(idx, "modified_file"); break;
			}
			git_thread_yield();
		}

		goto done;
	}

	/* only use explicit index in this test to prevent reloading */

	switch (thread & 0x03) {
	case 0: /* diff index to workdir */;
		cl_git_pass(git_diff_index_to_workdir(&diff, repo, idx, &opts));
		break;
	case 1: /* diff tree 'a' to index */;
		cl_git_pass(git_diff_tree_to_index(&diff, repo, _a, idx, &opts));
		break;
	case 2: /* diff tree 'b' to index */;
		cl_git_pass(git_diff_tree_to_index(&diff, repo, _b, idx, &opts));
		break;
	case 3: /* diff index to workdir reversed */;
		opts.flags |= GIT_DIFF_REVERSE;
		cl_git_pass(git_diff_index_to_workdir(&diff, repo, idx, &opts));
		break;
	}

	/* results will be unpredictable with index modifier thread running */

	git_diff_free(diff);

done:
	git_index_free(idx);
	git_repository_free(repo);
	git_error_clear();

	return arg;
}

void test_threads_diff__with_concurrent_index_modified(void)
{
	_repo = cl_git_sandbox_init("status");
	_check_counts = 0;

	run_in_parallel(
		5, 16, run_index_diffs_with_modifier, setup_trees, free_trees);
}
