#include "clar_libgit2.h"
#include "thread_helpers.h"

void run_in_parallel(
	int repeats,
	int threads,
	void *(*func)(void *),
	void (*before_test)(void),
	void (*after_test)(void))
{
	int r, t, *id = git__calloc(threads, sizeof(int));
#ifdef GIT_THREADS
	git_thread *th = git__calloc(threads, sizeof(git_thread));
	cl_assert(th != NULL);
#else
	void *th = NULL;
#endif

	cl_assert(id != NULL);

	for (r = 0; r < repeats; ++r) {
		if (before_test) before_test();

		for (t = 0; t < threads; ++t) {
			id[t] = t;
#ifdef GIT_THREADS
			cl_git_pass(git_thread_create(&th[t], func, &id[t]));
#else
			cl_assert(func(&id[t]) == &id[t]);
#endif
		}

#ifdef GIT_THREADS
		for (t = 0; t < threads; ++t)
			cl_git_pass(git_thread_join(&th[t], NULL));
		memset(th, 0, threads * sizeof(git_thread));
#endif

		if (after_test) after_test();
	}

	git__free(id);
	git__free(th);
}
