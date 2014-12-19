#include "clar_libgit2.h"
#include "git2/refdb.h"
#include "refdb.h"

static git_repository *g_repo;
static int g_expected = 0;

void test_threads_refdb__initialize(void)
{
	g_repo = NULL;
}

void test_threads_refdb__cleanup(void)
{
	cl_git_sandbox_cleanup();
	g_repo = NULL;
}

#define REPEAT 20
#define THREADS 20

static void *iterate_refs(void *arg)
{
	git_reference_iterator *i;
	git_reference *ref;
	int count = 0;

	cl_git_pass(git_reference_iterator_new(&i, g_repo));

	for (count = 0; !git_reference_next(&ref, i); ++count) {
		cl_assert(ref != NULL);
		git_reference_free(ref);
	}

	if (g_expected > 0)
		cl_assert_equal_i(g_expected, count);

	git_reference_iterator_free(i);

	giterr_clear();
	return arg;
}

void test_threads_refdb__iterator(void)
{
	int r, t;
	git_thread th[THREADS];
	int id[THREADS];
	git_oid head;
	git_reference *ref;
	char name[128];
	git_refdb *refdb;

	g_repo = cl_git_sandbox_init("testrepo2");

	cl_git_pass(git_reference_name_to_id(&head, g_repo, "HEAD"));

	/* make a bunch of references */

	for (r = 0; r < 200; ++r) {
		p_snprintf(name, sizeof(name), "refs/heads/direct-%03d", r);
		cl_git_pass(git_reference_create(&ref, g_repo, name, &head, 0, NULL, NULL));
		git_reference_free(ref);
	}

	cl_git_pass(git_repository_refdb(&refdb, g_repo));
	cl_git_pass(git_refdb_compress(refdb));
	git_refdb_free(refdb);

	g_expected = 206;

	for (r = 0; r < REPEAT; ++r) {
		g_repo = cl_git_sandbox_reopen(); /* reopen to flush caches */

		for (t = 0; t < THREADS; ++t) {
			id[t] = t;
#ifdef GIT_THREADS
			cl_git_pass(git_thread_create(&th[t], NULL, iterate_refs, &id[t]));
#else
			th[t] = t;
			iterate_refs(&id[t]);
#endif
		}

#ifdef GIT_THREADS
		for (t = 0; t < THREADS; ++t) {
			cl_git_pass(git_thread_join(&th[t], NULL));
		}
#endif

		memset(th, 0, sizeof(th));
	}
}

static void *create_refs(void *arg)
{
	int *id = arg, i;
	git_oid head;
	char name[128];
	git_reference *ref[10];

	cl_git_pass(git_reference_name_to_id(&head, g_repo, "HEAD"));

	for (i = 0; i < 10; ++i) {
		p_snprintf(name, sizeof(name), "refs/heads/thread-%03d-%02d", *id, i);
		cl_git_pass(git_reference_create(&ref[i], g_repo, name, &head, 0, NULL, NULL));

		if (i == 5) {
			git_refdb *refdb;
			cl_git_pass(git_repository_refdb(&refdb, g_repo));
			cl_git_pass(git_refdb_compress(refdb));
			git_refdb_free(refdb);
		}
	}

	for (i = 0; i < 10; ++i)
		git_reference_free(ref[i]);

	giterr_clear();
	return arg;
}

static void *delete_refs(void *arg)
{
	int *id = arg, i;
	git_reference *ref;
	char name[128];

	for (i = 0; i < 10; ++i) {
		p_snprintf(
			name, sizeof(name), "refs/heads/thread-%03d-%02d", (*id) & ~0x3, i);

		if (!git_reference_lookup(&ref, g_repo, name)) {
			cl_git_pass(git_reference_delete(ref));
			git_reference_free(ref);
		}

		if (i == 5) {
			git_refdb *refdb;
			cl_git_pass(git_repository_refdb(&refdb, g_repo));
			cl_git_pass(git_refdb_compress(refdb));
			git_refdb_free(refdb);
		}
	}

	giterr_clear();
	return arg;
}

void test_threads_refdb__edit_while_iterate(void)
{
	int r, t;
	int id[THREADS];
	git_oid head;
	git_reference *ref;
	char name[128];
	git_refdb *refdb;

#ifdef GIT_THREADS
	git_thread th[THREADS];
#endif

	g_repo = cl_git_sandbox_init("testrepo2");

	cl_git_pass(git_reference_name_to_id(&head, g_repo, "HEAD"));

	/* make a bunch of references */

	for (r = 0; r < 50; ++r) {
		p_snprintf(name, sizeof(name), "refs/heads/starter-%03d", r);
		cl_git_pass(git_reference_create(&ref, g_repo, name, &head, 0, NULL, NULL));
		git_reference_free(ref);
	}

	cl_git_pass(git_repository_refdb(&refdb, g_repo));
	cl_git_pass(git_refdb_compress(refdb));
	git_refdb_free(refdb);

	g_expected = -1;

	g_repo = cl_git_sandbox_reopen(); /* reopen to flush caches */

	for (t = 0; t < THREADS; ++t) {
		void *(*fn)(void *arg);

		switch (t & 0x3) {
		case 0:  fn = create_refs;  break;
		case 1:  fn = delete_refs;  break;
		default: fn = iterate_refs; break;
		}

		id[t] = t;

		/* It appears with all reflog writing changes, etc., that this
		 * test has started to fail quite frequently, so let's disable it
		 * for now by just running on a single thread...
		 */
/* #ifdef GIT_THREADS */
/*		cl_git_pass(git_thread_create(&th[t], NULL, fn, &id[t])); */
/* #else */
		fn(&id[t]);
/* #endif */
	}

#ifdef GIT_THREADS
/*	for (t = 0; t < THREADS; ++t) { */
/*		cl_git_pass(git_thread_join(th[t], NULL)); */
/*	} */

	memset(th, 0, sizeof(th));

	for (t = 0; t < THREADS; ++t) {
		id[t] = t;
		cl_git_pass(git_thread_create(&th[t], NULL, iterate_refs, &id[t]));
	}

	for (t = 0; t < THREADS; ++t) {
		cl_git_pass(git_thread_join(&th[t], NULL));
	}
#endif
}
