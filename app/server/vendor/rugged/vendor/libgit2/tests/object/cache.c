#include "clar_libgit2.h"
#include "repository.h"

static git_repository *g_repo;

void test_object_cache__initialize(void)
{
	g_repo = NULL;
}

void test_object_cache__cleanup(void)
{
	git_repository_free(g_repo);
	g_repo = NULL;

	git_libgit2_opts(GIT_OPT_SET_CACHE_OBJECT_LIMIT, (int)GIT_OBJ_BLOB, (size_t)0);
}

static struct {
	git_otype type;
	const char *sha;
} g_data[] = {
	/* HEAD */
	{ GIT_OBJ_BLOB, "a8233120f6ad708f843d861ce2b7228ec4e3dec6" }, /* README */
	{ GIT_OBJ_BLOB, "3697d64be941a53d4ae8f6a271e4e3fa56b022cc" }, /* branch_file.txt */
	{ GIT_OBJ_BLOB, "a71586c1dfe8a71c6cbf6c129f404c5642ff31bd" }, /* new.txt */

	/* refs/heads/subtrees */
	{ GIT_OBJ_BLOB, "1385f264afb75a56a5bec74243be9b367ba4ca08" }, /* README */
	{ GIT_OBJ_TREE, "f1425cef211cc08caa31e7b545ffb232acb098c3" }, /* ab */
	{ GIT_OBJ_BLOB, "d6c93164c249c8000205dd4ec5cbca1b516d487f" }, /* ab/4.txt */
	{ GIT_OBJ_TREE, "9a03079b8a8ee85a0bee58bf9be3da8b62414ed4" }, /* ab/c */
	{ GIT_OBJ_BLOB, "270b8ea76056d5cad83af921837702d3e3c2924d" }, /* ab/c/3.txt */
	{ GIT_OBJ_TREE, "b6361fc6a97178d8fc8639fdeed71c775ab52593" }, /* ab/de */
	{ GIT_OBJ_BLOB, "e7b4ad382349ff96dd8199000580b9b1e2042eb0" }, /* ab/de/2.txt */
	{ GIT_OBJ_TREE, "3259a6bd5b57fb9c1281bb7ed3167b50f224cb54" }, /* ab/de/fgh */
	{ GIT_OBJ_BLOB, "1f67fc4386b2d171e0d21be1c447e12660561f9b" }, /* ab/de/fgh/1.txt */
	{ GIT_OBJ_BLOB, "45b983be36b73c0788dc9cbcb76cbb80fc7bb057" }, /* branch_file.txt */
	{ GIT_OBJ_BLOB, "fa49b077972391ad58037050f2a75f74e3671e92" }, /* new.txt */

	/* refs/heads/chomped */
	{ GIT_OBJ_BLOB, "0266163a49e280c4f5ed1e08facd36a2bd716bcf" }, /* readme.txt */

	{ 0, NULL },
	{ 0, NULL }
};

void test_object_cache__cache_everything(void)
{
	int i, start;
	git_oid oid;
	git_odb_object *odb_obj;
	git_object *obj;
	git_odb *odb;

	git_libgit2_opts(
		GIT_OPT_SET_CACHE_OBJECT_LIMIT, (int)GIT_OBJ_BLOB, (size_t)32767);

	cl_git_pass(git_repository_open(&g_repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_repository_odb(&odb, g_repo));

	start = (int)git_cache_size(&g_repo->objects);

	for (i = 0; g_data[i].sha != NULL; ++i) {
		int count = (int)git_cache_size(&g_repo->objects);

		cl_git_pass(git_oid_fromstr(&oid, g_data[i].sha));

		/* alternate between loading raw and parsed objects */
		if ((i & 1) == 0) {
			cl_git_pass(git_odb_read(&odb_obj, odb, &oid));
			cl_assert(g_data[i].type == git_odb_object_type(odb_obj));
			git_odb_object_free(odb_obj);
		} else {
			cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));
			cl_assert(g_data[i].type == git_object_type(obj));
			git_object_free(obj);
		}

		cl_assert_equal_i(count + 1, (int)git_cache_size(&g_repo->objects));
	}

	cl_assert_equal_i(i, (int)git_cache_size(&g_repo->objects) - start);

	git_odb_free(odb);

	for (i = 0; g_data[i].sha != NULL; ++i) {
		int count = (int)git_cache_size(&g_repo->objects);

		cl_git_pass(git_oid_fromstr(&oid, g_data[i].sha));
		cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));
		cl_assert(g_data[i].type == git_object_type(obj));
		git_object_free(obj);

		cl_assert_equal_i(count, (int)git_cache_size(&g_repo->objects));
	}
}

void test_object_cache__cache_no_blobs(void)
{
	int i, start, nonblobs = 0;
	git_oid oid;
	git_odb_object *odb_obj;
	git_object *obj;
	git_odb *odb;

	git_libgit2_opts(GIT_OPT_SET_CACHE_OBJECT_LIMIT, (int)GIT_OBJ_BLOB, (size_t)0);

	cl_git_pass(git_repository_open(&g_repo, cl_fixture("testrepo.git")));
	cl_git_pass(git_repository_odb(&odb, g_repo));

	start = (int)git_cache_size(&g_repo->objects);

	for (i = 0; g_data[i].sha != NULL; ++i) {
		int count = (int)git_cache_size(&g_repo->objects);

		cl_git_pass(git_oid_fromstr(&oid, g_data[i].sha));

		/* alternate between loading raw and parsed objects */
		if ((i & 1) == 0) {
			cl_git_pass(git_odb_read(&odb_obj, odb, &oid));
			cl_assert(g_data[i].type == git_odb_object_type(odb_obj));
			git_odb_object_free(odb_obj);
		} else {
			cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));
			cl_assert(g_data[i].type == git_object_type(obj));
			git_object_free(obj);
		}

		if (g_data[i].type == GIT_OBJ_BLOB)
			cl_assert_equal_i(count, (int)git_cache_size(&g_repo->objects));
		else {
			cl_assert_equal_i(count + 1, (int)git_cache_size(&g_repo->objects));
			nonblobs++;
		}
	}

	cl_assert_equal_i(nonblobs, (int)git_cache_size(&g_repo->objects) - start);

	git_odb_free(odb);
}

static void *cache_parsed(void *arg)
{
	int i;
	git_oid oid;
	git_object *obj;

	for (i = ((int *)arg)[1]; g_data[i].sha != NULL; i += 2) {
		cl_git_pass(git_oid_fromstr(&oid, g_data[i].sha));
		cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));
		cl_assert(g_data[i].type == git_object_type(obj));
		git_object_free(obj);
	}

	for (i = 0; i < ((int *)arg)[1]; i += 2) {
		cl_git_pass(git_oid_fromstr(&oid, g_data[i].sha));
		cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));
		cl_assert(g_data[i].type == git_object_type(obj));
		git_object_free(obj);
	}

	return arg;
}

static void *cache_raw(void *arg)
{
	int i;
	git_oid oid;
	git_odb *odb;
	git_odb_object *odb_obj;

	cl_git_pass(git_repository_odb(&odb, g_repo));

	for (i = ((int *)arg)[1]; g_data[i].sha != NULL; i += 2) {
		cl_git_pass(git_oid_fromstr(&oid, g_data[i].sha));
		cl_git_pass(git_odb_read(&odb_obj, odb, &oid));
		cl_assert(g_data[i].type == git_odb_object_type(odb_obj));
		git_odb_object_free(odb_obj);
	}

	for (i = 0; i < ((int *)arg)[1]; i += 2) {
		cl_git_pass(git_oid_fromstr(&oid, g_data[i].sha));
		cl_git_pass(git_odb_read(&odb_obj, odb, &oid));
		cl_assert(g_data[i].type == git_odb_object_type(odb_obj));
		git_odb_object_free(odb_obj);
	}

	git_odb_free(odb);

	return arg;
}

#define REPEAT 20
#define THREADCOUNT 50

void test_object_cache__threadmania(void)
{
	int try, th, max_i;
	void *data;
	void *(*fn)(void *);

#ifdef GIT_THREADS
	git_thread t[THREADCOUNT];
#endif

	for (max_i = 0; g_data[max_i].sha != NULL; ++max_i)
		/* count up */;

	for (try = 0; try < REPEAT; ++try) {

		cl_git_pass(git_repository_open(&g_repo, cl_fixture("testrepo.git")));

		for (th = 0; th < THREADCOUNT; ++th) {
			data = git__malloc(2 * sizeof(int));

			((int *)data)[0] = th;
			((int *)data)[1] = th % max_i;

			fn = (th & 1) ? cache_parsed : cache_raw;

#ifdef GIT_THREADS
			cl_git_pass(git_thread_create(&t[th], NULL, fn, data));
#else
			cl_assert(fn(data) == data);
			git__free(data);
#endif
		}

#ifdef GIT_THREADS
		for (th = 0; th < THREADCOUNT; ++th) {
			cl_git_pass(git_thread_join(t[th], &data));
			cl_assert_equal_i(th, ((int *)data)[0]);
			git__free(data);
		}
#endif

		git_repository_free(g_repo);
		g_repo = NULL;
	}
}

static void *cache_quick(void *arg)
{
	git_oid oid;
	git_object *obj;

	cl_git_pass(git_oid_fromstr(&oid, g_data[4].sha));
	cl_git_pass(git_object_lookup(&obj, g_repo, &oid, GIT_OBJ_ANY));
	cl_assert(g_data[4].type == git_object_type(obj));
	git_object_free(obj);

	return arg;
}

void test_object_cache__fast_thread_rush(void)
{
	int try, th, data[THREADCOUNT*2];
#ifdef GIT_THREADS
	git_thread t[THREADCOUNT*2];
#endif

	for (try = 0; try < REPEAT; ++try) {
		cl_git_pass(git_repository_open(&g_repo, cl_fixture("testrepo.git")));

		for (th = 0; th < THREADCOUNT*2; ++th) {
			data[th] = th;
#ifdef GIT_THREADS
			cl_git_pass(
				git_thread_create(&t[th], NULL, cache_quick, &data[th]));
#else
			cl_assert(cache_quick(&data[th]) == &data[th]);
#endif
		}

#ifdef GIT_THREADS
		for (th = 0; th < THREADCOUNT*2; ++th) {
			void *rval;
			cl_git_pass(git_thread_join(t[th], &rval));
			cl_assert_equal_i(th, *((int *)rval));
		}
#endif

		git_repository_free(g_repo);
		g_repo = NULL;
	}
}
