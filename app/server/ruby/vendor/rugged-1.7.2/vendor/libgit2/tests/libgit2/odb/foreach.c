#include "clar_libgit2.h"
#include "odb.h"
#include "git2/odb_backend.h"
#include "pack.h"

static git_odb *_odb;
static git_repository *_repo;

void test_odb_foreach__cleanup(void)
{
	git_odb_free(_odb);
	git_repository_free(_repo);

	_odb = NULL;
	_repo = NULL;
}

static int foreach_cb(const git_oid *oid, void *data)
{
	int *nobj = data;
	(*nobj)++;

	GIT_UNUSED(oid);

	return 0;
}

/*
 * $ git --git-dir tests/resources/testrepo.git count-objects --verbose
 * count: 60
 * size: 240
 * in-pack: 1640
 * packs: 3
 * size-pack: 425
 * prune-packable: 0
 * garbage: 0
 */
void test_odb_foreach__foreach(void)
{
	int nobj = 0;

	cl_git_pass(git_repository_open(&_repo, cl_fixture("testrepo.git")));
	git_repository_odb(&_odb, _repo);

	cl_git_pass(git_odb_foreach(_odb, foreach_cb, &nobj));
	cl_assert_equal_i(60 + 1640, nobj); /* count + in-pack */
}

void test_odb_foreach__one_pack(void)
{
	git_odb_backend *backend = NULL;
	int nobj = 0;

	cl_git_pass(git_odb__new(&_odb, NULL));

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_odb_backend_one_pack(&backend,
		cl_fixture("testrepo.git/objects/pack/pack-a81e489679b7d3418f9ab594bda8ceb37dd4c695.idx"),
		NULL));
#else
	cl_git_pass(git_odb_backend_one_pack(&backend,
		cl_fixture("testrepo.git/objects/pack/pack-a81e489679b7d3418f9ab594bda8ceb37dd4c695.idx")));
#endif

	cl_git_pass(git_odb_add_backend(_odb, backend, 1));
	_repo = NULL;

	cl_git_pass(git_odb_foreach(_odb, foreach_cb, &nobj));
	cl_assert(nobj == 1628);
}

static int foreach_stop_cb(const git_oid *oid, void *data)
{
	int *nobj = data;
	(*nobj)++;

	GIT_UNUSED(oid);

	return (*nobj == 1000) ? -321 : 0;
}

static int foreach_stop_first_cb(const git_oid *oid, void *data)
{
	int *nobj = data;
	(*nobj)++;

	GIT_UNUSED(oid);

	return -123;
}

static int foreach_stop_cb_positive_ret(const git_oid *oid, void *data)
{
	int *nobj = data;
	(*nobj)++;

	GIT_UNUSED(oid);

	return (*nobj == 1000) ? 321 : 0;
}

void test_odb_foreach__interrupt_foreach(void)
{
	int nobj = 0;
	git_oid id;

	cl_git_pass(git_repository_open(&_repo, cl_fixture("testrepo.git")));
	git_repository_odb(&_odb, _repo);

	cl_assert_equal_i(-321, git_odb_foreach(_odb, foreach_stop_cb, &nobj));
	cl_assert(nobj == 1000);

	nobj = 0;

	cl_assert_equal_i(321, git_odb_foreach(_odb, foreach_stop_cb_positive_ret, &nobj));
	cl_assert(nobj == 1000);

	git_odb_free(_odb);
	git_repository_free(_repo);

	cl_git_pass(git_repository_init(&_repo, "onlyloose.git", true));
	git_repository_odb(&_odb, _repo);

	cl_git_pass(git_odb_write(&id, _odb, "", 0, GIT_OBJECT_BLOB));
	cl_assert_equal_i(-123, git_odb_foreach(_odb, foreach_stop_first_cb, &nobj));
}

void test_odb_foreach__files_in_objects_dir(void)
{
	git_repository *repo;
	git_odb *odb;
	git_str buf = GIT_STR_INIT;
	int nobj = 0;

	cl_fixture_sandbox("testrepo.git");
	cl_git_pass(git_repository_open(&repo, "testrepo.git"));

	cl_git_pass(git_str_joinpath(&buf, git_repository_path(repo), "objects/somefile"));
	cl_git_mkfile(buf.ptr, "");
	git_str_dispose(&buf);

	cl_git_pass(git_repository_odb(&odb, repo));
	cl_git_pass(git_odb_foreach(odb, foreach_cb, &nobj));
	cl_assert_equal_i(60 + 1640, nobj); /* count + in-pack */

	git_odb_free(odb);
	git_repository_free(repo);
	cl_fixture_cleanup("testrepo.git");
}
