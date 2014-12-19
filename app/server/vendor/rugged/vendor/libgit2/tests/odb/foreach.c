#include "clar_libgit2.h"
#include "odb.h"
#include "git2/odb_backend.h"
#include "pack.h"
#include "buffer.h"

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
 * count: 47
 * size: 4
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
	cl_assert_equal_i(47 + 1640, nobj); /* count + in-pack */
}

void test_odb_foreach__one_pack(void)
{
	git_odb_backend *backend = NULL;
	int nobj = 0;

	cl_git_pass(git_odb_new(&_odb));
	cl_git_pass(git_odb_backend_one_pack(&backend, cl_fixture("testrepo.git/objects/pack/pack-a81e489679b7d3418f9ab594bda8ceb37dd4c695.idx")));
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

void test_odb_foreach__interrupt_foreach(void)
{
	int nobj = 0;

	cl_git_pass(git_repository_open(&_repo, cl_fixture("testrepo.git")));
	git_repository_odb(&_odb, _repo);

	cl_assert_equal_i(-321, git_odb_foreach(_odb, foreach_stop_cb, &nobj));
	cl_assert(nobj == 1000);
}

void test_odb_foreach__files_in_objects_dir(void)
{
	git_repository *repo;
	git_odb *odb;
	git_buf buf = GIT_BUF_INIT;
	int nobj = 0;

	cl_fixture_sandbox("testrepo.git");
	cl_git_pass(git_repository_open(&repo, "testrepo.git"));

	cl_git_pass(git_buf_printf(&buf, "%s/objects/somefile", git_repository_path(repo)));
	cl_git_mkfile(buf.ptr, "");
	git_buf_free(&buf);

	cl_git_pass(git_repository_odb(&odb, repo));
	cl_git_pass(git_odb_foreach(odb, foreach_cb, &nobj));
	cl_assert_equal_i(47 + 1640, nobj); /* count + in-pack */

	git_odb_free(odb);
	git_repository_free(repo);
	cl_fixture_cleanup("testrepo.git");
}
