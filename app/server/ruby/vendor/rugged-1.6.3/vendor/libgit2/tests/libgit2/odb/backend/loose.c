#include "clar_libgit2.h"
#include "repository.h"
#include "odb.h"
#include "backend_helpers.h"
#include "git2/sys/mempack.h"

static git_repository *_repo;
static git_odb *_odb;

void test_odb_backend_loose__initialize(void)
{
	git_odb_backend *backend;

	cl_fixture_sandbox("testrepo.git");

#ifdef GIT_EXPERIMENTAL_SHA256
	cl_git_pass(git_odb_backend_loose(&backend, "testrepo.git/objects", NULL));
#else
	cl_git_pass(git_odb_backend_loose(&backend, "testrepo.git/objects", 0, 0, 0, 0));
#endif

	cl_git_pass(git_odb__new(&_odb, NULL));
	cl_git_pass(git_odb_add_backend(_odb, backend, 10));
	cl_git_pass(git_repository_wrap_odb(&_repo, _odb));
}

void test_odb_backend_loose__cleanup(void)
{
	git_odb_free(_odb);
	git_repository_free(_repo);

	cl_fixture_cleanup("testrepo.git");
}

void test_odb_backend_loose__read(void)
{
	git_oid oid;
	git_odb_object *obj;

	cl_git_pass(git_oid__fromstr(&oid, "1385f264afb75a56a5bec74243be9b367ba4ca08", GIT_OID_SHA1));
	cl_git_pass(git_odb_read(&obj, _odb, &oid));
	git_odb_object_free(obj);
}
