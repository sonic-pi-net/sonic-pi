#include "clar_libgit2.h"
#include "repository.h"
#include "odb.h"
#include "backend_helpers.h"
#include "git2/sys/mempack.h"

static git_odb *_odb;
static git_oid _oid;
static git_odb_object *_obj;
static git_repository *_repo;

void test_odb_backend_mempack__initialize(void)
{
	git_odb_backend *backend;

	cl_git_pass(git_mempack_new(&backend));
	cl_git_pass(git_odb__new(&_odb, NULL));
	cl_git_pass(git_odb_add_backend(_odb, backend, 10));
	cl_git_pass(git_repository__wrap_odb(&_repo, _odb, GIT_OID_SHA1));
}

void test_odb_backend_mempack__cleanup(void)
{
	git_odb_object_free(_obj);
	git_odb_free(_odb);
	git_repository_free(_repo);
}

void test_odb_backend_mempack__write_succeeds(void)
{
	const char *data = "data";
	cl_git_pass(git_odb_write(&_oid, _odb, data, strlen(data) + 1, GIT_OBJECT_BLOB));
	cl_git_pass(git_odb_read(&_obj, _odb, &_oid));
}

void test_odb_backend_mempack__read_of_missing_object_fails(void)
{
	cl_git_pass(git_oid__fromstr(&_oid, "f6ea0495187600e7b2288c8ac19c5886383a4633", GIT_OID_SHA1));
	cl_git_fail_with(GIT_ENOTFOUND, git_odb_read(&_obj, _odb, &_oid));
}

void test_odb_backend_mempack__exists_of_missing_object_fails(void)
{
	cl_git_pass(git_oid__fromstr(&_oid, "f6ea0495187600e7b2288c8ac19c5886383a4633", GIT_OID_SHA1));
	cl_assert(git_odb_exists(_odb, &_oid) == 0);
}

void test_odb_backend_mempack__exists_with_existing_objects_succeeds(void)
{
	const char *data = "data";
	cl_git_pass(git_odb_write(&_oid, _odb, data, strlen(data) + 1, GIT_OBJECT_BLOB));
	cl_assert(git_odb_exists(_odb, &_oid) == 1);
}

void test_odb_backend_mempack__blob_create_from_buffer_succeeds(void)
{
	const char *data = "data";

	cl_git_pass(git_blob_create_from_buffer(&_oid, _repo, data, strlen(data) + 1));
	cl_assert(git_odb_exists(_odb, &_oid) == 1);
}
