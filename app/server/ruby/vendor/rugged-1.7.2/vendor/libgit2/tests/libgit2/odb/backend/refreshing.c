#include "clar_libgit2.h"
#include "repository.h"
#include "backend_helpers.h"

static git_repository *_repo;
static fake_backend *_fake;

#define NONEXISTING_HASH "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
#define EXISTING_HASH "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"

static const fake_object _objects[] = {
	{ EXISTING_HASH, "" },
	{ NULL, NULL }
};

static git_oid _nonexisting_oid;
static git_oid _existing_oid;

static void setup_repository_and_backend(void)
{
	git_odb *odb = NULL;
	git_odb_backend *backend = NULL;

	_repo = cl_git_sandbox_init("testrepo.git");

	cl_git_pass(build_fake_backend(&backend, _objects, true));

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_git_pass(git_odb_add_backend(odb, backend, 10));

	_fake = (fake_backend *)backend;
}

void test_odb_backend_refreshing__initialize(void)
{
	git_oid__fromstr(&_nonexisting_oid, NONEXISTING_HASH, GIT_OID_SHA1);
	git_oid__fromstr(&_existing_oid, EXISTING_HASH, GIT_OID_SHA1);
	setup_repository_and_backend();
}

void test_odb_backend_refreshing__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_odb_backend_refreshing__exists_is_invoked_twice_on_failure(void)
{
	git_odb *odb;

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_assert_equal_b(false, git_odb_exists(odb, &_nonexisting_oid));

	cl_assert_equal_i(2, _fake->exists_calls);
	cl_assert_equal_i(1, _fake->refresh_calls);
}

void test_odb_backend_refreshing__read_is_invoked_twice_on_failure(void)
{
	git_object *obj;

	cl_git_fail_with(
		git_object_lookup(&obj, _repo, &_nonexisting_oid, GIT_OBJECT_ANY),
		GIT_ENOTFOUND);

	cl_assert_equal_i(2, _fake->read_calls);
	cl_assert_equal_i(1, _fake->refresh_calls);
}

void test_odb_backend_refreshing__readprefix_is_invoked_twice_on_failure(void)
{
	git_object *obj;

	cl_git_fail_with(
		git_object_lookup_prefix(&obj, _repo, &_nonexisting_oid, 7, GIT_OBJECT_ANY),
		GIT_ENOTFOUND);

	cl_assert_equal_i(2, _fake->read_prefix_calls);
	cl_assert_equal_i(1, _fake->refresh_calls);
}

void test_odb_backend_refreshing__readheader_is_invoked_twice_on_failure(void)
{
	git_odb *odb;
	size_t len;
	git_object_t type;

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));

	cl_git_fail_with(
		git_odb_read_header(&len, &type, odb, &_nonexisting_oid),
		GIT_ENOTFOUND);

	cl_assert_equal_i(2, _fake->read_header_calls);
	cl_assert_equal_i(1, _fake->refresh_calls);
}

void test_odb_backend_refreshing__exists_is_invoked_once_on_success(void)
{
	git_odb *odb;

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_assert_equal_b(true, git_odb_exists(odb, &_existing_oid));

	cl_assert_equal_i(1, _fake->exists_calls);
	cl_assert_equal_i(0, _fake->refresh_calls);
}

void test_odb_backend_refreshing__read_is_invoked_once_on_success(void)
{
	git_object *obj;

	cl_git_pass(git_object_lookup(&obj, _repo, &_existing_oid, GIT_OBJECT_ANY));

	cl_assert_equal_i(1, _fake->read_calls);
	cl_assert_equal_i(0, _fake->refresh_calls);

	git_object_free(obj);
}

void test_odb_backend_refreshing__readprefix_is_invoked_once_on_success(void)
{
	git_object *obj;

	cl_git_pass(git_object_lookup_prefix(&obj, _repo, &_existing_oid, 7, GIT_OBJECT_ANY));

	cl_assert_equal_i(1, _fake->read_prefix_calls);
	cl_assert_equal_i(0, _fake->refresh_calls);

	git_object_free(obj);
}

void test_odb_backend_refreshing__readheader_is_invoked_once_on_success(void)
{
	git_odb *odb;
	size_t len;
	git_object_t type;

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));

	cl_git_pass(git_odb_read_header(&len, &type, odb, &_existing_oid));

	cl_assert_equal_i(1, _fake->read_header_calls);
	cl_assert_equal_i(0, _fake->refresh_calls);
}

void test_odb_backend_refreshing__read_is_invoked_twice_when_revparsing_a_full_oid(void)
{
	git_object *obj;

	cl_git_fail_with(
		git_revparse_single(&obj, _repo, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"),
		GIT_ENOTFOUND);

	cl_assert_equal_i(2, _fake->read_calls);
	cl_assert_equal_i(1, _fake->refresh_calls);
}

void test_odb_backend_refreshing__refresh_is_invoked(void)
{
	git_odb *odb;

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_assert_equal_i(0, git_odb_refresh(odb));

	cl_assert_equal_i(1, _fake->refresh_calls);
}

void test_odb_backend_refreshing__refresh_suppressed_with_no_refresh(void)
{
	git_odb *odb;

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_assert_equal_b(false, git_odb_exists_ext(odb, &_nonexisting_oid, GIT_ODB_LOOKUP_NO_REFRESH));

	cl_assert_equal_i(0, _fake->refresh_calls);
}
