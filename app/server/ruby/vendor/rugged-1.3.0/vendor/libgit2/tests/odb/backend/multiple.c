#include "clar_libgit2.h"
#include "repository.h"
#include "backend_helpers.h"

#define EXISTING_HASH "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"

static git_repository *_repo;
static git_odb_object *_obj;
static fake_backend *_fake_empty;
static fake_backend *_fake_filled;

static git_oid _existing_oid;

static const fake_object _objects_filled[] = {
	{ EXISTING_HASH, "" },
	{ NULL, NULL }
};

static const fake_object _objects_empty[] = {
	{ NULL, NULL }
};

void test_odb_backend_multiple__initialize(void)
{
	git_odb_backend *backend;

	git_oid_fromstr(&_existing_oid, EXISTING_HASH);

	_obj = NULL;
	_repo = cl_git_sandbox_init("testrepo.git");

	cl_git_pass(build_fake_backend(&backend, _objects_filled));
	_fake_filled = (fake_backend *)backend;

	cl_git_pass(build_fake_backend(&backend, _objects_empty));
	_fake_empty = (fake_backend *)backend;
}

void test_odb_backend_multiple__cleanup(void)
{
	git_odb_object_free(_obj);
	cl_git_sandbox_cleanup();
}

void test_odb_backend_multiple__read_with_empty_first_succeeds(void)
{
	git_odb *odb;

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_git_pass(git_odb_add_backend(odb, (git_odb_backend *)_fake_filled, 10));
	cl_git_pass(git_odb_add_backend(odb, (git_odb_backend *)_fake_empty, 50));

	cl_git_pass(git_odb_read(&_obj, odb, &_existing_oid));

	cl_assert_equal_i(1, _fake_filled->read_calls);
	cl_assert_equal_i(1, _fake_empty->read_calls);
}

void test_odb_backend_multiple__read_with_first_matching_stops(void)
{
	git_odb *odb;

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_git_pass(git_odb_add_backend(odb, (git_odb_backend *)_fake_empty, 10));
	cl_git_pass(git_odb_add_backend(odb, (git_odb_backend *)_fake_filled, 50));

	cl_git_pass(git_odb_read(&_obj, odb, &_existing_oid));

	cl_assert_equal_i(1, _fake_filled->read_calls);
	cl_assert_equal_i(0, _fake_empty->read_calls);
}

void test_odb_backend_multiple__read_prefix_with_first_empty_succeeds(void)
{
	git_odb *odb;

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_git_pass(git_odb_add_backend(odb, (git_odb_backend *)_fake_filled, 10));
	cl_git_pass(git_odb_add_backend(odb, (git_odb_backend *)_fake_empty, 50));

	cl_git_pass(git_odb_read_prefix(&_obj, odb, &_existing_oid, 7));

	cl_assert_equal_i(1, _fake_filled->read_prefix_calls);
	cl_assert_equal_i(1, _fake_empty->read_prefix_calls);
}

void test_odb_backend_multiple__read_prefix_with_first_matching_reads_both(void)
{
	git_odb *odb;

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_git_pass(git_odb_add_backend(odb, (git_odb_backend *)_fake_empty, -10));
	cl_git_pass(git_odb_add_backend(odb, (git_odb_backend *)_fake_filled, 50));

	cl_git_pass(git_odb_read_prefix(&_obj, odb, &_existing_oid, 7));

	cl_assert_equal_i(1, _fake_filled->read_prefix_calls);
	cl_assert_equal_i(1, _fake_empty->read_prefix_calls);
}

void test_odb_backend_multiple__read_prefix_with_first_matching_succeeds_without_hash_verification(void)
{
	git_odb *odb;

	git_libgit2_opts(GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION, 0);

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_git_pass(git_odb_add_backend(odb, (git_odb_backend *)_fake_empty, -10));
	cl_git_pass(git_odb_add_backend(odb, (git_odb_backend *)_fake_filled, 50));

	cl_git_pass(git_odb_read_prefix(&_obj, odb, &_existing_oid, 7));

	/*
	 * Both backends should be checked as we have to check
	 * for collisions
	 */
	cl_assert_equal_i(1, _fake_filled->read_prefix_calls);
	cl_assert_equal_i(1, _fake_empty->read_prefix_calls);

	git_libgit2_opts(GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION, 1);
}
