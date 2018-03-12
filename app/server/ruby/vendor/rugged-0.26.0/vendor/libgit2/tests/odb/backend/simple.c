#include "clar_libgit2.h"
#include "repository.h"
#include "backend_helpers.h"

#define EMPTY_HASH "e69de29bb2d1d6434b8b29ae775ad8c2e48c5391"

static git_repository *_repo;
static git_odb *_odb;
static git_odb_object *_obj;
static git_oid _oid;

static void setup_backend(const fake_object *objs)
{
	git_odb_backend *backend;

	cl_git_pass(build_fake_backend(&backend, objs));

	cl_git_pass(git_repository_odb__weakptr(&_odb, _repo));
	cl_git_pass(git_odb_add_backend(_odb, backend, 10));
}

static void assert_object_contains(git_odb_object *obj, const char *expected)
{
	const char *actual = (const char *) git_odb_object_data(obj);

	cl_assert_equal_s(actual, expected);
}

void test_odb_backend_simple__initialize(void)
{
	_repo = cl_git_sandbox_init("testrepo.git");
	_odb = NULL;
	_obj = NULL;
}

void test_odb_backend_simple__cleanup(void)
{
	git_odb_object_free(_obj);
	cl_git_sandbox_cleanup();
	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION, 1));
}

void test_odb_backend_simple__read_of_object_succeeds(void)
{
	const fake_object objs[] = {
		{ "f6ea0495187600e7b2288c8ac19c5886383a4632", "foobar" },
		{ NULL, NULL }
	};

	setup_backend(objs);

	cl_git_pass(git_oid_fromstr(&_oid, objs[0].oid));
	cl_git_pass(git_odb_read(&_obj, _odb, &_oid));

	assert_object_contains(_obj, objs[0].content);
}

void test_odb_backend_simple__read_of_nonexisting_object_fails(void)
{
	const fake_object objs[] = {
		{ "f6ea0495187600e7b2288c8ac19c5886383a4632", "foobar" },
		{ NULL, NULL }
	};

	setup_backend(objs);

	cl_git_pass(git_oid_fromstr(&_oid, "f6ea0495187600e7b2288c8ac19c5886383a4633"));
	cl_git_fail_with(GIT_ENOTFOUND, git_odb_read(&_obj, _odb, &_oid));
}

void test_odb_backend_simple__read_with_hash_mismatch_fails(void)
{
	const fake_object objs[] = {
		{ "1234567890123456789012345678901234567890", "nonmatching content" },
		{ NULL, NULL }
	};

	setup_backend(objs);

	cl_git_pass(git_oid_fromstr(&_oid, objs[0].oid));
	cl_git_fail_with(GIT_EMISMATCH, git_odb_read(&_obj, _odb, &_oid));
}

void test_odb_backend_simple__read_with_hash_mismatch_succeeds_without_verification(void)
{
	const fake_object objs[] = {
		{ "1234567890123456789012345678901234567890", "nonmatching content" },
		{ NULL, NULL }
	};

	setup_backend(objs);
	cl_git_pass(git_oid_fromstr(&_oid, objs[0].oid));

	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION, 0));
	cl_git_pass(git_odb_read(&_obj, _odb, &_oid));

	assert_object_contains(_obj, objs[0].content);
}

void test_odb_backend_simple__read_prefix_succeeds(void)
{
	const fake_object objs[] = {
		{ "f6ea0495187600e7b2288c8ac19c5886383a4632", "foobar" },
		{ NULL, NULL }
	};

	setup_backend(objs);

	cl_git_pass(git_oid_fromstr(&_oid, "f6ea0495187600e7b2288c8ac19c5886383a4632"));
	cl_git_pass(git_odb_read(&_obj, _odb, &_oid));

	assert_object_contains(_obj, objs[0].content);
}

void test_odb_backend_simple__read_prefix_of_nonexisting_object_fails(void)
{
	const fake_object objs[] = {
		{ "f6ea0495187600e7b2288c8ac19c5886383a4632", "foobar" },
		{ NULL, NULL }
	};
	char *hash = "f6ea0495187600e8";

	setup_backend(objs);

	cl_git_pass(git_oid_fromstrn(&_oid, hash, strlen(hash)));
	cl_git_fail_with(GIT_ENOTFOUND, git_odb_read(&_obj, _odb, &_oid));
}

void test_odb_backend_simple__read_with_ambiguous_prefix_fails(void)
{
	const fake_object objs[] = {
		{ "1234567890111111111111111111111111111111", "first content" },
		{ "1234567890222222222222222222222222222222", "second content" },
		{ NULL, NULL }
	};

	setup_backend(objs);

	cl_git_pass(git_oid_fromstr(&_oid, objs[0].oid));
	cl_git_fail_with(GIT_EAMBIGUOUS, git_odb_read_prefix(&_obj, _odb, &_oid, 7));
}

void test_odb_backend_simple__read_with_highly_ambiguous_prefix(void)
{
	const fake_object objs[] = {
		{ "1234567890111111111111111111111111111111", "first content" },
		{ "1234567890111111111111111111111111111112", "second content" },
		{ NULL, NULL }
	};

	setup_backend(objs);

	cl_git_pass(git_oid_fromstr(&_oid, objs[0].oid));
	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION, 0));
	cl_git_fail_with(GIT_EAMBIGUOUS, git_odb_read_prefix(&_obj, _odb, &_oid, 39));
	cl_git_pass(git_odb_read_prefix(&_obj, _odb, &_oid, 40));
	assert_object_contains(_obj, objs[0].content);
}

void test_odb_backend_simple__exists_succeeds(void)
{
	const fake_object objs[] = {
		{ "f6ea0495187600e7b2288c8ac19c5886383a4632", "foobar" },
		{ NULL, NULL }
	};

	setup_backend(objs);

	cl_git_pass(git_oid_fromstr(&_oid, objs[0].oid));
	cl_assert(git_odb_exists(_odb, &_oid));
}

void test_odb_backend_simple__exists_fails_for_nonexisting_object(void)
{
	const fake_object objs[] = {
		{ "f6ea0495187600e7b2288c8ac19c5886383a4632", "foobar" },
		{ NULL, NULL }
	};

	setup_backend(objs);

	cl_git_pass(git_oid_fromstr(&_oid, "f6ea0495187600e7b2288c8ac19c5886383a4633"));
	cl_assert(git_odb_exists(_odb, &_oid) == 0);
}

void test_odb_backend_simple__exists_prefix_succeeds(void)
{
	const fake_object objs[] = {
		{ "1234567890111111111111111111111111111111", "first content" },
		{ "1234567890222222222222222222222222222222", "second content" },
		{ NULL, NULL }
	};
	git_oid found;

	setup_backend(objs);

	cl_git_pass(git_oid_fromstr(&_oid, objs[0].oid));
	cl_git_pass(git_odb_exists_prefix(&found, _odb, &_oid, 12));
	cl_assert(git_oid_equal(&found, &_oid));
}

void test_odb_backend_simple__exists_with_ambiguous_prefix_fails(void)
{
	const fake_object objs[] = {
		{ "1234567890111111111111111111111111111111", "first content" },
		{ "1234567890222222222222222222222222222222", "second content" },
		{ NULL, NULL }
	};

	setup_backend(objs);

	cl_git_pass(git_oid_fromstr(&_oid, objs[0].oid));
	cl_git_fail_with(GIT_EAMBIGUOUS, git_odb_exists_prefix(NULL, _odb, &_oid, 7));
}

void test_odb_backend_simple__exists_with_highly_ambiguous_prefix(void)
{
	const fake_object objs[] = {
		{ "1234567890111111111111111111111111111111", "first content" },
		{ "1234567890111111111111111111111111111112", "second content" },
		{ NULL, NULL }
	};
	git_oid found;

	setup_backend(objs);

	cl_git_pass(git_oid_fromstr(&_oid, objs[0].oid));
	cl_git_pass(git_libgit2_opts(GIT_OPT_ENABLE_STRICT_HASH_VERIFICATION, 0));
	cl_git_fail_with(GIT_EAMBIGUOUS, git_odb_exists_prefix(&found, _odb, &_oid, 39));
	cl_git_pass(git_odb_exists_prefix(&found, _odb, &_oid, 40));
	cl_assert(git_oid_equal(&found, &_oid));
}
