#include "clar_libgit2.h"
#include "git2/sys/odb_backend.h"
#include "repository.h"

typedef struct fake_backend {
	git_odb_backend parent;

	git_error_code error_code;

	int exists_calls;
	int read_calls;
	int read_header_calls;
	int read_prefix_calls;
} fake_backend;

static git_repository *_repo;
static fake_backend *_fake;
static git_oid _oid;

static int fake_backend__exists(git_odb_backend *backend, const git_oid *oid)
{
	fake_backend *fake;

	GIT_UNUSED(oid);

	fake = (fake_backend *)backend;

	fake->exists_calls++;

	return (fake->error_code == GIT_OK);
}

static int fake_backend__read(
	void **buffer_p, size_t *len_p, git_otype *type_p,
	git_odb_backend *backend, const git_oid *oid)
{
	fake_backend *fake;

	GIT_UNUSED(buffer_p);
	GIT_UNUSED(len_p);
	GIT_UNUSED(type_p);
	GIT_UNUSED(oid);

	fake = (fake_backend *)backend;

	fake->read_calls++;

	*len_p = 0;
	*buffer_p = NULL;
	*type_p = GIT_OBJ_BLOB;

	return fake->error_code;
}

static int fake_backend__read_header(
	size_t *len_p, git_otype *type_p,
	git_odb_backend *backend, const git_oid *oid)
{
	fake_backend *fake;

	GIT_UNUSED(len_p);
	GIT_UNUSED(type_p);
	GIT_UNUSED(oid);

	fake = (fake_backend *)backend;

	fake->read_header_calls++;

	*len_p = 0;
	*type_p = GIT_OBJ_BLOB;

	return fake->error_code;
}

static int fake_backend__read_prefix(
	git_oid *out_oid, void **buffer_p, size_t *len_p, git_otype *type_p,
	git_odb_backend *backend, const git_oid *short_oid,	size_t len)
{
	fake_backend *fake;

	GIT_UNUSED(out_oid);
	GIT_UNUSED(buffer_p);
	GIT_UNUSED(len_p);
	GIT_UNUSED(type_p);
	GIT_UNUSED(short_oid);
	GIT_UNUSED(len);

	fake = (fake_backend *)backend;

	fake->read_prefix_calls++;

	*len_p = 0;
	*buffer_p = NULL;
	*type_p = GIT_OBJ_BLOB;

	return fake->error_code;
}

static void fake_backend__free(git_odb_backend *_backend)
{
	fake_backend *backend;

	backend = (fake_backend *)_backend;

	git__free(backend);
}

static int build_fake_backend(
	git_odb_backend **out,
	git_error_code error_code)
{
	fake_backend *backend;

	backend = git__calloc(1, sizeof(fake_backend));
	GITERR_CHECK_ALLOC(backend);

	backend->parent.version = GIT_ODB_BACKEND_VERSION;

	backend->parent.refresh = NULL;
	backend->error_code = error_code;

	backend->parent.read = fake_backend__read;
	backend->parent.read_prefix = fake_backend__read_prefix;
	backend->parent.read_header = fake_backend__read_header;
	backend->parent.exists = fake_backend__exists;
	backend->parent.free = &fake_backend__free;

	*out = (git_odb_backend *)backend;

	return 0;
}

static void setup_repository_and_backend(git_error_code error_code)
{
	git_odb *odb = NULL;
	git_odb_backend *backend = NULL;

	_repo = cl_git_sandbox_init("testrepo.git");

	cl_git_pass(build_fake_backend(&backend, error_code));

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_git_pass(git_odb_add_backend(odb, backend, 10));

	_fake = (fake_backend *)backend;

	cl_git_pass(git_oid_fromstr(&_oid, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"));
}

void test_odb_backend_nonrefreshing__cleanup(void)
{
	cl_git_sandbox_cleanup();
}

void test_odb_backend_nonrefreshing__exists_is_invoked_once_on_failure(void)
{
	git_odb *odb;

	setup_repository_and_backend(GIT_ENOTFOUND);

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_assert_equal_b(false, git_odb_exists(odb, &_oid));

	cl_assert_equal_i(1, _fake->exists_calls);
}

void test_odb_backend_nonrefreshing__read_is_invoked_once_on_failure(void)
{
	git_object *obj;

	setup_repository_and_backend(GIT_ENOTFOUND);

	cl_git_fail_with(
		git_object_lookup(&obj, _repo, &_oid, GIT_OBJ_ANY),
		GIT_ENOTFOUND);

	cl_assert_equal_i(1, _fake->read_calls);
}

void test_odb_backend_nonrefreshing__readprefix_is_invoked_once_on_failure(void)
{
	git_object *obj;

	setup_repository_and_backend(GIT_ENOTFOUND);

	cl_git_fail_with(
		git_object_lookup_prefix(&obj, _repo, &_oid, 7, GIT_OBJ_ANY),
		GIT_ENOTFOUND);

	cl_assert_equal_i(1, _fake->read_prefix_calls);
}

void test_odb_backend_nonrefreshing__readheader_is_invoked_once_on_failure(void)
{
	git_odb *odb;
	size_t len;
	git_otype type;

	setup_repository_and_backend(GIT_ENOTFOUND);

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));

	cl_git_fail_with(
		git_odb_read_header(&len, &type, odb, &_oid),
		GIT_ENOTFOUND);

	cl_assert_equal_i(1, _fake->read_header_calls);
}

void test_odb_backend_nonrefreshing__exists_is_invoked_once_on_success(void)
{
	git_odb *odb;

	setup_repository_and_backend(GIT_OK);

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));
	cl_assert_equal_b(true, git_odb_exists(odb, &_oid));

	cl_assert_equal_i(1, _fake->exists_calls);
}

void test_odb_backend_nonrefreshing__read_is_invoked_once_on_success(void)
{
	git_object *obj;

	setup_repository_and_backend(GIT_OK);

	cl_git_pass(git_object_lookup(&obj, _repo, &_oid, GIT_OBJ_ANY));

	cl_assert_equal_i(1, _fake->read_calls);

	git_object_free(obj);
}

void test_odb_backend_nonrefreshing__readprefix_is_invoked_once_on_success(void)
{
	git_object *obj;

	setup_repository_and_backend(GIT_OK);

	cl_git_pass(git_object_lookup_prefix(&obj, _repo, &_oid, 7, GIT_OBJ_ANY));

	cl_assert_equal_i(1, _fake->read_prefix_calls);

	git_object_free(obj);
}

void test_odb_backend_nonrefreshing__readheader_is_invoked_once_on_success(void)
{
	git_odb *odb;
	size_t len;
	git_otype type;

	setup_repository_and_backend(GIT_OK);

	cl_git_pass(git_repository_odb__weakptr(&odb, _repo));

	cl_git_pass(git_odb_read_header(&len, &type, odb, &_oid));

	cl_assert_equal_i(1, _fake->read_header_calls);
}

void test_odb_backend_nonrefreshing__read_is_invoked_once_when_revparsing_a_full_oid(void)
{
	git_object *obj;

	setup_repository_and_backend(GIT_ENOTFOUND);

	cl_git_fail_with(
		git_revparse_single(&obj, _repo, "deadbeefdeadbeefdeadbeefdeadbeefdeadbeef"),
		GIT_ENOTFOUND);

	cl_assert_equal_i(1, _fake->read_calls);
}
